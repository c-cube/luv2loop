open Common_

type microtask = unit -> unit

type task =
  | T_start : 'a Fiber.t * (unit -> 'a) -> task
  | T_cont : Fiber.any * ('a, unit) ES.continuation * 'a -> task
  | T_discont : _ Fiber.t * (_, unit) ES.continuation * Exn_bt.t -> task
  | T_run : (unit -> unit) -> task

type t = {
  root_fiber: Fiber.any;  (** Fiber encompassing all other *)
  ev_loop: Event_loop.t;
  t_id: int;  (** Thread on which the scheduler runs *)
  mutable cur_fiber: Fiber.any option;  (** Currently running fiber *)
  mutable cur_span: Trace.span;  (** Span for the current fiber, if any *)
  mutable n_suspended: int;  (** Number of suspended tasks. *)
  task_q: task Queue.t;  (** Queue of regular tasks *)
  next_tick_tasks: task Queue.t;  (** Tasks for the next tick *)
  micro_tasks_q: microtask Queue.t;  (** Microtasks, run almost immediately *)
  outside_q: task Queue.t Lock.t;
      (** Queue of tasks from the outside (from other threads possibly) *)
  mutable n_tasks: int;  (** Total number of tasks processed since the start *)
}

let k_current_scheduler : t TLS.t = TLS.create ()

let () =
  (* to get current fiber, just get current scheduler *)
  Fiber.get_current_ :=
    fun () ->
      match TLS.get_exn k_current_scheduler with
      | s -> s.cur_fiber
      | exception TLS.Not_set -> None

exception Inactive

let _dummy_span = Trace.Collector.dummy_span

let[@inline] check_active_ (self : t) =
  let (Any_fiber f) = self.root_fiber in
  if Fiber.is_done f then raise Inactive

let[@inline] schedule_no_check_ (self : t) (task : task) : unit =
  Queue.push task self.next_tick_tasks

let[@inline] schedule_ (self : t) (task : task) : unit =
  check_active_ self;
  schedule_no_check_ self task

let[@inline] schedule_micro_task_ (self : t) f : unit =
  check_active_ self;
  Queue.push f self.micro_tasks_q

let[@inline] active self =
  let (Any_fiber f) = self.root_fiber in
  not (Fiber.is_done f)

let[@inline] n_tasks_since_beginning self = self.n_tasks

let[@inline] has_pending_tasks self : bool =
  not
    (Queue.is_empty self.task_q
    && Queue.is_empty self.micro_tasks_q
    && Queue.is_empty self.next_tick_tasks
    && Lock.map_no_exn Queue.is_empty self.outside_q)

let[@inline] has_suspended_tasks self : bool = self.n_suspended > 0

let[@inline] n_queued_tasks (self : t) : int =
  Queue.length self.task_q + Queue.length self.next_tick_tasks

let[@inline] ev_loop self = self.ev_loop

let create ~ev_loop () : t =
  {
    ev_loop;
    t_id = Thread.self () |> Thread.id;
    cur_fiber = None;
    cur_span = 0L;
    task_q = Queue.create ();
    n_suspended = 0;
    next_tick_tasks = Queue.create ();
    root_fiber = Any_fiber (Fiber.create ());
    micro_tasks_q = Queue.create ();
    outside_q = Lock.create @@ Queue.create ();
    n_tasks = 0;
  }

let dispose (self : t) : unit =
  (* cancel the main task *)
  let ebt = Exn_bt.get_callstack 15 Exit in
  Fiber.cancel_any self.root_fiber ebt

let[@inline] trace_enter_fiber_ (self : t) (fiber : _ Fiber.t) =
  if fiber.name <> "" then
    self.cur_span <- Trace.enter_span ~__FILE__ ~__LINE__ fiber.name

let[@inline] trace_exit_fiber_ (self : t) =
  if self.cur_span <> Trace.Collector.dummy_span then (
    Trace.exit_span self.cur_span;
    self.cur_span <- Trace.Collector.dummy_span
  )

(** Scheduler for the current thread *)
let[@inline] get_for_current_thread () : t =
  match TLS.get_exn k_current_scheduler with
  | exception TLS.Not_set ->
    failwith
      "Scheduler.get_for_current_thread: must be run from inside the event loop"
  | sch -> sch

let[@inline] schedule_micro_task (f : unit -> unit) : unit =
  let self = get_for_current_thread () in
  schedule_micro_task_ self f

let spawn ?name ?(propagate_cancel_to_parent = true) (f : unit -> 'a) :
    'a Fiber.t =
  let self : t = get_for_current_thread () in
  check_active_ self;

  (* build a switch for the fiber *)
  let (Any_fiber parent) =
    match self.cur_fiber with
    | None -> self.root_fiber
    | Some s -> s
  in

  let fiber = Fiber.create ?name () in
  Fiber.add_child ~protected:(not propagate_cancel_to_parent) parent fiber;
  schedule_ self (T_start (fiber, f));
  fiber

let spawn_as_child_of ?name ?(propagate_cancel_to_parent = true) (self : t)
    (parent : _ Fiber.t) f : _ Fiber.t =
  check_active_ self;
  let fiber = Fiber.create ?name () in
  Fiber.add_child ~protected:(not propagate_cancel_to_parent) parent fiber;
  schedule_ self (T_start (fiber, f));
  fiber

let spawn_from_anywhere ?name (self : t) f : _ Fiber.t =
  check_active_ self;
  let (Any_fiber parent) = self.root_fiber in
  let fiber = Fiber.create ?name () in
  Fiber.add_child ~protected:true parent fiber;
  Lock.with_ self.outside_q (fun q -> Queue.push (T_start (fiber, f)) q);
  fiber

let run_from_anywhere (self : t) f : unit =
  Lock.with_ self.outside_q (fun q -> Queue.push (T_run f) q)

(** call [f()] and resolve the fiber once [f()] is done *)
let run_task_and_resolve_fiber (self : t) fiber f =
  try
    let r = f () in
    trace_exit_fiber_ self;
    Fiber.resolve fiber r
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    trace_exit_fiber_ self;
    Fiber.cancel fiber (Exn_bt.make exn bt)

open struct
  let exnc exn = Printf.eprintf "fiber raised %s\n%!" (Printexc.to_string exn)

  let mk_handler self fiber : _ ES.handler =
    (* the main effect handler *)
    let effc : type b. b Effect.t -> ((b, unit) ES.continuation -> 'a) option =
      function
      | Effects.Suspend { before_suspend } ->
        Some
          (fun k ->
            trace_exit_fiber_ self;
            self.n_suspended <- 1 + self.n_suspended;

            let wakeup_from_same_thread () =
              self.n_suspended <- self.n_suspended - 1;
              schedule_no_check_ self (T_cont (Any_fiber fiber, k, ()))
              (* make sure we're not in the event loop, waiting for sth else *)
            in

            let wakeup () =
              (* Trace.message "wakeup suspended fiber"; *)
              if Thread.id @@ Thread.self () = self.t_id then
                wakeup_from_same_thread ()
              else (
                (* put in the thread-safe queue *)
                run_from_anywhere self (fun () -> wakeup_from_same_thread ());
                (* make sure that, if the scheduler thread is sleeping in the
                   event loop, we wake it up *)
                Event_loop.interrupt_if_in_blocking_section self.ev_loop
              )
            in

            match Fiber.Private_.as_cancelled fiber with
            | Some ebt ->
              (* fail the fiber *)
              run_from_anywhere self (fun () ->
                  schedule_no_check_ self (T_discont (fiber, k, ebt)))
            | None -> before_suspend ~wakeup)
      | Effects.Yield ->
        Some
          (fun k ->
            trace_exit_fiber_ self;
            schedule_ self (T_cont (Any_fiber fiber, k, ())))
      | _ -> None
    in

    { effc; retc = Fun.id; exnc }
end

let run_task (self : t) (task : task) : unit =
  match task with
  | T_start (fiber, f) ->
    if not (Fiber.is_cancelled fiber) then (
      (* whole fiber runs under the effect handler *)
      self.cur_fiber <- Some (Any_fiber fiber);
      trace_enter_fiber_ self fiber;

      ES.continue_with
        (ES.fiber (run_task_and_resolve_fiber self fiber))
        f (mk_handler self fiber)
    )
  | T_cont ((Any_fiber fiber as any_fib), k, x) ->
    (match Fiber.peek fiber with
    | Some (Error ebt) ->
      (* cleanup *)
      Exn_bt.discontinue_with k ebt (mk_handler self fiber)
    | Some _ -> assert false
    | None ->
      (* continue running the fiber *)
      self.cur_fiber <- Some any_fib;
      trace_enter_fiber_ self fiber;
      ES.continue_with k x (mk_handler self fiber))
  | T_discont (fiber, k, ebt) ->
    Exn_bt.discontinue_with k ebt (mk_handler self fiber)
  | T_run f -> f ()

let run_iteration (self : t) : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "fuseau.scheduler.iteration" in
  check_active_ self;

  (* move all pending tasks to [task_q] *)
  Queue.transfer self.next_tick_tasks self.task_q;
  Lock.with_ self.outside_q (fun q -> Queue.transfer q self.task_q);

  while not (Queue.is_empty self.task_q) do
    (* run microtasks *)
    while not (Queue.is_empty self.micro_tasks_q) do
      let f = Queue.pop self.micro_tasks_q in
      try f ()
      with e ->
        Printf.eprintf "warning: microtask raised %s\n%!" (Printexc.to_string e)
    done;

    let task = Queue.pop self.task_q in
    self.n_tasks <- 1 + self.n_tasks;

    run_task self task;
    (* cleanup *)
    self.cur_fiber <- None
  done
