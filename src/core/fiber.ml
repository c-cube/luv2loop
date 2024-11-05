open Common_
open Types
module Comp = Picos.Computation

type 'a state = 'a Types.fiber_status =
  | Done
  | Wait of {
      already_done: 'a Exn_bt.result option;
      children: any_fiber FM.t;  (** Set of children *)
    }

type 'a t = 'a Types.fiber
type 'a callback = 'a Types.fiber_callback
type cancel_callback = Exn_bt.t -> unit
type any = Types.any_fiber = Any_fiber : _ t -> any [@@unboxed]

let[@inline] peek self = Comp.peek self.comp

exception Running = Comp.Running

let[@inline] peek_exn self = Comp.peek_exn self.comp
let[@inline] is_done self = not (Comp.is_running self.comp)
let[@inline] is_cancelled self = Comp.is_canceled self.comp

let as_cancelled_ self =
  match A.get self.state with
  | Done -> Comp.canceled self.comp
  | Wait { already_done = Some (Error ebt); _ } -> Some ebt
  | _ -> None

(** Register [f] to be called when the fiber ends.
    If the fiber is done already, call [f] immediately.
    [f] is called exactly once. *)
let on_res (self : _ t) f =
  let run_on_res f self =
    let res =
      match Comp.peek self.comp with
      | None -> assert false
      | Some res -> res
    in
    f res
  in

  let trigger =
    (Picos.Trigger.from_action [@alert "-handler"]) f self (fun _tr f self ->
        run_on_res f self)
  in
  if not (Comp.try_attach self.comp trigger) then run_on_res f self

let fail_waiting_fiber (self : _ t) (ebt : Exn_bt.t) =
  while
    match A.get self.state with
    | Wait ({ already_done = Some (Ok _); _ } as wait) as old_st ->
      (* turn an about-to-succeed fiber to an about-to-fail one *)
      let new_st = Wait { wait with already_done = Some (Error ebt) } in
      not (A.compare_and_set self.state old_st new_st)
    | _ -> false
  do
    ()
  done

let resolve_to_final_state_and_call_waiters (self : _ t) : unit =
  (* get the final result *)
  match A.get self.state with
  | Wait { already_done = Some (Ok x); _ } ->
    A.set self.state Done;
    Comp.return self.comp x
  | Wait { already_done = Some (Error ebt); _ } ->
    A.set self.state Done;
    Comp.cancel self.comp (Exn_bt.exn ebt) (Exn_bt.bt ebt)
  | Done | Wait { already_done = None; _ } -> assert false

(** Call waiters with [res] once all [children] are done *)
let call_waiters_and_set_res_once_children_are_done ~children (self : _ t) :
    unit =
  let n_children = FM.cardinal children in
  if n_children > 0 then (
    (* wait for all children to be done *)
    let n_waiting = A.make (FM.cardinal children) in

    let on_child_finish (res : _ result) =
      (match res with
      | Error ebt -> fail_waiting_fiber self ebt
      | Ok _ -> ());

      (* if we're the last to finish, resolve the fiber *)
      if A.fetch_and_add n_waiting (-1) = 1 then
        resolve_to_final_state_and_call_waiters self
    in

    FM.iter (fun _ (Any_fiber f) -> on_res f on_child_finish) children
  ) else
    (* no children, can resolve immediately *)
    resolve_to_final_state_and_call_waiters self

(** Successfully resolve the fiber *)
let resolve (self : 'a t) (r : 'a) : unit =
  while
    match A.get self.state with
    | Wait ({ children; already_done = None; _ } as wait) as old ->
      (* switch to [already_done=Some r] *)
      let new_st = Wait { wait with already_done = Some (Ok r) } in
      if A.compare_and_set self.state old new_st then (
        call_waiters_and_set_res_once_children_are_done ~children self;
        false
      ) else
        true
    | Wait { already_done = Some _; _ } | Done -> false
  do
    ()
  done

let rec fail_fiber : type a. a t -> Exn_bt.t -> unit =
 fun self ebt ->
  (* Trace.messagef (fun k -> k "fail fiber[%d]" (self.id :> int)); *)
  while
    match A.get self.state with
    | Wait { already_done = Some (Error _); _ } -> false
    | Wait ({ children; already_done = _; _ } as wait) as old ->
      (* resolve as failed instead *)
      let new_st = Wait { wait with already_done = Some (Error ebt) } in
      if A.compare_and_set self.state old new_st then (
        (* here, unlike in {!resolve_fiber}, we immediately cancel children *)
        cancel_children ~children ebt;
        call_waiters_and_set_res_once_children_are_done ~children self;
        false
      ) else
        true
    | Done -> false
  do
    ()
  done

(** Cancel eagerly all children *)
and cancel_children ebt ~children : unit =
  FM.iter (fun _ (Any_fiber f) -> fail_fiber f ebt) children

let remove_child (self : _ t) (child : _ t) =
  while
    match A.get self.state with
    | Wait ({ children; _ } as wait) as old ->
      let new_st = Wait { wait with children = FM.remove child.id children } in
      not (A.compare_and_set self.state old new_st)
    | _ -> false
  do
    ()
  done

(** Add a child to [self].
    @param protected if true, the child's failure will not affect [self]. *)
let add_child ~protected (self : _ fiber) (child : _ fiber) =
  while
    match A.get self.state with
    | Wait ({ children; already_done = None; _ } as wait) as old ->
      let new_st =
        Wait { wait with children = FM.add child.id (Any_fiber child) children }
      in

      if A.compare_and_set self.state old new_st then (
        (* make sure to remove [child] from [self.children] once it's done *)
        on_res child (function
          | Ok _ -> remove_child self child
          | Error ebt ->
            remove_child self child;
            if not protected then fail_fiber self ebt);

        false
      ) else
        true
    | _ -> false
  do
    ()
  done

exception Cancelled of Exn_bt.t

let create ?(name = "") () =
  let id = Fiber_handle.fresh () in
  let comp = Comp.create () in
  {
    comp;
    state = A.make @@ Wait { already_done = None; children = FM.empty };
    id;
    name;
  }

let[@inline] return x : _ t =
  let id = Fiber_handle.fresh () in
  { id; name = ""; comp = Comp.returned x; state = A.make Done }

let fail ebt : _ t =
  let id = Fiber_handle.fresh () in
  let comp = Comp.create () in
  Comp.cancel comp (Exn_bt.exn ebt) (Exn_bt.bt ebt);
  { id; name = ""; comp; state = A.make Done }

let resolve = resolve
let cancel = fail_fiber
let add_child = add_child
let[@inline] cancel_any (Any_fiber f) ebt = cancel f ebt

(** A helper to get around circular dependencies. This is implemented via
      TLS, looking in the current thread's scheduler (if any). *)
let get_current_ : (unit -> any option) ref = ref (fun () -> None)

let[@inline] get_current () : any =
  match !get_current_ () with
  | None -> failwith "`Fiber.get_current` must be called from inside a fiber"
  | Some any -> any

let run_cancel_cb_ _trigger cb self =
  match Comp.peek_exn self.comp with
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    cb (Exn_bt.make exn bt)
  | _ -> ()

let add_cancel_cb_with_trigger_ (self : _ t) cb trigger : bool =
  let ok =
    (Picos.Trigger.on_signal [@alert "-handler"]) trigger cb self run_cancel_cb_
  in
  assert ok;
  Comp.try_attach self.comp trigger

let add_cancel_cb_ (self : _ t) cb =
  let trigger = Picos.Trigger.create () in
  add_cancel_cb_with_trigger_ self cb trigger

let with_cancel_callback cb (k : unit -> 'a) : 'a =
  match !get_current_ () with
  | None -> failwith "with_cancel_callback` must be called from inside a fiber"
  | Some (Any_fiber f) ->
    let trigger = Picos.Trigger.create () in
    if add_cancel_cb_ f cb then
      Fun.protect k ~finally:(fun () -> Comp.detach f.comp trigger)
    else
      k ()

let[@inline] await self = Comp.await self.comp

let try_await self =
  try Ok (await self)
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    let ebt = Exn_bt.make exn bt in
    Error ebt

let yield = Picos.Fiber.yield

module Private_ = struct
  let create = create
  let cancel = cancel
  let as_cancelled = as_cancelled_
end
