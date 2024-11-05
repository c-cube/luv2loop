type 'a t = {
  poll: unit -> 'a Exn_bt.result option;
  wait: (unit -> unit) -> Cancel_handle.t;
}

type 'ret branch = When : 'a t * ('a -> 'ret) -> 'ret branch

let _cancel_exn : Exn_bt.t = Exn_bt.get_callstack 0 Sys.Break

let rec select_rec_ brs =
  let rec poll_list_ = function
    | When (ev, f) :: tl ->
      (* see if [ev] is ready *)
      (match ev.poll () with
      | Some (Ok x) -> (f [@tailcall]) x
      | Some (Error ebt) -> Exn_bt.raise ebt
      | None -> poll_list_ tl)
    | [] ->
      (* no branch worked. Now let's wait. *)
      let cancel_handlers = ref [] in
      let trigger = Picos.Trigger.create () in

      (* make sure we call [wakeup] only once *)
      let woken_up = Atomic.make false in
      let wakeup_once () =
        if not (Atomic.exchange woken_up true) then Picos.Trigger.signal trigger
      in

      Picos.Trigger.await trigger |> Option.iter Exn_bt.raise;

      List.iter
        (fun (When (ev, _)) ->
          let cancel = ev.wait wakeup_once in
          if cancel != Cancel_handle.dummy then
            cancel_handlers := cancel :: !cancel_handlers)
        brs;

      (* poll again *)
      (select_rec_ [@tailcall]) brs
  in

  poll_list_ brs

let[@inline] select (brs : _ branch list) =
  match brs with
  | [] -> assert false
  | brs -> select_rec_ brs
