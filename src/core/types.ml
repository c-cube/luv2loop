(** Some type definitions *)

open Common_
module FM = Fiber_handle.Map

type 'a fiber_callback = ('a, Exn_bt.t) result -> unit
type cancel_callback = Exn_bt.t -> unit

type 'a fiber = {
  id: Fiber_handle.t;  (** unique identifier for this fiber *)
  name: string;  (** Optional name, for tracing *)
  comp: 'a Picos.Computation.t;  (** The underlying picos computation *)
  state: 'a fiber_status A.t;
      (** Current state of the fiber (result, or cancellation status) *)
}

and 'a fiber_status =
  | Done
  | Wait of {
      already_done: 'a Exn_bt.result option;
          (** If [Some _], the main computation has ended but we're
          waiting for children to terminate. This means we can't
          start new children. We can still change our mind and turn
          [Some (Ok _)] into [Some (Error ebt)] if a child fails. *)
      children: any_fiber FM.t;  (** Set of children *)
    }

and any_fiber = Any_fiber : _ fiber -> any_fiber [@@unboxed]
