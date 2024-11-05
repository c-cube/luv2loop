(** Abstraction over an event loop, from the pov of the scheduler.

    All the scheduler cares about is running an iteration
    of the loop, with or without blocking; and interrupting
    the loop if it's in the middle of blocking.
*)

(** Abstract event loop, inspired by Lwt engine *)
class type t = object
  method one_step : block:bool -> unit -> unit
  (** Run one step of the event loop.
        @param block if [true], the call might block until the next timeout
        or until the next IO event occurs. If [false], this does not
        block and returns after having processed the available events. *)

  method on_timer :
    float -> repeat:bool -> (Cancel_handle.t -> unit) -> Cancel_handle.t
  (** [on_timer delay ~repeat f] runs [f] after [delay].
        @param repeat if true runs [f] every [delay] seconds *)

  method interrupt_if_in_blocking_section : unit
  (** If run from inside the event loop when it's waiting, wakes the event loop up *)
end

let[@inline] one_step (self : #t) ~block () = self#one_step ~block ()

let[@inline] on_timer (self : #t) delay ~repeat f =
  self#on_timer delay ~repeat f

let[@inline] interrupt_if_in_blocking_section (self : #t) : unit =
  self#interrupt_if_in_blocking_section
