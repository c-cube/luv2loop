(** Exception with backtrace. *)

type t = exn * Printexc.raw_backtrace

val exn : t -> exn
val bt : t -> Printexc.raw_backtrace
val raise : t -> 'a
val get : exn -> t
val get_callstack : int -> exn -> t

val make : exn -> Printexc.raw_backtrace -> t
(** Trivial builder *)

val show : t -> string
(** Simple printing *)

val pp : Format.formatter -> t -> unit

type nonrec 'a result = ('a, t) result

val unwrap : 'a result -> 'a
(** [unwrap (Ok x)] is [x], [unwrap (Error ebt)] re-raises [ebt]. *)

val discontinue_with :
  ('a, 'b) Effect.Shallow.continuation ->
  t ->
  ('b, 'c) Effect.Shallow.handler ->
  'c
