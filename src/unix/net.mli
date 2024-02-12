(** Networking *)

module Inet_addr : sig
  type t = Unix.inet_addr

  val loopback : t
  val any : t
  val show : t -> string
  val of_string : string -> t option

  val of_string_exn : string -> t
  (** @raise Invalid_argument *)
end

(** Socket addresses *)
module Sockaddr : sig
  type t = Unix.sockaddr

  val show : t -> string
  val unix : string -> t
  val inet : Inet_addr.t -> int -> t
  val inet_parse : string -> int -> t option
  val inet_parse_exn : string -> int -> t
  val inet_local : int -> t
  val inet_any : int -> t
end

module TCP_server : sig
  type t

  val stop : t -> unit
  val join : t -> unit

  val with_serve :
    Sockaddr.t -> (Sockaddr.t -> IO_in.t -> IO_out.t -> unit) -> (t -> 'a) -> 'a
end
