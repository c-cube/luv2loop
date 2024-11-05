type t = Picos.Computation.packed

let dummy : t = Picos.Computation.(Packed (create ()))

(** Cancel the handle *)
let[@inline] cancel (self : t) (ebt : Exn_bt.t) : unit =
  let (Packed c) = self in
  Picos.Computation.cancel c (Exn_bt.exn ebt) (Exn_bt.bt ebt)
