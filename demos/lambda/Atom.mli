(* TEMPORARY document *)

(* Atoms. *)

type atom

type t =
    atom

val identity: atom -> int
val hint: atom -> string
val show: atom -> string
val print: out_channel -> atom -> unit

(* Producing fresh atoms. *)

val freshh: string -> atom
val fresha: atom -> atom

(* Comparison of atoms. *)

val equal: atom -> atom -> bool
val compare: atom -> atom -> int
val hash: atom -> int

(* Sets. *)

module Set : sig
  include Set.S with type elt = atom

  (* Sets of atoms form a monoid under union. *)

  class ['z] monoid : object
    constraint 'z = t
    method zero: 'z
    method plus: 'z -> 'z -> 'z
  end

  val show: t -> string
  val print: out_channel -> t -> unit
end

(* Maps. *)

module Map : Map.S with type key = atom
