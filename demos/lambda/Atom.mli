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
  val show: t -> string
  val print: out_channel -> t -> unit
end

(* Maps. *)

module Map : Map.S with type key = atom

(* Substitutions. *)

type subst

module Subst : sig
  val id: subst
  val apply: subst -> atom -> atom
  val extend: subst -> atom -> atom -> subst
  val is_fresh_for: atom -> subst -> bool
end
