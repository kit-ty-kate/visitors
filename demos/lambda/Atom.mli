(* TEMPORARY document *)

(* Atoms. *)

type identifier =
    string

type atom

type t =
    atom

val identity: atom -> int
val hint: atom -> identifier
val print: atom -> string

(* Producing fresh atoms. *)

val freshh: identifier -> atom
val fresha: atom -> atom

(* Comparison of atoms. *)

val equal: atom -> atom -> bool
val compare: atom -> atom -> int
val hash: atom -> int

(* Sets. *)

module Set : sig
  include Set.S with type elt = atom
  val print: t -> string
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
