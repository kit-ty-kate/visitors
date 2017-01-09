type ('bn, 'term) abstraction =
  'bn * 'term

module String2Atom : sig
  type env = Atom.t StringMap.t
  module Abstraction : sig
    val map:
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> string * 'term1 -> Atom.t * 'term2
  end
end

module Atom2Unit : sig
  type env = Atom.Set.t
  module Abstraction : sig
    val iter :
      _ ->
      (env -> 'term -> unit) ->
      env -> Atom.t * 'term -> unit
  end
end

module Atom2DeBruijn : sig
  type env = int Atom.Map.t * int (* TEMPORARY *)
  module Abstraction : sig
    val map :
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> Atom.t * 'term1 -> unit * 'term2
  end
end

(* TEMPORARY clean up *)
type void
class ['a] visit_'bn :
  object ('a)
    constraint 'a = < visit_'bn : void -> void -> 'b; .. >
    method visit_'bn : void -> void -> 'b
  end
class fv :
  object
    val mutable accu : Atom.Set.t
    method accu : Atom.Set.t
    method visit_'fn : Atom.Set.t -> Atom.Set.elt -> unit
  end
class n2db :
  object
    method visit_'bn : Atom2DeBruijn.env -> void -> unit
    method visit_'fn : int Atom.Map.t * int -> Atom.Map.key -> int
  end
