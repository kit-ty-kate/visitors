type void

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

  class fa : object
    val mutable accu : env
    method accu : env
    method visit_'fn : env -> Atom.t -> unit
    method visit_'bn : void -> void -> unit
  end

end

module Atom2DeBruijn : sig
  type env = int Atom.Map.t * int (* TEMPORARY abstract *)
  module Abstraction : sig
    val map :
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> Atom.t * 'term1 -> unit * 'term2
  end
end

(* TEMPORARY clean up *)

class n2db :
  object
    method visit_'fn : int Atom.Map.t * int -> Atom.Map.key -> int
    method visit_'bn : void -> void -> unit
  end
