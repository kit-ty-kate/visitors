type void

type ('bn, 'term) abstraction =
  'bn * 'term

module Bn : sig
  val iter: void -> void -> 'a
  val map: void -> void -> 'a
  val iter2: void -> void -> void -> 'a
  val map2: void -> void -> void -> 'a
end

module String2Atom : sig

  type env

  val empty: env

  module Abstraction : sig
    val map:
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> string * 'term1 -> Atom.t * 'term2
  end

  exception Unbound of string

  class map : object
    method visit_'fn : env -> string -> Atom.t
  end

end

module Atom2Unit : sig

  type env

  val empty: env

  module Abstraction : sig
    val iter :
      _ ->
      (env -> 'term -> unit) ->
      env -> Atom.t * 'term -> unit
  end

  class fa : object
    method accu : Atom.Set.t
    method visit_'fn : env -> Atom.t -> unit
  end

end

module Atom2DeBruijn : sig

  type env

  val empty: env

  module Abstraction : sig
    val map :
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> Atom.t * 'term1 -> unit * 'term2
  end

  class map : object
    method visit_'fn : env -> Atom.t -> int
  end

end
