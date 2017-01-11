(* TEMPORARY document; change the name of this file, as it offers not just
   the type [abstraction], but also ready-made traversal kits which deal
   with free names and abstractions. *)

type void

type ('bn, 'term) abstraction =
  'bn * 'term

module Bn : sig
  val iter: void -> void -> 'a
  val map: void -> void -> 'a
  val reduce: void -> void -> 'a
  val iter2: void -> void -> void -> 'a
  val map2: void -> void -> void -> 'a
  val reduce2: void -> void -> void -> 'a
end

module Invisible : sig
  module Abstraction : sig
    val iter: _ ->
      ('env -> 'term -> unit) ->
      'env -> ('bn, 'term) abstraction -> unit
    val map: _ ->
      ('env -> 'term1 -> 'term2) ->
      'env -> ('bn, 'term1) abstraction -> ('bn, 'term2) abstraction
    val reduce: _ ->
      ('env -> 'term -> 'a) ->
      'env -> ('bn, 'term) abstraction -> 'a
    val iter2: _ ->
      ('env -> 'term1 -> 'term2 -> unit) ->
      'env -> ('bn1, 'term1) abstraction -> ('bn2, 'term2) abstraction -> unit
    val reduce2: _ ->
      ('env -> 'term1 -> 'term2 -> 'a) ->
      'env -> ('bn1, 'term1) abstraction -> ('bn2, 'term2) abstraction -> 'a
  end
end

module Size : sig

  type env = unit

  module Abstraction : sig
    val reduce:
      _ ->
      (env -> 'term -> int) ->
      env -> (_, 'term) abstraction -> int
  end

  module Fn : sig
    val reduce: env -> _ -> int
  end

end

module Show : sig

  type env = unit

  module Abstraction : sig
    val map:
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> (Atom.t, 'term1) abstraction -> (string, 'term2) abstraction
  end

  module Fn : sig
    val map: env -> Atom.t -> string
  end

end

module String2Atom : sig

  type env

  val empty: env

  module Abstraction : sig
    val map:
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> (string, 'term1) abstraction -> (Atom.t, 'term2) abstraction
  end

  exception Unbound of string

  module Fn : sig
    val map: env -> string -> Atom.t
  end

end

module Atom2String : sig

  type env

  val empty: env

  module Abstraction : sig
    val map:
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> (Atom.t, 'term1) abstraction -> (string, 'term2) abstraction
  end

  module Fn : sig
    val map: env -> Atom.t -> string
  end

end

module Atom2Unit : sig

  type env

  val empty: Atom.Set.t ref -> env

  module Abstraction : sig
    val iter :
      _ ->
      (env -> 'term -> unit) ->
      env -> (Atom.t, 'term) abstraction -> unit
  end

  module Fn : sig
    val iter: env -> Atom.t -> unit
  end

end

module Fa : sig

  type env = unit

  module Abstraction : sig
    val reduce:
      _ ->
      (env -> 'term -> Atom.Set.t) ->
      env -> (Atom.t, 'term) abstraction -> Atom.Set.t
  end

  module Fn : sig
    val reduce: env -> Atom.t -> Atom.Set.t
  end

end

module Atom2DeBruijn : sig

  type env

  val empty: env

  module Abstraction : sig
    val map :
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> (Atom.t, 'term1) abstraction -> (unit, 'term2) abstraction
  end

  module Fn : sig
    val map: env -> Atom.t -> int
  end

end

module Atom2Atom : sig

  type env = Atom.subst

  module Abstraction : sig
    val map:
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> (Atom.t, 'term1) abstraction -> (Atom.t, 'term2) abstraction
  end

  module Fn : sig
    val map: env -> Atom.t -> Atom.t
  end

end

module Atom2Something : sig

  type 'term env =
    'term Atom.Map.t

  module Abstraction : sig
    val map:
      _ ->
      ('term env -> 'term1 -> 'term2) ->
      'term env -> (Atom.t, 'term1) abstraction -> (Atom.t, 'term2) abstraction
  end

  module Fn : sig
    val map: 'term env -> Atom.t -> Atom.t
  end

end

module Copy : sig

  type env

  val empty: env

  module Abstraction : sig
    val map:
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> (Atom.t, 'term1) abstraction -> (Atom.t, 'term2) abstraction
  end

  module Fn : sig
    val map: env -> Atom.t -> Atom.t
  end

end

module Equiv : sig

  type env

  val empty: env

  module Abstraction : sig
    val iter2:
      _ ->
      (env -> 'term -> 'term -> unit) ->
      env -> (Atom.t, 'term) abstraction -> (Atom.t, 'term) abstraction -> unit
  end

  module Fn : sig
    val iter2:
      env -> Atom.t -> Atom.t -> unit
  end

end

module Wf : sig

  type env

  val empty: env

  module Abstraction : sig
    val iter:
      _ ->
      (env -> 'term -> unit) ->
      env -> (Atom.t, 'term) abstraction -> unit
  end

  module Fn : sig
    val iter: env -> Atom.t -> unit
  end

end
