module Test0 = struct

  type u = Uber
   and point = u * u
    [@@deriving visitors]

end

module Test1 = struct

  type point =
    { x: int; y: int; mutable color: bool }
    [@@deriving visitors]

end

module Test2 = struct

  type name =
    string

  type binder =
    string

  type term =
    | TUnit
    | TIntLiteral of int
    | TVar of name
    | TLambda of binder * term
    | TApp of term * term
    | TPair of { fst: term; snd: term }
    | TTuple of term_list

  and term_list =
    | TLNil
    | TLCons of (term * term_list)
    [@@deriving visitors]

  class virtual ['self, 'env] iter = object
    inherit ['self, 'env] visitor
    (* Ascending methods for data constructors. *)
    method build_TUnit = ()
    method build_TIntLiteral _ = ()
    method build_TVar _ = ()
    method build_TLambda _ _ = ()
    method build_TApp _ _ = ()
    method build_TPair _ _ = ()
    method build_TTuple _ = ()
    method build_TLNil = ()
    method build_TLCons _ = ()
  end

  class virtual ['self, 'env] map = object
    inherit ['self, 'env] visitor
    (* Ascending methods for data constructors. *)
    method build_TUnit = TUnit
    method build_TIntLiteral x = TIntLiteral x
    method build_TVar x = TVar x
    method build_TLambda x t = TLambda (x, t)
    method build_TApp t u = TApp (t, u)
    method build_TPair t u = TPair { fst = t; snd = u }
    method build_TTuple ts = TTuple ts
    method build_TLNil = TLNil
    method build_TLCons x = TLCons x
  end

  let iter = object
    inherit [_, int] iter
    (* Descending methods for nonlocal types. *)
    method int env x = Printf.printf "(env=%d) int: %d\n%!" env x
    method name env x = Printf.printf "(env=%d) name: %s\n%!" env x
    method binder env x = Printf.printf "(env=%d) binder: %s\n%!" env x

  end

  let identity : term =
    TLambda ("x", TVar "x")

  let () =
    iter#term 33 identity

  let map = object
    inherit [_, unit] map
    (* Descending methods for nonlocal types. *)
    method int () x = x
    method name () x = "a" (* change name occurrences to [a], for fun *)
    method binder () x = x
  end

  let () =
    iter#term 33 (map#term () identity)

end
