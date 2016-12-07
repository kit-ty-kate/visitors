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

  let iter = object
    inherit [_, int] visitors

    (* Descending methods for nonlocal types. *)
    method int env x = Printf.printf "(env=%d) int: %d\n%!" env x
    method name env x = Printf.printf "(env=%d) name: %s\n%!" env x
    method binder env x = Printf.printf "(env=%d) binder: %s\n%!" env x

    (* Ascending methods for data constructors. *)
    method buildTUnit = ()
    method buildTIntLiteral _ = ()
    method buildTVar _ = ()
    method buildTLambda _ _ = ()
    method buildTApp _ _ = ()
    method buildTPair _ _ = ()
    method buildTTuple _ = ()
    method buildTLNil = ()
    method buildTLCons _ = ()
    method buildtuple _ _ = ()

  end

  let identity : term =
    TLambda ("x", TVar "x")

  let () =
    iter#term 33 identity

end
