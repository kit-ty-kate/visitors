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
