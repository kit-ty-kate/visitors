module Test0 = struct

  type u = U
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
    inherit visitors
    method int x = Printf.printf "int: %d\n%!" x
    method name x = Printf.printf "name: %s\n%!" x
    method binder x = Printf.printf "binder: %s\n%!" x
  end

  let identity : term =
    TLambda ("x", TVar "x")

  let () =
    iter#term identity

end
