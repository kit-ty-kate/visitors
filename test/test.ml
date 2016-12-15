module Test0 = struct

  type u = Uber
   and point = u * u
    [@@deriving visitors { max = 3 }]

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
    method visit_int env x = Printf.printf "(env=%d) int: %d\n%!" env x
    method visit_name env x = Printf.printf "(env=%d) name: %s\n%!" env x
    method visit_binder env x = Printf.printf "(env=%d) binder: %s\n%!" env x

  end

  let identity : term =
    TLambda ("x", TVar "x")

  let () =
    iter#visit_term 33 identity

  let map = object
    inherit [_, unit] map
    (* Descending methods for nonlocal types. *)
    method visit_int () x = x
    method visit_name () x = "a" (* change name occurrences to [a], for fun *)
    method visit_binder () x = x
  end

  let () =
    iter#visit_term 33 (map#visit_term () identity)

end

module Test3 = struct

  type ('var, 'binder) term =
    | TVar of 'var
    | TAbs of 'binder * ('var, 'binder) term
    | TApp of ('var, 'binder) term * ('var, 'binder) term
  [@@deriving visitors]

  (* Nominal. *)

  module StringSet = Set.Make(String)

  let iter = object(self)
    inherit [_, _] iter
    (* Descending methods for local types. *)
    method! match_TAbs env x t =
      let env = StringSet.add x env in
      self#visit_term env t
    (* Descending methods for nonlocal types. *)
    method visit_'binder env x = ()
    method visit_'var env x =
      if StringSet.mem x env then
        Printf.printf "%s is a bound variable.\n%!" x
      else
        Printf.printf "%s is a free variable.\n%!" x

  end

  let t : (_, _) term =
    TAbs ("x", TApp (TVar "x", TVar "y"))

  let () =
    iter#visit_term StringSet.empty t

  (* De Bruijn. *)

  let iter = object(self)
    inherit [_, _] iter
    (* Descending methods for local types. *)
    method! match_TAbs env x t =
      let env = 1 + env in
      self#visit_term env t
    (* Descending methods for nonlocal types. *)
    method visit_'binder env x = ()
    method visit_'var env x =
      if x < env then
        Printf.printf "%d is a bound variable.\n%!" x
      else
        Printf.printf "%d is a free variable.\n%!" x

  end

  let t : (_, _) term =
    TAbs ((), TApp (TVar 0, TVar 1))

  let () =
    iter#visit_term 0 t

end
