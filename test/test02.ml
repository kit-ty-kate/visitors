type term =
  | TUnit
  | TIntLiteral of int
  | TVar of string
  | TLambda of string * term
  | TApp of term * term
  | TPair of { fst: term; snd: term }
  | TTuple of term_list

and term_list =
  | TLNil
  | TLCons of (term * term_list)

[@@deriving
     visitors { name = "iter"; variety = "iter" },
     visitors { name = "map"; variety = "map" },
     visitors { name = "iter2"; variety = "iter2" },
     visitors { name = "map2"; variety = "map2" }
]

let identity : term =
  TLambda ("x", TVar "x")

let () =
  new iter#visit_term 33 identity

let () =
  new iter#visit_term 33 (new map#visit_term () identity)
