open Abstraction

type ('fn, 'bn) term =
  | TVar of 'fn
  | TLambda of ('bn, ('fn, 'bn) term) abstraction
  | TApp of ('fn, 'bn) term * ('fn, 'bn) term

  [@@deriving

    visitors { name = "iter"; variety = "iter"; public = ["visit_term"];
               ancestors = ["Bn.iter"; "Abstraction.iter"] }
    ,
    visitors { name = "map"; variety = "map"; public = ["visit_term"];
               ancestors = ["Bn.map"; "Abstraction.map"] }
    ,
    visitors { name = "reduce"; variety = "reduce"; public = ["visit_term"];
               ancestors = ["Bn.reduce"; "Abstraction.reduce"] }
    ,
    visitors { name = "iter2"; variety = "iter2"; public = ["visit_term"];
               ancestors = ["Bn.iter2"; "Abstraction.iter2"] }

  ]

type raw_term =
  (string, string) term

type nominal_term =
  (Atom.t, Atom.t) term

type db_term =
  (int, unit) term
