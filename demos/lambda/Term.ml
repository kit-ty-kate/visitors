open Abstraction

type ('fn, 'bn) term =
  | TVar of 'fn
  | TLambda of ('bn, ('fn, 'bn) term) abstraction
  | TApp of ('fn, 'bn) term * ('fn, 'bn) term

  [@@deriving

    visitors { name = "Fa";
               variety = "reduce"; monoid = "Atom.Set"; path = ["Fa"];
               freeze = ["bn"; "fn"]; final = true },

    visitors { name = "Show";
               variety = "map"; path = ["Show"];
               freeze = ["bn"; "fn"]; final = true },

    visitors { name = "size";
               variety = "reduce"; monoid = "Monoid.Sum"; path = ["Size"];
               freeze = ["bn"; "fn"]; final = false },

    visitors { name = "Atom2Unit";
               variety = "iter"; path = ["Atom2Unit"];
               freeze = ["bn"; "fn"]; final = true },

    visitors { name = "Atom2DeBruijn";
               variety = "map"; path = ["Atom2DeBruijn"];
               freeze = ["bn"; "fn"]; final = true },

    visitors { name = "String2Atom";
               variety = "map"; path = ["String2Atom"];
               freeze = ["bn"; "fn"]; final = true },

    visitors { name = "Atom2String";
               variety = "map"; path = ["Atom2String"];
               freeze = ["bn"; "fn"]; final = true },

    visitors { name = "Atom2Atom";
               variety = "map"; path = ["Atom2Atom"];
               freeze = ["bn"; "fn"]; final = true },

    visitors { name = "Copy";
               variety = "map"; path = ["Copy"];
               freeze = ["bn"; "fn"]; final = true },

    visitors { name = "atom2Something";
               variety = "map"; path = ["Atom2Something"];
               freeze = ["bn"; "fn"]; final = false },

    visitors { name = "Equiv";
               variety = "iter2"; path = ["Equiv"];
               freeze = ["bn"; "fn"]; final = true },

    visitors { name = "Wf";
               variety = "iter"; path = ["Wf"];
               freeze = ["bn"; "fn"]; final = true }

  ]

type raw_term =
  (string, string) term

type nominal_term =
  (Atom.t, Atom.t) term

type db_term =
  (int, unit) term
