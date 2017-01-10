open Abstraction

type ('fn, 'bn) term =
  | TVar of 'fn
  | TLambda of ('bn, ('fn, 'bn) term) abstraction
  | TApp of ('fn, 'bn) term * ('fn, 'bn) term
  [@@deriving
    visitors { name = "atom2unit"; variety = "iter"; path = ["Atom2Unit"]; freeze = ["bn"; "fn"]; final = true },
    visitors { name = "atom2bruijn"; variety = "map"; path = ["Atom2DeBruijn"]; freeze = ["bn"; "fn"]; final = true },
    visitors { name = "string2atom"; variety = "map"; path = ["String2Atom"]; freeze = ["bn"; "fn"]; final = true },
    visitors { name = "atom2atom"; variety = "map"; path = ["Atom2Atom"]; freeze = ["bn"; "fn"]; final = true }
  ]

type raw_term =
  (string, string) term

type nominal_term =
  (Atom.t, Atom.t) term

type db_term =
  (int, unit) term

let fa (t : nominal_term) : Atom.Set.t =
  let accu = ref Atom.Set.empty in
  Atom2unit.visit_term (Atom2Unit.empty accu) t;
  !accu

let atom2debruijn (t : nominal_term) : db_term =
  Atom2bruijn.visit_term Atom2DeBruijn.empty t

let string2atom (t : raw_term) : nominal_term =
  String2atom.visit_term String2Atom.empty t

let subst (sigma : Atom.subst) (t : nominal_term) : nominal_term =
  Atom2atom.visit_term sigma t

let x = Atom.freshh "x"

let identity =
  TLambda (x, TVar x)

let y =
  TVar (Atom.freshh "y")

let idy =
  TApp (identity, y)

let () =
  print_endline "fa(\\x.x):";
  print_endline (Atom.Set.print (fa identity));
  print_endline "fa(y):";
  print_endline (Atom.Set.print (fa y));
  print_endline "fa((\\x.x) y):";
  print_endline (Atom.Set.print (fa idy))

(* TODO:

capture-avoiding substitution with freshening (nominal w/o GUH)
substitution of terms for variables (must copy term when grafting)
linear substitution of term for variable (w/o copying; check linearity at runtime?)
alpha-equivalence test (nominal)
test x \in fv(t) (nominal)
entering a binder (and testing for global uniqueness)
simultaneous opening? (nominal)
printing (conversion from nominal back to raw)
copying (generating fresh bound names to maintain global uniqueness)
well-formedness checking (just traversing the term and checking for global uniqueness)
-- test for global uniqueness everywhere an environment is extended

implement a term printer

 *)
