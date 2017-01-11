let size (t : (_, _) Term.term) : int =
  let o = object
    inherit [_] Term.size as super
    method! visit_term env t =
      (* Every node counts 1 towards the size. *)
      1 + super#visit_term env t
  end in
  o # visit_term () t

let fa (t : Term.nominal_term) : Atom.Set.t =
  let accu = ref Atom.Set.empty in
  Term.Atom2Unit.visit_term (Abstraction.Atom2Unit.empty accu) t;
  !accu

(* TEMPORARY some of the following functions are restricted to closed
   terms, and should not be *)

let atom2debruijn (t : Term.nominal_term) : Term.db_term =
  Term.Atom2DeBruijn.visit_term Abstraction.Atom2DeBruijn.empty t

let string2atom (t : Term.raw_term) : Term.nominal_term =
  Term.String2Atom.visit_term Abstraction.String2Atom.empty t

let atom2string (t : Term.nominal_term) : Term.raw_term =
  Term.Atom2String.visit_term Abstraction.Atom2String.empty t

let subst (sigma : Atom.subst) (t : Term.nominal_term) : Term.nominal_term =
  Term.Atom2Atom.visit_term sigma t

let copy (t : Term.nominal_term) : Term.nominal_term =
  Term.Copy.visit_term Abstraction.Copy.empty t
