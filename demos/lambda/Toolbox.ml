let size (t : (_, _) Term.term) : int =
  let o = object
    inherit [_] Term.size as super
    method! visit_term env t =
      (* Every node counts 1 towards the size. *)
      1 + super#visit_term env t
  end in
  o # visit_term () t

let show (t : Term.nominal_term) : Term.raw_term =
  Term.Show.visit_term () t

let fa (t : Term.nominal_term) : Atom.Set.t =
  let accu = ref Atom.Set.empty in
  Term.Atom2Unit.visit_term (Abstraction.Atom2Unit.empty accu) t;
  !accu

let fa' (t : Term.nominal_term) : Atom.Set.t =
  Term.Fa.visit_term () t

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

type substitution =
  Term.nominal_term Atom.Map.t

let substitute (sigma : substitution) (t : Term.nominal_term) =
  let v = object
    inherit [_] Term.atom2Something
    method! visit_TVar _sigma x =
      (* This [sigma] is in fact the same as the [sigma] we started with,
         since the substitution is not modified in any way when a binder
         is entered. *)
      assert (sigma == _sigma);
      match Atom.Map.find x sigma with
      | u ->
          (* Do not forget to copy the term that is being grafted, so as
             to maintain the GUH. *)
          copy u
      | exception Not_found ->
          (* [x] lies outside the domain of [sigma]. *)
          Term.TVar x
  end in
  v # visit_term sigma t

let substitute1 u x t =
  substitute (Atom.Map.singleton x u) t

(* In [substitute], the precondition is [sigma * t]
   and the postcondition is [sigma * \result].
   The caller loses the permission to use [t]. *)
(* One could design other variants, e.g. one where the caller
   loses [sigma], so copying is not needed when a variable [x]
   is encountered for the first time. In that case, we need
   either a static affinity hypothesis (each variable in the
   domain of [sigma] occurs at most once in [t]) or a dynamic
   occurrence counting mechanism. *)

let equiv (t1 : Term.nominal_term) (t2 : Term.nominal_term) : bool =
  try
    Term.Equiv.visit_term Abstraction.Equiv.empty t1 t2;
    true
  with VisitorsRuntime.StructuralMismatch ->
    false

let wf (t : Term.nominal_term) : bool =
  try
    Term.Wf.visit_term Abstraction.Wf.empty t;
    true
  with VisitorsRuntime.StructuralMismatch ->
    false
