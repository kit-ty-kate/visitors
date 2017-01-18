open Term

(* TEMPORARY some of the following functions are restricted to closed
   terms, and should not be. *)

(* TEMPORARY try to make this a functor wrt terms and their visitors. *)

class ['self] size = object (_ : 'self)
  inherit [_] reduce as super
  inherit [_] KitTrivial.reduce
  inherit [_] VisitorsRuntime.addition_monoid
  method! visit_term env t =
    1 + super#visit_term env t
end

let size : 'fn 'bn . ('fn, 'bn) term -> int =
  fun t ->
    new size # visit_term () t

class ['self] show = object (_ : 'self)
  inherit [_] map
  inherit [_] KitShow.map
end

let show : nominal_term -> raw_term =
  new show # visit_term ()

class ['self] copy = object (_ : 'self)
  inherit [_] map
  inherit [_] KitCopy.map
end

let copy : nominal_term -> nominal_term =
  new copy # visit_term KitCopy.empty

class ['self] import = object (_ : 'self)
  inherit [_] map
  inherit [_] KitImport.map
end

let import : KitImport.env -> raw_term -> nominal_term =
  new import # visit_term

class ['self] export = object (_ : 'self)
  inherit [_] map
  inherit [_] KitExport.map
end

let export : KitExport.env -> nominal_term -> raw_term =
  new export # visit_term

class fa = object
  inherit [_] iter
  inherit [_] KitFa.iter
end

let fa (t : nominal_term) : Atom.Set.t =
  let fa = new fa in
  fa # visit_term KitFa.empty t;
  fa # accu

class fa' = object
  inherit [_] reduce
  inherit [_] KitFa.reduce
end

let fa' : nominal_term -> Atom.Set.t =
  new fa' # visit_term ()

class raw2debruijn = object
  inherit [_] map
  inherit [_] KitToDeBruijn.String.map
end

let raw2debruijn : raw_term -> db_term =
  new raw2debruijn # visit_term KitToDeBruijn.String.empty

class nominal2debruijn = object
  inherit [_] map
  inherit [_] KitToDeBruijn.Atom.map
end

let nominal2debruijn : nominal_term -> db_term =
  new nominal2debruijn # visit_term KitToDeBruijn.Atom.empty

class subst_atom = object
  inherit [_] endo (* we could also use [map] *)
  inherit [_] KitSubstAtom.map
end

let subst_atom : Atom.subst -> nominal_term -> nominal_term =
  new subst_atom # visit_term

let subst_atom1 u x t =
  subst_atom (Atom.Subst.singleton x u) t

class subst = object
  inherit [_] endo
  inherit [_] KitSubst.map
  method! private visit_TVar sigma this x =
    match Atom.Map.find x sigma with
    | u ->
        (* Do not forget to copy the term that is being grafted, so as
           to maintain the GUH. *)
        copy u
    | exception Not_found ->
        assert (this = TVar x);
        this
end

type substitution =
  nominal_term Atom.Map.t

let subst : substitution -> nominal_term -> nominal_term =
  new subst # visit_term

let subst1 u x t =
  subst (Atom.Map.singleton x u) t

(* In [substitute], the precondition is [sigma * t]
   and the postcondition is [sigma * \result].
   The caller loses the permission to use [t]. *)
(* One could design other variants, e.g. one where the caller
   loses [sigma], so copying is not needed when a variable [x]
   is encountered for the first time. In that case, we need
   either a static affinity hypothesis (each variable in the
   domain of [sigma] occurs at most once in [t]) or a dynamic
   occurrence counting mechanism. *)

class equiv = object
  inherit [_] iter2
  inherit [_] KitEquiv.iter2
end

let equiv : nominal_term -> nominal_term -> bool =
  KitEquiv.wrap2 (new equiv # visit_term KitEquiv.empty)

class wf = object
  inherit [_] iter
  inherit [_] KitWf.iter
end

let wf : nominal_term -> bool =
  KitWf.wrap (new wf # visit_term KitWf.empty)
