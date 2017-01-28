(* -------------------------------------------------------------------------- *)

(* This functor is applied to a type of terms, equipped with visitor classes.
   It produces a toolbox of useful functions that operate on terms. *)

module Make (Term : sig

  (* Suppose there is a type of terms, which is parameterized over the
     representations of free name occurrences and binding name occurrences. *)

  type ('fn, 'bn) term

  (* Suppose the type of terms is equipped with the following visitors. *)

  (* The private virtual method [visit_'fn] is used to specify what should
     be done at free name occurrences. The private virtual method [extend]
     is used to indicate how the environment should be extended when an
     abstraction is entered. In the [reduce] visitor, the private methods
     [zero] and [plus] are used to specify how summaries should be computed,
     while the private method [restrict] is used to specify how a summary
     should be restricted when an abstraction is exited. *)

  (* Suppose the data constructor for variables is named [TVar], so that
     the method [visit_TVar] is used to specify what behavior at variables
     is desired. *)

  class virtual ['self] iter : object ('self)
    method private virtual extend : 'bn -> 'env -> 'env
    method private virtual visit_'fn : 'env -> 'fn -> _
    method visit_term : 'env -> ('fn, 'bn) term -> unit
  end

  class virtual ['self] map : object ('self)
    method private virtual extend : 'bn1 -> 'env -> 'bn2 * 'env
    method private virtual visit_'fn : 'env -> 'fn1 -> 'fn2
    method visit_term : 'env -> ('fn1, 'bn1) term -> ('fn2, 'bn2) term
    method private visit_TVar : 'env -> 'fn1 -> ('fn2, 'bn2) term
  end

  class virtual ['self] endo : object ('self)
    method private virtual extend : 'bn -> 'env -> 'bn * 'env
    method private virtual visit_'fn : 'env -> 'fn -> 'fn
    method visit_term : 'env -> ('fn, 'bn) term -> ('fn, 'bn) term
    method private visit_TVar : 'env -> ('fn, 'bn) term -> 'fn -> ('fn, 'bn) term
  end

  class virtual ['self] reduce : object ('self)
    method private virtual extend : 'bn -> 'env -> 'env
    method private virtual visit_'fn : 'env -> 'fn -> 'z
    method private virtual zero : 'z
    method private virtual plus : 'z -> 'z -> 'z
    method private virtual restrict : 'bn -> 'z -> 'z
    method visit_term : 'env -> ('fn, 'bn) term -> 'z
  end

  class virtual ['self] iter2 : object ('self)
    method private virtual extend : 'bn1 -> 'bn2 -> 'env -> 'env
    method private virtual visit_'fn : 'env -> 'fn1 -> 'fn2 -> _
    method visit_term : 'env -> ('fn1, 'bn1) term -> ('fn2, 'bn2) term -> unit
  end

end) = struct

open Term

(* -------------------------------------------------------------------------- *)

(* A raw term is one where every name is represented as a string. This form is
   typically produced by a parser, and consumed by a printer. It is not used
   internally. *)

type raw_term =
  (string, string) term

(* A nominal term is one where every name is represented as an atom. Although
   this is not visible in this type definition, we may additionally impose a
   Global Uniqueness Hypothesis (GUH), that is, we may require every binding
   name occurrence to carry a distinct atom. *)

type nominal_term =
  (Atom.t, Atom.t) term

(* A de Bruijn term is one where free name occurrences are represented as a
   de Bruijn index and binding name occurrences carry no information. This
   definition is used for illustrative purposes only. *)

type debruijn_term =
  (int, unit) term

(* -------------------------------------------------------------------------- *)

(* [size] computes the size of its argument. *)

class ['self] size = object (_ : 'self)
  inherit [_] reduce as super
  inherit [_] KitTrivial.reduce
  inherit [_] VisitorsRuntime.addition_monoid
  method! visit_term env t =
    1 + super#visit_term env t
end

let size : 'fn 'bn . ('fn, 'bn) term -> int =
  fun t -> new size # visit_term () t

(* -------------------------------------------------------------------------- *)

(* [show] converts its argument to a raw term, in a NONHYGIENIC manner, using
   [Atom.show] both at free name occurrences and bound name occurrences. It
   is a debugging tool. *)

class ['self] show = object (_ : 'self)
  inherit [_] map
  inherit [_] KitShow.map
end

let show : nominal_term -> raw_term =
  new show # visit_term ()

(* -------------------------------------------------------------------------- *)

(* [copy] returns a copy of its argument where every bound name has been
   replaced with a fresh copy, and every free name is unchanged. *)

class ['self] copy = object (_ : 'self)
  inherit [_] map
  inherit [_] KitCopy.map
end

let copy : nominal_term -> nominal_term =
  new copy # visit_term KitCopy.empty

(* -------------------------------------------------------------------------- *)

(* TEMPORARY some of the following functions are restricted to closed
   terms, and should not be. *)

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

let raw2debruijn : raw_term -> debruijn_term =
  new raw2debruijn # visit_term KitToDeBruijn.String.empty

class nominal2debruijn = object
  inherit [_] map
  inherit [_] KitToDeBruijn.Atom.map
end

let nominal2debruijn : nominal_term -> debruijn_term =
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
  VisitorsRuntime.wrap2 (new equiv # visit_term KitEquiv.empty)

class wf = object
  inherit [_] iter
  inherit [_] KitWf.iter
end

let wf : nominal_term -> bool =
  VisitorsRuntime.wrap (new wf # visit_term KitWf.empty)

end
