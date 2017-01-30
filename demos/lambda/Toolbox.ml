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

(* [import] converts a raw term to a nominal term that satisfies the Global
   Uniqueness Hypothesis, that is, a nominal term where every binding name
   occurrence is represented by a unique atom. [import] expects every free
   name occurrence to be in the domain of [env]. If that is not the case,
   the exception [Unbound] is raised. *)

(* TEMPORARY use string * loc so as to be able to give a location *)

(* TEMPORARY maybe [module Import = KitImport] so that the user does not
   have to know about the kits at all. *)

exception Unbound = KitImport.Unbound

class ['self] import = object (_ : 'self)
  inherit [_] map
  inherit [_] KitImport.map
end

let import : KitImport.env -> raw_term -> nominal_term =
  new import # visit_term

(* -------------------------------------------------------------------------- *)

(* [export] converts a nominal term to a raw term, in a hygienic manner. This
   is the proper way of displaying a term. [export] expects every free name
   occurrence to be in the domain of [env]. *)

class ['self] export = object (_ : 'self)
  inherit [_] map
  inherit [_] KitExport.map
end

let export : KitExport.env -> nominal_term -> raw_term =
  new export # visit_term

(* -------------------------------------------------------------------------- *)

(* [fa] computes the free atoms of a term. *)

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

(* -------------------------------------------------------------------------- *)

(* Conversions from raw terms and from nominal terms to de Bruijn terms. *)

class raw2debruijn = object
  inherit [_] map
  inherit [_] KitToDeBruijn.String.map
end

let raw2debruijn : KitToDeBruijn.String.env -> raw_term -> debruijn_term =
  new raw2debruijn # visit_term

class nominal2debruijn = object
  inherit [_] map
  inherit [_] KitToDeBruijn.Atom.map
end

let nominal2debruijn : KitToDeBruijn.Atom.env -> nominal_term -> debruijn_term =
  new nominal2debruijn # visit_term

(* -------------------------------------------------------------------------- *)

(* [subst] applies a substitution to a nominal term, yielding a nominal term. *)

(* A substitution is a finite map of atoms to nominal terms. *)

type substitution =
  nominal_term Atom.Map.t

(* When applying a substitution [sigma] to a term [t], the GUH guarantees that
   the free atoms of (the codomain of) [sigma] cannot be captured by a binder
   within [t]. The GUH also guarantees that a binder within [t] cannot appear
   in the domain of [sigma], which means that we can go down into [t] and apply
   [sigma] to every variable. *)

(* The GUH is preserved by copying the terms that are grafted into [t]. Thus,
   it is not even necessary that [sigma] and [t] be disjoint, or that the
   terms in the codomain of [sigma] be pairwise disjoint. One should note,
   however, that the result of the substitution is not disjoint with [t], so
   one should no longer use [t] after the substitution (or, one should apply
   the substitution to a copy). *)

class subst = object
  inherit [_] endo (* we could also use [map] *)
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

let subst : substitution -> nominal_term -> nominal_term =
  new subst # visit_term

let subst1 u x t =
  subst (Atom.Map.singleton x u) t

(* -------------------------------------------------------------------------- *)

(* [equiv] tests whether two terms are alpha-equivalent. *)

class equiv = object
  inherit [_] iter2
  inherit [_] KitEquiv.iter2
end

let equiv : nominal_term -> nominal_term -> bool =
  VisitorsRuntime.wrap2 (new equiv # visit_term KitEquiv.empty)

(* -------------------------------------------------------------------------- *)

(* [ba] computes the set of bound atoms of a term and (at the same time)
   checks that this term is well-formed, that is, no atom is bound twice. The
   exception [IllFormed x] is raised if the atom [x] occurs twice in a binding
   position. *)

exception IllFormed = KitBa.IllFormed

class ['self] ba = object (_ : 'self)
  inherit [_] reduce
  inherit [_] KitBa.reduce
end

let ba : nominal_term -> Atom.Set.t =
  new ba # visit_term ()

(* [wf t] checks whether the term [t] is well-formed, and returns no result.
   The exception [IllFormed x] is raised if the atom [x] occurs twice in a
   binding position.*)

let wf t =
  let (_ : Atom.Set.t) = ba t in ()

(* -------------------------------------------------------------------------- *)

(* [filter p t] returns a free atom of the term [t] that satisfies the
   predicate [p], if such an atom exists. *)

(* [pick_fa t] returns a free atom of the term [t], if there is one. *)

(* [closed t] tests whether the term [t] is closed, i.e., has no free atom. *)

let filter p : nominal_term -> Atom.t option =
  let filter = object
    inherit [_] iter
    inherit KitFa.filter p
  end in
  KitFa.wrap (filter # visit_term KitFa.empty)

let pick_fa : nominal_term -> Atom.t option =
  filter (fun _ -> true)

let closed (t : nominal_term) : bool =
  match pick_fa t with None -> true | Some _ -> false

(* -------------------------------------------------------------------------- *)

end
