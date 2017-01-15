(* This kit serves to construct a [subst] function for terms -- a function
   that substitutes atoms for atoms. *)

(* An environment is a substitution [sigma]. We require every binder [x]
   encountered along the way to be fresh with respect to [sigma]. *)

type env = Atom.subst

let extend x sigma =
  (* Under the global uniqueness assumption, the atom [x] cannot appear
     in the domain or codomain of the substitution [sigma]. We check at
     runtime that this is the case. *)
  assert (Atom.Subst.is_fresh_for x sigma);
  (* Since [x] is fresh for [sigma], no capture is possible. Thus, no
     freshening of the bound name is required. Thus, we can keep the
     substitution [sigma], unchanged, under the binder. *)
  (* One might wish to extend [sigma] with a mapping of [x] to [x], so
     that [x] is not fresh for the extended [sigma], so that crossing
     another binding occurrence of [x] causes the above assertion to fail.
     That said, in principle, the global uniqueness assumption guarantees
     that we cannot encounter another binding occurrence of [x]. So, it
     seems preferable not to pay. The well-formedness of terms can be
     checked independently. *)
  x, sigma

class ['self] map = object (_ : 'self)
  method private extend = extend
  method private visit_'fn = Atom.Subst.apply
end

(* TEMPORARY 1. do we need a [subst] function at all?
             2. could we abandon the runtime check and use [KitTrivial] instead? *)
