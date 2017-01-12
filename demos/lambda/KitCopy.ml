(* This kit serves to construct a [copy] function for terms. *)

type env =
  Atom.subst

let empty =
    Atom.Subst.id

(* TEMPORARY move this function to [Atom.Subst]? *)
let extend x sigma =
  (* Under the global uniqueness assumption, the atom [x] cannot appear
     in the domain or codomain of the substitution [sigma]. We check at
     runtime that this is the case. *)
  assert (Atom.Subst.is_fresh_for x sigma);
  (* Generate a fresh copy of [x]. *)
  let x' = Atom.fresha x in
  (* Extend [sigma] when descending in the body. *)
  let sigma = Atom.Subst.extend sigma x x' in
  x', sigma

class ['self] map = object (_ : 'self)
  method extend = extend
  method visit_'fn = Atom.Subst.apply
end
