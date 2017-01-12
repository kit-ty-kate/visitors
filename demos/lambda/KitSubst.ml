(* This kit serves to construct a [subst] function for terms -- a function
   that substitutes things (possibly terms) for atoms. *)

(* An environment is a map [sigma] of atoms to things. We require every binder
   [x] encountered along the way to be fresh with respect to [sigma]. *)

type 'term env =
  'term Atom.Map.t

let extend x sigma =
  (* We would like to check that [x] is fresh for [sigma], but can only
     perform the domain check. The codomain check cannot be performed
     since the type of things is abstract here. *)
  assert (not (Atom.Map.mem x sigma));
  (* Since [x] is fresh for [sigma], no capture is possible. Thus, no
     freshening of the bound name is required. Thus, we can keep the
     substitution [sigma], unchanged, under the binder. *)
  x, sigma

class ['self] map = object (_ : 'self)
  method extend = extend
  (* [visit_'fn] is not implemented, as it is up to the user to identify
     variable nodes and replace them. *)
  method visit_'fn _sigma _x = assert false
end

(* TEMPORARY could we abandon the runtime check and use [KitTrivial]
   instead? *)
