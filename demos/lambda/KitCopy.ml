(* This kit serves to construct a [copy] function for terms. *)

(* An environment maps atoms to atoms. *)

type env =
  Atom.atom Atom.Map.t

let empty =
  Atom.Map.empty

(* TEMPORARY move these functions to [Atom]? *)

let lookup env x =
  try
    Atom.Map.find x env
  with Not_found ->
    (* Outside of its domain, the renaming acts as the identity. *)
    x

let extend x env =
  (* Under the global uniqueness assumption, the atom [x] cannot appear in the
     domain or codomain of the environment. We check at runtime that this is
     the case; however, only the domain check can be efficiently implemented. *)
  assert (not (Atom.Map.mem x env));
  (* Generate a fresh copy of [x]. *)
  let x' = Atom.fresha x in
  (* Extend [env] when descending in the body. *)
  x', Atom.Map.add x x' env

class ['self] map = object (_ : 'self)
  method private extend = extend
  method private visit_'fn = lookup
end
