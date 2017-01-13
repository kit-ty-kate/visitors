(* This kit serves to construct an [export] function for terms, that is, a
   function that transforms atoms back to strings. *)

(* The environment is an injective mapping of atoms to strings. We keep track
   of its codomain by recording a mapping of hints to integers. *)

(* TEMPORARY move the low-level code to [Atom]? *)

module StringMap =
  Map.Make(String)

type env = {
  graph: string Atom.Map.t;
  codomain: int StringMap.t;
}

let empty = {
  graph = Atom.Map.empty;
  codomain = StringMap.empty;
}

let next env hint : int =
  try
    StringMap.find hint env.codomain
  with Not_found ->
    0

let extend x env =
  (* Under the GUH, the atom [x] cannot appear in the domain of [env]. *)
  assert (not (Atom.Map.mem x env.graph));
  (* We must pick a suitable string to stand for the atom [x]. We must
     pick a string that does not appear in the image through [env] of
     the free atoms of [body]. However, at this point, we do not have
     access to the free atoms of [body], so we must pick a string [s]
     that does not appear in the codomain of [env]. *)
  let hint = Atom.hint x in
  let i = next env hint in
  let s =
    if i = 0 then hint (* a cosmetic detail *)
    else Printf.sprintf "%s%d" hint i
  in
  let env = {
    graph = Atom.Map.add x s env.graph;
    codomain = StringMap.add hint (i+1) env.codomain;
  } in
  s, env

let lookup env a =
  try
    Atom.Map.find a env.graph
  with Not_found ->
    (* The atom [a] must be in the domain of [env]. *)
    assert false

class ['self] map = object (_ : 'self)
  method extend = extend
  method visit_'fn = lookup
end

(* TEMPORARY can we precompute fa(every subterm) ahead of time and
   attach this info to binders, so that the free atoms are available when
   printing? *)
