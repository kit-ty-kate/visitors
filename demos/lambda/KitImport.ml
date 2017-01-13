(* This kit serves to construct an [import] function for terms, that is, a
   function that transforms strings to atoms. *)

(* We impose the GUH by mapping each binding occurrence to a fresh atom. *)

module StringMap =
  Map.Make(String)

type env =
  Atom.t StringMap.t

let empty =
  StringMap.empty

let extend (x : string) (env : env) : Atom.t * env =
  let a = Atom.freshh x in
  let env = StringMap.add x a env in
  a, env

exception Unbound of string

let lookup (env : env) (x : string) : Atom.t =
  try
    StringMap.find x env
  with Not_found ->
    raise (Unbound x)

class ['self] map = object (_ : 'self)
  method extend = extend
  method visit_'fn = lookup
end
