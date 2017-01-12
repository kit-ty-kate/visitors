(* This kit serves to construct a conversion from a nominal representation to
   de Bruijn representation. *)

(* The type of atoms is irrelevant, as long it is equipped with a [Map] data
   structure. E.g., atoms could be atoms as provided by the module [Atom], or
   they could be strings. For this reason, we wrap the code in a functor. *)

module Make (Map : sig
  type key
  type 'a t
  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val find: key -> 'a t -> 'a
end) = struct

  (* The environment is a pair of a map [m] of atoms to de Bruijn levels
     and a current de Bruijn level [n]. The conversion of a de Bruijn level
     to a de Bruijn index is performed when the environment is looked up. *)

  type env =
    int Map.t * int

  let empty =
    Map.empty, 0

  let extend x (m, n : env) =
    (* Increment the current de Bruijn level [n]. *)
    let n = n + 1 in
    (* Record a mapping of the name [x] to the de Bruijn level [n],
       so if [x] was looked up right now, it would receive level [n],
       therefore index [0]. *)
    let m = Map.add x n m in
    (), (m, n)

  let lookup (m, n : env) x =
    try
      (* Lookup the de Bruijn level associated with [x]. *)
      let k = Map.find x m in
      (* Convert it to a de Bruijn index. *)
      n - k
    with Not_found ->
      (* The name [x] is unknown. This should not happen if the environment
         was properly set up. *)
      assert false

  class ['self] map = object (_ : 'self)
    method extend = extend
    method visit_'fn = lookup
  end

end

(* -------------------------------------------------------------------------- *)

(* Instantiate the functor for strings and for atoms. *)

module String =
  Make(StringMap)

module Atom =
  Make(Atom.Map)
