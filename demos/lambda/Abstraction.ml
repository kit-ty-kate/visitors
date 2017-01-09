type void

(* -------------------------------------------------------------------------- *)

(* A universal type of single-name abstractions. *)

(* We wish to represent all kinds of abstractions -- e.g. in nominal style,
   in de Bruijn style, etc. -- so we parameterize the abstraction over the
   type ['bn] of the bound name and over the type ['term] of the body. This
   makes this type definition almost trivial -- it is just a pair -- but it
   still serves as a syntactic marker of where abstractions are located. *)

type ('bn, 'term) abstraction =
  'bn * 'term

(* -------------------------------------------------------------------------- *)

(* The main effect of an abstraction is to cause the environment to be enriched
   when the abstraction is traversed. As different kinds of traversals maintain
   different types of environments, we offer a variety of functions that enrich
   an environment. These functions are ready for use in a visitor. *)

(* -------------------------------------------------------------------------- *)

(* During a conversion of strings to atoms, the environment maps strings to
   atoms. *)

(* Even though this is not necessary in principle, we create unique atoms
   right away, that is, we map each binding occurrence to a fresh atom. *)

module String2Atom = struct

  type env = Atom.t StringMap.t

  let empty =
    StringMap.empty

  module Abstraction = struct
    let map _ f (env : env) (s, body) =
      let a = Atom.freshh s in
      let env = StringMap.add s a env in
      a, f env body
  end

  exception Unbound of string

  class map = object

    method visit_'fn (env : env) x =
      try
        StringMap.find x env
      with Not_found ->
        raise (Unbound x)

    method visit_'bn (_ : void) (_ : void) : Atom.t =
      (* This method is never invoked. *)
      assert false

  end

end

(* -------------------------------------------------------------------------- *)

(* During a free atom computation, the environment is a set of atoms. *)

module Atom2Unit = struct

  type env = Atom.Set.t

  let empty =
    Atom.Set.empty

  module Abstraction = struct
    let iter _ f env (x, body) =
      let env = Atom.Set.add x env in
      f env body
  end

  class fa = object

    val mutable accu = Atom.Set.empty
    method accu = accu

    method visit_'fn env x =
      if not (Atom.Set.mem x env) then
        accu <- Atom.Set.add x accu

    method visit_'bn (_ : void) (_ : void) : unit =
      (* This method is never invoked. *)
      assert false

  end

end

(* -------------------------------------------------------------------------- *)

(* During a conversion of nominal style to de Bruijn style, the environment is
   a pair of a map [m] of atoms to de Bruijn levels and a current de Bruijn
   level [n]. *)

module Atom2DeBruijn = struct

  type env = int Atom.Map.t * int

  let empty =
    (Atom.Map.empty, 0)

  module Abstraction = struct
    let map _ f (m, n : env) (x, body) =
      (* Increment the current de Bruijn level [n]. *)
      let n = n + 1 in
      (* Record a mapping of the name [x] to the de Bruijn level [n],
         so if [x] was looked up right now, it would receive level [n],
         therefore index [0]. *)
      let m = Atom.Map.add x n m in
      (* Traverse the body in the extended environment [m, n]. *)
      (), f (m, n) body
  end

  class map = object

    method visit_'fn (env, n) x =
      try
        (* Lookup the de Bruijn level associated with [x]. *)
        let k = Atom.Map.find x env in
        (* Convert it to a de Bruijn index. *)
        n - k
      with Not_found ->
        (* The name [x] is unknown. This should not happen if the environment
           was properly set up. *)
        assert false

    method visit_'bn (_ : void) (_ : void) : unit =
      (* This method is never invoked. *)
      assert false

  end

end

(* -------------------------------------------------------------------------- *)
