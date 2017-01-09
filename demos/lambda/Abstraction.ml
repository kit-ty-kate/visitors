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
  module Abstraction = struct
    let map _ f (env : env) (s, body) =
      let a = Atom.freshh s in
      let env = StringMap.add s a env in
      a, f env body
  end
end

(* -------------------------------------------------------------------------- *)

(* During a free atom computation, the environment is a set of atoms. *)

module Atom2Unit = struct

  type env = Atom.Set.t

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
      assert false (* never invoked *)
  end

end

(* -------------------------------------------------------------------------- *)

(* During a conversion of nominal style to de Bruijn style, the environment is
   a pair of a map [m] of atoms to de Bruijn levels and a current de Bruijn
   level [n]. *)

module Atom2DeBruijn = struct
  type env = int Atom.Map.t * int
  module Abstraction = struct
    let map _ f (m, n : env) (x, body) =
      let m = Atom.Map.add x n m in
      (), f (m, n+1) body
  end
end

(* -------------------------------------------------------------------------- *)

class n2db = object
  method visit_'fn (env, n) x =
    let level = Atom.Map.find x env in
    n - level
  method visit_'bn (_ : void) (_ : void) : unit =
    assert false (* never invoked *)
end
