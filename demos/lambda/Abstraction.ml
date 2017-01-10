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

(* The functions associated with the type variable ['bn] are never invoked. *)
(* TEMPORARY explain why *)

module Bn = struct
  let iter _env _x = assert false
  let map _env _x = assert false
  let iter2 _env _x1 _x2 = assert false
  let map2 _env _x1 _x2 = assert false
end

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

  module Fn = struct
    let map env x =
      try
        StringMap.find x env
      with Not_found ->
        raise (Unbound x)
  end

end

(* -------------------------------------------------------------------------- *)

(* During a free atom computation, the environment is a pair of a set of atoms
   (the atoms that are currently in scope) and a reference to a set of atoms
   (the free atoms that have been accumulated so far). *)

module Atom2Unit = struct

  type env = Atom.Set.t * Atom.Set.t ref

  let empty accu =
    Atom.Set.empty, accu

  module Abstraction = struct
    let iter _ f (env, accu) (x, body) =
      let env = Atom.Set.add x env in
      f (env, accu) body
  end

  module Fn = struct
    let iter (env, accu) x =
      if not (Atom.Set.mem x env) then
        accu := Atom.Set.add x !accu
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

  module Fn = struct
    let map (env, n) x =
      try
        (* Lookup the de Bruijn level associated with [x]. *)
        let k = Atom.Map.find x env in
        (* Convert it to a de Bruijn index. *)
        n - k
      with Not_found ->
        (* The name [x] is unknown. This should not happen if the environment
           was properly set up. *)
        assert false
  end

end

(* -------------------------------------------------------------------------- *)

(* During a substitution, *)

module Atom2Atom = struct

  type env = Atom.subst

  module Abstraction = struct
    let map _ f sigma (x, body) =
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
      x, f sigma body
  end

  module Fn = struct
    let map = Atom.Subst.apply
  end

end
