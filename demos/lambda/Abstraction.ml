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
  let reduce _env _x = assert false
  let reduce2 _env _x1 _x2 = assert false
end

(* -------------------------------------------------------------------------- *)

module Generic : sig

  val iter:
    ('bn -> 'env -> 'env) ->
    ('env -> 'term -> unit) ->
    'env -> ('bn, 'term) abstraction -> unit

  val map:
    ('bn1 -> 'env -> 'bn2 * 'env) ->
    ('env -> 'term1 -> 'term2) ->
    'env -> ('bn1, 'term1) abstraction -> ('bn2, 'term2) abstraction

  val reduce:
    ('bn -> 'env -> 'env) ->
    ('bn -> 'a -> 'a) ->
    ('env -> 'term -> 'a) ->
    'env -> ('bn, 'term) abstraction -> 'a

  val iter2:
    ('bn1 -> 'bn2 -> 'env -> 'env) ->
    ('env -> 'term1 -> 'term2 -> unit) ->
    'env -> ('bn1, 'term1) abstraction -> ('bn2, 'term2) abstraction -> unit

end = struct

  let iter extend f env (x, body) =
    let env' = extend x env in
    f env' body

  let map extend f env (x, body) =
    let x', env' = extend x env in
    x', f env' body

  let reduce extend restrict f env (x, body) =
    let env' = extend x env in
    restrict x (f env' body)

  let iter2 extend f env (x1, body1) (x2, body2) =
    let env' = extend x1 x2 env in
    f env' body1 body2

end

(* -------------------------------------------------------------------------- *)

(* A size computation. *)

module Size = struct
  include KitSize
  module Abstraction = struct
    let reduce _ = Generic.reduce extend restrict
  end
end

(* -------------------------------------------------------------------------- *)

(* Conversion of (bound and free) atoms to strings, using [Atom.show], for
   debugging purposes. *)

module Show = struct

  type env = unit

  let extend x env = Atom.show x, env

  module Abstraction = struct
    let map _ = Generic.map extend
  end

  module Fn = struct
    let map _env x =
      Atom.show x
  end

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

  let extend s env =
    let a = Atom.freshh s in
    let env = StringMap.add s a env in
    a, env

  module Abstraction = struct
    let map _ = Generic.map extend
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

(* During a conversion from atoms back to strings (that is, printing), the
   environment is an injective mapping of atoms to strings. We keep track
   of its codomain by recording a mapping of hints to integers. *)

(* TEMPORARY move the low-level code to [Atom] *)

(* We make the global uniqueness hypothesis. *)

module Atom2String = struct

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
    let s = Printf.sprintf "%s%d" hint i in
    let env = {
      graph = Atom.Map.add x s env.graph;
      codomain = StringMap.add hint (i+1) env.codomain;
    } in
    s, env

  module Abstraction = struct
    let map _ = Generic.map extend
  end

  module Fn = struct
    let map env a =
      try
        Atom.Map.find a env.graph
      with Not_found ->
        (* The atom [a] must be in the domain of [env]. *)
        assert false
  end

end

(* TEMPORARY can we precompute fa(every subterm) ahead of time and
   attach this info to binders, so that the free atoms are available when
   printing? *)

(* -------------------------------------------------------------------------- *)

(* During a free atom computation, the environment is a pair of a set of atoms
   (the atoms that are currently in scope) and a reference to a set of atoms
   (the free atoms that have been accumulated so far). *)

module Atom2Unit = struct

  type env = Atom.Set.t * Atom.Set.t ref

  let empty accu =
    Atom.Set.empty, accu

  let extend x (env, accu) =
    let env = Atom.Set.add x env in
    env, accu

  module Abstraction = struct
    let iter _ = Generic.iter extend
  end

  module Fn = struct
    let iter (env, accu) x =
      if not (Atom.Set.mem x env) then
        accu := Atom.Set.add x !accu
  end

end

(* -------------------------------------------------------------------------- *)

(* Free atom computation, viewed as a reduction. *)

module Fa = struct

  type env = unit

  let extend _x env = env
  let restrict = Atom.Set.remove

  module Abstraction = struct
    let reduce _ = Generic.reduce extend restrict
  end

  module Fn = struct
    let reduce _env x =
      Atom.Set.singleton x
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

  let extend x (m, n : env) =
    (* Increment the current de Bruijn level [n]. *)
    let n = n + 1 in
    (* Record a mapping of the name [x] to the de Bruijn level [n],
       so if [x] was looked up right now, it would receive level [n],
       therefore index [0]. *)
    let m = Atom.Map.add x n m in
    (), (m, n)

  module Abstraction = struct
    let map _ = Generic.map extend
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

(* Substitution of free atoms for free atoms. *)

(* We require every binding occurrence [x] encountered along the way to be
   fresh with respect to the substitution [sigma]. *)

module Atom2Atom = struct

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

  module Abstraction = struct
    let map _ = Generic.map extend
  end

  module Fn = struct
    let map = Atom.Subst.apply
  end

end

(* TEMPORARY 1. do we need this function at all?
             2. could we abandon the runtime check and use [Invisible]? *)

(* -------------------------------------------------------------------------- *)

(* Substitution of things (say, terms) for free atoms. *)

(* TEMPORARY note that the environment would be unused if we removed the
   dynamic check. Then, we could use [Invisible]. *)

module Atom2Something = struct

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

  module Abstraction = struct
    let map _ = Generic.map extend
  end

  module Fn = struct
    let map _sigma _x =
      (* We cannot deal with the case where [x] is outside the domain of
         [sigma]. Really, this function should not be called; instead,
         the user should override the [visitor] method for variable nodes. *)
      assert false
  end

end

(* -------------------------------------------------------------------------- *)

(* Copy, that is, substitution of fresh atoms for bound atoms. *)

module Copy = struct

  type env = Atom.subst

  let empty =
    Atom.Subst.id

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

  module Abstraction = struct
    let map _ = Generic.map extend
  end

  module Fn = struct
    let map = Atom.Subst.apply
  end

end

(* -------------------------------------------------------------------------- *)

(* Alpha-equivalence test. *)

module Equiv = struct

  (* The environment contains two maps of atoms to de Bruijn levels, as well
     as the current de Bruijn level. The equivalence test can in fact be
     viewed as two independent conversions to a locally-nameless
     representation, followed with a syntactic comparison of the two terms
     thus obtained. *)

  type m =
    int Atom.Map.t

  type env =
    m * m * int

  let empty =
    Atom.Map.empty, Atom.Map.empty, 0

  type status =
    | Local of int (* a de Bruijn level *)
    | Free         (* a free name *)

  let lookup (m : m) (x : Atom.t) : status =
    try
      (* A local name. *)
      Local (Atom.Map.find x m)
    with Not_found ->
      Free

  let extend (m : m) (n : int) (x : Atom.t) : m =
    assert (not (Atom.Map.mem x m));
    Atom.Map.add x n m

  let extend x1 x2 (m1, m2, n) =
    let m1 = extend m1 n x1
    and m2 = extend m2 n x2
    and n = n + 1 in
    m1, m2, n

  module Abstraction = struct
    let iter2 _ = Generic.iter2 extend
  end

  module Fn = struct
    let iter2 (m1, m2, _) x1 x2 =
      match lookup m1 x1, lookup m2 x2 with
      | Local i1, Local i2 ->
          if i1 <> i2 then
            VisitorsRuntime.fail()
      | Free, Free ->
          if not (Atom.equal x1 x2) then
            VisitorsRuntime.fail()
      | Local _, Free
      | Free, Local _ ->
          VisitorsRuntime.fail()
  end

end

(* -------------------------------------------------------------------------- *)

(* Well-formedness checking. *)

module Wf = struct

  type env = Atom.Set.t

  let empty =
    Atom.Set.empty

  let extend x env =
    (* Check the GUH. *)
    if Atom.Set.mem x env then
      VisitorsRuntime.fail();
    (* Enrich the environment. *)
    Atom.Set.add x env

  module Abstraction = struct
    let iter _ = Generic.iter extend
  end

  module Fn = struct
    let iter env x =
      (* Check that every atom is known. *)
      if not (Atom.Set.mem x env) then
        VisitorsRuntime.fail()
  end

end
(* TEMPORARY could construct error messages *)
