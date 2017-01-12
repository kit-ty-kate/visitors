(* -------------------------------------------------------------------------- *)

(* A universal, concrete type of single-name abstractions. *)

(* We wish to represent all kinds of abstractions -- e.g. in nominal style,
   in de Bruijn style, etc. -- so we parameterize the abstraction over the
   type ['bn] of the bound name and over the type ['term] of the body. This
   makes this type definition almost trivial -- it is just a pair -- but it
   still serves as a syntactic marker of where abstractions are located. *)

type ('bn, 'term) abstraction =
  'bn * 'term

(* -------------------------------------------------------------------------- *)

(* The main effect of an abstraction is to cause the environment to be
   enriched when the abstraction is traversed. The following classes define
   where the environment is enriched. *)

(* These classes do not know the type of the environment, and do not know how
   it is enriched; the latter task is delegated to virtual methods, such as
   [extend] and [restrict]. The implementation of these methods is provided
   by separate ``kits''. *)

(* We need one class per variety of visitor, which is a bit painful. *)

(* The method [visit_abstraction] is polymorphic in the type of terms. This is
   important, as it means that one can use several instances of [abstraction]
   in a single type definition and still be able to construct well-typed
   visitors. *)

(* The virtual methods [extend] and [restrict] are not polymorphic in the
   types of bound names and environments. On the contrary, each kit comes
   with certain specific types of bound names and environments. *)

class virtual ['self] iter = object (self : 'self)

  method virtual extend: 'bn -> 'env -> 'env

  method visit_abstraction: 'term .
    _ ->
    ('env -> 'term -> unit) ->
    'env -> ('bn, 'term) abstraction -> unit
  = fun _ f env (x, body) ->
      let env' = self#extend x env in
      f env' body

end

class virtual ['self] map = object (self : 'self)

  method virtual extend: 'bn1 -> 'env -> 'bn2 * 'env

  method visit_abstraction: 'term1 'term2 .
    _ ->
    ('env -> 'term1 -> 'term2) ->
    'env -> ('bn1, 'term1) abstraction -> ('bn2, 'term2) abstraction
  = fun _ f env (x, body) ->
      let x', env' = self#extend x env in
      x', f env' body

end

class virtual ['self] reduce = object (self : 'self)

  method virtual extend: 'bn -> 'env -> 'env

  method virtual restrict: 'bn -> 'z -> 'z

  method visit_abstraction: 'term .
    _ ->
    ('env -> 'term -> 'z) ->
    'env -> ('bn, 'term) abstraction -> 'z
  = fun _ f env (x, body) ->
      let env' = self#extend x env in
      self#restrict x (f env' body)

end

class virtual ['self] iter2 = object (self : 'self)

  method virtual extend: 'bn1 -> 'bn2 -> 'env -> 'env

  method visit_abstraction: 'term1 'term2 .
    _ ->
    ('env -> 'term1 -> 'term2 -> unit) ->
    'env -> ('bn1, 'term1) abstraction -> ('bn2, 'term2) abstraction -> unit
  = fun _ f env (x1, body1) (x2, body2) ->
      let env' = self#extend x1 x2 env in
      f env' body1 body2

end

class virtual ['self] map2 = object (self : 'self)

  method virtual extend: 'bn1 -> 'bn2 -> 'env -> 'bn3 * 'env

  method visit_abstraction: 'term1 'term2 'term3 .
    _ ->
    ('env -> 'term1 -> 'term2 -> 'term3) ->
    'env -> ('bn1, 'term1) abstraction -> ('bn2, 'term2) abstraction -> ('bn3, 'term3) abstraction
  = fun _ f env (x1, body1) (x2, body2) ->
      let x3, env' = self#extend x1 x2 env in
      x3, f env' body1 body2

end

class virtual ['self] reduce2 = object (self : 'self)

  method virtual extend: 'bn1 -> 'bn2 -> 'env -> 'env

  method virtual restrict: 'bn1 -> 'bn2 -> 'z -> 'z

  method visit_abstraction: 'term1 'term2 .
    _ ->
    ('env -> 'term1 -> 'term2 -> 'z) ->
    'env -> ('bn1, 'term1) abstraction -> ('bn2, 'term2) abstraction -> 'z
  = fun _ f env (x1, body1) (x2, body2) ->
      let env' = self#extend x1 x2 env in
      self#restrict x1 x2 (f env' body1 body2)

end



(* -------------------------------------------------------------------------- *)

(* TEMPORARY
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
 *)
(*
module Atom2Unit : sig

  type env

  val empty: Atom.Set.t ref -> env
  val extend: Atom.t -> env -> env

  module Abstraction : sig
    val iter :
      _ ->
      (env -> 'term -> unit) ->
      env -> (Atom.t, 'term) abstraction -> unit
  end

  module Fn : sig
    val iter: env -> Atom.t -> unit
  end

end

module Fa : sig

  type env = unit

  module Abstraction : sig
    val reduce:
      _ ->
      (env -> 'term -> Atom.Set.t) ->
      env -> (Atom.t, 'term) abstraction -> Atom.Set.t
  end

  module Fn : sig
    val reduce: env -> Atom.t -> Atom.Set.t
  end

end

module Atom2DeBruijn : sig

  type env

  val empty: env

  module Abstraction : sig
    val map :
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> (Atom.t, 'term1) abstraction -> (unit, 'term2) abstraction
  end

  module Fn : sig
    val map: env -> Atom.t -> int
  end

end

module Atom2Atom : sig

  type env = Atom.subst

  module Abstraction : sig
    val map:
      _ ->
      (env -> 'term1 -> 'term2) ->
      env -> (Atom.t, 'term1) abstraction -> (Atom.t, 'term2) abstraction
  end

  module Fn : sig
    val map: env -> Atom.t -> Atom.t
  end

end

module Atom2Something : sig

  type 'term env =
    'term Atom.Map.t

  module Abstraction : sig
    val map:
      _ ->
      ('term env -> 'term1 -> 'term2) ->
      'term env -> (Atom.t, 'term1) abstraction -> (Atom.t, 'term2) abstraction
  end

  module Fn : sig
    val map: 'term env -> Atom.t -> Atom.t
  end

end

module Equiv : sig

  type env

  val empty: env

  module Abstraction : sig
    val iter2:
      _ ->
      (env -> 'term -> 'term -> unit) ->
      env -> (Atom.t, 'term) abstraction -> (Atom.t, 'term) abstraction -> unit
  end

  module Fn : sig
    val iter2:
      env -> Atom.t -> Atom.t -> unit
  end

end

module Wf : sig

  type env

  val empty: env

  module Abstraction : sig
    val iter:
      _ ->
      (env -> 'term -> unit) ->
      env -> (Atom.t, 'term) abstraction -> unit
  end

  module Fn : sig
    val iter: env -> Atom.t -> unit
  end

end
 *)
