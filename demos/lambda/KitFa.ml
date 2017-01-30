(* These kits serve to compute the free atoms of a term. *)

(* This computation can be viewed either as an instance of [iter], where the
   free atoms are accumulated in a reference, or as an instance of [reduce],
   where the free atoms are computed bottom-up. The latter approach uses set
   insertion operations, whereas the latter uses set union operations. *)

(* -------------------------------------------------------------------------- *)

(* The auxiliary class [scope] defines the environment to be a set of atoms
   (the atoms that are currently in scope). It defines the method [extend]
   so as to update this set. *)

class ['self] scope = object (_ : 'self)
  method private extend = Atom.Set.add
end

(* -------------------------------------------------------------------------- *)

(* The auxiliary class [free] inherits [scope] and further defines the method
   [visit_'fn] so that, at a free name occurrence: -1. if the name is local,
   nothing happens; -2. if the name is free, then the method [visit_free] is
   invoked. *)

class virtual ['self] free = object (self : 'self)
  inherit [_] scope
  method private visit_'fn env x =
    if not (Atom.Set.mem x env) then
      self#visit_free x
  method virtual visit_free: _
end

(* -------------------------------------------------------------------------- *)

(* Computing the free atoms of a term, via [iter]. *)

(* The environment is a set of atoms (the atoms that are currently in scope). *)

(* We use a mutable instance variable to keep track of the free atoms that
   have been accumulated so far. (One could also use a reference and store
   it in the environment, which would then be a pair. That would be slightly
   clumsier.) *)

type env =
  Atom.Set.t

let empty =
  Atom.Set.empty

class ['self] iter = object (_ : 'self)
  inherit [_] free

  val mutable accu = Atom.Set.empty

  method accu = accu (* must be public *)

  method private visit_free x =
    accu <- Atom.Set.add x accu

end

(* -------------------------------------------------------------------------- *)

(* Computing the free atoms of a term, via [reduce]. *)

(* In this style, no environment is required. *)

(* type env = unit *)

class ['self] reduce = object (_ : 'self)

  (* The monoid of sets of atoms is used. *)
  inherit [_] Atom.Set.monoid

  method private extend _x () = ()

  (* The atom [x] is removed from the set of free atoms when the scope of [x]
     is exited. *)
  method private restrict = Atom.Set.remove

  method private visit_'fn () x = Atom.Set.singleton x

end

(* -------------------------------------------------------------------------- *)

(* Testing whether a term has a free atom. *)

exception Free of Atom.t

class closed = object
  inherit [_] free
  method visit_free x =
    raise (Free x)
end

let wrap (f : 'term -> unit) : 'term -> Atom.t option =
  fun t ->
    try f t; None with Free x -> Some x
