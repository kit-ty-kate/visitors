(* These kits serve to compute the free atoms of a term. *)

(* This computation can be viewed either as an instance of [iter], where the
   free atoms are accumulated in a reference, or as an instance of [reduce],
   where the free atoms are computed bottom-up. The latter approach uses set
   insertion operations, whereas the latter uses set union operations. *)

(* -------------------------------------------------------------------------- *)

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

  val mutable accu = Atom.Set.empty

  method accu = accu

  method extend = Atom.Set.add

  method visit_'fn env x =
    if not (Atom.Set.mem x env) then
      accu <- Atom.Set.add x accu

end
