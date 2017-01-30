(* This kit serves to compute the set of ``bound atoms'' of a term, that is,
   the set of all binding name occurrences. At the same time, we check that
   the term is well-formed, that is, no atom is bound twice. The exception
   [IllFormed x] is raised if the atom [x] occurs twice in a binding
   position. *)

exception IllFormed of Atom.t

type env = unit

class ['self] reduce = object (_ : 'self)

  method private extend _x () =
    ()

  method private visit_'fn () _x =
    Atom.Set.empty

  (* The monoid of sets of atoms, equipped with disjoint union, is used. *)
  method zero =
    Atom.Set.empty
  method plus xs ys =
    match Atom.Set.choose (Atom.Set.inter xs ys) with
    | exception Not_found ->
        (* The intersection of [xs] and [ys] is empty; good.
           Compute their disjoint union. *)
        Atom.Set.union xs ys
    | x ->
        raise (IllFormed x)

  (* The atom [x] is added to the set of bound atoms when its scope is
     exited. *)
  method private restrict x xs =
    if Atom.Set.mem x xs then
      raise (IllFormed x)
    else
      Atom.Set.add x xs

end
