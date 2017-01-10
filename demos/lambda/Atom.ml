(* -------------------------------------------------------------------------- *)

(* An atom is implemented as a pair of an integer, the atom's identity, and a
   string, which serves as a printing hint. We maintain the invariant that a
   hint is nonempty and does not end in a digit. This allows us to later
   produce unique identifiers, without risk of collisions, by concatenating a
   hint and a unique number. *)

type identifier =
    string

type atom = {
    identity: int;
        hint: identifier
  }

type t =
    atom

let identity a =
  a.identity

let hint a =
  a.hint

(* This printing function should be used for debugging purposes only. *)

let print a =
  Printf.sprintf "%s%d" a.hint a.identity

(* -------------------------------------------------------------------------- *)

(* A global integer counter holds the next available identity. *)

let counter =
  ref 0

let allocate () =
  let number = !counter in
  counter := number + 1;
  assert (number >= 0);
  number

(* [freshh hint] produces a fresh atom. *)

let rec freshh hint =
  let n = String.length hint in
  (* The argument [hint] must not be a string of digits. *)
  assert (n > 0);
  let c = hint.[n-1] in
  if Char.code '0' <= Char.code c && Char.code c <= Char.code '9' then
    (* There is a trailing digit. Remove it. *)
    freshh (String.sub hint 0 (n-1))
  else
    { identity = allocate(); hint }

(* [fresha a] returns a fresh atom modeled after the atom [a]. *)

let fresha a =
  freshh a.hint

(* -------------------------------------------------------------------------- *)

(* Comparison of atoms. *)

let equal a b =
  a.identity = b.identity

let compare a b =
  (* Identities are always positive numbers (see [allocate] above)
     so I believe overflow is impossible here. *)
  a.identity - b.identity

let hash a =
  Hashtbl.hash a.identity

(* -------------------------------------------------------------------------- *)

(* A printing utility. *)

let separated_iter_to_string printer separator iter xs =
  let b = Buffer.create 32 in
  let first = ref true in
  iter (fun x ->
    if !first then begin
      Buffer.add_string b (printer x);
      first := false
    end
    else begin
      Buffer.add_string b separator;
      Buffer.add_string b (printer x)
    end
  ) xs;
  Buffer.contents b

(* -------------------------------------------------------------------------- *)

(* Sets and maps. *)

module Order = struct
  type t = atom
  let compare = compare
end

module Set = struct

  include Set.Make(Order)

  (* This printing function should be used for debugging purposes only. *)

  let print xs =
    separated_iter_to_string
      print
      ", "
      iter
      xs

end

module Map =
  Map.Make(Order)

(* -------------------------------------------------------------------------- *)

(* A substitution is a finite map of atoms to atoms. It acts as the identity
   outside of its domain (a finite set of atoms). It is not necessarily
   injective. *)

(* We keep track of the codomain of a substitution, so as to allow checking
   that an atom [x] is fresh for a substitution [sigma], i.e., [x] appears
   neither in the domain nor in the codomain of [sigma]. If this check is
   used only as part of assertions, then it can be disabled. *)

type subst = {
  graph: atom Map.t;
  codomain: Set.t;
}

module Subst = struct

  let id = {
    graph = Map.empty;
    codomain = Set.empty;
  }

  let apply sigma a =
    try
      Map.find a sigma.graph
    with Not_found ->
      (* Outside of its domain, the substitution acts as the identity. *)
      a

  let extend sigma a b =
    (* This is a strict extension operation: we require [a] not to appear
       in the domain of [sigma]. *)
    assert (not (Map.mem a sigma.graph));
    (* Under the assumption that [a] does not appear in the domain of [sigma],
       the new codomain is computed as follows. Without this assumption, the
       codomain computation would be problematic. One might wish to first
       remove from the codomain the previous image of [a], yet it should not
       be removed if it is the image of other atoms. *)
    {
      graph = Map.add a b sigma.graph;
      codomain = Set.add b sigma.codomain;
    }

  let is_fresh_for a sigma =
    (* The fact that we keep track of codomains is exploited here. *)
    not (Map.mem a sigma.graph || Set.mem a sigma.codomain)

end
