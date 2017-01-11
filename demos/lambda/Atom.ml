(* -------------------------------------------------------------------------- *)

(* We impose maximal sharing on strings so as to reduce the total amount of
   space that they occupy. This is done using a weak hash set. *)

module StringStorage =
  Weak.Make(struct
    type t = string
    let equal (s1 : string) (s2 : string) = (s1 = s2)
    let hash = Hashtbl.hash
  end)

let share : string -> string =
  StringStorage.merge (StringStorage.create 128)

(* -------------------------------------------------------------------------- *)

(* Removing any trailing digits in a string. *)

let is_digit c =
  Char.code '0' <= Char.code c && Char.code c <= Char.code '9'

let remove_trailing_digits (s : string) : string =
  let n = ref (String.length s) in
  while !n > 0 && is_digit s.[!n-1] do n := !n-1 done;
  (* We assume that there is at least one non-digit character in the string. *)
  assert (!n > 0);
  String.sub s 0 !n

(* -------------------------------------------------------------------------- *)

(* An atom is implemented as a pair of an integer identity and a string that
   serves as a printing hint. *)

(* We maintain the invariant that a hint is nonempty and does not end in a
   digit. This allows us to later produce unique identifiers, without risk of
   collisions, by concatenating a hint and a unique number. *)

(* To preserve space, hints are maximally shared. This is not essential for
   correctness, though. *)

type atom = { identity: int; hint: string }

type t = atom

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

(* The argument [hint] must not be a string of digits. *)

let freshh hint =
  let identity = allocate()
  and hint = share (remove_trailing_digits hint) in
  { identity; hint }

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
