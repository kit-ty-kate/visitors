(* -------------------------------------------------------------------------- *)

(* An atom is implemented as a pair of an integer, the atom's identity,
   and a string, which serves as a printing hint. *)

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

let freshh hint = {
  identity = allocate();
  hint
}

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

(* A substitution is a map of atoms to atoms. It acts as the identity
   everywhere except on a finite set (its domain). *)

type subst =
  atom Map.t

module Subst = struct

  let id =
    Map.empty

  let apply sigma a =
    try
      Map.find a sigma
    with Not_found ->
      (* Outside of its explicit domain, the substitution acts
         as the identity. *)
      a

  let extend sigma a b =
    Map.add a b sigma

end
