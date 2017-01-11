open Printf
open Term
open Toolbox

(* Sample terms. *)

let x =
  Atom.freshh "x"

let y =
  Atom.freshh "y"

let id =
  TLambda (x, TVar x)

let delta =
  TLambda (x, TApp (TVar x, TVar x))

let omega =
  TApp (delta, copy delta)

let samples = [
    TVar y;
    id;
    TApp (id, TVar y);
    TApp (id, copy id);
    delta;
    omega;
  ]

let evaluate f =
  List.iter f samples

(* A non-hygienic term printer. This printer shows the real (internal) identity
   of atoms, using [Atom.show]. *)

let nhprint oc t =
  Print.term oc (show t)

let print_fa t =
  (* This uses the debugging term printer, not the hygienic term printer. *)
  (* Similarly, it uses the debugging printer for sets of atoms. *)
  printf "fa(%a) = %a\n%!"
    nhprint t
    Atom.Set.print (fa t)

let () =
  evaluate print_fa

let print_size t =
  (* This uses the debugging term printer, not the hygienic term printer. *)
  printf "size(%a) = %d\n%!"
    nhprint t
    (size t)

let () =
  evaluate print_size

let print_copy t =
  (* This uses the debugging term printer, not the hygienic term printer. *)
  printf "copy(%a) = %a\n%!"
    nhprint t
    nhprint (copy t)

let () =
  evaluate print_copy
