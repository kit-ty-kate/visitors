open Printf
open Term
open Toolbox

let x = Atom.freshh "x"

let identity =
  TLambda (x, TVar x)

let y =
  TVar (Atom.freshh "y")

let idy =
  TApp (identity, y)

let print_fa t =
  (* This uses the debugging term printer, not the hygienic term printer. *)
  (* Similarly, it uses the debugging printer for sets of atoms. *)
  printf "fa(%a) = %a\n%!"
    Print.term (show t)
    Atom.Set.print (fa t)

let () =
  List.iter print_fa [ y; identity; idy ]

let print_size t =
  (* This uses the debugging term printer, not the hygienic term printer. *)
  printf "size(%a) = %d\n%!"
    Print.term (show t)
    (size t)

let () =
  List.iter print_size [ y; identity; idy ]
