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
  printf "fa(%a) = %a\n%!"
    Print.term (atom2string t)
    Atom.Set.print (fa t)

let () =
  print_fa identity
(* TEMPORARY these fail because the terms are not closed:
let () =
  print_fa y
let () =
  print_fa idy
 *)

let print_size t =
  printf "size(%a) = %d\n%!"
    Print.term (atom2string t)
    (size t)

let () =
  print_size identity
(* TEMPORARY these fail because the terms are not closed:
let () =
  print_size idy
 *)
