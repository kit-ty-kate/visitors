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

let () =
  printf "fa(%a):" Print.term (atom2string identity);
  print_endline (Atom.Set.print (fa identity));
  print_endline "fa(y):";
  print_endline (Atom.Set.print (fa y));
  print_endline "fa((\\x.x) y):";
  print_endline (Atom.Set.print (fa idy));
  print_endline "size(\\x.x):";
  printf  "%d\n" (size identity);
  print_endline "size((\\x.x) y):";
  printf  "%d\n" (size idy)
