open Asttypes
open Parsetree

(* This module offers helper functions for abstract syntax tree analysis. *)

(* -------------------------------------------------------------------------- *)

type tycon = string

(* -------------------------------------------------------------------------- *)

(* [ld_to_lty] converts an OCaml label declaration to a pair of a label and
   type. *)

let ld_to_lty (ld : label_declaration) : label * core_type =
  (* Extract the label and type. *)
  let { pld_name = { txt = label; _ }; pld_type = ty; _ } = ld in
  label, ty

(* [defined decls] extracts the list of type constructors that are declared by
   the type declarations [decls]. *)

let defined (decls : type_declaration list) : tycon list =
  List.map (fun decl -> decl.ptype_name.txt) decls
