open Longident
open Asttypes
open Parsetree

(* This module offers helper functions for abstract syntax tree analysis. *)

(* -------------------------------------------------------------------------- *)

type tycon = string

(* -------------------------------------------------------------------------- *)

(* [ld_label] and [ld_ty] extract a label and type out of an OCaml record label
   declaration. *)

let ld_label (ld : label_declaration) : label =
  ld.pld_name.txt

let ld_ty (ld : label_declaration) : core_type =
  ld.pld_type

(* [local decls] extracts the list of type constructors that are declared by
   the type declarations [decls]. *)

let local (decls : type_declaration list) : tycon list =
  List.map (fun decl -> decl.ptype_name.txt) decls

(* [is_local decls tycon] tests whether the type constructor [tycon] is
   declared by the type declarations [decls]. *)

let is_local (decls : type_declaration list) (tycon : tycon) : bool =
  List.exists (fun decl -> decl.ptype_name.txt = tycon) decls

let is_local (decls : type_declaration list) (tycon : Longident.t) : bool =
  match tycon with
  | Lident tycon ->
      is_local decls tycon
  | Ldot _
  | Lapply _ ->
      false
