open Longident
open Asttypes
open Parsetree

(* This module offers helper functions for abstract syntax tree analysis. *)

(* -------------------------------------------------------------------------- *)

type tycon = string
type tyvar = string

(* -------------------------------------------------------------------------- *)

(* [ld_label] and [ld_ty] extract a label and type out of an OCaml record label
   declaration. *)

let ld_label (ld : label_declaration) : label =
  ld.pld_name.txt

let ld_labels =
  List.map ld_label

let ld_ty (ld : label_declaration) : core_type =
  ld.pld_type

let ld_tys =
  List.map ld_ty

(* [is_local decls tycon] tests whether the type constructor [tycon] is
   declared by the type declarations [decls]. If so, it returns the list
   of its formal type parameters. *)

let rec is_local (decls : type_declaration list) (tycon : tycon) : tyvar list option =
  match decls with
  | [] ->
      None
  | decl :: decls ->
      if decl.ptype_name.txt = tycon then
        let extract : core_type * variance -> tyvar =
          fun (ty, _) ->
            match ty.ptyp_desc with
            | Ptyp_var tv -> tv
            | _ -> assert false
        in
        Some (List.map extract decl.ptype_params)
      else
        is_local decls tycon

let is_local (decls : type_declaration list) (tycon : Longident.t) : tyvar list option =
  match tycon with
  | Lident tycon ->
      is_local decls tycon
  | Ldot _
  | Lapply _ ->
      None
