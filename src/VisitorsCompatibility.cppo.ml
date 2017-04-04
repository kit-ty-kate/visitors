open Asttypes
open Parsetree
open Ast_helper

(* OCaml's abstract syntax tree evolves with time. We depend on this tree
   because we analyze it (that is, we analyze type definitions) and because we
   construct it (that is, we generate code). This module gathers the ugly bits
   whose definition varies depending on the version of OCaml that we are
   working with. *)

#if OCAML_VERSION < (4, 03, 0)
#define Nolabel ""
#endif

(* Constructing an arrow type. *)

let ty_arrow (a : core_type) (b : core_type) : core_type =
  Typ.arrow Nolabel a b

(* Constructing a function. *)

let plambda (p : pattern) (e : expression) : expression =
  Exp.fun_ Nolabel None p e

(* Constructing a string literal. *)

let const_string (w : string) =
#if OCAML_VERSION < (4, 03, 0)
  Const_string (w, None)
#else
  Const.string w
#endif

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

(* Analyzing the definition of a data constructor. *)

(* A data constructor is either a traditional data constructor, whose
   components are anonymous, or a data constructor whose components
   form an ``inline record''. This is a new feature of OCaml 4.03. *)

type data_constructor_variety =
  | DataTraditional of core_type list
  | DataInlineRecord of label list * core_type list

let data_constructor_variety (cd : constructor_declaration) =
  #if OCAML_VERSION < (4, 03, 0)
    DataTraditional cd.pcd_args
  #else
    match cd.pcd_args with
    (* A traditional data constructor. *)
    | Pcstr_tuple tys ->
        DataTraditional tys
    (* An ``inline record'' data constructor. *)
    | Pcstr_record lds ->
        DataInlineRecord (ld_labels lds, ld_tys lds)
  #endif
