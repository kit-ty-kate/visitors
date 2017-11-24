let mknoloc = Location.mknoloc
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

(* Between OCaml 4.04 and OCaml 4.05, the types of several functions in [Ast_helper]
   have changed. They used to take arguments of type [string], and now take arguments
   of type [str], thus requiring a conversion. These functions include [Typ.object_],
   [Typ.poly], [Exp.send], [Exp.newtype], [Ctf.val_], [Ctf.method_], [Cf.inherit_].  *)

let string2str (s : string) =
  #if OCAML_VERSION < (4, 05, 0)
    s
  #else
    mknoloc s
  #endif

let typ_poly (tyvars : string list) (cty : core_type) : core_type =
  Typ.poly (List.map string2str tyvars) cty

let exp_send (e : expression) (m : string) : expression =
  Exp.send e (string2str m)

(* The function [str2string] converts from either [string] (<4.05) or [string loc] (4.05)
   to [string]. *)

let str2string s : string =
  #if OCAML_VERSION < (4, 05, 0)
    s
  #else
    s.txt
  #endif

(* In the data constructor [Ptyp_poly (qs, ty)], the type of [qs] has changed from
   [string list] to [string loc list] between OCaml 4.04 and 4.05.
   See commit b0e880c448c78ed0cedff28356fcaf88f1436eef.
   The function [quantifiers] compensates for this. *)

let quantifiers qs : string list =
  List.map str2string qs
