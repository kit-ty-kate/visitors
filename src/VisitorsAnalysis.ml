open Longident
open Asttypes
open Parsetree
open Ppx_deriving
open VisitorsPlugin

(* This module offers helper functions for abstract syntax tree analysis. *)

(* -------------------------------------------------------------------------- *)

type tycon = string
type tyvar = string
type tyvars = tyvar list

(* -------------------------------------------------------------------------- *)

(* Testing for the presence of attributes. *)

(* [opacity attrs] tests whether the attribute list [attrs] contains an
   [@opaque] attribute. *)

type opacity =
  | Opaque
  | NonOpaque

let is_opaque (attr : attribute) : bool =
  let { txt = name; _ }, _payload = attr in
  name = "opaque"

let opacity (attrs : attributes) : opacity =
  if List.exists is_opaque attrs then Opaque else NonOpaque

(* -------------------------------------------------------------------------- *)

(* When parsing a record declaration, the OCaml parser attaches attributes
   with field labels, whereas the user might naturally expect them to be
   attached with the type. We rectify this situation by copying all attributes
   from the label to the type. This might seem dangerous, but we use it only
   to test for the presence of an [@opaque] attribute. *)

let paste (ty : core_type) (attrs : attributes) : core_type =
  { ty with ptyp_attributes = attrs @ ty.ptyp_attributes }

let fix (ld : label_declaration) : label_declaration =
  { ld with pld_type = paste ld.pld_type ld.pld_attributes }

let fix =
  List.map fix

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

(* [type_param_to_tyvar] expects a type parameter as found in the field
   [ptype_params] of a type definition, and returns the underlying type
   variable. *)

let type_param_to_tyvar ((ty, _) : core_type * variance) : tyvar =
  match ty.ptyp_desc with
  | Ptyp_var tv ->
      tv
  | Ptyp_any ->
      (* This error occurs if a formal type parameter is a wildcard [_].
         We could support this form, but it makes life slightly simpler
         to disallow it. It is usually used only in GADTs anyway. *)
      raise_errorf ~loc:ty.ptyp_loc
        "%s: every formal type parameter should be named." plugin
  | _ ->
      assert false

let type_params_to_tyvars =
  List.map type_param_to_tyvar

(* [decl_params decl] returns the type parameters of the declaration [decl]. *)

let decl_params (decl : type_declaration) : tyvars =
  type_params_to_tyvars decl.ptype_params

(* [is_local decls tycon] tests whether the type constructor [tycon] is
   declared by the type declarations [decls]. If so, it returns the list
   of its formal type parameters. *)

let rec is_local (decls : type_declaration list) (tycon : tycon) : tyvars option =
  match decls with
  | [] ->
      None
  | decl :: decls ->
      if decl.ptype_name.txt = tycon then
        Some (decl_params decl)
      else
        is_local decls tycon

let is_local (decls : type_declaration list) (tycon : Longident.t) : tyvars option =
  match tycon with
  | Lident tycon ->
      is_local decls tycon
  | Ldot _
  | Lapply _ ->
      None

(* -------------------------------------------------------------------------- *)

(* Testing whether an identifier is valid. *)

(* We use OCaml's lexer to analyze the string and check if it is a valid
   identifier. This method is slightly unorthodox, as the lexer can have
   undesired side effects, such as raising an [Error] exception or printing
   warnings. We do our best to hide these effects. The strength of this
   approach is to give us (at little cost) a correct criterion for deciding if
   an identifier is valid. *)

type classification =
  | LIDENT
  | UIDENT
  | OTHER

let classify (s : string) : classification =
  let lexbuf = Lexing.from_string s in
  let backup = !Location.formatter_for_warnings in
  let null = Format.formatter_of_buffer (Buffer.create 0) in
  Location.formatter_for_warnings := null;
  let result = try
      let token1 = Lexer.token lexbuf in
      let token2 = Lexer.token lexbuf in
      match token1, token2 with
      | Parser.LIDENT _, Parser.EOF ->
         LIDENT
      | Parser.UIDENT _, Parser.EOF ->
         UIDENT
      | _, _ ->
         OTHER
    with Lexer.Error _ ->
      OTHER
  in
  Location.formatter_for_warnings := backup;
  result

(* -------------------------------------------------------------------------- *)

(* Testing if a string is a valid [mod_longident], i.e., a possibly-qualified
   module identifier. *)

(* We might wish to use OCaml's parser for this purpose, but [mod_longident] is
   not declared as a start symbol. Furthermore, that would be perhaps slightly
   too lenient, e.g., allowing whitespace and comments inside. Our solution is
   to split at the dots using [Longident.parse], then check that every piece
   is a valid module name. *)

let is_valid_mod_longident (m : string) : bool =
  String.length m > 0 &&
  let ms = Longident.flatten (Longident.parse m) in
  List.for_all (fun m -> classify m = UIDENT) ms

(* -------------------------------------------------------------------------- *)

(* Testing if a string is a valid [class_longident], i.e., a possibly-qualified
   class identifier. *)

let is_valid_class_longident (m : string) : bool =
  String.length m > 0 &&
  match Longident.parse m with
  | Lident c ->
      classify c = LIDENT
  | Ldot (m, c) ->
      List.for_all (fun m -> classify m = UIDENT) (Longident.flatten m) &&
      classify c = LIDENT
  | Lapply _ ->
      assert false (* this cannot happen *)

(* -------------------------------------------------------------------------- *)

(* [subst_core_type rho ty] applies [rho], a renaming of type variables,
   to the type [ty]. *)

(* We do not go down into [@opaque] types. We replace every opaque type with
   a wildcard [_]. *)

(* TEMPORARY this is too restrictive; we should replace an opaque type with
   a fresh type variable *and* quantify this variable universally *)

type renaming =
  tyvar -> tyvar

let rec subst_core_type (rho : renaming) (ty : core_type) : core_type =
  { ty with ptyp_desc = subst_core_type_desc rho ty }

and subst_core_types rho tys =
  List.map (subst_core_type rho) tys

and subst_core_type_desc rho ty =
  match opacity ty.ptyp_attributes, ty.ptyp_desc with
  | NonOpaque, Ptyp_any ->
      Ptyp_any
  | NonOpaque, Ptyp_var alpha ->
      Ptyp_var (rho alpha)
  | NonOpaque, Ptyp_tuple tys ->
      Ptyp_tuple (subst_core_types rho tys)
  | NonOpaque, Ptyp_constr (tycon, tys) ->
      Ptyp_constr (tycon, subst_core_types rho tys)
  | Opaque, _ ->
      Ptyp_any
  | NonOpaque, Ptyp_arrow _
  | NonOpaque, Ptyp_object _
  | NonOpaque, Ptyp_class _
  | NonOpaque, Ptyp_alias _
  | NonOpaque, Ptyp_variant _
  | NonOpaque, Ptyp_poly _
  | NonOpaque, Ptyp_package _
  | NonOpaque, Ptyp_extension _ ->
      let loc = ty.ptyp_loc in
      raise_errorf ~loc
        "%s: cannot deal with the type %s.\n\
         Consider annotating it with [@opaque]."
        plugin
        (string_of_core_type ty)
