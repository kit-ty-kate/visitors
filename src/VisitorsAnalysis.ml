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
  let pieces = Longident.flatten (Longident.parse m) in
  List.for_all (fun piece -> classify piece = UIDENT) pieces
