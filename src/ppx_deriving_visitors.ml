open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience
open Ppx_deriving

(* -------------------------------------------------------------------------- *)

(* General infrastructure. *)

let plugin =
  "visitors"

(* No options are supported. *)

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ ->
       raise_errorf
         ~loc:expr.pexp_loc
         "%s does not support option %s"
         plugin
         name
  )

(* -------------------------------------------------------------------------- *)

(* Helper functions for abstract syntax tree analysis. *)

let ld_to_lty (ld : label_declaration) : label * core_type =
  (* Extract the label and type. *)
  let { pld_name = { txt = label; _ }; pld_type = ty; _ } = ld in
  label, ty

(* [defined decls] extracts the list of types that are declared by the type
   declarations [decls]. *)

let defined (decls : type_declaration list) : string list =
  List.map (fun decl -> decl.ptype_name.txt) decls

(* -------------------------------------------------------------------------- *)

(* Helper functions for code generation. *)

(* [plambda p e] constructs a function [fun p -> e]. *)

let plambda (p : pattern) (e : expression) : expression =
  Exp.fun_ Nolabel None p e

(* [lambda x e] constructs a function [fun x -> e]. *)

let lambda (x : string) (e : expression) : expression =
  plambda (pvar x) e

(* [lambdas xs e] constructs a multi-argument function [fun xs -> e]. *)

let lambdas (xs : string list) (e : expression) : expression =
  List.fold_right lambda xs e

(* [sequence es] constructs a sequence of the expressions in the list [es]. *)

let fold_right1 f xs accu =
  match List.rev xs with
  | [] ->
      accu
  | x :: xs ->
      let xs = List.rev xs in
      (* We have decomposed [xs] as [xs] followed with [x]. We can now
         ignore [accu] and use [x] as the initial accumulator in our
         right-to-left sweep of the list. *)
      List.fold_right f xs x

let sequence (es : expression list) : expression =
  (* Using [fold_right1] instead of [List.fold_right] allows us to get
     rid of a final [()] constant at the end of the sequence. Cosmetic. *)
  fold_right1
    (fun e accu -> [%expr [%e e]; [%e accu]])
    es
     [%expr ()]

(* [pconstrrec datacon lps] produces a pattern for an ``inline record''.
   [datacon] is the data constructor; [lps] is the label-pattern list. *)

let pconstrrec (datacon : string) (lps : (string * pattern) list) =
  pconstr datacon [precord ~closed:Closed lps]

(* [mkclass name self fields] constructs a class declaration. We make all of
   our classes virtual, because it does not make sense to use them without
   overriding any methods. *)

let mkclass
  (params : (core_type * variance) list)
  (name : string) (self : pattern)
  (fields : class_field list)
  : class_declaration =
  {
    pci_virt = Virtual;
    pci_params = params;
    pci_name = mknoloc name;
    pci_expr = Cl.structure (Cstr.mk self fields);
    pci_loc = Location.none;
    pci_attributes = [];
  }

(* -------------------------------------------------------------------------- *)

(* Public naming conventions. *)

(* The name of the visitor method associated with a type constructor [tycon]. *)

let visitor (tycon : string) : string =
  tycon

let visitor (tycon : Longident.t) : string =
  match tycon with
  | Lident tycon
  | Ldot (_, tycon) ->
      visitor tycon
  | Lapply _ ->
      assert false (* should not happen...? *)

(* The name of the visitor method associated with a data constructor [datacon]. *)

let datacon_visitor (datacon : string) : string =
  String.lowercase_ascii datacon

(* -------------------------------------------------------------------------- *)

(* Private naming conventions. *)

(* The variable [self] refers to the visitor object we are constructing. *)

let self =
  "self"

(* The variable [env] refers to the environment that is carried down into
   recursive calls. The type variable [ty_env] denotes its type. *)

let env =
  "env"

let ty_env : core_type =
  Typ.var "env"

let penv : pattern =
  Pat.constraint_ (pvar env) ty_env

(* -------------------------------------------------------------------------- *)

(* Per-run global state. *)

module Run (Current : sig val decls : type_declaration list end) = struct

(* [nonlocal] records the set of nonlocal type constructors that have been
   encountered as we go. *)

let nonlocal : string list ref =
  ref []

let insert_nonlocal (s : string) =
  if not (List.mem s !nonlocal) then
    nonlocal := s :: !nonlocal

(* [is_local tycon] tests whether the type constructor [tycon] is local,
   that is, whether it is declared by the current set of type declarations.
   At the same time, if [tycon] is found to be non-local, then it is added
   (in an unqualified form) to the set [nonlocal]. *)

let is_local (tycon : Longident.t) : bool =
  match tycon with
  | Lident s ->
      let is_local = List.mem s (defined Current.decls) in
      if not is_local then insert_nonlocal s;
      is_local
  | Ldot (_, s) ->
      insert_nonlocal s;
      false
  | Lapply _ ->
      false (* should not happen? *)

(* [auxiliary_methods] holds a list of auxiliary methods that are generated as
   we go. *)

let auxiliary_methods : class_field list ref =
  ref []

let generate_auxiliary_method (cf : class_field) =
  auxiliary_methods := cf :: !auxiliary_methods

let auxiliary_methods () =
  List.rev !auxiliary_methods

(* Suppose [e] is an expression whose free variables are [xs]. If the optional
   method name [om] is absent, then [hook om xs e] is just the expression [e].
   If [om] is [Some m], on the other hand, then [hook om xs e] produces a call
   of the form [self#m xs], and (as a side effect) defines an auxiliary method
   [method m xs = e]. The behavior is the same, but, in the latter case, we
   offer the user a hook, named [m], which allows this behavior to be
   overridden. *)

let hook (om : string option) (xs : string list) (e : expression) : expression =
  match om with
  | None ->
      e
  | Some m ->
      generate_auxiliary_method (
        Cf.method_
          (mknoloc m)
          Public
          (Cf.concrete Fresh (lambdas xs e))
      );
      app
        (Exp.send (evar self) m)
        (List.map (fun x -> evar x) xs)

(* -------------------------------------------------------------------------- *)

(* [core_type ty] builds a small expression, typically a variable or a function
   call, which represents the derived function associated with the type [ty]. *)

let rec core_type (ty : core_type) : expression =
  match ty with

  (* A type constructor [tycon] applied to type parameters [tys]. *)
  | { ptyp_desc = Ptyp_constr ({ txt = tycon; _ }, tys); _ } ->
      let tycon : Longident.t = tycon
      and tys : core_type list = tys in
      (* Check if this is a local type constructor. If not, then we need
         to generate a virtual method for it. *)
      let (_ : bool) = is_local tycon in
      (* Construct the name of the [visit] method associated with [tycon].
         Apply it to the derived functions associated with [tys] and to
         the environment [env]. *)
      app
        (Exp.send (evar self) (visitor tycon))
        ((core_types tys) @ [evar env])

  (* A tuple type. *)
  | { ptyp_desc = Ptyp_tuple tys; _ } ->
      (* Construct a function. *)
      let ptuple (ps : pattern list) : pattern = ptuple ps in
      let p, e = tuple_type None ptuple tys in
      plambda p e

  (* An unsupported construct. *)
  | { ptyp_loc; _ } ->
      raise_errorf
        ~loc:ptyp_loc
        "%s cannot be derived for %s"
        plugin
        (string_of_core_type ty)

and core_types (tys : core_type list) : expression list =
  List.map core_type tys

and tuple_type (om : string option) (pat : pattern list -> pattern) (tys : core_type list) : pattern * expression =
  (* Set up a naming convention for the tuple components. Each component must
     receive a distinct name. The simplest convention is to use a fixed
     prefix followed with a numeric index. *)
  let x i = Printf.sprintf "c%d" i in
  (* Construct a pattern and expression. *)
  let xs = List.mapi (fun i _ty -> x i) tys in
  let ps = List.map  (fun x -> pvar x) xs
  and es = List.mapi (fun i ty -> app (core_type ty) [evar (x i)]) tys in
  (* Construct a case, that is, a pattern/expression pair. We are parametric
     in the pattern constructor [pat], which can be instantiated with [ptuple]
     and with [pconstr datacon]. *)
  pat ps,
  hook om (env :: xs) (sequence es)

(* -------------------------------------------------------------------------- *)

(* [constructor_declaration] turns a constructor declaration (as found in a
   declaration of a sum type) into a case, that is, a branch in a case
   analysis construct. *)

let constructor_declaration (cd : constructor_declaration) : case =
  (* Extract the data constructor name and arguments. *)
  let { pcd_name = { txt = datacon; _ }; pcd_args; _ } = cd in
  match pcd_args with

  (* A traditional constructor, whose arguments are anonymous. *)
  | Pcstr_tuple tys ->
      let p, e = tuple_type (Some (datacon_visitor datacon)) (pconstr datacon) tys in
      Exp.case p e

  (* An ``inline record'' constructor, whose arguments are named. (As of OCaml 4.03.) *)
  | Pcstr_record lds ->
      let ltys = List.map ld_to_lty lds in
      (* Set up a naming convention for the fields. The simplest convention
         is to use a fixed prefix followed with the field name. *)
      let x label = Printf.sprintf "f%s" label in
      (* Construct the pattern and expression. *)
      let lps = List.map (fun (label, _ty) -> label,              pvar (x label)) ltys
      and es  = List.map (fun (label,  ty) -> app (core_type ty) [evar (x label)]) ltys in
      Exp.case (pconstrrec datacon lps) (sequence es)

(* -------------------------------------------------------------------------- *)

(* [type_decl_rhs decl] produces the right-hand side of the value binding
   associated with the type declaration [decl]. *)

let type_decl_rhs (decl : type_declaration) : expression =
  match decl.ptype_kind, decl.ptype_manifest with

  (* A type abbreviation. *)
  | Ptype_abstract, Some ty ->
      core_type ty

  (* A record type. *)
  | Ptype_record (lds : label_declaration list), _ ->
      let ltys = List.map ld_to_lty lds in
      (* Set up a naming convention for the record itself. Any name would do,
         but we choose to use the name of the type that is being declared. *)
      let x = decl.ptype_name.txt in
      (* Generate one function call for each field. *)
      let es : expression list = List.map (fun (label, ty) ->
        app (core_type ty) [ Exp.field (evar x) (mknoloc (Lident label)) ]
      ) ltys in
      (* Construct a sequence of these calls, and place it in a function body. *)
      plambda (pvar x) (sequence es)

  (* A sum type. *)
  | Ptype_variant (cds : constructor_declaration list), _ ->
      (* Generate one case per constructor, and place them in a function
         body, whose formal parameter is anonymous. *)
      Exp.function_ (List.map constructor_declaration cds)

  (* An unsupported construct. *)
  | _ ->
      raise_errorf
        ~loc:decl.ptype_loc
        "%s cannot be derived for this sort of type"
        plugin

(* -------------------------------------------------------------------------- *)

(* [type_decl decl] produces a class field (e.g., a method) associated with
   the type declaration [decl]. *)

let type_decl (decl : type_declaration) : class_field =
  (* Produce a single method definition, whose name is based on this type
     declaration. *)
  Cf.method_
    (mknoloc (visitor (Lident decl.ptype_name.txt)))
    Public
    (Cf.concrete Fresh (plambda penv (type_decl_rhs decl)))

(* -------------------------------------------------------------------------- *)

(* [method_type tycon] constructs the type of the virtual method associated
   with the nonlocal type [tycon]. *)

let ty_unit : core_type =
  tconstr "unit" []

let arrow ty1 ty2 =
  Typ.arrow Nolabel ty1 ty2

let method_type (tycon : string) : core_type =
  let ty = tconstr tycon [] in
  arrow ty_env (arrow ty ty_unit)

(* -------------------------------------------------------------------------- *)

(* [nonlocal_type tycon] produces a class field (e.g., a virtual method)
   associated with a reference to a nonlocal type [tycon]. *)

let nonlocal_type (tycon : string) : class_field =
  Cf.method_
    (mknoloc (visitor (Lident tycon)))
    Public
    (Cf.virtual_ (method_type tycon))

let nonlocal_types () : class_field list =
  List.map nonlocal_type !nonlocal

end

(* -------------------------------------------------------------------------- *)

(* [type_decls decls] produces class fields (that is, mainly methods)
   associated with the type declarations [decls]. *)

let type_decls (decls : type_declaration list) : class_field list =
  let module R = Run(struct let decls = decls end) in
  (* Traverse the type declarations, producing methods for the local types. *)
  let concrete_methods = List.map R.type_decl decls in
  (* Then, produce virtual methods for the nonlocal types that have been
     encountered along the way. *)
  let virtual_methods = R.nonlocal_types() in
  (* Also include any auxiliary methods that may have been generated along
     the way. *)
  concrete_methods @ R.auxiliary_methods() @ virtual_methods

(* -------------------------------------------------------------------------- *)

(* [type_decls decls] produces structure items (that is, toplevel definitions)
   associated with the type declarations [decls]. *)

let type_decls ~options ~path:_ (decls : type_declaration list) : structure =
  parse_options options;
  (* Produce one class definition. It is parameterized over the type of the
     environment parameter [env]. *)
  let params = [ ty_env, Contravariant ] in
  [ Str.class_ [ mkclass params plugin (pvar self) (type_decls decls) ] ]

(* -------------------------------------------------------------------------- *)

(* We do not allow deriving any code on the fly, outside of a type declaration.
   Indeed, that would not make any sense: we need to generate a class and have
   a [self] parameter. *)

let no_core_type ty =
  raise_errorf
    ~loc:ty.ptyp_loc
    "%s cannot be used on the fly"
    plugin

(* -------------------------------------------------------------------------- *)

(* Register our plugin with [ppx_deriving]. *)

let () =
  register (
      create
        plugin
        ~core_type:no_core_type
        ~type_decl_str:type_decls
        ()
    )
