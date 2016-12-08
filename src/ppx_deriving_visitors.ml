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

let pvars (xs : string list) : pattern list =
  List.map (fun x -> pvar x) xs

let evars (xs : string list) : expression list =
  List.map (fun x -> evar x) xs

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
    (fun e accu -> Exp.sequence e accu)
    es
    (unit())

(* [mlet es e] creates a series of [let] bindings so that each of the
   expressions in the list [es] is evaluated in turn and its result is
   bound to some variable, say [x i]; then, the expression [e], which
   is allowed to depend on these variables, is evaluated. *)

let mlet (es : expression list) (e : expression list -> expression) : expression =
  (* Set up a naming convention for the intermediate results. Each result must
     receive a distinct name. The simplest convention is to use a fixed prefix
     followed with a numeric index. *)
  let x i = Printf.sprintf "r%d" i in
  (* Construct a list of value bindings. *)
  let bindings = List.mapi (fun i e -> Vb.mk (pvar (x i)) e) es in
  let xs = List.mapi (fun i _ -> evar (x i)) es in
  (* Create a series of [let] bindings around the expression [e]. *)
  List.fold_right
    (fun vb k -> Exp.let_ Nonrecursive [vb] k)
    bindings
    (e xs)

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
    pci_loc = !default_loc;
    pci_attributes = [];
  }

(* [mkconcretemethod m e] produces a definition of a public method whose name
   is [m] and whose body is [e]. *)

let mkconcretemethod (m : string) (e : expression) : class_field =
  Cf.method_
    (mknoloc m)
    Public
    (Cf.concrete Fresh e)

(* [mkvirtualmethod m e] produces a definition of a virtual public method
   whose name is [m] and whose type is unspecified. *)

let mkvirtualmethod (m : string) : class_field =
  Cf.method_
    (mknoloc m)
    Public
    (Cf.virtual_ (Typ.any()))

(* -------------------------------------------------------------------------- *)

(* Public naming conventions. *)

(* The name of the visitor base class. *)

let visitor =
  "visitor"

(* The name of the visitor method associated with a type constructor [tycon]. *)

let visitor_method (tycon : string) : string =
  tycon

let visitor_method (tycon : Longident.t) : string =
  match tycon with
  | Lident tycon
  | Ldot (_, tycon) ->
      visitor_method tycon
  | Lapply _ ->
      assert false (* should not happen...? *)

(* The name of the descending method associated with a data constructor [datacon]. *)

let datacon_visitor (datacon : string) : string =
  "match_" ^ datacon

(* The name of the constructor method associated with a data constructor [datacon]. *)

let datacon_constructor (datacon : string) : string =
  "build_" ^ datacon

(* -------------------------------------------------------------------------- *)

(* Private naming conventions. *)

(* The variable [self] refers to the visitor object we are constructing.
   The type variable [ty_self] denotes its type. *)

let self =
  "self"

let ty_self =
  Typ.var "self"

let pself =
  Pat.constraint_ (pvar self) ty_self

(* [call m es] produces a self-call to the method [m] with arguments [es]. *)

let call (m : string) (es : expression list) : expression =
  app (Exp.send (evar self) m) es

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

(* Suppose [e] is an expression whose free variables are [xs]. [hook m xs e]
   produces a call of the form [self#m xs], and (as a side effect) defines an
   auxiliary method [method m xs = e]. The default behavior of this expression
   is the behavior of [e], but we offer the user a hook, named [m], which
   allows this default to be overridden. *)

let hook (m : string) (xs : string list) (e : expression) : expression =
  (* Generate an auxiliary method. We note that its parameters [xs] don't
     need a type annotation: because this method has a call site, its type
     can be inferred. *)
  generate_auxiliary_method (
    mkconcretemethod m (lambdas xs e)
  );
  (* Generate a method call. *)
  call m (evars xs)

(* [postprocess m es] evaluates the expressions [es] in turn, binding their
   results to some variables [xs], then makes a self call to the method [m],
   passing the variables [xs] as arguments. This is used in the ascending
   phase of the visitor: the variables [xs] represent the results of the
   recursive calls and the method call [self#m xs] is in charge of
   reconstructing a tree node (or some other result). *)

let postprocess (m : string) (es : expression list) : expression =
  (* Generate a declaration of [m] as an auxiliary virtual method. We note
     that it does not need a type annotation: because we have used the trick
     of parameterizing the class over its ['self] type, no annotations at all
     are needed. *)
  generate_auxiliary_method (
    mkvirtualmethod m
  );
  (* Generate a method call. *)
  mlet es (fun xs -> call m xs)

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
      call (visitor_method tycon) (List.map core_type tys @ [evar env])

  (* A tuple type. *)
  | { ptyp_desc = Ptyp_tuple tys; _ } ->
      (* Construct a function. In the case of tuples, we do not call an
         ascending auxiliary method, as we would need one method name
         per tuple type, and that would be messy. Instead, we make the
         most general choice of ascending computation, which is to rebuild
         a tuple on the way up. Happily, this is always well-typed. *)
      let xs, es = tuple_type tys in
      plambda (ptuple (pvars xs)) (tuple es)

  (* An unsupported construct. *)
  | { ptyp_loc; _ } ->
      raise_errorf
        ~loc:ptyp_loc
        "%s cannot be derived for %s"
        plugin
        (string_of_core_type ty)

and tuple_type (tys : core_type list) : string list * expression list =
  (* Set up a naming convention for the tuple components. Each component must
     receive a distinct name. The simplest convention is to use a fixed
     prefix followed with a numeric index. *)
  let x i = Printf.sprintf "c%d" i in
  (* Construct a pattern and expression. *)
  let xs = List.mapi (fun i _ty -> x i) tys in
  let es = List.mapi (fun i ty -> app (core_type ty) [evar (x i)]) tys in
  xs, es

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
      let xs, es = tuple_type tys in
      Exp.case
        (pconstr datacon (pvars xs))
        (hook (datacon_visitor datacon) (env :: xs) (postprocess (datacon_constructor datacon) es))

  (* An ``inline record'' constructor, whose arguments are named. (As of OCaml 4.03.) *)
  | Pcstr_record lds ->
      let ltys = List.map ld_to_lty lds in
      (* Set up a naming convention for the fields. The simplest convention
         is to use a fixed prefix followed with the field name. *)
      let x label = Printf.sprintf "f%s" label in
      (* Construct the pattern and expression. *)
      let lps = List.map (fun (label, _ty) -> label,              pvar (x label)) ltys
      and es  = List.map (fun (label,  ty) -> app (core_type ty) [evar (x label)]) ltys in
      Exp.case (pconstrrec datacon lps) (postprocess (datacon_constructor datacon) es)

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
      lambda x (sequence es)

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
  mkconcretemethod
    (visitor_method (Lident decl.ptype_name.txt))
    (plambda penv (type_decl_rhs decl))

(* -------------------------------------------------------------------------- *)

(* [nonlocal_type tycon] produces a class field (e.g., a virtual method)
   associated with a reference to a nonlocal type [tycon]. *)

let nonlocal_type (tycon : string) : class_field =
  mkvirtualmethod (visitor_method (Lident tycon))

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
  (* Produce one class definition. It is parameterized over the type variable
     ['env]. It is also parameterized over the type variable ['self], with a
     constraint that this is the type of [self]. This trick allows us to omit
     the declaration of the types of the virtual methods, even if these types
     include type variables. *)
  let params = [
    ty_self, Invariant;
    ty_env, Contravariant
  ] in
  [ Str.class_ [ mkclass params visitor pself (type_decls decls) ] ]

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
