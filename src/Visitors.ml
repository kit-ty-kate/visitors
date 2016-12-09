open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience
open Ppx_deriving
open VisitorsAnalysis
open VisitorsGeneration

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

(* Public naming conventions. *)

(* We generate three classes: a [visitor] base class and two subclasses, [iter]
   and [map]. *)

let visitor : classe =
  "visitor"

let iter : classe =
  "iter"

let map : classe =
  "map"

(* For every type constructor [tycon], there is a visitor method, also called
   a descending method, as it is invoked when going down into the tree. *)

let tycon_visitor_method (tycon : Longident.t) : methode =
  (* We support qualified names, and use the last part of the name as the name
     of the visitor method. A qualified name is probably a nonlocal type, that
     is, not part of the current set of type declarations. *)
  last tycon

(* For every data constructor [datacon], there is a descending visitor method,
   which is invoked on the way down, when this data constructor is discovered. *)

let datacon_descending_method (datacon : datacon) : methode =
  "match_" ^ datacon

(* For every data constructor [datacon], there is also an ascending visitor
   method, which is invoked on the way up, after children have been processed,
   in order to finish the processing of this node. In the base class, this
   ascending method is virtual. In the class [iter], it is defined to return a
   unit value. In the class [map], it is defined to reconstruct a new tree
   node using [datacon]. *)

let datacon_ascending_method (datacon : datacon) : methode =
  "build_" ^ datacon

(* -------------------------------------------------------------------------- *)

(* Private naming conventions. *)

(* These conventions must be set up so as to avoid collisions within each name
   space separately: e.g., variables, methods, type variables, and so on. *)

(* In a class, the variable [self] refers to self.
   The type variable [ty_self] denotes its type. *)

let self : variable =
  "self"

let ty_self : core_type =
  Typ.var "self"

let pself : pattern =
  Pat.constraint_ (pvar self) ty_self

(* The variable [env] refers to the environment that is carried down into
   recursive calls. The type variable [ty_env] denotes its type. *)

let env : variable =
  "env"

let ty_env : core_type =
  Typ.var "env"

let penv : pattern =
  Pat.constraint_ (pvar env) ty_env

(* The variables [component i] denote tuple components. *)

let component (i : int) : variable =
  Printf.sprintf "c%d" i

(* The variable [record tycon] denotes a record of type [tycon]. *)

let record (tycon : tycon) : variable =
  Printf.sprintf "_%s" tycon

(* The variables [field label] denote record fields. *)

let field (label : label) : variable =
  Printf.sprintf "f%s" label

(* The variables [result i] denote results of recursive calls. *)

let result (i : int) : variable =
  Printf.sprintf "r%d" i

(* -------------------------------------------------------------------------- *)

(* Per-run global state. *)

module Run (Current : sig val decls : type_declaration list end) = struct

(* As we generate several classes at the same time, we maintain, for each
   generated class, a list of methods that we generate as we go. The following
   line brings [generate] and [dump] into scope. *)

include ClassFieldStore(struct end)

(* -------------------------------------------------------------------------- *)

(* Suppose [e] is an expression whose free variables are [xs]. [hook m xs e]
   produces a call of the form [self#m xs], and (as a side effect) defines an
   auxiliary method [method m xs = e]. The default behavior of this expression
   is the behavior of [e], but we offer the user a hook, named [m], which
   allows this default to be overridden. *)

let hook (m : string) (xs : string list) (e : expression) : expression =
  (* Generate an auxiliary method. We note that its parameters [xs] don't
     need a type annotation: because this method has a call site, its type
     can be inferred. *)
  generate visitor (
    mkconcretemethod m (lambdas xs e)
  );
  (* Generate a method call. *)
  send self m (evars xs)

(* [postprocess m es] evaluates the expressions [es] in turn, binding their
   results to some variables [xs], then makes a self call to the method [m],
   passing the variables [xs] as arguments. This is used in the ascending
   phase of the visitor: the variables [xs] represent the results of the
   recursive calls and the method call [self#m xs] is in charge of
   reconstructing a tree node (or some other result). *)

(* TEMPORARY needs cleaning up *)

let postprocess reconstruct (m : string) (es : expression list) : expression =
  (* Generate a declaration of [m] as an auxiliary virtual method. We note
     that it does not need a type annotation: because we have used the trick
     of parameterizing the class over its ['self] type, no annotations at all
     are needed. *)
  generate visitor (
    mkvirtualmethod m
  );
  (* This virtual method is defined in the subclass [iter] to always return
     unit. *)
  let wildcards = List.map (fun _ -> Pat.any()) es in
  generate iter (
    mkconcretemethod m (plambdas wildcards (unit()))
  );
  (* It is defined in the subclass [map] to always reconstruct a tree node. *)
  (* Generate a method call. *)
  mlet result es (fun xs ->
    generate map (mkconcretemethod m (lambdas xs (reconstruct xs)));
    send self m (evars xs)
  )

(* -------------------------------------------------------------------------- *)

(* [core_type ty] builds a small expression, typically a variable or a function
   call, which represents the derived function associated with the type [ty]. *)

let rec core_type (ty : core_type) : expression =
  match ty with

  (* A type constructor [tycon] applied to type parameters [tys]. *)
  | { ptyp_desc = Ptyp_constr ({ txt = tycon; _ }, tys); _ } ->
      let tycon : Longident.t = tycon
      and tys : core_type list = tys in
      (* Check if this is a local type constructor. If not, declare a
         virtual method for it. We rely on the fact that it is permitted
       for a virtual method to be declared several times. *)
      if not (is_local Current.decls tycon) then
        generate visitor (
          mkvirtualmethod (tycon_visitor_method tycon)
        );
      (* Construct the name of the [visit] method associated with [tycon].
         Apply it to the derived functions associated with [tys] and to
         the environment [env]. *)
      send self (tycon_visitor_method tycon) (List.map env_core_type tys @ [evar env])

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

and env_core_type ty =
  lambda env (core_type ty)

and tuple_type (tys : core_type list) : string list * expression list =
  let x = component in
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
      let reconstruct (xs : string list) : expression = constr datacon (evars xs) in
      Exp.case
        (pconstr datacon (pvars xs))
        (hook (datacon_descending_method datacon) (env :: xs) (postprocess reconstruct (datacon_ascending_method datacon) es))

  (* An ``inline record'' constructor, whose arguments are named. (As of OCaml 4.03.) *)
  | Pcstr_record lds ->
      let ltys = List.map ld_to_lty lds in
      let x = field in
      (* Construct the pattern and expression. *)
      (* TEMPORARY use field access expressions and share code with normal records? *)
      let lps = List.map (fun (label, _ty) -> label,              pvar (x label)) ltys
      and es  = List.map (fun (label,  ty) -> app (core_type ty) [evar (x label)]) ltys in
      let reconstruct (xs : string list) : expression =
        assert (List.length xs = List.length ltys);
        let lxs = List.map2 (fun (label, _ty) x -> (label, evar x)) ltys xs in
        constrrec datacon lxs
      in
      Exp.case (pconstrrec datacon lps) (postprocess reconstruct (datacon_ascending_method datacon) es)

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
      let x = record decl.ptype_name.txt in
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

let type_decl (decl : type_declaration) =
  (* Produce a single method definition, whose name is based on this type
     declaration. *)
  generate visitor (
    mkconcretemethod
      (tycon_visitor_method (Lident decl.ptype_name.txt))
      (plambda penv (type_decl_rhs decl))
  )

end

(* -------------------------------------------------------------------------- *)

(* [type_decls decls] produces structure items (that is, toplevel definitions)
   associated with the type declarations [decls]. *)

let type_decls ~options ~path:_ (decls : type_declaration list) : structure =
  parse_options options;
  (* Analyze the type definitions. *)
  let module R = Run(struct let decls = decls end) in
  R.generate iter (Cf.inherit_ Fresh (Cl.constr (mknoloc (Lident visitor)) [ ty_self; ty_env ]) None);
  R.generate map (Cf.inherit_ Fresh (Cl.constr (mknoloc (Lident visitor)) [ ty_self; ty_env ]) None);
  List.iter R.type_decl decls;
  (* Produce class definitions. Our classes are parameterized over the type
     variable ['env]. They are also parameterized over the type variable
     ['self], with a constraint that this is the type of [self]. This trick
     allows us to omit the declaration of the types of the virtual methods,
     even if these types include type variables. *)
  let params = [
    ty_self, Invariant;
    ty_env, Invariant;
  ] in
  [
    Str.class_ [ mkclass params visitor pself (R.dump visitor) ];
    Str.class_ [ mkclass params iter pself (R.dump iter) ];
    Str.class_ [ mkclass params map pself (R.dump map) ];
  ]

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
