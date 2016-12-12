open List
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
  options |> iter (fun (name, expr) ->
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

let cvisitor : classe =
  "visitor"

let citer : classe =
  "iter"

let cmap : classe =
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

let components (xs : _ list) : variable list =
  mapi (fun i _ -> component i) xs

(* The variable [thing tycon] denotes a record of type [tycon]. *)

let thing (tycon : tycon) : variable =
  Printf.sprintf "_%s" tycon

(* The variables [field label] denote record fields. *)

let field (label : label) : variable =
  Printf.sprintf "f%s" label

let fields (labels : label list) : variable list =
  map field labels

(* The variables [result i] denote results of recursive calls. *)

let result (i : int) : variable =
  Printf.sprintf "r%d" i

let results (xs : _ list) : variable list =
  mapi (fun i _ -> result i) xs

(* -------------------------------------------------------------------------- *)

(* Per-run global state. *)

module Run (Current : sig val decls : type_declaration list end) = struct

(* As we generate several classes at the same time, we maintain, for each
   generated class, a list of methods that we generate as we go. The following
   line brings [generate] and [dump] into scope. *)

include ClassFieldStore(struct end)

(* -------------------------------------------------------------------------- *)

(* Suppose [e] is an expression whose free variables are [xs]. [hook m xs e]
   constructs a call of the form [self#m xs], and (as a side effect) defines a
   method [method m xs = e]. The default behavior of this expression is the
   behavior of [e], but we offer the user a hook, named [m], which allows this
   default to be overridden. *)

let hook (m : string) (xs : string list) (e : expression) : expression =
  (* Generate a method in the class [visitor]. We note that the formal
     parameters [xs] don't need a type annotation: because this method has a
     call site, its type can be inferred. *)
  generate cvisitor (concrete_method m (lambdas xs e));
  (* Construct a method call. *)
  send self m (evars xs)

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
        generate cvisitor (
          virtual_method (tycon_visitor_method tycon)
        );
      (* Construct the name of the [visit] method associated with [tycon].
         Apply it to the derived functions associated with [tys] and to
         the environment [env]. *)
      send self (tycon_visitor_method tycon) (map env_core_type tys @ [evar env])

  (* A tuple type. *)
  | { ptyp_desc = Ptyp_tuple tys; _ } ->
      (* Construct a function. In the case of tuples, we do not call an
         ascending auxiliary method, as we would need one method name
         per tuple type, and that would be messy. Instead, we make the
         most general choice of ascending computation, which is to rebuild
         a tuple on the way up. Happily, this is always well-typed. *)
      let xs = components tys in
      let es = core_types tys xs in
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

and core_types tys xs =
  map2 app1 (map core_type tys) (evars xs)

(* -------------------------------------------------------------------------- *)

(* [constructor_declaration] turns a constructor declaration (as found in a
   declaration of a sum type) into a case, that is, a branch in the case
   analysis construct that forms the body of the visitor method for this sum
   type. At the same time, it generates several auxiliary method declarations
   and definitions. *)

let constructor_declaration (cd : constructor_declaration) : case =

  (* This is either a traditional data constructor, whose components are
     anonymous, or a data constructor whose components form an ``inline
     record''. This is a new feature of OCaml 4.03. *)

  (* In order to treat these two cases uniformly, we extract the following
     information.
     [xs]       the names under which the components are known.
     [tys]      the types of the components
     [ps]       the patterns that bind [xs], on the way down
     [build]    the expressions that rebuild a data constructor, on the way up
  *)

  let xs, tys, ps, (build : variable list -> expression list) =
  match cd.pcd_args with
    (* A traditional data constructor. *)
    | Pcstr_tuple tys ->
        let xs = components tys in
        xs, tys, pvars xs, evars

    (* An ``inline record'' data constructor. *)
    | Pcstr_record lds ->
        let labels, tys = ld_labels lds, ld_tys lds in
        let xs = fields labels in
        xs, tys,
        [precord ~closed:Closed (combine labels (pvars xs))],
        fun rs -> [record (combine labels (evars rs))]
  in

  let datacon = cd.pcd_name.txt in
  let rs = results xs in
  let m = datacon_ascending_method datacon in

  let case =
    Exp.case
      (pconstr datacon ps)
      (hook
         (datacon_descending_method datacon)
         (env :: xs)
         (letn rs (core_types tys xs) (send self m (evars rs)))
      )
  in

  (* Generate a declaration of [m] as an auxiliary virtual method. We note
     that it does not need a type annotation: because we have used the trick
     of parameterizing the class over its ['self] type, no annotations at all
     are needed. *)
  generate cvisitor (virtual_method m);
  (* This method is defined in the subclass [iter] to always return unit. *)
  generate citer (concrete_method m (plambdas (wildcards xs) (unit())));
  (* It is defined in the subclass [map] to always reconstruct a tree node. *)
  generate cmap (concrete_method m (lambdas rs (constr datacon (build rs))));

  case

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
      let labels = ld_labels lds
      and tys = ld_tys lds in
      let x = thing decl.ptype_name.txt in
      (* Generate one function call for each field. *)
      let es : expression list = map2 (fun label ty ->
        app (core_type ty) [ Exp.field (evar x) (mknoloc (Lident label)) ]
      ) labels tys in
      (* Construct a sequence of these calls, and place it in a function body. *)
      lambda x (sequence es)

  (* A sum type. *)
  | Ptype_variant (cds : constructor_declaration list), _ ->
      (* Generate one case per constructor, and place them in a function
         body, whose formal parameter is anonymous. *)
      Exp.function_ (map constructor_declaration cds)

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
  generate cvisitor (
    concrete_method
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
  R.generate citer (Cf.inherit_ Fresh (Cl.constr (mknoloc (Lident cvisitor)) [ ty_self; ty_env ]) None);
  R.generate cmap (Cf.inherit_ Fresh (Cl.constr (mknoloc (Lident cvisitor)) [ ty_self; ty_env ]) None);
  iter R.type_decl decls;
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
    class1 params cvisitor pself (R.dump cvisitor);
    class1 params citer pself (R.dump citer);
    class1 params cmap pself (R.dump cmap);
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
