open VisitorsList
open Longident
open List
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience
open Ppx_deriving
open VisitorsAnalysis
open VisitorsGeneration
open VisitorsSettings

(* -------------------------------------------------------------------------- *)

(* Per-run global state. *)

module Setup (X : SETTINGS) = struct

let arity =
  X.arity

(* -------------------------------------------------------------------------- *)

(* If the [public] option is absent, then every method is public. If it is
   present, then every method is private, unless its name occurs in the list
   [X.public]. *)

let visibility m =
  match X.public with
  | None ->
      Public
  | Some ms ->
      if List.mem m ms then Public else Private

(* The following brings [generate] and [dump] into scope. *)

include ClassFieldStore(struct end)

let generate_concrete_method m e =
  generate (concrete_method (visibility m) m e)

let generate_virtual_method m =
  generate (virtual_method (visibility m) m)

(* -------------------------------------------------------------------------- *)

(* The following brings [warning] and [warnings] into scope. *)

include WarningStore(struct end)

(* -------------------------------------------------------------------------- *)

(* [choose] is used to generate different code depending on [scheme]. *)

(* Each branch is described by a function [e] and an argument [x]. We
   apply [e] to [x] for the desired branch, and only for that branch. *)

let choose e1 x1 e2 x2 e3 x3 =
  match X.scheme with
  | Iter -> e1 x1
  | Map  -> e2 x2
  | Reduce -> e3 x3

(* -------------------------------------------------------------------------- *)

(* We support parameterized type declarations. We require them to be regular.
   That is, for instance, if a type ['a term] is being defined, then every
   use of [_ term] in the definition should be ['a term]; it cannot be, say,
   [int term] or [('a * 'a) term]. *)

(* To enforce this, we check that, in every use of a local type constructor,
   the actual type parameters coincide with the formal type parameters. *)

let check_regularity loc tycon (formals : tyvar list) (actuals : core_type list) =
  (* Check that the numbers of parameters match. *)
  if length formals <> length actuals then
    raise_errorf ~loc
      "%s: the type constructor %s expects %s,\n\
       but is applied to %s."
      plugin tycon
      (number (length formals) "type parameter")
      (number (length actuals) "type parameter");
  (* Check that the parameters match. *)
  if not X.irregular && not (
    fold_left2 (fun ok formal actual ->
      ok && actual.ptyp_desc = Ptyp_var formal
    ) true formals actuals
  ) then
    raise_errorf ~loc "%s: the type constructor %s is irregular." plugin tycon

(* -------------------------------------------------------------------------- *)

(* Public naming conventions. *)

(* For every type constructor [tycon], there is a visitor method, also called
   a descending method, as it is invoked when going down into the tree. *)

let tycon_visitor_method (tycon : Longident.t) : methode =
  (* We support qualified names, and, in that case, use every part of the
     qualified name to obtain the name of the visitor method. A qualified name
     is probably a nonlocal type, that is, not part of the current set of type
     declarations. *)
  (* One might like to use [last tycon] directly as the name of the method, but
     that could (in theory) create a conflict with the names of other methods.
     In order to guarantee the absence of conflicts, we must use a nonempty
     prefix. *)
  "visit_" ^ String.concat "_" (
    map String.uncapitalize_ascii (Longident.flatten tycon)
    (* TEMPORARY there could be clashes *)
  )

(* Type variables are treated as nonlocal type constructors, so they also have
   a descending method. We include a quote in the method name so as to ensure
   the absence of collisions. *)

let tyvar_visitor_method (tv : tyvar) : methode =
  "visit_'" ^ tv

(* For every data constructor [datacon], there is a descending visitor method,
   which is invoked on the way down, when this data constructor is discovered. *)

let datacon_descending_method (datacon : datacon) : methode =
  "visit_" ^ datacon

(* At arity 2, for every sum type constructor [tycon] which has at least two
   data constructors, there is a failure method, which is invoked when the
   left-hand and right-hand arguments do not exhibit the same tags. *)

let failure_method (tycon : tycon) : methode =
  "fail_" ^ tycon

(* For every nonlocal type constructor [tycon], we need a visitor function.
   E.g., if we are generating a class [map], then for the type constructor
   [list], we need [List.map]. Note that this is not an absolute name: its
   interpretation depends on which modules have been opened. This can be
   influenced by the user via the option [nonlocal]. *)

(* TEMPORARY
let nonlocal_tycon_module (tycon : Longident.t) : Longident.t =
  match tycon with
  | Lident tycon ->
      (* Turn [list] into [List]. *)
      Lident (String.capitalize_ascii tycon)
  | Ldot (path, "t") ->
      (* Turn [Foo.t] into [Foo]. *)
      path
  | Ldot (path, tycon) ->
      (* Turn [Foo.list] into [Foo.List]. *)
      Ldot (path, String.capitalize_ascii tycon)
  | Lapply _ ->
      assert false

let nonlocal_tycon_function (tycon : Longident.t) : Longident.t =
  (* For [list], we need [List.map]. *)
  Ldot (nonlocal_tycon_module tycon, X.variety)
 *)

(* When [scheme] is [Reduce], we need a monoid, that is, a unit [zero] and a
   binary operation [plus]. The names [zero] and [plus] are fixed. We assume
   that there exist virtual methods by these names. It is up to the user to
   provide these methods via inheritance, that is, via the [ancestors]
   option. *)

let zero =
  "zero"

let plus =
  "plus"

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
   recursive calls. *)

let env : variable =
  "env"

(* We sometimes need two (or more) copies of a variable: one copy for each
   index [j] ranging in the interval [0..arity). *)

let copy (j : int) (x : string) : string =
  assert (0 <= j && j < arity);
  if arity = 1 then
    (* No alteration required. *)
    x
  else
    sprintf "%s_%d" x j

(* The variables [component i j] denote tuple components. The index [i]
   ranges over tuple components; the index [j] ranges in [0..arity). *)

let component (i : int) (j : int) : variable =
  copy j (sprintf "c%d" i)

let components (i : int) : variable list =
  map (component i) (interval 0 arity)

let componentss (xs : _ list) : variable list list =
  mapi (fun i _ -> components i) xs

(* The variable [thing tycon j] denotes a value of type [tycon]. *)

let thing (tycon : tycon) (j : int) : variable =
  copy j (sprintf "this_%s" tycon)

let things (tycon : tycon) : variable list =
  map (thing tycon) (interval 0 arity)

(* The variables [field label j] denote record fields. *)

let field (label : label) (j : int) : variable =
  copy j (sprintf "f%s" label)

let fields (label : label) : variable list =
  map (field label) (interval 0 arity)

let fieldss (labels : label list) : variable list list =
  map fields labels

(* The variables [result i] denote results of recursive calls. *)

let result (i : int) : variable =
  sprintf "r%d" i

let results (xs : _ list) : variable list =
  mapi (fun i _ -> result i) xs

(* -------------------------------------------------------------------------- *)

(* [call m es] emits a method call of the form [self#m es]. *)

let call (m : methode) (es : expression list) : expression =
  send self m es

(* -------------------------------------------------------------------------- *)

(* Access to the monoid operations. *)

let monoid_unit () : expression =
  assert (X.scheme = Reduce);
  call zero []

let monoid_law () : expression =
  assert (X.scheme = Reduce);
  call plus []

(* -------------------------------------------------------------------------- *)

(* [reduce es] is used when [scheme] is [Reduce]. It reduces the expressions
   [es], that is, it combines them, using a monoid, which provides a unit and
   a binary operation. The reduction is performed left-to-right. This could
   be of importance if the monoid is not associative-commutative. *)

let reduce es =
  assert (X.scheme = Reduce);
  let unit = monoid_unit()
  and law = monoid_law() in
  fold_left1 (fun e1 e2 -> app law [e1; e2]) unit es

(* -------------------------------------------------------------------------- *)

(* [hook m xs e] constructs a call of the form [self#m xs], and (as a side
   effect) generates a method [method m xs = e]. The free variables of the
   expression [e] must be (a subset of) [xs]. *)

(* Thus, by default, the expression [hook m xs e] behaves in the same way
   as the expression [e]. But a hook, named [m], allows this default to be
   overridden. *)

let hook (m : string) (xs : string list) (e : expression) : expression =
  (* Generate a method. The formal parameters [xs] don't need a type
     annotation: because this method has a call site, its type can be
     inferred. *)
  generate_concrete_method m (lambdas xs e);
  (* Construct a method call. *)
  call m (evars xs)

(* -------------------------------------------------------------------------- *)

(* [visit_type env_in_scope ty] builds a small expression that represents the
   visiting code associated with the OCaml type [ty]. For instance, if [ty] is
   a local type constructor, this could be a call to the visitor method
   associated with this type constructor. *)

(* This expression may refer to the variable [self]. *)

(* If [env_in_scope] is true, then this expression may refer to the variable
   [env]. If [env_in_scope] is false, then this expression should denote a
   function of [env]. The use of [env_in_scope] complicates things slightly,
   but allows us to avoid the production of certain eta-redexes. *)

let rec visit_type (env_in_scope : bool) (ty : core_type) : expression =
  match env_in_scope, ty with

  (* A type constructor [tycon] applied to type parameters [tys]. We handle
     the case where [env_in_scope] is false, so we construct a function of
     [env]. *)
  | false,
    { ptyp_desc = Ptyp_constr ({ txt = (tycon : Longident.t); _ }, tys); _ } ->
      begin match is_local X.decls tycon with
      | Some formals ->
          (* [tycon] is a local type constructor, whose formal type parameters
             are [formals]. *)
          (* Check that [tys] and [formals] coincide. If they do not, we cannot
             handle this type declaration. *)
          check_regularity ty.ptyp_loc (last tycon) formals tys;
          (* Return the visitor method associated with [tycon]. Contrary to
             the nonlocal case (below), this method must not be applied to the
             visitor functions associated with [tys]. *)
          call
            (tycon_visitor_method tycon)
            []
      | None ->
          (* [tycon] is a nonlocal type constructor. Return the visitor method
             associated with [tycon], applied to the visitor functions associated
             with [tys]. This method is NOT generated by us, so it MUST be
             inherited from an ancestor class; it is up to the user to ensure
             that this method exists. (It may be virtual.) This method may be
             polymorphic, so multiple call sites do not pollute one another. *)
          app
            (call (tycon_visitor_method tycon) [])
            (map (visit_type false) tys)
      end

  (* A type variable [tv] is handled by a virtual visitor method. *)
  | false,
    { ptyp_desc = Ptyp_var tv; _ } ->
      generate_virtual_method (tyvar_visitor_method tv);
      call (tyvar_visitor_method tv) []

  (* A tuple type. We handle the case where [env_in_scope] is true, as it
     is easier. *)
  | true,
    { ptyp_desc = Ptyp_tuple tys; _ } ->
      (* Construct a function that takes [arity] tuples as arugments. *)
      (* See [constructor_declaration] for comments. *)
      let xss = componentss tys in
      let rs = results xss in
      plambdas
        (ptuples (transpose arity (pvarss xss)))
        (letn rs (visit_types tys (evarss xss))
          (choose
            unit()
            tuple (evars rs)
            reduce (evars rs)
          )
        )

  (* If [env_in_scope] does not have the desired value, wrap a recursive call
     within an application or abstraction. At most one recursive call takes
     place, so we never produce an eta-redex. *)
  | true, { ptyp_desc = (Ptyp_constr _ | Ptyp_var _); _ } ->
     app (visit_type false ty) [evar env]
  | false, { ptyp_desc = (Ptyp_tuple _); _ } ->
     lambda env (visit_type true ty)

  (* An unsupported construct. *)
  | _, _ ->
      let loc = ty.ptyp_loc in
      raise_errorf ~loc "%s: cannot deal with the type %s." plugin
        (string_of_core_type ty)

and visit_types tys (ess : expression list list) : expression list =
  (* The matrix [ess] is indexed first by component, then by index [j].
     Thus, to each type [ty], corresponds a row [es] of expressions,
     whose length is [arity]. *)
  assert (is_matrix (length tys) arity ess);
  map2 (fun ty es ->
    app (visit_type true ty) es
  ) tys ess

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
     [xss]      the names under which the components are known.
                this matrix has [length tys] rows -- one per component --
                and [arity] columns.
     [tys]      the types of the components.
     [pss]      the patterns that bind [xss], on the way down.
                this matrix has [arity] rows.
                it has [length tys] columns in the case of tuples,
                and 1 column in the case of inline records.
     [build]    the expressions that rebuild a data constructor, on the way up.
  *)

  let xss, tys, pss, (build : variable list -> expression list) =
  match cd.pcd_args with
    (* A traditional data constructor. *)
    | Pcstr_tuple tys ->
        let xss = componentss tys in
        let pss = transpose arity (pvarss xss) in
        xss, tys, pss, evars
    (* An ``inline record'' data constructor. *)
    | Pcstr_record lds ->
        let labels, tys = ld_labels lds, ld_tys lds in
        let xss = fieldss labels in
        let pss = transpose arity (pvarss xss) in
        xss, tys,
        map (fun ps -> [precord ~closed:Closed (combine labels ps)]) pss,
        fun rs -> [record (combine labels (evars rs))]
  in
  assert (is_matrix (length tys) arity xss);
  assert (length pss = arity);

  (* Get the name of this data constructor. *)
  let datacon = cd.pcd_name.txt in
  (* Create new names [rs] for the results of the recursive calls of visitor
     methods. *)
  let rs = results xss in

  (* Construct a case for this data constructor in the visitor method
     associated with this sum type. This case analyzes a tuple of width
     [arity]. After binding the components [xss], we call the descending
     method associated with this data constructor, with arguments [env] and
     [xss]. This method binds the variables [rs] to the results of the
     recursive calls to visitor methods, then (in the class [iter]) returns a
     unit value or (in the class [map]) reconstructs a tree node. *)
  Exp.case
    (ptuple (map (pconstr datacon) pss))
    (hook (datacon_descending_method datacon) (env :: flatten xss)
       (letn
          rs (visit_types tys (evarss xss))
          (choose
            unit()
            (constr datacon) (build rs)
            reduce (evars rs)
          )
       )
    )

(* -------------------------------------------------------------------------- *)

(* [visit_decl decl] constructs an expression that represents the visiting
   code associated with the type declaration [decl]. In other words, it is
   the body of the visitor method associated with [decl]. *)

let visit_decl (decl : type_declaration) : expression =

  (* Bind the values to a vector of variables [xs]. *)
  let tycon = decl.ptype_name.txt in
  let xs = things tycon in
  assert (length xs = arity);

  match decl.ptype_kind, decl.ptype_manifest with

  (* A type abbreviation. *)
  | Ptype_abstract, Some ty ->
      visit_type true ty

  (* A record type. *)
  | Ptype_record (lds : label_declaration list), _ ->
      let labels, tys = ld_labels lds, ld_tys lds in
      (* See [constructor_declaration] for comments. *)
      lambdas xs (
        let rs = results labels in
        letn rs (visit_types tys (accesses xs labels))
          (choose
            unit()
            record (combine labels (evars rs))
            reduce (evars rs)
          )
      )

  (* A sum type. *)
  | Ptype_variant (cds : constructor_declaration list), _ ->
      (* Generate one case per data constructor. Place these cases in a
         [match] construct, which itself is placed in a function body. *)
      (* If [arity] is greater than 1 and if there is more than one data
         constructor, then generate also a default case. In this default
         case, invoke the failure method, which raises an exception. The
         failure method receives [env] and [xs] as arguments. *)
      let default() : case =
        Exp.case
          (ptuple (pvars xs))
          (hook (failure_method tycon) (env :: xs)
            (efail (tycon_visitor_method (Lident tycon)))
          )
      in
      let complete (cs : case list) : case list =
        if arity = 1 || length cs <= 1 then cs else cs @ [ default() ]
      in
      lambdas xs (
        Exp.match_
          (tuple (evars xs))
          (complete (map constructor_declaration cds))
      )

  (* Unsupported constructs. *)
  | Ptype_abstract, None ->
      let loc = decl.ptype_loc in
      raise_errorf ~loc "%s: cannot deal with abstract types." plugin

  | Ptype_open, _ ->
      let loc = decl.ptype_loc in
      raise_errorf ~loc "%s: cannot deal with open types." plugin

(* -------------------------------------------------------------------------- *)

(* [type_decl decl] generates the main visitor method associated with the type
   declaration [decl], as well as the necessary auxiliary methods. It returns
   no result. *)

let type_decl (decl : type_declaration) : unit =
  generate_concrete_method
    (tycon_visitor_method (Lident decl.ptype_name.txt))
    (plambda (pvar env) (visit_decl decl))

(* -------------------------------------------------------------------------- *)

(* [type_decls decls] processes the type declarations [decl] and produces a
   list of structure items. It is the main entry point inside the body of
   the functor [Setup]. *)

let type_decls (decls : type_declaration list) : structure =
  (* Analyze the type definitions, and populate our classes with methods. *)
  iter type_decl decls;
  (* Emit our preprocessor warnings (if any). *)
  warnings() @
  (* In the generated code, disable certain warnings, so that the user sees
     no warnings, even if she explicitly enables them. We disable warnings
     26, 27 (unused variables) and 4 (fragile pattern matching; a feature
     intentionally exploited by [iter2] and [map2]). *)
  [ with_warnings "-4-26-27" (
    (* Surround the generated code with floating attributes, which can be
       used as markers to find and review the generated code. We use this
       mechanism to show the generated code in the documentation. *)
    floating "VISITORS.BEGIN" [] ::
      (* Produce a class definition. *)
      (* Our classes are parameterized over the type variable ['env]. They are
         also parameterized over the type variable ['self], with a constraint
         that this is the type of [self]. This trick allows us to omit the types
         of the virtual methods, even if these types include type variables. *)
    dump X.concrete X.ancestors [ ty_self, Invariant ] pself X.name ::
    floating "VISITORS.END" [] ::
    []
  )]

end

(* -------------------------------------------------------------------------- *)

(* [type_decls decls] produces a list of structure items (that is, toplevel
   definitions) associated with the type declarations [decls]. It is the
   main entry point outside of the functor [Setup]. *)

let type_decls ~options ~path:_ (decls : type_declaration list) : structure =
  assert (decls <> []);
  let module Process = Setup(Parse(struct
    let loc = (VisitorsList.last decls).ptype_loc (* an approximation *)
    let options = options
    let decls = decls
  end)) in
  Process.type_decls decls

(* -------------------------------------------------------------------------- *)

(* Register our plugin with [ppx_deriving]. *)

let () =
  register (create plugin ~type_decl_str:type_decls ())
