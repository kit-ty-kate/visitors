open Longident
open List
let sprintf = Printf.sprintf
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience
open Ppx_deriving
open VisitorsList
open VisitorsAnalysis
open VisitorsGeneration

let plugin =
  "visitors"

(* -------------------------------------------------------------------------- *)

(* Option processing. *)

(* The option [irregular = true] suppresses the regularity check and allows a
   local parameterized type to be instantiated; e.g., the definition of ['a t]
   can then refer to [int t]. However, in most situations, this will lead to
   ill-typed generated code. The generated code should be well-typed if [t] is
   always instantiated in the same manner, e.g., if there are references to
   [int t] but not to other instances of [t]. *)

let irregular =
  ref false

(* The option [max], accompanied with an integer parameter, allows setting the
   maximum arity up to which we generate code. If its value is 1, we generate
   a class [visitor]; if its value is 2, we also generate a class [visitor2];
   and so on. *)

let max_arity =
  ref 2

let valid_max_arity (s : string) : bool =
  try int_of_string s > 0 with Failure _ -> false

let parse_options options =
  (* The default values are specified here. *)
  max_arity := 2;
  irregular := false;
  (* Analysis. *)
  iter (fun (o, e) ->
    match o, e with
    | "irregular", { pexp_desc = Pexp_construct ({ txt = Lident "true"; _}, None) ; _ } ->
        irregular := true
    | "irregular", { pexp_desc = Pexp_construct ({ txt = Lident "false"; _}, None) ; _ } ->
        ()
    | "irregular", _ ->
        let loc = e.pexp_loc in
        raise_errorf ~loc "%s: option %s expects a Boolean value." plugin o
    | "max", { pexp_desc = Pexp_constant (Pconst_integer (s, None)) ; _ }
      when valid_max_arity s ->
        max_arity := int_of_string s
    | "max", _ ->
        let loc = e.pexp_loc in
        raise_errorf ~loc "%s: option %s expects a positive integer value." plugin o
    | _ ->
        let loc = e.pexp_loc in
        raise_errorf ~loc "%s: option %s is not supported." plugin o
  ) options

(* -------------------------------------------------------------------------- *)

(* We support parameterized type declarations, we require them to be regular.
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
  if not !irregular && not (
    fold_left2 (fun ok formal actual ->
      ok && actual.ptyp_desc = Ptyp_var formal
    ) true formals actuals
  ) then
    raise_errorf ~loc "%s: the type constructor %s is irregular." plugin tycon

(* -------------------------------------------------------------------------- *)

(* Per-run global state. *)

module Run (X : sig

  (* The type declarations that we are processing. *)
  val decls : type_declaration list

  (* The arity of the generated code, e.g., 1 if one wishes to generate [visitor],
     [iter] and [map], 2 if one wishes to generate [visitor2], [iter2] and [map2],
     and so on. *)
  val arity: int

end) = struct

let is_local =
  is_local X.decls

let arity =
  X.arity

(* As we generate several classes at the same time, we maintain, for each
   generated class, a list of methods that we generate as we go. The following
   line brings [generate] and [dump] into scope. *)

include ClassFieldStore(struct end)

(* -------------------------------------------------------------------------- *)

(* Public naming conventions. *)

(* We support multiple arities, e.g., we can also generate [visitor2], [iter2],
   [map2]. Our naming scheme is as follows. *)

let scheme (c : classe) : classe =
  if arity = 1 then
    (* No need for the suffix [1]. *)
    c
  else
    sprintf "%s%d" c arity

(* We generate three classes: a [visitor] base class and two subclasses, [iter]
   and [map]. *)

let cvisitor : classe =
  scheme "visitor"

let citer : classe =
  scheme "iter"

let cmap : classe =
  scheme "map"

(* For every type constructor [tycon], there is a visitor method, also called
   a descending method, as it is invoked when going down into the tree. *)

let tycon_visitor_method (tycon : Longident.t) : methode =
  (* We support qualified names, and, in that case, use the last part of the
     qualified name to obtain the name of the visitor method. A qualified name
     is probably a nonlocal type, that is, not part of the current set of type
     declarations. *)
  (* I would like to use [last tycon] directly as the name of the method, but
     that could (in theory) create a conflict with the names of other methods.
     In order to guarantee the absence of conflicts, we must use a nonempty
     prefix. *)
  "visit_" ^ last tycon

(* Type variables are treated as nonlocal type constructors, so they also have
   a descending method. We include a quote in the method name so as to ensure
   the absence of collisions. *)

let tyvar_visitor_method (tv : tyvar) : methode =
  "visit_'" ^ tv

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

(* For every record type constructor [tycon], there is an ascending visitor
   method. (See above description.) *)

let record_ascending_method (tycon : tycon) : methode =
  "build_" ^ tycon

(* At arity 2, for every sum type constructor [tycon] which has at least two
   data constructors, there is a failure method, which is invoked when the
   left-hand and right-hand arguments do not exhibit the same tags. *)

let failure_method (tycon : tycon) : methode =
  "fail_" ^ tycon

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
  copy j (sprintf "_%s" tycon)

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

(* [hook m xs e] constructs a call of the form [self#m xs], and (as a side
   effect) generates a method [method m xs = e]. The free variables of the
   expression [e] must be (a subset of) [xs]. *)

(* Thus, by default, the expression [hook m xs e] behaves in the same way
   as the expression [e]. But a hook, named [m], allows this default to be
   overridden. *)

let hook (m : string) (xs : string list) (e : expression) : expression =
  (* Generate a method in the class [visitor]. We note that the formal
     parameters [xs] don't need a type annotation: because this method has a
     call site, its type can be inferred. *)
  generate cvisitor (concrete_method m (lambdas xs e));
  (* Construct a method call. *)
  send self m (evars xs)

(* [hooks m xs e1 e2] constructs a call of the form [self#m xs], and (as a side
   effect) generates a virtual method named [m] in the class [visitor]. It also
   generates an implementation [method m xs = e1] in the subclass [iter] and an
   implementation [method m xs = e2] in the subclass [map]. The free variables
   of the expressions [e1] and [e2] must be (a subset of) [xs]. *)

let hooks (m : string) (xs : variable list)
          (e1 : expression) (e2 : expression) : expression =
  (* Generate a declaration of [m] as a virtual method in the class [visitor].
     We note that it does not need a type annotation: because we have used the
     trick of parameterizing the class over its ['self] type, no annotations
     at all are needed. *)
  generate cvisitor (virtual_method m);
  (* Define this method in the subclass [iter]. *)
  (* An ad hoc detail: we assume that [e1] does not use its arguments [xs],
     so, in order to avoid OCaml's warning about unused variables, we replace
     them with wildcard patterns. *)
  generate citer (concrete_method m (plambdas (wildcards xs) e1));
  (* Define this method in the subclass [map]. *)
  generate cmap (concrete_method m (lambdas xs e2));
  (* Construct a method call. *)
  send self m (evars xs)

(* -------------------------------------------------------------------------- *)

(* [visit_type ty] builds a small expression that represents the visiting code
   associated with the OCaml type [ty]. For instance, if [ty] is a local type
   constructor, this could be a call to the visitor method associated with
   this type constructor. The expression constructed by [visit_type ty] may
   refer to the variables [self] and [env]. *)

let rec visit_type (ty : core_type) : expression =
  match ty with

  (* A type constructor [tycon] applied to type parameters [tys]. *)
  | { ptyp_desc = Ptyp_constr ({ txt = (tycon : Longident.t); _ }, tys); _ } ->
      begin match is_local tycon with
      | Some formals ->
          (* [tycon] is a local type constructor, whose formal type parameters
             are [formals]. *)
          (* Check that [tys] and [formals] coincide. If they do not, we cannot
             handle this type declaration. *)
          check_regularity ty.ptyp_loc (last tycon) formals tys;
          (* Return the visitor method associated with [tycon], applied to the
             environment [env]. Contrary to the nonlocal case (below), this
             method must not be applied to the visitor functions associated with
             [tys]. *)
          send self
            (tycon_visitor_method tycon)
            [evar env]
      | None ->
          (* [tycon] is a nonlocal type constructor. Declare a virtual method for
             it. We rely on the fact that it is permitted for a virtual method to
             be declared several times. *)
          generate cvisitor (virtual_method (tycon_visitor_method tycon));
          (* Return the visitor method associated with [tycon], applied to the
             visitor functions associated with [tys] and to [env]. *)
          send self
            (tycon_visitor_method tycon)
            (map lambda_env_visit_type tys @ [evar env])
      end

  (* A type variable [tv] is treated like a nonlocal type constructor. *)
  | { ptyp_desc = Ptyp_var tv; _ } ->
      generate cvisitor (virtual_method (tyvar_visitor_method tv));
      send self
        (tyvar_visitor_method tv)
        [evar env]

  (* A tuple type. *)
  | { ptyp_desc = Ptyp_tuple tys; _ } ->
      (* Construct a function that takes [arity] tuples as arugments. *)
      (* In the case of tuples, as opposed to data constructors, we do not
         call an ascending method, as we would need one method name per tuple
         type, and that would be messy. Instead, we make the most general
         choice of ascending computation, which is to rebuild a tuple on the
         way up. Fortunately, this should always be well-typed, I believe. We
         do not need a descending method either because there is no case
         analysis. *)
      let xss = componentss tys in
      plambdas
        (ptuples (transpose arity (pvarss xss)))
        (tuple (visit_types tys (evarss xss)))

  (* An unsupported construct. *)
  | _ ->
      let loc = ty.ptyp_loc in
      raise_errorf ~loc "%s: cannot deal with the type %s." plugin
        (string_of_core_type ty)

and lambda_env_visit_type ty =
  lambda env (visit_type ty)

and visit_types tys (ess : expression list list) : expression list =
  (* The matrix [ess] is indexed first by component, then by index [j].
     Thus, to each type [ty], corresponds a row [es] of expressions,
     whose length is [arity]. *)
  assert (is_matrix (length tys) arity ess);
  map2 (fun ty es ->
    app (visit_type ty) es
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
     [xss]. This method is implemented in the [visitor] class. It binds the
     variables [rs] to the results of the recursive calls to visitor methods,
     then calls the ascending method associated with this data constructor,
     with arguments [rs]. This method is declared in the [visitor] class and
     implemented in the subclasses [iter] and [map]. In the subclass [iter],
     it always returns a unit value. In the subclass [map], it reconstructs a
     tree node. *)
  Exp.case
    (ptuple (map (pconstr datacon) pss))
    (hook (datacon_descending_method datacon) (env :: flatten xss)
       (letn
          rs (visit_types tys (evarss xss))
          (hooks (datacon_ascending_method datacon) rs
            (unit())
            (constr datacon (build rs))
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
      visit_type ty

  (* A record type. *)
  | Ptype_record (lds : label_declaration list), _ ->
      let labels, tys = ld_labels lds, ld_tys lds in
      (* See [constructor_declaration] for comments. *)
      lambdas xs (
        let rs = results labels in
        letn rs (visit_types tys (accesses xs labels))
          (hooks (record_ascending_method tycon) rs
            (unit())
            (record (combine labels (evars rs)))
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
            (* This method ignores its arguments, which can cause warnings. *)
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
   declaration [decl], as well as the necessary auxiliary methods. *)

let type_decl (decl : type_declaration) : unit =
  generate cvisitor (
    concrete_method
      (tycon_visitor_method (Lident decl.ptype_name.txt))
      (plambda penv (visit_decl decl))
  )

end

(* -------------------------------------------------------------------------- *)

(* [type_decls decls arity] produces a list of structure items (that is,
   toplevel definitions) associated with the type declarations [decls] at
   arity [arity]. *)

(* Our classes are parameterized over the type variable ['env]. They are also
   parameterized over the type variable ['self], with a constraint that this
   is the type of [self]. This trick allows us to omit the types of the
   virtual methods, even if these types include type variables. *)

let type_decls (decls : type_declaration list) (arity : int) : structure =
  let module R = Run(struct let decls = decls let arity = arity end) in
  let open R in
  (* Generate [inherit] clauses for the subclasses. *)
  let actuals = [ ty_self; ty_env ] in
  generate citer (inherit_ cvisitor actuals);
  generate cmap  (inherit_ cvisitor actuals);
  (* Analyze the type definitions, and populate our classes with methods. *)
  iter type_decl decls;
  (* Produce class definitions. *)
  let formals = [ ty_self, Invariant; ty_env, Invariant ] in
  map
    (fun c -> class1 formals c pself (dump c))
    [ cvisitor; citer; cmap ]

(* [type_decls arity decls] produces a list of structure items (that is,
   toplevel definitions) associated with the type declarations [decls]. *)

let type_decls ~options ~path:_ (decls : type_declaration list) : structure =
  parse_options options;
  (* Generate classes at arity 1, 2, ... up to [!max_arity]. *)
  flatten (map (type_decls decls) (interval 1 (!max_arity + 1)))

(* -------------------------------------------------------------------------- *)

(* We disallow the syntax [[%derive.visitors: ty]], where [ty] is a type.
   Indeed, we cannot produce meaningful visiting code outside of a class
   declaration. (We could create a fresh instance of the class [iter],
   but such a visitor does nothing. It is necessary to override at least
   one method in order to obtain a nontrivial visitor.) *)

let disallowed ty =
  let loc = ty.ptyp_loc in
  raise_errorf ~loc "%s: the syntax [%%derive.%s] is disallowed." plugin plugin

(* -------------------------------------------------------------------------- *)

(* Register our plugin with [ppx_deriving]. *)

let () =
  register (create plugin
    ~core_type:disallowed
    ~type_decl_str:type_decls
    ()
  )
