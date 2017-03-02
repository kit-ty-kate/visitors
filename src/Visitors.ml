open VisitorsList
open Longident
open List
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience
open Ppx_deriving
open VisitorsPlugin
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

let generate_concrete_method m e oty =
  generate (concrete_method (visibility m) m e oty)

let generate_virtual_method m =
  generate (virtual_method (visibility m) m None)
    (* TEMPORARY type annotation *)

(* -------------------------------------------------------------------------- *)

(* The following brings [warning] and [warnings] into scope. *)

include WarningStore(struct end)

(* -------------------------------------------------------------------------- *)

(* We support parameterized type declarations. We require them to be regular.
   That is, for instance, if a type ['a term] is being defined, then every
   use of [_ term] in the definition should be ['a term]; it cannot be, say,
   [int term] or [('a * 'a) term]. *)

(* To enforce this, we check that, in every use of a local type constructor,
   the actual type parameters coincide with the formal type parameters. *)

let check_regularity loc tycon (formals : tyvars) (actuals : core_type list) =
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
  (* We support qualified names, and, in that case, use the last part of the
     qualified name to obtain the name of the visitor method. A qualified name
     must denote a nonlocal type. *)
  (* One might like to use [last tycon] directly as the name of the method, but
     that could (in theory) create a conflict with the names of other methods.
     In order to guarantee the absence of conflicts, we must use a nonempty
     prefix. *)
  "visit_" ^ Longident.last tycon

(* For every local record type constructor [tycon], there is an ascending
   method, which is invoked on the way up, in order to re-build some data
   structure. This method is virtual and exists only when the scheme is
   [fold]. *)

let tycon_ascending_method (tycon : string) : methode =
  "build_" ^ tycon

(* Type variables are treated as nonlocal type constructors, so they also have
   a descending method. We include a quote in the method name so as to ensure
   the absence of collisions. *)

let tyvar_visitor_method (tv : tyvar) : methode =
  "visit_'" ^ tv

(* For every data constructor [datacon], there is a descending visitor method,
   which is invoked on the way down, when this data constructor is discovered. *)

let datacon_descending_method (datacon : datacon) : methode =
  "visit_" ^ datacon

(* For every data constructor [datacon], there is a ascending visitor method,
   which is invoked on the way up, in order to re-build some data structure.
   This method is virtual and exists only when the scheme is [fold]. *)

let datacon_ascending_method (datacon : datacon) : methode =
  "build_" ^ datacon

(* At arity 2, for every sum type constructor [tycon] which has at least two
   data constructors, there is a failure method, which is invoked when the
   left-hand and right-hand arguments do not exhibit the same tags. *)

let failure_method (tycon : tycon) : methode =
  "fail_" ^ tycon

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

(* The variable [thing j] denotes an input value. *)

let thing (j : int) : variable =
  copy j "this"

let things : variable list =
  map thing (interval 0 arity)

(* The variable [this] is used only in the generation of [endo] visitors. *)

let this =
  thing 0

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

(* The variables [summary i] denote results of recursive calls.
   When the scheme is [MapReduce], each recursive call produces
   a pair; we use [result i] and [summary i] as the names of the
   pair components. *)

let summary (i : int) : variable =
  sprintf "s%d" i

let summaries (xs : _ list) : variable list =
  mapi (fun i _ -> summary i) xs

(* Naming conventions for type variables in type annotations. If ['a]
   is a type variable named by the user, we use ['a_i], where [i] is
   in [0..arity]. Indices [i] in the interval [0..arity) are used for
   the arguments of a visitor method. The index [arity] is used for
   the result of a visitor method. *)

(* If [scheme] is [Endo], then the argument and result must have the
   same type, so we do not introduce a variation in the type variables. *)

let variant (i : int) (alpha : tyvar) : tyvar =
  assert (0 <= i && i <= arity);
  if X.scheme = Endo then alpha else sprintf "%s_%d" alpha i

let variant (i : int) (ty : core_type) : core_type =
  subst_core_type (variant i) ty

(* [ty_monoid] is the type of monoid elements. *)

let ty_monoid =
  ty_any

(* [ty_env] is the type of the environment. Note that different visitor methods
   can have different types of environment. For now, we get away with using a
   wildcard, which can mean different things in different places. *)

(* TEMPORARY allow the type of the environment to be quantified *)

let ty_env =
  ty_any

(* -------------------------------------------------------------------------- *)

(* Construction of type annotations. *)

(* A visitor function takes an environment, followed with [arity] arguments,
   and produces a result. Thus, if [argument] and [result] are types, then the
   type of a visitor function has the following shape:

     ty_env ->
     argument_0 -> ... -> argument_{arity-1} ->
     result_{arity}

   where [ty_{i}] denotes a copy of the type [ty] whose type variables have
   been renamed by the renaming [variant i]. *)

let visitor_fun_type (argument : core_type) (result : core_type) : core_type =
  let argument i = variant i argument
  and result = variant arity result in
  ty_arrows (ty_env :: init 0 arity argument) result

(* [result_type scheme decl] is the result type of a visitor method associated
   with the type [decl]. *)

let rec result_type scheme (decl : type_declaration) : core_type =
  match scheme with
  | Iter ->
      (* An [iter] visitor method returns nothing. *)
      ty_unit
  | Map | Endo ->
      (* A [map] or [endo] visitor method for the type [_ foo] returns a
         value of type [_ foo]. Note that this is a non-constant skeleton. *)
      decl_type decl
  | Reduce ->
      (* A [reduce] visitor method returns a monoid element. *)
      ty_monoid
  | MapReduce ->
      (* A [mapreduce] visitor method returns a pair of the results produced
         by a [map] visitor method and by a [reduce] visitor method. *)
      Typ.tuple [ result_type Map decl; result_type Reduce decl ]
  | Fold ->
      (* This is where we have a problem. We would really like to allow the
         user to specify which skeleton should be used here, as we cannot
         guess it. We might allow it in the future. For now, we impose the
         monomorphic skeleton [_], which is not as general as we would like,
         since it requires the method to have monomorphic result type. *)
      ty_any

let result_type =
  result_type X.scheme

(* [visitor_method_type decl] is the type of the visitor method associated
   with the type [decl]. *)

let visitor_method_type (decl : type_declaration) : core_type =
  (* For now, this is a monomorphic type annotation. *)
  visitor_fun_type
    (decl_type decl)
    (result_type decl)

let visitor_method_type (decl : type_declaration) : core_type option =
  (* A type annotation is generated only if [polymorphic] is [true]. *)
  if X.polymorphic then Some (visitor_method_type decl) else None

(* -------------------------------------------------------------------------- *)

(* [bind rs ss] is a binding construct which, depending on the scheme, binds
   either the variables [rs], or the variables [ss], or both, using pair
   patterns. It is used to bind the results of recursive calls to visitor
   methods. *)

let bind (rs : variable list) (ss : variable list)
  : expression list -> expression -> expression =
  match X.scheme with
  | Iter
  | Map
  | Endo
  | Fold ->
      letn rs
  | Reduce ->
      letn ss
  | MapReduce ->
      letnp rs ss

(* -------------------------------------------------------------------------- *)

(* Assuming [ess] is a matrix of width [arity] and assuming [arity] is [1],
   [column ess] returns the first column of [ess], represented as a list. *)

let column (ess : 'a list list) : 'a list =
  assert (for_all (fun es -> length es = arity) ess);
  assert (arity = 1);
  map hd ess

(* Under the above assumptions about [ess], [ifeqphys ess rs e1 e2] constructs
   an [if/then/else] construct whose condition is the pointwise conjunction of
   physical equalities between [column ess] and [evars rs]. *)

let ifeqphys (ess : expression list list) (rs : variable list) e1 e2 =
  Exp.ifthenelse (eqphys (column ess) (evars rs)) e1 (Some e2)

(* -------------------------------------------------------------------------- *)

(* [call m es] emits a method call of the form [self#m es]. *)

let call (m : methode) (es : expression list) : expression =
  send self m es

(* -------------------------------------------------------------------------- *)

(* Access to the monoid operations. *)

let monoid_unit () : expression =
  assert (X.scheme = Reduce || X.scheme = MapReduce);
  call zero []

let monoid_law () : expression =
  assert (X.scheme = Reduce || X.scheme = MapReduce);
  call plus []

(* -------------------------------------------------------------------------- *)

(* [reduce es] reduces the expressions [es], that is, it combines them, using
   a monoid, which provides a unit and a binary operation. The reduction is
   performed left-to-right. This could be of importance if the monoid is not
   associative-commutative. *)

let reduce es =
  let unit = monoid_unit()
  and law = monoid_law() in
  fold_left1 (fun e1 e2 -> app law [e1; e2]) unit es

(* -------------------------------------------------------------------------- *)

(* [alias x ps] requires the pattern list [ps] to have length [arity]. If
   [scheme] is [Endo], then it further requires [arity] to be 1. It adds a
   binding of the variable [x], using an [as] pattern, at the top level of the
   pattern. The result is again packaged as a pattern list of length [arity].
   If scheme is not [Endo], then [alias x ps] is just [ps]. *)

let alias (x : variable) (ps : pattern list) : pattern list =
  assert (length ps = arity);
  match X.scheme with
  | Endo ->
      assert (arity = 1);
      map (fun p ->
        Pat.alias p (Location.mknoloc x)
      ) ps
  | _ ->
      ps

(* If [scheme] is [Endo], then [transmit x xs] is [x :: xs]. Otherwise, it is
   just [xs]. *)

let transmit x xs =
  match X.scheme with
  | Endo ->
      x :: xs
  | _ ->
      xs

(* -------------------------------------------------------------------------- *)

(* [hook m xs oty e] constructs a call of the form [self#m xs], and (as a side
   effect) generates a method [method m xs = e]. The free variables of the
   expression [e] must be (a subset of) [xs]. The type [oty], if given, is
   the type of the method. *)

(* Thus, by default, the expression [hook m xs e] behaves in the same way
   as the expression [e]. But a hook, named [m], allows this default to be
   overridden. *)

let hook (m : methode) (xs : variable list) (oty : core_type option) (e : expression) : expression =
  (* Generate a method. *)
  generate_concrete_method m (lambdas xs e) oty;
  (* Construct a method call. *)
  call m (evars xs)

(* -------------------------------------------------------------------------- *)

(* [vhook m xs] constructs a call of the form [self#m xs], and (as a side
   effect) generates a virtual method [method m: _]. *)

let vhook (m : methode) (xs : variable list) : expression =
  generate_virtual_method m;
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

(* If [ty] carries the attribute [@opaque], then we act as if there is nothing
   to visit. The nature of the type [ty], in that case, plays no role. *)

let rec visit_type (env_in_scope : bool) (ty : core_type) : expression =
  match env_in_scope, opacity ty.ptyp_attributes, ty.ptyp_desc with

  (* A type constructor [tycon] applied to type parameters [tys]. We handle
     the case where [env_in_scope] is false, so we construct a function of
     [env]. *)
  | false,
    NonOpaque,
    Ptyp_constr ({ txt = (tycon : Longident.t); _ }, tys) ->
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
    NonOpaque,
    Ptyp_var tv ->
      vhook (tyvar_visitor_method tv) []

  (* A tuple type. We handle the case where [env_in_scope] is true, as it
     is easier. *)
  | true,
    NonOpaque,
    Ptyp_tuple tys ->
      (* Construct a function that takes [arity] tuples as arguments. *)
      (* See [constructor_declaration] for comments. *)
      let xss = componentss tys in
      let subjects = evarss xss in
      let rs = results xss
      and ss = summaries xss in
      plambdas
        (alias this (ptuples (transpose arity (pvarss xss))))
        (bind rs ss (visit_types tys subjects)
          (let rec body scheme =
             match scheme with
             | Iter      -> unit()
             | Map       -> tuple (evars rs)
             | Endo      -> ifeqphys subjects rs (evar this) (body Map)
             | Reduce    -> reduce (evars ss)
             | MapReduce -> tuple [ body Map; body Reduce ]
             | Fold      -> (* Without loss of generality, re-build a tuple. *)
                            body Map
           in body X.scheme
          )
        )

  (* If [env_in_scope] does not have the desired value, wrap a recursive call
     within an application or abstraction. At most one recursive call takes
     place, so we never produce an eta-redex. *)
  | true, NonOpaque, (Ptyp_constr _ | Ptyp_var _) ->
     app (visit_type false ty) [evar env]
  | false, _, _ ->
     lambda env (visit_type true ty)

  (* If [ty] is marked opaque, then we ignore the structure of [ty] and carry
     out appropriate action, based on the current scheme. *)
  | true, Opaque, _ ->
      (* Construct a function that takes [arity] arguments. *)
      let xs = things in
      lambdas xs
        (let rec body scheme =
           match scheme with
           | Iter      -> unit()        (* At arity > 1, NO EQUALITY TEST takes place. *)
           | Map       -> evar (hd xs)  (* At arity > 1, this is an ARBITRARY choice. *)
           | Endo      -> body Map      (* Arity is 1, so this is fine. *)
           | Reduce    -> monoid_unit() (* This is fine. *)
           | MapReduce -> tuple [ body Map; body Reduce ]
           | Fold -> (* At arity 1, the best thing to do, without loss of
                        generality, is to behave as the identity, that is,
                        behave as in [map]. At arity > 1, it is debatable
                        whether we should make an arbitrary choice (like
                        [map] does) or invoke a virtual method whose
                        parameters are [env :: xs]. The issue with the
                        latter approach would be, how many distinct such
                        methods do we need?, how do we name them?, etc. *)
                     body Map
         in body X.scheme
        )

  (* An unsupported construct. *)
  | _, _, Ptyp_any
  | _, _, Ptyp_arrow _
  | _, _, Ptyp_object _
  | _, _, Ptyp_class _
  | _, _, Ptyp_alias _
  | _, _, Ptyp_variant _
  | _, _, Ptyp_poly _
  | _, _, Ptyp_package _
  | _, _, Ptyp_extension _ ->
      let loc = ty.ptyp_loc in
      raise_errorf ~loc
        "%s: cannot deal with the type %s.\n\
         Consider annotating it with [@opaque]."
        plugin
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

let constructor_declaration decl (cd : constructor_declaration) : case =

  (* A technical warning. One should not write "A of int[@opaque]".
     Instead, one should write "A of (int[@opaque])".
     In the case of records fields, we fix this silently, but in
     the case of data constructors with multiple fields, it is
     preferable to be strict. *)

  if opacity cd.pcd_attributes = Opaque then
    warning cd.pcd_loc
      (sprintf
         "%s: @opaque, attached to a data constructor, is ignored.\n\
          It should be attached to a type. Please use parentheses."
         plugin);

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
  let subjects = evarss xss in

  (* Get the name of this data constructor. *)
  let datacon = cd.pcd_name.txt in
  (* Create new names [rs] for the results of the recursive calls of visitor
     methods. *)
  let rs = results xss
  and ss = summaries xss in

  (* Prepare a call to [hook] which creates the visitor method associated
     with this data constructor. We must compute the name [m] of the method,
     the list [args] of its arguments, and the list [ty_args] of the types of
     its arguments, from which we build the type [ty_method] of the method. *)

  let tyss = hextend tys arity variant in (* [tyss] are the types of [xss] *)
  let ty_this = decl_type decl in         (* the type of [this] *)

  let m = datacon_descending_method datacon
  and    args =    env :: transmit    this (flatten  xss)
  and ty_args = ty_env :: transmit ty_this (flatten tyss)
  and ty_result = variant arity (result_type decl) in
  let ty_method = ty_arrows ty_args ty_result in

  let hook = hook m args (Some ty_method) in

  (* Construct a case for this data constructor in the visitor method
     associated with this sum type. This case analyzes a tuple of width
     [arity]. After binding the components [xss], we call the descending
     method associated with this data constructor, with arguments [env] and
     [xss]. This method binds the variables [rs] to the results of the
     recursive calls to visitor methods, then (if the variety is [iter])
     returns a unit value or (if the variety is [map]) reconstructs a tree
     node. *)
  (* If the scheme is [endo], then we additionally bind the variable [this] to
     the whole memory block. This variable is transmitted to the descending
     method. When the time comes to allocate a new memory block, if the
     components of the new block are physically equal to the components of the
     existing block, then the address of the existing block is returned;
     otherwise a new block is allocated, as in [map]. *)
  Exp.case
    (ptuple (alias this (map (pconstr datacon) pss)))
    (hook
       (bind rs ss
          (visit_types tys subjects)
          (let rec body scheme =
             match scheme with
             | Iter      -> unit()
             | Map       -> constr datacon (build rs)
             | Endo      -> ifeqphys subjects rs (evar this) (body Map)
             | Reduce    -> reduce (evars ss)
             | MapReduce -> tuple [ body Map; body Reduce ]
             | Fold      -> vhook (datacon_ascending_method datacon) (env :: rs)
           in body X.scheme
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
  let xs = things in
  assert (length xs = arity);

  match decl.ptype_kind, decl.ptype_manifest with

  (* A type abbreviation. *)
  | Ptype_abstract, Some ty ->
      visit_type true ty

  (* A record type. *)
  | Ptype_record (lds : label_declaration list), _ ->
      let labels, tys = ld_labels lds, ld_tys (fix lds) in
      (* See [constructor_declaration] for comments. *)
      let subjects = accesses xs labels in
      lambdas xs (
        let rs = results labels
        and ss = summaries labels in
        bind rs ss (visit_types tys subjects)
          (let rec body scheme =
             match scheme with
             | Iter      -> unit()
             | Map       -> record (combine labels (evars rs))
             | Endo      -> ifeqphys subjects rs (evar (hd xs)) (body Map)
             | Reduce    -> reduce (evars ss)
             | MapReduce -> tuple [ body Map; body Reduce ]
             | Fold      -> vhook (tycon_ascending_method tycon) (env :: rs)
           in body X.scheme
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
        let ty_xs = init 0 arity (fun i -> variant i (decl_type decl)) in
        let ty_result = variant arity (result_type decl) in
        let hook =
          hook
            (failure_method tycon)
            (env :: xs)
            (Some (ty_arrows (ty_env :: ty_xs) ty_result))
        in
        Exp.case
          (ptuple (pvars xs))
          (hook
            (efail (tycon_visitor_method (Lident tycon)))
          )
      in
      let complete (cs : case list) : case list =
        if arity = 1 || length cs <= 1 then cs else cs @ [ default() ]
      in
      lambdas xs (
        Exp.match_
          (tuple (evars xs))
          (complete (map (constructor_declaration decl) cds))
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
    (visitor_method_type decl)

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
