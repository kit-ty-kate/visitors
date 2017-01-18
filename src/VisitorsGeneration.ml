open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience
open VisitorsList

(* This module offers helper functions for code generation. *)

(* -------------------------------------------------------------------------- *)

type variable = string
type datacon = string
type label = string
type classe = string
type methode = string
type tyvar = string

(* -------------------------------------------------------------------------- *)

(* [unit] produces a unit constant. [tuple] produces a tuple. [record]
   produces a record. These functions already exist; we redefine them without
   any optional arguments so as avoid OCaml's warning 48 (implicit elimination
   of optional arguments). *)

let unit() =
  unit()

let tuple es =
  tuple es

let record les =
  record les

(* -------------------------------------------------------------------------- *)

(* [number i thing] constructs an English description of "[i] thing(s)". *)

let number i s =
  match i with
  | 0 ->
      Printf.sprintf "zero %s" s
  | 1 ->
      Printf.sprintf "one %s" s
  | _ ->
      Printf.sprintf "%d %ss" i s

(* -------------------------------------------------------------------------- *)

(* [eident] converts a (possibly-qualified) identifier to an expression. *)

let eident (id : Longident.t) : expression =
  Exp.ident (mknoloc id)

(* -------------------------------------------------------------------------- *)

(* [pvars] converts a list of variables to a list of patterns. *)

let pvars (xs : variable list) : pattern list =
  List.map (fun x -> pvar x) xs

(* [evars] converts a list of variables to a list of expressions. *)

let evars (xs : variable list) : expression list =
  List.map (fun x -> evar x) xs

(* [pvarss] converts a matrix of variables to a matrix of patterns. *)

let pvarss (xss : variable list list) : pattern list list =
  List.map pvars xss

(* [evarss] converts a matrix of variables to a matrix of expressions. *)

let evarss (xss : variable list list) : expression list list =
  List.map evars xss

(* -------------------------------------------------------------------------- *)

(* [wildcards] converts a list of anything to a list of wildcard patterns. *)

let wildcards xs =
  List.map (fun _ -> Pat.any()) xs

(* -------------------------------------------------------------------------- *)

(* [plambda p e] constructs a function [fun p -> e]. *)

let plambda (p : pattern) (e : expression) : expression =
  Exp.fun_ Nolabel None p e

(* [lambda x e] constructs a function [fun x -> e]. *)

let lambda (x : variable) (e : expression) : expression =
  plambda (pvar x) e

(* [plambdas ps e] constructs a multi-argument function [fun ps -> e]. *)

let plambdas (ps : pattern list) (e : expression) : expression =
  List.fold_right plambda ps e

(* [lambdas xs e] constructs a multi-argument function [fun xs -> e]. *)

let lambdas (xs : variable list) (e : expression) : expression =
  List.fold_right lambda xs e

(* -------------------------------------------------------------------------- *)

(* [app] works like [Ast_convenience.app] (which it shadows), except it avoids
   constructing nested applications of the form [(f x) y], transforming them
   instead into a single application [f x y]. The difference is probably just
   cosmetic. *)

let app (e : expression) (es2 : expression list) : expression =
  match e.pexp_desc with
  | Pexp_apply (e1, les1) ->
      let les2 = List.map (fun e -> Label.nolabel, e) es2 in
      { e with pexp_desc = Pexp_apply (e1, les1 @ les2) }
  | _ ->
      app e es2

(* -------------------------------------------------------------------------- *)

(* [sequence es] constructs a sequence of the expressions in the list [es]. *)

let sequence (es : expression list) : expression =
  (* Using [fold_right1] instead of [List.fold_right] allows us to get
     rid of a final [()] constant at the end of the sequence. Cosmetic. *)
  fold_right1
    (fun e accu -> Exp.sequence e accu)
    es
    (unit())

(* -------------------------------------------------------------------------- *)

(* [vblet1 vb e] constructs a single [let] binding. *)

let vblet1 (vb : value_binding) (e : expression) : expression =
  Exp.let_ Nonrecursive [vb] e

(* [let1 x e1 e2] constructs a single [let] binding. *)

let let1 (x : variable) (e1 : expression) (e2 : expression) : expression =
  vblet1 (Vb.mk (pvar x) e1) e2

(* [vbletn vbs e] constructs a series of nested [let] bindings. *)

let vbletn (vbs : value_binding list) (e : expression) : expression =
  List.fold_right vblet1 vbs e

(* [letn xs es e] constructs a series of nested [let] bindings. *)

let letn (xs : variable list) (es : expression list) (e : expression) =
  List.fold_right2 let1 xs es e

(* -------------------------------------------------------------------------- *)

(* [access x label] constructs a record access expression [x.label]. *)

let access (x : variable) (label : label) : expression =
  Exp.field (evar x) (mknoloc (Lident label))

(* [accesses labels xs] constructs a matrix of record access expressions of
   the form [x.label]. There is a row for every [label] and a column for every
   [x]. *)

let accesses (xs : variable list) (labels : label list) : expression list list =
  List.map (fun label -> List.map (fun x -> access x label) xs) labels

(* -------------------------------------------------------------------------- *)

(* [ptuple] is [Ast_convenience.ptuple], deprived of its optional arguments. *)

let ptuple (ps : pattern list) : pattern =
  ptuple ps

(* [ptuples] is [map ptuple]. *)

let ptuples (pss : pattern list list) : pattern list =
  List.map ptuple pss

(* -------------------------------------------------------------------------- *)

(* [eassertfalse] is the expression [assert false]. *)

let eassertfalse : expression =
  Exp.assert_ (Exp.construct (mknoloc (Lident "false")) None)

(* -------------------------------------------------------------------------- *)

(* [eforce e] is the expression [Lazy.force e]. *)

let eforce : expression =
  eident (Longident.parse "Lazy.force")
    (* danger: the module name [Lazy] must not be shadowed. *)

let eforce (e : expression) : expression =
  app eforce [e]

(* -------------------------------------------------------------------------- *)

(* [eqphy e1 e2] is the expression [e1 == e2]. *)

let eqphy : expression =
  eident (Longident.parse "Pervasives.==")
    (* danger: the module name [Pervasives] must not be shadowed. *)

let eqphy (e1 : expression) (e2 : expression) : expression =
  app eqphy [e1; e2]

(* -------------------------------------------------------------------------- *)

(* [efail s] generates a call to [VisitorsRuntime.fail]. The parameter [s] is
   a string, which could represent the place where a failure occurred, or the
   reason why a failure occurred. As of now, it is unused. *)

let efail : expression =
  eident (Ldot (Lident "VisitorsRuntime", "fail"))
    (* danger: the module name [VisitorsRuntime] must not be shadowed. *)

let efail (_ : string) : expression =
  app efail [ unit() ]

(* -------------------------------------------------------------------------- *)

(* [letopen m e] produces a single [let open!] binding. The bang character
   indicates intentional shadowing and disables OCaml's warning 44. *)

let letopen (m : Longident.t) (e : expression) : expression =
  Exp.open_ Override (mknoloc m) e

(* [letopen ms e] produces a series of [let open!] bindings. *)

let letopen (ms : Longident.t list) (e : expression) : expression =
  List.fold_right letopen ms e

(* [stropen m] produces a single [open!] declaration. *)

let stropen (m : Longident.t) : structure_item =
  Str.open_ (Opn.mk ~override:Override (mknoloc m))

(* [stropen ms] produces a series of [open!] declarations. *)

let stropen (ms : Longident.t list) : structure =
  List.map stropen ms

(* -------------------------------------------------------------------------- *)

(* [include_ e] constructs an [include] declaration. *)

let include_ (e : module_expr) : structure_item =
  Str.include_ {
    pincl_mod = e;
    pincl_loc = Location.none;
    pincl_attributes = [];
  }

(* -------------------------------------------------------------------------- *)

(* [floating s items] produces a floating attribute whose name is [s] and
   whose payload is the list of structure items [items]. *)

let floating (s : string) (items : structure) : structure_item =
  Str.attribute (mknoloc s, PStr items)

(* -------------------------------------------------------------------------- *)

(* [with_warnings w items] wraps the structure items [items] in such a way
   that the warning directive [w] is applied to these items. Technically, this
   is done by emitting [include struct [@@@ocaml.warning <w>] <items> end]. *)

let with_warnings (w : string) (items : structure_item list) : structure_item =
  include_ (Mod.structure (
     floating "ocaml.warning" [ Str.eval (Exp.constant (Const.string w)) ]
  :: items
  ))

(* -------------------------------------------------------------------------- *)

(* [class1 concrete ancestors params name self fields] builds a class
   declaration and packages it as a structure item. (This implies that it
   cannot be recursive with other class declarations). *)

let class1
  (concrete : bool)
  (params : (core_type * variance) list)
  (name : classe)
  (self : pattern)
  (fields : class_field list)
  : structure_item =
  Str.class_ [{
    pci_virt = if concrete then Concrete else Virtual;
    pci_params = params;
    pci_name = mknoloc name;
    pci_expr = Cl.structure (Cstr.mk self fields);
    pci_loc = !default_loc;
    pci_attributes = [];
  }]

(* -------------------------------------------------------------------------- *)

(* [inherit_ c tys] builds an [inherit] clause, where the superclass is [c]
   and its actual type parameters are [tys]. No [super] identifier is bound. *)

let inherit_ (c : Longident.t) (tys : core_type list) : class_field =
  Cf.inherit_ Fresh (Cl.constr (mknoloc c) tys) None

(* -------------------------------------------------------------------------- *)

(* An algebraic data type of the methods that we generate. These include
   concrete methods (with code) and virtual methods (without code). They may
   be public or private. The method type is not given -- it is inferred by
   OCaml. *)

type meth =
  Meth of private_flag * methode * expression option

let concrete_method p m e =
  Meth (p, m, Some e)

let virtual_method p m =
  Meth (p, m, None)

(* -------------------------------------------------------------------------- *)

(* Converting a method description to OCaml abstract syntax. *)

let oe2cfk (oe : expression option) : class_field_kind =
  match oe with
  | Some e ->
      Cf.concrete Fresh e
  | None ->
      Cf.virtual_ (Typ.any())

let meth2cf (Meth (p, m, oe)) : class_field =
  Cf.method_ (mknoloc m) p (oe2cfk oe)

(* -------------------------------------------------------------------------- *)

(* [method_name] extracts a method name out of a method description. *)

let method_name (Meth (_, m, _)) : string =
  m

(* -------------------------------------------------------------------------- *)

(* [is_virtual] tests whether a method description represents a virtual
   method. *)

let is_virtual (Meth (_, _, oe)) : bool =
  oe = None

(* -------------------------------------------------------------------------- *)

(* [send o m es] produces a call to the method [o#m] with arguments [es]. *)

let send (o : variable) (m : methode) (es : expression list) : expression =
  app (Exp.send (evar o) m) es

(* -------------------------------------------------------------------------- *)

(* An algebraic data type of the ``hoisted expressions'' that we generate. *)

(* A ``hoisted expression'' is evaluated at most once after the object is
   allocated. Its value is stored in an instance field. We allow such an
   expression to reference [self], as long as it does not actually invoke any
   methods. *)

type hoisted =
  Hoisted of string     (* the name of the instance field *)
           * expression (* the hoisted expression *)

(* -------------------------------------------------------------------------- *)

(* Converting a hoisted field description to OCaml abstract syntax. *)

(* We generate a mutable field declaration, followed with an initialization:

     val mutable x =  lazy (assert false)
     initializer x <- lazy e

   We must do this in two steps because the expression [e] might contain
   references to [self], which are invalid in a field declaration, whereas
   they are allowed in an initializer.

   The potential danger in this idiom lies in forcing [x] before the
   initializer has finished running, leading to an assertion failure.
   This should not happen if [e] does not perform any method calls
   or read any fields. *)

let hoisted2cf (Hoisted (x, e)) : class_field list =
  [
    Cf.val_ (mknoloc x) (Mutable) (Cf.concrete Fresh (Exp.lazy_ eassertfalse));
    Cf.initializer_ (Exp.setinstvar (mknoloc x) (Exp.lazy_ e))
  ]

(* -------------------------------------------------------------------------- *)

(* A facility for generating a class. *)

module ClassFieldStore (X : sig end) : sig

  (* [generate meth] adds [meth] to the list of methods. *)
  val generate: meth -> unit

  (* [hoist e] causes the expression [e] to be hoisted, that is, computed
     once after the object is allocated. The result of evaluating [e] is
     stored in a field. The call [hoist e] returns an expression which
     reads this field. *)
  val hoist: expression -> expression

  (* [dump concrete ancestors params self c] returns a class definition. *)
  val dump:
    bool ->
    Longident.t list ->
    (core_type * variance) list ->
    pattern ->
    classe ->
    structure_item

end = struct

  let meths : meth list ref =
    ref []

  let generate meth =
    meths := meth :: !meths

  let dump () : class_field list =
    let methods = List.rev !meths in
    (* Move all of the virtual methods up front. If two virtual methods have
       the same name, keep only one of them. This is useful because we allow
       a virtual method declaration to be generated several times. In fact,
       OCaml supports this, but it looks tidier if we remove duplicates. *)
    let virtual_methods, concrete_methods = List.partition is_virtual methods in
    let cmp meth1 meth2 = compare (method_name meth1) (method_name meth2) in
    let virtual_methods = VisitorsList.weed cmp virtual_methods in
    let methods = virtual_methods @ concrete_methods in
    List.map meth2cf methods

  let hoisted : hoisted list ref =
    ref []

  let fresh : unit -> int =
    let c = ref 0 in
    fun () ->
      let x = !c in
      c := x + 1;
      x

  let hoist (e : expression) : expression =
    let x = Printf.sprintf "h%d" (fresh()) in
    hoisted := Hoisted (x, e) :: !hoisted;
    eforce (evar x)

  let dump concrete ancestors params self c : structure_item =
    class1 concrete params c self (
      (* [inherit] clauses. *)
      (* We ARBITRARILY assume that every ancestor class is parameterized
         with ONE type parameter. *)
      List.map (fun c -> inherit_ c [ Typ.any() ]) ancestors @
      (* Hoisted expressions. *)
      List.flatten (List.map hoisted2cf (List.rev !hoisted)) @
      (* Methods. *)
      dump()
    )

end

(* -------------------------------------------------------------------------- *)

(* A facility for emitting preprocessor warnings. *)

(* Warnings must be emitted under the form of [ppwarning] attributes, placed
   in the generated code. This is not very convenient; we must store these
   warnings, waiting for a convenient time to emit them. *)

module WarningStore (X : sig end) : sig

  (* [warning loc msg] emits a warning. *)
  val warning: loc -> string -> unit

  (* [warnings()] returns a list of all warnings emitted so far. *)
  val warnings: unit -> structure

end = struct

  let warnings : attribute list ref =
    ref []

  let warning loc msg =
    warnings := Ast_mapper.attribute_of_warning loc msg :: !warnings

  let warnings () =
    let ws = !warnings in
    warnings := [];
    List.map (fun a -> Str.attribute a) (List.rev ws)

end
