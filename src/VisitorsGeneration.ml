open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

(* This module offers helper functions for code generation. *)

(* -------------------------------------------------------------------------- *)

type variable = string
type datacon = string
type label = string
type classe = string
type methode = string
type tyvar = string

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

(* [efail s] generates a call to [VisitorsRuntime.fail]. The parameter [s] is
   a string, which could represent the place where a failure occurred, or the
   reason why a failure occurred. As of now, it is unused. *)

let efail : expression =
  eident (Ldot (Lident "VisitorsRuntime", "fail"))

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

(* [class1 params name self fields] builds a class declaration and packages it
   as a structure item. (This implies that it cannot be recursive with other
   class declarations). The class is declared virtual if and only if at least
   one virtual method explicitly appears as part of the list [fields]. *)

let is_virtual_method (field : class_field) : bool =
  match field.pcf_desc with
  | Pcf_method (_, _, Cfk_virtual _) ->
      true
  | _ ->
      false

let has_virtual_method (fields : class_field list) : bool =
  List.exists is_virtual_method fields

let class1
  (params : (core_type * variance) list)
  (name : classe)
  (self : pattern)
  (fields : class_field list)
  : structure_item =
  Str.class_ [{
    pci_virt = if has_virtual_method fields then Virtual else Concrete;
    pci_params = params;
    pci_name = mknoloc name;
    pci_expr = Cl.structure (Cstr.mk self fields);
    pci_loc = !default_loc;
    pci_attributes = [];
  }]

(* -------------------------------------------------------------------------- *)

(* [inherit_ c tys] builds an [inherit] clause, where the superclass is [c]
   and its actual type parameters are [tys]. No [super] identifier is bound. *)

let inherit_ (c : classe) (tys : core_type list) : class_field =
  Cf.inherit_ Fresh (Cl.constr (mknoloc (Lident c)) tys) None

(* -------------------------------------------------------------------------- *)

(* [concrete_method m e] constructs a public method whose name is [m] and
   whose body is [e]. *)

let concrete_method (m : methode) (e : expression) : class_field =
  Cf.method_
    (mknoloc m)
    Public
    (Cf.concrete Fresh e)

(* -------------------------------------------------------------------------- *)

(* [virtual_method m e] constructs a virtual public method whose name is [m]
   and whose type is unspecified. *)

let virtual_method (m : methode) : class_field =
  Cf.method_
    (mknoloc m)
    Public
    (Cf.virtual_ (Typ.any()))

(* -------------------------------------------------------------------------- *)

(* [method_name] extracts a method name out of a class field (which must be
   a method definition). *)

let method_name (cf : class_field) : string =
  match cf.pcf_desc with
  | Pcf_method (m, _, _) ->
      m.txt
  | _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* [is_virtual] tests whether a class field (which must be a method definition)
   represents a virtual method. *)

let is_virtual (cf : class_field) : bool =
  match cf.pcf_desc with
  | Pcf_method (_, _, Cfk_virtual _) ->
      true
  | Pcf_method (_, _, Cfk_concrete _) ->
      false
  | _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* [send o m es] produces a call to the method [o#m] with arguments [es]. *)

let send (o : variable) (m : methode) (es : expression list) : expression =
  app (Exp.send (evar o) m) es

(* -------------------------------------------------------------------------- *)

(* A facility for generating several classes at the same time. We maintain,
   for each generated class, a list of class fields. The call [generate c cf]
   adds the class field [cf] to the list associated with [c]. The call [dump c]
   returns the list of class fields associated with [c]. *)

module ClassFieldStore (X : sig end) : sig

  val generate: classe -> class_field -> unit
  val dump: classe -> class_field list

end = struct

  module StringMap =
    Map.Make(String)

  let store : class_field list StringMap.t ref =
    ref StringMap.empty

  let get c =
    try StringMap.find c !store with Not_found -> []

  let generate c cf =
    store := StringMap.add c (cf :: get c) !store

  let dump c =
    let fields = List.rev (get c) in
    (* Move all of the virtual methods up front. If two virtual methods have
       the same name, keep only one of them. This is useful because we allow
       a virtual method declaration to be generated several times. In fact,
       OCaml supports this, but it looks tidier if we remove duplicates. *)
    let virtual_fields, concrete_fields = List.partition is_virtual fields in
    let cmp cf1 cf2 = compare (method_name cf1) (method_name cf2) in
    let virtual_fields = VisitorsList.weed cmp virtual_fields in
    virtual_fields @ concrete_fields

end
