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

(* -------------------------------------------------------------------------- *)

(* [pvars] converts a list of variables to a list of patterns. *)

let pvars (xs : variable list) : pattern list =
  List.map (fun x -> pvar x) xs

(* [evars] converts a list of expressions to a list of patterns. *)

let evars (xs : variable list) : expression list =
  List.map (fun x -> evar x) xs

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

(* [mlet es e] creates a series of [let] bindings so that each of the
   expressions in the list [es] is evaluated in turn and its result is
   bound to some variable, say [x i]; then, the expression [e], which
   is allowed to depend on these variables, is evaluated. *)

(* TEMPORARY naming convention should be chosen outside *)

let mlet (es : expression list) (e : variable list -> expression) : expression =
  (* Set up a naming convention for the intermediate results. Each result must
     receive a distinct name. The simplest convention is to use a fixed prefix
     followed with a numeric index. *)
  let x i = Printf.sprintf "r%d" i in
  (* Construct a list of value bindings. *)
  let bindings = List.mapi (fun i e -> Vb.mk (pvar (x i)) e) es in
  let xs = List.mapi (fun i _ -> x i) es in
  (* Create a series of [let] bindings around the expression [e]. *)
  List.fold_right
    (fun vb k -> Exp.let_ Nonrecursive [vb] k)
    bindings
    (e xs)

(* -------------------------------------------------------------------------- *)

(* [constrrec datacon les] produces an expression for an ``inline record''. *)

let constrrec (datacon : datacon) (les : (label * expression) list) =
  constr datacon [record les]

(* [pconstrrec datacon lps] produces a pattern for an ``inline record''. *)

let pconstrrec (datacon : datacon) (lps : (label * pattern) list) =
  pconstr datacon [precord ~closed:Closed lps]

(* -------------------------------------------------------------------------- *)

(* [mkclass params name self fields] constructs a virtual class declaration. *)

let mkclass
  (params : (core_type * variance) list)
  (name : classe)
  (self : pattern)
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

(* -------------------------------------------------------------------------- *)

(* [mkconcretemethod m e] constructs a public method whose name is [m] and
   whose body is [e]. *)

let mkconcretemethod (m : methode) (e : expression) : class_field =
  Cf.method_
    (mknoloc m)
    Public
    (Cf.concrete Fresh e)

(* -------------------------------------------------------------------------- *)

(* [mkvirtualmethod m e] constructs a virtual public method whose name is [m]
   and whose type is unspecified. *)

let mkvirtualmethod (m : methode) : class_field =
  Cf.method_
    (mknoloc m)
    Public
    (Cf.virtual_ (Typ.any()))
