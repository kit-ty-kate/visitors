open VisitorsString
open List
let sprintf = Printf.sprintf
open Parsetree
open Ppx_deriving
open VisitorsGeneration

(* -------------------------------------------------------------------------- *)

(* The name of our [ppx_deriving] plugin. *)

let plugin =
  "visitors"

(* -------------------------------------------------------------------------- *)

(* We can generate two classes, [iter] and [map]. They are mostly identical,
   and differ only in the code that is executed after the recursive calls. In
   [iter], this code does nothing; in [map], it reconstructs a data
   structure. *)

type scheme =
  | Iter
  | Map

(* -------------------------------------------------------------------------- *)

(* The parameters that can be set by the user. *)

module type SETTINGS = sig

  (* The type declarations that we are processing. *)
  val decls: type_declaration list

  (* The name of the generated class. *)
  val name: classe

  (* The arity of the generated code, e.g., 1 if one wishes to generate [iter]
     and [map], 2 if one wishes to generate [iter2] and [map2], and so on. *)
  val arity: int

  (* The scheme of visitor that we wish to generate (see the definition of
     the type [scheme] above). *)
  val scheme: scheme

  (* [variety] combines the information in [scheme] and [arity]. It is just
     the string provided by the user. *)
  val variety: string

  (* If [final] is false, which is the default, we generate OCaml classes.
     If [final] is true, we generate nests of mutually recursive functions.
     This requires that there be no virtual methods. *)
  val final: bool

  (* The type variables that should be treated as nonlocal types. Following
     OCaml's convention, the name of a type variable does not include a
     leading quote. *)
  val freeze: string list

  (* If [irregular] is [true], the regularity check is suppressed; this allows
     a local parameterized type to be instantiated. The definition of ['a t]
     can then refer to [int t]. However, in most situations, this will lead to
     ill-typed generated code. The generated code should be well-typed if [t]
     is always instantiated in the same manner, e.g., if there are references
     to [int t] but not to other instances of [t]. *)
  val irregular: bool

  (* A list of module names that should be searched for nonlocal functions,
     such as [List.iter]. The modules that appear first in the list are
     searched last. *)
  val path: Longident.t list

end

(* -------------------------------------------------------------------------- *)

(* [parse_variety] takes a variety, which could be "iter", "map2", etc. and
   returns a pair of a scheme and an arity. *)

let parse_variety loc (s : string) : scheme * int =
  try
    if prefix "map" s then
      let s = remainder "map" s in
      let i = if s = "" then 1 else int_of_string s in
      if i <= 0 then failwith "negative integer";
      Map, i
    else if prefix "iter" s then
      let s = remainder "iter" s in
      let i = if s = "" then 1 else int_of_string s in
      if i <= 0 then failwith "negative integer";
      Iter, i
    else
      failwith "unexpected prefix"
  with
  | Failure _ ->
      raise_errorf ~loc
      "%s: invalid variety.\n\
       A valid variety is iter, map, iter2, map2, etc." plugin

(* -------------------------------------------------------------------------- *)

(* TEMPORARY implement these properly *)
let is_valid_class_name (c : classe) : bool =
  String.length c > 0 && String.uncapitalize_ascii c = c

let is_valid_module_name (c : classe) : bool =
  String.length c > 0 && String.capitalize_ascii c = c

let is_valid_name final (c : classe) : bool =
         final && is_valid_module_name c
  || not final && is_valid_class_name c

(* -------------------------------------------------------------------------- *)

(* The option processing code constructs a module of type [SETTINGS]. *)

module Parse (O : sig
  val loc: Location.t
  val decls: type_declaration list
  val options: (string * expression) list
end)
: SETTINGS
= struct
  open O

  (* Set up a few parsers. *)

  let bool = Arg.get_expr ~deriver:plugin Arg.bool
  let string = Arg.get_expr ~deriver:plugin Arg.string
  let strings = Arg.get_expr ~deriver:plugin (Arg.list Arg.string)

  (* Default values. *)

  let arity = ref 1 (* dummy: [variety] is mandatory; see below *)
  let final = ref false
  let freeze = ref []
  let irregular = ref false
  let names = ref [] (* dummy: [name] is mandatory; see below *)
  let path = ref []
  let scheme = ref Iter (* dummy: [variety] is mandatory; see below *)
  let variety = ref None

  (* Parse every option. *)

  let () =
    iter (fun (o, e) ->
      let loc = e.pexp_loc in
      match o with
      | "final" ->
           final := bool e
      | "freeze" ->
           freeze := strings e
      | "irregular" ->
          irregular := bool e
      | "name" ->
          names := string e :: !names;
      | "path" ->
          path := strings e
      | "variety" ->
          let v = string e in
          variety := Some v;
          let s, a = parse_variety loc v in
          scheme := s;
          arity := a
      | _ ->
          (* TEMPORARY warn instead of failing, for better forward compatibility *)
          raise_errorf ~loc "%s: option %s is not supported." plugin o
    ) options

  (* Export the results. *)

  let decls = decls
  let arity = !arity
  let final = !final
  let freeze = !freeze
  let irregular = !irregular
  let path = !path
  let scheme = !scheme

  (* Perform sanity checking. *)

  (* The parameter [name] is not optional. Also, it should be given multiple
     times. So, we first accumulate a list of names, then check that this list
     has length 1. *)
  let name =
    if List.length !names = 0 then
      raise_errorf ~loc
        "%s: please specify the name of the generated class.\n\
         e.g. [@@deriving visitors { name = \"traverse\" }]" plugin;
    if List.length !names > 1 then
      raise_errorf ~loc
        "%s: please specify only ONE name for the generated class." plugin;
    match !names with
    | [ name ] ->
        if not (is_valid_name final name) then
          raise_errorf ~loc
            "%s: %s is not a valid %s name."
            plugin name
            (if final then "module" else "class");
        name
    | []
    | _ :: _ :: _ ->
        assert false (* already checked length *)

  (* The parameter [variety] is not optional. *)
  let variety =
    match !variety with
    | None ->
        raise_errorf ~loc
          "%s: please specify the variety of the generated class.\n\
           e.g. [@@deriving visitors { variety = \"iter\" }]" plugin
    | Some variety ->
        variety

  (* TEMPORARY check that every string in the list is a valid module name *)
  (* We always open [VisitorsRuntime], but allow it to be shadowed by
     user-specified modules. *)
  let path =
    map Longident.parse ("VisitorsRuntime" :: path)

end
