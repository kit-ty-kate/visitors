open String

(* [prefix s1 s2] tests whether [s1] is a prefix of [s2], i.e. whether
   [s2] begins with [s1]. *)

let prefix s1 s2 =
  let n1 = length s1 in
  n1 <= length s2 && s1 = sub s2 0 n1

(* [remainder s1 s2] assumes that [s1] is a prefix of [s2], and chops
   [s1] off [s2], returning the remainder. *)

let remainder s1 s2 =
  assert (prefix s1 s2);
  let n1 = length s1 in
  let n2 = length s2 in
  sub s2 n1 (n2 - n1)

(* [unquote alpha] removes a leading quote in the string [alpha], if
   there is one. *)

let unquote alpha =
  let n = String.length alpha in
  if n > 0 && alpha.[0] = '\'' then
    String.sub alpha 1 (n-1)
  else
    alpha

(* [print_longident] converts an OCaml long identifier to a string. *)

let print_longident (x : Longident.t) : string =
  String.concat "." (Longident.flatten x)

(* Suppose the function [f] is a lossy (non-injective) mapping of ['a] to
   [string]. Then, the function [protect equal f warn] is also a function of
   ['a] to [string], which behaves like [f], except it warns when [f] is
   applied to two values of type ['a] that have the same image of type
   [string]. *)

(* [equal] should implement equality at type ['a]. *)

(* [warn x1 x2 y] is invoked when [f] is applied at two values [x1] and [x2]
   that have the same image [y] through [f]. *)

module H = Hashtbl

let protect
  (equal : 'a -> 'a -> bool)
  (f : 'a -> string)
  (warn : 'a -> 'a -> string -> unit)
: 'a -> string =
  (* A hash table memoizes the inverse of [f]. *)
  let table : (string, 'a) H.t = H.create 127 in
  fun (x : 'a) ->
    let y = f x in
    begin try
      let x' = H.find table y in
      if not (equal x x') then
        warn x' x y
    with Not_found ->
      H.add table y x
    end;
    y
