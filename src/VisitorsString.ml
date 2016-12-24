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
