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

(* [split_on_char c s] splits the string [s] into substrings delimited
   by the character [c]. *)

(* Copied from OCaml 4.04. *)

let split_on_char sep s =
  let r = ref [] in
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if unsafe_get s i = sep then begin
      r := sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  sub s 0 !j :: !r
