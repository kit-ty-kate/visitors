open List

(* [interval i j] constructs a list representation of the semi-open interval
   [i..j). *)

let rec interval i j : int list =
  if i < j then
    i :: interval (i + 1) j
  else
    []

(* [init i j f] constructs a list of the values [f i] up to [f (j - 1)]. *)

let init i j (f : int -> 'a) : 'a list =
  map f (interval i j)

(* [is_matrix m n xss] checks that [xss] is an [m]-by-[n] matrix, represented
   as a list of lists. *)

let is_matrix m n (xss : _ list list) =
  length xss = m && for_all (fun xs -> length xs = n) xss

(* [transpose xss] transposes a matrix, represented as a list of lists. *)

let transpose (xss : 'a list list) : 'a list list =
  let m = length xss in
  (* [m] must be nonzero, otherwise [n] is undefined and we can't know
     what the length of the output list should be. *)
  assert (m > 0);
  let n = length (hd xss) in
  assert (is_matrix m n xss);
  (* Convert [xss] to an array, for speed. *)
  let xss : 'a array array =
    Array.map Array.of_list (Array.of_list xss)
  in
  (* We have an [m]-by-[n] matrix, and construct an [n]-by-[m] matrix. *)
  init 0 n (fun j ->
    init 0 m (fun i ->
      xss.(i).(j)
    )
  )
