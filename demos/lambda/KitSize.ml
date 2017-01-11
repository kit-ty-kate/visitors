(* This kit allows computing the size of a term. *)

(* No environment is needed. *)

type env = unit

let extend _x () = ()

(* At an abstraction, no computation happens. In other words, an
   abstraction does not contribute to the size. *)

let restrict _x n = n

(* A name does not contribute to the size. *)

module Fn = struct
  let reduce () _x = 0
end
