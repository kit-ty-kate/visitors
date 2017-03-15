(* Testing @build attributes. *)

type foo =
  | A
  | B of int
  | C of foo * foo
[@@deriving visitors { variety = "map" }]
