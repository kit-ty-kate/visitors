(* Testing @build attributes. *)

type foo =
  | A
  | B of int
  | C of foo * foo [@build fun x y -> C (x, y)]

and point =
  { x: int; y: int } [@@build fun x y -> { x; y }]

[@@deriving visitors { variety = "map" }]
