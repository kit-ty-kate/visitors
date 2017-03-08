type ('a, 'b) dictionary =
  | Empty
  | NonEmpty of 'a * 'b * ('a, 'b) dictionary
[@@deriving visitors { variety = "map"; polymorphic = ["'b"] }]

type ('a, 'b) dictionary2 =
  | Empty
  | NonEmpty of 'a * 'b * ('a, 'b) dictionary2
[@@deriving visitors { name = "map2"; variety = "map"; polymorphic = ["'b"; "'env"] }]
