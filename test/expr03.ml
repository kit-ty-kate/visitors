type expr =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { name = "map2"; variety = "map2" }]
