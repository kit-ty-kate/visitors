type expr =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { name = "iter2"; variety = "iter2" }]
