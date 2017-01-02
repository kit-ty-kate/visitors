type expr =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { name = "iter"; variety = "iter" }]
