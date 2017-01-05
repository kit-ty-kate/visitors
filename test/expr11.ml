type expr =
  | EConst of int
  | EAdd of expr list
  [@@deriving visitors { name = "iter"; variety = "iter" }]
