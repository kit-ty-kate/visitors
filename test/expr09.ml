type 'expr expr_node =
  | EConst of int
  | EAdd of 'expr * 'expr
  [@@deriving visitors { name = "map"; variety = "map" }]
