module Info = struct
  let map env i = i + 1
end

type 'info expr_node =
  | EConst of int
  | EAdd of 'info expr * 'info expr

and 'info expr =
  { info: 'info; node: 'info expr_node }
  [@@deriving visitors { name = "Map"; variety = "map"; freeze = ["info"]; final = true }]

let map : int expr -> int expr =
  fun e -> Map.visit_expr () e
