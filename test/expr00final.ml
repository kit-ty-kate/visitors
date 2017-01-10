type expr =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { name = "iter"; variety = "iter"; final = true },
              visitors { name = "map"; variety = "map"; final = true },
              visitors { name = "iter2"; variety = "iter2"; final = true },
              visitors { name = "map2"; variety = "map2"; final = true }]
