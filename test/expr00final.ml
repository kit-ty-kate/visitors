type expr =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { name = "Iter"; variety = "iter"; final = true },
              visitors { name = "Map"; variety = "map"; final = true },
              visitors { name = "Iter2"; variety = "iter2"; final = true },
              visitors { name = "Map2"; variety = "map2"; final = true }]
