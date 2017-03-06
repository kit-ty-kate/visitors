type 'a expr =
  | EConst of ('a[@opaque])
  | EAdd of 'a expr * 'a expr
[@@deriving
visitors { variety = "iter"; polymorphic = true },
visitors { variety = "map"; polymorphic = true },
visitors { variety = "endo"; polymorphic = true },
visitors { variety = "reduce"; polymorphic = true },
visitors { variety = "mapreduce"; polymorphic = true },
visitors { variety = "iter2"; polymorphic = true },
visitors { variety = "map2"; polymorphic = true },
visitors { variety = "reduce2"; polymorphic = true },
visitors { variety = "mapreduce2"; polymorphic = true }
]
