type point =
  { x: int; y: int; mutable color: bool }
[@@deriving
     visitors { name = "iter"; variety = "iter" },
     visitors { name = "map"; variety = "map" },
     visitors { name = "iter2"; variety = "iter2" },
     visitors { name = "map2"; variety = "map2" }
]
