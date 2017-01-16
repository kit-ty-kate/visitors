type point =
  { x: int; y: int; mutable color: bool }
[@@deriving
     visitors { variety = "iter" },
     visitors { variety = "map" },
     visitors { variety = "iter2" },
     visitors { variety = "map2" }
]
