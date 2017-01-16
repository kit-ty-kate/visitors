type u = Uber
 and point = u * u
[@@deriving
     visitors { variety = "iter" },
     visitors { variety = "map" },
     visitors { variety = "iter2" },
     visitors { variety = "map2" }
]
