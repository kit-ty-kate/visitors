class ['self] base = object (_ : 'self)
  method visit_real _env x = x
end

type cloud =
  | Point of (float[@name "real"]) * (float[@name "real"])
  | Clouds of cloud list
  [@@name "nuage"]
  [@@deriving visitors { variety = "map"; ancestors = ["base"] }]

module List = struct

  type 'a mylist = 'a list =
    | []                     [@name "nil"]
    | (::) of 'a * 'a mylist [@name "cons"]
    [@@deriving visitors { variety = "map" }]

end
