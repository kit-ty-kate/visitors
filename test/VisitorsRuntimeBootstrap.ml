type 'a list =
  | []
  | (::) of 'a * 'a list

[@@deriving visitors { variety = "iter"; public = []; polymorphic = true; data = false; nude = true }]
