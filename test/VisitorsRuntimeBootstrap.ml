type 'a list =
  | []
  | (::) of 'a * 'a list

[@@deriving visitors { variety = "iter"; data = false }]
