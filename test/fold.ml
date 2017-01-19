type person = {
  firstname: string[@opaque];
  surname:   string[@opaque]
}

and crowd =
  | Nobody
  | Someone of person * crowd
[@@deriving visitors { variety = "fold" }]

class convert = object
  inherit [_] fold
  method build_person  () f s = (f, s)
  method build_Nobody  ()     = []
  method build_Someone () p c = p :: c
end

let convert : crowd -> (string * string) list =
  new convert # visit_crowd ()
