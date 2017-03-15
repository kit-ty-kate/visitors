(* Testing @name attributes. *)

type foo =
  | A [@name "transform_A"]
  | B of int [@name "transform_B"]
  | C of int * int [@name "transform_C"]
[@@deriving visitors { variety = "map"; concrete = true }]

let f (x : foo) =
  let o = object
    inherit [_] map
    method! transform_A _env = B 0
    method! transform_B _env x = B (x + 1)
    method! transform_C _env x y = C (x, x + y)
  end in
  o # visit_foo () x

let () =
  assert (f A = B 0);
  assert (f (B 0) = B 1);
  assert (f (C (1, 1)) = C (1, 2));
  ()
