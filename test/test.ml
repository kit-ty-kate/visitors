type foo = A

and point = { x: foo; y: foo }

and clac = { mutable a: alternative }

and alternative =
  | Nothing
  | Foo of point * point
  | Bar of alternative * int
  | Quux of (foo * foo)
  | Pschitt of { foo: foo; point: point }
  [@@deriving visitors]

let f : foo = A

let p : point = { x = f; y = f }

let a1: alternative = Foo (p, p)
let a2: alternative = Bar (a1, 42)
let a3: alternative = Quux (f, f)
let a4: alternative = Pschitt { foo = f; point = p }

let v = object
  inherit visitors
  method int x = Printf.printf "Visited %d\n%!" x
end

let () =
  v # foo f

let () =
  v # point p

let () =
  v # alternative a1;
  v # alternative a2;
  v # alternative a3;
  v # alternative a4;
