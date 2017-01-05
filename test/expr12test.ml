open Expr12

let const k = E (EConst k)
let add e1 e2 = E (EAdd (e1, e2))

let rec eval (E e) =
  match e with
  | EConst k -> k
  | EAdd (e1, e2) -> eval e1 + eval e2

let e : expr =
  add (const 1) (const 2)

let () =
  Printf.printf "%d\n%d\n%!" (eval e) (eval (double e))
