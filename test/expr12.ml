type 'expr oexpr =
  | EConst of int
  | EAdd of 'expr * 'expr
  [@@deriving visitors { name = "omap"; variety = "map" }]

type expr =
  E of expr oexpr [@@unboxed]

class ['self] map = object (self : 'self)
  inherit [_] omap
  method visit_'expr env (E e) =
    E (self#visit_oexpr env e)
end

let double (e : expr) : expr =
  let v = object
    inherit [_] map
    method! visit_EConst _env k = EConst (2 * k)
  end in
  v # visit_'expr () e

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
