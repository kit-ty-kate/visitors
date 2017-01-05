open Hashcons

type 'expr oexpr =
  | EConst of int
  | EAdd of 'expr * 'expr
  [@@deriving visitors { name = "map0"; variety = "map" }]

type expr =
  E of expr oexpr hash_consed [@@unboxed]

let table : expr oexpr Hashcons.t =
  create 128

let make : expr oexpr -> expr =
  fun e -> E (hashcons table e)

class ['self] map = object(self : 'self)
  inherit [_] map0
  method visit_'expr env (E { node = e; _ }) =
    make (self#visit_oexpr env e)
end

let econst e = make (EConst e)
let eadd e1 e2 = make (EAdd (e1, e2))

let increment (e : expr) : expr =
  let v = object
    inherit [_] map
    method! visit_EConst _env i =
      EConst (i + 1)
  end in
  v # visit_'expr () e


let e1 : expr = eadd (econst 0) (econst 1)
let e2 : expr = new map # visit_'expr () e1
let () =
  Printf.printf "%b\n%!" (e1 == e2)
let e3 : expr = eadd (econst 1) (econst 2)
let e4 : expr = increment e1
let () =
  Printf.printf "%b\n%!" (e3 == e4)
