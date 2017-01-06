open Expr12
open Expr08

let econst e = make (EConst e)
let eadd e1 e2 = make (EAdd (e1, e2))

let increment (e : hexpr) : hexpr =
  let v = object
    inherit [_] map
    method! visit_EConst _env i =
      EConst (i + 1)
  end in
  v # visit_'expr () e


let e1 : hexpr = eadd (econst 0) (econst 1)
let e2 : hexpr = new map # visit_'expr () e1
let () =
  Printf.printf "%b\n%!" (e1 == e2)
let e3 : hexpr = eadd (econst 1) (econst 2)
let e4 : hexpr = increment e1
let () =
  Printf.printf "%b\n%!" (e3 == e4)
