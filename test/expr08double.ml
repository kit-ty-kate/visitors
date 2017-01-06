open Expr12 (* [oexpr] *)
open Expr08 (* [hexpr] *)

let double (e : hexpr) : hexpr =
  let v = object
    inherit [_] map
    method! visit_EConst _env k =
      EConst (2 * k)
  end in
  v # visit_'expr () e
