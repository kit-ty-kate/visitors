open Expr15

let size : expr -> int =
  let o = object
    inherit [_] reduce as super
    inherit [_] VisitorsRuntime.addition_monoid
    method! visit_EConst _ _ = 0
    method! visit_expr env e =
      1 + super # visit_expr env e
  end in
  o # visit_expr ()
