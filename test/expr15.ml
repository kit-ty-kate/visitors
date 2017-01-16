type expr =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { name = "size"; variety = "reduce"; ancestors = ["VisitorsRuntime.addition_monoid"] }]

let size : expr -> int =
  let o = object
    inherit [_] size as super
    method! visit_EConst () (_ : int) = 0
    method! visit_expr () e =
      1 + super # visit_expr () e
  end in
  o # visit_expr ()

let () =
  Printf.printf "%d\n" (size (EAdd (EConst 22, EConst 11)))
