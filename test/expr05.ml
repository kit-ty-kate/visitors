open Expr02

let equal (e1 : expr) (e2 : expr) : bool =
  try
    new iter2 # visit_expr () e1 e2;
    true
  with VisitorsRuntime.StructuralMismatch ->
    false
