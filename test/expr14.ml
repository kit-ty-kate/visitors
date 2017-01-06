open Expr12 (* [oexpr] *)
open Expr13 (*  [expr] *)
open Expr08 (* [hexpr] *)

let import (e : expr) : hexpr =
  let v = object (self)
    inherit [_] omap
    method visit_'expr _env (E e) =
      make (self#visit_oexpr _env e)
  end in
  v # visit_'expr () e
