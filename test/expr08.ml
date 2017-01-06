open Expr12 (* oexpr *)

open Hashcons

type expr =
  E of expr oexpr hash_consed [@@unboxed]

let table : expr oexpr Hashcons.t =
  create 128

let make : expr oexpr -> expr =
  fun e -> E (hashcons table e)

class ['self] map = object(self : 'self)
  inherit [_] omap
  method visit_'expr env (E { node = e; _ }) =
    make (self#visit_oexpr env e)
end
