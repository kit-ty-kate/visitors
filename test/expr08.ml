open Expr12 (* oexpr *)

open Hashcons

type hexpr =
  H of hexpr oexpr hash_consed [@@unboxed]

let table : hexpr oexpr Hashcons.t =
  create 128

let make : hexpr oexpr -> hexpr =
  fun e -> H (hashcons table e)

class ['self] map = object(self : 'self)
  inherit [_] omap
  method visit_'expr env (H { node = e; _ }) =
    make (self#visit_oexpr env e)
end
