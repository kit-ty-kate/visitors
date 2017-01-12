module Foo = struct
  type env = Atom.Set.t * Atom.Set.t ref
  class ['self] kit = object
    method extend x (env, accu : env) =
      let env = Atom.Set.add x env in
      env, accu
    method visit_'fn (env, accu : env) x =
      if not (Atom.Set.mem x env) then
          accu := Atom.Set.add x !accu
  end
end

open Abstraction

type ('fn, 'bn) term =
  | TVar of 'fn
  | TLambda of ('bn, ('fn, 'bn) term) abstraction
  | TApp of ('fn, 'bn) term * ('fn, 'bn) term

  [@@deriving

    visitors { name = "iter"; variety = "iter"; ancestors = ["Bn.iter"; "Abstraction.iter"] }

  ]

class atom2unit = object
  inherit [_] iter
  inherit [_] Foo.kit
end
