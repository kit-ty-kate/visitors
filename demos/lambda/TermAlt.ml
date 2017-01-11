type void

module BnKit = struct
  class ['self] iter = object (self : 'self)
    method visit_'bn _env _x : void = assert false
  end
end

module AbstractionKit = struct
  class virtual ['self] iter = object (self : 'self)
    method virtual extend: 'bn -> 'env -> 'env
    method visit_abstraction: 'term .
      _ -> ('env -> 'term -> unit) ->
      'env -> ('bn, 'term) Abstraction.abstraction -> unit
    = fun _ f env (x, body) ->
        let env = self#extend x env in
        f env body
  end
end

type ('fn, 'bn) term =
  | TVar of 'fn
  | TLambda of 'bn * ('fn, 'bn) term
  | TApp of ('fn, 'bn) term * ('fn, 'bn) term

  [@@deriving

    visitors { name = "iter_"; variety = "iter"; ancestors = ["BnKit.iter"; "AbstractionKit.iter"] }

  ]

class virtual ['self] iter = object (self : 'self)
  inherit [_] iter_
  method! visit_TLambda env x t =
    self#visit_abstraction self#visit_'bn self#visit_term env (x, t)
end

class atom2unit = object
  inherit [_] iter
  (* TEMPORARY the methods below should form a kit *)
  method visit_'fn env x =
    Abstraction.Atom2Unit.Fn.iter env x
  method extend x env =
    Abstraction.Atom2Unit.extend x env
end
