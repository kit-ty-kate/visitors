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

    visitors { name = "iter"; variety = "iter";
               ancestors = ["Bn.iter"; "Abstraction.iter"] }
    ,
    visitors { name = "map"; variety = "map";
               ancestors = ["Bn.map"; "Abstraction.map"] }
    ,
    visitors { name = "reduce"; variety = "reduce";
               ancestors = ["Bn.reduce"; "Abstraction.reduce"] }

  ]

type raw_term =
  (string, string) term

type nominal_term =
  (Atom.t, Atom.t) term

type db_term =
  (int, unit) term

class ['self] size = object (_ : 'self)
  inherit [_] reduce as super
  inherit [_] KitTrivial.reduce
  inherit [_] VisitorsRuntime.addition_monoid
  method! visit_term env t =
    1 + super#visit_term env t
end

let size : 'fn 'bn . ('fn, 'bn) term -> int =
  fun t ->
    new size # visit_term () t

class ['self] show = object (_ : 'self)
  inherit [_] map
  inherit [_] KitShow.map
end

let show : nominal_term -> raw_term =
  new show # visit_term ()

class ['self] copy = object (_ : 'self)
  inherit [_] map
  inherit [_] KitCopy.map
end

let copy : nominal_term -> nominal_term =
  new copy # visit_term KitCopy.empty

class ['self] import = object (_ : 'self)
  inherit [_] map
  inherit [_] KitImport.map
end

let import : KitImport.env -> raw_term -> nominal_term =
  new import # visit_term

class atom2unit = object
  inherit [_] iter
  inherit [_] Foo.kit
end
