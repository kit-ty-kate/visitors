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
    visitors { name = "size_"; variety = "reduce";
               ancestors = ["Bn.reduce"; "Abstraction.reduce";
                            "KitTrivial.reduce"; "VisitorsRuntime.addition_monoid"] }
    ,
    visitors { name = "show"; variety = "map";
               ancestors = ["Bn.map"; "Abstraction.map"; "KitShow.map"];
               concrete = true }
    ,
    visitors { name = "copy"; variety = "map";
               ancestors = ["Bn.map"; "Abstraction.map"; "KitCopy.map"];
               concrete = true }

  ]

type raw_term =
  (string, string) term

type nominal_term =
  (Atom.t, Atom.t) term

type db_term =
  (int, unit) term

class ['self] size = object (_ : 'self)
  inherit [_] size_ as super
  method! visit_term env t =
    1 + super#visit_term env t
end

let size : 'fn 'bn . ('fn, 'bn) term -> int =
  fun t ->
    new size # visit_term () t

let show : nominal_term -> raw_term =
  new show # visit_term ()

let copy : nominal_term -> nominal_term =
  new copy # visit_term KitCopy.empty

class atom2unit = object
  inherit [_] iter
  inherit [_] Foo.kit
end
