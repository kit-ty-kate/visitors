type ('bn, 'term) abstraction =
  'bn * 'term

module Nominal = struct
  module Abstraction = struct
    let iter _ f env (x, body) =
      let env = Atom.Set.add x env in
      f env body
  end
end

module Nominal2DeBruijn = struct
  type env =
    int Atom.Map.t * int
  module Abstraction = struct
    let map _ f (env, n) (x, body) =
      let env = Atom.Map.add x n env in
      (), f (env, n+1) body
  end
end

type void

class fv = object
  val mutable accu = Atom.Set.empty
  method visit_'fn (env : 'env) x =
    if not (Atom.Set.mem x env) then
      accu <- Atom.Set.add x accu
  method visit_'bn (_ : 'env) (_ : void) : unit =
    assert false (* never invoked *)
  method accu = accu
end

class import = object
  method visit_'fn (env, n) x =
    let level = Atom.Map.find x env in
    n - level
  method visit_'bn (_ : Nominal2DeBruijn.env) (_ : void) : unit =
    assert false (* never invoked *)
end

type ('fn, 'bn) term =
  | TVar of 'fn
  | TLambda of ('bn, ('fn, 'bn) term) abstraction
  | TApp of ('fn, 'bn) term * ('fn, 'bn) term
  [@@deriving
    visitors { name = "iter"; variety = "iter"; nonlocal = ["Nominal"] },
    visitors { name = "map"; variety = "map"; nonlocal = ["Nominal2DeBruijn"] }
  ]

(* Nominal. *)

type nominal_term =
  (Atom.t, Atom.t) term

type db_term =
  (int, unit) term

let fv (t : nominal_term) : Atom.Set.t =
  let o = object
    inherit [_] iter
    inherit fv
  end in
  o # visit_term Atom.Set.empty t;
  o # accu

let import (t : nominal_term) : db_term =
  let o = object
    inherit [_] map
    inherit import
  end in
  o # visit_term (Atom.Map.empty, 0) t

let x = Atom.freshh "x"

let identity =
  TLambda (x, TVar x)

let y =
  TVar (Atom.freshh "y")

let idy =
  TApp (identity, y)

let () =
  print_endline "fv(\\x.x):";
  print_endline (Atom.Set.print (fv identity));
  print_endline "fv(y):";
  print_endline (Atom.Set.print (fv y));
  print_endline "fv((\\x.x) y):";
  print_endline (Atom.Set.print (fv idy))

(*

class ['self] subst (sigma : name -> name) = object (self : 'self)
  (* TEMPORARY incorrect if [sigma x] is in [env]! need freshening *)
  method visit_name env x =
    if Atom.Set.mem x env then x else sigma x
  method visit_binder term env (x, t) =
    let env = Atom.Set.add x env in
    x, term env t
end

let subst (sigma : name -> name) (t : term) : term =
  let subst = object
    inherit [_] map
    inherit [_] subst sigma
  end in
  let env = Atom.Set.empty in
  subst#visit_term env t

(* TEMPORARY need a term printer, too *)


let () =
  print (fv (subst (function "x" -> "z" | x -> x) idy))
 *)
