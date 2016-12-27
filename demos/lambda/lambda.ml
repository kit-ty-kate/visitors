module StringSet = struct

  include Set.Make(String)

  let print xs =
    iter (fun x ->
      Printf.printf "%s\n" x
    ) xs

end

module StringMap = Map.Make(String)

type ('bn, 'term) abstraction =
  'bn * 'term

module Nominal = struct
  module Abstraction = struct
    let iter _ f env (x, body) =
      let env = StringSet.add x env in
      f env body
  end
end

module Nominal2DeBruijn = struct
  module Abstraction = struct
    let map _ f (env, n) (x, body) =
      let env = StringMap.add x n env in
      (), f (env, n+1) body
  end
end

type void

class fv = object
  val mutable accu = StringSet.empty
  method visit_'fn (env : 'env) x =
    if not (StringSet.mem x env) then
      accu <- StringSet.add x accu
  method visit_'bn (env : 'env) (x : void) : unit =
    assert false (* never invoked *)
  method accu = accu
end

type ('fn, 'bn) term =
  | TVar of 'fn
  | TLambda of ('bn, ('fn, 'bn) term) abstraction
  | TApp of ('fn, 'bn) term * ('fn, 'bn) term
  [@@deriving
    visitors { name = "iter"; variety = "iter"; nonlocal = ["Nominal"] },
    visitors { name = "import"; variety = "map"; nonlocal = ["Nominal2DeBruijn"] }
  ]

(* Nominal. *)

let fv (t : (string, string) term) : StringSet.t =
  let o = object
    inherit [_, _] iter
    inherit fv
  end in
  o # visit_term StringSet.empty t;
  o # accu

let identity =
  TLambda ("x", TVar "x")

let y =
  TVar "y"

let idy =
  TApp (identity, y)

let () =
  print_endline "fv(\\x.x):";
  StringSet.print (fv identity);
  print_endline "fv(y):";
  StringSet.print (fv y);
  print_endline "fv((\\x.x) y):";
  StringSet.print (fv idy)

(*

class ['self] subst (sigma : name -> name) = object (self : 'self)
  (* TEMPORARY incorrect if [sigma x] is in [env]! need freshening *)
  method visit_name env x =
    if StringSet.mem x env then x else sigma x
  method visit_binder term env (x, t) =
    let env = StringSet.add x env in
    x, term env t
end

let subst (sigma : name -> name) (t : term) : term =
  let subst = object
    inherit [_, _] map
    inherit [_] subst sigma
  end in
  let env = StringSet.empty in
  subst#visit_term env t

(* TEMPORARY need a term printer, too *)


let () =
  print (fv (subst (function "x" -> "z" | x -> x) idy))
 *)
