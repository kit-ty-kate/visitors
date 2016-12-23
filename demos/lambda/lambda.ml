module StringSet = struct

  include Set.Make(String)

  let print xs =
    iter (fun x ->
      Printf.printf "%s\n" x
    ) xs

end

type ('bn, 'term) abstraction =
  'bn * 'term

module Nominal = struct
  module Abstraction = struct
    let iter _ f env (x, body) =
      let env = StringSet.add x env in
      f env body
  end
end

type ('fn, 'bn) term =
  | TVar of 'fn
  | TLambda of ('bn, ('fn, 'bn) term) abstraction
  | TApp of ('fn, 'bn) term * ('fn, 'bn) term
  [@@deriving
    visitors { name = "fv"; variety = "iter"; nonlocal = ["Nominal"] }
  ]

(* Nominal. *)

class ['a] accu (init : 'a) = object
  val mutable accu = init
  method accu = accu
end

let fv (t : (string, string) term) : StringSet.t =
  let env = StringSet.empty in
  let o = object
    inherit [_] accu (StringSet.empty)
    inherit [_, _] fv
    method visit_'fn env x =
      if not (StringSet.mem x env) then
        accu <- StringSet.add x accu
    method visit_'bn env x =
      assert false (* never invoked *)
  end in
  o # visit_term env t;
  o # accu

let identity =
  TLambda ("x", TVar "x")

let y =
  TVar "y"

let idy =
  TApp (identity, y)

let () =
  print_endline "fv(\x.x):";
  StringSet.print (fv identity);
  print_endline "fv(y):";
  StringSet.print (fv y);
  print_endline "fv((\x.x) y):";
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
