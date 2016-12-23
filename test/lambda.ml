module StringSet =
  Set.Make(String)

type name =
  string

module Name = struct

  let iter env (x : name) =
    ()

  let map env (x : name) =
    x

end

type 'a binder =
  string * 'a

module Binder = struct

  let iter f env (x, body) =
    let env = StringSet.add x env in
    f env body

  let map f env (x, body) =
    let env = StringSet.add x env in
    x, f env body

end

type term =
  | TVar of name
  | TLambda of term binder
  | TApp of term * term
  [@@deriving
    visitors { name = "iter"; variety = "iter" },
    visitors { name = "map"; variety = "map"  }
  ]

let identity =
  TLambda ("x", TVar "x")

let y =
  TVar "y"

let idy =
  TApp (identity, y)

class ['a] accu (init : 'a) = object
  val mutable accu = init
  method accu = accu
end

class ['self] fv = object (self : 'self)
  inherit [_] accu StringSet.empty
  method visit_name env x =
    if not (StringSet.mem x env) then
      accu <- StringSet.add x accu
  method visit_binder term env (x, t) =
    let env = StringSet.add x env in
    term env t
end

let fv (t : term) : StringSet.t =
  let fv = object
    inherit [_, _] iter
    inherit [_] fv (* TEMPORARY useless *)
  end in
  let env = StringSet.empty in
  fv#visit_term env t;
  fv#accu

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

let print (xs : StringSet.t) =
  StringSet.iter (fun x ->
    Printf.printf "%s\n" x
  ) xs

(* TEMPORARY need a term printer, too *)

let () =
  print (fv idy)

let () =
  print (fv (subst (function "x" -> "z" | x -> x) idy))
