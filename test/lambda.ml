type name =
  string

type binder =
  string

type term =
  | TVar of name
  | TLambda of binder * term
  | TApp of term * term
  [@@deriving visitors]

let identity =
  TLambda ("x", TVar "x")

let y =
  TVar "y"

let idy =
  TApp (identity, y)

module StringSet =
  Set.Make(String)

type void

class fv = object(self)
  inherit [_, _] iter
  val mutable accu = StringSet.empty
  method accu = accu
  method name env x =
    if not (StringSet.mem x env) then
      accu <- StringSet.add x accu
  method binder env x : void =
    assert false
  method match_TLambda env x t =
    let env = StringSet.add x env in
    self#term env t
end

let fv (t : term) : StringSet.t =
  let fv = new fv in
  fv#term StringSet.empty t;
  fv#accu

let print (xs : StringSet.t) =
  StringSet.iter (fun x ->
    Printf.printf "%s\n" x
  ) xs

let () =
  print (fv idy)
