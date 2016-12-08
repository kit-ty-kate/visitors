type name =
  string

type 'a binder =
  string * 'a

type term =
  | TVar of name
  | TLambda of term binder
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

class ['a] accu (init : 'a) = object
  val mutable accu = init
  method accu = accu
end

class ['self] fv = object (self : 'self)
  inherit [_] accu StringSet.empty
  method name env x =
    if not (StringSet.mem x env) then
      accu <- StringSet.add x accu
  method binder term env (x, t) =
    let env = StringSet.add x env in
    term env t
end

let fv (t : term) : StringSet.t =
  let fv = object
    inherit [_, _] iter
    inherit [_] fv
  end in
  let env = StringSet.empty in
  fv#term env t;
  fv#accu

let print (xs : StringSet.t) =
  StringSet.iter (fun x ->
    Printf.printf "%s\n" x
  ) xs

let () =
  print (fv idy)
