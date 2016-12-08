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

class ['self] fv accu = object(self : 'self)
  method name env x =
    if not (StringSet.mem x env) then
      accu := StringSet.add x !accu
  method binder term env (x, t) =
    let env = StringSet.add x env in
    term env t
end

let fv (t : term) : StringSet.t =
  let accu = ref StringSet.empty in
  let fv = object
    inherit [_] fv accu
    inherit [_, _] iter
  end in
  let env = StringSet.empty in
  fv#term env t;
  !accu

let print (xs : StringSet.t) =
  StringSet.iter (fun x ->
    Printf.printf "%s\n" x
  ) xs

let () =
  print (fv idy)
