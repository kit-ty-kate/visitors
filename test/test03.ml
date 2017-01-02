type ('var, 'binder) term =
  | TVar of 'var
  | TAbs of 'binder * ('var, 'binder) term
  | TApp of ('var, 'binder) term * ('var, 'binder) term
[@@deriving
     visitors { name = "iter"; variety = "iter" },
     visitors { name = "map"; variety = "map" },
     visitors { name = "iter2"; variety = "iter2" },
     visitors { name = "map2"; variety = "map2" }
]

(* Nominal. *)

module StringSet = Set.Make(String)

let iter = object(self)
  inherit [_, _] iter
  (* Descending methods for local types. *)
  method! match_TAbs env x t =
    let env = StringSet.add x env in
    self#visit_term env t
  (* Descending methods for nonlocal types. *)
  method visit_'binder env x = ()
  method visit_'var env x =
    if StringSet.mem x env then
      Printf.printf "%s is a bound variable.\n%!" x
    else
      Printf.printf "%s is a free variable.\n%!" x

end

let t : (_, _) term =
  TAbs ("x", TApp (TVar "x", TVar "y"))

let () =
  iter#visit_term StringSet.empty t

(* De Bruijn. *)

let iter = object(self)
  inherit [_, _] iter
  (* Descending methods for local types. *)
  method! match_TAbs env x t =
    let env = 1 + env in
    self#visit_term env t
  (* Descending methods for nonlocal types. *)
  method visit_'binder env x = ()
  method visit_'var env x =
    if x < env then
      Printf.printf "%d is a bound variable.\n%!" x
    else
      Printf.printf "%d is a free variable.\n%!" x

end

let t : (_, _) term =
  TAbs ((), TApp (TVar 0, TVar 1))

let () =
  iter#visit_term 0 t
