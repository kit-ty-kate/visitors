type expr =
  | EConst of int
  | EAdd of expr * expr
  | EList of expr list
  [@@deriving visitors { variety = "iter"; concrete = true },
              visitors { variety = "map"; concrete = true }]

let rec native_iter env e =
  match e with
  | EConst _ ->
      ()
  | EAdd (e1, e2) ->
      native_iter env e1;
      native_iter env e2
  | EList es ->
      VisitorsRuntime.TempList.iter native_iter env es
      (* List.iter (native_iter env) es *)

let split n =
  assert (n >= 0);
  let n1 = Random.int (n + 1) in
  let n2 = n - n1 in
  assert (0 <= n1 && n1 <= n);
  assert (0 <= n2 && n2 <= n);
  n1, n2

let rec generate n =
  assert (n >= 0);
  if n = 0 then
    EConst (Random.int 100)
  else
    match Random.int 2 with
    | 0 ->
        let n1, n2 = split (n - 1) in
        EAdd (generate n1, generate n2)
    | 1 ->
        let n1, n2 = split (n - 1) in
        EList [ generate n1; generate n2 ]
    | _ ->
        assert false

let rec list_init i n f =
  if i = n then
    []
  else
    let x = f i in
    x :: list_init (i + 1) n f

let list_init n f =
  list_init 0 n f

let samples =
  list_init 100 (fun _ -> generate 100)

let iter : expr -> unit =
  new iter # visit_expr ()

let test f () =
  List.iter f samples

let tests = [
  "iter", test iter;
  "native_iter", test (native_iter ());
]

module Bench = Core_bench.Std.Bench
module Command = Core.Std.Command

let tests =
  List.map (fun (name, test) -> Bench.Test.create ~name test) tests

let () =
  Command.run (Bench.make_command tests)
