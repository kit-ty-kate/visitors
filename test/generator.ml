type 'a delay = 'a

type 'a delaytree =
  | DTNothing
  | DTElement of 'a
  | DTConcat of 'a delaytree * 'a delaytree
  | DTDelay of (unit -> 'a delaytree)

(* https://github.com/ocaml/ocaml/pull/1002 *)

type 'a cascade =
  unit -> 'a head

and 'a head =
  | Nil
  | Cons of 'a * 'a cascade

let nil : 'a cascade =
  fun () -> Nil

let force thunk =
  thunk()

let head (xs : 'a cascade) : 'a =
  match force xs with
  | Nil ->
      invalid_arg "head"
  | Cons (x, _) ->
      x

let tail (xs : 'a cascade) : 'a cascade =
  match force xs with
  | Nil ->
      invalid_arg "tail"
  | Cons (_, xs) ->
      xs

type 'a iterator =
  unit -> 'a option

let cascade2iterator (xs : 'a cascade) : 'a iterator =
  let s = ref xs in
  fun () ->
    match force !s with
    | Nil ->
        s := nil; (* avoid repeating this work, next time *)
        None
    | Cons (x, xs) ->
        s := xs;
        Some x

let rec delaytree2cascade (dt : 'a delaytree) (k : 'a cascade) : 'a cascade =
  fun () -> delaytree2head dt k

and delaytree2head (dt : 'a delaytree) (k : 'a cascade) : 'a head =
  match dt with
  | DTNothing ->
      force k
  | DTElement x ->
     Cons (x, k)
  | DTConcat (dt1, dt2) ->
     delaytree2head dt1 (delaytree2cascade dt2 k)
  | DTDelay dt ->
     delaytree2head (force dt) k

let delaytree2cascade dt =
  delaytree2cascade dt nil

let yield _env x =
  DTElement x

class ['self] delaytree_monoid = object (_ : 'self)

  method private zero =
    DTNothing

  method private plus s1 s2 =
    match s1, s2 with
    | DTNothing, s
    | s, DTNothing ->
        (* An optional optimization. *)
        s
    | _, _ ->
        DTConcat (s1, s2)

  method visit_delay: 'env 'a .
    ('env -> 'a -> 'b delaytree) ->
    'env -> 'a delay -> 'b delaytree
  = fun visit_'a env x ->
      DTDelay (fun () -> visit_'a env x)

end

type 'a kctree =
  | Leaf
  | Node of 'a kctree * 'a * 'a kctree

type 'a t = 'a kctree =
  | Leaf
  | Node of 'a u * 'a * 'a u

and 'a u = 'a t delay
[@@deriving visitors { variety = "reduce"; ancestors = ["delaytree_monoid"]; polymorphic = true; concrete = true }]

class ['self] verbose_reduce = object (_ : 'self)
  inherit [_] reduce as super
  method! visit_Leaf visit_'a env =
    Printf.printf "Visiting a leaf.\n%!";
    super#visit_Leaf visit_'a env
  method! visit_Node visit_'a env t1 x t2 =
    Printf.printf "Visiting a node.\n%!";
    super#visit_Node visit_'a env t1 x t2
end

let t2delaytree (t : 'a kctree) =
  new verbose_reduce # visit_u yield () t

let t2cascade t =
  delaytree2cascade (t2delaytree t)

let t2iterator (t : 'a kctree) : 'a iterator =
  cascade2iterator (t2cascade t)

let t =
  Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf))

let i = t2iterator t

let _ = i()

let u =
  Node (Node (Leaf, false, Leaf), true, Node (Leaf, false, Leaf))

let j = t2iterator u

let _ = j()
