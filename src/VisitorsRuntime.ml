(* -------------------------------------------------------------------------- *)

(* An exception used at arity 2 and above. *)

exception StructuralMismatch

let fail () =
  raise StructuralMismatch

(* -------------------------------------------------------------------------- *)

(* Module-based packaging. *)

module Inert = struct

  let iter _env _x =
    ()

  let map _env x =
    x

  let iter2 _env x1 x2 =
    if x1 = x2 then
      ()
    else
      fail()

  let map2 _env x1 x2 =
    if x1 = x2 then
      x1
    else
      fail()

end

module Array = struct

  let iter f env xs =
    Array.iter (f env) xs

  let map f env xs =
    Array.map (f env) xs

  let iter2 f env xs1 xs2 =
    if Array.length xs1 = Array.length xs2 then
      Array.iter2 (f env) xs1 xs2
    else
      fail()

  let map2 f env xs1 xs2 =
    if Array.length xs1 = Array.length xs2 then
      Array.map2 (f env) xs1 xs2
    else
      fail()

end

module Bool = Inert

module Char = Inert

module Float = Inert

module Int = Inert

module Int32 = Inert

module Int64 = Inert

module List = struct

  (* We could reuse the functions provided by OCaml's [List] module,
     as we did above for arrays. *)

  let rec iter f env xs =
    match xs with
    | [] ->
        ()
    | x :: xs ->
        f env x;
        iter f env xs

  let rec map f env xs =
    match xs with
    | [] ->
        []
    | x :: xs ->
        let x = f env x in
        x :: map f env xs

  let rec iter2 f env xs1 xs2 =
    match xs1, xs2 with
    | [], [] ->
        ()
    | x1 :: xs1, x2 :: xs2 ->
        f env x1 x2;
        iter2 f env xs1 xs2
    | _, _ ->
        fail()

  let rec map2 f env xs1 xs2 =
    match xs1, xs2 with
    | [], [] ->
        []
    | x1 :: xs1, x2 :: xs2 ->
        let x = f env x1 x2 in
        x :: map2 f env xs1 xs2
    | _, _ ->
        fail()

end

module Option = struct

  let iter f env ox =
    match ox with
    | None ->
        ()
    | Some x ->
        f env x

  let map f env ox =
    match ox with
    | None ->
        None
    | Some x ->
        Some (f env x)

  let iter2 f env ox1 ox2 =
    match ox1, ox2 with
    | None, None ->
        ()
    | Some x1, Some x2 ->
        f env x1 x2
    | _, _ ->
        fail()

  let map2 f env ox1 ox2 =
    match ox1, ox2 with
    | None, None ->
        None
    | Some x1, Some x2 ->
        let x = f env x1 x2 in
        Some x
    | _, _ ->
        fail()

end

module Ref = struct

  let iter f env rx =
    f env !rx

  let map f env rx =
    ref (f env !rx)

  let iter2 f env rx1 rx2 =
    f env !rx1 !rx2

  let map2 f env rx1 rx2 =
    ref (f env !rx1 !rx2)

end

module Result = struct

  let iter f g env r =
    match r with
    | Ok a -> f env a
    | Error b -> g env b

  let map f g env r =
    match r with
    | Ok a -> Ok (f env a)
    | Error b -> Error (g env b)

  let iter2 f g env r1 r2 =
    match r1, r2 with
    | Ok a1, Ok a2 -> f env a1 a2
    | Error b1, Error b2 -> g env b1 b2
    | _, _ -> fail()

  let map2 f g env r1 r2 =
    match r1, r2 with
    | Ok a1, Ok a2 -> Ok (f env a1 a2)
    | Error b1, Error b2 -> Error (g env b1 b2)
    | _, _ -> fail()

end

module String = Inert

module Unit = Inert

(* -------------------------------------------------------------------------- *)

(* Class-based packaging. *)

class ['self] iter = object

  method visit_array: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a array -> unit
  = Array.iter

  method visit_bool: 'env .
    'env -> bool -> unit
  = Bool.iter

  method visit_char: 'env .
    'env -> char -> unit
  = Char.iter

  method visit_float: 'env .
    'env -> float -> unit
  = Float.iter

  method visit_int: 'env .
    'env -> int -> unit
  = Int.iter

  method visit_int32: 'env .
    'env -> int32 -> unit
  = Int32.iter

  method visit_int64: 'env .
    'env -> int64 -> unit
  = Int64.iter

  method visit_list: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a list -> unit
  = List.iter

  method visit_option: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a option -> unit
  = Option.iter

  method visit_ref: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
  = Ref.iter

  method visit_result: 'env 'a 'e.
    ('env -> 'a -> unit) ->
    ('env -> 'e -> unit) ->
     'env -> ('a, 'e) result -> unit
  = Result.iter

  method visit_string: 'env .
    'env -> string -> unit
  = String.iter

  method visit_unit: 'env .
    'env -> unit -> unit
  = Unit.iter

end

class ['self] map = object

  method visit_array: 'env 'a 'b .
    ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
  = Array.map

  method visit_bool: 'env .
    'env -> bool -> bool
  = Bool.map

  method visit_char: 'env .
    'env -> char -> char
  = Char.map

  method visit_float: 'env .
    'env -> float -> float
  = Float.map

  method visit_int: 'env .
    'env -> int -> int
  = Int.map

  method visit_int32: 'env .
    'env -> int32 -> int32
  = Int32.map

  method visit_int64: 'env .
    'env -> int64 -> int64
  = Int64.map

  method visit_list: 'env 'a 'b .
    ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
  = List.map

  method visit_option: 'env 'a 'b .
    ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
  = Option.map

  method visit_ref: 'env 'a 'b .
    ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
  = Ref.map

  method visit_result: 'env 'a 'b 'e 'f .
    ('env -> 'a -> 'b) ->
    ('env -> 'e -> 'f) ->
     'env -> ('a, 'e) result -> ('b, 'f) result
  = Result.map

  method visit_string: 'env .
    'env -> string -> string
  = String.map

  method visit_unit: 'env .
    'env -> unit -> unit
  = Unit.map

end
