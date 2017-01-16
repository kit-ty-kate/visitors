(* -------------------------------------------------------------------------- *)

(* An exception used at arity 2 and above. *)

exception StructuralMismatch

let fail () =
  raise StructuralMismatch

(* -------------------------------------------------------------------------- *)

(* A virtual base class for monoids. *)

class virtual ['z] monoid = object
  method private virtual zero: 'z
  method private virtual plus: 'z -> 'z -> 'z
end

(* -------------------------------------------------------------------------- *)

(* Common monoids. *)

class ['z] addition_monoid = object
  inherit ['z] monoid
  method private zero = 0
  method private plus = (+)
end

(* -------------------------------------------------------------------------- *)

(* Module-based packaging. *)

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

(* -------------------------------------------------------------------------- *)

(* Class-based packaging. *)

class ['self] iter = object (self)

  method private visit_array: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a array -> unit
  = fun f env xs ->
      (* For speed, we inline [Array.iter]. Chances are, we save a closure
         allocation, as using [Array.iter] would require us to build [f env]. *)
      for i = 0 to Array.length xs - 1 do
        f env (Array.unsafe_get xs i)
      done

  method private visit_bool: 'env .
    'env -> bool -> unit
  = fun _ _ -> ()

  method private visit_char: 'env .
    'env -> char -> unit
  = fun _ _ -> ()

  method private visit_float: 'env .
    'env -> float -> unit
  = fun _ _ -> ()

  method private visit_int: 'env .
    'env -> int -> unit
  = fun _ _ -> ()

  method private visit_int32: 'env .
    'env -> int32 -> unit
  = fun _ _ -> ()

  method private visit_int64: 'env .
    'env -> int64 -> unit
  = fun _ _ -> ()

  method private visit_list: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a list -> unit
  = fun f env xs ->
      match xs with
      | [] ->
          ()
      | x :: xs ->
          f env x;
          self # visit_list f env xs

  method private visit_option: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a option -> unit
  = Option.iter

  method private visit_ref: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
  = Ref.iter

  method private visit_result: 'env 'a 'e.
    ('env -> 'a -> unit) ->
    ('env -> 'e -> unit) ->
     'env -> ('a, 'e) result -> unit
  = Result.iter

  method private visit_string: 'env .
    'env -> string -> unit
  = fun _ _ -> ()

  method private visit_unit: 'env .
    'env -> unit -> unit
  = fun _ _ -> ()

end

class ['self] map = object (self)

  method private visit_array: 'env 'a 'b .
    ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
  = fun f env xs ->
      Array.map (f env) xs

  method private visit_bool: 'env .
    'env -> bool -> bool
  = fun _ x -> x

  method private visit_char: 'env .
    'env -> char -> char
  = fun _ x -> x

  method private visit_float: 'env .
    'env -> float -> float
  = fun _ x -> x

  method private visit_int: 'env .
    'env -> int -> int
  = fun _ x -> x

  method private visit_int32: 'env .
    'env -> int32 -> int32
  = fun _ x -> x

  method private visit_int64: 'env .
    'env -> int64 -> int64
  = fun _ x -> x

  method private visit_list: 'env 'a 'b .
    ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
  = fun f env xs ->
      match xs with
      | [] ->
          []
      | x :: xs ->
          let x = f env x in
          x :: self # visit_list f env xs

  method private visit_option: 'env 'a 'b .
    ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
  = Option.map

  method private visit_ref: 'env 'a 'b .
    ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
  = Ref.map

  method private visit_result: 'env 'a 'b 'e 'f .
    ('env -> 'a -> 'b) ->
    ('env -> 'e -> 'f) ->
     'env -> ('a, 'e) result -> ('b, 'f) result
  = Result.map

  method private visit_string: 'env .
    'env -> string -> string
  = fun _ x -> x

  method private visit_unit: 'env .
    'env -> unit -> unit
  = fun _ x -> x

end

class virtual ['self] reduce = object (self : 'self)

  inherit ['z] monoid

  method private visit_array: 'env 'a .
    ('env -> 'a -> 'z) -> 'env -> 'a array -> 'z
  = fun f env xs ->
      Array.fold_left (fun z x -> self#plus z (f env x)) self#zero xs

  method private visit_bool: 'env .
    'env -> bool -> 'z
  = fun _env _ -> self#zero

  method private visit_char: 'env .
    'env -> char -> 'z
  = fun _env _ -> self#zero

  method private visit_float: 'env .
    'env -> float -> 'z
  = fun _env _ -> self#zero

  method private visit_int: 'env .
    'env -> int -> 'z
  = fun _env _ -> self#zero

  method private visit_int32: 'env .
    'env -> int32 -> 'z
  = fun _env _ -> self#zero

  method private visit_int64: 'env .
    'env -> int64 -> 'z
  = fun _env _ -> self#zero

  method private visit_list: 'env 'a .
    ('env -> 'a -> 'z) -> 'env -> 'a list -> 'z
  = fun f env xs ->
      List.fold_left (fun z x -> self#plus z (f env x)) self#zero xs

  method private visit_option: 'env 'a .
    ('env -> 'a -> 'z) -> 'env -> 'a option -> 'z
  = fun f env ox ->
      match ox with
      | Some x ->
          f env x
      | None ->
          self#zero

  method private visit_ref: 'env 'a .
    ('env -> 'a -> 'z) -> 'env -> 'a ref -> 'z
  = fun f env rx ->
      f env !rx

  method private visit_result: 'env 'a 'e .
    ('env -> 'a -> 'z) ->
    ('env -> 'e -> 'z) ->
     'env -> ('a, 'e) result -> 'z
  = fun f g env r ->
      match r with
      | Ok a ->
          f env a
      | Error b ->
          g env b

  method private visit_string: 'env .
    'env -> string -> 'z
  = fun _env _ -> self#zero

  method private visit_unit: 'env .
    'env -> unit -> 'z
  = fun _env _ -> self#zero

end

class ['self] iter2 = object (self)

  method private visit_array: 'env 'a 'b .
    ('env -> 'a -> 'b -> unit) -> 'env -> 'a array -> 'b array -> unit
  = fun f env xs1 xs2 ->
      (* We inline [Array.iter2]. *)
      if Array.length xs1 = Array.length xs2 then
        for i = 0 to Array.length xs1 - 1 do
          f env (Array.unsafe_get xs1 i) (Array.unsafe_get xs2 i)
        done
      else
        fail()

  method private visit_bool: 'env .
    'env -> bool -> bool -> unit
  = fun _ x1 x2 -> if x1 = x2 then () else fail()

  method private visit_char: 'env .
    'env -> char -> char -> unit
  = fun _ x1 x2 -> if x1 = x2 then () else fail()

  method private visit_float: 'env .
    'env -> float -> float -> unit
  = fun _ x1 x2 -> if x1 = x2 then () else fail()

  method private visit_int: 'env .
    'env -> int -> int -> unit
  = fun _ x1 x2 -> if x1 = x2 then () else fail()

  method private visit_int32: 'env .
    'env -> int32 -> int32 -> unit
  = fun _ x1 x2 -> if x1 = x2 then () else fail()

  method private visit_int64: 'env .
    'env -> int64 -> int64 -> unit
  = fun _ x1 x2 -> if x1 = x2 then () else fail()

  method private visit_list: 'env 'a 'b .
    ('env -> 'a -> 'b -> unit) -> 'env -> 'a list -> 'b list -> unit
  = fun f env xs1 xs2 ->
      match xs1, xs2 with
      | [], [] ->
          ()
      | x1 :: xs1, x2 :: xs2 ->
          f env x1 x2;
          self # visit_list f env xs1 xs2
      | _, _ ->
          fail()

  method private visit_option: 'env 'a 'b .
    ('env -> 'a -> 'b -> unit) -> 'env -> 'a option -> 'b option -> unit
  = Option.iter2

  method private visit_ref: 'env 'a 'b .
    ('env -> 'a -> 'b -> unit) -> 'env -> 'a ref -> 'b ref -> unit
  = Ref.iter2

  method private visit_result: 'env 'a 'b 'e 'f .
    ('env -> 'a -> 'b -> unit) ->
    ('env -> 'e -> 'f -> unit) ->
     'env -> ('a, 'e) result -> ('b, 'f) result -> unit
  = Result.iter2

  method private visit_string: 'env .
    'env -> string -> string -> unit
  = fun _ x1 x2 -> if x1 = x2 then () else fail()

  method private visit_unit: 'env .
    'env -> unit -> unit -> unit
  = fun _ _x1 _x2 -> ()

end

class ['self] map2 = object (self)

  method private visit_array: 'env 'a 'b 'c .
    ('env -> 'a -> 'b -> 'c) -> 'env -> 'a array -> 'b array -> 'c array
  = fun f env xs1 xs2 ->
      if Array.length xs1 = Array.length xs2 then
        Array.map2 (f env) xs1 xs2
      else
        fail()

  method private visit_bool: 'env .
    'env -> bool -> bool -> bool
  = fun _ x1 x2 -> if x1 = x2 then x1 else fail()

  method private visit_char: 'env .
    'env -> char -> char -> char
  = fun _ x1 x2 -> if x1 = x2 then x1 else fail()

  method private visit_float: 'env .
    'env -> float -> float -> float
  = fun _ x1 x2 -> if x1 = x2 then x1 else fail()

  method private visit_int: 'env .
    'env -> int -> int -> int
  = fun _ x1 x2 -> if x1 = x2 then x1 else fail()

  method private visit_int32: 'env .
    'env -> int32 -> int32 -> int32
  = fun _ x1 x2 -> if x1 = x2 then x1 else fail()

  method private visit_int64: 'env .
    'env -> int64 -> int64 -> int64
  = fun _ x1 x2 -> if x1 = x2 then x1 else fail()

  method private visit_list: 'env 'a 'b 'c .
    ('env -> 'a -> 'b -> 'c) -> 'env -> 'a list -> 'b list -> 'c list
  = fun f env xs1 xs2 ->
      match xs1, xs2 with
      | [], [] ->
          []
      | x1 :: xs1, x2 :: xs2 ->
          let x = f env x1 x2 in
          x :: self # visit_list f env xs1 xs2
      | _, _ ->
          fail()

  method private visit_option: 'env 'a 'b 'c .
    ('env -> 'a -> 'b -> 'c) -> 'env -> 'a option -> 'b option -> 'c option
  = Option.map2

  method private visit_ref: 'env 'a 'b 'c .
    ('env -> 'a -> 'b -> 'c) -> 'env -> 'a ref -> 'b ref -> 'c ref
  = Ref.map2

  method private visit_result: 'env 'a 'b 'c 'e 'f 'g .
    ('env -> 'a -> 'b -> 'c) ->
    ('env -> 'e -> 'f -> 'g) ->
     'env -> ('a, 'e) result -> ('b, 'f) result -> ('c, 'g) result
  = Result.map2

  method private visit_string: 'env .
    'env -> string -> string -> string
  = fun _ x1 x2 -> if x1 = x2 then x1 else fail()

  method private visit_unit: 'env .
    'env -> unit -> unit -> unit
  = fun _ _x1 _x2 -> ()

end

class virtual ['self] reduce2 = object (self : 'self)

  inherit ['z] monoid

  method private visit_array: 'env 'a 'b .
    ('env -> 'a -> 'b -> 'z) -> 'env -> 'a array -> 'b array -> 'z
  = fun f env xs1 xs2 ->
      (* OCaml does not offer [Array.fold_left2], so we use [Array.iter2],
         which we inline. *)
      if Array.length xs1 = Array.length xs2 then
        let z = ref self#zero in
        for i = 0 to Array.length xs1 - 1 do
          let x1 = Array.unsafe_get xs1 i
          and x2 = Array.unsafe_get xs2 i in
          z := self#plus !z (f env x1 x2)
        done;
        !z
      else
        fail()

  method private visit_bool: 'env .
    'env -> bool -> bool -> 'z
  = fun _env x1 x2 ->
      if x1 = x2 then self#zero else fail()

  method private visit_char: 'env .
    'env -> char -> char -> 'z
  = fun _env x1 x2 ->
      if x1 = x2 then self#zero else fail()

  method private visit_float: 'env .
    'env -> float -> float -> 'z
  = fun _env x1 x2 ->
      if x1 = x2 then self#zero else fail()

  method private visit_int: 'env .
    'env -> int -> int -> 'z
  = fun _env x1 x2 ->
      if x1 = x2 then self#zero else fail()

  method private visit_int32: 'env .
    'env -> int32 -> int32 -> 'z
  = fun _env x1 x2 ->
      if x1 = x2 then self#zero else fail()

  method private visit_int64: 'env .
    'env -> int64 -> int64 -> 'z
  = fun _env x1 x2 ->
      if x1 = x2 then self#zero else fail()

  method private visit_list: 'env 'a 'b .
    ('env -> 'a -> 'b -> 'z) -> 'env -> 'a list -> 'b list -> 'z
  = fun f env xs1 xs2 ->
      if List.length xs1 = List.length xs2 then
        List.fold_left2 (fun z x1 x2 -> self#plus z (f env x1 x2)) self#zero xs1 xs2
      else
        fail()

  method private visit_option: 'env 'a 'b .
    ('env -> 'a -> 'b -> 'z) -> 'env -> 'a option -> 'b option -> 'z
  = fun f env ox1 ox2 ->
      match ox1, ox2 with
      | Some x1, Some x2 ->
          f env x1 x2
      | None, None ->
          self#zero
      | Some _, None
      | None, Some _ ->
          fail()

  method private visit_ref: 'env 'a 'b .
    ('env -> 'a -> 'b -> 'z) -> 'env -> 'a ref -> 'b ref -> 'z
  = fun f env rx1 rx2 ->
      f env !rx1 !rx2

  method private visit_result: 'env 'a 'b 'e 'f .
    ('env -> 'a -> 'b -> 'z) ->
    ('env -> 'e -> 'f -> 'z) ->
     'env -> ('a, 'e) result -> ('b, 'f) result -> 'z
  = fun f g env r1 r2 ->
      match r1, r2 with
      | Ok a1, Ok a2 ->
          f env a1 a2
      | Error b1, Error b2 ->
          g env b1 b2
      | Ok _, Error _
      | Error _, Ok _ ->
          fail()

  method private visit_string: 'env .
    'env -> string -> string -> 'z
  = fun _env x1 x2 ->
      if x1 = x2 then self#zero else fail()

  method private visit_unit: 'env .
    'env -> unit -> unit -> 'z
  = fun _env () () ->
      self#zero

end
