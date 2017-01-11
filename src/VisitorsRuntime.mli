exception StructuralMismatch

val fail: unit -> 'a

module Inert : sig
  val iter:  'env -> 'a -> unit
  val map:   'env -> 'a ->  'a
  val iter2: 'env -> 'a -> 'a -> unit
  val map2:  'env -> 'a -> 'a ->  'a
end

module Array : sig
  val iter:  ('env -> 'a -> unit) -> 'env -> 'a array ->     unit
  val map:   ('env -> 'a ->   'b) -> 'env -> 'a array -> 'b array
  val iter2: ('env -> 'a -> 'b -> unit) -> 'env -> 'a array -> 'b array ->     unit
  val map2:  ('env -> 'a -> 'b ->   'c) -> 'env -> 'a array -> 'b array -> 'c array
end

module Bool : sig
  val iter:  'env -> bool -> unit
  val map:   'env -> bool -> bool
  val iter2: 'env -> bool -> bool -> unit
  val map2:  'env -> bool -> bool -> bool
end

module Char : sig
  val iter:  'env -> char -> unit
  val map:   'env -> char -> char
  val iter2: 'env -> char -> char -> unit
  val map2:  'env -> char -> char -> char
end

module Float : sig
  val iter:  'env -> float ->  unit
  val map:   'env -> float -> float
  val iter2: 'env -> float -> float ->  unit
  val map2:  'env -> float -> float -> float
end

module Int : sig
  val iter:  'env -> int -> unit
  val map:   'env -> int ->  int
  val iter2: 'env -> int -> int -> unit
  val map2:  'env -> int -> int ->  int
end

module Int32 : sig
  val iter:  'env -> int32 -> unit
  val map:   'env -> int32 ->  int32
  val iter2: 'env -> int32 -> int32 -> unit
  val map2:  'env -> int32 -> int32 ->  int32
end

module Int64 : sig
  val iter:  'env -> int64 -> unit
  val map:   'env -> int64 ->  int64
  val iter2: 'env -> int64 -> int64 -> unit
  val map2:  'env -> int64 -> int64 ->  int64
end

module List : sig
  val iter:  ('env -> 'a -> unit) -> 'env -> 'a list ->    unit
  val map:   ('env -> 'a ->   'b) -> 'env -> 'a list -> 'b list
  val iter2: ('env -> 'a -> 'b -> unit) -> 'env -> 'a list -> 'b list ->    unit
  val map2:  ('env -> 'a -> 'b ->   'c) -> 'env -> 'a list -> 'b list -> 'c list
end

module Option : sig
  val iter:  ('env -> 'a -> unit) -> 'env -> 'a option ->      unit
  val map:   ('env -> 'a ->   'b) -> 'env -> 'a option -> 'b option
  val iter2: ('env -> 'a -> 'b -> unit) -> 'env -> 'a option -> 'b option ->      unit
  val map2:  ('env -> 'a -> 'b ->   'c) -> 'env -> 'a option -> 'b option -> 'c option
end

module Ref : sig
  val iter:  ('env -> 'a -> unit) -> 'env -> 'a ref ->   unit
  val map:   ('env -> 'a ->   'b) -> 'env -> 'a ref -> 'b ref
  val iter2: ('env -> 'a -> 'b -> unit) -> 'env -> 'a ref -> 'b ref ->   unit
  val map2:  ('env -> 'a -> 'b ->   'c) -> 'env -> 'a ref -> 'b ref -> 'c ref
end

module Result : sig
  val iter:  ('env -> 'a -> unit) ->
             ('env -> 'e -> unit) ->
              'env -> ('a, 'e) result -> unit
  val map:   ('env -> 'a -> 'b) ->
             ('env -> 'e -> 'f) ->
              'env -> ('a, 'e) result -> ('b, 'f) result
  val iter2: ('env -> 'a1 -> 'a2 -> unit) ->
             ('env -> 'e1 -> 'e2 -> unit) ->
              'env -> ('a1, 'e1) result -> ('a2, 'e2) result -> unit
  val map2:  ('env -> 'a1 -> 'a2 -> 'b) ->
             ('env -> 'e1 -> 'e2 -> 'f) ->
              'env -> ('a1, 'e1) result -> ('a2, 'e2) result -> ('b, 'f) result
end

module String : sig
  val iter: 'env -> string ->   unit
  val map:  'env -> string -> string
  val iter2: 'env -> string -> string ->   unit
  val map2:  'env -> string -> string -> string
end

module Unit : sig
  val iter: 'env -> unit -> unit
  val map:  'env -> unit -> unit
  val iter2: 'env -> unit -> unit -> unit
  val map2:  'env -> unit -> unit -> unit
end

(* -------------------------------------------------------------------------- *)

(* Class-based packaging. *)

class ['self] iter : object

  method visit_array: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a array -> unit

  method visit_bool: 'env .
    'env -> bool -> unit

  method visit_char: 'env .
    'env -> char -> unit

  method visit_float: 'env .
    'env -> float -> unit

  method visit_int: 'env .
    'env -> int -> unit

  method visit_int32: 'env .
    'env -> int32 -> unit

  method visit_int64: 'env .
    'env -> int64 -> unit

  method visit_list: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a list -> unit

  method visit_option: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a option -> unit

  method visit_ref: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a ref -> unit

  method visit_result: 'env 'a 'e.
    ('env -> 'a -> unit) ->
    ('env -> 'e -> unit) ->
     'env -> ('a, 'e) result -> unit

  method visit_string: 'env .
    'env -> string -> unit

  method visit_unit: 'env .
    'env -> unit -> unit

end
