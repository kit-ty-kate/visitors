exception StructuralMismatch

module Array : sig
  val iter: ('env -> 'a -> unit) -> 'env -> 'a array ->     unit
  val map:  ('env -> 'a ->   'b) -> 'env -> 'a array -> 'b array
  val iter2: ('env -> 'a -> 'b -> unit) -> 'env -> 'a array -> 'b array ->     unit
  val map2:  ('env -> 'a -> 'b ->   'c) -> 'env -> 'a array -> 'b array -> 'c array
end

module Bool : sig
  val iter: 'env -> bool -> unit
  val map:  'env -> bool -> bool
  val iter2: 'env -> bool -> bool -> unit
  val map2:  'env -> bool -> bool -> bool
end

module Char : sig
  val iter: 'env -> char -> unit
  val map:  'env -> char -> char
  val iter2: 'env -> char -> char -> unit
  val map2:  'env -> char -> char -> char
end

module Float : sig
  val iter: 'env -> float ->  unit
  val map:  'env -> float -> float
  val iter2: 'env -> float -> float ->  unit
  val map2:  'env -> float -> float -> float
end

module Int : sig
  val iter: 'env -> int -> unit
  val map:  'env -> int ->  int
  val iter2: 'env -> int -> int -> unit
  val map2:  'env -> int -> int ->  int
end

module List : sig
  val iter: ('env -> 'a -> unit) -> 'env -> 'a list ->    unit
  val map:  ('env -> 'a ->   'b) -> 'env -> 'a list -> 'b list
  val iter2: ('env -> 'a -> 'b -> unit) -> 'env -> 'a list -> 'b list ->    unit
  val map2:  ('env -> 'a -> 'b ->   'c) -> 'env -> 'a list -> 'b list -> 'c list
end

module Option : sig
  val iter: ('env -> 'a -> unit) -> 'env -> 'a option ->      unit
  val map:  ('env -> 'a ->   'b) -> 'env -> 'a option -> 'b option
  val iter2: ('env -> 'a -> 'b -> unit) -> 'env -> 'a option -> 'b option ->      unit
  val map2:  ('env -> 'a -> 'b ->   'c) -> 'env -> 'a option -> 'b option -> 'c option
end

module Ref : sig
  val iter: ('env -> 'a -> unit) -> 'env -> 'a ref ->   unit
  val map:  ('env -> 'a ->   'b) -> 'env -> 'a ref -> 'b ref
  val iter2: ('env -> 'a -> 'b -> unit) -> 'env -> 'a ref -> 'b ref ->   unit
  val map2:  ('env -> 'a -> 'b ->   'c) -> 'env -> 'a ref -> 'b ref -> 'c ref
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
