exception StructuralMismatch

module Array : sig
  val iter: ('env -> 'a -> unit) -> 'env -> 'a array ->     unit
  val map:  ('env -> 'a ->   'b) -> 'env -> 'a array -> 'b array
  val iter2: ('env -> 'a -> 'b -> unit) -> 'env -> 'a array -> 'b array ->     unit
  val map2:  ('env -> 'a -> 'b ->   'c) -> 'env -> 'a array -> 'b array -> 'c array
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
