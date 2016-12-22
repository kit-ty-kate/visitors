exception StructuralMismatch

module List : sig
  val iter2: ('env -> 'a -> 'b -> unit) -> 'env -> 'a list -> 'b list ->    unit
  val map2:  ('env -> 'a -> 'b ->   'c) -> 'env -> 'a list -> 'b list -> 'c list
end

module Option : sig
  val iter2: ('env -> 'a -> 'b -> unit) -> 'env -> 'a option -> 'b option ->      unit
  val map2:  ('env -> 'a -> 'b ->   'c) -> 'env -> 'a option -> 'b option -> 'c option
end
