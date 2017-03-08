open Hashcons

(* Although we cannot implement the method [visit_hash_consed] yet, as we need
   a hash-consing table to do that, we can announce what its type should be.
   The key subtlety here is that this method must be monomorphic in ['b].
   Indeed, we cannot hope to be able to build values of type ['b hash_consed]
   for every ['b]. We can only hope to build values of type ['b hash_consed]
   for a fixed (as yet undetermined) ['b], if we have a hash-consing table of
   type ['b Hashcons.t]. *)

class virtual ['self] hashcons_map_placeholder = object (_ : 'self)
  method virtual visit_hash_consed: 'env 'a .
    ('env -> 'a -> 'b) ->
    'env -> 'a hash_consed -> 'b hash_consed
end

(* This allows us to define the types [expr] and [hexpr]... *)

type 'expr oexpr =
  | EConst of int
  | EAdd of 'expr * 'expr

and hexpr =
  H of hexpr oexpr hash_consed [@@unboxed]

(* ... and generate a visitor class for them, where [visit_hash_consed]
   is a virtual method. So far, so good, it seems. *)

[@@deriving visitors { variety = "map"; name = "map_incomplete";
                       polymorphic = true;
                       ancestors = ["hashcons_map_placeholder"] }]

(* Assuming that a hash-consing table is given, we can then use this
   table in a concrete definition of [visit_hash_consed]. *)

(* The type ['b] is instantiated here with [hexpr oexpr]. We do not
   have a choice: this type is fixed by the definition of [hexpr].
   The generated method [visit_hexpr] contains a call to
   [visit_hash_consed] at this type. *)

class ['self] map (table : hexpr oexpr Hashcons.t) = object (_ : 'self)
  inherit [_] map_incomplete
  method visit_hash_consed visit_'a env { node = e; _ } =
    hashcons table (visit_'a env e)
end
