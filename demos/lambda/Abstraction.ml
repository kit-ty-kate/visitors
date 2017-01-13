(* -------------------------------------------------------------------------- *)

(* A universal, concrete type of single-name abstractions. *)

(* We wish to represent all kinds of abstractions -- e.g. in nominal style,
   in de Bruijn style, etc. -- so we parameterize the abstraction over the
   type ['bn] of the bound name and over the type ['term] of the body. This
   makes this type definition almost trivial -- it is just a pair -- but it
   still serves as a syntactic marker of where abstractions are located. *)

type ('bn, 'term) abstraction =
  'bn * 'term

(* -------------------------------------------------------------------------- *)

(* The main effect of an abstraction is to cause the environment to be
   enriched when the abstraction is traversed. The following classes define
   where the environment is enriched. *)

(* These classes do not know the type of the environment, and do not know how
   it is enriched; the latter task is delegated to virtual methods, such as
   [extend] and [restrict]. The implementation of these methods is provided
   by separate ``kits''. *)

(* We need one class per variety of visitor, which is a bit painful. *)

(* The method [visit_abstraction] is polymorphic in the type of terms. This is
   important, as it means that one can use several instances of [abstraction]
   in a single type definition and still be able to construct well-typed
   visitors. *)

(* The virtual methods [extend] and [restrict] are not polymorphic in the
   types of bound names and environments. On the contrary, each kit comes
   with certain specific types of bound names and environments. *)

(* Because [iter] and [iter2] are special cases of [reduce] and [reduce2],
   respectively, we define them that way, so as to save effort. *)

class virtual ['self] map = object (self : 'self)

  method virtual extend: 'bn1 -> 'env -> 'bn2 * 'env

  method visit_abstraction: 'term1 'term2 .
    _ ->
    ('env -> 'term1 -> 'term2) ->
    'env -> ('bn1, 'term1) abstraction -> ('bn2, 'term2) abstraction
  = fun _ f env (x, body) ->
      let x', env' = self#extend x env in
      x', f env' body

end

class virtual ['self] reduce = object (self : 'self)

  method virtual extend: 'bn -> 'env -> 'env

  method virtual restrict: 'bn -> 'z -> 'z

  method visit_abstraction: 'term .
    _ ->
    ('env -> 'term -> 'z) ->
    'env -> ('bn, 'term) abstraction -> 'z
  = fun _ f env (x, body) ->
      let env' = self#extend x env in
      self#restrict x (f env' body)

end

class virtual ['self] iter = object (_ : 'self)

  inherit ['self] reduce

  method restrict _ () = ()

end

class virtual ['self] map2 = object (self : 'self)

  method virtual extend: 'bn1 -> 'bn2 -> 'env -> 'bn3 * 'env

  method visit_abstraction: 'term1 'term2 'term3 .
    _ ->
    ('env -> 'term1 -> 'term2 -> 'term3) ->
    'env -> ('bn1, 'term1) abstraction -> ('bn2, 'term2) abstraction -> ('bn3, 'term3) abstraction
  = fun _ f env (x1, body1) (x2, body2) ->
      let x3, env' = self#extend x1 x2 env in
      x3, f env' body1 body2

end

class virtual ['self] reduce2 = object (self : 'self)

  method virtual extend: 'bn1 -> 'bn2 -> 'env -> 'env

  method virtual restrict: 'bn1 -> 'bn2 -> 'z -> 'z

  method visit_abstraction: 'term1 'term2 .
    _ ->
    ('env -> 'term1 -> 'term2 -> 'z) ->
    'env -> ('bn1, 'term1) abstraction -> ('bn2, 'term2) abstraction -> 'z
  = fun _ f env (x1, body1) (x2, body2) ->
      let env' = self#extend x1 x2 env in
      self#restrict x1 x2 (f env' body1 body2)

end

class virtual ['self] iter2 = object (_ : 'self)

  inherit ['self] reduce2

  method restrict _ _ () = ()

end
