(* This kit serves to construct a [show] function for terms. At an abstraction
   or at a name occurrence, [Atom.show] is applied. *)

class ['self] map = object (_ : 'self)
  method extend x env = Atom.show x, env
  method visit_'fn _env x = Atom.show x
end
