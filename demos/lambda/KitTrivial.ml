(* This is a trivial kit. At an abstraction or at a name occurrence,
   nothing special happens. The type of the environment is
   undetermined. *)

class virtual ['self] iter = object (_ : 'self)
  method extend _x env = env
  method visit_'fn _env _x = ()
end

class virtual ['self] map = object (_ : 'self)
  method extend x env = x, env
  method visit_'fn _env x = x
end

class virtual ['self] reduce = object (self : 'self)
  method virtual zero: _
  method extend _x env = env
  method restrict _x z = z
  method visit_'fn _env _x = self#zero
end

(* TEMPORARY iter2, map2, reduce2 *)
