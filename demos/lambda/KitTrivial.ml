(* This is a trivial kit. *)

(* At an abstraction or at a name occurrence, nothing special happens. *)

(* The type of the environment is undetermined. *)

class ['self] iter = object (_ : 'self)
  method private extend _x env = env
  method private visit_'fn _env _x = ()
end

class ['self] map = object (_ : 'self)
  method private extend x env = x, env
  method private visit_'fn _env x = x
end

class virtual ['self] reduce = object (self : 'self)
  method private virtual zero: _
  method private extend _x env = env
  method private restrict _x z = z
  method private visit_'fn _env _x = self#zero
end

(* TEMPORARY iter2, map2, reduce2 *)
