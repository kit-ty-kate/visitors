type void

class ['self] iter = object
  method private visit_'bn: void -> void -> void
  = fun _ _ -> assert false
end

class ['self] map = object
  method private visit_'bn: void -> void -> void
  = fun _ _ -> assert false
end

class ['self] reduce = object
  method private visit_'bn: void -> void -> void
  = fun _ _ -> assert false
end

class ['self] iter2 = object
  method private visit_'bn: void -> void -> void -> void
  = fun _ _ _ -> assert false
end

class ['self] map2 = object
  method private visit_'bn: void -> void -> void -> void
  = fun _ _ _ -> assert false
end

class ['self] reduce2 = object
  method private visit_'bn: void -> void -> void -> void
  = fun _ _ _ -> assert false
end
