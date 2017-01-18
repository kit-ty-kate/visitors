type t =
  | Array of t array
  | Bool of bool
  | Bytes of bytes
  | Char of char
  | Float of float
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Lazy of t Lazy.t
  | List of t list
  | Nativeint of nativeint
  | Option of t option
  | Ref of t ref
  | Result of (t, t) result
  | String of string
  | Unit of unit
  | Tuple2 of (t * t)
  | Tuple3 of (t * t * t)
[@@deriving
     visitors { variety = "iter" },
     visitors { variety = "map" },
     visitors { variety = "reduce" },
     visitors { variety = "endo" },
     visitors { variety = "iter2" },
     visitors { variety = "map2" },
     visitors { variety = "reduce2" }
]
