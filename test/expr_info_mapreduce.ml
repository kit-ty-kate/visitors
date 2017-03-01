type 'info expr_node =
  | EConst of (int[@opaque])
  | EAdd of 'info expr * 'info expr

and 'info expr =
  { info: 'info; node: 'info expr_node }
  [@@deriving visitors { variety = "mapreduce" }]
