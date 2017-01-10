module SetupSize = struct
  module Int = struct
    let reduce _env _i = 1
  end
end

module Monoid = struct
  let zero = 0
  let plus = (+)
end

type expr =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { name = "Size"; variety = "reduce"; path = ["SetupSize"]; final = true }]

let size : expr -> int =
  Size.visit_expr ()
