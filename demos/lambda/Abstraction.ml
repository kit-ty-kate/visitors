type ('bn, 'term) abstraction =
  'bn * 'term

(* Initial conversion of strings to atoms. We create unique atoms
   right away, even though this is not necessary in principle. *)

module String2Atom = struct
  module StringMap = Map.Make(String)
  module Abstraction = struct
    let map _ f env (s, body) =
      let a = Atom.freshh s in
      let env = StringMap.add s a env in
      a, f env body
  end
end

module Nominal = struct
  module Abstraction = struct
    let iter _ f env (x, body) =
      let env = Atom.Set.add x env in
      f env body
  end
end

module Nominal2DeBruijn = struct
  type env =
    int Atom.Map.t * int
  module Abstraction = struct
    let map _ f (env, n) (x, body) =
      let env = Atom.Map.add x n env in
      (), f (env, n+1) body
  end
end

type void

class ['self] visit_'bn = object (_ : 'self)
  method visit_'bn (_ : void) (_ : void) : _ =
    assert false (* never invoked *)
end

class fv = object
  val mutable accu = Atom.Set.empty
  method visit_'fn (env : 'env) x =
    if not (Atom.Set.mem x env) then
      accu <- Atom.Set.add x accu
  method accu = accu
end

class n2db = object
  method visit_'fn (env, n) x =
    let level = Atom.Map.find x env in
    n - level
  method visit_'bn (_ : Nominal2DeBruijn.env) (_ : void) : unit =
    assert false (* never invoked *)
end
