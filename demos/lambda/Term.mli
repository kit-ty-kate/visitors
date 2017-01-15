type ('fn, 'bn) term =
    TVar of 'fn
  | TLambda of ('bn, ('fn, 'bn) term) Abstraction.abstraction
  | TApp of ('fn, 'bn) term * ('fn, 'bn) term

class virtual ['self] iter : object ('self)
  method private virtual extend : 'bn -> 'env -> 'env
  method private virtual visit_'fn : 'env -> 'fn -> _ (* TEMPORARY why can't I write [unit]? *)
  method visit_term : 'env -> ('fn, 'bn) term -> unit
end

class virtual ['self] map : object ('self)
  method private virtual extend : 'bn1 -> 'env -> 'bn2 * 'env
  method private virtual visit_'fn : 'env -> 'fn1 -> 'fn2
  method visit_term : 'env -> ('fn1, 'bn1) term -> ('fn2, 'bn2) term
  method private visit_TVar : 'env -> 'fn1 -> ('fn2, 'bn2) term
end

class virtual ['self] reduce : object ('self)
  method private virtual extend : 'bn -> 'env -> 'env
  method private virtual plus : 'z -> 'z -> 'z
  method private virtual restrict : 'bn -> 'z -> 'z
  method private virtual visit_'fn : 'env -> 'fn -> 'z
  method visit_term : 'env -> ('fn, 'bn) term -> 'z
  method private virtual zero : 'z
end

class virtual ['self] iter2 : object ('self)
  method private virtual extend : 'bn1 -> 'bn2 -> 'env -> 'env
  method private virtual visit_'fn : 'env -> 'fn1 -> 'fn2 -> _
  method visit_term : 'env -> ('fn1, 'bn1) term -> ('fn2, 'bn2) term -> unit
end

type raw_term = (string, string) term
type nominal_term = (Atom.t, Atom.t) term
type db_term = (int, unit) term
