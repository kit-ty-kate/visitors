(* This kit serves to construct an alpha-equivalence test. *)

(* The environment contains two maps of atoms to de Bruijn levels, as well as
   the current de Bruijn level. The equivalence test can in fact be viewed as
   two independent conversions to a locally-nameless representation, followed
   with a syntactic comparison of the two terms thus obtained. *)

type m =
  int Atom.Map.t

type env =
  m * m * int

let empty =
  Atom.Map.empty, Atom.Map.empty, 0

type status =
  | Local of int (* a de Bruijn level *)
  | Free         (* a free name *)

let lookup (m : m) (x : Atom.t) : status =
  try
    (* A local name. *)
    Local (Atom.Map.find x m)
  with Not_found ->
    Free

let extend (m : m) (n : int) (x : Atom.t) : m =
  assert (not (Atom.Map.mem x m));
  Atom.Map.add x n m

let extend x1 x2 (m1, m2, n) =
  let m1 = extend m1 n x1
  and m2 = extend m2 n x2
  and n = n + 1 in
  m1, m2, n

let lookup (m1, m2, _) x1 x2 =
  match lookup m1 x1, lookup m2 x2 with
  | Local i1, Local i2 ->
      if i1 <> i2 then
        VisitorsRuntime.fail()
  | Free, Free ->
      if not (Atom.equal x1 x2) then
        VisitorsRuntime.fail()
  | Local _, Free
  | Free, Local _ ->
      VisitorsRuntime.fail()

class ['self] iter2 = object (_ : 'self)
  method extend = extend
  method visit_'fn = lookup
end

let wrap2 f t1 t2 =
  try
    f t1 t2;
    true
  with VisitorsRuntime.StructuralMismatch ->
    false
