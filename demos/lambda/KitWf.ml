(* This kit serves to construct a well-formedness test. *)

(* TEMPORARY clarify: this imposes the UABH -- uniqueness along
   a branch -- not the GUH -- global uniqueness. *)

(* TEMPORARY Also, it checks that every atom is known. So, it
   tests whether a term is closed. Clarify these two roles.
   wf should fail with a clear error message (and return unit)
   closed should return a boolean
*)

(* The environment keeps track of which atoms are in scope. *)

type env = Atom.Set.t

let empty =
  Atom.Set.empty

let extend x env =
  (* Check the GUH. *)
  if Atom.Set.mem x env then
    VisitorsRuntime.fail();
  (* Enrich the environment. *)
  Atom.Set.add x env

let lookup env x =
  (* Check that every atom is known. *)
  if not (Atom.Set.mem x env) then
    VisitorsRuntime.fail()

class ['self] iter = object (_ : 'self)
  method extend = extend
  method visit_'fn = lookup
end

let wrap f t =
  try
    f t;
    true
  with VisitorsRuntime.StructuralMismatch ->
    false
