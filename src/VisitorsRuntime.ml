exception StructuralMismatch

let fail () =
  raise StructuralMismatch

module List = struct

  let rec iter2 f env xs1 xs2 =
    match xs1, xs2 with
    | [], [] ->
        ()
    | x1 :: xs1, x2 :: xs2 ->
        f env x1 x2;
        iter2 f env xs1 xs2
    | _, _ ->
        fail()

  let rec map2 f env xs1 xs2 =
    match xs1, xs2 with
    | [], [] ->
        []
    | x1 :: xs1, x2 :: xs2 ->
        let x = f env x1 x2 in
        x :: map2 f env xs1 xs2
    | _, _ ->
        fail()

end

module Option = struct

  let rec iter2 f env ox1 ox2 =
    match ox1, ox2 with
    | None, None ->
        ()
    | Some x1, Some x2 ->
        f env x1 x2
    | _, _ ->
        fail()

  let rec map2 f env ox1 ox2 =
    match ox1, ox2 with
    | None, None ->
        None
    | Some x1, Some x2 ->
        let x = f env x1 x2 in
        Some x
    | _, _ ->
        fail()

end
