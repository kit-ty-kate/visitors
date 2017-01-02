exception StructuralMismatch

let fail () =
  raise StructuralMismatch

module Inert = struct

  let iter _env _x =
    ()

  let map _env x =
    x

  let iter2 _env x1 x2 =
    if x1 = x2 then
      ()
    else
      fail()

  let map2 _env x1 x2 =
    if x1 = x2 then
      x1
    else
      fail()

end

module Array = struct

  let iter f env xs =
    Array.iter (f env) xs

  let map f env xs =
    Array.map (f env) xs

  let iter2 f env xs1 xs2 =
    if Array.length xs1 = Array.length xs2 then
      Array.iter2 (f env) xs1 xs2
    else
      fail()

  let map2 f env xs1 xs2 =
    if Array.length xs1 = Array.length xs2 then
      Array.map2 (f env) xs1 xs2
    else
      fail()

end

module Bool = Inert

module Char = Inert

module Float = Inert

module Int = Inert

module Int32 = Inert

module Int64 = Inert

module List = struct

  (* We could reuse the functions provided by OCaml's [List] module,
     as we did above for arrays. *)

  let rec iter f env xs =
    match xs with
    | [] ->
        ()
    | x :: xs ->
        f env x;
        iter f env xs

  let rec map f env xs =
    match xs with
    | [] ->
        []
    | x :: xs ->
        let x = f env x in
        x :: map f env xs

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

  let iter f env ox =
    match ox with
    | None ->
        ()
    | Some x ->
        f env x

  let map f env ox =
    match ox with
    | None ->
        None
    | Some x ->
        Some (f env x)

  let iter2 f env ox1 ox2 =
    match ox1, ox2 with
    | None, None ->
        ()
    | Some x1, Some x2 ->
        f env x1 x2
    | _, _ ->
        fail()

  let map2 f env ox1 ox2 =
    match ox1, ox2 with
    | None, None ->
        None
    | Some x1, Some x2 ->
        let x = f env x1 x2 in
        Some x
    | _, _ ->
        fail()

end

module Ref = struct

  let iter f env rx =
    f env !rx

  let map f env rx =
    ref (f env !rx)

  let iter2 f env rx1 rx2 =
    f env !rx1 !rx2

  let map2 f env rx1 rx2 =
    ref (f env !rx1 !rx2)

end

module String = Inert

module Unit = Inert
