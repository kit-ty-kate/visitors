(* This means: please use [cppo] to generate [%.ml] from [%.cppo.ml]. *)
let () =
  Ocamlbuild_plugin.dispatch (fun phase ->
    Ocamlbuild_cppo.dispatcher phase
  )
