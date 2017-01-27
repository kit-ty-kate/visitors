An OCaml syntax extension (technically, a ppx_deriving plugin) which generates
object-oriented visitors for traversing and transforming data structures.

The easiest way of installing the latest released version of this package is
via `opam`, the OCaml package manager.
```bash
opam install visitors
```

To install the latest development version, also via `opam`, please proceed as follows:
```bash
  git clone git@gitlab.inria.fr:fpottier/visitors.git
  cd visitors
  make pin
```

To install the latest development version, outside of `opam`, please proceed as follows:
```bash
  git clone git@gitlab.inria.fr:fpottier/visitors.git
  cd visitors
  make -C src install
```
This requires `ocamlfind`, `ocamlbuild`, `ppx_tools`, and `ppx_deriving`.
