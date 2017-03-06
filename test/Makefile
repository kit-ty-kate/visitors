export VISITORS_BUILDING_DOCUMENTATION=true
  # For consistency with doc/Makefile, which relies on the same
  # set of %.processed.ml files.

TESTS := \
  $(shell ls *.ml | grep -v processed)

TARGETS := \
  $(patsubst %.ml,%.native,$(TESTS))

MLI := \
  $(patsubst %.ml,%.inferred.mli,$(TESTS))

OCAMLBUILD := \
  ocamlbuild \
  -use-ocamlfind \
  -classic-display \

.PHONY: test bench mli processed clean reinstall

test: reinstall
	@ echo Re-compiling...
	@ $(OCAMLBUILD) $(TARGETS)

%.native: reinstall
	@ echo Re-compiling...
	@ $(OCAMLBUILD) $@

bench: reinstall
	@ echo Re-compiling...
	@ $(OCAMLBUILD) bench.native
	@ echo Running...
	@ ./bench.native

mli: reinstall
	@ echo Re-compiling...
	@ $(OCAMLBUILD) $(MLI)

include ../src/Makefile.preprocess

processed: $(patsubst %.ml,%.processed.ml,$(TESTS))

clean:
	@ rm -f *~
	@ rm -f *.processed.ml
	@ $(OCAMLBUILD) -clean

reinstall:
	@ echo Re-installing...
	@ make -C ../src reinstall >/dev/null 2>/dev/null
