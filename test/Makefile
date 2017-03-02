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

.PHONY: test bench mli processed clean adhoc

test:
	@ make -C ../src reinstall
	@ $(OCAMLBUILD) $(TARGETS)
#	@ for i in $(TARGET) ; do echo "Running $$i..." && ./$$i ; done

%.native: clean
	@ echo Re-installing...
	@ make -C ../src reinstall >/dev/null 2>/dev/null
	@ echo Re-compiling...
	@ $(OCAMLBUILD) $@

bench:
	@ make -C ../src reinstall
	@ $(OCAMLBUILD) bench.native
	@ ./bench.native

mli:
	@ make -C ../src reinstall
	@ $(OCAMLBUILD) $(MLI)

include ../src/Makefile.preprocess

processed: $(patsubst %.ml,%.processed.ml,$(TESTS))

clean:
	@ rm -f *~
	@ rm -f *.processed.ml
	@ $(OCAMLBUILD) -clean

adhoc: expr_info_polymorphic.native
	@ make expr_info_polymorphic.processed.ml
