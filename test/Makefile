TESTS := \
  $(shell ls *.ml | grep -v processed)

TARGETS := \
  $(patsubst %.ml,%.native,$(TESTS))

MLI := \
  $(patsubst %.ml,%.inferred.mli,$(TESTS))

OCAMLBUILD := \
  ocamlbuild \
  -j 0 \
  -use-ocamlfind \
  -classic-display \
  -cflags "-g" -lflags "-g" \

.PHONY: test mli processed clean

test:
	@ make -C ../src uninstall || true
	@ make -C ../src install
	@ $(OCAMLBUILD) $(TARGETS)
#	@ for i in $(TARGET) ; do echo "Running $$i..." && ./$$i ; done

mli:
	@ make -C ../src uninstall || true
	@ make -C ../src install
	@ $(OCAMLBUILD) $(MLI)
	@ more $(patsubst %,_build/%,$(MLI))

include ../src/Makefile.preprocess

processed: $(patsubst %.ml,%.processed.ml,$(TESTS))

clean:
	@ rm -f *~
	@ rm -f *.processed.ml
	@ $(OCAMLBUILD) -clean
