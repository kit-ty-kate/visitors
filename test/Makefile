TESTS := \
  $(wildcard *.ml)

TARGETS := \
  $(patsubst %.ml,%.native,$(TESTS))

MLI := \
  $(patsubst %.ml,%.inferred.mli,$(TESTS))

OCAMLBUILD := \
  ocamlbuild \
  -use-ocamlfind \
  -classic-display \
  -cflags "-g" -lflags "-g" \

.PHONY: test mli clean

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

clean:
	@ rm -f *~
	@ $(OCAMLBUILD) -clean
