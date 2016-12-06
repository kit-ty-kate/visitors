OCAMLBUILD_TARGET := \
  test.native

OCAMLBUILD_FLAGS := \
  -use-ocamlfind \
  -classic-display \
  -cflags "-g -dsource" -lflags "-g" \

.PHONY: test clean

test:
	make -C ../src uninstall || true
	make -C ../src install
	ocamlbuild $(OCAMLBUILD_FLAGS) $(OCAMLBUILD_TARGET)
	./$(OCAMLBUILD_TARGET)

clean:
	rm -f *~
	ocamlbuild $(OCAMLBUILD_FLAGS) -clean
