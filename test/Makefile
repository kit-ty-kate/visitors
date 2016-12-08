TARGET := \
  test.native \
  lambda.native \

OCAMLBUILD_FLAGS := \
  -use-ocamlfind \
  -classic-display \
  -cflags "-g -dsource" -lflags "-g" \

.PHONY: test infer clean

test:
	make -C ../src uninstall || true
	make -C ../src install
	ocamlbuild $(OCAMLBUILD_FLAGS) $(TARGET)
	./$(TARGET)

infer:
	make -C ../src uninstall || true
	make -C ../src install
	ocamlbuild $(OCAMLBUILD_FLAGS) test.inferred.mli
	cat _build/test.inferred.mli

clean:
	rm -f *~
	ocamlbuild $(OCAMLBUILD_FLAGS) -clean
