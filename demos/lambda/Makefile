TARGET := \
  Main.native

OCAMLBUILD := \
  ocamlbuild \
  -use-ocamlfind \
  -classic-display \
  -cflags "-g" -lflags "-g" \

SRC := \
  ../../src

.PHONY: all test processed clean

all:
	@ echo 'Re-installing visitors...'
	@ make -C $(SRC) reinstall >/dev/null 2>/dev/null
	@ echo 'Compiling...'
	@ $(OCAMLBUILD) $(TARGET) TermAlt.inferred.mli

test: all
	@ ./$(TARGET)

include $(SRC)/Makefile.preprocess

processed: Term.processed.ml TermAlt.processed.ml

clean:
	@ rm -f *~
	@ rm -f *.processed.ml
	@ $(OCAMLBUILD) -clean
