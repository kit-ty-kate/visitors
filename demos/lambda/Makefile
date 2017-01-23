TARGET := \
  Main.native

OCAMLBUILD := \
  ocamlbuild \
  -use-ocamlfind \
  -classic-display \
  -cflags "-g" -lflags "-g" \

SRC := \
  ../../src

MLI := \
  $(patsubst %.ml,%.inferred.mli,$(wildcard *.ml))

.PHONY: all test mli processed clean

all:
	@ echo 'Re-installing visitors...'
	@ make -C $(SRC) reinstall >/dev/null 2>/dev/null
	@ echo 'Compiling...'
	@ $(OCAMLBUILD) $(TARGET)

test: all
	@ ./$(TARGET)

mli:
	@ $(OCAMLBUILD) $(MLI)

include $(SRC)/Makefile.preprocess

processed: Term.processed.ml

clean:
	@ rm -f *~
	@ rm -f *.processed.ml
	@ $(OCAMLBUILD) -clean
