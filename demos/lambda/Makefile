TARGET := \
  Main.native

OCAMLBUILD := \
  ocamlbuild \
  -use-ocamlfind \
  -classic-display \
  -cflags "-g" -lflags "-g" \

SRC := \
  ../../src

.PHONY: all clean test

all:
	@ echo 'Re-installing visitors...'
	@ make -C $(SRC) reinstall >/dev/null 2>/dev/null
	@ echo 'Compiling...'
	@ $(OCAMLBUILD) $(TARGET)

test: all
	@ ./$(TARGET)

clean:
	@ rm -f *~
	@ $(OCAMLBUILD) -clean
