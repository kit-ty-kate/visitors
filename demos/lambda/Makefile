TARGET := \
  lambda.native

OCAMLBUILD := \
  ocamlbuild \
  -use-ocamlfind \
  -classic-display \
  -cflags "-g" -lflags "-g" \

SRC := \
  ../../src

.PHONY: all clean

all:
	@ make -C $(SRC) uninstall || true
	@ make -C $(SRC) install
	@ $(OCAMLBUILD) $(TARGET)
	@ ./$(TARGET)

clean:
	@ rm -f *~
	@ $(OCAMLBUILD) -clean
