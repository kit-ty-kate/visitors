# ------------------------------------------------------------------------------

# Variables.

PACKAGE := \
  visitors

PLUGIN := \
  ppx_deriving_$(PACKAGE)

OCAMLBUILD_TARGET := \
  $(PLUGIN).cma $(PLUGIN).cmxa $(PLUGIN).cmxs

OCAMLBUILD_FLAGS := \
  -use-ocamlfind \
  -classic-display \
  -cflags "-g" -lflags "-g"

# ------------------------------------------------------------------------------

# Rules.

.PHONY: all clean install uninstall

all: $(PLUGIN).mllib $(PLUGIN).mldylib
	ocamlbuild $(OCAMLBUILD_FLAGS) $(OCAMLBUILD_TARGET)

clean:
	rm -f $(PLUGIN).mllib $(PLUGIN).mldylib
	rm -f *~
	ocamlbuild $(OCAMLBUILD_FLAGS) -clean

install: all
	ocamlfind install $(PACKAGE) META $(patsubst %,_build/%,$(OCAMLBUILD_TARGET))

uninstall:
	ocamlfind remove $(PACKAGE)

$(PLUGIN).mllib: manifest
	cp -f $< $@

$(PLUGIN).mldylib: manifest
	cp -f $< $@
