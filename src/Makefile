# ------------------------------------------------------------------------------

# Variables.

PACKAGE := \
  visitors

PLUGIN := \
  ppx_deriving_$(PACKAGE)

RUNTIME := \
  VisitorsRuntime

# Not sure whether all of the following files are really required.
OCAMLBUILD_TARGET := \
  $(patsubst %,$(PLUGIN).%,a cma cmxa cmxs) \
  $(patsubst %,$(RUNTIME).%,a cma cmi cmo cmx cmxa o) \

OCAMLBUILD_FLAGS := \
  -use-ocamlfind \
  -classic-display \
  -cflags "-g" -lflags "-g"

# ------------------------------------------------------------------------------

# Rules.

.PHONY: all clean install uninstall

all:
	ocamlbuild $(OCAMLBUILD_FLAGS) $(OCAMLBUILD_TARGET)

clean:
	rm -f *~
	ocamlbuild $(OCAMLBUILD_FLAGS) -clean

install: all
	ocamlfind install $(PACKAGE) META $(patsubst %,_build/%,$(OCAMLBUILD_TARGET))

uninstall:
	ocamlfind remove $(PACKAGE)

# ------------------------------------------------------------------------------

# Testing the preprocessor. (This assumes the package has been installed.)

PPX_DERIVING := $(shell ocamlfind query ppx_deriving)
PPX          := $(PPX_DERIVING)/ppx_deriving _build/$(PLUGIN).cma
REWRITE      := ocamlfind ppx_tools/rewriter -ppx '$(PPX)'

%.ml.processed: %.ml
	$(REWRITE) $< > $@
	ocp-indent --inplace $@
