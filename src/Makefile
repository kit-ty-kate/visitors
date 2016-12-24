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

# The rewriting command.
PPX          := `ocamlfind query ppx_deriving`/ppx_deriving \
                `ocamlfind query visitors`/$(PLUGIN).cma
REWRITE      := ocamlfind ppx_tools/rewriter -ppx '$(PPX)'

# Use GNU sed to extract the generated code.
# This requires GNU sed 3.95 or above, I am told.
SED          := $(shell if command -v gsed >/dev/null ; then echo gsed ; else echo sed ; fi)
EXTRACT      := $(SED) -e '/VISITORS.BEGIN/,/VISITORS.END/!d;//d'

# Use ocp-indent to beautify the generated code.
INDENT       := ocp-indent --config=JaneStreet

%.ml.processed: %.ml
	$(REWRITE) $< | $(EXTRACT) | $(INDENT) > $@
