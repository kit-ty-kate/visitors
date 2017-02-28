# ------------------------------------------------------------------------------

# Variables.

PACKAGE := \
  visitors

PLUGIN := \
  ppx_deriving_$(PACKAGE)

RUNTIME := \
  VisitorsRuntime

OCAMLBUILD := \
  ocamlbuild \
  -use-ocamlfind \
  -classic-display \

# The targets that should be built (using ocamlbuild).
# Not sure whether all of the following files are really required.
TARGET := \
  $(patsubst %,$(PLUGIN).%,a cma cmxa cmxs) \
  $(patsubst %,$(RUNTIME).%,a cma cmi cmo cmx cmxa o) \

# The files that should be installed (using ocamlfind).
FILES := \
  META \
  Makefile.preprocess \
  $(patsubst %,_build/%,$(TARGET)) \

# ------------------------------------------------------------------------------

# Rules.

.PHONY: all clean install uninstall reinstall

all:
	$(OCAMLBUILD) $(TARGET)

clean:
	rm -f *~
	$(OCAMLBUILD) -clean

install: all
	ocamlfind install $(PACKAGE) $(FILES)

# [make uninstall] attempts to uninstall, but succeeds even if uninstallation
# fails (probably because the package was not installed in the first place).
uninstall:
	ocamlfind remove $(PACKAGE) || true

reinstall: uninstall
	@ $(MAKE) install
