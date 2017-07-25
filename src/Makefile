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
  -plugin-tag 'package(cppo_ocamlbuild)' \

# Detect whether ocamlopt is available.
NATIVE := $(shell if env ocamlopt >/dev/null 2>/dev/null ; then \
                  echo yes ; else echo no ; fi)

# The targets that should be built (using ocamlbuild).
# Not sure whether all of the following files are really required.
ifeq ($(NATIVE),yes)
  MSG := "Compiling for byte code and native code."
  TARGETS := \
    $(patsubst %,$(PLUGIN).%,cma a cmxa cmxs) \
    $(patsubst %,$(RUNTIME).%,cmi cmo cma a cmx cmxa o)
else
  MSG := "Compiling for byte code only."
  TARGETS := \
    $(patsubst %,$(PLUGIN).%,cma) \
    $(patsubst %,$(RUNTIME).%,cmi cmo cma)
endif

# The files that should be installed (using ocamlfind).
FILES := \
  META \
  Makefile.preprocess \
  $(patsubst %,_build/%,$(TARGETS)) \

# ------------------------------------------------------------------------------

# Rules.

.PHONY: all clean install uninstall reinstall

all:
	@ echo $(MSG)
	$(OCAMLBUILD) $(TARGETS)

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
