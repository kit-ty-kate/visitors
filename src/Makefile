# ------------------------------------------------------------------------------

# Variables.

PACKAGE := \
  visitors

PLUGIN := \
  ppx_deriving_$(PACKAGE)

RUNTIME := \
  VisitorsRuntime

# Not sure whether all of the following files are really required.
TARGET := \
  $(patsubst %,$(PLUGIN).%,a cma cmxa cmxs) \
  $(patsubst %,$(RUNTIME).%,a cma cmi cmo cmx cmxa o) \

OCAMLBUILD := \
  ocamlbuild \
  -j 0 \
  -use-ocamlfind \
  -classic-display \
  -cflags "-g" -lflags "-g"

# ------------------------------------------------------------------------------

# Rules.

.PHONY: all clean install uninstall reinstall

all:
	$(OCAMLBUILD) $(TARGET)

clean:
	rm -f *~
	$(OCAMLBUILD) -clean

install: all
	ocamlfind install $(PACKAGE) META $(patsubst %,_build/%,$(TARGET))

uninstall:
	ocamlfind remove $(PACKAGE)

reinstall: uninstall
	@ $(MAKE) install
