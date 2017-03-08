# ocamlbuild options.
OCAMLBUILD := ocamlbuild -use-ocamlfind -classic-display

# For consistency with doc/Makefile, which relies on the same set of
# %.processed.ml files, we set this variable to true.
export VISITORS_BUILDING_DOCUMENTATION=true

# The source files.
TESTS := $(shell ls *.ml | grep -v processed)

# ------------------------------------------------------------------------------

.PHONY: test reinstall coverage bench mli processed clean

# Because ocamlbuild is not good at parallel builds, we have broken up the set
# of test sources into several subsets, each of which is described by an
# [.mllib] file. For each such file, we make a separate ocamlbuild invocation,
# and we let [make] run these invocations in parallel. (Each of them must use
# a distinct build directory, and ignore everyone else's build directory.)

test: reinstall
	@ echo Compiling...
	@ make -j4 $(patsubst %.mllib,%.cma,$(shell ls *.mllib))

%.cma:
	@ $(OCAMLBUILD) -build-dir _build_$* $@

# To ensure that we always have the current version of [visitors], we
# reinstall it before compiling.

reinstall:
	@ echo Re-installing...
	@ make -C ../src reinstall >/dev/null 2>/dev/null

# Checking that every .ml file is covered by one .mllib file.

coverage:
	@ for i in $(TESTS) ; do \
	  if ! grep -iw `basename $$i .ml` *.mllib >/dev/null ; then \
	    echo [ERROR] $$i is not covered by any .mllib file. ; \
	  fi \
	done

# ------------------------------------------------------------------------------

# A tiny speed benchmark.

bench: reinstall
	@ $(OCAMLBUILD) -build-dir _build_bench bench.native
	@ echo Running...
	@ _build_bench/bench.native

# Building an executable.

%.native:
	@ $(OCAMLBUILD) -build-dir \
	    _build_$(shell grep -iw $* *.mllib | sed -e 's/\([^.]*\).mllib:.*/\1/') \
	    $@

# ------------------------------------------------------------------------------

# Building inferred .mli files.

mli: reinstall
	make -j4 $(patsubst %.ml,%.inferred.mli,$(TESTS))

# This one is a bit tricky, as we must specify the right build directory.
# We grep to find out which .mllib file covers the desired .mli file,
# then construct the name of the build directory from the name of the
# .mllib file.

%.inferred.mli:
	@ $(OCAMLBUILD) -build-dir \
	    _build_$(shell grep -iw $* *.mllib | sed -e 's/\([^.]*\).mllib:.*/\1/') \
	    $@

# ------------------------------------------------------------------------------

# Preprocessing all source files.

processed: reinstall
	make -j4 -f ../src/Makefile.preprocess \
	  $(patsubst %.ml,%.processed.ml,$(TESTS))

# ------------------------------------------------------------------------------

# Cleaning up.

clean:
	@ rm -f *~
	@ rm -f *.processed.ml
	@ rm -rf _build*

# ------------------------------------------------------------------------------
