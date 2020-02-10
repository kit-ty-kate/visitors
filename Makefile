# -------------------------------------------------------------------------

# Require bash.
SHELL := bash
# Prevent the built-in bash cd from displaying information.
export CDPATH=

# The package name.
THIS=visitors

# -------------------------------------------------------------------------

# Compilation and installation rules.

.PHONY: all
all:
	@ dune build @install
# @install is smaller than @all, as it does not include the tests.
# or: dune build -p $(THIS)

.PHONY: install
install: all
	@ dune install

.PHONY: uninstall
uninstall:
	@ dune uninstall

.PHONY: clean
clean::
	@ dune clean

.PHONY: test
test:
	@ dune build --display short @all

.PHONY: bench
bench: test
	@ _build/default/test/bench.exe

.PHONY: versions
versions:
	@ dune build --workspace dune-workspace.versions @all
# Each opam switch listed in the file dune-workspace.versions
# should exist and should have the following packages installed:
# ppx_deriving result hashcons ocp-indent ppx_import

.PHONY: pin
pin:
	opam pin --yes add $(THIS).dev .

.PHONY: unpin
unpin:
	opam pin --yes remove $(THIS)

# -------------------------------------------------------------------------

# Cleaning up.

clean::
	@ find . -name "*~" -exec rm '{}' \;
	@ find . -name "*.processed.ml" -exec rm '{}' \;
	@ for i in doc ; do \
	  $(MAKE) -C $$i $@ ; \
	done

# -------------------------------------------------------------------------

# Creating a release.

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

# A release commit is created off the main branch, on the side, and tagged.
# Indeed, some files need to be changed or removed for a release.

CURRENT:= $(shell git symbolic-ref --short HEAD)
BRANCH := release-branch-$(DATE)

# The documentation files $(DOC) are copied to the directory $(RELEASE).
# They are also copied to the directory $(WWW).

DOC     := doc/manual.pdf
RELEASE := releases/$(DATE)

# Prior to making a release, please make sure that `CHANGES.md` has been
# properly updated. Run [make test] and [make versions]. Test the opam package
# by running [make pin]. (You may wish to run [make pin] in a dedicated
# switch, so as to avoid clobbering your regular installation.)

.PHONY: release
release:
# Check if this is the master branch.
	@ if [ "$(CURRENT)" != "master" ] ; then \
	  echo "Error: this is not the master branch." ; \
	  git branch ; \
	  exit 1 ; \
	fi
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  fi
# Check the current package description.
	@ opam lint
# Create a fresh git branch and switch to it.
	@ echo "Preparing a release commit on a fresh release branch..."
	@ git checkout -b $(BRANCH)
# Hardcode a version number in the files that need it.
	@ sed -i.bak 's/unreleased/$(DATE)/' dune-project
	@ rm -f dune-project.bak
	@ git add dune-project
	@ echo '\gdef\visitorsversion{$(DATE)}' > doc/version.tex
	@ git add doc/version.tex
# Check that compilation and installation appear to work.
	@ echo "Testing the package by pinning it..."
	@ opam pin --yes remove $(THIS)
	@ opam pin --yes add $(THIS).dev .
# Compile the documentation.
# This requires creating the files test/*.processed.ml,
# so the package must be installed.
# We have just installed it above.
	@ echo "Building the documentation..."
	@ make --quiet -C doc clean >/dev/null
	@ make --quiet -C doc all   >/dev/null
	@ git add -f $(DOC)
	@ echo '(include dune.manual)' >> doc/dune
	@ git add doc/dune
# Remove subdirectories that need not (or must not) be distributed.
# The test/ directory could in principle be removed at this point.
# However, it is needed in order to build the manual, and Debian
# says the manual is not free unless it can be rebuilt. So, keep it.
	@ git rm -rf --quiet releases
# Remove files that need not (or must not) be distributed.
	@ git rm --quiet \
	    Makefile dune-workspace.versions \
	    TODO* NOTES
# Uninstall.
	@ echo "Now unpinning the package..."
	@ opam pin --yes remove $(THIS)
# Commit.
	@ echo "Committing..."
	@ git commit -m "Release $(DATE)." --quiet
# Create a git tag.
	@ git tag -a $(DATE) -m "Release $(DATE)."
# Save a copy of the manual.
	@ mkdir -p $(RELEASE)/doc
	@ cp $(DOC) $(RELEASE)/doc
# Switch back to the master branch.
	@ echo "Switching back to the $(CURRENT) branch..."
	@ git checkout $(CURRENT)
# Commit a copy of the manual *in the master branch* in releases/.
	@ echo "Committing a copy of the documentation..."
	@ cd $(RELEASE) && git add -f $(DOC)
	@ git commit -m "Saved documentation for release $(DATE)."
# Done.
	@ echo "Done."
	@ echo "If happy, please type:"
	@ echo "  \"make publish\"   to push this release to gitlab.inria.fr"
	@ echo "  \"make export\"    to upload the manual to yquem.inria.fr"
	@ echo "  \"make opam\"      to create a new opam package"
	@ echo "Otherwise, please type:"
	@ echo "  \"make undo\"      to undo this release"

.PHONY: publish
publish:
# Push the new branch and tag to gitlab.inria.fr.
	@ git push origin $(BRANCH)
	@ git push --tags

.PHONY: undo
undo:
# Delete the new branch and tag.
	@ git branch -D $(BRANCH)
	@ git tag -d $(DATE)
# Delete the new commit on the master branch.
	@ git reset --hard HEAD~1

# -------------------------------------------------------------------------

# Copying the documentation to François' page on yquem.

# This assumes that [make release] has been run on the same day.

RSYNC   := scp -p -C
TARGET  := yquem.inria.fr:public_html/$(THIS)/

.PHONY: export
export:
	$(RSYNC) $(RELEASE)/$(DOC) $(TARGET)

# -------------------------------------------------------------------------

# Publishing a new version of the opam packages.

# This assumes that [make release] has been run on the same day.

# The repository URL (https).
REPO     := https://gitlab.inria.fr/fpottier/$(THIS)

# The archive URL (https).
ARCHIVE  := $(REPO)/repository/$(DATE)/archive.tar.gz

.PHONY: opam
opam:
# Publish an opam description.
	@ opam publish -v $(DATE) $(THIS).opam $(ARCHIVE)
