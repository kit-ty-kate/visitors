# -------------------------------------------------------------------------

# This Makefile is not distributed.

SHELL := bash
export CDPATH=

.PHONY: package check export opam pin unpin

# -------------------------------------------------------------------------

include Makefile

# -------------------------------------------------------------------------

# Utilities.

MD5SUM  := $(shell if command -v md5  2>/dev/null ; then echo "md5 -r" ; else echo md5sum ; fi)

# -------------------------------------------------------------------------

# Distribution.

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

PACKAGE  := visitors-$(DATE)
CURRENT  := $(shell pwd)
TARBALL  := $(CURRENT)/$(PACKAGE).tar.gz

# -------------------------------------------------------------------------

# A list of files to copy without changes to the package.
#
# This does not include the src/ and doc/ directories, which require
# special treatment.

DISTRIBUTED_FILES := AUTHORS CHANGES LICENSE Makefile

# -------------------------------------------------------------------------

# Creating a tarball for distribution.

package:
# Make sure the correct version is installed.
	@ make -C src reinstall
# Create a directory to store the distributed files temporarily.
	@ rm -rf $(PACKAGE)
	@ mkdir -p $(PACKAGE)/src
	@ cp $(DISTRIBUTED_FILES) $(PACKAGE)
	@ cp src/*.ml{,i,lib} src/Makefile src/Makefile.preprocess src/META src/_tags $(PACKAGE)/src
# Set the version number into the files that mention it.
# These include version.tex, META.
	@ echo "Setting version to $(DATE)."
	@ echo version = \"$(DATE)\" >> $(PACKAGE)/src/META
# Copy and compile the documentation.
# This requires %.processed.ml files in the test/ directory.
	@ echo "Generating the documentation."
	@ cp -r doc test $(PACKAGE)
	@ make -C $(PACKAGE)/test clean processed
	@ echo '\gdef\visitorsversion{$(DATE)}' > $(PACKAGE)/doc/version.tex
	@ make -C $(PACKAGE)/doc clean all
	@ mv $(PACKAGE)/doc/main.pdf $(PACKAGE)/manual.pdf
	@ rm -rf $(PACKAGE)/doc $(PACKAGE)/test
# Create the tarball.
	@ echo "Creating a tarball."
	tar --exclude=.gitignore -cvz -f $(TARBALL) $(PACKAGE)
	@ echo "The package $(PACKAGE).tar.gz is ready."

# -------------------------------------------------------------------------

# Checking the tarball that was created above.

check:
	@ echo "Checking the package ..."
# Create a temporary directory; extract, build, and install.
	@ TEMPDIR=`mktemp -d /tmp/visitors-test.XXXXXX` && { \
	echo "   * Extracting. " && \
	(cd $$TEMPDIR && tar xfz $(TARBALL)) && \
	echo "   * Compiling and installing." && \
	(cd $$TEMPDIR/$(PACKAGE) && make reinstall \
	) > $$TEMPDIR/install.log 2>&1 \
		|| (cat $$TEMPDIR/install.log; exit 1) && \
	echo "   * Uninstalling." && \
	(cd $$TEMPDIR/$(PACKAGE) && make uninstall \
	) > $$TEMPDIR/uninstall.log 2>&1 \
		|| (cat $$TEMPDIR/uninstall.log; exit 1) && \
	rm -rf $$TEMPDIR ; }
	@ echo "The package $(PACKAGE) seems ready for distribution!"

# -------------------------------------------------------------------------

# Copying the tarball to my Web site.

RSYNC   := scp -p -C
TARGET  := yquem.inria.fr:public_html/visitors/
PAGE    := $(HOME)/dev/page

export:
# Copier l'archive et la doc vers yquem.
	$(RSYNC) $(TARBALL) $(TARGET)
	$(RSYNC) $(PACKAGE)/manual.pdf $(TARGET)

# -------------------------------------------------------------------------

# Updating the opam package.

# This entry assumes that "make package" and "make export" have been
# run on the same day.

OPAM := $(HOME)/dev/opam-repository
CSUM  = $(shell $(MD5SUM) visitors-$(DATE).tar.gz | cut -d ' ' -f 1)

opam:
# Update my local copy of the opam repository.
	@ echo "Updating local opam repository..."
	@ cd $(OPAM) && \
	  git fetch upstream && \
	  git merge upstream/master
# Create a new package, based on the last one.
	@ echo "Creating a new package description visitors-$(DATE)..."
	@ cd $(OPAM)/packages/visitors && \
	  cp -r `ls | grep visitors | tail -1` visitors.$(DATE)
# Update the file "url".
	@ cd $(OPAM)/packages/visitors/visitors.$(DATE) && \
	  rm url && \
	  echo 'archive: "http://gallium.inria.fr/~fpottier/visitors/visitors-$(DATE).tar.gz"' >> url && \
	  echo 'checksum: "$(CSUM)"' >> url
# Copy the file "opam" from Visitors's repository to opam's.
	@ cp -f opam $(OPAM)/packages/visitors/visitors.$(DATE)
# Prepare a commit.
	@ echo "Preparing a new commit..."
	@ cd $(OPAM)/packages/visitors && \
	  git add visitors.$(DATE) && \
	  git status
# Ask for review.
	@ echo "If happy, please run:"
	@ echo "  cd $(OPAM)/packages/visitors && git commit -a && git push && firefox https://github.com/"
	@ echo "and issue a pull request."

# -------------------------------------------------------------------------

# Pinning.

pin:
	opam pin add visitors `pwd` -k git

unpin:
	opam pin remove visitors
