include ../src/Makefile.preprocess

export TEXINPUTS=.:./code:

.PHONY: all clean

all: main.pdf

SOURCES   := $(wildcard *.tex) $(wildcard *.bib) $(wildcard *.sty)
ML        := $(wildcard code/*.ml)
PROCESSED := $(patsubst %.ml,%.processed.ml,$(ML))

%.pdf: %.tex $(SOURCES) $(ML) $(PROCESSED)
	pdflatex $*
	pdflatex $*

clean:
	rm -f *.log *.aux *.bbl *.blg *.out *.toc *~ main.pdf
