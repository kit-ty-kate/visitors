export VISITORS_BUILDING_DOCUMENTATION=true

include ../src/Makefile.preprocess

export TEXINPUTS=.:../test:

.PHONY: all loop clean

all: main.pdf

SOURCES   := $(wildcard *.tex) $(wildcard *.bib) $(wildcard *.sty)
ML        := $(shell ls ../test/*.ml | grep -v processed)
PROCESSED := $(patsubst %.ml,%.processed.ml,$(ML))

%.pdf: %.tex $(SOURCES) $(ML) $(PROCESSED)
	pdflatex $*
	bibtex $*
	pdflatex $*
	pdflatex $*

loop: $(SOURCES) $(ML) $(PROCESSED)
	latexmk -pdf -pvc main

clean:
	rm -f *.log *.aux *.bbl *.blg *.out *.toc *~ main.pdf
