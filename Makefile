SOURCES := $(wildcard *.qmd)
TARGETS=$(SOURCES:%.qmd=%.pdf)

%.pdf: %.qmd
	@echo "$< -> $@"
	quarto render '$<'

default: $(TARGETS)

clean:
	rm -f *.pdf
	rm -f Rpackages.bib
	rm -f *.rds
	latexmk -c
