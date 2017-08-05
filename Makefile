# Usually, only these lines need changing
TEXFILE= RobHyndmanCV
BIBFILES := $(wildcard *.bib)

all: Rpackages.bib $(TEXFILE).pdf

# Generate bib file for all R packages
Rpackages.bib: Rpackages.R
	R CMD BATCH Rpackages.R

# Compile main tex file
$(TEXFILE).pdf: $(TEXFILE).tex $(BIBFILES)
	latexmk -pdf $(TEXFILE)

# View main tex file
view: $(TEXFILE).pdf
	evince $(TEXFILE).pdf &

# Clean up
clean:
	latexmk -c
	rm -f $(TEXFILE).pdf
	rm -f Rpackages.bib

.PHONY: all clean

