# Usually, only these lines need changing
TEXFILE= RobHyndmanCV
BIBFILES := $(wildcard *.bib)

all: $(TEXFILE).pdf

# Compile main tex file
$(TEXFILE).pdf: $(TEXFILE).tex $(BIBFILES)
	latexmk -pdf $(TEXFILE)
	cp $(TEXFILE).pdf ~/git/Website/static/

# View main tex file
view: $(TEXFILE).pdf
	evince $(TEXFILE).pdf &

# Clean up stray files
clean:
	latexmk -c
	rm -rf $(TEXFILE).pdf

.PHONY: all clean

