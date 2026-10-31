# Use TeX Live's pdflatex rather than texres (texres can't use the Fira fonts)
TEXLIVE_BIN ?= /usr/local/texlive/2026/bin/x86_64-linux
export PATH := $(TEXLIVE_BIN):$(PATH)

# uvr (a shell command) doesn't read ~/.Renviron, so take GITHUB_PAT from there
# to avoid GitHub API rate limits. An existing environment value takes precedence.
export GITHUB_PAT ?= $(shell sed -n 's/^GITHUB_PAT=//p' $(HOME)/.Renviron | tr -d "\"'")

# Everything that, if changed, requires tar_make() to do real work.
INPUTS := _targets.R _quarto.yml $(wildcard R/*.R) $(wildcard *.qmd) \
          $(wildcard _extensions/cv/*) rjhpubs.bib rjhreports.bib \
          github_r_repos.txt Grant_income.csv
PDFS   := RobHyndmanCV.pdf RobHyndman_1page.pdf RobHyndman_2page.pdf RobHyndman_3page.pdf

# The date target (Google Scholar citations, package downloads) refreshes daily,
# so the stamp is per-day: the first make each day always runs tar_make().
STAMPDIR := .make
STAMP    := $(STAMPDIR)/targets-$(shell date +%F)

default: targets

# Rebuild only if the day, an input or an output PDF has changed.
# (A missing PDF counts as out of date via the empty rule below.)
targets: $(STAMP)

$(STAMP): $(INPUTS) $(PDFS) $(STAMPDIR)/synced
	. .uvr/activate && R_ENVIRON_USER=$$HOME/.Renviron Rscript -e "targets::tar_make()"
	@rm -f $(STAMPDIR)/targets-*
	@touch $@

$(PDFS): ;

# Only run uvr sync when the dependencies have changed (or: make sync)
$(STAMPDIR)/synced: uvr.toml uvr.lock | $(STAMPDIR)
	uvr sync
	@touch $@

$(STAMPDIR):
	@mkdir -p $@

sync:
	rm -rf .uvr/library/pkgmeta
	uvr update && uvr sync
	@mkdir -p $(STAMPDIR) && touch $(STAMPDIR)/synced

clean: 
	. .uvr/activate && Rscript -e "targets::tar_destroy(destroy = 'objects')"
	@rm -f $(STAMPDIR)/targets-*
	@rm -rf $(HOME)/.cache/R/pkgmeta

.PHONY: default targets sync clean
