# Use TeX Live's pdflatex rather than texres (texres can't use the Fira fonts)
TEXLIVE_BIN ?= /usr/local/texlive/2026/bin/x86_64-linux
export PATH := $(TEXLIVE_BIN):$(PATH)

default: targets

sync:
	uvr sync

targets: sync
	. .uvr/activate && R_ENVIRON_USER=$$HOME/.Renviron Rscript -e "targets::tar_make()"

clean: sync
	. .uvr/activate && Rscript -e "targets::tar_destroy(destroy = 'objects')"
