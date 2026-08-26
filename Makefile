default: targets

sync:
	uvr sync

targets: sync
	. .uvr/activate && R_ENVIRON_USER=$$HOME/.Renviron Rscript -e "targets::tar_make()"

clean: sync
	. .uvr/activate && Rscript -e "targets::tar_destroy(destroy = 'objects')"
