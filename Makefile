default: targets

targets:
	Rscript -e "targets::tar_make()"

clean:
	Rscript -e "targets::tar_destroy()"
