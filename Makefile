all: compile

compile:
	Rscript -e 'rmarkdown::render("RobHyndmanCV.Rmd")'

clean:
	rm -rf RobHyndmanCV.pdf
	rm -rf Rpackages.bib
	latexmk -c
