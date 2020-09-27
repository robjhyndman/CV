all: compile

compile:
	Rscript -e 'rmarkdown::render("RobHyndmanCV.Rmd")'

clean:
	rm RobHyndmanCV.pdf
	latexmk -c
