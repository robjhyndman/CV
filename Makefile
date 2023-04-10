all: compile

compile:
	quarto render RobHyndmanCV.qmd

clean:
	rm -rf RobHyndmanCV.pdf
	rm -rf Rpackages.bib
	rm -rf *.rds
	latexmk -c
