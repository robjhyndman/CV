# devtools::install_github("ropensci/RefManageR")
library(RefManageR)

rjh <- ReadBib("rjhpubs.bib", check = "warn")
for (i in seq_along(rjh)) {
  rjh[[i]]$abstract <- NULL
  rjh[[i]]$keywords <- NULL
}
WriteBib(sort(rjh, sorting = "ydnt"), "rjhpubs.bib")
# Beware: Carta2011 authors get screwed up by RefManageR
# Replace with author = {Davide Carta and Laura Villanova and Stefano Costacurta and Alessandro Patelli and Irene Poli and Simone Vezzù and Paolo Scopece and Fabio Lisi and Kate Smith-Miles and Rob J Hyndman and Anita J Hill and Paolo Falcaro},