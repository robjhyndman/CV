#devtools::install_github("ropensci/RefManageR")
library(RefManageR)

rjh <- ReadBib("rjhpubs.bib", check="warn")
for(i in seq_along(rjh))
{
  rjh[[i]]$abstract <- NULL
  rjh[[i]]$keywords <- NULL
}
WriteBib(sort(rjh, sorting="ydnt"), "rjhpubs.bib")
# Beware: Carta2011 authors get screwed up by RefManageR
