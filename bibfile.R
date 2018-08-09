library(RefManageR)

rjh <- ReadBib("rjhpubs.bib", check="warn")
WriteBib(sort(rjh, sorting="ydnt"), "rjhpubs.bib")
