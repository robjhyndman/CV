library(RefManageR)

rjh <- ReadBib("rjhpubs.bib", check="warn")
for(i in seq_along(rjh))
{
  rjh[[i]]$abstract <- NULL
  rjh[[i]]$keywords <- NULL
}
WriteBib(sort(rjh, sorting="ydnt"), "rjhpubs.bib")
