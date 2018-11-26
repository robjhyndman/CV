# Function to produce very basic table, no lines or headings

baretable <- function(tbl, digits = 0,
                      include.colnames=FALSE, include.rownames=FALSE,
                      hline.after=NULL,
                      size = getOption("xtable.size", NULL),
                      add.to.row =  getOption("xtable.add.to.row", NULL),
                      ...) {
  tbl %>%
    xtable::xtable(digits = digits, ...) %>%
    print(
      include.colnames = include.colnames,
      include.rownames = include.rownames,
      hline.after = hline.after,
      comment = FALSE,
      latex.environments = NULL,
      floating = FALSE,
      size=size,
      add.to.row=add.to.row,
      sanitize.text.function = function(x) {
        x
      }
    )
}

