custom <- RefManageR:::MakeBibLaTeX()
with(
  custom,
  custom$formatArticle <- function(paper) {
    collapse(c(
      fmtPrefix(paper),
      fmtBAuthor(paper),
      fmtJournDate(paper),
      fmtJTitle(paper$title),
      sentenceP(paste0(c(
        paste0(c(
          fmtJournal(paper),
          fmtSeries(paper$series)
        ),
        collapse = ""
        ),
        fmtVolume(paper$volume, paper$number)
      ), collapse = " "),
      fmtBTitle(paper$issuetitle, paper$issuesubtitle),
      fmtEditor(paper, suffix = NULL, prefix = ". "),
      fmtNote(paper$note, prefix = ". ", suffix = NULL),
      pgs = fmtPages(paper$pages, "none"),
      sep = ""
      ),
      fmtEprint(paper),
      fmtAddendum(paper$addendum),
      fmtPubstate(paper$pubstate)
    ))
  }
)
with(
  custom,
  custom$shortName <- function(pers) {
    if (length(pers$family)) {
      res <- cleanupLatex(pers$family)
      if (length(pers$given)) {
        if (.BibOptions$first.inits) {
          paste0(c(substr(vapply(pers$given, cleanupLatex, ""),
            start = 1L, stop = 1L
          ), " ", res), collapse = "")
        } else {
          cleanupLatex(as.character(pers))
        }
      } else {
        res
      }
    } else {
      paste(cleanupLatex(pers$given), collapse = " ")
    }
  }
)
with(
  custom,
  custom$fmtJournal <- function(s) {
    if (length(s$journaltitle)) {
      res <- emph(cleanupLatex(s$journaltitle))
      if (length(s$journalsubtitle)) {
        res <- paste(addPeriod(res), emph(cleanupLatex(s$journalsubtitle)))
      }
      return(res)
    } else if (!is.null(s$journal)) {
      emph(cleanupLatex(s$journal))
    }
  }
)
with(
  custom,
  custom$fmtVolume <- function(vol, num) {
    if (length(vol)) {
      res <- vol
      if (length(num)) {
        res <- paste0(vol, "(", num, ")")
      }
      res
    }
  }
)
tools::bibstyle("custom", custom)
BibOptions(bib.style = "custom", max.names=200)
