# Generate biblatex section from BibEntry list
# Optionally add citation numbers from Google Scholar
# To allow citation numbers to be added, we need to
# generate a new bib file on the fly
# If rewrite = FALSE, only create section and let user
# add bibliography file in the yaml

add_bib_section <- function(x, rjhcites = NULL,
  sorting = "ynt", show_cites = FALSE, rewrite = TRUE, ...) {
  # Sort references as required
  x <- sort(x, sorting=sorting)
  # Rewrite bib file with relevant references
  if(rewrite) {
    # Generate random keys in order to avoid clashes with any other bib files
    keys <- sort(replicate(length(x), paste0(sample(c(letters, 0:9), 5, replace = TRUE), collapse = "")))
    names(x) <- keys
    # Add cites?
    if (show_cites) {
      if(is.null(rjhcites))
        stop("No citation data provided")
      cites <- x |>
        as.data.frame() |>
        mutate(
          key = keys,
          title = stringr::str_replace_all(title, "[{}]", "")
        ) |>
        fuzzyjoin::stringdist_left_join(
          rjhcites |> select(title, n_citations),
          by = "title", ignore_case = TRUE, distance_col = "dist"
        ) |>
        rename(title = title.x) |>
        # When there are multiple matches, choose the one with largest citations
        # This happens, for example, when a book review has the same title as the book
        # and the book is much more highly cited than its review
        group_by(title) |>
        slice_max(n_citations) |>
        ungroup() |>
        # Put back in original order after grouping operation
        arrange(key) |>
        # Add note column
        mutate(note = paste0("\\emph{[Citations: ", n_citations, "]}.")) |>
        pull(note)
      # Add cites to bib list
      if(length(cites) != length(x))
        stop("Can't find all citations")
      for (i in seq_along(x)) {
        x[[i]]$addendum <- cites[i]
      }
    }
    # Create new bib file with x
    WriteBib(x, file = "temp.bib", append = TRUE)
  } else {
    keys <- names(x)
  }
  # Now add refsection
  cat("\\begin{refsection}")
  cat(paste0("\\nocite{", keys, "}"))
  cat("\\printbibliography[heading=none]")
  cat("\\end{refsection}")
}

# Change all bib references to a new type
# This is to allow non-standard types in bib file
# but need to change them to a standard type
# so RefManageR can handle them

change_bibtype <- function(x, newtype) {
  purrr::map(x, function(u){
    attributes(u)$bibtype <- newtype
    return(u)}) |>
    as.BibEntry()
}
