# Google scholar stats
get_gcites <- function(date) {
  "https://scholar.google.com.au/citations?user=vamErfkAAAAJ&hl=en" |>
    gcite::gcite_url() |>
    gcite::gcite_citation_index()
}

# Get Google scholar citation data
get_scholar_cites <- function(date) {
  # Need to load in lots of 100 to avoid connection issues
  gspapers <- list()
  complete <- FALSE
  while (!complete) {
    k <- length(gspapers)
    gspapers[[k + 1]] <- gcite::gcite_url(
      url = "https://scholar.google.com.au/citations?user=vamErfkAAAAJ&hl=en",
      cstart = k * 100,
      pagesize = 100
    ) |>
      gcite::gcite_papers() |>
      suppressWarnings()
    if (NROW(gspapers[[k + 1]]) < 100) {
      complete <- TRUE
    }
  }
  dplyr::bind_rows(gspapers) |>
    tibble::as_tibble()
}
