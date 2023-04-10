# Google scholar citations
# Need to load in lots of 100 to avoid connection issues

if(file.exists("gspapers.rds")) {
  gspapers <- readRDS("gspapers.rds")
} else {
  # Grab all citation info, 100 per call
  gspapers <- list()
  complete <- FALSE
  while(!complete) {
    k <- length(gspapers)
    gspapers[[k+1]] <- gcite_url(
      url = "https://scholar.google.com.au/citations?user=vamErfkAAAAJ&hl=en",
      cstart = k*100,
      pagesize = 100
    ) |>
    gcite_papers()
    if(NROW(gspapers[[k+1]]) < 100)
      complete <- TRUE
  }
  gspapers <- bind_rows(gspapers) |>
    as_tibble()
  saveRDS(gspapers, "gspapers.rds")
}

rjh <- gcite_citation_index(
  gcite_url("https://scholar.google.com.au/citations?user=vamErfkAAAAJ&hl=en")
)
