# Google scholar citations
# Need to load in lots of 100 to avoid connection issues
library(gcite)

 if(fs::file_exists(here::here("gspapers.rds"))) {
    gspapers <- readRDS(here::here("gspapers.rds"))
    info <- fs::file_info(here::here("gspapers.rds"))
    recent_run <- (Sys.Date() == anytime::anydate(info$modification_time))
  } else {
    recent_run <- FALSE
  }

if(!recent_run) {
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
