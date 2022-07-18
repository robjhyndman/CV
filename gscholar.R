# Google scholar citations
# Need to load in lots of 100 to avoid connection issues

if(file.exists("gspapers.rds")) {
  gspapers <- readRDS("gspapers.rds")
} else {
  # Grab all citation info, 100 per call
  gspapers1 <- gcite_url(
    url = "https://scholar.google.com.au/citations?user=vamErfkAAAAJ&hl=en",
    cstart = 0, pagesize = 100
  ) %>%
    gcite_papers()
  gspapers2 <- gcite_url(
    url = "https://scholar.google.com.au/citations?user=vamErfkAAAAJ&hl=en",
    cstart = 100, pagesize = 100
  ) %>%
    gcite_papers()
  gspapers3 <- gcite_url(
    url = "https://scholar.google.com.au/citations?user=vamErfkAAAAJ&hl=en",
    cstart = 200, pagesize = 100
  ) %>%
    gcite_papers()
  gspapers <- bind_rows(gspapers1, gspapers2, gspapers3) %>%
    as_tibble()
  saveRDS(gspapers, "gspapers.rds")
}

rjh <- gcite_citation_index(
  gcite_url("https://scholar.google.com.au/citations?user=vamErfkAAAAJ&hl=en")
)
