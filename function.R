# An Rscript that takes a bib file as input and return a tibble containing
# one row per publication with a column indicating the class: A*, A, B, etc.
# The script should take the refs tibble, and append a column based on the ADBC and ERA lists.


prepare_bib_report <- function(bib_file) {
  fuzzmatch_adbc <- function(x) {
    return(agrep(x, adbc_ranking$Journal.Title, value = TRUE, ignore.case = TRUE))
  }
  fuzzmatch_ERA <- function(x) {
    return(agrep(x, era_ranking$Title, value = TRUE, ignore.case = TRUE))
  }

  best_match <- function(x, y) {
    return(y[which.min(adist(x, y, ignore.case = TRUE))])
  }

  adbc_ranking <- read.csv("abdc2016.csv", header = TRUE)
  era_ranking <- read.csv("era2010.csv", header = TRUE)

  levels(adbc_ranking$ABDC.Rating) <- levels(era_ranking$Rank)

  df <-  ReadBib(bib_file) %>%
    as_tibble() %>%
    mutate(
    	title = stringr::str_remove_all(title, "[{}]"),
      ADBC_ranking = adbc_ranking[match(journal, adbc_ranking$Journal.Title), "ABDC.Rating"],
      ERA_ranking = era_ranking[match(journal, era_ranking$Title), "Rank"],
      Rank = coalesce(ADBC_ranking, ERA_ranking),
      Rank = as.character(fct_explicit_na(Rank, na_level = "None"))
    )

  df1 <- df %>% subset(Rank != "None")

  ### check journal names from adbc_ranking database
  no_matchdf <- df %>%
    subset(Rank == "None") %>%
    mutate(
      journal = fct_explicit_na(journal, na_level = "999999999"),
      journal = as.character(journal)
    )

  out <- sapply(no_matchdf$journal, fuzzmatch_adbc)

  ## select the best match
  i <- lapply(out, function(x) {
    length(x) > 1
  }) %>% unlist(use.names = FALSE)
  out[i] <- sapply(names(out[i]), best_match, y = unlist(out[i], use.names = FALSE))



  indx <- !sapply(out, is_empty)
  no_matchdf <- no_matchdf %>%
    mutate(
      journal = unlist(ifelse(indx, out, journal)),
      ADBC_ranking = adbc_ranking[match(journal, adbc_ranking$Journal.Title), "ABDC.Rating"],
      Rank = ADBC_ranking,
      Rank = as.character(fct_explicit_na(Rank, na_level = "None"))
    )
  df2 <- no_matchdf %>% subset(Rank != "None")

  ### check journal names from ERA_ranking database
  no_match_adbc <- no_matchdf %>% subset(Rank == "None")
  out <- sapply(no_match_adbc$journal, fuzzmatch_ERA)

  ## select the best match
  i <- lapply(out, function(x) {
    length(x) > 1
  }) %>% unlist(use.names = FALSE)
  out[i] <- sapply(names(out[i]), best_match, y = unlist(out[i], use.names = FALSE))

  indx <- !sapply(out, is_empty)
  df3 <- no_match_adbc %>%
    mutate(
      journal = unlist(ifelse(indx, out, journal)),
      ERA_ranking = era_ranking[match(journal, era_ranking$Title), "Rank"],
      Rank = ERA_ranking,
      Rank = as.character(fct_explicit_na(Rank, na_level = "None"))
    )

  bind_rows(df1, df2, df3) %>%
    mutate(
    	journal = ifelse((journal == 999999999), NA, journal),
    	Rank = factor(Rank, levels=c("A*","A","B","C","None"))
    ) %>%
    select(bibtype:year, journal, title, type, ADBC_ranking:Rank, institution, url:pages, doi:school)  %>%
    return()
}
