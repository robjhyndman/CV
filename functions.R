# Function to produce very basic table, no lines or headings
baretable <- function(tbl, digits = 0,
                      include.colnames = FALSE, include.rownames = FALSE,
                      hline.after = NULL,
                      size = getOption("xtable.size", NULL),
                      add.to.row = getOption("xtable.add.to.row", NULL),
                      longtable = FALSE,
                      ...) {
  xtable::xtable(tbl, digits = digits, ...) %>%
    print(
      include.colnames = include.colnames,
      include.rownames = include.rownames,
      hline.after = hline.after,
      comment = FALSE,
      tabular.environment = if_else(longtable, "longtable", "tabular"),
      floating = FALSE,
      size = size,
      add.to.row = add.to.row,
      sanitize.text.function = function(x) {
        x
      }
    )
}

# Create bib file for R packages
# Uses CRAN version if it exists. Otherwise uses github version
# packages is output from pkgmeta::get_meta()

write_packages_bib <- function(packages, file) {
  fh <- file(file, open = "w+")
  on.exit(if (isOpen(fh)) close(fh))
  for (i in seq(NROW(packages))) {
    bibs <- try(getbibentry(packages[i,]))
    if ("try-error" %in% class(bibs)) {
      stop(paste("Package not found:", packages[i,]$package))
    } else {
      message("Writing ", packages[i,]$package)
      writeLines(toBibtex(bibs), fh)
    }
  }
  message(paste("OK\nResults written to", file))
}

# Create bib entry for package pkg (one row tibble).
getbibentry <- function(pkg) {
  meta <- as.list(pkg)
  meta$year <- lubridate::year(meta$date)
  # Fix any & in title
  meta$title <- gsub("&", "\\\\&", meta$title)
  # Keep title case
  meta$title <- paste0("{", meta$title, "}")
  # Fix weird characters
  meta$authors <- gsub("<U+000a>"," ",meta$authors)
  # Add J to my name
  meta$authors <- gsub("Rob Hyndman", "Rob J Hyndman", meta$authors)
  # Fix Souhaib's name
  meta$authors <- gsub("Ben Taieb", "{Ben~Taieb}", meta$authors)
  # Replace R Core Team with {R Core Team}
  meta$authors <- gsub("R Core Team", "{R Core Team}", meta$authors)
  # Replace AEC
  meta$authors <- gsub("Commonwealth of Australia AEC", "{Commonwealth of Australia AEC}", meta$authors)
  # Replace ABS
  meta$authors <- gsub("Australian Bureau of Statistics ABS", "{Australian Bureau of Statistics ABS}", meta$authors)
  # Remove comments in author fields
  meta$authors <- gsub("\\([a-zA-Z0-9\\-\\s,&\\(\\)<>:/\\.']*\\)", " ", meta$authors, perl = TRUE)
  # Remove email addresses
  meta$authors <- gsub("<[a-zA-Z@.]*>","", meta$authors, perl=TRUE)
  # Remove contribution classification
  meta$authors <- gsub("\\[[a-zA-Z, ]*\\]","", meta$authors, perl=TRUE)
  # Remove github handles
  meta$authors <- gsub("\\([@a-zA-Z0-9\\-]*\\)","", meta$authors, perl=TRUE)
  # Replace line breaks with "and"
  meta$authors <- gsub("\\n", " and ", meta$authors)
  # Replace commas with "and"
  meta$authors <- gsub(",", " and ", meta$authors)
  # Trim spaces
  meta$authors <- trimws(meta$authors)
  meta$authors <- gsub("  +"," ", meta$authors, perl=TRUE)
  # Remove duplicate ands
  meta$authors <- gsub("and and and ","and ", meta$authors, perl=TRUE)
  meta$authors <- gsub("and and ","and ", meta$authors, perl=TRUE)

  # Remove contributions from author list (for demography)
  if (grepl("with contributions", meta$authors)) {
    author_split <- stringr::str_split(meta$authors, "with contributions ")
    meta$authors <- author_split[[1]][1]
    #meta$note <- paste0("with contributions ", author_split[[1]][2])
    #meta$note <- gsub("^\\.|\\.$", "", meta$note)
    #meta$note <- gsub("(^[a-z])", "\\U\\1", meta$note, perl = TRUE)
  } else {
    meta$note <- NULL
  }

  # Create bibentry
  bibentry(
    bibtype = "Manual",
    title = paste("{", meta$package, "}: ", meta$title, sep = ""),
    year = meta$year,
    author = meta$authors,
    url = strsplit(meta$url, ",")[[1]][1],
    version = meta$version,
    key = paste("R", meta$package, sep = ""),
    note = meta$note
  )
}

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

  df <- ReadBib(bib_file) %>%
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
      Rank = factor(Rank, levels = c("A*", "A", "B", "C", "None"))
    ) %>%
    select(bibtype:year, journal, title, type, ADBC_ranking:Rank, institution, url:pages, doi:school) %>%
    return()
}

# Return dollars in pretty manner.
# Similar to prettyNum but with $ sign and working for numbers greater than 1e7
dollars <- function(x) {
  out <- paste0("\\$", sprintf("%.0f", x))
  paste0(gsub(
    "^0+\\.", ".",
    unname(prettyNum(out, ",", preserve.width = "none", scientific = FALSE))
  ))
}
