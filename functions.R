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

# Return CRAN packages with Hyndman as author
rjh_cran_packages <- function() {
  pkgsearch::ps("Hyndman", size = 100) %>%
    filter(purrr::map_lgl(
      package_data, ~ grepl("Hyndman", .x$Author, fixed = TRUE)
    )) %>%
    select(package) %>%
    mutate(on_cran = TRUE)
}

get_rjh_packages <- function(github) {
  # Check if this has been run in last day
  recent_run <- fs::file_exists(here::here("packages.rds"))
  if (recent_run) {
    info <- fs::file_info(here::here("packages.rds"))
    recent_run <- (Sys.Date() == anytime::anydate(info$modification_time))
  }
  if (recent_run) {
    return(readRDS(here::here("packages.rds")))
  }

  packages <- tibble(github = github) %>%
    # Extract packages from github repos
    mutate(
      package = stringr::str_extract(github, "/[a-zA-Z0-9\\-]*"),
      package = stringr::str_remove(package, "/"),
      package = stringr::str_extract(package, "[a-zA-Z0-9]*")
    ) %>%
    # Add in CRAN packages
    full_join(rjh_cran_packages(), by = "package") %>%
    replace_na(list(on_cran = FALSE))

  # Compute monthly downloads
  downloads <- packages %>%
    filter(on_cran) %>%
    pull(package) %>%
    cranlogs::cran_downloads(from = "2015-01-01") %>%
    as_tibble() %>%
    mutate(month = tsibble::yearmonth(date)) %>%
    group_by(package) %>%
    summarise(count = sum(count), .groups = "drop")

  # CRAN package meta data
  cran_meta <- packages %>%
    filter(on_cran) %>%
    get_meta_cran()

  # Github package meta data (need to install packages)
  github_meta <- packages %>%
    filter(!on_cran) %>%
    pull(github) %>%
    get_meta_github()

  # Add downloads and titles to packages
  packages <- packages %>%
    left_join(downloads, by = "package") %>%
    left_join(
      bind_rows(cran_meta, github_meta),
      by = "package"
    )

  # Add URLs
  packages <- packages %>%
    mutate(
      github_url = if_else(is.na(github), NA_character_,
        paste0("https://github.com/", github)
      ),
      cran_url = if_else(!on_cran, NA_character_,
        paste0("https://CRAN.R-project.org/package=", package)
      ),
      url = if_else(on_cran, cran_url,
              if_else(!is.na(url), url, github_url)
      )
    )

  # Save result and return it
  saveRDS(packages, file = here::here("packages.rds"))
  return(packages)
}

# Get meta data for vector of packages on CRAN
get_meta_cran <- function(packages) {
  title <- version <- date <- authors <- url <- rep(NA_character_, NROW(packages))
  for (i in seq_along(packages$package)) {
    meta <- pkgsearch::cran_package(packages$package[i])
    date[i] <- meta$date
    title[i] <- meta$Title
    version[i] <- meta$Version
    # Replace new line unicodes with spaces
    authors[i] <- gsub("<U\\+000a>", " ", meta$Author, perl=TRUE)
    # Trim final period
    authors[i] <- gsub("\\.$","",authors[i])
    if (!is.null(meta$URL)) {
      url[i] <- (str_split(meta$URL, ",") %>% unlist())[1]
    }
  }
  tibble(package = packages$package, date = date, url = url, title = title, version = version, authors = authors)
}

# Get meta data for vector of packages on github
get_meta_github <- function(repos) {
  title <- version <- date <- authors <- url <- package <- character(length(repos))
  tmp <- tempfile()
  for (i in seq_along(repos)) {
    date[i] <- gh::gh(paste0("/repos/", repos[i]))$updated_at
    download.file(gh::gh(paste0("/repos/", repos[i], "/contents/DESCRIPTION"))$download_url, tmp)
    package[i] <- desc::desc_get_field("Package", file=tmp)
    title[i] <- desc::desc_get_field("Title", file = tmp)
    version[i] <- as.character(desc::desc_get_version(tmp))
    auth <- desc::desc_get_author("aut", tmp)
    if(!is.null(auth))
      authors[i] <- paste(as.character(auth), sep = "\n", collapse = "\n")
    else
      authors[i] <- desc::desc_get_field("Author",file=tmp)
    url[i] <- desc::desc_get_field("URL", file = tmp,
      default = gh::gh(paste0("/repos/", repos[i]))$html_url
    )
    url[i] <- (str_split(url[i], ",") %>% unlist())[1]
  }
  tibble(package = package, date = date, url = url, title = title, version = version, authors = authors)
}

# Create bib file for R packages
# Uses CRAN version if it exists. Otherwise uses github version
# pkglist is output from get_rjh_packages()

write_packages_bib <- function(pkglist, file) {
  fh <- file(file, open = "w+")
  on.exit(if (isOpen(fh)) close(fh))
  for (i in seq(NROW(pkglist))) {
    bibs <- try(getbibentry(pkglist[i,]))
    if ("try-error" %in% class(bibs)) {
      stop(paste("Package not found:", pkglist[i,]$package))
    } else {
      message("Writing ", pkglist[i,]$package)
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

cran_meta <- function(x) {
  df <- try(versions::available.versions(x), silent = TRUE)
  if ("try-error" %in% class(df)) {
    warning(paste(x, "package not on CRAN"))
    return(NULL)
  }
  tibble(
    package = x,
    maintainer = maintainer(x),
    first_release = tail(df[[1]]$date, 1) %>% as.Date(format = "%Y-%m-%d"),
    last_release = head(df[[1]]$date, 1) %>% as.Date(format = "%Y-%m-%d"),
    current_version = head(df[[1]]$version, 1)
  ) %>%
    mutate(
      maintainer = str_trim(str_extract(maintainer, "[A-Za-z'\ ]*")),
    )
}

cran_downloads <- function(x) {
  # Compute monthly download counts
  down <- cranlogs::cran_downloads(x, from = "2000-01-01") %>%
    as_tibble() %>%
    mutate(month = tsibble::yearmonth(date)) %>%
    group_by(month) %>%
    summarise(count = sum(count), package = x, .groups = "keep")
  # Strip out initial zeros
  first_nonzero <- down %>%
    filter(count > 0) %>%
    head(1)
  if (NROW(first_nonzero) == 0L) {
    return(NULL)
  } else {
    filter(down, month >= first_nonzero$month)
  }
}
