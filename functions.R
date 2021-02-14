# Function to produce very basic table, no lines or headings
baretable <- function(tbl, digits = 0,
                      include.colnames = FALSE, include.rownames = FALSE,
                      hline.after = NULL,
                      size = getOption("xtable.size", NULL),
                      add.to.row = getOption("xtable.add.to.row", NULL),
                      longtable = FALSE,
                      ...) {
  tbl %>%
    xtable::xtable(digits = digits, ...) %>%
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
  if(recent_run) {
    info <- fs::file_info(here::here("packages.rds"))
    recent_run <- (Sys.Date() == anytime::anydate(info$modification_time))
  }
  if(recent_run)
    return(readRDS(here::here("packages.rds")))

  packages <- tibble(github = github) %>%
    # Extract packages from github repos
    mutate(
      package = stringr::str_extract(github,"/[a-zA-Z0-9\\-]*"),
      package = stringr::str_remove(package,"/"),
      package = stringr::str_extract(package,"[a-zA-Z0-9]*")
    ) %>%
    # Add in CRAN packages
    full_join(rjh_cran_packages(), by="package") %>%
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
    get_meta(cran=TRUE)

  # Github package meta data (need to install packages)
  packages %>%
    filter(!on_cran) %>%
    pull(github) %>%
    remotes::install_github()
  github_meta <- packages %>%
    filter(!on_cran) %>%
    get_meta(cran=FALSE)

  # Add downloads and titles to packages
  packages <- packages %>%
    left_join(downloads, by="package") %>%
    left_join(
      bind_rows(cran_meta, github_meta),
      by="package"
    )

  # Add URLs
  packages <- packages %>%
    mutate(
      github_url = if_else(is.na(github), NA_character_,
                           paste0("https://github.com/",github)),
      cran_url = if_else(!on_cran, NA_character_,
                         paste0("https://cloud.r-project.org/package=",package)),
      url = if_else(!is.na(url), url,
                    if_else(!is.na(github), github_url, cran_url))
    )

  # Save result and return it
  saveRDS(packages, file=here::here("packages.rds"))
  return(packages)
}

# Get meta data for vector of package names
get_meta <- function(packages, cran=TRUE) {
  get_meta_fn <- ifelse(cran, pkgsearch::cran_package, packageDescription)
  #description <-
  title <- url <- rep(NA_character_, NROW(packages))
  for(i in seq_along(packages$package)) {
    meta <- get_meta_fn(packages$package[i])
    title[i] <- meta$Title
    #description[i] <- meta$Description
    if(!is.null(meta$URL))
      url[i] <- (str_split(meta$URL,",") %>% unlist())[1]
  }
  tibble(package=packages$package, url=url, title=title)
}

# Create bib file for R packages
# Uses CRAN version if it exists. Otherwise uses github version

write_packages_bib <- function(pkglist, file) {
  fh <- file(file, open = "w+")
  on.exit(if (isOpen(fh)) close(fh))
  for (i in seq_along(pkglist)) {
    bibs <- try(getbibentry(pkglist[i]))
    if ("try-error" %in% class(bibs)) {
      stop(paste("Package not found:", pkglist[i]))
    } else {
      message("Writing ", pkglist[i])
      writeLines(toBibtex(bibs), fh)
    }
  }
  message(paste("OK\nResults written to", file))
}

# Create bib entry for package pkg (character string).

getbibentry <- function(pkg) {
  # Grab locally stored package info
  meta <- suppressWarnings(packageDescription(pkg))
  if (!is.list(meta)) {
    stop("No package info found")
  }

  # Check if CRAN version exists
  cran <- pkgsearch::ps(pkg) %>% filter(package == pkg)
  found <- ifelse(NROW(cran) > 0, cran$package == pkg, FALSE)

  # Grab CRAN info if the package is on CRAN
  if (found) {
    meta$Version <- cran$version
    meta$Year <- lubridate::year(cran$date)
    meta$URL <- paste("https://CRAN.R-project.org/package=",
      pkg,
      sep = ""
    )
  } else {
    # Grab github info
    if (is.null(meta$URL)) {
      meta$URL <- paste("https://github.com/", meta$RemoteUsername,
        "/", meta$RemoteRepo,
        sep = ""
      )
    }
    # Find last github commit
    commits <- gh::gh(paste("GET /repos/", meta$RemoteUsername, "/", meta$RemoteRepo, "/commits", sep = ""))
    meta$Year <- substr(commits[1][[1]]$commit$author$date, 1, 4)
  }

  # Fix any & in title
  meta$Title <- gsub("&", "\\\\&", meta$Title)
  # Keep title case
  meta$Title <- paste0("{", meta$Title, "}")
  # Add J to my name
  meta$Author <- gsub("Rob Hyndman", "Rob J Hyndman", meta$Author)
  # Fix Souhaib's name
  meta$Author <- gsub("Ben Taieb", "{Ben~Taieb}", meta$Author)
  # Replace R Core Team with {R Core Team}
  meta$Author <- gsub("R Core Team", "{R Core Team}", meta$Author)
  # Replace AEC
  meta$Author <- gsub("Commonwealth of Australia AEC", "{Commonwealth of Australia AEC}", meta$Author)
  # Replace ABS
  meta$Author <- gsub("Australian Bureau of Statistics ABS", "{Australian Bureau of Statistics ABS}", meta$Author)
  # Remove comments in author fields
  meta$Author <- gsub("\\([a-zA-Z0-9\\-\\s,&\\(\\)<>:/\\.]*\\)", " ", meta$Author, perl = TRUE)

    # Turn contributions into note (for demography)
  if (grepl("with contributions", meta$Author)) {
    author_split <- stringr::str_split(meta$Author, "with contributions ")
    meta$Author <- author_split[[1]][1]
    meta$Note <- paste0("with contributions ", author_split[[1]][2])
    meta$Note <- gsub("^\\.|\\.$", "", meta$Note)
    meta$Note <- gsub("(^[a-z])", "\\U\\1", meta$Note, perl = TRUE)
  }
  else {
    meta$Note <- NULL
  }

  # Create bibentry
  bibentry(
    bibtype = "Manual",
    title = paste("{", meta$Package, "}: ", meta$Title, sep = ""),
    year = meta$Year,
    author = meta$Author,
    url = strsplit(meta$URL, ",")[[1]][1],
    version = meta$Version,
    key = paste("R", meta$Package, sep = ""),
    note = meta$Note
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
