# Google scholar stats
get_gcites <- function(date) {
  "https://scholar.google.com.au/citations?user=vamErfkAAAAJ&hl=en" |>
    gcite::gcite_url() |>
    gcite::gcite_citation_index()
}

# Function to produce very basic table, no lines or headings
baretable <- function(tbl, digits = 0,
                      include.colnames = FALSE, include.rownames = FALSE,
                      hline.after = NULL,
                      size = getOption("xtable.size", NULL),
                      add.to.row = getOption("xtable.add.to.row", NULL),
                      longtable = FALSE,
                      ...) {
  xtable::xtable(tbl, digits = digits, ...) |>
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

# Return dollars in pretty manner.
# Similar to prettyNum but with $ sign and working for numbers greater than 1e7
dollars <- function(x) {
  out <- paste0("\\$", sprintf("%.0f", x))
  paste0(gsub(
    "^0+\\.", ".",
    unname(prettyNum(out, ",", preserve.width = "none", scientific = FALSE))
  ))
}

# Read and save tibble with my packages
get_rjh_packages <- function(date) {
  # CRAN packages I've coauthored
  rjh_packages <- try(pkgmeta::get_meta(
      cran_author = "Hyndman",
      include_downloads = TRUE, start = "2015-01-01",
      github_repos = read.table("github_r_repos.txt")$V1
    )) |>
    # Sort by package name (case insensitive)
    mutate(lower_case_package = stringr::str_to_lower(package)) |>
    arrange(lower_case_package) |>
    select(-lower_case_package)

  # Fix URL of fpp3 package
  rjh_packages <- rjh_packages |>
    mutate(
      url = if_else(url == "https://OTexts.com/fpp3/",
            "http://pkg.robjhyndman.com/fpp3package/",
            url)
    )
  # Return tibble of package info
  return(rjh_packages)
}

# Create bib file for R packages
# Uses CRAN version if it exists. Otherwise uses github version
# packages is output from pkgmeta::get_meta()

write_packages_bib <- function(packages, file) {
  fh <- file(file, open = "w+")
  on.exit(if (isOpen(fh)) close(fh))
  for (i in seq(NROW(packages))) {
    bibs <- try(getbibentry(packages[i, ]))
    if ("try-error" %in% class(bibs)) {
      stop(paste("Package not found:", packages[i, ]$package))
    } else {
      message("Writing ", packages[i, ]$package)
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
  meta$authors <- gsub("<U+000a>", " ", meta$authors)
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
  meta$authors <- gsub("<[a-zA-Z@.]*>", "", meta$authors, perl = TRUE)
  # Remove contribution classification
  meta$authors <- gsub("\\[[a-zA-Z, ]*\\]", "", meta$authors, perl = TRUE)
  # Remove github handles
  meta$authors <- gsub("\\([@a-zA-Z0-9\\-]*\\)", "", meta$authors, perl = TRUE)
  # Replace line breaks with "and"
  meta$authors <- gsub("\\n", " and ", meta$authors)
  # Replace commas with "and"
  meta$authors <- gsub(",", " and ", meta$authors)
  # Trim spaces
  meta$authors <- trimws(meta$authors)
  meta$authors <- gsub("  +", " ", meta$authors, perl = TRUE)
  # Remove duplicate ands
  meta$authors <- gsub("and and and ", "and ", meta$authors, perl = TRUE)
  meta$authors <- gsub("and and ", "and ", meta$authors, perl = TRUE)

  # Remove contributions from author list (for demography)
  if (grepl("with contributions", meta$authors)) {
    author_split <- stringr::str_split(meta$authors, "with contributions ")
    meta$authors <- author_split[[1]][1]
    # meta$note <- paste0("with contributions ", author_split[[1]][2])
    # meta$note <- gsub("^\\.|\\.$", "", meta$note)
    # meta$note <- gsub("(^[a-z])", "\\U\\1", meta$note, perl = TRUE)
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

# Generate biblatex section from BibEntry list
# Optionally add citation numbers from Google Scholar
# To allow citation numbers to be added, we need to
# generate a new bib file on the fly
# If rewrite = FALSE, only create section and let user
# add bibliography file in the yaml

add_bib_section <- function(x, sorting = "ynt", show_cites = FALSE, rewrite = TRUE, ...) {
  # Sort references as required
  x <- sort(x, sorting=sorting)
  # Rewrite bib file with relevant references
  if(rewrite) {
    # Generate random keys in order to avoid clashes with any other bib files
    keys <- sort(replicate(length(x), paste0(sample(c(letters, 0:9), 5, replace = TRUE), collapse = "")))
    names(x) <- keys
    # Add cites?
    if (show_cites) {
      cites <- x |>
        as.data.frame() |>
        mutate(
          key = keys,
          title = stringr::str_replace_all(title, "[{}]", "")
        ) |>
        fuzzyjoin::stringdist_left_join(
          get_scholar_cites() |> select(title, n_citations),
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

# Get Google scholar citations for RJH
get_scholar_cites <- function() {
  # Check if this has been run in the last day
  if (fs::file_exists(here::here("gspapers.rds"))) {
    gspapers <- readRDS(here::here("gspapers.rds"))
    info <- fs::file_info(here::here("gspapers.rds"))
    recent_run <- (Sys.Date() == anytime::anydate(info$modification_time))
  } else {
    recent_run <- FALSE
  }
  # If not run recently, grab all citation info from G Scholar
  if (!recent_run) {
    # Need to load in lots of 100 to avoid connection issues
    gspapers <- list()
    complete <- FALSE
    while (!complete) {
      k <- length(gspapers)
      gspapers[[k + 1]] <- gcite_url(
        url = "https://scholar.google.com.au/citations?user=vamErfkAAAAJ&hl=en",
        cstart = k * 100,
        pagesize = 100
      ) |>
        gcite_papers()
      if (NROW(gspapers[[k + 1]]) < 100) {
        complete <- TRUE
      }
    }
    gspapers <- bind_rows(gspapers) |>
      as_tibble()
    # Save for next time
    saveRDS(gspapers, "gspapers.rds")
  }
  return(gspapers)
}

# Remove any temporary bib file
if (fs::file_exists("temp.bib")) {
  fs::file_delete("temp.bib")
}
