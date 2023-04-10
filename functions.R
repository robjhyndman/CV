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

# Return dollars in pretty manner.
# Similar to prettyNum but with $ sign and working for numbers greater than 1e7
dollars <- function(x) {
  out <- paste0("\\$", sprintf("%.0f", x))
  paste0(gsub(
    "^0+\\.", ".",
    unname(prettyNum(out, ",", preserve.width = "none", scientific = FALSE))
  ))
}

# Generate biblatex section from vitae tibble
add_bib_section <- function(x) {
  cat("\\begin{refsection}")
  cat(paste0("\\nocite{",x$id,"}"))
  cat("\\printbibliography[heading=none]")
  cat("\\end{refsection}")
}

# Read and save tibble with my packages
get_rjh_packages <- function() {
  # Check if this has been run in last day
  if(fs::file_exists(here::here("packages.rds"))) {
    packages <- readRDS(here::here("packages.rds"))
    info <- fs::file_info(here::here("packages.rds"))
    recent_run <- (Sys.Date() == anytime::anydate(info$modification_time))
  } else
    recent_run <- FALSE
  if (!recent_run) {
    # CRAN packages I've coauthored
    rjh_packages <- try(pkgmeta::get_meta(
      cran_author = "Hyndman",
      include_downloads=TRUE, start="2015-01-01",
      github_repos = read.table("github_r_repos.txt")$V1
    ))
    # Add in any packages not on r-universe
    # This is only necessary until pkgsearch is updated.
    # Then pkgmeta should return a complete set
    missing_packages <- pkgmeta:::get_meta_cran(
      c("bayesforecast", "fds", "fpp", "ftsa", "rainbow", "smoothAPC", "stR"),
      include_downloads = TRUE, start = "2015-01-01"
    )
    rjh_packages <- bind_rows(rjh_packages, missing_packages) |>
      # Sort by package name (case insensitive)
      mutate(lower_case_package = stringr::str_to_lower(package)) |>
      arrange(lower_case_package) |>
      select(-lower_case_package) |>
      distinct()

    # Fix URL of fpp3 package
    rjh_packages <- rjh_packages |>
      mutate(
        url = if_else(url == "https://OTexts.com/fpp3/",
                      "http://pkg.robjhyndman.com/fpp3package/",
                      url)
      )

    if(!("try-error" %in% class(rjh_packages))) {
      # Save new version
      packages <- rjh_packages
      saveRDS(packages, file = here::here("packages.rds"))
    }
  }

  write_packages_bib(packages, file="Rpackages.bib")
  return(packages)
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

