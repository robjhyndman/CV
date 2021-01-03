library(tidyverse)
library(RefManageR)

# Check if this has been run in last hour
recent_run <- fs::file_exists("Rpackages.bib")
if(recent_run) {
  if(Sys.time() > fs::file_info("Rpackages.bib")$modification_time + 3600)
    recent_run <- FALSE
}

if(!recent_run)
{
  # Find installed or CRAN packages with Hyndman as an author
  rjhpkgs <- c(find_rjh_packages(),
      # Now add github-only packages
      "addb",
      "anomalous",
      "compenginets",
      "cricketdata",
      "fasster",
      "MEFM",
      "MonashEBSTemplates",
      "oddwater",
      "ozbabynames",
      "rcademy",
      "tscompdata",
      "tsdl"
    ) %>%
    unique() %>%
    sort()

  downloads <- map_dfr(rjhpkgs, cran_downloads) %>%
    mutate(month = tsibble::yearmonth(month))
  since2015 <- downloads %>%
    filter(month >= tsibble::yearmonth("2015 Jan"))
  # Write bib file
  write_packages_bib(rjhpkgs, file="Rpackages.bib")
  # Save rds files
  saveRDS(rjhpkgs, "rjhpkgs.rds")
  saveRDS(since2015, "since2015.rds")
} else {
  # Read rds files
  rjhpkgs <- readRDS("rjhpkgs.rds")
  since2015 <- readRDS("since2015.rds")
  message("Run recently")
}
