library(tidyverse)
library(pkgmeta)
library(RefManageR)
source("functions.R")

# Check if this has been run in last day
recent_run <- fs::file_exists(here::here("packages.rds"))
if (recent_run) {
  info <- fs::file_info(here::here("packages.rds"))
  recent_run <- (Sys.Date() == anytime::anydate(info$modification_time))
}
if (recent_run) {
  rjh_packages <- readRDS(here::here("packages.rds"))
} else {
  # CRAN packages I've coauthored
  rjh_packages <- pkgmeta::get_meta(
    cran_author = "Hyndman",
    include_downloads=TRUE, start="2015-01-01",
    github_repos = read.table("github_r_repos.txt")$V1
  )
  # Save result and return it
  saveRDS(rjh_packages, file = here::here("packages.rds"))
}

write_packages_bib(rjh_packages, file="Rpackages.bib")
