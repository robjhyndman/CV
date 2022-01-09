library(tidyverse)
library(pkgmeta)
library(RefManageR)
source("functions.R")

github <- c(
  "AndriSignorell/DescTools",
  "AU-BURGr/ozdata",
  "earowang/hts",
  "earowang/sugrrants",
  "eddelbuettel/binb",
  "FinYang/tsdl",
  "haghbinh/sfar",
  "jforbes14/eechidna",
  "mitchelloharawild/fasster",
  "mitchelloharawild/vitae",
  "numbats/monash",
  "pridiltal/oddstream",
  "pridiltal/oddwater",
  "pridiltal/stray",
  "robjhyndman/addb",
  "robjhyndman/anomalous",
  "robjhyndman/compenginets",
  "robjhyndman/cricketdata",
  "robjhyndman/demography",
  "robjhyndman/expsmooth",
  "robjhyndman/fma",
  "robjhyndman/forecast",
  "robjhyndman/fpp2-package",
  "robjhyndman/fpp3-package",
  "robjhyndman/hdrcde",
  "robjhyndman/Mcomp",
  "robjhyndman/MEFM-package",
  "robjhyndman/pkgmeta",
  "robjhyndman/thief",
  "robjhyndman/tscompdata",
  "robjhyndman/tsfeatures",
  "ropenscilabs/ozbabynames",
  "ropenscilabs/rcademy",
  "sayani07/gravitas",
  "sevvandi/lookout",
  "thiyangt/seer",
  "tidyverts/fable",
  "tidyverts/fabletools",
  "tidyverts/feasts",
  "tidyverts/tsibble",
  "tidyverts/tsibbledata",
  "verbe039/bfast",
  "ykang/gratis",
  NULL
)

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
    github_repos = github
  )
  # Save result and return it
  saveRDS(rjh_packages, file = here::here("packages.rds"))
}

write_packages_bib(rjh_packages, file="Rpackages.bib")
