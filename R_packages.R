library(tidyverse)
library(RefManageR)
source("functions.R")

# Github packages I've coauthored
github <- c(
  "AU-BURGr/ozdata",
  "earowang/hts",
  "earowang/sugrrants",
  "eddelbuettel/binb",
  "FinYang/tsdl",
  "jforbes14/eechidna",
  "mitchelloharawild/fasster",
  "mitchelloharawild/vitae",
  "pridiltal/oddstream",
  "pridiltal/oddwater",
  "pridiltal/stray",
  "robjhyndman/addb",
  "robjhyndman/anomalous",
  "robjhyndman/compenginets",
  "robjhyndman/demography",
  "robjhyndman/expsmooth",
  "robjhyndman/fma",
  "robjhyndman/forecast",
  "robjhyndman/fpp",
  "robjhyndman/fpp2-package",
  "robjhyndman/fpp3-package",
  "robjhyndman/hdrcde",
  "robjhyndman/Mcomp",
  "robjhyndman/MEFM-package",
  "robjhyndman/MonashEBSTemplates",
  "robjhyndman/thief",
  "robjhyndman/tscompdata",
  "robjhyndman/tsfeatures",
  "ropenscilabs/cricketdata",
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
  "ykang/gratis"
)
rjhpackages <- get_rjh_packages(github)

# Check if this has been run today
# Inefficient as it gets the emta data all over again. Need
# to update get_rjh_packages to include the required meta data and
# pass whole object to write_bib function
recent_run <- fs::file_exists("Rpackages.bib")
if(recent_run) {
  if(Sys.Date() > anytime::anydate(fs::file_info("Rpackages.bib")$modification_time))
    recent_run <- FALSE
}
if(!recent_run)
  write_packages_bib(packages$package, file="Rpackages.bib")
