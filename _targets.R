library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("RefManageR", "dplyr")
)

# Run the R scripts in the R/ folder containing functions:
tar_source()

list(
  # Current date
  tar_target(date, Sys.Date()),
  # Update Google scholar citations
  tar_target(rjh, get_gcites(date)),
  # List of R packages I've coauthored
  tar_target(rjh_packages, get_rjh_packages(date)),
  tar_target(rpackages_bib, write_packages_bib(rjh_packages, file = Rpackages)),
  # Publications bib entries
  tar_target(rjhpubs, "rjhpubs.bib", format = "file"),
  tar_target(pubs, ReadBib(rjhpubs, check = FALSE)),
  # Reports bib entries
  tar_target(rjhreports, "rjhreports.bib", format = "file"),
  tar_target(reports, ReadBib(rjhreports, check = FALSE)),
  # R packages bib entries
  tar_target(Rpackages, "Rpackages.bib", format = "file"),
  tar_target(packages, ReadBib(Rpackages, check = FALSE)),
  # Grant income csv
  tar_target(Grant_income, "Grant_income.csv", format = "file"),
  tar_target(grants, readr::read_csv(Grant_income)),
  tar_quarto(CV, "RobHyndmanCV.qmd")
)
