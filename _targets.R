library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("RefManageR", "dplyr", "gcite", "quarto")
)

# Run the R scripts in the R/ folder containing functions:
tar_source()

list(
  # Current date
  tar_target(date, Sys.Date(), cue = tar_cue(mode = "always")),
  # Update Google scholar citations
  tar_target(rjh, get_gcites(date)),
  tar_target(rjhcites, get_scholar_cites(date)),
  # List of R packages I've coauthored
  tar_target(github_repos, "github_r_repos.txt", format = "file"),
  tar_target(rjh_packages, get_rjh_packages(date, github_repos)),
  tar_target(rpackages_bib, write_packages_bib(rjh_packages, file = Rpackages)),
  # Publications bib entries
  tar_target(rjhpubs, "rjhpubs.bib", format = "file"),
  tar_target(pubs, ReadBib(rjhpubs, check = FALSE)),
  # Reports bib entries
  tar_target(rjhreports, "rjhreports.bib", format = "file"),
  tar_target(reports, ReadBib(rjhreports, check = FALSE)),
  # R packages bib entries
  tar_target(Rpackages, "Rpackages.bib", format = "file"),
  tar_target(packages, read_bib(rpackages_bib, Rpackages)),
  # Grant income csv
  tar_target(Grant_income, "Grant_income.csv", format = "file"),
  tar_target(grants, readr::read_csv(Grant_income)),
  # Generate CV
  #tar_quarto(CV_1page, "RobHyndman_1page.qmd"),
  #tar_quarto(CV_2page, "RobHyndman_2page.qmd"),
  #tar_quarto(CV_3page, "RobHyndman_3page.qmd"),
  tar_quarto(CV, "RobHyndmanCV.qmd")
)
