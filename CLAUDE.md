# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- **Build all CV versions**: `make` (runs `targets::tar_make()`)
- **Clean build artifacts**: `make clean` (runs `targets::tar_destroy()`)
- **Restore R dependencies**: `Rscript -e "renv::restore()"` if packages are missing

## Architecture

This repo generates multiple CV PDF variants from a single data pipeline using the `targets` package for dependency management.

### Targets Pipeline (`_targets.R`)

1. **Data collection** (always runs fresh via `tar_cue(mode = "always")`):
   - Google Scholar citations via `gcite` package
   - Publication/report bibliographies from `.bib` files (`rjhpubs.bib`, `rjhreports.bib`)
   - R package metadata from CRAN/GitHub (`github_r_repos.txt`)
   - Grant income from `Grant_income.csv`

2. **Document rendering** via `tar_quarto()`:
   - `RobHyndmanCV.qmd` — full CV
   - `RobHyndman_1page.qmd`, `RobHyndman_2page.qmd`, `RobHyndman_3page.qmd` — shortened variants

All `.qmd` files use the custom `cv-pdf` Quarto format (defined in `_extensions/cv/`) and load targets data with `targets::tar_load()`.

### R Helper Functions (`R/`)

- `gcite.R` — fetches Google Scholar citation stats and per-paper citation counts
- `packages.R` — fetches R package metadata from CRAN/GitHub, generates `Rpackages.bib`
- `add_bib_section.R` — renders `refsection` LaTeX blocks from `BibEntry` lists; optionally annotates with citation counts via fuzzy title matching
- `pretty.R` — `baretable()` for LaTeX tables, `dollars()` for currency formatting

### Bibliography Files

- `rjhpubs.bib` — manually maintained publications
- `rjhreports.bib` — manually maintained reports
- `Rpackages.bib` — **auto-generated**, do not edit directly
- `temp.bib` — created during render, deleted at start of each render

### Key Conventions

- Author name normalization in `R/packages.R`: "Rob Hyndman" → "Rob J Hyndman"; names like "Ben Taieb" → `{Ben~Taieb}` to prevent BibTeX splitting; organizational authors wrapped in braces.
- Header color in `.qmd` files is set via `\definecolor{headcolor}{HTML}{000088}` in `header-includes`.
- Changing any `.bib` file or `github_r_repos.txt` automatically triggers rebuilds.
