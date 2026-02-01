# Copilot Instructions for CV Repository

## Build Commands

- **Build all CV versions**: `make` or `make targets`
  - Runs `targets::tar_make()` to execute the targets pipeline
  - Generates all CV variants (full, 1-page, 2-page, 3-page)
- **Clean build artifacts**: `make clean`
  - Runs `targets::tar_destroy()` to remove all targets

The build system uses the `targets` package to manage dependencies and ensure reproducible builds.

## Architecture Overview

This repository generates multiple CV variants from a single data pipeline:

### Targets Pipeline (_targets.R)

The build process follows this dependency graph:

1. **Data Collection** (always runs fresh):
   - Fetches Google Scholar citations via `gcite` package
   - Reads publication/report bibliographies from `.bib` files
   - Retrieves R package metadata from CRAN/GitHub
   - Loads grant income from CSV

2. **Data Processing**:
   - Functions in `R/` directory transform raw data
   - `R/gcite.R`: Google Scholar citation fetching
   - `R/packages.R`: R package metadata and bibliography generation
   - `R/pretty.R`: Formatting helpers for CV content
   - `R/add_bib_section.R`: Bibliography section utilities

3. **Document Rendering**:
   - `tar_quarto()` renders each `.qmd` file to PDF
   - Each CV variant loads the same targets data but formats differently
   - Main file: `RobHyndmanCV.qmd` (full CV)
   - Variants: `RobHyndman_1page.qmd`, `RobHyndman_2page.qmd`, `RobHyndman_3page.qmd`

### Document Structure

All `.qmd` files follow a similar pattern:
- YAML header defines metadata and formatting (custom `cv-pdf` format)
- Setup chunk loads targets data with `targets::tar_load()`
- Content sections use loaded data (publications, packages, grants, citations)
- Bibliography sections use `RefManageR` to filter and format citations

The custom Quarto extension (`_extensions/`) provides the `cv-pdf` format with specialized LaTeX styling.

## Key Conventions

### Targets Workflow

- **Never manually edit generated files**: `Rpackages.bib` is auto-generated from CRAN/GitHub metadata
- **Data refresh**: The `date` target uses `cue = tar_cue(mode = "always")` to force daily updates
- **Fallback behavior**: If package metadata fetch fails, the pipeline uses the last successful version with a warning
- **Dependency tracking**: Changing any `.bib` file or `github_r_repos.txt` triggers rebuilds automatically

### Bibliography Management

- Publications: `rjhpubs.bib` (manually maintained)
- Reports: `rjhreports.bib` (manually maintained)  
- R Packages: `Rpackages.bib` (auto-generated, do not edit directly)
- Temporary bib files (`temp.bib`) are deleted on each render

### R Package Handling

Author name normalization in `R/packages.R`:
- "Rob Hyndman" → "Rob J Hyndman"
- Special handling for names like "Ben Taieb" → "{Ben~Taieb}" (prevents BibTeX splitting)
- Organizational authors wrapped in braces: `{R Core Team}`, `{Commonwealth of Australia AEC}`
- Comments, emails, and contribution annotations are stripped from DESCRIPTION author fields

### Custom Quarto Format

The `cv-pdf` format (defined in `_extensions/`) provides:
- Specialized LaTeX styling for academic CVs
- Custom spacing and geometry settings
- Header color customization via `\definecolor{headcolor}{HTML}{000088}`

## Environment

- **R environment**: Managed by `renv` (see `renv.lock`)
  - Activate with `renv::restore()` if dependencies are missing
- **Project structure**: Standard R project (`CV.Rproj`)
- **Configuration**: `.Renviron` and `.Rprofile` for local settings
