# Create bib file for R packages
# Uses CRAN version if it exists.
# Otherwise uses github version

write_packages_bib <- function(pkglist, file)
{
  fh <- file(file, open = "w+")
  on.exit( if( isOpen(fh) ) close(fh) )
  for(i in seq_along(pkglist))
  {
    bibs <- try(getbibentry(pkglist[i]))
    if("try-error" %in% class(bibs))
      stop(paste("Package not found:",pkglist[i]))
    else {
      cat("\n Writing",pkglist[i])
      writeLines(toBibtex(bibs), fh)
    }
  }
  message(paste("OK\nResults written to",file))
}

# Create bib entry for package pkg (character string).

getbibentry <- function(pkg)
{
  # Grab locally stored package info
  meta <- suppressWarnings(packageDescription(pkg))
  if(!is.list(meta))
  {
    if(is.na(meta))
    {
      install.packages(pkg)
      meta <- suppressWarnings(packageDescription(pkg))
    }
  }
  # Check if CRAN version exists
  url <- paste("https://CRAN.R-project.org/web/packages/",
               pkg,"/index.html",sep="")
  z <- suppressWarnings(rvest::html_session(url))
  # Grab CRAN info if the package is on CRAN
  if(z$response$status != 404)
  {
    x <- rvest::html_nodes(z, "td")
    meta$Version <- stringr::str_extract(as.character(x[2]), "([0-9.]+)")
    pub <- which(!is.na(stringr::str_locate(as.character(x), "<td>Published:</td>")[,1]))
    meta$Year <- stringr::str_extract(as.character(x[pub+1]), "([0-9]+)")
    meta$URL <- paste("https://CRAN.R-project.org/package=",
                   pkg,sep="")

  }
  else # Grab github info
  {
    if(is.null(meta$URL))
      meta$URL <- paste("https://github.com/",meta$RemoteUsername,
                        "/",meta$RemoteRepo,sep="")
    # Find last github commit
    commits <- gh::gh(paste("GET /repos/",meta$RemoteUsername,"/",meta$RemoteRepo,"/commits",sep=""))
    meta$Year <- substr(commits[1][[1]]$commit$author$date,1,4)
  }

  # Fix any & in title
  meta$Title <- gsub("&","\\\\&",meta$Title)

  # Add J to my name
  meta$Author <- gsub("Rob Hyndman","Rob J Hyndman",meta$Author)

  # Fix Souhaib's name
  meta$Author <- gsub("Ben Taieb", "Ben~Taieb", meta$Author)

  # Replace R Core Team with {R Core Team}
  meta$Author <- gsub("R Core Team","{R Core Team}",meta$Author)

  # Replace AEC
  meta$Author <- gsub("Commonwealth of Australia AEC","{Commonwealth of Australia AEC}",meta$Author)

  # Turn contributions into note (for demography)
  if(grepl("with contributions", meta$Author)) {
    author_split <- stringr::str_split(meta$Author, "with contributions ")
    meta$Author <- author_split[[1]][1]
    meta$Note <- paste0("with contributions ",author_split[[1]][2])
    meta$Note <- gsub('^\\.|\\.$', '', meta$Note)
    meta$Note <- gsub('(^[a-z])','\\U\\1', meta$Note, perl=TRUE)
  }
  else
    meta$Note <- NULL

  # Create bibentry
  rref <- bibentry(
    bibtype="Manual",
    title=paste(meta$Package,": ",meta$Title, sep=""),
    year=meta$Year,
    author = meta$Author,
    url = strsplit(meta$URL,",")[[1]][1],
    version = meta$Version,
    key = paste("R",meta$Package,sep=""),
    note = meta$Note
  )
  return(rref)
}
