getbibentry <- function(pkg)
{
  # Grab locally stored package info
  meta <- packageDescription(pkg)
  # Check if CRAN version exists
  url <- paste("https://CRAN.R-project.org/web/packages/",
               pkg,"/index.html",sep="")
  z <- suppressWarnings(rvest::html_session(url))
  # Grab CRAN info if the package is on CRAN
  if(z$response$status != 404)
  {
    x <- rvest::html_nodes(z, "td")
    meta$Version <- stringr::str_extract(as.character(x[2]), "([0-9.]+)")
    pub <- which(!is.na(stringr::str_locate(x, "<td>Published:</td>")[,1]))
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

  # Create bibentry
  rref <- bibentry(
    bibtype="Manual",
    title=paste(meta$Package,": ",meta$Title, sep=""),
    year=meta$Year,
    author = meta$Author,
    url = strsplit(meta$URL,",")[[1]][1],
    version = meta$Version,
    key = paste("R",meta$Package,sep="")
  )
  return(rref)
}

write.bib <- function(pkglist)
{
  fh <- file("Rpackages.bib", open = "w+")
  on.exit( if( isOpen(fh) ) close(fh) )
  for(i in seq_along(pkglist))
  {
    bibs <- getbibentry(pkglist[i])
    writeLines(toBibtex(bibs), fh)
  }
  message("OK\nResults written to Rpackages.bib")
}

write.bib(
  c("addb",
    "anomalous",
    "bfast",
    "cricketdata",
    "demography",
    "eechidna",
    "emma",
    "expsmooth",
    "fds",
    "fma",
    "forecast",
    "fpp",
    "fpp2",
    "ftsa",
    "hdrcde",
    "hts",
    "Mcomp",
    "MEFM",
    "ozdata",
    "rainbow",
    "robets",
    "smoothAPC",
    "stR",
    "sugrrants",
    "thief",
    "tscompdata",
    "tsfeatures"))

