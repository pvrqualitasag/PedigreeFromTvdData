###
###
###
###   Purpose:   collection of utility functions
###   started:   2017-12-06 (pvr)
###
### ############################################## ###

#' Cleanup, build vignettes, documents and install
#'
#'
clean_build_install <- function(pkg = "."){
  devtools::clean_vignettes(pkg = pkg)
  devtools::build_vignettes(pkg = pkg)
  devtools::document(pkg = pkg)
  devtools::install(pkg = pkg)
  return((invisible(TRUE)))
}
