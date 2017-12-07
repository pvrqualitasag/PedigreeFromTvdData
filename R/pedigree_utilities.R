###
###
###
###   Purpose:   collection of utility functions
###   started:   2017-12-06 (pvr)
###
### ############################################## ###

#' Cleanup, build vignettes, documents and install
#'
#' Simple utility to clean up old vignette output,
#' to build vignettes for the current package, to
#' roxygenize all help-texts and to install the
#' new version of the package given by pkg.
#'
#' @param pkg package directory
#' @export clean_build_install
clean_build_install <- function(pkg = "."){
  devtools::clean_vignettes(pkg = pkg)
  devtools::build_vignettes(pkg = pkg)
  devtools::document(pkg = pkg)
  devtools::install(pkg = pkg)

  return((invisible(TRUE)))
}
