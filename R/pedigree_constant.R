###
###
###
###   Purpose:   Constants related to pedigree input
###   started:   2017-11-06 (skn and pvr)
###
### ################################################## ###

#' Return default border values for fixed input format
#'
#' @return list of format borders
#' @export getFormatBorder
getFormatBorder <- function(){
  return(list(TierId = list(lower=79, upper=92),
              MutterId = list(lower=23, upper=36),
              VaterId = list(lower=98, upper=111),
              Geburtsdatum = list(lower=71, upper=78),
              TierRassecode = list(lower=93, upper=95),
              MutterRassecode = list(lower=37, upper=39),
              VaterRassecode = list(lower=112, upper=114),
              Sex = list(lower=96, upper=96)))
}

#' @export getTVDIdBorder
getTVDIdBorder <- function(){
  return(list(TVDCountry = list(lower=1, upper=2),
              TVDNumber = list(lower=3, upper=14)))
}

#' @export getBirthdateBorder
getBirthdateBorder <- function(){
  return(list(Year = list(lower=1, upper=4),
              Month = list(lower=5, upper=6),
              Day = list(lower=7, upper=8)))
}
