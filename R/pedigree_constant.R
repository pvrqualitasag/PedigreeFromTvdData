###
###
###
###   Purpose:   Constants related to pedigree input
###   started:   2017-11-06 (skn and pvr)
###
### ################################################## ###


#' Return default column indices for Ids
#'
#' @return list with Id columns
#' @export getTvdIdCols
getTvdIdCols <- function(){
  return(list(TierIdCol = 6, MutterIdCol = 2, VaterIdCol = 9))
}

#' Return default column positions for fixed width coming from TVD
#'
#' @return vector with column positions
#' @export getColPositions
getColPositions <- function(){
  return(c(22,14,3,31,8,14,3,1,14,3))
}


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

#' Get consistency border for TVDid
#'
#' @export getTVDIdBorder
getTVDIdBorder <- function(){
  return(list(TVDCountry = list(lower=1, upper=2),
              TVDNumber = list(lower=3, upper=14)))
}

#' Get consistency border for birthdate
#'
#' @export getBirthdateBorder
getBirthdateBorder <- function(){
  return(list(Year = list(lower=1, upper=4),
              Month = list(lower=5, upper=6),
              Day = list(lower=7, upper=8)))
}

#' Get consistency limits for birthdates
#'
#' @export getBirthdayConsistencyLimit
getBirthdayConsistencyLimit <- function(){
  return(list(cLowestLimitYear = 1950,
              cLowestLimitMonth = 1,
              cHighestLimitMonth = 12,
              cLowestLimitDay = 1,
              cHighestLimitDay = 31))
}

#' Get column index of birthdate in tbl_df pedigree
#'
#' Because there are no package-global variables, we
#' return the fix-coded column index of birthdate as
#' the result of a function.
#'
#' @return column index of birthdate in tbl_df pedigree
#' @export getBirthdateColIdx
getBirthdateColIdx <- function(){
  return(5)
}


#' Get consistency sex
#'
#' @export getConsistencySex
getConsistencySex <- function(){
  return(list(
    cMaennlich = 1,
    cWeiblich = 2
  ))
}
