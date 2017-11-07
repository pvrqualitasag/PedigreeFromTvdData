###
###
###
###   Purpose:   All scripts related to consistency checks
###   started:   2017-11-06 (skn and pvr)
###
### ######################################################## ###

#' Check whether parents appear as animals
#'
#' @description
#' Given a pedigree imported from TVD-data, we want to
#' check whether a given parent also exists as an animal
#'
#' @details
#' This section contains details about the function.
#'
#' @param plResultPedigree input pedigree
#' @return lCheckResultPedigree with mother and father changed according to checks
#' @export check_parent_as_animal
check_parent_as_animal <- function(plPedigree){
  ### # initialize result
  lCheckResultPedigree <- plPedigree
  ### # animals are in names of plResultPedigree
  vecAnimals <- names(lCheckResultPedigree)
  ### # loop over pedigree
  for (idxPed in 1:length(lCheckResultPedigree)){
    lCurrentAni <- lCheckResultPedigree[[idxPed]]
    if(!is.element(lCurrentAni$MutterId, vecAnimals)){
      lCurrentAni$MutterId <- NA
    }
    if(!is.element(lCurrentAni$VaterId, vecAnimals)){
      lCurrentAni$VaterId <- NA
    }
    lCheckResultPedigree[[idxPed]] <- lCurrentAni
  }
  return(lCheckResultPedigree)
}



### ######################################################## ###

#' Validation of tvd-number
#'
#' Given a pedigree imported from TVD-data, we want to
#' check if the first two chars == country, after twelve chars numeric
#'
#' @param plPedigree input pedigree
#' @return lCheckedPedigree2 with mother and father changed according to checks
#' @export check_tvdid
check_tvdid <- function(plPedigree,lFormatBorder = getTVDIdBorder()){
  ### # initialize result
  lCheckedPedigree2 <- plPedigree
  for(idxPed in 1:length(lCheckedPedigree2)){
    lCurrentAni <- lCheckedPedigree2[[idxPed]]
    if(is.notletter(pId = substr(lCurrentAni$TierId,
                               start = lFormatBorder$TVDCountry$lower,
                               stop  = lFormatBorder$TVDCountry$upper))){
      lCurrentAni$TierId <- NA
    }
    if(is.notnumber(pId = substr(lCurrentAni$TierId,
                               start = lFormatBorder$TVDNumber$lower,
                               stop  = lFormatBorder$TVDNumber$upper))){
      lCurrentAni$TierId <- NA
    }
    if(is.notletter(pId = substr(lCurrentAni$MutterId,
                                  start = lFormatBorder$TVDCountry$lower,
                                  stop  = lFormatBorder$TVDCountry$upper))){
      lCurrentAni$MutterId <- NA
    }
    if(is.notnumber(pId = substr(lCurrentAni$MutterId,
                                  start = lFormatBorder$TVDNumber$lower,
                                  stop  = lFormatBorder$TVDNumber$upper))){
      lCurrentAni$MutterId <- NA
    }
    if(is.notletter(pId = substr(lCurrentAni$VaterId,
                                  start = lFormatBorder$TVDCountry$lower,
                                  stop  = lFormatBorder$TVDCountry$upper))){
      lCurrentAni$VaterId <- NA
    }
    if(is.notnumber(pId = substr(lCurrentAni$VaterId,
                                  start = lFormatBorder$TVDNumber$lower,
                                  stop  = lFormatBorder$TVDNumber$upper))){
      lCurrentAni$VaterId <- NA
    }
    lCheckedPedigree2[[idxPed]] <- lCurrentAni
  }
  return(lCheckedPedigree2)
}

#' Check the country-code in TVDid
#'
#' Check if there are something else than letter in the country-part of tvdid
#' @param pId
#' @export is.notletter
is.notletter <- function(pId){
  grepl("[^[:alpha:]]", pId)
}

#' Check the number-code in TVDid
#'
#' Check if there are something else than number after the country letters of tvdid
#' @param pId
#' @export is.notnumber
is.notnumber <- function(pId){
  grepl("[^[:digit:]]", pId)
}

### ######################################################## ###
#' Validation of birthdate border and limit
#'
#' Format of the birdate (YearMonthDay) have to be checked with some limits
#'
#' @return lCheckedPedigree3
#' @export check_birthdate
check_birthdate <- function(plPedigree,lFormatBorder = getBirthdateBorder(), lLimitValue = getBirthdayConsistencyLimit()){
  ### # initialize result
  lCheckedPedigree3 <- plPedigree
  for(idxPed in 1:length(lCheckedPedigree3)){
    lCurrentAni <- lCheckedPedigree3[[idxPed]]
    if(is.na(as.numeric(lCurrentAni$Geburtsdatum))){
      lCurrentAni$Geburtsdatum <- NA
    }
    else{
      if(as.numeric(substr(lCurrentAni$Geburtsdatum,
              start = lFormatBorder$Year$lower,
              stop  = lFormatBorder$Year$upper)) < lLimitValue$cLowestLimitYear){
      lCurrentAni$Geburtsdatum <- NA
      }
      if(as.numeric(substr(lCurrentAni$Geburtsdatum,
              start = lFormatBorder$Month$lower,
              stop  = lFormatBorder$Month$upper)) < lLimitValue$cLowestLimitMonth ||
       as.numeric(substr(lCurrentAni$Geburtsdatum,
              start = lFormatBorder$Month$lower,
              stop  = lFormatBorder$Month$upper)) > lLimitValue$cHighestLimitMonth){
      lCurrentAni$Geburtsdatum <- NA
      }
      if(as.numeric(substr(lCurrentAni$Geburtsdatum,
              start = lFormatBorder$Day$lower,
              stop  = lFormatBorder$Day$upper)) < lLimitValue$cLowestLimitDay ||
       as.numeric(substr(lCurrentAni$Geburtsdatum,
              start = lFormatBorder$Day$lower,
              stop  = lFormatBorder$Day$upper)) > lLimitValue$cHighestLimitDay){
      lCurrentAni$Geburtsdatum <- NA
      }
    }
    lCheckedPedigree3[[idxPed]] <- lCurrentAni
  }
  return(lCheckedPedigree3)
}

### ######################################################## ###
#' Validation of sex
#'
#' Sex of the parent will be checked
#'
#' @param plPedigree
#' @return lCheckedPedigree4
#' @export check_sex
check_sex <- function(plPedigree, lsex = getConsistencySex()){
  ### # initialize result
  lCheckedPedigree4 <- plPedigree
  vecAnimals <- names(lCheckedPedigree4)
  for(idxPed in 1:length(lCheckedPedigree4)){
    lCurrentAni <- lCheckedPedigree4[[idxPed]]
    if(is.element(lCurrentAni$MutterId, vecAnimals)){
      if(lCheckedPedigree4[[lCurrentAni$MutterId]]$Sex != lsex$cWeiblich){
        lCheckedPedigree4[[lCurrentAni$MutterId]]$Sex <- NA
        }
    }
    if(is.element(lCurrentAni$VaterId, vecAnimals)){
      if(lCheckedPedigree4[[lCurrentAni$VaterId]]$Sex != lsex$cMaennlich){
        lCheckedPedigree4[[lCurrentAni$VaterId]]$Sex <- NA
      }
    }
    lCheckedPedigree4[[idxPed]] <- lCurrentAni
  }
  return(lCheckedPedigree4)
}
