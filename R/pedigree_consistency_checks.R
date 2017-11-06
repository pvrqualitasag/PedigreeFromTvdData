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
    if(!is.notletter(pId = substr(lCurrentAni$TierId,
                               start = lFormatBorder$TVDCountry$lower,
                               stop  = lFormatBorder$TVDCountry$upper))){
      lCurrentAni$TierId <- NA
    }
    if(!is.notnumber(pId = substr(lCurrentAni$TierId,
                               start = lFormatBorder$TVDNumber$lower,
                               stop  = lFormatBorder$TVDNumber$upper))){
      lCurrentAni$TierId <- NA
    }
    if(!is.notletter(pId = substr(lCurrentAni$MutterId,
                                  start = lFormatBorder$TVDCountry$lower,
                                  stop  = lFormatBorder$TVDCountry$upper))){
      lCurrentAni$MutterId <- NA
    }
    if(!is.notnumber(pId = substr(lCurrentAni$MutterId,
                                  start = lFormatBorder$TVDNumber$lower,
                                  stop  = lFormatBorder$TVDNumber$upper))){
      lCurrentAni$MutterId <- NA
    }
    if(!is.notletter(pId = substr(lCurrentAni$VaterId,
                                  start = lFormatBorder$TVDCountry$lower,
                                  stop  = lFormatBorder$TVDCountry$upper))){
      lCurrentAni$VaterId <- NA
    }
    if(!is.notnumber(pId = substr(lCurrentAni$VaterId,
                                  start = lFormatBorder$TVDNumber$lower,
                                  stop  = lFormatBorder$TVDNumber$upper))){
      lCurrentAni$VaterId <- NA
    }
  }
  return(lCheckedPedigree2)
}

#' Check the country-code in TVDid
#'
#' @export is.notletter
is.notletter <- function(pId){
  grepl("[^[:alpha:]]", pId)
}

#' Check the number-code in TVDid
#'
#' @export is.notnumber
is.notnumber <- function(pId){
  grepl("[^[:digit:]]", pId)
}

### ######################################################## ###
#' Validation of birthdate
#'
#' @return lCheckedPedigree3
#' @export check_birthdate
check_birthdate <- function(plPedigree,lFormatBorder = getBirthdateBorder()){
  ### # initialize result
  lCheckedPedigree3 <- plPedigree
  for(idxPed in 1:length(lCheckedPedigree3)){
    lCurrentAni <- lCheckedPedigree3[[idxPed]]
    if(substr(lCurrentAni$Geburtsdatum,
              start = lFormatBorder$Year$lower,
              stop  = lFormatBorder$Year$upper) < "1950"){
      lCurrentAni$Geburtsdatum <- NA
    }
    if(substr(lCurrentAni$Geburtsdatum,
              start = lFormatBorder$Month$lower,
              stop  = lFormatBorder$Month$upper) < "1" ||
       substr(lCurrentAni$Geburtsdatum,
              start = lFormatBorder$Month$lower,
              stop  = lFormatBorder$Month$upper) > "12"){
      lCurrentAni$Geburtsdatum <- NA
    }
    if(substr(lCurrentAni$Geburtsdatum,
              start = lFormatBorder$Day$lower,
              stop  = lFormatBorder$Day$upper) < "1" ||
       substr(lCurrentAni$Geburtsdatum,
              start = lFormatBorder$Day$lower,
              stop  = lFormatBorder$Day$upper) > "31"){
      lCurrentAni$Geburtsdatum <- NA
    }
  }
  return(lCheckedPedigree3)
}
