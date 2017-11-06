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
    if(!is.letter(pId = substr(lCurrentAni$TierId,
                               start = lFormatBorder$TVDCountry$lower,
                               stop  = lFormatBorder$TVDCountry$upper))){
      lCurrentAni$TierId <- NA
    }
    if(!is.number(pId = substr(lCurrentAni$TierId,
                               start = lFormatBorder$TVDNumber$lower,
                               stop  = lFormatBorder$TVDNumber$upper))){
      lCurrentAni$TierId <- NA
    }
  }
  return(lCheckedPedigree2)
}

#' @export is.letter
is.letter <- function(pId){
  grepl("[[:alpha:]]", pId)
}

#' @export is.number
is.number <- function(pId){
  grepl("[[:digit:]]", pId)
}
