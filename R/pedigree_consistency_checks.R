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

#' Correct Ids in column pnIdCol which do not have the correct format
#'
#' Given a pedigree in a tibble, set all values in pnIdCol to NA which
#' do not have the correct format.
#'
#' @param ptblPedigreeResult input pedigree to be checked as tibble
#' @param plFormatBorder list with format borders
#' @param pnIdCol column to be checked inside of the pedigree
#' @return tblPedigreeResult corrected tibble pedigree
correct_tvd_format <- function(ptblPedigreeResult, plFormatBorder, pnIdCol) {
  ### # copy argument to result
  tblPedigreeResult <- ptblPedigreeResult

  ### # check whether first two positions TierId are letters
  vecCountryIdx <- which(is.notletter(substr(tblPedigreeResult[[pnIdCol]],
                                             plFormatBorder$TVDCountry$lower,
                                             plFormatBorder$TVDCountry$upper)))
  if (length(vecCountryIdx > 0)){
    tblPedigreeResult[[pnIdCol]][vecCountryIdx] <- NA
  }
  ### # check whether other positions are numbers TVDNumber
  vecNumberIdx <- which(is.notnumber(substr(tblPedigreeResult[[pnIdCol]],
                                           plFormatBorder$TVDNumber$lower,
                                           plFormatBorder$TVDNumber$upper)))
  if (length(vecNumberIdx) > 0){
    tblPedigreeResult[[pnIdCol]][vecNumberIdx] <- NA
  }

  return(tblPedigreeResult)
}


#' Checking Format of TVD-Ids in Pedigree ptblPedigree
#'
#' @param ptblPedigree pedigree as tibble
#' @param lFormatBorder list with format borders
#' @param lIdCols list with column indices where TVD-ids are stored
#' @return corrected pedigree
#' @export check_tvd_id_tbl
check_tvd_id_tbl<- function(ptblPedigree,
                          plFormatBorder = getTVDIdBorder(),
                          plIdCols = getTvdIdCols()){
  ### # copy argument to result
  tblPedigreeResult <- ptblPedigree

  ### # checks for TierId
  tblPedigreeResult <- correct_tvd_format(ptblPedigreeResult = tblPedigreeResult,
                                          plFormatBorder = plFormatBorder,
                                          pnIdCol = plIdCols$TierIdCol)

  ### # checks for MutterId
  tblPedigreeResult <- correct_tvd_format(ptblPedigreeResult = tblPedigreeResult,
                                          plFormatBorder = plFormatBorder,
                                          pnIdCol = plIdCols$MutterIdCol)

  ### # checks for VaterId
  tblPedigreeResult <- correct_tvd_format(ptblPedigreeResult = tblPedigreeResult,
                                          plFormatBorder = plFormatBorder,
                                          pnIdCol = plIdCols$VaterIdCol)


  return(tblPedigreeResult)
}

#' Validation of tvd-number
#'
#' Given a pedigree imported from TVD-data, we want to
#' check if the first two chars == country, after twelve chars numeric
#'
#' @param plPedigree input pedigree
#' @param lFormatBorder list of consistency border by default taken from getTVDIdBorder()
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
#' @param pId Id to be tested
#' @export is.notletter
is.notletter <- function(pId){
  grepl("[^[:alpha:]]", pId)
}

#' Check the number-code in TVDid
#'
#' Check if there are something else than number after the country letters of tvdid
#' @param pId Id to be tested
#' @export is.notnumber
is.notnumber <- function(pId){
  grepl("[^[:digit:]]", pId)
}

### ######################################################## ###
#' Validation of birthdate border and limit
#'
#' Format of the birdate (YearMonthDay) have to be checked with some limits
#'
#' @param lFormatBorder list of consistency border by default taken from getBirthdateBorder()
#' @param lLimitValue list of consistency limit value by default taken from getBirthdayConsistencyLimit()
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


#' Validation of birthdate format using tbl_df pedigree
#'
#'
check_birthdate_tbl <- function(ptblPedigree,
                                lFormatBorder = getBirthdateBorder(),
                                lLimitValue = getBirthdayConsistencyLimit(),
                                pnBirthdateColIdx = getBirthdateColIdx()){
  tblPedigreeResult <- ptblPedigree

  ### # check whether day is within limits
  vecDay <- tblPedigreeResult[,pnBirthdateColIdx] %% 100
  vecInvalidDay <- which((vecDay < lLimitValue$cLowestLimitDay |
                           vecDay > lLimitValue$cHighestLimitDay) &
                           !is.na(vecDay))
  if (length(vecInvalidDay) > 0){
    tblPedigreeResult[vecInvalidDay,pnBirthdateColIdx] <- NA
  }

  ### # month
  vecMonth <- (tblPedigreeResult[,pnBirthdateColIdx] %/% 100) %% 100
  vecInvalidMonth <- which((vecMonth < lLimitValue$cLowestLimitMonth |
                            vecMonth > lLimitValue$cHighestLimitMonth) &
                             !is.na(vecMonth))
  if (length(vecInvalidMonth) > 0){
    tblPedigreeResult[vecInvalidMonth,pnBirthdateColIdx] <- NA
  }

  ### # year

  ### # birthdates which are not missing
  !is.na(tblPedigreeResult[,pnBirthdateColIdx])



}


### ######################################################## ###
#' Validation of sex
#'
#' Sex of the parent will be checked
#'
#' @param plPedigree list of list containing the pedigree information
#' @param lsex list of consistency values by default taken from getConsistencySex()
#' @return lCheckedPedigree4
#' @export check_sex
check_sex <- function(plPedigree, lsex = getConsistencySex()){
  ### # initialize result
  lCheckedPedigree4 <- plPedigree
  vecAnimals <- names(lCheckedPedigree4)
  for(idxPed in 1:length(lCheckedPedigree4)){
    lCurrentAni <- lCheckedPedigree4[[idxPed]]
    if(is.element(lCurrentAni$MutterId, vecAnimals)){
      if(!is.na(lCheckedPedigree4[[lCurrentAni$MutterId]]$Sex)){
        if(lCheckedPedigree4[[lCurrentAni$MutterId]]$Sex != lsex$cWeiblich){
          lCheckedPedigree4[[lCurrentAni$MutterId]]$Sex <- NA
        }
      }
    }
    if(is.element(lCurrentAni$VaterId, vecAnimals)){
      if(!is.na(lCheckedPedigree4[[lCurrentAni$VaterId]]$Sex)){
        if(lCheckedPedigree4[[lCurrentAni$VaterId]]$Sex != lsex$cMaennlich){
          lCheckedPedigree4[[lCurrentAni$VaterId]]$Sex <- NA
        }
      }
    }
    lCheckedPedigree4[[idxPed]] <- lCurrentAni
  }
  return(lCheckedPedigree4)
}
