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
                          plIdCols = getTvdIdColsDsch()){
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
#' Birthdates in pedigree ptblPedigree are validated, assuming
#' that they are in numeric format as YYYYMMDD. In case the
#' birthdate is read as character, it first gets converted to
#' a numeric value. For each part of the date (year, month and
#' day), there are constant limits defined. Whenever a validation
#' fails, the date is set to NA.
#'

#' @param ptblPedigree pedigree in tbl_df format
#' @param lLimitValue list with fixed limits for year, month and date
#' @param pnBirthdateColIdx column index of birthdates in ptblPedigree
#' @return validated and modified tbl_df pedigree
#' @export check_birthdate_tbl
check_birthdate_tbl <- function(ptblPedigree,
                                lLimitValue   = getBirthdayConsistencyLimit(),
                                pnBirthdateColIdx = getBirthdateColIdxDsch()){
  tblPedigreeResult <- ptblPedigree
  ### # birthdate seams to be read as char - convert
  if (is.character(tblPedigreeResult[[pnBirthdateColIdx]])){
    tblPedigreeResult[[pnBirthdateColIdx]] <- as.numeric(tblPedigreeResult[[pnBirthdateColIdx]])
  }

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
  vecYear <- (tblPedigreeResult[,pnBirthdateColIdx] %/% 100) %/% 100
  vecInvalidYear <- which((vecYear < lLimitValue$cLowestLimitYear |
                           vecYear > as.numeric(format(Sys.Date(), "%Y")) &
                             !is.na(vecYear)))
  if (length(vecInvalidYear) > 0){
    tblPedigreeResult[vecInvalidYear,pnBirthdateColIdx] <- NA
  }
  ### # verified and modified result pedigree is returned
  return(tblPedigreeResult)
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


#' Validation of sex format using tbl_df pedigree
#'
#'
#' @param ptblPedigree pedigree in tbl_df format
#' @param lsex list of consistency values by default taken from getConsistencySex()
#' @export check_sex_tbl
check_sex_tbl <- function(ptblPedigree,
                          lsex = getConsistencySex(),
                          pnTvdIdColIdx = getTvdIdCols()){
#  tblPedigreeResult <- ptblPedigree
#  if(!is.na(tblPedigreeResult[,pnTvdIdColIdx$MutterIdCol])){
#    if(){}
  }


#' Check whether all ids of a given parent (mother or father) have consistent IDs
#'
#' Given a pedigree as tbl_df, it is first run through the TVD-ID check using
#' the function PedigreeFromTvdData::check_tvd_id_tbl(). As a result, we get the
#' checked pedigree. The original pedigree in p_tbl_ped and the checked pedigree
#' are compared and the result is returned as a list indicating whether the
#' two pedigrees are consistent and if not with a vector of row-indices where
#' inconsistencies did occur.
#'
#' @param p_tbl_ped original pedigree as tbl_df
#' @param plIdCols list with column indices for all ids, required for checking
#' @param pn_parent_col column index of parent to be checked
#' @param p_b_out flag indicating whether output should be written
#' @return list indicating result of consistency check and list of row indices with inconsistent IDs
#' @export all_parent_id_consistent
all_parent_id_consistent <- function(p_tbl_ped, plIdCols, pn_parent_col, p_b_out = FALSE){
  ### # initialize a result list
  l_check_result <- list(b_consistency_check = FALSE, vec_incons_rows = NA)
  ### # run the check of the pedigree ids
  tbl_ped_checked <- PedigreeFromTvdData::check_tvd_id_tbl(ptblPedigree = p_tbl_ped,
                                                           plIdCols = plIdCols)

  ### # check result for parents, start with number of non-NA's in both pedigrees
  n_not_na_ped <- length(p_tbl_ped[[pn_parent_col]][!is.na(p_tbl_ped[[pn_parent_col]])])
  n_not_na_ped_checked <-
    length(tbl_ped_checked[[pn_parent_col]][!is.na(tbl_ped_checked[[pn_parent_col]])])
  ### # number of non-na records are the same for original and checked pedigree
  if ( n_not_na_ped == n_not_na_ped_checked ){
    if (p_b_out) cat(" *** All parent-ids consistent:\n")
    l_check_result$b_consistency_check <-
      all(tbl_ped_checked[[pn_parent_col]][!is.na(tbl_ped_checked[[pn_parent_col]])] ==
            p_tbl_ped[[pn_parent_col]][!is.na(p_tbl_ped[[pn_parent_col]])])
  } else {
    ### # row indices of records that are different
    l_check_result$vec_incons_rows <-
      which(is.na(tbl_ped_checked[[pn_parent_col]]) & !is.na(p_tbl_ped[[pn_parent_col]]))
    if (p_b_out) {
      cat(" *** Parent-Ids different after check:\n")
      print(l_check_result$vec_incons_rows)
    }
  }
  return(l_check_result)
}


#' check whether all non-NA birthdates are consistent
#'
#' Birthdates are checked using check_birthdate_tbl() and the
#' resulting validated tbl_df pedigree is compared to the
#' original pedigree. In case that we find differences, we
#' return a list of row-indices that are not consistent
#'
#' @param p_tbl_ped original pedigree as tbl_df
#' @param pn_bd_col_idx column index of birthdate
#' @return list of check result and row indices of non-consistent records
#' @export all_birthdate_consistent
all_birthdate_consistent <- function(p_tbl_ped, pn_bd_col_idx, p_b_out = FALSE){
  ### # initialize a result list
  l_check_result <- list(b_consistency_check = FALSE, vec_incons_rows = NA)
  ### # run the check of the pedigree ids
  tbl_ped_checked <- PedigreeFromTvdData::check_birthdate_tbl(ptblPedigree = p_tbl_ped,
                                                              pnBirthdateColIdx = pn_bd_col_idx)

  ### # number of non-NA birthdates in original and checked pedigrees
  n_not_na_ped <- length(!is.na(p_tbl_ped[[n_bd_col_idx]]))
  n_not_na_ped_checked <- length(!is.na(tbl_ped_checked[[n_bd_col_idx]]))
  ### # if those numbers are the same, and
  if ((n_not_na_ped_checked == n_not_na_ped) &&
      all(p_tbl_ped[[n_bd_col_idx]][!is.na(p_tbl_ped[[n_bd_col_idx]])] ==
          tbl_ped_checked[[n_bd_col_idx]][!is.na(tbl_ped_checked[[n_bd_col_idx]])])) {
    if (p_b_out) cat("*** Birthdates all consistent\n")
    l_check_result$b_consistency_check <- TRUE
  } else {
    if (p_b_out) cat("*** Some birthdates are not consistent\n")
    l_check_result$vec_incons_rows <-
      which(is.na(tbl_ped_checked[[n_bd_col_idx]]) & !is.na(p_tbl_ped[[n_bd_col_idx]]))

  }
  return(l_check_result)
}
