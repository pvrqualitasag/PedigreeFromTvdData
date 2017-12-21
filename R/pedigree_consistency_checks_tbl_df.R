###
###
###
###   Purpose:   Scripts for consistency checks based on
###              tbl_df and dplyr
###   started:   2017-12-14 (skn and pvr)
###
### ######################################################## ###


#' Checking Format of TVD-Ids in Pedigree ptblPedigree
#'
#' @param ptblPedigree pedigree as tibble
#' @param plFormatBorder list with format borders
#' @param plIdCols list with column indices where TVD-ids are stored
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
    tblPedigreeResult[[pnBirthdateColIdx]] <- as.integer(tblPedigreeResult[[pnBirthdateColIdx]])
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


### ######################################################### ###
###
###
#' @title Check that parents are older than their offspring
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @description
#' Given a pedigree in tbl_df format, all parents that are also
#' present as animals are filtered, if they are not older than
#' their offspring.
#'
#' @details
#' From the given pedigree in ptbl_pedigree, the three columns
#' containing animal-Id, birthdate and a parent-id where parent
#' can either be mother or father are extracted using `dplyr::select`.
#' The selected columns are given new names for easier readability
#' of the remaining code. In case the birthdates are formatted as
#' characters, they are converted into integers. From the original
#' set of pedigree records, all parents are selected into a separate
#' tbl_df. Their birthdate is searched using a `dplyr::inner_join()`
#' back to the orginal pedigree records. Once the birthdates for the
#' parents are found we can filter those out which have a birthdate
#' which is closer to the birthdate of the offspring than a given
#' tolerance value.
#'
#' @param ptbl_pedigree pedigree in tbl_df format
#' @param pn_offspring_col column index for offspring
#' @param pn_birthday_col column index for birthdates of offspring
#' @param pn_parent_col column index for parents
#' @param pn_date_diff_tol minimum difference between birthdates of parents and offspring
#' @return tbl_df of pedigree records not fullfilling requirements
#' @export check_parent_older_offspring
#' @usage check_parent_older_offspring(ptbl_pedigree,
#'                                     pn_offspring_col,
#'                                     pn_birthday_col,
#'                                     pn_parent_col,
#'                                     pn_date_diff_tol)
#' @examples
#' s_data_file <- system.file(file.path("extdata","KLDAT_20170524_10000.txt"),
#'                            package = "PedigreeFromTvdData")
#' tbl_ped <- PedigreeFromTvdData::laf_open_fwf_tvd_input(ps_input_file = s_data_file)
#' l_tvd_id_col_dsch <- PedigreeFromTvdData::getTvdIdColsDsch()
#' n_bd_col_idx <- PedigreeFromTvdData::getBirthdateColIdxDsch()
#' PedigreeFromTvdData::check_parent_older_offspring(ptbl_pedigree = tbl_ped,
#'                                                   pn_offspring_col = l_tvd_id_col_dsch$TierIdCol,
#'                                                   pn_birthday_col = n_bd_col_idx,
#'                                                   pn_parent_col = l_tvd_id_col_dsch$MutterIdCol)
#'
check_parent_older_offspring <- function(ptbl_pedigree,
                                         pn_offspring_col,
                                         pn_birthday_col,
                                         pn_parent_col,
                                         pn_date_diff_tol = 10^4) {

  ### # using pipes, we can link all the steps together
  tbl_age_check  <- ptbl_pedigree %>% select(pn_offspring_col,
                                             pn_birthday_col,
                                             pn_parent_col)
  ### # assign names
  names(tbl_age_check) <- c("Animal", "Birthdate", "Parent")

  ### # in case column with "Birthdate" is of type character, convert it to integer
  if (is.character(tbl_age_check$Birthdate)){
    tbl_age_check$Birthdate <- as.integer(tbl_age_check$Birthdate)
  }

  ### # piping all selections, joins and filters together
  tbl_inconsistent_result <-
    tbl_age_check %>%
    filter("Parent" != "") %>%
    select("Parent") %>%
    inner_join(tbl_age_check, by = c("Parent" = "Animal")) %>%
    select("Parent","Birthdate") %>%
    inner_join(tbl_age_check, by = "Parent") %>%
    filter((Birthdate.y - Birthdate.x) < pn_date_diff_tol)

  return(tbl_inconsistent_result[,c("Animal", "Birthdate.y", "Parent", "Birthdate.x")])
}

### ######################################################## ###
#' Validation of sex format using tbl_df pedigree
#'
#'
#' @param ptblPedigree pedigree in tbl_df format
#' @param lsex list of consistency values by default taken from getConsistencySex()
#' @param pnTvdIdColIdx vector of column indices of TVD-Ids
#' @export check_sex_tbl
check_sex_tbl <- function(ptblPedigree,
                          lsex = getConsistencySex(),
                          pnTvdIdColIdx = getTvdIdCols()){
#  tblPedigreeResult <- ptblPedigree
#  if(!is.na(tblPedigreeResult[,pnTvdIdColIdx$MutterIdCol])){
#    if(){}
  }


#' @title Check for uniqueness of Animal-IDs in a pedigree
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#'
#' @description
#' Given a pedigree in tbl_df-format, we have to make sure
#' that identifiers of animals (animal-IDs) are unique. This
#' follows from the general definition of a pedigree and
#' it is also required, because we want to use the column
#' as a primary key of the animal's pedigree record.
#'
#' @param ptbl_pedigree pedigree in tbl_df format
#' @param pn_ani_id_col_idx column index for animal-ID in pedigree
#' @param pb_out flag whether debugging output should be written
#' @return tbl_rec_result tbl_df with non-unique IDs and number of occurences
#' @export check_unique_animal_id
#' @usage check_unique_animal_id(ptbl_pedigree, pn_ani_id_col_idx, pb_out)
#' @examples
#' s_data_file <- system.file(file.path("extdata","KLDAT_20170524_10000.txt"),
#'                            package = "PedigreeFromTvdData")
#' tbl_ped <- PedigreeFromTvdData::laf_open_fwf_tvd_input(ps_input_file = s_data_file)
#' PedigreeFromTvdData::check_unique_animal_id(ptbl_pedigree = tbl_ped)
check_unique_animal_id <- function(ptbl_pedigree,
                                   pn_ani_id_col_idx = getTvdIdColsDsch()$TierIdCol,
                                   pb_out = FALSE){

  if (pb_out)
    cat(" *** Checking pedigree with number of records: ", nrow(ptbl_pedigree), "\n")
  ### # grouping records in ptbl_pedigree according to values
  ### #  in the column of animal-Ids and filtering all of
  ### #  the records that have counts greater 1
  tbl_rec_result <- ptbl_pedigree %>%
    group_by(.[[pn_ani_id_col_idx]]) %>%
    summarise(n = n()) %>%
    filter(n > 1)

  ### # specify names of result
  names(tbl_rec_result) <- c("Animal", "n")

  if (pb_out)
    cat(" *** Number of non-unique Ids found: ", nrow(tbl_rec_result), "\n")

  ### # return result
  return(tbl_rec_result)
}


### ######################################################## ###
###   Functions below this line are helper function for the  ###
###   vignette on checking data consistency.                 ###
###
#
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
#' @param pb_out flag indicating whether debugging output should be written
#' @return list indicating result of consistency check and list of row indices with inconsistent IDs
#' @export all_parent_id_consistent
all_parent_id_consistent <- function(p_tbl_ped, plIdCols, pn_parent_col, pb_out = FALSE){
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
    if (pb_out) cat(" *** All parent-ids consistent:\n")
    l_check_result$b_consistency_check <-
      all(tbl_ped_checked[[pn_parent_col]][!is.na(tbl_ped_checked[[pn_parent_col]])] ==
            p_tbl_ped[[pn_parent_col]][!is.na(p_tbl_ped[[pn_parent_col]])])
  } else {
    ### # row indices of records that are different
    l_check_result$vec_incons_rows <-
      which(is.na(tbl_ped_checked[[pn_parent_col]]) & !is.na(p_tbl_ped[[pn_parent_col]]))
    if (pb_out) {
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
#' @param pb_out flag indicating whether debugging output should be written
#' @return list of check result and row indices of non-consistent records
#' @export all_birthdate_consistent
#' @usage all_birthdate_consistent(p_tbl_ped, pn_bd_col_idx, pb_out)
all_birthdate_consistent <- function(p_tbl_ped, pn_bd_col_idx, pb_out = FALSE){
  ### # initialize a result list
  l_check_result <- list(b_consistency_check = FALSE, vec_incons_rows = NA)
  ### # run the check of the pedigree ids
  tbl_ped_checked <- PedigreeFromTvdData::check_birthdate_tbl(ptblPedigree = p_tbl_ped,
                                                              pnBirthdateColIdx = pn_bd_col_idx)

  ### # number of non-NA birthdates in original and checked pedigrees
  n_not_na_ped <- length(!is.na(p_tbl_ped[[pn_bd_col_idx]]))
  n_not_na_ped_checked <- length(!is.na(tbl_ped_checked[[pn_bd_col_idx]]))
  ### # if those numbers are the same, and
  if ((n_not_na_ped_checked == n_not_na_ped) &&
      all(p_tbl_ped[[pn_bd_col_idx]][!is.na(p_tbl_ped[[pn_bd_col_idx]])] ==
          tbl_ped_checked[[pn_bd_col_idx]][!is.na(tbl_ped_checked[[pn_bd_col_idx]])])) {
    if (pb_out) cat("*** Birthdates all consistent\n")
    l_check_result$b_consistency_check <- TRUE
  } else {
    if (pb_out) cat("*** Some birthdates are not consistent\n")
    l_check_result$vec_incons_rows <-
      which(is.na(tbl_ped_checked[[pn_bd_col_idx]]) & !is.na(p_tbl_ped[[pn_bd_col_idx]]))

  }
  return(l_check_result)
}
