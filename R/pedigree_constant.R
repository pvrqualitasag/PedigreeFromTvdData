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
  return(c(22,14,3,31,8,14,3,1,1,14,3))
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
#' The results returned by this function are based
#' on the definition of a TVD-ID which has two positions
#' for a country code and twelve positions for a number
#'
#' @return list of start and end positions for parts of TVD-ID
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


#' @title Get Format from DSCH xlsx-file as tbl_df
#'
#' @importFrom readxl read_excel
#'
#' @description
#' The data format defined in DSCH (Datenschnittstelle)
#' Rindvieh.ch is read from an xlsx file and returned
#' as a tbl_df. The result contains the complete description
#' of all formats. From that result single format definitions
#' can be extracted.
#'
#' @param psFormatDschFile path to DSCH-xlsx file
#' @return tbl_df containing DSCH format definitions
#' @export getDschFormatTblDf
getDschFormatTblDf <- function(psFormatDschFile){
  # check that file psFormatDschFile exists
  if (!file.exists(psFormatDschFile)){
    stop(" *** ERROR: Cannot find DSCH format file: ", psFormatDschFile)
  }
  return(readxl::read_excel(path = psFormatDschFile))
}


#' Get K11 Format from the DSCH-format file
#'
#' Given the DSCH-format read from its xlsx file,
#' the format definition of K11 data is
#' extracted and returned as tbl_df
#'
#' @param psFormatDschFile path to DSCH-xlsx file
#' @param pnTagCol column index containing tags in DSCH-tbl_df (default = 2)
#' @param psStartTag Tag indicating starting row of K11 format (default = "Satzart K11")
#' @param psEndTag Tag indicating end row of K11 format (default = NA)
#' @return tbl_df containing K11 definition
#' @export getK11ColPositionFromDsch
getK11ColPositionFromDsch <- function(psFormatDschFile = getFormatDSCHFile(),
                                      pnTagCol   = 2,
                                      psStartTag = "Satzart K11",
                                      psEndTag   = NA) {
  tbl_dsch_format <- getDschFormatTblDf(psFormatDschFile = psFormatDschFile)

  ### # K11 start row
  n_k11_start_row <- which(tbl_dsch_format[,pnTagCol] == psStartTag)
  ### # we should only have one starting tag
  if (length(n_k11_start_row) != 1){
    stop("*** No unique starting position found:")
    print(n_k11_start_row)
  }
  ### # maching positions of end tags
  if (is.na(psEndTag)){
    vecEndPos <- which(is.na(tbl_dsch_format[,pnTagCol]))
  } else {
    vecEndPos <- which(tbl_dsch_format[,pnTagCol] == psEndTag)
  }
  ### # end tag must be found at least once, otherwise stop with error
  if (length(vecEndPos) < 1)
    stop(" *** Cannot find position of end tag: ", psEndTag)

  ### # in case more than one ending positions were found, take
  ### #  the first after the starting position
  if(length(vecEndPos) > 1){
    n_k11_end_row <- vecEndPos[vecEndPos > n_k11_start_row][1]
  } else {
    n_k11_end_row <- vecEndPos
  }

  ### # check that end is not before start
  if (n_k11_end_row < n_k11_start_row)
    stop(" End-row of K11 smaller than start-row")

  ### # return rows between start and end
  return(tbl_dsch_format[n_k11_start_row:(n_k11_end_row-1),])
}


#' Get K11 Column positions as vector
#'
#' Positions where columns are separated in a fixed-width-formatted
#' input file for the K11 format are taken from the xlsx-DSCH format
#' definition file. The column separator positions are returned as
#' a vector of integers, because K11 does not contain any floating
#' point columns.
#'
#' @param psFormatDschFile name of xlsx-file conaining DSCH-formats
#' @param pnColPosition column index where positions are stored
#' @return vector with K11 column positions
#' @export getK11ColPositionVecFromDsch
getK11ColPositionVecFromDsch <- function(psFormatDschFile = getFormatDSCHFile(),
                                         pnColPosition = 5){
  ### # read DSCH format from given xlsx-file
  tbl_dsch_k11 <- getK11ColPositionFromDsch(psFormatDschFile = psFormatDschFile)
  return(as.integer(tbl_dsch_k11[[pnColPosition]]))

}


#' Get column indices of different IDs in pedigree tbl_df
#'
#' Given the DSCH-format of K11 read from psFormatDschFile, the
#' column index in the peidgree tbl_df is searched using some
#' tag patterns that are specific for the ID-columns
#'
#' @param psFormatDschFile DSCH format file (default = "RindviehCH_Schnittstelle_4_13_D_K11.xlsx")
#' @param pnTagCol column in DSCH format where tags occur
#' @param ps_pattern_tier_id Tag for TierID (default = "Kalb Id")
#' @param ps_pattern_mutter_id Tag for MutterId (default = "Mutter Id")
#' @param ps_pattern_vater_id Tag for VaterId (default = "Vater Id")
#' @return list of column indices for TierId, MutterId and VaterId
#' @export getTvdIdColsDsch
getTvdIdColsDsch <- function(psFormatDschFile = getFormatDSCHFile(),
                             pnTagCol   = 2,
                             ps_pattern_tier_id = "Kalb Id",
                             ps_pattern_mutter_id = "Mutter Id",
                             ps_pattern_vater_id = "Vater Id"){
  ### # read DSCH format from given xlsx-file
  tbl_dsch_k11 <- getK11ColPositionFromDsch(psFormatDschFile = psFormatDschFile)
  ### # return list column positions found using the specified patterns
  return(list(TierIdCol = grep(pattern = ps_pattern_tier_id, tbl_dsch_k11[[pnTagCol]]),
              MutterIdCol = grep(pattern = ps_pattern_mutter_id, tbl_dsch_k11[[pnTagCol]]),
              VaterIdCol = grep(pattern = ps_pattern_vater_id, tbl_dsch_k11[[pnTagCol]])))
}


#' Get column index for birthdate with new DSCH-format
#'
#' @param psFormatDschFile name of the DSCH format file
#' @param pnTagCol column index of tag values
#' @param ps_pattern_geburt_dat tag value to search for
#' @export getBirthdateColIdxDsch
getBirthdateColIdxDsch <- function(psFormatDschFile = getFormatDSCHFile(),
                                   pnTagCol = 2,
                                   ps_pattern_geburt_dat = "Kalbedatum"){
  ### # read DSCH format from given xlsx-file
  tbl_dsch_k11 <- getK11ColPositionFromDsch(psFormatDschFile = psFormatDschFile)
  ### # return list column positions found using the specified patterns
  return(grep(pattern = ps_pattern_geburt_dat, tbl_dsch_k11[[pnTagCol]]))
}


#' Return path to the DSCH-format file
#'
#' @return Path to the DSCH-format file
#' @export getFormatDSCHFile
getFormatDSCHFile <- function(){
  return(system.file(file.path("extdata",
                               "RindviehCH_Schnittstelle_4_13_D_K11.xlsx"),
                     package = "PedigreeFromTvdData"))
}
