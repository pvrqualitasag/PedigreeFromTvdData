###
###
###
###   Purpose:   Input TVD-Data from file
###   started:   2017-11-06 (skn and pvr)
###
### ######################################### ###

#' @title Reading TVD data and construct a pedigree
#'
#' @description
#' First attempt to read a pedigree from a fwf-file and storing
#' the pedigree-information in a nested list of lists.
#' @section NOTE:
#' The state of this function is deprecated and hence it
#' should not be used. When reading large files this takes
#' a very long time to executed and can therefore not be
#' used for real pedigree data.
#' @param  psInputFile input file with TVD-daten
#' @param  lFormatBorder list of format borders
#' @param  pbOut flag to indicate whether debugging out should be written to the console
#' @return lResultPedigree constructed pedigree as list
#' @export read_tvd_input
read_tvd_input <- function(psInputFile,
                           lFormatBorder = getFormatBorder(),
                           pbOut = FALSE){
  # check that input file exists
  if (!file.exists(psInputFile))
    stop("Cannot find input file: ",psInputFile )

  ### # initialize
  lResultPedigree <- NULL

  ### # open connection to input file
  conInput <- file(description = psInputFile, open = "r")
  nrLine <- 0
  while (length(curLine  <- readLines(con = conInput, n = 1)) > 0) {
    ### # … Aufbau der Liste mit den Pedigree-Infos hier einfügen
    ### # …
    ### # line number as
    nrLine <- nrLine + 1
    ### # extract Infos
    sTierId <- substr(x = curLine,
                      start = lFormatBorder$TierId$lower,
                      stop  = lFormatBorder$TierId$upper)
    if (pbOut) cat("Line: ", nrLine, " TierId: ", sTierId, "\n")
    sMutterId <- substr(x = curLine,
                        start = lFormatBorder$MutterId$lower,
                        stop  = lFormatBorder$MutterId$upper)
    if (pbOut) cat("Line: ", nrLine, " MutterId: ", sMutterId, "\n")
    sVaterId <- substr(x = curLine,
                       start = lFormatBorder$VaterId$lower,
                       stop  = lFormatBorder$VaterId$upper)
    if (pbOut) cat("Line: ", nrLine, " VaterId: ", sVaterId, "\n")
    sGeburtsdatum <- substr(x = curLine,
                            start = lFormatBorder$Geburtsdatum$lower,
                            stop  = lFormatBorder$Geburtsdatum$upper)
    if (pbOut) cat("Line: ", nrLine, " Geburtsdatum: ", sGeburtsdatum, "\n")
    sTierRassecode <- substr(x = curLine,
                             start = lFormatBorder$TierRassecode$lower,
                             stop  = lFormatBorder$TierRassecode$upper)
    if (pbOut) cat("Line: ", nrLine, " TierRassecode: ", sTierRassecode, "\n")
    sMutterRassecode <- substr(x = curLine,
                               start = lFormatBorder$MutterRassecode$lower,
                               stop  = lFormatBorder$MutterRassecode$upper)
    if (pbOut) cat("Line: ", nrLine, " MutterRassecode: ", sMutterRassecode, "\n")
    sVaterRassecode <- substr(x = curLine,
                              start = lFormatBorder$VaterRassecode$lower,
                              stop  = lFormatBorder$VaterRassecode$upper)
    if (pbOut) cat("Line: ", nrLine, " VaterRassecode: ", sVaterRassecode, "\n")
    sSex <- substr(x = curLine,
                      start = lFormatBorder$Sex$lower,
                      stop  = lFormatBorder$Sex$upper)
    if (pbOut) cat("Line: ", nrLine, " Sex: ", sSex, "\n")


    ### # list of animal info, empty field to fill with NA
    lAniInfo <- list(TierId = subBlank(psValue = sTierId),
                     MutterId = subBlank(psValue = sMutterId),
                     VaterId = subBlank(psValue = sVaterId),
                     Geburtsdatum = subBlank(psValue = sGeburtsdatum),
                     Sex = subBlank(psValue = sSex),
                     TierRassecode = subBlank(psValue = sTierRassecode),
                     MutterRassecode = subBlank(psValue = sMutterRassecode),
                     VaterRassecode = subBlank(psValue = sVaterRassecode))

    ### # store all info in lResultPedigree
    ### #  sTierId is only added if is not already in the list
    if (is.null(lResultPedigree[[sTierId]])) {
      ### # sTierId is only added if not emtpy
      if (sTierId != ""){
        lResultPedigree[[sTierId]] <- lAniInfo
      } else {
        cat("Empty TierId on line: ", nrLine, "\n")
      }
    } else {
      cat("Already existing animal with Id: ", sTierId, "\n")
      ### # further checks, with respect to amount of information of duplicate animal
      ### # ...
    }

  }
  close(conInput)
  ### # result pedigree as list
  return(lResultPedigree)
}


#' Substitute blanks and replace empty string by NA
#'
#' @param psValue value input
#' @return String psValue with all blanks removed
#' @export subBlank
subBlank <- function(psValue){
  sResult <- gsub(pattern = "[[:blank:]+]",
              replacement = "",
              x = psValue)
  if (sResult == ""){
    return (NA)
  }
  return(sResult)
}


#' @title Read TVD-Pedigree from file using readr::read_fwf()
#'
#' @importFrom readr read_fwf
#' @importFrom readr fwf_widths
#' @description
#' The content of the fwf-file psInputFile is read using the function
#' \code{readr::read_fwf}. The result contains the pedigree information
#' as tbl_df.
#' @param psInputFile name of the input file
#' @param pvecColPosition vector with column positions
#' @param pbOut flag indicating whether output should be written
#' @return tbl_result_pedigree resulting pedigree as tibble
#' @export readr_fwf_tvd_input
#' @usage readr_fwf_tvd_input(psInputFile, pvecColPosition, pbOut)
#' @examples
#' s_data_file <- system.file(file.path("extdata","KLDAT_20170524_10000.txt"),
#'                            package = "PedigreeFromTvdData")
#' tbl_ped <- PedigreeFromTvdData::readr_fwf_tvd_input(psInputFile = s_data_file)
readr_fwf_tvd_input <- function(psInputFile,
                                pvecColPosition = getColPositions(),
                                pbOut = FALSE){
  if (pbOut) cat(" ==> Reading TVD Pedigree input from file: ", psInputFile, "\n")
  tbl_result_pedigree <- readr::read_fwf(file = psInputFile,
                                         col_positions = readr::fwf_widths(widths = pvecColPosition))
  if (pbOut) cat(" ==> Input file read with dimensions: nrow/ncol: ",
                 nrow(tbl_result_pedigree), "/", ncol(tbl_result_pedigree), "\n")

  return(tbl_result_pedigree)

}


#' @title Read TVD-Pedigree from inputfile using LaF::laf_open_fwf()
#'
#' @importFrom LaF laf_open_fwf
#' @importFrom dplyr tbl_df
#' @description
#' The content of the fwf-file ps_input_file is read using the function
#' \code{LaF::laf_open_fwf()}. Besides the name of the input-file, the
#' column types and the vector of column widths must also be specified.
#' Column types are hard-coded and set to char. The column-widths are
#' taken from the result of the function \code{getK11ColPositionVecFromDsch()}
#' by default.
#' @param ps_input_file name of the input file
#' @param pvec_col_position vector with column positions
#' @param pb_out flag indicating whether output should be written
#' @return tbl_df containing the pedigree info read from ps_input_file
#' @export laf_open_fwf_tvd_input
#' @usage laf_open_fwf_tvd_input(ps_input_file, pvec_col_position, pb_out)
#' @examples
#' s_data_file <- system.file(file.path("extdata","KLDAT_20170524_10000.txt"),
#'                            package = "PedigreeFromTvdData")
#' tbl_ped <- PedigreeFromTvdData::laf_open_fwf_tvd_input(ps_input_file = s_data_file)
laf_open_fwf_tvd_input <- function(ps_input_file,
                                   pvec_col_position = getK11ColPositionVecFromDsch(),
                                   pb_out = FALSE){
  if (pb_out)
    cat(" ==> laf_open_fwf_tvd_input: Reading TVD Pedigree input from file: ",
        ps_input_file, "\n")

  laf <- LaF::laf_open_fwf(filename = ps_input_file,
                          column_types = rep("character", length(pvec_col_position)),
                          column_widths = pvec_col_position)

  ### # convert laf to tbl_df
  tbl_pedigree_result <- dplyr::tbl_df(laf[ , ])

<<<<<<< HEAD
<<<<<<< HEAD
  ### # check number of records read
  nr_recs_read <- nrow(tbl_pedigree_result)
  if (pb_out)
    cat(" ==> number of records read: ", nr_recs_read, "\n")


=======
=======
>>>>>>> 8b75c43563c40087f7b0f4b53e72732c7e3c84ed
  ### # check number of rows read
  if (pb_out){
    cat(" ==> number of records read: ", nrow(tbl_pedigree_result), "\n")
    cat(" ==> number of columns read: ", ncol(tbl_pedigree_result), "\n")
  }


  ### # return pedigree as tbl_df
<<<<<<< HEAD
>>>>>>> origin/devel
=======
>>>>>>> 8b75c43563c40087f7b0f4b53e72732c7e3c84ed
  return(tbl_pedigree_result)
}
