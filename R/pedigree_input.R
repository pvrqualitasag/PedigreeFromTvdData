###
###
###
###   Purpose:   Input TVD-Data from file
###   started:   2017-11-06 (skn and pvr)
###
### ######################################### ###

#' Reading TVD data and construct a pedigree
#'
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


#' Read TVD-Pedigree from file using readr::read_fwf()
#'
#' @param psInputFile name of the input file
#' @param pvecColPosition vector with column positions
#' @param pbOut flag indicating whether output should be written
#' @return tbl_result_pedigree resulting pedigree as tibble
#' @export readr_fwf_tvd_input
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
