###
###
###
###   Purpose:   Input TVD-Data from file
###   started:   2017-11-06 (skn and pvr)
###
### ######################################### ###

#' Reading TVD data and construct a pedigree
#'
#' @param psInputFile input file with TVD-daten
#' @param lFormatBorder list of format borders
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
    sGeburtsJahr <- substr(x = curLine,
                           start = lFormatBorder$GeburtsJahr$lower,
                           stop  = lFormatBorder$GeburtsJahr$upper)
    if (pbOut) cat("Line: ", nrLine, " GeburtsJahr: ", sGeburtsJahr, "\n")
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


    ### # list of animal info
    lAniInfo <- list(TierId=sTierId, MutterId=sMutterId, VaterId=sVaterId,
                     GeburtsJahr=sGeburtsJahr, Geburtsdatum=sGeburtsdatum, Sex=sSex,
                     TierRassecode=sTierRassecode, MutterRassecode=sMutterRassecode, VaterRassecode=sVaterRassecode)
    ### # store all info in lResultPedigree
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
