###
###
###
###   Purpose:   Build Pedigree from TVD-DAta
###   started:   2017/11/02 (skn and pvr)
###
### ######################################### ###


### # constants about format
lFormatBorder <- list(TierId = list(lower=79, upper=92),
                      MutterId = list(lower=23, upper=36),
                      VaterId = list(lower=98, upper=111),
                      GeburtsJahr = list(lower=71, upper=74),
                      Geburtsdatum = list(lower=71, upper=78),
                      TierRassecode = list(lower=93, upper=95),
                      MutterRassecode = list(lower=37, upper=39),
                      VaterRassecode = list(lower=112, upper=114))

### # init result
lResultPedigree <- NULL



sSplitChar <- ""   # character, welcher Felder trennt
sInputFile <- "data/KLDAT_20170524_20.txt"
# check that input file exists
if (!file.exists(sInputFile))
  stop("Cannot find input file: ",sInputFile )

### # open connection to input file
conInput <- file(description = sInputFile, open = "r")
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
  cat("Line: ", nrLine, " TierId: ", sTierId, "\n")
  sMutterId <- substr(x = curLine, 
                      start = lFormatBorder$MutterId$lower,
                      stop  = lFormatBorder$MutterId$upper)
  cat("Line: ", nrLine, " MutterId: ", sMutterId, "\n")
  sVaterId <- substr(x = curLine, 
                    start = lFormatBorder$VaterId$lower,
                    stop  = lFormatBorder$VaterId$upper)
  cat("Line: ", nrLine, " VaterId: ", sVaterId, "\n")
  sGeburtsJahr <- substr(x = curLine, 
                     start = lFormatBorder$GeburtsJahr$lower,
                     stop  = lFormatBorder$GeburtsJahr$upper)
  cat("Line: ", nrLine, " GeburtsJahr: ", sGeburtsJahr, "\n")
  sGeburtsdatum <- substr(x = curLine, 
                         start = lFormatBorder$Geburtsdatum$lower,
                         stop  = lFormatBorder$Geburtsdatum$upper)
  cat("Line: ", nrLine, " Geburtsdatum: ", sGeburtsdatum, "\n")
  sTierRassecode <- substr(x = curLine, 
                          start = lFormatBorder$TierRassecode$lower,
                          stop  = lFormatBorder$TierRassecode$upper)
  cat("Line: ", nrLine, " TierRassecode: ", sTierRassecode, "\n")
  sMutterRassecode <- substr(x = curLine, 
                           start = lFormatBorder$MutterRassecode$lower,
                           stop  = lFormatBorder$MutterRassecode$upper)
  cat("Line: ", nrLine, " MutterRassecode: ", sMutterRassecode, "\n")
  sVaterRassecode <- substr(x = curLine, 
                             start = lFormatBorder$VaterRassecode$lower,
                             stop  = lFormatBorder$VaterRassecode$upper)
  cat("Line: ", nrLine, " VaterRassecode: ", sVaterRassecode, "\n")
  
  
  ### # list of animal info
  lAniInfo <- list(TierId=sTierId, MutterId=sMutterId, VaterId=sVaterId, 
                   GeburtsJahr=sGeburtsJahr, Geburtsdatum=sGeburtsdatum, 
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
  }
  
}
close(conInput)

### # generate output
