###
###
###
###   Purpose:   Build Pedigree from TVD-DAta
###   started:   2017/11/02 (skn and pvr)
###
### ######################################### ###


### # constants about format
lFormatBorder <- list(TierId = list(lower=79, upper=92),
                      MutterId = list(lower=23, upper=36))

### # init result
lResultPedigree <- NULL

### # new stuff comes here

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
  
  ### # list of animal info
  lAniInfo <- list(TierId=sTierId, MutterId=sMutterId)
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
