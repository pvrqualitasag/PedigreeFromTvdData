###
###
###
###   Purpose:   Output Pedigree from TVD-Data
###   started:   2017-11-07 (skn and pvr)
###
### ######################################### ###


#' Output Pedigree from TVD-Data
#'
#' @param plPedigree processed Pedigree
#' @param psOutfile output Pedigree
#' @param psSepChar  parameter by default for delimiter symbol
#' @export output
output <- function(plPedigree, psOutfile = sOutputFile, psSepChar = "\t"){
  ### # initialize result
  for(idxPed in 1:length(plPedigree)){
    lCurrentAni <- plPedigree[[idxPed]]
    cat(plPedigree[[lCurrentAni$TierId]]$TierId, psSepChar,
        plPedigree[[lCurrentAni$TierId]]$VaterId, psSepChar,
        plPedigree[[lCurrentAni$TierId]]$MutterId, psSepChar,
        plPedigree[[lCurrentAni$TierId]]$Geburtsdatum,"\n",
        file = psOutfile,
        append = TRUE)
  }
  return(invisible(TRUE))
}
