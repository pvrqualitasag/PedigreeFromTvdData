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
#' @param psSepChar  parameter by default for delimiter symbol
#' @export output
output <- function(plPedigree, psSepChar = "\t"){
  ### # initialize result
  for(idxPed in 1:length(plPedigree)){
    lCurrentAni <- plPedigree[[idxPed]]
    cat(plPedigree[[lCurrentAni$TierId]]$NumId,psSepChar, plPedigree[[lCurrentAni$VaterId]]$NumId,psSepChar, plPedigree[[lCurrentAni$MutterId]]$NumId,psSepChar, plPedigree[[lCurrentAni$TierId]]$TierId,psSepChar, plPedigree[[lCurrentAni$TierId]]$Geburtsdatum,"\n")
  }
  return(invisible(TRUE))
}
