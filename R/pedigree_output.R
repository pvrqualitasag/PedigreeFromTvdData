###
###
###
###   Purpose:   Output Pedigree from TVD-Data
###   started:   2017-11-07 (skn and pvr)
###
### ######################################### ###


#' @export output
output <- function(plPedigree, psOutfile = "ped_from_tvd.dat", psSepChar = "\t"){
  ### # initialize result
  Output <- plPedigree
  for(idxPed in 1:length(Output)){
    lCurrentAni <- Output[[idxPed]]
    cat(Output[[lCurrentAni$TierId]]$NumId,psSepChar, Output[[lCurrentAni$VaterId]]$NumId,psSepChar, Output[[lCurrentAni$MutterId]]$NumId,psSepChar, Output[[lCurrentAni$TierId]]$TierId,psSepChar, Output[[lCurrentAni$TierId]]$Geburtsdatum,"\n", file = psOutfile)
  }
  return(invisible(TRUE))
}
