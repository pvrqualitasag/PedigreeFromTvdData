###
###
###
###   Purpose:   Output Pedigree from TVD-Data
###   started:   2017-11-07 (skn and pvr)
###
### ######################################### ###


#' @export output
output <- function(plPedigree){
  ### # initialize result
  Output <- plPedigree
  for(idxPed in 1:length(Output)){
    lCurrentAni <- Output[[idxPed]]
    cat(Output[[lCurrentAni$TierId]]$NumId,"\t", Output[[lCurrentAni$VaterId]]$NumId,"\t", Output[[lCurrentAni$MutterId]]$NumId,"\t", Output[[lCurrentAni$TierId]]$TierId,"\t", Output[[lCurrentAni$TierId]]$Geburtsdatum,"\n")
  }
}
