###
###
###
###   Purpose:   Write unique ids for all animals
###   started:   2017/11/07 (skn and pvr)
###
### ######################################### ###



#' @export unique_id
unique_id <- function(plPedigree){
  ### # initialize result
  lProcessedPedigree <- plPedigree
  ### # loop over pedigree
  for (idxPed in 1:length(lProcessedPedigree)){
    lCurrentAni <- lProcessedPedigree[[idxPed]]
    lProcessedPedigree[[lCurrentAni]]$NumId <- idxPed
    lProcessedPedigree[[idxPed]] <- lCurrentAni
  }
  return(lProcessedPedigree)
}
