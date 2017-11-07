###
###
###
###   Purpose:   Write unique ids for all animals
###   started:   2017/11/07 (skn and pvr)
###
### ######################################### ###


#' Add unique number to each record
#'
#' @param plPedigree original pedigree
#' @return lProcessedPedigree pedigree with unique ids added
#' @export unique_id
unique_id <- function(plPedigree){
  ### # initialize result
  lProcessedPedigree <- plPedigree
  ### # loop over pedigree
  for (idxPed in 1:length(lProcessedPedigree)){
    lProcessedPedigree[[idxPed]]$NumId <- idxPed
  }
  return(lProcessedPedigree)
}
