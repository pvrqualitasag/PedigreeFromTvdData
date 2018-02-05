###
###
###
###   Purpose:   Scripts for transformation based on
###              consistency checks
###   started:   2018-02-05 (skn and pvr)
###
### ######################################################## ###


#' @title Transformation after heck for uniqueness of Animal-IDs in a pedigree
#'
#' @export transform_unique_animal_id
transform_unique_animal_id <- function(output_check,
                                       ptbl_pedigree,
                                       pn_ani_id_col_idx = getTvdIdColsDsch()$TierIdCol,
                                       pb_out = FALSE){

    if (pb_out)
    cat(" *** Non-unique Ids found: ", ptbl_pedigree %>% filter(V12==output_check$Animal))

  tbl_transform_ped <- ptbl_pedigree %>%
    group_by(.[[pn_ani_id_col_idx]]) %>%
    summarise(n = n()) %>%
    filter(!(n > 1))

  if (pb_out)
    cat(" *** Transformation duplicate Ids, none should be found : ", tbl_transform_ped %>% filter(V12==output_check$Animal))

  ### # return result
  return(tbl_transform_ped)
}
