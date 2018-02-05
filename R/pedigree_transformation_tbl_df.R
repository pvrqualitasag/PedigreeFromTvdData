###
###
###
###   Purpose:   Scripts for transformation based on
###              consistency checks
###   started:   2018-02-05 (skn and pvr)
###
### ######################################################## ###


#' @title Transformation after check for uniqueness of Animal-IDs in a pedigree
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#'
#' @description
#' For a given pedigree, we first test whether there are any duplicate records. If
#' duplicate records are found, we run a dplyr::filter to retain just the unique
#' records.
#'
#' @param ptbl_pedigree pedigree in tbl_df format
#' @param pn_ani_id_col_idx column index for animal-ID in pedigree
#' @param pb_out flag whether debugging output should be written
#' @export transform_unique_animal_id
transform_unique_animal_id <- function(ptbl_pedigree,
                                       pn_ani_id_col_idx = getTvdIdColsDsch()$TierIdCol,
                                       pb_out = FALSE){

  ### # assign result that will be returned
  tbl_transform_ped <- ptbl_pedigree

  ### # Output number of rows in original pedigree for debuggin
   if (pb_out) {
     cat(" *** Unique ID pedigree transformation on original pedigree with nr records: ",
         nrow(tbl_transform_ped), "\n")
     # cat(" *** Non-unique Ids found: ", ptbl_pedigree %>% filter(V12==output_check$Animal))
   }

  ### # check whether we have to do a transformation using the check function
  tbl_non_uni_dup_id <- check_unique_animal_id(ptbl_pedigree = tbl_transform_ped)
  ### # debugging output with number of duplicate records
  if (pb_out)
    cat(" *** Number of records with non unique ids found: ", nrow(tbl_non_uni_dup_id), "\n") # Sophie: Logfile mit alle Transformation oder pro Funktion in Transformation???
  ### # if duplicate records are found, do the transformation by retaining only
  ### #  the unique records.
  if (nrow(tbl_non_uni_dup_id) > 0) {
    tbl_transform_ped <- tbl_transform_ped %>%
      group_by(.[[pn_ani_id_col_idx]]) %>%
      summarise(n = n()) %>%
      filter((n == 1))
      #filter(!(n > 1)) #Sophie -> Alle Non-Unique Ids löschen, keine behalten ????

  }

  ### # debugging output with number of unique pedigree records after transformation
  if (pb_out){
    cat(" *** Number of records after transformation: ", nrow(tbl_transform_ped))
    # cat(" *** Transformation duplicate Ids, none should be found : ", tbl_transform_ped %>% filter(V12==output_check$Animal))
  }

  ### # return result
  return(tbl_transform_ped)
}
