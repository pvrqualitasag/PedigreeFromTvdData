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
#' For a given pedigree, we first test whether there are any duplicate records,
#' using the corresponding check-function for uniqueness of IDs. If
#' duplicate records are found, we run a series of dplyr::group_by - dplyr::summarise -
#' dplyr::filter - operations to retain just the unique records.
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
  if (pb_out) {
    cat(" *** Records with non unique ids:\n")
    print(tbl_non_uni_dup_id)
    # Sophie: Logfile mit alle Transformation oder pro Funktion in Transformation???
    # Peter: Logfile kann erstellt werden durch Umleitung des Outputs in eine Datei
  }

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



### ######################################################### ###
###
###
#' @title Transformation after check if parents are older than their offspring
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @description
#' Given a pedigree in tbl_df format, all parents that are also
#' present as animals are invalidated, if they are older than
#' their offspring.
#'
#' @param ptbl_pedigree pedigree in tbl_df format
#' @param pn_offspring_col column index for offspring
#' @param pn_birthday_col column index for birthdates of offspring
#' @param pn_parent_col column index for parents
#' @param pn_date_diff_tol minimum difference between birthdates of parents and offspring
#' @return tbl_df of pedigree records not fullfilling requirements
#' @export transform_check_parent_older_offspring
transform_check_parent_older_offspring <- function(ptbl_pedigree,
                                                   output_check,
                                                   pb_out = FALSE) {

  ### # assign result that will be returned
  tbl_transform_ped <- ptbl_pedigree

  ### # Output number of rows in original pedigree for debuggin
  if (pb_out) {
    cat(" *** Original pedigree with nr records: ",
        nrow(tbl_transform_ped), "\n")
  }

  ### # debugging output if parent are younger than offspring
  if (pb_out) {
    cat(" *** Records with to small difference of age: \n")
    print(output_check)
  }

  ### # if records are found, do the transformation by invalidating with NA
  ### # the birthdates of animal and parents.
  if (nrow(output_check) > 0) {

  }






}

