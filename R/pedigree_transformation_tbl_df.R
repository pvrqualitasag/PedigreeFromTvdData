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
#' A given pedigree is transformed such that the resulting pedigree
#' has only unique animal-IDs. The pedigree-records , we first test whether there are any duplicate records,
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

  ### # debugging output with number of duplicate records
  if (pb_out) {
    ### # check whether we have to do a transformation using the check function
    tbl_non_uni_dup_id <- check_unique_animal_id(ptbl_pedigree = tbl_transform_ped)
    cat(" *** Records with non unique ids:\n")
    print(tbl_non_uni_dup_id)
    # Sophie: Logfile mit alle Transformation oder pro Funktion in Transformation???
    # Peter: Logfile kann erstellt werden durch Umleitung des Outputs in eine Datei
  }

  ### # do the transformation by retaining only
  ### #  the unique records.
  tbl_transform_ped <- tbl_transform_ped %>%
      group_by(.[[pn_ani_id_col_idx]]) %>%
      summarise(n = n()) %>%
      filter((n == 1))
      #filter(!(n > 1)) #Sophie -> Alle Non-Unique Ids löschen, keine behalten ????

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
#' Given a pedigree in tbl_df format, invalidating birthdate of parent and offspring
#' where parents are older than their offspring.
#'
#' @param ptbl_pedigree pedigree in tbl_df format
#' @param output_check output of function check_unique_animal_id in tbl_df format
#' @param pb_keep_dup should one record of those with duplicate ids be kept
#' @param pb_out logfile production with TRUE
#' @return tbl_transform_ped of pedigree records not fullfilling requirements
#' @export transform_check_parent_older_offspring
transform_check_parent_older_offspring <- function(ptbl_pedigree,
                                                   output_check,
                                                   pb_keep_dup = FALSE,
                                                   pb_out = FALSE) {

  ### # assign result that will be returned
  tbl_transform_ped <- ptbl_pedigree

  ### # debugging output if parent are younger than offspring
  if (pb_out) {
    cat(" *** Records with to small difference of age: \n")
    print(output_check)
    cat(" *** Original records shows birthdates of Animal which after transformation should be invalidated: \n")
    print(ptbl_pedigree %>% inner_join(output_check, by = c("V12" = "Animal")) %>% select(V12,V11))
    cat(" *** Original records shows birthdates of Parent which after transformation should be invalidated: \n")
    print(ptbl_pedigree %>% inner_join(output_check, by = c("V12" = "Parent")) %>% select(V12,V11))
  }

  ### # if records are found, do the transformation by invalidating with NA
  ### # the birthdates of animal and parents.
  if (nrow(output_check) > 0) {
    vec_ani_ids <- c(output_check$Animal, output_check$Parent)
    ### # Line number of the ids are required for replace
    vec_ani_idx <- sapply(vec_ani_ids, function(x) which(ptbl_pedigree$V12 == x), USE.NAMES = FALSE)
    tbl_transform_ped <- ptbl_pedigree %>% mutate(V11 = replace(V11, vec_ani_idx, NA))
  }

  ### # debugging output after transformation
  if (pb_out){
    cat(" *** Transformated records where birthdate is invalidate for Animal: \n")
    print(tbl_transform_ped %>% inner_join(output_check, by = c("V12" = "Animal")) %>% select(V12,V11))
    cat(" *** Transformated records where birthdate is invalidate for Parent: \n")
    print(tbl_transform_ped %>% inner_join(output_check, by = c("V12" = "Parent")) %>% select(V12,V11))
  }

  ### # return result
  return(tbl_transform_ped)

}


### ######################################################## ###
#' Transformation after check of sex format using tbl_df pedigree
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @param ptbl_pedigree pedigree in tbl_df format
#' @param output_check output of function check_sex_tbl in tbl_df format
#' @export transform_check_sex_tbl
transform_check_sex_tbl <- function(ptbl_pedigree,
                                    output_check,
                                    pb_out = FALSE){

  ### # assign result that will be returned
  tbl_transform_ped <- ptbl_pedigree

  ### # debugging output if parent are younger than offspring
  if (pb_out) {
    cat(" *** Records with sex inconsistency: \n")
    print(output_check)
    cat(" *** Original records shows birthdates of Animal which after transformation should be invalidated: \n")
    print(ptbl_pedigree %>% inner_join(output_check, by = c("V12" = "Animal")) %>% select(V12,V14))
  }

  ### # if records are found, do the transformation by invalidating with NA
  ### # the sex of animal.
  if (nrow(output_check) > 0) {
    vec_ani_ids <- c(output_check$Animal)
    ### # Line number of the ids are required for replace
    vec_ani_idx <- sapply(vec_ani_ids, function(x) which(ptbl_pedigree$V12 == x), USE.NAMES = FALSE)
    tbl_transform_ped <- ptbl_pedigree %>% mutate(V14 = replace(V14, vec_ani_idx, NA))
  }

  ### # debugging output after transformation
  if (pb_out){
    cat(" *** Transformated records where sex is invalidate for Animal: \n")
    print(tbl_transform_ped %>% inner_join(output_check, by = c("V12" = "Animal")) %>% select(V12,V14))
  }

  ### # return result
  return(tbl_transform_ped)

}



### ######################################################## ###
#' Transformation after check Correct Ids format in column pnIdCol using tbl
#'
#'
#'
#' @param ptbl_pedigree input pedigree to be transformed as tibble
#' @param output_check output of function correct_tvd_format_tbl in tbl_df format
#' @export transform_correct_tvd_format_tbl
transform_correct_tvd_format_tbl <- function(ptbl_pedigree,
                                             output_check,
                                             pb_out = FALSE){
  ### # assign result that will be returned
  tbl_transform_ped <- ptbl_pedigree

  ### # debugging output if ID format is incorrect
  if (pb_out) {
    cat(" *** Records with ids incorrectness: \n")
    print(output_check)
    cat(" *** Original records shows ids of Animal which after transformation should be deleted: \n")
    print(ptbl_pedigree %>% inner_join(output_check, by = c("V12" = "TvdID")) %>% select(V12,V16,V5))
  }

  ### # if records are found, do the transformation by invalidating with NA
  ### # the tvdid of animal.
  if (nrow(output_check) > 0) {
    vec_ani_ids <- c(output_check$TvdID)
    ### # Line number of the ids are required for replace
    vec_ani_idx <- sapply(vec_ani_ids, function(x) which(ptbl_pedigree$V12 == x), USE.NAMES = FALSE)
    tbl_transform_ped <- ptbl_pedigree %>% mutate(V12 = replace(V12, vec_ani_idx, NA))
  }

  ### # debugging output after transformation
  if (pb_out){
    cat(" *** Transformated records where tvd is invalidate for Animal: \n")
    print(tbl_transform_ped %>% inner_join(output_check, by = c("V12" = "TvdID")) %>% select(V12,V16,V5))
    #Sophie: Wie kann man die Transformation in diesem Fall kontrollieren?
  }

  ### # return result
  return(tbl_transform_ped)

}


### ######################################################## ###
#' Transformation of incorrect birthdate format using tbl_df pedigree
#'
#'
#' @param ptblPedigree pedigree in tbl_df format
#' @param lLimitValue list with fixed limits for year, month and date
#' @param pnBirthdateColIdx column index of birthdates in ptblPedigree
#' @return validated and modified tbl_df pedigree
#' @export transform_check_birthdate_tbl
transform_check_birthdate_tbl <- function(ptbl_pedigree,
                                          output_check,
                                          pb_out = FALSE){

  ### # assign result that will be returned
  tbl_transform_ped <- ptbl_pedigree

  ### # debugging output if birthdate format is incorrect
  if (pb_out) {
    cat(" *** Records with bithdate incorrectness: \n")
    print(output_check)
  }

  ### # if records are found, do the transformation by invalidating with NA
  ### # the bithdate of animal.
  if (!is.null(output_check)) {
    if(nrow(output_check) > 0) {
      vec_ani_ids <- c(output_check$TvdId)
      ### # Line number of the ids are required for replace
      vec_ani_idx <- sapply(vec_ani_ids, function(x) which(ptbl_pedigree$V12 == x), USE.NAMES = FALSE)
      tbl_transform_ped <- ptbl_pedigree %>% mutate(V11 = replace(V11, vec_ani_idx, NA))
    }
  }


  ### # debugging output after transformation
  if (pb_out){
    cat(" *** Transformated records where birthdate is invalidate for Animal: \n")
    print(tbl_transform_ped %>% inner_join(output_check, by = c("V12" = "TvdId")) %>% select(V12,V11))
  }

  ### # return result
  return(tbl_transform_ped)

}
