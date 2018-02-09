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
#' @param output_check output of function check_unique_animal_id
#' @param pb_out flag whether debugging output should be written
#' @export transform_unique_animal_id
#' @return tbl_transform_ped of pedigree records not fullfilling requirements
transform_unique_animal_id <- function(ptbl_pedigree,
                                       output_check,
                                       pn_ani_id_col_idx = getTvdIdColsDsch()$TierIdCol,
                                       pb_out = FALSE){

  ### # assign result that will be returned
  tbl_transform_ped <- ptbl_pedigree

  ### # Output number of rows in original pedigree for debuggin
   if (pb_out) {
     cat(" *** Unique ID pedigree transformation on original pedigree with nr records: ",
         nrow(tbl_transform_ped), "\n")
   }

  ### # debugging output with number of duplicate records
  if (pb_out) {
    cat(" *** Records with non unique ids:\n")
    print(output_check)
  }

  ### # do the transformation by retaining only
  ### #  the unique records.
  if (nrow(output_check) > 0) {
    tbl_transform_ped_TEST <- tbl_transform_ped %>%
      group_by(.[[pn_ani_id_col_idx]]) %>%
      summarise(n = n()) %>%
      filter((n == 1))
    names(tbl_transform_ped_TEST) <- c("Animal","n")
    tbl_transform_ped_TEST <- tbl_transform_ped_TEST[,"Animal"]
    tbl_transform_ped <- tbl_transform_ped %>% inner_join(tbl_transform_ped_TEST, by = c("V12" = "Animal"))

  }

  ### # debugging output with number of unique pedigree records after transformation
  if (pb_out){
    cat(" *** Number of records after transformation: ", nrow(tbl_transform_ped))
  }

  ### # return result
  return(tbl_transform_ped)
}



### ######################################################### ###
#' @title Transformation after check if parents are older than their offspring
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @description
#' Given a pedigree in tbl_df format, invalidating birthdate of parent and offspring
#' when parents are older than their offspring.
#'
#' @param ptbl_pedigree pedigree in tbl_df format
#' @param output_check output of function check_unique_animal_id in tbl_df format
#' @param pb_out flag whether debugging output should be written
#' @return tbl_transform_ped of pedigree records not fullfilling requirements
#' @export transform_check_parent_older_offspring
transform_check_parent_older_offspring <- function(ptbl_pedigree,
                                                   output_check,
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
#' @title Transformation after check of sex format using tbl_df pedigree
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @param ptbl_pedigree pedigree in tbl_df format
#' @param output_check output of function check_sex_tbl in tbl_df format
#' @param pb_out flag whether debugging output should be written
#' @export transform_check_sex_tbl
#' @return tbl_transform_ped of pedigree records not fullfilling requirements
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
#' @title Transformation after check Correct Ids format in column pnIdCol using tbl
#'
#'
#'
#' @param ptbl_pedigree input pedigree to be transformed as tibble
#' @param output_check output of function correct_tvd_format_tbl in tbl_df format
#' @param pb_out flag whether debugging output should be written
#' @export transform_correct_tvd_format_tbl
#' @return tbl_transform_ped of pedigree records not fullfilling requirements
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
    cat(" *** Original records shows ids of Mother which after transformation should be invalidated: \n")
    print(ptbl_pedigree %>% inner_join(output_check, by = c("V5" = "TvdID")) %>% select(V5))
    cat(" *** Original records shows ids of Father which after transformation should be invalidated: \n")
    print(ptbl_pedigree %>% inner_join(output_check, by = c("V16" = "TvdID")) %>% select(V16))
  }

  ### # if records are found, do the transformation by invalidating with NA
  ### # the tvdid of animal.
  if (nrow(output_check) > 0) {
    vec_ani_ids <- c(output_check$TvdID)
    for(i in vec_ani_ids){
      if(i %in% tbl_transform_ped$V12){
        ### # Remove Animal Ids
        tbl_transform_ped <- remove_rec(tbl_transform_ped,
                                        pvec_rec_tbr_pk = i)
      }
      if(i %in% tbl_transform_ped$V5){
        ### # Line number of the ids are required for replace
        vec_ani_idx <- which(tbl_transform_ped$V5 == i)
        tbl_transform_ped <- tbl_transform_ped %>% mutate(V5 = replace(V5, vec_ani_idx, NA))
      }
      if(i %in% tbl_transform_ped$V16){
        ### # Line number of the ids are required for replace
        vec_ani_idx <- which(tbl_transform_ped$V16 == i)
        tbl_transform_ped <- tbl_transform_ped %>% mutate(V16 = replace(V16, vec_ani_idx, NA))
      }
    }
  }

  ### # debugging output after transformation
  if (pb_out){
    cat(" *** Searching the deleted records shows: \n")
    print(tbl_transform_ped %>% dplyr::filter(V12 %in% vec_ani_ids))
    cat(" *** Transformated records where tvd is invalidate for Mother: \n")
    print(tbl_transform_ped %>% inner_join(output_check, by = c("V5" = "TvdID")) %>% select(V5))
    cat(" *** Transformated records where tvd is invalidate for Father: \n")
    print(tbl_transform_ped %>% inner_join(output_check, by = c("V16" = "TvdID")) %>% select(V16))
  }

  ### # return result
  return(tbl_transform_ped)

}


### ######################################################## ###
#' @title Removing a series of records
#'
#'
#' @param ptbl_pedigree pedigree in tbl_df format
#' @param pvec_rec_tbr_pk pvec_rec_tbr_pk
#' @param pn_pk_col_idx pn_pk_col_idx
#' @export remove_rec
#' @return tbl_transform_ped of pedigree records not fullfilling requirements
remove_rec <- function(tbl_transform_ped,
                       pvec_rec_tbr_pk,
                       pn_pk_col_idx = getTvdIdColsDsch()$TierIdCol){

  tbl_transform_ped <- tbl_transform_ped %>% dplyr::filter(!.[[pn_pk_col_idx]] %in% pvec_rec_tbr_pk)

  return(tbl_transform_ped)
}


### ######################################################## ###
#' @title Setting one field of a list of records in a pedigree to NA
#'
#' @importFrom dplyr enquo
#' @importFrom dplyr quo_name
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#'
#' @description
#' Changing certain variables or fields (columns in tbl_df) of
#' a given set of records is done by a combination of dplyr::mutate
#' to change the variables and dplyr::if_else() to specify in which
#' rows the variables should be changed. The re-factoring work to
#' integrate this operation into a function is done based on the
#' material available at http://dplyr.tidyverse.org/articles/programming.html
#'
#' @details
#' When previous functions were working with column indices to
#' identify the variables or fields in the tbl_df, this function
#' here switched to the use of column names. For a given pedigree,
#' we have to define utility functions that return the column name
#' for certain record fields.
#'
#' @param ptbl_pedigree pedigree as tbl_df
#' @param pvec_rec_pk vector of primary keys identifying the records
#' @param ps_field_name column name in ptbl_pedigree where certain fields must be set to NA
#' @param ps_pk_name column name where primary keys can be found
#' @return tbl_pedigree_result pedigree with given fields set to NA
#' @examples
#' s_ped_file <- system.file(file.path("extdata","KLDAT_20170524_10000.txt"),
#'                           package = "PedigreeFromTvdData")
#' tbl_ped <- laf_open_fwf_tvd_input(ps_input_file = s_ped_file)
#' vec_rec_tbd_pk <- c("CH120001976905", "CH120006405592", "CH120001807094", "CH120003434748")
#' tbl_ped_vna <- set_field_na(tbl_ped, vec_rec_pk, V5, V12)
#' tbl_ped_vna %>% filter(V12 %in% vec_rec_pk) %>% select(V5,V11,V12,V16)
#' @export set_field_na
set_field_na <- function(ptbl_pedigree,
                         pvec_rec_pk,
                         ps_field_name,
                         ps_pk_name){
  ### # create an expression from the column name of primary keys
  expr <- dplyr::enquo(ps_pk_name)
  ### # create an expression from the column name where the fields
  ### #  are that should be set to NA
  var_expr <- dplyr::enquo(ps_field_name)
  ### # create the variable name which is used for the lhs of the
  ### #  assignement
  var_name <- dplyr::quo_name(var_expr)

  tbl_pedigree_result <-
    dplyr::mutate(ptbl_pedigree,
                  !!var_name := dplyr::if_else((!!expr) %in% pvec_rec_pk,
                                               NA_character_,
                                               !!var_expr))

  return(tbl_pedigree_result)
}


### ######################################################## ###
#' @title Transformation of incorrect birthdate format using tbl_df pedigree
#'
#'
#' @param ptbl_pedigree pedigree in tbl_df format
#' @param output_check output of function correct_tvd_format_tbl in tbl_df format
#' @param pb_out flag whether debugging output should be written
#' @export transform_check_birthdate_tbl
#' @return tbl_transform_ped of pedigree records not fullfilling requirements
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
