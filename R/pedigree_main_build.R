###
###
###
###   Purpose:   Read K11-TVD data, build pedigree and run checks
###   started:   2017-12-07 (skn and pvr)
###
### ############################################################### ###


#' Read K11-formatted TVD-data to build a pedigree
#'
#' @param ps_tvd_file file containing K11-formatted TVD-data
#' @param pvec_format vector with column separator positions of pedigree
#' @export build_check_pedigree_from_tvd
build_check_pedigree_from_tvd <- function(ps_tvd_file,
                                    pvec_format = getK11ColPositionVecFromDsch()){
  ### # check whether specified tvd-input file exists
  if (!file.exists(ps_tvd_file)){
    stop("*** ERROR in build_pedigree_from_tvd:\n*** ***** Cannot find TVD input file: ", ps_tvd_file)
  }
  ### # read pedigree from fwf-file into a tbl_df
  tbl_ped <- laf_open_fwf_tvd_input(ps_input_file = ps_tvd_file,
                                    pvec_col_position = pvec_format)
  ### ############################################################### ###
  ### CHECKS ######################################################## ###
  ### ############################################################### ###
  ### Properties of a Directed Acyclic Graphs (DAG)
  ### ############################################################### ###
  ### # Uniqueness of individuals
  tbl_result_checks <- check_unique_animal_id(ptbl_pedigree = tbl_ped)

  ### # No cycles -> no

  ### # In-degree of nodes -> no

  ### # Parents older than offspring
  tbl_result_checks <- check_parent_older_offspring (ptbl_pedigree = tbl_ped,
                                             pn_offspring_col,
                                             pn_birthday_col,
                                             pn_parent_col,
                                             pn_date_diff_tol = 10^4)

  ### # Parents must have the correct sex
  tbl_result_checks <- check_sex_tbl(ptblPedigree = tbl_ped)
  ### ############################################################### ###
  ### Properties related to data-processing issues
  ### ############################################################### ###
  ### # correct formats of IDs for individual
  tbl_result_checks <- correct_tvd_format_tbl(p_tbl_ped = tbl_ped,
                                    pnIdCol = plIdCols$TierIdCol)

  ### # correct formats of IDs for mother
  tbl_result_checks <- correct_tvd_format_tbl(p_tbl_ped = tbl_ped,
                                    pnIdCol = plIdCols$MutterIdCol)

  ### # correct formats of IDs for father
  tbl_result_checks <- correct_tvd_format_tbl(p_tbl_ped = tbl_ped,
                                    pnIdCol = plIdCols$VaterIdCol)


  ### # correct formats of birthdates
  tbl_result_checks <- check_birthdate_tbl(ptblPedigree = tbl_ped)

  ### # finally return
  return(tbl_result_checks)
}

### ############################################################### ###
### TRANSFORMATION ################################################ ###
### ############################################################### ###

