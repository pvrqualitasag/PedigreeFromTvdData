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
#' @return tbl_transform_ped of pedigree records not fullfilling requirements
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
  ### CHECKS & TRANSFORMATION ####################################### ###
  ### ############################################################### ###
  ### Properties of a Directed Acyclic Graphs (DAG)
  ### ############################################################### ###
  tbl_transform_ped <- tbl_ped
  l_tvd_id_col_dsch <- getTvdIdColsDsch()
  n_bd_col_idx <- getBirthdateColIdxDsch()
   ### # Uniqueness of individuals
  tbl_result_checks <- check_unique_animal_id(ptbl_pedigree = tbl_transform_ped)
  if (nrow(tbl_result_checks) > 0) {
    tbl_transform_ped <- transform_unique_animal_id(ptbl_pedigree = tbl_transform_ped,
                               output_check = tbl_result_checks,
                               pn_ani_id_col_idx = l_tvd_id_col_dsch$TierIdCol)
  }

  ### # No cycles -> no
  ### # In-degree of nodes -> no

  ### # Parents older than offspring
  ### # 1) Mother
  tbl_result_checks <- check_parent_older_offspring(ptbl_pedigree = tbl_transform_ped,
                                                       pn_offspring_col = l_tvd_id_col_dsch$TierIdCol,
                                                       pn_birthday_col = n_bd_col_idx,
                                                       pn_parent_col = l_tvd_id_col_dsch$MutterIdCol)

  if (nrow(tbl_result_checks) > 0) {
    tbl_transform_ped <- transform_check_parent_older_offspring(ptbl_pedigree = tbl_transform_ped,
                                           output_check = tbl_result_checks)
  }
  ### # 2) Father
  tbl_result_checks <- check_parent_older_offspring(ptbl_pedigree = tbl_transform_ped,
                                                    pn_offspring_col = l_tvd_id_col_dsch$TierIdCol,
                                                    pn_birthday_col = n_bd_col_idx,
                                                    pn_parent_col = l_tvd_id_col_dsch$VaterIdCol)

  if (nrow(tbl_result_checks) > 0) {
    tbl_transform_ped <- transform_check_parent_older_offspring(ptbl_pedigree = tbl_transform_ped,
                                                                output_check = tbl_result_checks)
  }

  ### # Parents must have the correct sex
  tbl_result_checks <- check_sex_tbl(ptblPedigree = tbl_transform_ped)

  if (nrow(tbl_result_checks) > 0) {
    tbl_transform_ped <- transform_check_sex_tbl(ptbl_pedigree = tbl_transform_ped,
                                                  output_check = tbl_result_checks)

  }

  ### ############################################################### ###
  ### Properties related to data-processing issues
  ### ############################################################### ###
  ### # correct formats of IDs for individual
  tbl_result_checks <- correct_tvd_format_tbl(p_tbl_ped = tbl_transform_ped,
                                    pnIdCol = l_tvd_id_col_dsch$TierIdCol)
  if (nrow(tbl_result_checks) > 0) {
    tbl_transform_ped <- transform_correct_tvd_format_tbl(ptbl_pedigree = tbl_transform_ped,
                                                           output_check = tbl_result_checks)
  }

  ### # correct formats of IDs for mother
  tbl_result_checks <- correct_tvd_format_tbl(p_tbl_ped = tbl_transform_ped,
                                    pnIdCol = l_tvd_id_col_dsch$MutterIdCol)
  if (nrow(tbl_result_checks) > 0) {
    tbl_transform_ped <- transform_correct_tvd_format_tbl(ptbl_pedigree = tbl_transform_ped,
                                                          output_check = tbl_result_checks)
  }

  ### # correct formats of IDs for father
  tbl_result_checks <- correct_tvd_format_tbl(p_tbl_ped = tbl_transform_ped,
                                    pnIdCol = l_tvd_id_col_dsch$VaterIdCol)
  if (nrow(tbl_result_checks) > 0) {
    tbl_transform_ped <- transform_correct_tvd_format_tbl(ptbl_pedigree = tbl_transform_ped,
                                                          output_check = tbl_result_checks)
  }

  ### # correct formats of birthdates
  tbl_result_checks <- check_birthdate_tbl(ptblPedigree = tbl_transform_ped)

  if (!is.null(tbl_result_checks)) {
    if(nrow(tbl_result_checks) > 0) {
      tbl_transform_ped <- transform_check_birthdate_tbl(ptbl_pedigree = tbl_transform_ped,
                                                          output_check = tbl_result_checks)

    }
  }

  ### # finally return
  return(tbl_transform_ped)
  #Sophie: Vielleicht könnte man ein Pedigree mit der Funktion write.csv() in einem File ausschreiben
  # oder ob wir eine weitere Funktion für das Output schreiben sollten, damit Output in einem bestimmten Format kommt.
}


#' @title Write checked pedigree to an output file
#'
#' @param ps_tvd_file tvd input file
#' @param ps_out_file output file for checked pedigree
#' @param pvec_format vector with file format borders
#' @param pb_out flag to indicate whether output should be written
#' @export write_checkedtransformed_pedgiree_from_tvd
write_checkedtransformed_pedgiree_from_tvd <- function(ps_tvd_file,
                                            ps_out_file = paste0(ps_tvd_file, ".out"),
                                            pvec_format = getK11ColPositionVecFromDsch(),
                                            pb_out = FALSE){

  if (pb_out)
    cat(" *** Starting write_checkedtransformed_pedgiree_from_tvd ...", format(Sys.time(), "%Y%m%d%H%M%S"), "\n")

  ### # build checked pedigree
  tbl_ped_result <- build_check_pedigree_from_tvd(ps_tvd_file = ps_tvd_file, pvec_format = pvec_format)

  ### # memory usage
  if (pb_out)
    cat("Memory usage for pedigree: ", pryr::object_size(tbl_ped_result), "\n")

  ### # write pedigree to output file
  readr::write_csv(x = tbl_ped_result, path = ps_out_file)

  if (pb_out)
    cat(" *** End of write_checkedtransformed_pedgiree_from_tvd ...", format(Sys.time(), "%Y%m%d%H%M%S"), "\n")

  return(invisible(TRUE))

}





