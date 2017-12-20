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
#' @param ps_out_file output file for validated pedigree
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

  ### # running the checks, first IDs
  tbl_ped <- check_tvd_id_tbl(ptblPedigree = tbl_ped)

  ### # checking for birthdate
  tbl_ped <- check_birthdate_tbl(ptblPedigree = tbl_ped)

  ### # more checks can be done here

  ### # finally return
  return(tbl_ped)
}
