###
###
###
###   Purpose:   Read K11-TVD data, build pedigree and run checks
###   started:   2017-12-07 (skn and pvr)
###
### ############################################################### ###


#' Read K11-formatted TVD-data to construct a pedigree
#'
#'
build_pedigree_from_tvd <- function(ps_tvd_file,
                                    ps_out_file = paste(ps_tvd_file, "out", sep = "."),
                                    pvec_format){
  if (!file.exists(ps_tvd_file)){
    stop("*** ERROR in build_pedigree_from_tvd:\n*** ***** Cannot find TVD input file: ", ps_tvd_file)
  }
  ### # read pedigree from fwf-file into a tbl_df
  tbl_ped <- laf_open_fwf_tvd_input(ps_input_file = ps_tvd_file,
                                    pvec_col_position = pvec_format)

}
