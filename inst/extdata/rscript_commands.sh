

Rscript -e 'PedigreeFromTvdData::build_check_pedigree_from_tvd(ps_tvd_file = "/Library/Frameworks/R.framework/Versions/3.4/Resources/library/PedigreeFromTvdData/extdata/KLDAT_20170524_AllFehler.txt",
ps_out_file = "ped.out")'

Rscript -e 'PedigreeFromTvdData::write_checked_pedgiree_from_tvd(ps_tvd_file = "/Library/Frameworks/R.framework/Versions/3.4/Resources/library/PedigreeFromTvdData/extdata/KLDAT_20170524_AllFehler.txt",
ps_out_file = "ped.out", pb_out = TRUE)'

Rscript -e 'tbl_ped <- PedigreeFromTvdData::build_check_pedigree_from_tvd(ps_tvd_file = "/Library/Frameworks/R.framework/Versions/3.4/Resources/library/PedigreeFromTvdData/extdata/KLDAT_20170524_AllFehler.txt",
ps_out_file = "ped.out");tbl_rrtdm <- read_rrtdm(...);merge(tbl_ped,tbl_rrtdm)'