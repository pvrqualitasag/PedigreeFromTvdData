% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pedigree_main_build.R
\name{write_checkedtransformed_pedgiree_from_tvd}
\alias{write_checkedtransformed_pedgiree_from_tvd}
\title{Write checked pedigree to an output file}
\usage{
write_checkedtransformed_pedgiree_from_tvd(ps_tvd_file,
  ps_out_file = paste0(ps_tvd_file, ".out"),
  pvec_format = getK11ColPositionVecFromDsch(), pb_out = FALSE)
}
\arguments{
\item{ps_tvd_file}{tvd input file}

\item{ps_out_file}{output file for checked pedigree}

\item{pvec_format}{vector with file format borders}

\item{pb_out}{flag to indicate whether output should be written}
}
