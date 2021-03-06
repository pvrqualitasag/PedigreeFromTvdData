% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pedigree_transformation_tbl_df.R
\name{remove_rec}
\alias{remove_rec}
\title{Removing a series of records}
\usage{
remove_rec(tbl_transform_ped, pvec_rec_tbr_pk,
  pn_pk_col_idx = getTvdIdColsDsch()$TierIdCol)
}
\arguments{
\item{pvec_rec_tbr_pk}{pvec_rec_tbr_pk}

\item{pn_pk_col_idx}{pn_pk_col_idx}

\item{ptbl_pedigree}{pedigree in tbl_df format}
}
\value{
tbl_transform_ped of pedigree records not fullfilling requirements
}
