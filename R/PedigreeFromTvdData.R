#' PedigreeFromTvdData: Import pedigree from K11-formatted TVD-data.
#'
#' Package PedigreeFromTvdData is used to construct a pedigree from
#' TVD-input files. TVD-input files are in a fixed-format according
#' to the specification from the interface definition called
#' "Datenschnittstelle Rindvieh". The package PedigreeFromTvdData
#' provides three categories of functions: data-import,
#' consistency-checking, pedigree-data-output.
#'
#' @section Input Data Format:
#' The input data used in this package to extract the required
#' pedigree information is a fixed-width format such
#' as defined in the K11-format of the DSCH Rindvieh.ch. The definition
#' of DSCH is given by an xlsx-spreadsheet which was adapted to the
#' format of the input data used here. The vector of column widths
#' is obtained by calling the function \code{getK11ColPositionVecFromDsch()}.
#'
#' @section Data Import:
#' The data import using the a line-by-line input strategy and saving
#' the data in nested lists did take too long. Hence, we compared different
#' input functions. See the vignette entitled "Reading Fixed Width Formatted Data"
#' for the comparison results. In the current version the data is read
#' using the function laf_open_fwf_tvd_input() which is based on the package LaF.
#' This function takes as input the name of the input file. Optionally,
#' the field borders can be specified as a function argument. The last
#' argument is a flag that determines whether debugging output is
#' written to the console or not.
#'
#' @section Consistency Checks:
#' Three types of consistency checks can be done, once the pedigree is read.
#' \enumerate{
#' \item Format of TVD-Ids for animals and parents
#' \item Format and values of birthdates
#' \item Sex of parents
#' }
#'
#' @section Data Output:
#' The data output format is defined as ...
#'
#' @docType package
#' @name PedigreeFromTvdData
NULL
