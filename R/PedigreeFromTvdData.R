#' PedigreeFromTvdData: Importing pedigree data from TVD-input-files.
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
#' information for #' the construction of a pedigree is a fixed format.
#' The input data is organised in a file consists of a list of records.
#' Each record takes up one line of the input file. Records can be split
#' into fields and each field takes a specific number of characters. The
#' number of characters for each field is defined by a constant list of
#' border values. This list is obtained by the function
#' \code{getFormatBorder()}.
#'
#' @section Data Import:
#' The data import is implemented in function \code{read_tvd_input()}.
#' This function takes as input the name of the input file. Optionally,
#' the field borders can be specified as a function argument. The last
#' argument is a flag that determines whether debugging output is
#' written to the console or not.
#'
#' @section Consistency Checks:
#' A pedigree is a directed acyclic graph which implies that the parent
#' offspring relationship must not contain any loops. Furthermore,
#' parents must be older than offspring and the sex of parents must
#' be consistent according to their assignment.
#'
#' @section Data Output:
#' The data output format is defined as ...
#'
#' @docType package
#' @name PedigreeFromTvdData
NULL
