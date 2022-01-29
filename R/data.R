#' Example induction data for use with the inductFit function
#'
#' @format A data.frame with 24 rows and 3 columns:
#' \describe{
#' \item{DonorID}{Unique ID for the donor}
#' \item{Concentration_uM}{Concentration of the inducer (uM)}
#' \item{FoldInduction}{Fold induction observed compared to control}}
"IndData"

#' Dummy plasma concentration-time data after a 5 mg PO MDZ dose. Format of the
#' data are the same as the output from \code{\link{extractConcTime}}.
#'
#' @format A data.frame with 2211 rows and 7 columns:
#' \describe{
#' \item{Compound}{the name of the compound}
#' \item{Individual}{a unique ID for each simulated individual}
#' \item{Simulated}{TRUE or FALSE for whether the data were simulated (all TRUE
#' in this dummy example)}
#' \item{Time}{time since dose}
#' \item{Conc}{plasma drug concentration}
#' \item{Time_units}{units for the time column}
#' \item{Conc_units}{units for the concentration column}}
"MDZConcTime"

#' Simcyp colors used in the PowerPoint template
#'
#' @format A named character vector of colors
#' @examples
#' scales::show_col(SimcypColors)
"SimcypColors"

#' Paths for Simcyp's large file directory and the SharePoint directory
#'
#' @format A two-item list:
#' \describe{
#' \item{LgFileDir}{the large-file directory}
#' \item{SharePtDir}{the SharePoint directory}}
"SimcypDir"

#' Report input forms to be used in conjunction with
#' \code{\link{getSectionInfo}} and \code{\link{so_table}}
#'
#' @format A named list of two forms: "Observed data form" for entering
#'   information about a clinical study and "Section input form" for entering
#'   data about a specific simulation file to be described in this section of a
#'   report
"ReportInputForm"

#' All possible PK parameters that can be extracted from a simulator output file
#' using \code{\link{extractPK}}.
#'
#' @format A data.frame 3 columns: \describe{ \item{PKparameter}{the PK
#'   parameter name to use with \code{\link{extractPK}} for the argument
#'   \code{PKparameters}} \item{Sheet}{the sheet in a simulator output file
#'   where the PK parameter will be extracted, if possible}
#'   \item{Notes}{an explanation of what the parameter is}}
"AllPKParameters"
