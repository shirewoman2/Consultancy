#' Example induction data for use with the inductFit function
#'
#' @format A data.frame with 24 rows and 3 columns:
#' \describe{
#' \item{DonorID}{Unique ID for the donor}
#' \item{Concentration_uM}{Concentration of the inducer (uM)}
#' \item{FoldInduction}{Fold induction observed compared to control}}
"IndData"

#' Dummy plasma concentration-time data after a 5 mg PO MDZ dose. Format of the
#' data are the same as the output from \code{extractConcTime}.
#'
#' @format A data.frame with 2211 rows and 7 columns:
#' \describe{
#' \item{Compound}{the name of the compound}
#' \item{ID}{a unique ID for each subject}
#' \item{Simulated}{TRUE or FALSE for whether the data were simulated (all TRUE
#' in this dummy example)}
#' \item{Time}{time since dose}
#' \item{Conc}{plasma drug concentration}
#' \item{Time_units}{units for the time column}
#' \item{Conc_units}{units for the concentration column}}
"ConcTime"



