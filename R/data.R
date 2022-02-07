#' Example induction data for use with the inductFit function
#'
#' @format A data.frame with 24 rows and 3 columns:
#' \describe{
#' \item{DonorID}{Unique ID for the donor}
#' \item{Concentration_uM}{Concentration of the inducer (uM)}
#' \item{FoldInduction}{Fold induction observed compared to control}}
"IndData"

#' Plasma concentration-time data after 8 days of 60 mg PO QD letermovir. Format
#' of the data are the same as the output from \code{\link{extractConcTime}}.
#'
#' @format A data.frame with 14 columns "LMVct"
"LMVct"

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
#' @format A data.frame with 3 columns: \describe{ \item{PKparameter}{the PK
#'   parameter name to use with \code{\link{extractPK}} for the argument
#'   \code{PKparameters}} \item{Sheet}{the sheet in a simulator output file
#'   where the PK parameter will be extracted, if possible}
#'   \item{AppliesToSingleDose}{TRUE or FALSE for whether this item applies only
#'   to single-dose data} \item{AppliesOnlyWhenEffectorPresent}{TRUE or FALSE
#'   for whether this item only applies when an effector is present in the
#'   simulation} \item{Notes}{an explanation of what the parameter is}}
"AllPKParameters"

#' All possible experimental details that can be extracted from a simulator
#' output file using \code{\link{extractExpDetails}}.
#'
#' @format A data.frame with 10 columns: \describe{
#'
#'   \item{Detail}{the experimental detail name to use with
#'   \code{\link{extractExpDetails}} for the argument \code{exp_details}}
#'
#'   \item{Compound}{the specific compound this experimental detail applies to}
#'
#'   \item{Notes}{an explanation of what the experimental detail is}
#'
#'   \item{NameColDetect}{FOR INTERNAL USE}
#'
#'   \item{Class}{Data class}
#'
#'   \item{Sheet}{Which simulator output sheet this detail is extracted from}
#'
#'   \item{NameCol}{Which column in the simulator output tab will be searched
#'   for this detail}
#'
#'   \item{ValueCol}{Which column in the simulator output tab will contains the
#'   value used for this detail}
#'
#'   \item{CDSInputMatch}{FOR INTERNAL USE: Which compound data sheet item on
#'   the "Simcyp Inputs and QC" tab match this experimental detail}
#'
#'   \item{SimulatorSection}{FOR INTERNAL USE: For matching with the CDS, which section does
#'   this detail belong? Options: Absorption, Distribution, Elimination,
#'   Interaction, Phys Chem and Blood Binding.}
#'
#'   }
"AllExpDetails"

