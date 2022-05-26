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


#' Plasma concentration-time data after a single dose of 5 mg MDZ but with
#' varying fa values, one for each of 4 simulator output files. Format of the
#' data are the same as the output from \code{\link{extractConcTime_mult}}.
#'
#' @format A data.frame with 15 columns "MDZct"
"MDZct"


#' Plasma concentration-time data of multiple-dose midazolam plus multiple-dose
#' ketoconazole, including data for both substrate and inhibitor. Format of the
#' data are the same as the output from \code{\link{extractConcTime_mult}}.
#'
#' @format A data.frame with 15 columns "MDZ_Keto"
"MDZ_Keto"




#' Simcyp colors used in the PowerPoint template
#'
#' @format A named character vector of colors
#' @examples
#' scales::show_col(SimcypColors)
"SimcypColors"

#' Paths for Simcyp's large-file store, SharePoint directory, and the specific
#' folder on the large-file store for sharing R-related files
#'
#' @format A two-item list: \describe{ \item{LgFileDir}{the large-file store}
#'   \item{SharePtDir}{the SharePoint directory} \item{RDir}{the folder on the
#'   large-file store for sharing SimcypConsultancy R files}}
"SimcypDir"

#' Report input forms to be used in conjunction with
#' \code{\link{getSectionInfo}} and \code{\link{pksummary_table}}
#'
#' @format A named list of the data that fill the tabs for
#'   "generateReportInputForm"
"ReportInputForm"

#' All possible PK parameters that can be extracted from a simulator output file
#' using \code{\link{extractPK}}. For the more user-friendly version of this
#' data.frame, which omits columns used only for coding purposes, please see
#' \code{\link{PKParameterDefinitions}}. 
#'
#' @format A data.frame with 6 columns: \describe{ \item{PKparameter}{the PK
#'   parameter name to use with \code{\link{extractPK}} for the argument
#'   \code{PKparameters}} \item{Sheet}{the sheet in a simulator output file
#'   where the PK parameter will be extracted, if possible}
#'   \item{AppliesToSingleDose}{TRUE or FALSE for whether this item applies only
#'   to single-dose data} \item{AppliesOnlyWhenEffectorPresent}{TRUE or FALSE
#'   for whether this item only applies when an effector is present in the
#'   simulation} \item{SortOrder}{the order to arrange columns for pksummary_table and
#'   pksummary_table} \item{Notes}{an explanation of what the parameter is}}
"AllPKParameters"



#' All possible PK parameters that can be extracted from a simulator
#' output file using \code{\link{extractPK}}. For the version that
#' includes additional columns only used for coding purposes, see
#' \code{\link{AllPKParameters}}.
#' @format A data.frame with 6 columns: \describe{ \item{PKparameter}{the PK
#'   parameter name to use with \code{\link{extractPK}} for the argument
#'   \code{PKparameters}} \item{Sheet}{the sheet in a simulator output file
#'   where the PK parameter will be extracted, if possible}
#'   \item{AppliesToSingleDose}{TRUE or FALSE for whether this item applies only
#'   to single-dose data} \item{AppliesOnlyWhenEffectorPresent}{TRUE or FALSE
#'   for whether this item only applies when an effector is present in the
#'   simulation} \item{SortOrder}{the order to arrange columns for pksummary_table and
#'   pksummary_table} \item{Notes}{an explanation of what the parameter is}}
"PKParameterDefinitions"


#' All possible experimental details that can be extracted from a simulator
#' output file using \code{\link{extractExpDetails}}. This version includes
#' columns used only internally with coding. For the more user-friendly version,
#' see \code{\link{ExpDetailDefinitions}}.
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
#'   \item{NameColDetect}{FOR INTERNAL USE. When searching the Input Sheet for
#'   the set of columns to use, what regular expression should be used.}
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
#'   \item{SimulatorSection}{FOR INTERNAL USE: For matching with the CDS, which
#'   section does this detail belong? Options: Absorption, Distribution,
#'   Elimination, Interaction, Phys Chem and Blood Binding.}
#'
#'   }
"AllExpDetails"

#' All possible experimental details that can be extracted from a simulator
#' output file using \code{\link{extractExpDetails}}. For the version that
#' includes additional columns only used for coding purposes, see
#' \code{\link{AllExpDetails}}.
#'
#' @format A data.frame with the following columns: \describe{
#'
#'   \item{Detail}{the experimental detail name to use with
#'   \code{\link{extractExpDetails}} for the argument \code{exp_details}}
#'
#'   \item{Compound}{the specific compound this experimental detail applies to}
#'
#'   \item{SimulatorSection}{the part of the simulator that this detail applies
#'   to}
#'   
#'   \item{Notes}{an explanation of what the experimental detail is}
#'
#'   \item{Sheet}{Which simulator output sheet this detail is extracted from}
#'
#'   }
"ExpDetailDefinitions"




#' All possible observed dependent variable options in the Excel file template
#' for converting observed data to an XML file the Simulator can use
#'
#' @format A data.frame with the following columns: \describe{
#'
#'   \item{ID}{the ID used by the Excel template}
#'
#'   \item{Tissue}{the tissue}
#'
#'   \item{CompoundID}{the compound ID that is used in the SimcypConsultancy R
#'   package}
#'
#'   \item{Effector}{whether an effector were present; listed as "none" or
#'   "inhibitor"} 
#'   
#'   }
#'   
"ObsDVoptions"
