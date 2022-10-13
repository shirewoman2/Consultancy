#' Example induction data for use with the inductFit function
#'
#' @format A data.frame with 24 rows and 3 columns:
#' \describe{
#' \item{DonorID}{Unique ID for the donor}
#' \item{Concentration_uM}{Concentration of the inducer (uM)}
#' \item{FoldInduction}{Fold induction observed compared to control}}
"IndData"

#' Plasma concentration-time data for a simulation of letermovir
#'
#' Plasma concentration-time data after 8 days of 60 mg PO QD letermovir,
#' including observed data for the last dose. Format of the data are the same as
#' the output from \code{\link{extractConcTime}}.
#'
#' @format A data.frame 
"LMVct"


#' Plasma concentration-time data for multiple simulations of midazolam
#' 
#' Plasma concentration-time data after a single dose of 5 mg MDZ but with
#' varying fa values, one for each of 4 simulator output files. Format of the
#' data are the same as the output from \code{\link{extractConcTime_mult}}.
#'
#' @format A data.frame 
"MDZct"


#' Plasma concentration-time data for midazolam and ketoconazole
#'
#' Plasma concentration-time data of multiple-dose midazolam plus multiple-dose
#' ketoconazole, including data for both substrate and inhibitor and observed
#' data. Format of the data are the same as the output from
#' \code{\link{extractConcTime_mult}}.
#'
#' @format A data.frame
"MDZ_Keto"


#' CYP3A4 enzyme-abundance data in the liver
#'
#' Enzyem-abundance levels for liver CYP3A4 in the presence and absence of
#' multiple-dose ritonavir. Format of the data are the same as the output from
#' \code{\link{extractEnzAbund}}.
#'
#' @format A data.frame 
"CYP3A4_liver"



#' CYP3A4 enzyme-abundance data in the gut
#'
#' Enzyem-abundance levels for CYP3A4 in the colon and in the small intestine in
#' the presence and absence of multiple-dose ritonavir. Format of the data are
#' the same as the output from \code{\link{extractEnzAbund}}.
#'
#' @format A data.frame 
"CYP3A4_gut"



#' Simcyp colors used in the PowerPoint template
#'
#' @format A named character vector of colors
#' @examples
#' scales::show_col(SimcypColors)
"SimcypColors"

#' Simcyp paths
#'
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

#' All possible PK parameters for \code{\link{extractPK}} (version for coding
#' purposes)
#'
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
#'   simulation} \item{SortOrder}{the order to arrange columns for
#'   pksummary_table and pksummary_table} \item{Notes}{an explanation of what
#'   the parameter is}}
"AllPKParameters"



#' All possible PK parameters for \code{\link{extractPK}}
#' 
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


#' Useful, preformatted PK parameter labels for using on graphs
#'
#' PK expressions with special characters or subscripting for labeling graphs.
#' For example, if you want the y axis of your graph to be "Cmax (ng/mL)" but
#' have the "max" be a subscript, use, e.g., \code{MyGraph +
#' ylab(PKexpressions[["Cmax"]])}. Make sure to use the double brackets when
#' referring to these. Also, there are myriad options for units, so if you'd
#' like, say, mg/mL instead of ng/mL, you can always call on PKexpressions to
#' just give you an example of how you might set things up. Run
#' \code{PKexpressions[["Cmax"]]} and see where you can swap out the "ng/mL".
#' @format A list of named expressions
"PKexpressions"



#' All possible experimental details for \code{\link{extractExpDetails}}
#' (version for coding purposes)
#'
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
#'   \item{CompoundID}{the specific compound ID this experimental detail applies
#'   to, e.g., "substrate", "inhibitor 1", etc.}
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

#' All possible experimental details for \code{\link{extractExpDetails}}
#'
#' All possible experimental details that can be extracted from a simulator
#' output file using \code{\link{extractExpDetails}} or
#' \code{\link{extractExpDetails_mult}}. For the version that includes additional
#' columns only used for coding purposes, see \code{\link{AllExpDetails}}.
#'
#'@format A data.frame with the following columns: \describe{
#'
#'  \item{Detail}{the experimental detail name to use with
#'  \code{\link{extractExpDetails}} for the argument \code{exp_details}}
#'
#'  \item{Compound ID}{the specific compound this experimental detail applies
#'  to, e.g., "substrate", "inhibitor 1", etc.}
#'
#'  \item{SimulatorSection}{the part of the simulator that this detail applies
#'  to}
#'
#'  \item{Notes}{an explanation of what the experimental detail is}
#'
#'  \item{Sheet}{Which simulator output sheet this detail is extracted from}
#'
#'  }
"ExpDetailDefinitions"


#' Example output from \code{extractExpDetails_mult}
#'
#' Example output from running \code{extractExpDetails_mult} on some midazolam
#' simulations
#'
#' @format a data.frame with rows for each of 4 simulator files and columns for
#'   each simulation experimental detail   
"MDZdetails"


#' Observed data DV options
#' 
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



#' Example output from \code{extractForestData}
#'
#' Example output from running \code{extractForestData} on some simulations with
#' bufuralol as the substrate and then 4 different effectors.
#'
#' @format a data.frame with rows for each of 4 simulator files and the columns
#'   "File", "Substrate", "Dose_sub", "Inhibitor1", "Dose_inhib",
#'   "AUCt_ratio_dose1__GMR", "AUCt_ratio_dose1__CI90_lo",
#'   "AUCt_ratio_dose1__CI90_hi", "Cmax_ratio_dose1__GMR",
#'   "Cmax_ratio_dose1__CI90_lo", and "Cmax_ratio_dose1__CI90_hi"
"ForestData"


