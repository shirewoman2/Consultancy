#' All possible compound IDs, suffixes, etc., including ADC-related compounds
#'
#' @format a data.frame 
"AllCompounds"


#' All regular compound IDs, suffixes, etc.
#'
#' @format a data.frame 
"AllRegCompounds"



#' All possible experimental details for \code{\link{extractExpDetails}}
#'
#' INTERNAL USE: All possible experimental details that can be extracted from a
#' simulator output file using \code{\link{extractExpDetails}}. This version
#' includes columns used only internally with coding. For the more user-friendly
#' version, see \code{\link{ExpDetailDefinitions}}.
#'
#' @format A data.frame with columns: \describe{
#'
#'   \item{Detail}{the experimental detail name to use with
#'   \code{\link{extractExpDetails}} for the argument \code{exp_details}}
#'
#'   \item{Notes}{an explanation of what the experimental detail is}
#'
#'   \item{ReportTableText}{Similar to Notes except more formally written and
#'   includes no coding info; info on that detail that will go in the report table}
#'
#'   \item{Regex_row}{When searching the specified column, what regular
#'   expression should be used for finding the correct row.}
#'
#'   \item{Class}{Data class}
#'
#'   \item{DataSource}{Where did this info come from? simulator output sheet
#'   this detail is extracted from or whether it's from the workspace or
#'   database file or calculated}
#'
#'   \item{NameCol}{the index of the column in the simulator output tab that will
#'   be searched for matches to the text in the column "Regex_row".}
#'
#'   \item{ValueCol}{index of the column in the simulator output tab that
#'   contains the value used for this detail. This applies when the column
#'   doesn't move, e.g., \emph{not} Input Sheet compound-specific details.}
#'
#'   \item{ColsChangeWithCmpd}{TRUE or FALSE for whether NameCol and/or ValueCol
#'   change depending on what compound ID it is}
#'
#'   \item{CmpdsForWhichDeetAvail}{Compounds for which this detail is available.
#'   Many compound-specific details are available for all compounds (entry here
#'   is "all"), but some are only available for the substrate (entry here would
#'   be "substrate") or only for the substrate and inhibitor 1
#'   ("substrate, inhibitor 1").}
#'
#'   \item{CDSInputMatch}{compound data sheet item on
#'   the "Simcyp Inputs and QC" tab that matches this experimental detail}
#'
#'   \item{SimulatorSection}{For matching with the CDS, in which
#'   section does this detail belong? Options: Absorption, Distribution,
#'   Elimination, Interaction, Phys Chem and Blood Binding.}
#'
#'   \item{ADAMParameter}{TRUE or FALSE for whether the
#'   parameter only comes into play when it's an ADAM model}
#'
#'   \item{SimulatorAvailability}{Is this detail available for Simcyp Discovery
#'   simulations? Options are "Simulator only", "Discovery only", or "Simulator
#'   and Discovery". This is used to determine which details to check and which
#'   regex to use for those details.}
#'
#'   \item{OffsetRows}{When there just isn't good regex in the
#'   specific row for this detail, we need to look for a value and then go down
#'   this number of rows to get to the actual value we want.}
#'
#'   \item{SortOrder}{Order in which this detail should show
#'   up in the output from \code{\link{annotateDetails}}}
#'
#'   \item{Level1, Level2, etc.}{the level in the XML file
#'   where the information lives}
#'
#'   \item{XMLswitch}{If there is a switch involved in the
#'   XML file, what is the tag to use for checking on that switch}
#'
#'   \item{SwitchTo}{If the XML switch is turned to "1" or "true"
#'   in the XML file, this is the tag to switch to for looking up the value for
#'   this detail.}
#'
#'   \item{CodingNotes}{Notes specifically to coders}
#'
#'   \item{SimcypParameterType}{For extracting data from database files or
#'   workspaces, what kind of parameter is this? Options are "compound",
#'   "general", "population", or "other" and will affect where to find the info
#'   needed}
#'
#'   \item{SimcypTag}{tag used for identifying this parameter in the database or
#'   workspace file}
#'
#'   \item{SimcypSubcategory}{when a subcategory applies in the Simcyp package,
#'   what is it? Not currently used, but I wanted a placeholder for better,
#'   more seamless data extraction with the Simcyp package.}
#'
#'   \item{CompoundID}{CompoundID (a.k.a. compound position in the Simulator).
#'   This only applies for siutations when that detail is ONLY for a specific
#'   CompoundID, which sometimes happens with database or workspace info in
#'   particular.}
#'
#'   \item{Suffix}{suffix to append for this detail when it's for a specific
#'   compound ID}
#'
#'   }
"AllExpDetails"



#' All possible PK parameters for \code{\link{extractPK}} (version for coding
#' purposes)
#'
#' All possible PK parameters that can be extracted from a simulator output file
#' using \code{\link{extractPK}}. For the more user-friendly version of this
#' data.frame, which omits columns used only for coding purposes, please see
#' \code{\link{PKParameterDefinitions}}.
#'
#' @format A data.frame with 6 columns: \describe{\item{PKparameter}{the PK
#'   parameter name to use with \code{\link{extractPK}} for the argument
#'   \code{PKparameters}}
#'
#'   \item{BasePKparameter}{PKparameter without any mention of whether it was
#'   in the presence of a perpetrator}
#'
#'   \item{PKparameter_nodosenum}{PKparameter without any mention of the dose
#'   number. This is for matching up prettified names when the user has
#'   requested that no dose number be shown, e.g., when all the doses were for
#'   dose 1, it's redundant to see "Dose 1" on all of the table columns. This is
#'   DISTINCT from PK parameters that are specifically for user-defined AUC
#'   intervals. See column "UserInterval". }
#'
#'   \item{Sheet}{the sheet in a simulator output file
#'   where the PK parameter will be extracted, if possible}
#'   \item{AppliesToSingleDose}{TRUE or FALSE for whether this item applies only
#'   to single-dose data}
#'
#'   \item{SearchText}{Regex to use for finding the parameter}
#'
#'   \item{AUCtab_StartColText}{For the older AUC tabs, you had to look at one
#'   row above the main row to figure out which sets of PK values were for
#'   baseline, which were for the DDI, which were in plasma, which were in
#'   blood, etc., so this addresses at least the DDI part of the question.}
#'
#'   \item{AppliesToSingleDose}{TRUE or FALSE for whether this applies to
#'   scenarios where only a single dose was administered. Note that all
#'   UserInterval parameters have this set to true b/c you could conceivably
#'   have a situation where someone had a user-defined interval that was
#'   shorter than the full simulation.}
#'
#'   \item{AppliesOnlyWhenPerpPresent}{TRUE or FALSE
#'   for whether this item only applies when a perpetrator is present in the
#'   simulation}
#'
#'   \item{UserInterval}{Applies specifically to user-defined interval data}
#'
#'   \item{SwitchWhenInhib}{Sometimes, the regex changes from what is used for
#'   the substrate when the compound of interest is a perpetrator. This notes
#'   what the regex changes to in that situation.}
#'
#'   \item{SortOrder}{the order to arrange columns for pk_table}
#'   
#'   \item{PrettifiedNames}{Pretty names to use in tables, etc.}
#'   
#'   \item{PrettifiedNames_nodosenum}{Pretty names to use in tables, etc., when
#'   the user doesn't want the dose number included.}
#'
#'   \item{Notes}{an explanation of what
#'   the parameter is}
#'
#'   }
"AllPKParameters"



#' All names for statistics used in the package
#'
#' INTERNAL USE: We needed something to make sure we were being consistent with
#' how columns were named in various situations and for how stats were described
#' in reports. This does that. \code{\link{renameStats}} calls on this.
#'
#' @format A data.frame with columns: \describe{ \item{InternalColNames}{how a
#'   stat is referred to internal to the package, which generally needs to be
#'   compatible with standard R column-naming rules.}
#'   
#'   \item{SimulatorNames}{how the Simulator names those stats}
#'   
#'   \item{ReportNames}{how the stat should show up in a report}}
"AllStats"


#' All possible tissues and tissue subtypes
#' 
#' @format a data.frame
"AllTissues"



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


#' Plasma concentration-time data for a simulation of letermovir
#'
#' Plasma concentration-time data after 8 days of 60 mg PO QD letermovir,
#' including arithmetic-mean observed data for the last dose. Format of the data
#' are the same as the output from \code{\link{extractConcTime}}.
#'
#' @format A data.frame
"LMVct_mean"


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



#' Report input forms to be used in conjunction with
#' \code{\link{getSectionInfo}} and \code{\link{pk_table}}
#'
#' @format A named list of the data that fill the tabs for
#'   "generateReportInputForm"
"ReportInputForm"

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
#'   to single-dose data} \item{AppliesOnlyWhenPerpPresent}{TRUE or FALSE
#'   for whether this item only applies when a perpetrator is present in the
#'   simulation} \item{SortOrder}{the order to arrange columns for pk_table and
#'   pk_table} \item{Notes}{an explanation of what the parameter is}}
"PKParameterDefinitions"


#' Formatted PK parameter labels for use with graphs
#'
#' A list of named PK expressions with special characters or subscripting for
#' labeling graphs. For example, if you want the y axis of your graph to be
#' "Cmax (ng/mL)" but have the "max" be a subscript, use, e.g., \code{MyGraph +
#' ylab(PKexpressions[["Cmax"]])}. Make sure to use the double brackets when
#' referring to these. Also, there are myriad options for units, so if you'd
#' like, say, mg/mL instead of ng/mL, you can always call on PKexpressions to
#' just give you an example of how you might set things up. Run
#' \code{PKexpressions[["Cmax"]]} and see where you can swap out the "ng/mL". A
#' few tips: \itemize{\item{You can put things in quotes to make them be just
#' normal text.} \item{Subscript with brackets. Superscript with ^.}
#' \item{Indicate Greek characters by spelling them out.} \item{Indicate a space
#' with "~".} \item{See examples with \code{demo(plotmath)}} }
#' @format A list of named expressions
"PKexpressions"




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


#' All possible experimental details for \code{\link{extractExpDetails}}
#'
#' All possible experimental details that can be extracted from a Simcyp
#' Discovery output file using \code{\link{extractExpDetails}} or
#' \code{\link{extractExpDetails_mult}}. For the version that includes
#' additional columns only used for coding purposes, see
#' \code{\link{AllExpDetails}}.
#'
#' @format A data.frame with the following columns: \describe{
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
#'   \item{Notes}{an explanation of what the experimental detail is}
#'
#'   \item{Sheet}{Which simulator output sheet this detail is extracted from}
#'
#'   }
"ExpDetailDefinitions_Discovery"

#' The names of the items that should be in the list output from running
#' \code{\link{extractExpDetails}} or \code{\link{extractExpDetails_mult}}
#'
#' INTERNAL USE. 
#'
#' @format a character vector 
"ExpDetailListItems"


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
#'   \item{Perpetrator}{whether a perpetrator were present; listed as "none" or
#'   "inhibitor"} 
#'   
#'   }
#'   
"ObsDVoptions"



#' Example output from \code{extractForestData}
#'
#' Example output from running \code{extractForestData} on some simulations with
#' bufuralol as the substrate and then 4 different perpetrators. All PK
#' parameters in any forest data will be for the victim compound listed. The
#' data.frame also includes columns for all the compounds that were included for
#' the simulation, but all PK are for the victim compound listed.
#'
#' @format a data.frame
"BufForestData"


#' Example output from \code{extractForestData}
#'
#' Example output from running \code{extractForestData} on some simulations with
#' bufuralol dosed at 20 mg as the substrate and then 4 different perpetrators.
#' All PK parameters in any forest data will be for the victim compound listed.
#' The data.frame also includes columns for all the compounds that were included
#' for the simulation, but all PK are for the victim compound listed.
#'
#' @format a data.frame
"BufForestData_20mg"


#' Example observed PK data for working with \code{forest_plot}
#'
#' Fake observed bufuralol PK data for showing examples with the forest_plot
#' function. 
#'
#' @format a data.frame 
"BufObsForestData"



#' Example simulated vs. observed PK data for use with the so_graph function
#'
#' @format a data.frame 
"SOdata"


#' Equations used for calculating drug-metabolizing enzyme and drug transport
#' ontogenies in pediatric populations
#' 
#' @format a data.frame
"OntogenyEquations"


#' Examples for specifying PK parameters with pk_table
#'
#' @format a data.frame
"PKexamples"


#' Examples for specifying PK parameters with calc_PK_ratios and
#' calc_PK_ratios_mult
#'
#' @format a data.frame
"PKexamples_ratios"


#' Column names for observed concentration-time data by Simulator version
#' 
#' @format a data.frame
"ObsColNames"


#' Examples for setting up data for use with the VBE_safe_space_plot function
#'
#' @format a data.frame
"VBE_disso_example"



