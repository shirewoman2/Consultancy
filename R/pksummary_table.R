#' Make summary PK tables for reports
#'
#' \code{pksummary_table} creates tables of PK parameters for reports and
#' presentations, including reporting means, CVs, and confidence intervals or
#' percentiles and, optionally, comparisons to observed data. This function
#' automatically finds the correct tabs and the correct cells in a Simulator
#' output Excel file to obtain those data. \strong{Notes:} \itemize{\item{Please
#' see the notes at the bottom of this help file for how to supply observed data
#' in a standardized fashion that this function can read.} \item{If you would
#' like to make a single PK table for multiple files at once, please see the
#' function \code{\link{pksummary_mult}}.} \item{You can specify which compound
#' (substrate, inhibitor 1, etc.) and which tissue (plasma, blood, or unbound
#' versions of each) you want to get the PK data for.}} For detailed
#' instructions and examples, please see the SharePoint file "Simcyp PBPKConsult
#' R Files - Simcyp PBPKConsult R Files/SimcypConsultancy function examples and
#' instructions/Making PK tables/PK-tables.docx". (Sorry, we are unable to
#' include a link to it here.)
#'
#' Because we need to have a standardized way to input observed data, setting up
#' the input for this function requires creating a data.frame or named vector of
#' the observed PK data, supplying a csv or Excel file with observed PK data, or
#' filling out an Excel form.
#'
#' \strong{OPTION A: Supply a data.frame or a named vector.} If you supply a
#' data.frame, the column names will indicate which PK parameter you want, and
#' if you supply a named numeric vector, the names of the vector will perform
#' the same task. If you have CV values for any observed data that you'd like to
#' include in the table, make the name be the PK parameter with a suffix of
#' "_CV".
#'
#' An example of specifying a data.frame: \code{observed_PK =
#' data.frame(AUCinf_dose1 = 60, AUCinf_dose1_CV = 0.38, Cmax_dose1 = 22,
#' Cmax_dose1_CV = 0.24)}
#'
#' An example of specifying a named vector: \code{observed_PK = c("AUCinf_dose1"
#' = 60, "AUCinf_dose1_CV" = 0.38, "Cmax_dose1" = 22, "Cmax_dose1_CV" = 0.24)}.
#'
#' \strong{OPTION B: Use a csv file of observed PK data.} In Excel, create a csv
#' file where the first row is the PK parameters you want and the second row
#' lists the values for each. This should look the same as the examples for
#' Option A. To see an example of how this should look, run this in the console
#' and then open the csv file:
#'
#' \code{write.csv(data.frame(AUCinf_dose1 = 60, AUCinf_dose1_CV = 0.38,
#' Cmax_dose1 = 22, Cmax_dose1_CV = 0.24), file = "Example observed PK
#' values.csv", row.names = FALSE)}
#'
#' When you call on \code{pksummary_table}, use the following syntax,
#' substituting your file name for the example: \code{observed_PK = "Example
#' observed PK values.csv"}
#'
#' \strong{OPTION C: Use a tab named "observed PK" in the compound data sheet
#' for your project.} Just as with the other examples, the first row lists the
#' PK parameters you want, and subsequent rows list the values for those
#' parameters. Since this will also work for looking at PK for other possible
#' simulations, please include the column "File" and list the name of the
#' simulation output file that you want to compare. It's ok to leave blank any
#' cells where you don't have a value, and it's ok to change the PK parameter
#' names to something else that you want as long as it's one of the options for
#' "PKparameter" in the table you can see by running this in the console:
#' \code{view(PKParameterDefinitions)}. Note that this will require that a copy
#' of the compound data sheet exists somewhere that R can read it, i.e., not on
#' a SharePoint folder with no path in Windows.
#'
#' When you call on \code{pksummary_table}, use the following syntax,
#' substituting your project's compound data sheet file name for the example:
#' \code{observed_PK = paste0(SimcypDir$SharePtDir, "abc-1a/Research/abc-1a
#' compound data sheet.xlsx")}  (This assumes that the compound data sheet lives
#' on the SharePoint drive in the "Research" folder for you project. Change
#' "abc-1a" and the file name to whatever you need for your project.)
#'
#' \strong{OPTION D: Fill out an Excel form.} Here are the steps to take for
#' this option: \enumerate{
#'
#' \item{Use the function \code{\link{generateReportInputForm}} to create an
#' Excel file where you can enter information about your project. Example:
#' \code{generateReportInputForm("My report input form.xlsx")}}
#'
#' \item{Go to the tab "study info - DDI" or "study info - no DDI", whichever is
#' appropriate for your situation. Under the heading "Simulated data", enter the
#' name of the specific simulator output Excel file you want to compare.}
#'
#' \item{Under the heading "Observed data" on that same tab, enter details about
#' your observed data. It's ok if you don't have all the information; anything
#' that's missing won't be included in the final S/O table. It's also ok to
#' rename this tab or make copies of it within the same Excel file for making
#' other S/O tables.}
#'
#' \item{Save the report input form.}
#'
#' \item{Back in RStudio, run this function using the file name of that Excel
#' report form as input for \code{report_input_file} and the name of the "study
#' info - DDI/no DDI" tab as the input for \code{sheet_report}. Note: If the
#' Excel file lives on SharePoint, you'll need to close it or this function will
#' just keep running and not generate any output while it waits for access to
#' the file.} }
#'
#'
#' @param sim_data_file a simulator output file. If you supplied a file name in
#'   a data.frame of observed PK or a csv or Excel file of observed PK for
#'   \code{observed_PK}, that file name will be used preferentially and you can
#'   leave this blank. Similarly, if you supply a filled-out report input form
#'   to the argument \code{report_input_file}, the file name you supplied
#'   \emph{there} will be used preferentially, and you can leave this blank.
#' @param report_input_file (optional) This argument is an alternative way to
#'   specify both what simulator Excel file to use and also what the observed PK
#'   parameters were. Input is the name of the Excel file created by running
#'   \code{\link{generateReportInputForm}}, which you have now filled out,
#'   including the path if it's in any other directory than the current one.
#'   Please see the "Details" section at the bottom for more information on this
#'   option.
#' @param sheet_report the sheet in the Excel report file that contains
#'   information about the study, e.g., "study info - DDI" or "study info - no
#'   DDI" if you haven't renamed the tab. This only applies if you have supplied
#'   an Excel file name for \code{report_input_file}. If you're supplying a
#'   simulator output Excel file for \code{sim_data_file}, ignore this.
#' @param PKparameters (optional) the PK parameters to include as a character
#'   vector. \itemize{
#'
#'   \item{By default, if you have a single-dose simulation, the parameters will
#'   include AUC and Cmax for dose 1, and, if you have a multiple-dose
#'   simulation, AUC and Cmax for the last dose. Also by default, if you have an
#'   perpetrator present, the parameters will include the AUC and Cmax values with
#'   and without the perpetrator as well as those ratios.}
#'
#'   \item{Alternatively, you can specify a vector of any combination of
#'   specific, individual parameters, e.g., \code{c("Cmax_dose1",
#'   "AUCtau_last").} Be sure to encapsulate the parameters you want with
#'   \code{c(...)}. To see the full set of possible parameters to extract, enter
#'   \code{view(PKParameterDefinitions)} into the console.}
#'
#'   \item{If you would like PK pulled from a specific custom interval, please
#'   supply a named character vector where the names are the PK parameters and the
#'   values are the tabs. Example: \code{sheet_PKparameters = c("AUCinf_dose1" =
#'   NA, "AUCt" = "Int AUC userT(1)(Sub)(CPlasma)", "AUCtau_last" = NA)}
#'   \strong{Please note that we would like the PK parameters that are for either dose 1 or the
#'   last dose to have NA listed for the tab.} Another note: The code will
#'   work best if any PK parameters for a custom interval do not have
#'   a suffix indicating the dose number. Good: "AUCt". Bad: "AUCtau_last".
#'   This is because we do not know
#'   which dose number a custom interval is. This also helps use make sure that
#'   each PK parameter has only one value so that it's clear which PK data are
#'   being described. It is ok to supply this named
#'   character vector to the argument \code{sheet_PKparameters} instead, but please do
#'   not supply it to both.}
#'
#'   \item{If you supply observed data using either the argument
#'   \code{report_input_file} or the argument \code{observed_PK}, those PK
#'   parameters will be included automatically.}
#'
#'   \item{Parameters that don't make sense for your scenario -- such as asking
#'   for \code{AUCinf_dose1_withInhib} when your simulation did not include an
#'   inhibitor or perpetrator -- will not be included.}
#'
#'   \item{tmax will be listed as median, minimum, and maximum rather than mean,
#'   lower and higher 90\% confidence interval or 5th to 95th percentiles.
#'   Similarly, if you request trial means, the values for tmax will be the
#'   range of medians for the trials rather than the range of means.}}
#'
#'   An example of acceptable input here: \code{PKparameters = c("AUCtau_last",
#'   "AUCtau_last_withInhib", "Cmax_last", "Cmax_last_withInhib",
#'   "AUCtau_ratio_last", "Cmax_ratio_last")}.
#' @param PKorder Would you like the order of the PK parameters to be the order
#'   specified in the Consultancy Report Template (default), or would you like
#'   the order to match the order you specified with the argument
#'   \code{PKparameters}? Options are "default" or "user specified".
#' @param sheet_PKparameters (optional) If you want the PK parameters to be
#'   pulled from a specific tab in the simulator output file, list that tab
#'   here. Otherwise, this should be left as NA. If you want some parameters
#'   from a custom-interval tab and others from the regular tabs, please supply
#'   a named character vector where the names are the PK parameters and the
#'   values are the tabs. Example: \code{sheet_PKparameters = c("AUCinf_dose1" =
#'   NA, "AUCt" = "Int AUC userT(1)(Sub)(CPlasma)", "AUCtau_last" = NA)}
#'   \itemize{\item{\strong{Please note that we would like the PK parameters that are for either dose 1 or the
#'   last dose to have NA listed for the tab.}} \item{Another note: The code will
#'   work best if any PK parameters for a custom interval do not have
#'   a suffix indicating the dose number. Good: "AUCt". Bad: "AUCtau_last".
#'   This is because we do not know
#'   which dose number a custom interval is. This also helps use make sure that
#'   each PK parameter has only one value so that it's clear which PK data are
#'   being described.}
#'   \item{It is ok to supply this named
#'   character vector to the argument \code{PKparameters} instead, but please do
#'   not supply it to both.}}
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default), "unbound plasma", "blood", or "unbound
#'   blood".
#' @param compoundToExtract For which compound do you want to extract PK data?
#'   Options are: \itemize{\item{"substrate" (default),} \item{"primary
#'   metabolite 1",} \item{"primary metabolite 2",} \item{"secondary
#'   metabolite",} \item{"inhibitor 1" -- this can be an inducer, inhibitor,
#'   activator, or suppresesor, but it's labeled as "Inhibitor 1" in the
#'   simulator,} \item{"inhibitor 2" for the 2nd inhibitor listed in the
#'   simulation,} \item{"inhibitor 1 metabolite" for the primary metabolite of
#'   inhibitor 1}}
#' @param observed_PK (optional) If you have a data.frame, a named numeric
#'   vector, or an Excel or csv file with observed PK parameters, supply the
#'   full file name in quotes or the data.frame or vector here, and the
#'   simulated-to-observed mean ratios will be calculated. If you supply an
#'   Excel file, it \emph{must} have a tab titled "observed PK", and that's what
#'   will be read. The supplied data.frame or file must include columns for each
#'   of the PK parameters you would like to compare, and those column names
#'   \emph{must} be among the PK parameter options listed in
#'   \code{PKParameterDefinitions}. If you would like the output table to
#'   include the observed data CV for any of the parameters, add "_CV" to the
#'   end of the parameter name, e.g., "AUCinf_dose1_CV". Please see the
#'   "Example" section of this help file for examples of how to set this up. 
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#' @param mean_type What kind of means and CVs do you want listed in the output
#'   table? Options are "arithmetic" or "geometric" (default). If you supplied a
#'   report input form, only specify this if you'd like to override the value
#'   listed there.
#' @param includeTrialMeans TRUE or FALSE (default) for whether to include the
#'   range of trial means for a given parameter. Note: This is calculated from
#'   individual values rather than being pulled directly from the output.
#' @param includeCV TRUE (default) or FALSE for whether to include rows for CV
#'   in the table
#' @param includeSD TRUE or FALSE (default) for whether to include rows for the
#'   standard deviation in the table
#' @param includeMedian TRUE or FALSE (default) for whether to include rows for
#'   the median in the table
#' @param includeConfInt TRUE (default) or FALSE for whether to include whatever
#'   confidence intervals were included in the simulator output file. Note that
#'   the confidence intervals are geometric since that's what the simulator
#'   outputs (see an AUC tab and the summary statistics; these values are the
#'   ones for, e.g., "90\% confidence interval around the geometric mean(lower
#'   limit)").
#' @param includePerc TRUE or FALSE (default) for whether to include the 5th to
#'   95th percentiles
#' @param includeRange TRUE or FALSE (default) for whether to include the
#'   minimum and maximum values
#' @param concatVariability TRUE or FALSE (default) for whether to concatenate
#'   the variability. If "TRUE", the output will be formatted into a single row
#'   and listed as the lower confidence interval or percentile to the upper CI
#'   or percentile, e.g., "2400 to 2700". Please note that the current
#'   SimcypConsultancy template lists one row for each of the upper and lower
#'   values, so this should be set to FALSE for official reports.
#' @param variability_format formatting used to indicate the variability When
#'   the variability is concatenated. Options are "to" (default) to get output
#'   like "X to Y", "hyphen" to get output like "X - Y", "brackets" to get
#'   output like "[X, Y]", or "parentheses" for the eponymous symbol if you're
#'   an American and a bracket if you're British, e.g., "(X, Y)". (Sorry for the
#'   ambiguity; this was written by an American who didn't originally realize
#'   that there was another name for parentheses.)
#' @param include_dose_num NA (default), TRUE, or FALSE on whether to include
#'   the dose number when listing the PK parameter. By default, the parameter
#'   will be labeled, e.g., "Dose 1 Cmax ratio" or "Last dose AUCtau ratio", if
#'   you have PK data for both the first dose and the last dose. Also by
#'   default, if you have data only for the first dose or only for the last
#'   dose, the dose number will be omitted and it will be labeled, e.g., "AUCtau
#'   ratio" or "Cmax ratio". Set this to TRUE or FALSE as desired to override
#'   the default behavior and get exactly what you want.
#' @param rounding option for what rounding to perform, if any. Options are:
#'   \describe{\item{NA or "Consultancy"}{All output will be rounded according
#'   to Simcyp Consultancy Team standards: to three significant figures when the
#'   value is < 100 or to the ones place if the value is >= 100. Please see the
#'   function \code{\link{round_consultancy}}, which does the rounding here.}
#'   \item{"none"}{No rounding will be performed.} \item{"significant X" where
#'   "X" is a number}{Output will be rounded to X significant figures. "signif
#'   X" also works fine.} \item{"round X" where "X" is a number}{Output will be
#'   rounded to X digits} \item{"Word only"}{Output saved to Word or a csv file
#'   will be rounded using the function \code{\link{round_consultancy}}, but
#'   nothing will be rounded in the output R object. This can be useful when you
#'   want to have nicely rounded and formatted output in a Word file but you
#'   \emph{also} want to use the results from \code{pksummary_table} to make
#'   forest plots, which requires numbers that are \emph{not} rounded.}}
#' @param adjust_conc_units Would you like to adjust the units to something
#'   other than what was used in the simulation? Default is NA to leave the
#'   units as is, but if you set the concentration units to something else, this
#'   will attempt to adjust the units to match that. This only adjusts only the
#'   simulated values, since we're assuming that that's the most likely problem
#'   and that observed units are relatively easy to fix, and it also only
#'   affects AUC and Cmax values. Acceptable input is any concentration unit
#'   listed in the Excel form for PE data entry, e.g. \code{adjust_conc_units =
#'   "ng/mL"} or \code{adjust_conc_units = "uM"}. Molar concentrations will be
#'   automatically converted using the molecular weight of whatever you set for
#'   \code{compoundToExtract}.
#' @param prettify_columns TRUE (default) or FALSE for whether to make easily
#'   human-readable column names. TRUE makes pretty column names such as "Dose 1
#'   AUCinf (h*ng/mL)" whereas FALSE leaves the column with the R-friendly name
#'   from \code{\link{extractPK}}, e.g., "AUCinf_dose1".
#' @param prettify_compound_names TRUE (default) or FALSE on whether to make
#'   compound names prettier in the prettified column titles and in any Word
#'   output files. This was designed for simulations where the substrate and any
#'   metabolites, perpetrators, or perpetrator metabolites are among the
#'   standard
#'   options for the simulator, and leaving \code{prettify_compound_names =
#'   TRUE} will make the name of those compounds something more human readable.
#'   For example, "SV-Rifampicin-MD" will become "rifampicin", and
#'   "Sim-Midazolam" will become "midazolam". Set each compound to the name
#'   you'd prefer to see in your column titles if you would like something
#'   different. For example, \code{prettify_compound_names = c("perpetrator" =
#'   "teeswiftavir", "substrate" = "superstatin")}. Please note that "perpetrator"
#'   includes \emph{all} the perpetrators and perpetrator metabolites present,
#'   so, if you're setting the perpetrator name, you really should use something
#'   like this
#'   if you're including perpetrator metabolites: \code{prettify_compound_names =
#'   c("perpetrator" = "teeswiftavir and 1-OH-teeswiftavir", "substrate" =
#'   "superstatin")}.
#' @param extract_forest_data TRUE or FALSE (default) to get forest-plot data at
#'   the same time. This only applies when the compound to extract is the
#'   substrate or a substrate metabolite. If set to TRUE, this will return a
#'   list that includes data formatted for use with the function
#'   \code{\link{forest_plot}}. Since the \code{\link{forest_plot}} function
#'   only works with simulations with perpetrators (at least, for now), this
#'   will only work for simulations that included a perpetrator. This is
#'   probably most useful for the \code{\link{pksummary_mult}} function since a
#'   forest plot with only one simulation isn't terribly informative.
#' @param checkDataSource TRUE (default) or FALSE for whether to include in the
#'   output a data.frame that lists exactly where the data were pulled from the
#'   simulator output file. Useful for QCing.
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.docx" or "My table.csv", depending on whether you'd prefer to have
#'   the main PK table saved as a Word or csv file.  Do not include any slashes,
#'   dollar signs, or periods in the file name. (If you assign the output of
#'   \code{pksummary_table} to an R object, you can also save the table later to
#'   a Word file with the function \code{\link{formatTable_Simcyp}}.) If you
#'   supply only the file extension, e.g., \code{save_table = "docx"}, the name
#'   of the file will be the file name plus "PK summary table" with that
#'   extension, and the output file will be located in the same folder as
#'   \code{sim_data_file}. If you supply something other than just "docx" or
#'   just "csv" for the file name but you leave off the file extension, we'll
#'   assume you want it to be ".csv". While the main PK table data will be in
#'   whatever file format you requested, if you set \code{checkDataSource =
#'   TRUE}, the QC data will be in a csv file on its own and will have "- QC"
#'   added to the end of the file name.
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#' @param highlight_gmr_colors optionally specify a set of colors to use for
#'   highlighting geometric mean ratios for DDIs. Options are "yellow to red",
#'   "green to red" or a vector of 4 colors of your choosing. If left as NA, no
#'   highlighting for GMR level will be done.
#' @param highlight_so_cutoffs optionally specify cutoffs for highlighting any
#'   simulated-to-observed ratios. Anything that is above those values or below
#'   the inverse of those values will be highlighted. To figure out what cells
#'   to highlight, this looks for a column titled "Statistic" or "Stat", then
#'   looks for what row contains "S/O" or "simulated (something something)
#'   observed" (as in, we'll use some wildcards to try to match your specific
#'   text). Next, it looks for any values in that same row that are above those
#'   cutoffs. This overrides anything else you specified for highlighting. The
#'   default is NA, for \emph{not} highlighting based on S/O value. Acceptable
#'   input for, say, highlighting values that are > 125\% or < 80\% of the
#'   observed and also, with a second color, values that are > 150\% or < 66\%
#'   would be: \code{highlight_so_cutoffs = c(1.25, 1.5)}. If you would like the
#'   middle range of values to be highlighted, include 1 in your cutoffs. For
#'   example, say you would like everything that's < 80\% or > 125\% to be
#'   highlighted red but you'd like the "good" values from 80\% to 125\% to be
#'   green, you can get that by specifying
#'   \code{highlight_so_cutoffs = c(1, 1.25)} and \code{highlight_so_colors =
#'   c("green", "red")}. This only applies when you save the table as a Word file.
#' @param highlight_so_colors optionally specify a set of colors to use in the
#'   Word file output for highlighting S/O values outside the limits you
#'   specified with \code{highlight_so_cutoffs}. Options: \describe{
#'
#'   \item{"yellow to red" (default)}{A range of light yellow to light orange to
#'   light red. If you have included 1 in your cutoffs and you leave
#'   \code{highlight_so_colors} with the default setting, values in the middle,
#'   "good" range of S/O values will be highlighted a light green.}
#'
#'   \item{"traffic"}{light green, yellow, and red designed to display values
#'   outside 1.25, 1.5, and 2 fold of unity, respectively. If you include 1 in
#'   \code{highlight_so_cutoffs}, you'll get a darker green for "good" S/O
#'   values. This color scheme was borrowed from Lisa, so if you've seen her
#'   slides, these will look familiar.}
#'
#'   \item{a character vector of specific colors}{Any R-acceptable colors, will
#'   work here, e.g., \code{highlight_so_colors = c("yellow", "orange", "red")}}
#'   If you do specify your own bespoke colors, you'll need to make sure that
#'   you supply one color for every value in \code{highlight_so_cutoffs}.}
#' @param return_PK_pulled TRUE or FALSE (default) for whether to return as a
#'   list item what PK parameters were pulled. This is used internally for
#'   writing table headings later.
#'
#' @return Returns a data.frame of PK summary data and, if observed data were
#'   provided, simulated-to-observed ratios. If \code{checkDataSource = TRUE},
#'   output will instead be a list of that data.frame (named "Table") and
#'   information on where the values came from for QCing (named "QC").
#' @export
#' @examples
#' pksummary_table("abc1a-5mg-qd.xlsx")
#'
#' pksummary_table(report_input_file = "My report input - project abc-1a.xlsx",
#'          sheet_report = "study info - Clinical study 001A",
#'          includeTrialMeans = TRUE)
#'
#' # An example of how to format observed data as a data.frame:
#' pksummary_table(sim_data_file = "My simulated data.xlsx",
#'                 observed_PK = data.frame(AUCinf_dose1 = 60,
#'                                          AUCinf_dose1_CV = 0.38,
#'                                          Cmax_dose1 = 22,
#'                                          Cmax_dose1_CV = 0.24))
#'
#' # Or you can supply a named numeric vector:
#' pksummary_table(sim_data_file = "My simulated data.xlsx",
#'                 observed_PK = c("AUCinf_dose1" = 60,
#'                                 "AUCinf_dose1_CV" = 0.38,
#'                                 "Cmax_dose1" = 22,
#'                                 "Cmax_dose1_CV" = 0.24))
#'
#' # Or an Excel or csv file:
#' pksummary_table(sim_data_file = "mdz-5mg-sd.xlsx",
#'                 observed_PK = "mdz observed PK.csv")
#' 

pksummary_table <- function(sim_data_file = NA, 
                            compoundToExtract = "substrate",
                            tissue = "plasma", 
                            PKparameters = NA,
                            PKorder = "default", 
                            sheet_PKparameters = NA,
                            observed_PK = NA, 
                            existing_exp_details = NA,
                            report_input_file = NA,
                            sheet_report = NA,
                            mean_type = NA,
                            includeCV = TRUE,
                            includeSD = FALSE,
                            includeConfInt = TRUE,
                            includeMedian = FALSE, 
                            includeRange = FALSE,
                            includePerc = FALSE,
                            includeTrialMeans = FALSE,
                            concatVariability = FALSE,
                            variability_format = "to",
                            adjust_conc_units = NA,
                            include_dose_num = NA,
                            rounding = NA,
                            prettify_columns = TRUE,
                            prettify_compound_names = TRUE, 
                            extract_forest_data = FALSE, 
                            checkDataSource = TRUE, 
                            return_PK_pulled = FALSE, 
                            highlight_gmr_colors = NA,
                            highlight_so_cutoffs = NA, 
                            highlight_so_colors = "yellow to red",
                            save_table = NA, 
                            fontsize = 11){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   sim_data_file <- ifelse(str_detect(sim_data_file, "xlsx$"), 
                           sim_data_file, paste0(sim_data_file, ".xlsx"))
   
   # Check for appropriate input for arguments
   tissue <- tolower(tissue)
   if(tissue %in% c("plasma", "unbound plasma", "blood", "unbound blood") == FALSE){
      warning("You have not supplied a permissible value for tissue. Options are `plasma`, `unbound plasma`, `blood`, or `unbound blood`. The PK parameters will be for plasma.", 
              call. = FALSE)
      tissue <- "plasma"
   }
   
   PKorder <- tolower(PKorder)
   if(PKorder %in% c("default", "user specified") == FALSE){
      warning("You have not supplied a permissible value for the order of PK parameters. Options are `default` or `user specified`. The default PK parameter order will be used.", 
              call. = FALSE)
      PKorder <- "default"
   }
   
   if(class(prettify_compound_names) == "character" &&
      is.null(names(prettify_compound_names))){
      warning("You have supplied values for `prettify_compound_names` but not assigned them with compound IDs. That means we don't know which one is the substrate and which one is the perpetrator(s). For now, we'll try our best to prettify the compound names, but if the result is not what you want, please supply a named character vector for what you want to use for the substrate and what you want to use for the perpetrator.", 
              call. = FALSE)
      prettify_compound_names <- TRUE
   }
   
   if(class(prettify_compound_names) == "character"){
      if(any(str_detect(names(prettify_compound_names), "inhibitor"))){
         names(prettify_compound_names)[
            which(str_detect(names(prettify_compound_names), "inhibitor"))] <- "perpetrator"
      }
      
      if("substrate" %in% names(prettify_compound_names) == FALSE){
         warning("The compound IDs you supplied for `prettify_compound_names` must include compound IDs of `substrate` and, if there are any perpetrators, `perpetrator` for the compounds to be prettified as requested. For now, we'll just try our best to prettify the compound names, but if the result is not what you want, please supply a named character vector for what you want to use for the substrate and what you want to use for the perpetrator.", 
                 call. = FALSE)
         prettify_compound_names <- TRUE
      }
   }
   
   # If user asked for a specific sheet and that sheet is the same thing as the
   # AUC tab, then set sheet_PKparameters to NA b/c a) it will automatically
   # look there 1st for the PK and b) it won't mess up anything for knowing
   # which dose the PK is for. The other sheets w/PK parameters are not obvious
   # as to which dose it is, so for those, we need to remove the "_dose1" or
   # "_last".
   if(any(complete.cases(sheet_PKparameters))){
      sheet_PKparameters[sheet_PKparameters %in% c("AUC", "AUC_CI", "AUC_SD")] <- NA
   }
   
   # If the user supplies named character vectors to BOTH sheet_PKparameters AND
   # PKparameters, that will be confusing and I bet people will be inconsistent,
   # too. Only use the ones listed with sheet_PKparameters, and give a warning.
   if(is.null(names(sheet_PKparameters)) == FALSE &
      is.null(names(PKparameters)) == FALSE){
      warning("You supplied a named character vector for both the argument `PKparameters` and the argument `sheet_PKparameters`, so we're not sure which one you want. Please only supply one or the other next time. For now, we'll use what you supplied for `sheet_PKparameters`.\n", 
              call. = FALSE)
      PKparameters <- NA
   }
   
   # Make sure that input to variability_format is ok
   if(variability_format %in% c("to", "hyphen", "brackets", "parentheses") == FALSE){
      warning("The input for variability_format is not among the acceptable options, which are `to`, `hyphen`, `brackets` for square brackets, or `parentheses` for the eponymous symbol if you're an American and a bracket if you're British. We'll use the default of `to`.\n", 
              call. = FALSE)
      variability_format <- "to"
   }
   
   # Harmonizing PK parameter syntax
   if(any(complete.cases(sheet_PKparameters))){
      if(is.null(names(sheet_PKparameters))){
         # Scenario: User has supplied a single specific sheet and also a specific
         # set of PK parameters they want. We want there to be no dose number
         # suffix for consistency w/extractPK.
         PKparameters <- sub("_dose1|_last", "", PKparameters)
         PKparameters <- sub("AUCtau", "AUCt", PKparameters)
         PKparameters <- unique(PKparameters)
         
      } else {
         # If they specified a named character vector for sheet_PKparameters,
         # we need to make sure that we get all the parameters they requested.
         PKparameters <- names(sheet_PKparameters)
      }
   }
   
   # Checking mean type syntax
   if(complete.cases(mean_type) &&
      mean_type %in% c("geometric", "arithmetic") == FALSE){
      if(mean_type == "mean"){
         warning("Technically, the input for mean_type should be either `geometric` (default) or `arithmetic`. You specified a mean type of `mean`, so we think you want arithmetic means and that is what will be reported. If that's incorrect, please set mean_type to `geometric`.\n", 
                 call. = FALSE)
      }
      mean_type <- case_when(str_detect(tolower(mean_type), "geo") ~ "geometric", 
                             mean_type == "mean" ~ "arithmetic")
      
      if(mean_type %in% c("geometric", "arithmetic") == FALSE){
         warning("You specified something other than `geometric` (default) or `arithmetic` for the mean type, so we're not sure what you would like. We'll use the default of geometric means.\n", 
                 call. = FALSE)
         
         mean_type <- "geometric"
      }
   }
   
   PKparameters <- harmonize_PK_names(PKparameters)
   PKparameters_orig <- PKparameters
   
   # Main body of function --------------------------------------------------
   
   ## Reading in all data and tidying ------------------------------------
   if(complete.cases(report_input_file)){
      
      # If they didn't include ".xlsx" at the end of whatever they supplied for
      # report_input_file, add that.
      report_input_file <- ifelse(str_detect(report_input_file, "xlsx$"), 
                                  report_input_file, paste0(report_input_file, ".xlsx"))
      
      if(is.na(sheet_report)){
         warning("You must supply a value for `sheet_report` if you supply a report input file.", 
                 call. = FALSE)
         return(list())
      }
      
      sectionInfo <- getSectionInfo(report_input_file = report_input_file,
                                    sheet_report = sheet_report)
      
      if(complete.cases(sim_data_file) & sim_data_file != sectionInfo$File){
         warning(paste0("The value supplied for `sim_data_file` was `", 
                        sim_data_file, 
                        "``, but the value you supplied in the report input file `",
                        report_input_file, "` was `", 
                        sectionInfo$File,
                        "`. The file listed in the report input file will be used."), 
                 call. = FALSE)
      }
      
      sim_data_file <- sectionInfo$sim_data_file
      # Should we add an error catch here for when user fills out
      # report_input_file but doesn't include any observed data to compare?
      # Maybe not. If the user doesn't want to include any obs data there,
      # just fill out sim_data_file.
      
      # If they supplied both a report_input_file and observed_PK, warn the
      # user that this will preferentially read the report_input_file.
      if(complete.cases(observed_PK[1])){
         warning("You have supplied both a report input file and, separately, observed data. The report input file will be used preferentially and the observed data will be ignored.", 
                 call. = FALSE)
      }
      
      observed_PK <- as.data.frame(sectionInfo$ObsData)
      
   } else {
      
      # Setting this for use later since it's easiest if sectionInfo is
      # logical when it doesn't apply. 
      sectionInfo <- FALSE
      
      # If they supplied observed_PK, get sim_data_file from that. 
      if(any(complete.cases(observed_PK)) && "character" %in% class(observed_PK)){
         observed_PK <- switch(str_extract(observed_PK, "csv|xlsx"), 
                               "csv" = read.csv(observed_PK), 
                               "xlsx" = xlsx::read.xlsx(observed_PK, 
                                                        sheetName = "observed PK"))
         
         # If there's anything named anything like "File", use that for the
         # "File" column. This is useful to deal with capitalization mismatches
         # and also because, if the user saves the file as certain kinds of csv
         # files, R has trouble importing and will add extra symbols to the 1st
         # column name.
         names(observed_PK)[str_detect(tolower(names(observed_PK)), "file")][1] <- 
            "File"
         
      } else if("numeric" %in% class(observed_PK)){ # This is when user has supplied a named numeric vector
         
         observed_PK <- as.data.frame(t(observed_PK))
      }
   }
   
   # At this point, observed_PK, if it exists, should be a data.frame b/c it
   # either was a data.frame at the outset, it has been created by reading an
   # Excel or csv file for observed data, or it came from a report input form.
   # It could be in either wide or long format.
   
   # Cleaning up and harmonizing observed data
   if("data.frame" %in% class(observed_PK)){
      # Convert to long format as needed
      if(any(names(observed_PK) %in% c(AllPKParameters$PKparameter, 
                                       tolower(AllPKParameters$PKparameter)))){
         
         # If "File" isn't already present as a column, adding it to use for
         # joining later. 
         if("File" %in% names(observed_PK) == FALSE){
            observed_PK$File <- sim_data_file
         }
         
         # Set aside variability columns for a moment to pivot correctly
         if(any(str_detect(names(observed_PK), "_CV"))){
            observed_PK_var <- observed_PK %>% 
               select(any_of(c("File", paste0(c(AllPKParameters$PKparameter, 
                                                sub("_dose1|_last", "", AllPKParameters$PKparameter)),
                                              "_CV")))) %>% 
               pivot_longer(cols = -File, 
                            names_to = "PKparameter", 
                            values_to = "CV") %>% 
               mutate(PKparameter = sub("_CV", "",  PKparameter))
         } else {
            observed_PK_var <- data.frame(File = sim_data_file, 
                                          PKparameter = NA)
         }
         
         observed_PK <- observed_PK %>% 
            select(-any_of(paste0(c(AllPKParameters$PKparameter, 
                                    tolower(AllPKParameters$PKparameter), 
                                    AllPKParameters$PKparameter_nodosenum, 
                                    tolower(AllPKParameters$PKparameter_nodosenum)), "_CV"))) %>% 
            pivot_longer(cols = any_of(c(AllPKParameters$PKparameter, 
                                         tolower(AllPKParameters$PKparameter), 
                                         AllPKParameters$PKparameter_nodosenum, 
                                         tolower(AllPKParameters$PKparameter_nodosenum))), 
                         names_to = "PKparameter", 
                         values_to = "Value") %>% 
            left_join(observed_PK_var, by = c("File", "PKparameter"))
      } 
      
      # If they've included several possibilities for mean types, need to get
      # ONLY the appropriate one.
      if("value" %in% tolower(names(observed_PK))){
         names(observed_PK)[which(tolower(names(observed_PK)) == "value")] <- "Value"
      } else if(any(tolower(c("GeoMean", "Mean", "Median")) %in% names(observed_PK))){
         observed_PK <- observed_PK %>% 
            # Dealing with any inconsistencies in capitalization. 
            rename_with(.cols = any_of(c("GeoMean", "Mean", "Median")), 
                        .fn = tolower) %>% 
            mutate(Value = case_when(
               {mean_type} == "geometric" & !str_detect(PKparameter, "tmax") ~ "geomean", 
               {mean_type} == "arithmetic" & !str_detect(PKparameter, "tmax") ~ "mean", 
               str_detect(PKparameter, "tmax") ~ "median"))
      }
      
      if(any(tolower(c("GeoCV", "ArithCV")) %in%
             tolower(names(observed_PK)))){
         observed_PK <- observed_PK %>% 
            # Dealing with any inconsistencies in capitalization. 
            rename_with(.cols = any_of(c("GeoCV", "ArithCV")), 
                        .fn = tolower) %>% 
            mutate(CV = case_when(
               {mean_type} == "geometric" & !str_detect(PKparameter, "tmax") ~ "geocv", 
               {mean_type} == "arithmetic" & !str_detect(PKparameter, "tmax") ~ "arithcv"))
      }
      
      # Harmonizing PK parameter names
      observed_PK$PKparameter <- harmonize_PK_names(observed_PK$PKparameter)
      
      observed_PK <- observed_PK %>% 
         select(any_of(c("File", "Tab", "PKparameter", "Value", "CV"))) %>%  
         # Only keeping parameters that we've set up data extraction for,
         # and only keeping complete.cases of obs data
         filter(PKparameter %in% c(AllPKParameters$PKparameter, 
                                   sub("_dose1|_last", "", AllPKParameters$PKparameter)) &
                   complete.cases(Value))
      
      if("File" %in% names(observed_PK)){
         # Need to adjust a few things b/c of challenges w/file path when this
         # is called from rmarkdown files.
         observed_PK <- observed_PK %>%
            mutate(BaseNameFile = basename(as.character(File))) %>% 
            filter(str_detect(BaseNameFile, basename(sim_data_file))) # ok for user to drop file extension; this should still work
      } else {
         # If File is not in the column names, then assume that it's the
         # same as sim_data_file anyway.
         observed_PK$File <- sim_data_file
      }
      
      # Checking that they haven't provided more than one value for a given PK
      # parameter for this sim_data_file. If they have, we don't know which
      # observed data to compare.
      ObsFileCheck <- observed_PK %>% 
         unique() %>% group_by(File, PKparameter) %>% 
         summarize(NVals = n())
      
      if(any(ObsFileCheck$NVals > 1)){
         warning("You have supplied more than one value for a given PK parameter for this simulator output file, so we don't know which one to use. We will not be able to include observed data in your table.", 
                 call. = FALSE)
         observed_PK <- data.frame()
      } 
      
      if(nrow(observed_PK) < 1){
         warning("None of the supplied observed PK were for the supplied sim_data_file. We cannot make any comparisons between simulated and observed PK.", 
                 call. = FALSE)
         observed_PK <- NA
      }  
      
      # Removing file name here. We should have already filtered to get only the
      # appropriate files, and it's messing up joining with sim data later.
      MyObsPK <- observed_PK %>% select(-File)
      
      # If user provided observed PK, then make sure those PK parameters are
      # included in the PK to extract.
      PKparameters <- sort(unique(union(PKparameters, MyObsPK$PKparameter)))
      
   }
   
   # Now that we have all the possible PK parameters they want, check on whether
   # they've supplied a value for a custom interval w/out specifying the sheet
   # it should come from. If they have supplied only NA for the sheet, then only
   # PKparameters with dose number specifications are valid. Remove any others.
   GoodPKParam <- intersect(PKparameters, 
                            AllPKParameters %>% pull(PKparameter) %>% unique())
   BadPKParam <- setdiff(PKparameters, GoodPKParam)
   if(all(is.na(sheet_PKparameters)) & any(complete.cases(PKparameters)) &
      length(BadPKParam) > 0){
      warning(paste0("The PK parameters ", 
                     str_comma(paste0("`", BadPKParam, "`")), 
                     " look like you want them to come from a custom interval tab in the Excel results file because they don't indicate whether they should be for the first dose or the last dose. However, you have not specified the name of the tab with that custom interval, so we don't know where to look for them. They will be omitted.\n"), 
              call. = FALSE)
      PKparameters <- GoodPKParam
   }
   rm(GoodPKParam, BadPKParam)
   
   # From here down, function is set up for observed PK to be a data.frame or,
   # if the user did not provide observed PK, then a logical. If something went
   # wrong with the data they supplied for observed PK, making the observed PK
   # data.frame just have a single column, "File".
   if("data.frame" %in% class(observed_PK) && ncol(observed_PK) == 1){
      observed_PK <- NA
   }
   
   # If user specified PKorder, then use *either* PKparameters OR observed PK
   # to set the order. Not setting order here; just checking whether user input
   # is acceptable based on whether observed_PK exists.
   if(PKorder != "default" & is.na(PKparameters_orig[1]) & "logical" %in% class(observed_PK)){
      warning("You have requested `user specified` for the argument 'PKorder', which sets the order of columns in the table, but you have not specified what that order should be with the argument `PKparameters` or by supplying observed PK data. The order will be the default order from the Consultancy Report Template.", 
              call. = FALSE)
      PKorder <- "default"
   } 
   
   # At this point, we should have the sim_data_file. 
   if(is.na(sim_data_file)){
      warning("You must enter a simulator output file name for `sim_data_file`, include a simulator output file name with observed PK data, or include a simulator output file name within the Excel file you supplied for `report_input_file`. We don't know what file to use for your simulated data.", 
              call. = FALSE)
      return(list())
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   sim_data_file <- ifelse(str_detect(sim_data_file, "xlsx$"), 
                           sim_data_file, paste0(sim_data_file, ".xlsx"))
   
   ## Figuring out what kind of means user wants, experimental details, etc. --------
   
   # First, the scenarios where there are observed data to compare from a
   # filled-out report template (sectionInfo exists)
   if("logical" %in% class(sectionInfo) == FALSE){
      
      MeanType <- ifelse(is.na(mean_type),
                         sectionInfo$ObsData$MeanType,
                         mean_type)
      MeanType <- ifelse(is.na(MeanType), "geometric", MeanType)
      GMR_mean_type <- sectionInfo$ObsData$GMR_mean_type
      if(is.null(GMR_mean_type)){GMR_mean_type <- MeanType}
      Deets <- sectionInfo
      PerpPresent <- complete.cases(Deets$Inhibitor1)
      DoseRegimen <- Deets$Regimen_sub
      
   } else {
      
      # And second, the scenario where user has not supplied a filled-out
      # report form.
      
      MeanType <- ifelse(is.na(mean_type), "geometric", mean_type)
      GMR_mean_type <- MeanType
      # NB re. GMR_mean_type: I originally had this set to "geometric" all the
      # time because that's nearly always what we report. However, the more I
      # thought about it, the more I realized that people will probably expect
      # this to be whatever mean type they set for the main mean type and it's
      # just going to be confusing to change it. If it turns out to be an
      # issue, revisit this. - LSh
      
      # Checking experimental details to only pull details that apply. NB:
      # "Deets" in all pksummary functions means ONLY the experimental details
      # for the single file in question -- either the only file for
      # pksummary_table or the specific file we're dealing with in that
      # iteration of the loop in pksummary_mult. By contrast,
      # existing_exp_details will include ALL experimental details provided or
      # extracted inside the function.
      if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA
         Deets <- extractExpDetails(sim_data_file = sim_data_file)[["MainDetails"]]
      } else {
         
         existing_exp_details <- 
            harmonize_details(existing_exp_details = existing_exp_details)
         
         Deets <- existing_exp_details$MainDetails %>% 
            filter(File == sim_data_file)
         
         if(nrow(Deets) == 0){
            Deets <- extractExpDetails(sim_data_file = sim_data_file)[["MainDetails"]]
         }
      }
      
      # extractExpDetails will check whether the Excel file provided was, in
      # fact, a Simulator output file and return a list of length 0 if not.
      # Checking for that here.
      if(length(Deets) == 0){
         warning(paste0("The file ", sim_data_file,
                        " is not a Simulator output file and will be skipped.\n", call. = FALSE))
         return(list())
      }
   }
   
   # Need to check that the compound they requested was included in the
   # simulation
   if(is.na(switch(compoundToExtract, 
                   "substrate" = Deets$Substrate,
                   "primary metabolite 1" = Deets$PrimaryMetabolite1,
                   "primary metabolite 2" = Deets$PrimaryMetabolite2,
                   "secondary metabolite" = Deets$SecondaryMetabolite,
                   "inhibitor 1" = Deets$Inhibitor1,
                   "inhibitor 2" = Deets$Inhibitor2,
                   "inhibitor 1 metabolite" = Deets$Inhibitor1Metabolite))){
      warning(paste0("You requested PK data for the ", 
                     compoundToExtract, 
                     " but that compound is not present in the simulation. We cannot return any PK data for it.\n"), call. = FALSE)
      return(list())
   }
   
   PerpPresent <- complete.cases(Deets$Inhibitor1)
   DoseRegimen <- switch(compoundToExtract, 
                         "substrate" = Deets$Regimen_sub,
                         "primary metabolite 1" = Deets$Regimen_sub,
                         "primary metabolite 2" = Deets$Regimen_sub,
                         "secondary metabolite" = Deets$Regimen_sub,
                         "inhibitor 1" = Deets$Regimen_inhib,
                         "inhibitor 2" = Deets$Regimen_inhib2,
                         "inhibitor 1 metabolite" = Deets$Regimen_inhib)
   
   if(Deets$PopRepSim == "Yes"){
      warning(paste0("The simulator file supplied, `", 
                     sim_data_file, 
                     "`, is for a population-representative simulation and thus doesn't have any aggregate data. This function only really works with aggregate data, so this file will be skipped."),
              call. = FALSE)
      return(list())
   }
   
   ## Determining which PK parameters to pull --------------------------------
   if(any(complete.cases(PKparameters))){
      
      PKToPull <- PKparameters
      
   } else {
      
      if("logical" %in% class(sectionInfo)){ # sectionInfo is logical if they did not supply a report input form
         if("data.frame" %in% class(observed_PK)){
            # If user supplies an observed file, then pull the parameters
            # they want to match. 
            
            PKToPull <- observed_PK$PKparameter
            
         } else if(any(complete.cases(sheet_PKparameters))){ 
            PKToPull <- NA # This will retrieve all possible parameters from user-defined interval.
         } else {
            # If the user didn't specify an observed file, didn't list
            # specific parameters they want, and didn't fill out a report
            # input form, then pull the most commonly requested PK
            # parameters.
            PKToPull <- AllPKParameters %>%
               # Per Hannah and template: Only include CL/F, t1/2, or tmax
               # if there's a specific reason to.
               filter(str_detect(PKparameter, "AUCinf_[^P]|AUCt|Cmax")) %>%
               filter(!str_detect(PKparameter, "_hepatic|CLpo")) %>%
               pull(PKparameter) %>% unique()
         }
         
      } else {
         # Pull the PK parameters that match the observed data in the report
         # input form if one was provided.
         PKToPull <- AllPKParameters %>% 
            filter(PKparameter %in% names(sectionInfo$ObsData)) %>% 
            pull(PKparameter) %>% unique()
      }
   }
   
   # !!! IMPORTANT!!! If it was a custom-dosing regimen, then any parameters
   # that are not dose 1 parameters are not necessarily in their usual
   # locations! Do NOT pull last-dose parameters for a custom-dosing simulation
   # UNLESS the user has specified the sheet to use! Giving a warning about
   # that.
   if(((compoundToExtract %in% c("substrate", "primary metabolite 1", 
                                 "primary metabolite 2", "secondary metabolite") & 
        complete.cases(Deets$DoseInt_sub) && Deets$DoseInt_sub == "custom dosing") |
       
       (compoundToExtract %in% c("inhibitor 1", "inhibitor 1 metabolite") &
        is.null(Deets$DoseInt_inhib) == FALSE && 
        complete.cases(Deets$DoseInt_inhib) &&
        Deets$DoseInt_inhib == "custom dosing") |
       
       (compoundToExtract == "inhibitor 2" && 
        is.null(Deets$DoseInt_inhib2) == FALSE && 
        complete.cases(Deets$DoseInt_inhib2) &&
        Deets$DoseInt_inhib2 == "custom dosing")) &
      
      (length(PKToPull) > 0 && any(str_detect(PKToPull, "_last"), na.rm = T)) &
      all(is.na(sheet_PKparameters))){
      warning(paste0("The file `",
                     sim_data_file,
                     "` had a custom dosing regimen for the compound you requested or its parent, which means that PK data for the last dose are NOT in their usual locations.\nWe cannot pull any last-dose PK data for you unless you supply a specific tab using the argument `sheet_PKparameters`."), 
              call. = FALSE)
      PKToPull <- PKToPull[!str_detect(PKToPull, "_last")]
   }
   
   SDParam <- AllPKParameters %>% 
      filter(AppliesToSingleDose == TRUE) %>% 
      pull(PKparameter)
   
   # If dose regimen were single-dose, then only pull dose 1 data.
   if(DoseRegimen == "Single Dose"){
      PKToPull <- PKToPull[PKToPull %in% SDParam]
   } else if(DoseRegimen == "Multiple Dose"){
      
      # If it were multiple dose *and* if they did not specify PK parameters to
      # pull or have observed data to compare and did not specify a user-defined
      # interval, then only pull last dose parameters.
      if(all(is.na(PKparameters_orig)) &
         class(sectionInfo) == "logical" & 
         "logical" %in% class(observed_PK) & all(is.na(sheet_PKparameters))){
         PKToPull <- PKToPull[!str_detect(PKToPull, "_dose1")]
      }
   }
   
   # NB: One other scenario that we are NOT removing any possible PK parameters
   # for: DoseRegimen == "Single Dose and Multiple Dose". This odd thing can
   # happen if it's a single bolus dose and then multiple doses after that. Best
   # to get both SD and MD parameters in that situation b/c it won't always be
   # clear which ones user will want.
   
   # If there was no perpetrator, then don't pull any interaction info
   if(is.na(Deets$Inhibitor1)){
      EffParam <- AllPKParameters %>%
         filter(AppliesOnlyWhenPerpPresent == TRUE) %>%
         pull(PKparameter)
      
      PKToPull <- PKToPull[!PKToPull %in% c(EffParam, 
                                            sub("_dose1|_last", "", EffParam))]
   }
   
   # Give a useful message if there are no parameters to pull
   if(length(PKToPull) == 0){
      warning(paste0("None of the parameters you requested are available from the supplied simulator output file `",
                     sim_data_file, "`. Please check that the parameters requested make sense for the simulation. For example, did you request multiple-dose parameters for a single-dose regimen?"),
              call. = FALSE)
      return(list())
   }
   
   ## Getting PK parameters -------------------------------------------------
   
   # If they have not requested specific sheets for specific PK parameters, then
   # just add AUCt_dose1 to the set of parameters to pull in case of trouble
   # with extrapolation.
   if(is.null(names(sheet_PKparameters))){
      PKparameters_temp <- unique(c(PKToPull,
                                    sub("AUCinf_dose1",
                                        "AUCt_dose1", PKToPull),
                                    sub("AUCinf$",
                                        "AUCt$", PKToPull))) 
   } else {
      # If they *have* requested specific sheets, then we want the PKparameters
      # argument in extractPK to be NA b/c sheet_PKparameters will be contain
      # the info on which PK parameters to pull.
      PKparameters_temp <- NA
   }
   
   suppressWarnings(
      MyPKResults_all <- extractPK(sim_data_file = sim_data_file,
                                   PKparameters = PKparameters_temp,
                                   tissue = tissue,
                                   compoundToExtract = compoundToExtract,
                                   sheet = sheet_PKparameters, 
                                   existing_exp_details = existing_exp_details,
                                   returnAggregateOrIndiv =
                                      switch(as.character(includeTrialMeans),
                                             "TRUE" = c("aggregate", "individual"),
                                             "FALSE" = "aggregate")))
   
   # If there were no PK parameters to be pulled, MyPKResults_all will have
   # length 0 and we can't proceed.
   if(length(MyPKResults_all) == 0){
      warning(paste0("No PK results were found in the file `",
                     sim_data_file, "` for ", compoundToExtract, " in ", tissue,
                     "."), 
              call. = FALSE)
      return()
   }
   
   suppressWarnings(
      CheckDoseInt <- check_doseint(sim_data_file = sim_data_file, 
                                    existing_exp_details = existing_exp_details,
                                    compoundID = compoundToExtract,
                                    stop_or_warn = "warn")
   )
   
   # Sometimes missing problems with extrapolation to infinity. Checking for
   # that here. I thought that there wouldn't be any values for AUCinf, but
   # there definitely are. If any of the AUCinf_X parameters have trouble with
   # extrapolation, the others won't be useful either. Checking for any NA
   # values in geomean, mean, or median. 
   ExtrapCheck <- MyPKResults_all$aggregate %>% 
      filter(Statistic %in% c("mean", "median", "geometric mean")) %>%
      select(matches("AUCinf")) %>% 
      summarize(across(.cols = everything(), .fns = function(x) any(is.na(x))))
   
   if(any(ExtrapCheck == TRUE)){
      MyPKResults_all$aggregate <- MyPKResults_all$aggregate %>% 
         select(-matches("AUCinf"))
   }
   
   # Changing units if user wants. 
   if(complete.cases(adjust_conc_units)){
      # Only adjusting AUC and Cmax values and not adjusting time portion of
      # units -- only conc.
      if(Deets$Units_Cmax != adjust_conc_units){
         ColsToChange <- names(MyPKResults_all$aggregate)[
            intersect(which(str_detect(names(MyPKResults_all$aggregate), "AUC|Cmax")), 
                      which(!str_detect(names(MyPKResults_all$aggregate), "ratio")))
         ]
         
         for(i in ColsToChange){
            TEMP <- match_units(
               MyPKResults_all$aggregate %>% 
                  rename(Conc = i) %>% 
                  mutate(CompoundID = compoundToExtract, 
                         Conc_units = Deets$Units_Cmax, 
                         Time = 1, Time_units = "hours"),
               goodunits = list("Conc_units" = adjust_conc_units, 
                                "Time_units" = "hours"), 
               MW = c(compoundToExtract = 
                         switch(compoundToExtract, 
                                "substrate" = Deets$MW_sub, 
                                "primary metabolite 1" = Deets$MW_met1, 
                                "primary metabolite 2" = Deets$MW_met2, 
                                "secondary metabolite" = Deets$MW_secmet, 
                                "inhibitor 1" = Deets$MW_inhib, 
                                "inhibitor 2" = Deets$MW_inhib2, 
                                "inhibitor 1 metabolite" = Deets$MW_inhib1met)))
            GoodRows <- which(!str_detect(TEMP$Statistic, "CV|cv|Skewness|Fold"))
            MyPKResults_all$aggregate[GoodRows, i] <- TEMP$Conc[GoodRows]
            rm(TEMP)
            
            if("individual" %in% names(MyPKResults_all)){
               TEMP <- match_units(
                  MyPKResults_all$individual %>% 
                     rename(Conc = i) %>% 
                     mutate(CompoundID = compoundToExtract, 
                            Conc_units = Deets$Units_Cmax, 
                            Time = 1, Time_units = "hours"),
                  goodunits = list("Conc_units" = adjust_conc_units, 
                                   "Time_units" = "hours"), 
                  MW = c(compoundToExtract = 
                            switch(compoundToExtract, 
                                   "substrate" = Deets$MW_sub, 
                                   "primary metabolite 1" = Deets$MW_met1, 
                                   "primary metabolite 2" = Deets$MW_met2, 
                                   "secondary metabolite" = Deets$MW_secmet, 
                                   "inhibitor 1" = Deets$MW_inhib, 
                                   "inhibitor 2" = Deets$MW_inhib2, 
                                   "inhibitor 1 metabolite" = Deets$MW_inhib1met)))
               MyPKResults_all$individual[, i] <- TEMP$Conc
               rm(TEMP)
            }
         }
         
         # Need to change units in Deets now to match.
         Deets$Units_AUC <- sub(Deets$Units_Cmax, adjust_conc_units, Deets$Units_AUC)
         Deets$Units_Cmax <- adjust_conc_units
      }
   }
   
   # If they requested AUCinf but there was trouble with that extrapolation,
   # AUCinf won't be present in the data but AUCt will be. Check for that and
   # change PKToPull to reflect that change.
   if(length(PKToPull) > 0 && 
      any(str_detect(PKToPull, "AUCinf_[^P]"), na.rm = T) & 
      (("data.frame" %in% class(MyPKResults_all[[1]]) & 
        all(PKToPull[str_detect(PKToPull, "AUCinf_[^P]")] %in% 
            names(MyPKResults_all[[1]])) == FALSE) |
       ("data.frame" %in% class(MyPKResults_all[[1]]) == FALSE &
        !str_detect(names(MyPKResults_all)[1], "AUCinf_[^P]")))){
      warning(paste0("AUCinf included NA values in the file `", 
                     sim_data_file, 
                     "`, meaning that the Simulator had trouble extrapolating to infinity and thus making the AUCinf summary data unreliable. We will supply AUCt instead."),
              call. = FALSE)
      PKToPull <- unique(sub("AUCinf", "AUCt", PKToPull))
   }
   
   # If they requested multiple parameters but only some were present, need to
   # change PKToPull. This is especially a problem if there's only 1 parameter
   # remaining for which there are data.
   if("data.frame" %in% class(MyPKResults_all[[1]])){
      Missing <- setdiff(PKToPull, names(MyPKResults_all[[1]]))
      PKToPull <- intersect(PKToPull, names(MyPKResults_all[[1]]))
   } else {
      # This is when there was only 1 parameter found.
      PKToPull <- names(MyPKResults_all)[1]
      Missing <- setdiff(AllPKParameters %>%
                            mutate(PKparameter_lower = tolower(PKparameter)) %>% 
                            filter(PKparameter_lower %in% tolower(PKparameters)) %>% 
                            pull(PKparameter), PKToPull)
   }
   
   if(length(Missing) > 0 & complete.cases(PKparameters[1])){
      warning(paste("The following parameters were requested but not found in your simulator output file:",
                    str_comma(Missing)),
              call. = FALSE)
   }
   
   MyPKResults <- MyPKResults_all$aggregate
   
   # Accounting for when mean_type is arithmetic but the user requests that the
   # ratio for + perpetrator over - perpetrator be a GMR. This will replace the
   # arithmetic mean ratio data with geometric mean ratio data. However,
   # because later we need to join that with obs data and we need to use the
   # correct mean type throughout, this will be labeled as "mean" rather than
   # "geomean". Yes, that's confusing, so my apologies, but I couldn't come up
   # with a better way to do this. -LSh
   if(MeanType == "arithmetic" &&
      PerpPresent == TRUE &&
      complete.cases(GMR_mean_type) &&
      GMR_mean_type == "geometric"){
      
      MyPKResults[MyPKResults$Statistic == "mean",
                  str_detect(names(MyPKResults), "ratio")] <-
         MyPKResults[MyPKResults$Statistic == "geometric mean",
                     str_detect(names(MyPKResults), "ratio")]
   }
   
   # Adding trial means since they're not part of the default output
   if(includeTrialMeans){
      
      TrialMeans <- MyPKResults_all$individual %>%
         group_by(Trial) %>%
         summarize(across(.cols = -Individual,
                          .fns = list("geomean" = gm_mean, 
                                      "mean" = mean, 
                                      "median" = median), 
                          .names = "{.col}-{.fn}")) %>%
         ungroup() %>%
         pivot_longer(cols = -Trial, names_to = "Parameter",
                      values_to = "Value") %>%
         separate(col = Parameter, into = c("Parameter", "Stat"), 
                  sep = "-") %>% 
         filter((str_detect(Parameter, "tmax") & Stat == "median") |
                   (!str_detect(Parameter, "tmax") & 
                       Stat == switch(MeanType, "geometric" = "geomean", 
                                      "arithmetic" = "mean"))) %>% 
         group_by(Parameter) %>%
         summarize(MinMean = min(Value),
                   MaxMean = max(Value)) %>%
         pivot_longer(cols = -Parameter,
                      names_to = "Statistic", values_to = "Value") %>%
         pivot_wider(names_from = Parameter, values_from = Value)
      
      MyPKResults <- MyPKResults %>% bind_rows(TrialMeans)
   }
   
   # Renaming stats for ease of coding
   MyPKResults <- MyPKResults %>% mutate(Stat = renameStats(Statistic))
   
   # Adjusting tmax values since the mean row will actually be the median, the
   # lower range of conf interval and percentiles will be the min, and the
   # upper range will be the max.
   if("tmax_dose1" %in% names(MyPKResults)){
      MyPKResults$tmax_dose1[
         which(MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean"))] <-
         MyPKResults$tmax_dose1[which(MyPKResults$Stat == "median")]
      
      MyPKResults$tmax_dose1[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
         MyPKResults$tmax_dose1[MyPKResults$Stat == "min"]
      
      MyPKResults$tmax_dose1[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
         MyPKResults$tmax_dose1[MyPKResults$Stat == "max"]
      
      if(PerpPresent & "tmax_dose1_withInhib" %in% names(MyPKResults)){
         MyPKResults$tmax_dose1_withInhib[
            MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
            MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "median"]
         
         MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
            MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "min"]
         
         MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
            MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "max"]
      }
   }
   
   if("tmax_last" %in% names(MyPKResults)){
      MyPKResults$tmax_last[
         MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
         MyPKResults$tmax_last[MyPKResults$Stat == "median"]
      MyPKResults$tmax_last[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
         MyPKResults$tmax_last[MyPKResults$Stat == "min"]
      MyPKResults$tmax_last[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
         MyPKResults$tmax_last[MyPKResults$Stat == "max"]
      
      if(PerpPresent & "tmax_last_withInhib" %in% names(MyPKResults)){
         MyPKResults$tmax_last_withInhib[
            MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
            MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "median"]
         MyPKResults$tmax_last_withInhib[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
            MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "min"]
         MyPKResults$tmax_last_withInhib[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
            MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "max"]
      }
      
   }
   
   # For scenario where user specifies which tab to get data from
   if("tmax" %in% names(MyPKResults)){
      MyPKResults$tmax[
         MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
         MyPKResults$tmax[MyPKResults$Stat == "median"]
      MyPKResults$tmax[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
         MyPKResults$tmax[MyPKResults$Stat == "min"]
      MyPKResults$tmax[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
         MyPKResults$tmax[MyPKResults$Stat == "max"]
      
      if(PerpPresent & "tmax_withInhib" %in% names(MyPKResults)){
         MyPKResults$tmax_withInhib[
            MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
            MyPKResults$tmax_withInhib[MyPKResults$Stat == "median"]
         MyPKResults$tmax_withInhib[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
            MyPKResults$tmax_withInhib[MyPKResults$Stat == "min"]
         MyPKResults$tmax_withInhib[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
            MyPKResults$tmax_withInhib[MyPKResults$Stat == "max"]
      }
      
   }
   
   # CV and SD should be NA for all tmax values b/c we're reporting medians and
   # range and NOT reporting a mean or geometric mean. Setting that.
   MyPKResults <- MyPKResults %>% 
      mutate(across(.cols = matches("tmax"), 
                    ~replace(., Stat %in% c("CV", "GCV", "SD"), NA)))
   
   VarOptions <- c("CV" = includeCV & MeanType == "arithmetic", 
                   "GCV" = includeCV & MeanType == "geometric",
                   "CI90_low" = includeConfInt,
                   "CI90_high" = includeConfInt, 
                   "CI95_low" = includeConfInt,
                   "CI95_high" = includeConfInt, 
                   "per5" = includePerc, 
                   "per95" = includePerc, 
                   "MinMean" = includeTrialMeans, 
                   "MaxMean" = includeTrialMeans, 
                   "min" = includeRange, 
                   "max" = includeRange, 
                   "SD" = includeSD, 
                   "median" = includeMedian)
   VarOptions <- names(VarOptions)[which(VarOptions)]
   VarOptions <- intersect(VarOptions, MyPKResults$Stat)
   
   MyPKResults <- MyPKResults %>%
      select(-Statistic) %>%
      select(Stat, everything()) %>%
      pivot_longer(cols = -Stat, names_to = "PKParam",
                   values_to = "Sim")
   
   # Setting aside data before filtering away some stats and before rounding for
   # forest data.
   FD <- MyPKResults %>% mutate(SorO = "Sim")
   
   MyPKResults <- MyPKResults %>%
      filter(Stat %in% c(VarOptions, 
                         switch(MeanType, "geometric" = "geomean", 
                                "arithmetic" = "mean")))
   
   
   # observed data -----------------------------------------------------
   
   if(exists("MyObsPK", inherits = FALSE)){
      # Renaming column for PKparameter.
      MyObsPK <- MyObsPK %>% rename(PKParam = PKparameter, Obs = Value)
      
      if("CV" %in% names(MyObsPK)){
         MyObsPK$CV <- as.numeric(MyObsPK$CV)
         
         if(MeanType == "geometric" & "GCV" %in% names(MyObsPK)){
            MyObsPK <- MyObsPK %>% select(-CV) %>% 
               rename(GCV = CV) %>% 
               mutate(CV = as.numeric(CV))
         }
      }
      
      # Pivoting longer by stat
      MyObsPK <- MyObsPK %>% pivot_longer(cols = any_of(c("Obs", "CV", "GCV")),
                                          names_to = "Stat",
                                          values_to = "Obs") %>% 
         mutate(Stat = ifelse(Stat == "Obs", 
                              switch(MeanType, 
                                     "geometric" = "geomean", 
                                     "arithmetic" = "mean"), Stat))
      
      # Calculating S/O
      suppressMessages(
         SOratios <- MyPKResults %>% 
            filter(Stat == switch(MeanType, 
                                  "geometric" = "geomean",
                                  "arithmetic" = "mean")) %>%
            left_join(MyObsPK) %>%
            mutate(Value = Sim / Obs,
                   Stat = "S_O", 
                   SorO = "S_O") %>%
            select(PKParam, Stat, Value, SorO)
      )
      
      suppressMessages(
         MyPKResults <- MyPKResults %>% 
            full_join(MyObsPK) %>% 
            pivot_longer(names_to = "SorO", values_to = "Value", 
                         cols = c(Sim, Obs)) %>% 
            full_join(SOratios)
      )
      
      # If user supplied obs data and did NOT specify PK parameters that they
      # wanted, only keep PK parameters where there are values for the
      # observed mean data.
      if(is.na(PKparameters_orig[1])){
         PKToPull <- MyObsPK %>% filter(complete.cases(Obs)) %>% 
            pull(PKParam) %>% unique()
      }
      
   } else {
      MyPKResults <- MyPKResults %>% 
         rename(Value = Sim) %>% 
         mutate(SorO = "Sim")
   }
   
   ## Arranging forest data ------------------------------------------
   
   if(extract_forest_data){
      
      # Extracting forest data only works when the compound to extract is the
      # substrate or a substrate metabolite. Could change that in the future
      # if there's call for it, but that's all for now.
      if(compoundToExtract %in% c("substrate", "primary metabolite 1", 
                                  "primary metabolite 2", 
                                  "secondary metabolite") == FALSE){
         warning("This function is currently only set up to extract forest data for the substrate or a substrate metabolite, so any other compounds will be skipped.", 
                 call. = FALSE)
         FD <- list()
         
      } else {
         
         # For forest data, only keeping ratios and removing observed data from
         # here b/c we supply it separately for the forest_plot function.
         FD <- FD %>% 
            filter(str_detect(PKParam, "ratio") & SorO == "Sim")
         
         FD <- FD %>% 
            # Harmonizing. Yes, some of these look like duplicates, but they
            # don't show up together, so we shouldn't have duplicated column
            # names after pivoting wider.
            mutate(Stat = case_match(Stat, 
                                     "Geometric Mean" ~ "GeoMean",
                                     "CI90_low" ~ "CI_Lower", 
                                     "CI90_high" ~ "CI_Upper",
                                     "per5" ~ "Centile_Lower", 
                                     "per95" ~ "Centile_Upper",
                                     "geomean" ~ "GeoMean",
                                     "mean" ~ "Mean", 
                                     "Mean" ~ "Mean", 
                                     "Median" ~ "Median",
                                     "median" ~ "Median", 
                                     "90% confidence interval around the geometric mean(lower limit)" ~ "CI_Lower", 
                                     "90% confidence interval around the geometric mean(upper limit)" ~ "CI_Upper", 
                                     "95% confidence interval around the geometric mean(lower limit)" ~ "CI_Lower",
                                     "95% confidence interval around the geometric mean(upper limit)" ~ "CI_Upper",
                                     "5th centile" ~ "Centile_Lower", 
                                     "95th centile" ~ "Centile_Upper", 
                                     "Min Val" ~ "Min", 
                                     "min" ~ "Min", 
                                     "Max Val" ~ "Max", 
                                     "max" ~ "Max", 
                                     "fold" ~ "Fold", 
                                     "cv" ~ "ArithCV", 
                                     "Geometric CV" ~ "GeoCV", 
                                     "Std Dev" ~ "SD", 
                                     .default = Stat)) %>% 
            rename(PKparameter = PKParam) %>% 
            filter(str_detect(PKparameter, "AUCinf_[^P]|AUCt|Cmax")) %>% 
            mutate(File = sim_data_file, 
                   Substrate = switch(compoundToExtract, 
                                      "substrate" = Deets$Substrate, 
                                      "primary metabolite 1" = Deets$PrimaryMetabolite1, 
                                      "primary metabolite 2" = Deets$PrimaryMetabolite2, 
                                      "secondary metabolite" = Deets$SecondaryMetabolite), 
                   Dose_sub = Deets$Dose_sub, 
                   Inhibitor1 = ifelse("Inhibitor1" %in% names(Deets),
                                       Deets$Inhibitor1, NA),
                   Dose_inhib = ifelse("Dose_inhib" %in% names(Deets),
                                       Deets$Dose_inhib, NA)) %>% 
            pivot_wider(names_from = Stat, values_from = Sim) %>% 
            select(File, Substrate, Dose_sub, Inhibitor1, Dose_inhib, 
                   everything())
         
      }
   }
   
   # Putting everything together and formatting -------------------------
   
   # Formatting and selecting only rows where there are data. Also removing any
   # PKparameters where we have only observed data, which can happen if user
   # specifies a PK parameter w/out a dose number but then does not specify a
   # custom interval tab. That messes up other formatting down the line
   # w/writing to Word.
   MyPKResults <- MyPKResults %>%
      mutate(Value = if_else(str_detect(Stat, "CV"), 
                             round_opt(100*Value, rounding),
                             round_opt(Value, rounding))) %>%
      filter(Stat %in% c(ifelse(MeanType == "geometric", "geomean", "mean"),
                         "CI90_low", "CI90_high", "CI95_low", "CI95_high",
                         "min", "max", "per5", "per95", 
                         ifelse(MeanType == "geometric", "GCV", "CV"), 
                         "MinMean", "MaxMean", "S_O", "SD", "median"))
   
   # Checking for any PK parameters where there are no simulated data.
   GoodPKParam <- MyPKResults %>% 
      filter(Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean") &
                SorO == "Sim" &
                complete.cases(Value)) %>% pull(PKParam) %>% unique()
   
   MyPKResults <- MyPKResults %>%
      filter(PKParam %in% GoodPKParam) %>% 
      pivot_wider(names_from = PKParam, values_from = Value) %>% 
      mutate(SorO = factor(SorO, levels = c("Sim", "Obs", "S_O")), 
             Stat = factor(Stat, levels = c("mean", "geomean", "median", "CV", "GCV", 
                                            "min", "max",
                                            "CI90_low", "CI90_high", "CI95_low", 
                                            "CI95_high", "per5", "per95",
                                            "MinMean", "MaxMean", "SD", "S_O"))) %>% 
      arrange(SorO, Stat) %>% 
      filter(if_any(.cols = -c(Stat, SorO), .fns = complete.cases)) %>% 
      mutate(across(.cols = everything(), .fns = as.character)) 
   
   rm(GoodPKParam)
   
   # Putting trial means into appropriate format
   if(includeTrialMeans){
      TM <- MyPKResults %>% filter(Stat %in% c("MinMean", "MaxMean")) %>%
         summarize(across(.cols = -c(Stat, SorO),
                          .fns = function(x) {paste(x[1], "to", x[2])}))
      MyPKResults <- MyPKResults %>%
         filter(Stat != "MaxMean")
      MyPKResults[which(MyPKResults$Stat == "MinMean"), 
                  3:ncol(MyPKResults)] <-
         TM
   }
   
   # Concatenating the rows w/lower and upper limits of variability when
   # requested
   if(concatVariability){
      
      # Note: When multiple options are chosen for variability type,
      # concatVariability doesn't work. Will need to fix this later.
      VarRows <- list("ConfInt90" = c("CI90_low", "CI90_high"), 
                      "ConfInt95" = c("CI95_low", "CI95_high"),
                      "Perc" = c("per5", "per95"), 
                      "Range" = c("min", "max"))
      VarRows <- VarRows[sapply(VarRows, function(x) unlist(x[[1]])) %in% VarOptions]
      
      VarRows[["obs"]] <- c("CIL_obs", "CIU_obs")
      for(j in names(VarRows)){
         temp <- MyPKResults %>%
            filter(Stat %in% as.character(unlist(VarRows[[j]]))) %>%
            mutate(across(.cols = !matches("Stat"),
                          .fns = function(x) {
                             ifelse(all(complete.cases(c(x[1], x[2]))),
                                    switch(variability_format, 
                                           "to" = paste(x[1], "to", x[2]),
                                           "hyphen" = paste(x[1], "-", x[2]),
                                           "brackets" = paste0("[", x[1], ", ", x[2], "]"), 
                                           "parentheses" = paste0("(", x[1], ", ", x[2], ")")),
                                    NA)}),
                   Stat = switch(j,
                                 "ConfInt90" = "CI90concat",
                                 "ConfInt95" = "CI95concat",
                                 "Perc" = "per95concat",
                                 "Range" = "Rangeconcat",
                                 "obs" = "CIobsconcat"))
         
         MyPKResults[which(MyPKResults$Stat == VarRows[[j]][1]), ] <-
            temp[1, ]
         MyPKResults <- MyPKResults %>% filter(Stat != VarRows[[j]][2])
         rm(temp)
      }
   }
   
   # Renaming statistics to match what's in template
   StatNames <- c("geomean" = "Simulated",
                  "mean" = "Simulated",
                  "GCV" = "CV%",
                  "CV" = "CV%",
                  "SD" = "Standard deviation",
                  "CI90_low" = "90% CI - Lower",
                  "CI90_high" = "90% CI - Upper",
                  "CI90concat" = "90% CI",
                  "CI95_low" = "95% CI - Lower",
                  "CI95_high" = "95% CI - Upper",
                  "CI95concat" = "95% CI",
                  "per5" = "5th Percentile",
                  "per95" = "95th Percentile",
                  "per95concat" = "5th to 95th Percentile",
                  "min" = "Minimum", 
                  "max" = "Maximum",
                  "median" = "Median",
                  "Rangeconcat" = "Range",
                  # "geomean_obs" = "Observed",
                  # "CV_obs" = "CV%",
                  # "CIL_obs" = "observed CI - Lower",
                  # "CIU_obs" = "observed CI - Upper",
                  # "CIobsconcat" = "Observed CI",
                  "S_O" = "S/O",
                  "MinMean" = "Range of trial means")
   
   MyPKResults <- MyPKResults %>%
      mutate(Statistic = as.character(Stat),
             Statistic = StatNames[Statistic], 
             Statistic = ifelse(SorO == "Obs" & Statistic == "Simulated", 
                                "Observed", Statistic)) %>%
      select(-Stat) %>%
      select(Statistic, everything())
   
   # setting levels for PK parameters so that they're in a nice order. This
   # requires values for PKToPull, as does saving output to Word. PKToPull could
   # be empty depending on user input, so adjusting for that here.
   PKToPull <- names(MyPKResults)[
      names(MyPKResults) %in% c(AllPKParameters$PKparameter, 
                                sub("_dose1|_last", "", AllPKParameters$PKparameter))]
   
   PKlevels <- switch(PKorder, 
                      
                      # the default scenario
                      "default" =
                         bind_rows(AllPKParameters, 
                                   AllPKParameters %>% 
                                      mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
                                             SortOrder = SortOrder + 25)) %>% # This should work based on how I've got the SortOrder set up in AllPKParameters.
                         select(PKparameter, SortOrder) %>% 
                         arrange(SortOrder) %>%
                         pull(PKparameter) %>% unique(), 
                      
                      # user wants a specific order but using default tabs
                      "user specified" = PKparameters)
   
   # Checking for whether to include AUCinf, AUCt, or both for dose 1 based on
   # what user requested initially and whether there were any problems with
   # extrapolation. If there were problems with extrapolation for either
   # AUCinf_dose1 OR for AUCinf_dose1_withInhib, then we want only AUCt values
   # b/c need to be able to make the correct comparison. If there were no
   # problems, then we only want AUCinf values unless they speicifcally
   # requested AUCt.
   
   AUCParam <- PKToPull[str_detect(PKToPull, "AUC.*_dose1")]
   NonAUCParam <- setdiff(PKToPull, AUCParam)
   
   # Any time AUCinf_dose1 and AUCinf_dose1_withInhib are both included in
   # PKToPull, only retain any AUCt_X that were specfically requested.
   if("AUCinf_dose1" %in% PKToPull & 
      "AUCt_dose1" %in% PKparameters_orig == FALSE){
      AUCParam <- setdiff(AUCParam, "AUCt_dose1")
   }
   
   if("AUCinf_dose1_withInhib" %in% PKToPull & 
      "AUCt_dose1_withInhib" %in% PKparameters_orig == FALSE){
      AUCParam <- setdiff(AUCParam, "AUCt_dose1_withInhib")
   }
   
   if("AUCinf_ratio_dose1" %in% PKToPull & 
      "AUCt_ratio_dose1" %in% PKparameters_orig == FALSE){
      AUCParam <- setdiff(AUCParam, "AUCt_ratio_dose1")
   }
   
   PKToPull <- c(AUCParam, NonAUCParam)
   PKToPull <- factor(PKToPull, levels = PKlevels)
   PKToPull <- sort(unique(PKToPull))
   
   # Getting columns in a good order
   MyPKResults <- MyPKResults %>%
      select(any_of(c("Statistic", as.character(PKToPull))))
   
   # Optionally adding final column names
   if(prettify_columns){
      
      # Checking position of columns with custom intervals.
      CustomIntCols <- which(
         PKToPull %in% sub("_dose1|_last", "", AllPKParameters$PKparameter))
      
      # If user specified tab, then need to adjust PK parameters here, too.
      if(any(complete.cases(sheet_PKparameters)) & 
         any(str_detect(names(MyPKResults_all[[1]]), "_dose1|_last")) == FALSE){
         AllPKParameters_mod <- 
            AllPKParameters %>% select(PKparameter, PrettifiedNames) %>% 
            mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
                   PrettifiedNames = str_trim(sub("Last dose|Dose 1", "", 
                                                  PrettifiedNames))) %>% 
            unique()
         
         # We don't know whether an AUC was actually AUCtau, so make it AUCt.
         # First, though, if there are multiples of AUCt and AUCtau, remove the
         # AUCtau or we'll get replicate columns. Since the user specified a
         # sheet here, we do know that AUCt and AUCtau would be the same column
         # getting pulled twice. Not sure where I messed up that code, but this
         # fixes that problem.
         if(all(c("AUCtau", "AUCt") %in% PKToPull) |
            all(c("AUCtau_withInhib", "AUCt_withInhib") %in% PKToPull) |
            all(c("AUCtau_ratio", "AUCt_ratio") %in% PKToPull)){
            
            PKToPull <- PKToPull[!str_detect(PKToPull, "AUCtau")]
            MyPKResults <- MyPKResults %>% 
               select(!matches("AUCtau"))
         }
         
         PKToPull <- sub("AUCtau", "AUCt", PKToPull)
         
         suppressMessages(
            PrettyCol <- data.frame(PKparameter = PKToPull) %>% 
               left_join(AllPKParameters_mod) %>% 
               pull(PrettifiedNames)
         )
      } else {
         suppressMessages( 
            PrettyCol <- data.frame(PKparameter = PKToPull) %>% 
               left_join(
                  bind_rows(AllPKParameters, 
                            AllPKParameters %>% 
                               mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
                                      PrettifiedNames = sub("Dose 1 |Last dose ", "", PrettifiedNames))) %>% 
                     select(PKparameter, PrettifiedNames)) %>% 
               unique() %>% 
               pull(PrettifiedNames)
         )
      }
      
      # Adding time interval to any data that came from custom AUC interval
      # sheets.
      if(any(complete.cases(sheet_PKparameters))){
         IntToAdd <- MyPKResults_all$TimeInterval %>% 
            filter(Sheet %in% sheet_PKparameters) %>% 
            pull(Interval)
         
         UnitsToAdd <- str_extract(PrettyCol[CustomIntCols], 
                                   " \\(h\\)| \\(ng/mL(.h)?\\)| \\(L/h\\)")
         
         PrettyCol[CustomIntCols] <- 
            sub(" \\(h\\)| \\(ng/mL(.h)?\\)| \\(L/h\\)", "", PrettyCol[CustomIntCols])
         PrettyCol[CustomIntCols] <- paste0(PrettyCol[CustomIntCols], 
                                            " for interval ", IntToAdd, 
                                            UnitsToAdd)
      }
      
      if(complete.cases(adjust_conc_units)){
         PrettyCol <- gsub(Deets$Units_Cmax,  adjust_conc_units, PrettyCol)
      }
      
      # Adjusting units as needed.
      PrettyCol <- sub("\\(ng/mL.h\\)", paste0("(", Deets$Units_AUC, ")"), PrettyCol)
      PrettyCol <- sub("\\(L/h\\)", paste0("(", Deets$Units_CL, ")"), PrettyCol)
      PrettyCol <- sub("\\(ng/mL\\)", paste0("(", Deets$Units_Cmax, ")"), PrettyCol)
      PrettyCol <- sub("\\(h\\)", paste0("(", Deets$Units_tmax, ")"), PrettyCol)
      PrettyCol <- gsub("ug/mL", "µg/mL", PrettyCol)
      
      MyPerpetrator <- determine_myperpetrator(Deets, prettify_compound_names)
      
      if(any(complete.cases(MyPerpetrator))){
         PrettyCol <- sub("perpetrator", MyPerpetrator, PrettyCol)
      }
      
      names(MyPKResults) <- c("Statistic", PrettyCol)
      
      
   } else if(any(complete.cases(sheet_PKparameters)) & 
             any(str_detect(names(MyPKResults_all[[1]]), "_dose1|_last")) == FALSE){
      # This is when it's a user-defined sheet but we're not prettifying column
      # names. We don't know whether an AUC was actually AUCtau, so make it
      # AUCt.
      PKToPull <- sub("AUCtau", "AUCt", PKToPull)
   }
   
   if(is.na(include_dose_num)){
      # Dropping dose number depending on input. First, checking whether they have
      # both dose 1 and last-dose data.
      DoseCheck <- c("first" = any(str_detect(names(MyPKResults), "Dose 1")), 
                     "last" = any(str_detect(names(MyPKResults), "Last dose")))
      
      # Next, checking whether they have a mix of custom AUC intervals and
      # regular b/c need to retain dose num in that case.
      if(any(PKparameters %in% 
             c(setdiff(unique(AllPKParameters$PKparameter_nodosenum), 
                       unique(AllPKParameters$PKparameter)), 
               setdiff(unique(AllPKParameters$PrettifiedNames_nodosenum), 
                       unique(AllPKParameters$PrettifiedNames))))){
         DoseCheck <- TRUE
      }
      
      include_dose_num <- all(DoseCheck)
   }
   
   # include_dose_num now should be either T or F no matter what, so checking
   # that.
   if(is.logical(include_dose_num) == FALSE){
      warning("Something is amiss with your input for `include_dose_num`, which should be NA, TRUE, or FALSE. We'll assume you meant for it to be TRUE.", 
              call. = FALSE)
      include_dose_num <- TRUE
   }
   
   if(include_dose_num == FALSE){
      names(MyPKResults) <- sub("Dose 1 |Last dose ", "", names(MyPKResults))
   }
   
   if(checkDataSource){
      
      ColsToInclude <- c("PKparam", "File", "Tab", 
                         switch(MeanType,
                                "arithmetic" = "mean",
                                "geometric" = "geomean"))
      
      if(length(PKToPull) > 0 && any(str_detect(PKToPull, "tmax"), na.rm = T)){
         ColsToInclude <- c(ColsToInclude, "min", "max", "median")
      }
      
      if(includeConfInt){
         ColsToInclude <- c(ColsToInclude, "CI90_low", "CI90_high")
      }
      
      if(includeCV){
         ColsToInclude <- c(ColsToInclude, 
                            switch(MeanType, 
                                   "arithmetic" = "CV", 
                                   "geometric" = "GCV"))
      }
      
      if(includePerc){
         ColsToInclude <- c(ColsToInclude, "per5", "per95")
      }
      
      if(includeRange){
         ColsToInclude <- c(ColsToInclude, "max", "min")
      }
      
      if(includeMedian){
         ColsToInclude <- c(ColsToInclude, "median")
      }
      
      if(includeSD){
         ColsToInclude <- c(ColsToInclude, "SD")
      }
      
      OutQC <- MyPKResults_all$QC %>% 
         select(PKparam, File, matches(ColsToInclude))
      
   }
   
   PKpulled <- PKToPull # Need to rename here for consistency w/other pksummary functions and Rmd files.
   
   # Saving --------------------------------------------------------------
   if(complete.cases(save_table)){
      
      # Checking whether they have specified just "docx" or just "csv" for
      # output b/c then, we'll use sim_data_file as file name. This allows us
      # to determine what the path should be, too, for either sim_data_file or
      # for some specified file name.
      if(str_detect(sub("\\.", "", save_table), "^docx$|^csv$")){
         OutPath <- dirname(sim_data_file)
         save_table <- sub("xlsx", 
                           # If they included "." at the beginning of the
                           # file exension, need to remove that here.
                           sub("\\.", "", save_table),
                           basename(sim_data_file))
      } else {
         # If they supplied something other than just "docx" or just "csv",
         # then check whether that file name is formatted appropriately.
         
         if(str_detect(basename(save_table), "\\..*")){
            if(str_detect(basename(save_table), "\\.docx") == FALSE){
               # If they specified a file extension that wasn't docx, make that
               # file extension be .csv
               save_table <- sub("\\..*", ".csv", save_table)
            }
         } else {
            # If they didn't specify a file extension at all, make it .csv. 
            save_table <- paste0(save_table, ".csv")
         }
         
         # Now that the file should have an appropriate extension, check what
         # the path and basename should be.
         OutPath <- dirname(save_table)
         save_table <- basename(save_table)
      }
      
      if(str_detect(save_table, "docx")){ 
         # This is when they want a Word file as output
         
         # May need to change the working directory temporarily, so
         # determining what it is now
         CurrDir <- getwd()
         
         OutPath <- dirname(save_table)
         if(OutPath == "."){
            OutPath <- getwd()
         }
         
         FileName <- basename(save_table)
         FromCalcPKRatios <- FALSE
         
         rmarkdown::render(system.file("rmarkdown/templates/pk-summary-table/skeleton/skeleton.Rmd",
                                       package="SimcypConsultancy"), 
                           output_dir = OutPath, 
                           output_file = FileName, 
                           quiet = TRUE)
         # Note: The "system.file" part of the call means "go to where the
         # package is installed, search for the file listed, and return its
         # full path.
         
      } else {
         # This is when they want a .csv file as output. In this scenario,
         # changing the value "simulated" in the list of stats to include
         # whether it was arithmetic or geometric b/c that info is included
         # in the Word file but not in the table itself.
         MyPKResults <- MyPKResults %>% 
            mutate(Statistic = sub("Simulated", 
                                   paste("Simulated", MeanType, "mean"), Statistic))
         WarningDF <- data.frame(Col1 = "WARNING:",
                                 Col2 = "This table was saved to a csv file, and Excel automatically drops any trailing zeroes. Please check your sig figs to make sure you haven't inadvertently dropped a trailing zero.")
         names(WarningDF) <- names(MyPKResults)[1:2]
         
         write.csv(bind_rows(MyPKResults, WarningDF),
                   paste0(OutPath, "/", save_table), row.names = F)
      }
   }
   
   Out <- list("Table" = MyPKResults)
   
   if(checkDataSource){
      Out[["QC"]] <- OutQC
      
      if(complete.cases(save_table)){
         
         write.csv(OutQC, sub(".csv|.docx", " - QC.csv", save_table), row.names = F)
         
      }
   }
   
   if(extract_forest_data){
      Out[["ForestData"]] <- FD
      
      if(complete.cases(save_table)){ 
         write.csv(OutQC, sub(".csv|.docx", " - forest data.csv", save_table), row.names = F)
      }
   }
   
   if(return_PK_pulled){
      Out[["PKpulled"]] <- PKpulled
   }
   
   if(length(Out) == 1){
      Out <- Out[["Table"]]
   }
   
   if(CheckDoseInt$message == "mismatch" & any(str_detect(PKpulled, "_last"))){
      warning("The time used for integrating the AUC for the last dose was not the same as the dosing interval.\n", 
              call. = FALSE)
   }
   
   return(Out)
   
}



