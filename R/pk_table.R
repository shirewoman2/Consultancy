#' Make tables of PK values from Simulator output Excel files
#'
#' \code{pk_table} creates tables of PK parameters for reports and
#' presentations, including reporting means, CVs, and confidence intervals or
#' percentiles and, optionally, comparisons to observed data. This function
#' automatically finds the correct tabs and the correct cells in a Simulator
#' output Excel file to obtain those data. \strong{Notes:} \itemize{\item{Coding
#' requires having a standardized way to input all the myriad
#' possibilities for PK parameters, which can be tricky. Please try
#' running \code{\link{make_example_PK_input}} to see examples for how to
#' specify the PK parameters you need.} \item{For detailed instructions and
#' examples, please see the SharePoint file "Simcyp PBPKConsult
#' R Files - Simcyp PBPKConsult R Files/SimcypConsultancy function examples and
#' instructions/Making PK tables/PK-tables.docx". (Sorry, we are unable to
#' include a link to it here.)} \item{In the results, tmax will be listed as
#' median, min, and max rather than mean, lower and higher confidence interval
#' or percentiles. Similarly, if you request trial means, the values for tmax
#' will be the range of medians for the trials rather than the range of
#' means.} \item{We strongly recommend saving the output to a Word file, which
#' will apply any highlighting you request and some other nice formatting,
#' will not drop trailing zeroes, and will include some text for possible
#' table headings and captions.}}
#'
#'
#' @param PKparameters the PK parameters to include. There are two main options
#'   for this: 1) supply a file to read or a data.frame (R speak for "a table")
#'   that lists which simulation files, compounds, tissues, and PK you want or
#'   2) supply a character vector of which PK parameters you want and then also
#'   specify what you need in terms of which tissues, which compounds, which
#'   simulation files, and which tab to get the data from with the arguments
#'   \code{tissues}, \code{compoundsToExtract}, \code{sim_data_files}, and
#'   \code{sheet_PKparameters}.
#'   \strong{Details on each option:} \describe{
#'
#'   \item{\strong{Option 1: }a file to read or a data.frame}{This
#'   is the most versatile option and, we think, the clearest in terms of
#'   getting what you expected. Please try running \code{\link{make_example_PK_input}}
#'   to see examples for how to set up a csv or Excel file or data.frame to
#'   specify exactly which simulation file should get which PK parameter from
#'   which tissue and, when user-specified intervals are involved, from which
#'   tab in the Excel file those data should be pulled. Whatever you supply, the
#'   columns that will be read are: \itemize{\item{"File" (same thing as the argument
#'   \code{sim_data_files})} \item{"Sheet" (same thing as the argument
#'   \code{sheet_PKparameters})} \item{"Tissue" (same as the argument \code{tissues})}
#'   \item{"CompoundID" (same as the argument \code{compoundsToExtract})
#'   \item{"ObsValue" for any observed data (no equivalent argument)}
#'   \item{"Variability" for any observed variability (no equivalent argument
#'   here, either)}} If you
#'   omit any of those columns, whatever you supply for their respective
#'   arguments will be used instead. If you supply something for one of them
#'   in the data.frame or file and \emph{also} something for its argument, the
#'   argument will be ignored. \cr
#'
#'   Here is how to specify each of the possible
#'   inputs for Option 1:\describe{\item{a csv file}{list the file
#'   name, including the file extension ".csv" inside quotes, e.g., "PK needed.csv"}
#'
#'   \item{an Excel file}{list the file name, including the file extension
#'   ".xlsx", inside quotes, e.g., "PK needed.xlsx". We will be looking for a
#'   tab titled "PKparameters" (all one word and with the same capitalization).}
#'
#'   \item{a data.frame}{If you'd like to supply a data.frame with the same
#'   columns you would have had in the .csv or Excel file, that works just the
#'   same.}}}}
#'
#'   \item{\strong{Option 2: }specify the PK parameters you want for all your
#'   simulations}{This is a good option when you want the same information
#'   from all your simulations. List the PK parameters you want here and then,
#'   in the arguments
#'   \code{tissues}, \code{compoundsToExtract}, \code{sim_data_files}, and
#'   \code{sheet_PKparameters} specify which of each of those items you want.
#'   You'll get all possible combinations of these, so, for example, if you say
#'   \code{PKparameters = c("AUCinf_dose1", "Cmax_dose1")} and
#'   \code{tissues = c("blood", "plasma")}, you'll get the dose 1 AUCinf and
#'   Cmax for both blood and plasma for all the simulation files you list with
#'   \code{sim_data_files}. If you're going this route, here are the two options
#'   you have for the argument \code{PKparameters}: \describe{
#'
#'   \item{NA}{If you leave this as NA, by default, if you have a single-dose
#'   simulation, the parameters will
#'   include AUC and Cmax for dose 1, or, if you have a multiple-dose
#'   simulation, AUC and Cmax for the last dose. Also by default, if you have a
#'   perpetrator present, the parameters will include the AUC and Cmax values with
#'   and without the perpetrator as well as those ratios.}
#'
#'   \item{a character vector of any combination of specific, individual
#'   parameters}{This character vector must contain SimcypConsultancy package
#'   coded names for each parameter you want, e.g., \code{c("Cmax_dose1",
#'   "AUCtau_last").} Be sure to encapsulate the parameters you want with
#'   \code{c(...)}. Please try running \code{\link{make_example_PK_input}} to
#'   see examples, or, to see the full set of all possible parameters to extract, enter
#'   \code{view(PKParameterDefinitions)} into the console.}}}}
#'
#'   Parameters that don't make sense for your scenario -- such as asking for
#'   \code{AUCinf_dose1_withInhib} when your simulation did not include a
#'   perpetrator -- will be ignored.
#'
#' @param sim_data_files the Simcyp Simulator output Excel files to use. Options
#'   for how to specify these: \itemize{\item{NA to extract PK data
#'   for \emph{all} the Excel files in the current folder or for all the files
#'   listed in what you supply to the argument \code{PKparameters}}
#'   \item{"recursive" to extract PK data for all the Excel files
#'   in the current folder and all subfolders.} \item{a character
#'   vector of simulator output files, each in quotes and encapsulated with
#'   \code{c(...)}} } If you do want specific simulations, please take pity on your
#'   poor R coders and do not use the same simulation file names in different
#'   subfolders; duplicate file names are just too confusing, and we might give
#'   you incorrect results.
#' @param compoundsToExtract For which compounds do you want to extract PK data?
#'   Options are any combination of the following:
#'   \itemize{\item{"substrate" (default)} \item{"primary metabolite 1"}
#'   \item{"primary metabolite 2"} \item{"secondary metabolite"}
#'   \item{"inhibitor 1" -- this can be an inducer, inhibitor, activator, or
#'   suppresesor, but it's labeled as "Inhibitor 1" in the simulator}
#'   \item{"inhibitor 2" for the 2nd inhibitor listed in the simulation}
#'   \item{"inhibitor 1 metabolite" for the primary metabolite of inhibitor 1}
#'   \item{"all" for all possible compounds present in the simulations}} To
#'   specify multiple compounds, enclose the compound IDs with parentheses,
#'   e.g., \code{compoundsToExtract = c("substrate", "inhibitor 1")}, or, and we
#'   recommend this second option instead for clarity, supply them in a
#'   data.frame or csv file for the argument \code{PKparameters}.
#' @param tissues For which tissue(s) would you like the PK parameters to be
#'   pulled? Options are any combination of: \itemize{\item{"plasma" (default)}
#'   \item{"unbound plasma"} \item{"blood"} \item{"unbound blood"}
#'   \item{"peripheral plasma"} \item{"peripheral blood"}} For multiple tissues,
#'   enclose them with parentheses, e.g., \code{tissues = c("blood", "plasma")}
#'   or, better, do not supply anything here and instead supply which tissue you
#'   want for which simulation and which compound, etc. when you supply a
#'   data.frame or csv file to the argument \code{PKparameters}.
#' @param PKorder Would you like the order of the PK parameters to be the order
#'   specified in the Consultancy Report Template (default), or would you like
#'   the order to match the order you specified with the argument
#'   \code{PKparameters}? Options are "default" or "user specified".
#' @param file_order order of the simulations in the output table, default is to
#'   leave the order "as is", in which case the order will be whatever is
#'   specified with \code{sim_data_files}.
#' @param sheet_PKparameters (optional) If you want the PK parameters to be
#'   pulled from a specific tab in the simulator output file, list that tab
#'   here. Otherwise, this should be left as NA. \code{sheet_PKparameters} can
#'   only have a \emph{single value}, though. If you want some parameters from a
#'   custom-interval tab and others from the regular tabs, you must supply that
#'   as part of a data.frame or csv file for the argument \code{PKparameters}.
#'   Please try running \code{\link{make_example_PK_input}} to see examples for
#'   how to do this.
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get all the details from the "Input
#'   Sheet" (e.g., when you ran extractExpDetails you said \code{exp_details =
#'   "Summary and Input"} or \code{exp_details = "all"}), you can save some processing
#'   time by supplying that object here, unquoted. If left as NA, this function
#'   will run \code{extractExpDetails} behind the scenes anyway to figure out
#'   some information about your experimental set up.
#' @param return_caption TRUE or FALSE (default) for whether to return any
#'   caption text to use with the table. If set to TRUE, you'll get as output a
#'   list of the table, the table heading, and the table caption. When you've
#'   requested multiple individual tables, e.g., when \code{single_table =
#'   FALSE}, this will not return anything at this point. We may add that later,
#'   but at present, this is not set up to return multiple table captions.
#' @param mean_type What kind of means and CVs do you want listed in the output
#'   table? Options are "geometric" (default), "arithmetic", or "arithmetic for
#'   most, geometric for ratios".
#' @param use_median_for_tmax TRUE (default) or FALSE for whether to use median
#'   for tmax values, regardless of what the other summary statistics are. This
#'   is typically the case, but, if you've got client data where they actually
#'   gave you tmax using the same summary statistic as the other PK parameters
#'   (like geometric mean, for example), then set this to FALSE and whatever
#'   mean type you specified with the argument \code{mean_type} will also be
#'   used for tmax.
#' @param includeCV TRUE (default) or FALSE for whether to include rows for CV
#'   in the table
#' @param includeSD TRUE or FALSE (default) for whether to include rows for the
#'   standard deviation in the table
#' @param includeConfInt TRUE (default) or FALSE for whether to include whatever
#'   confidence intervals were included in the simulator output file. Note that
#'   the confidence intervals are geometric since that's what the simulator
#'   outputs (see an AUC tab and the summary statistics; these values are the
#'   ones for, e.g., "90\% confidence interval around the geometric mean(lower
#'   limit)").
#' @param includeMedian TRUE or FALSE (default) for whether to include rows for
#'   the median in the table
#' @param includeRange TRUE or FALSE (default) for whether to include the
#'   minimum and maximum values
#' @param includePerc TRUE or FALSE (default) for whether to include 5th to 95th
#'   percentiles
#' @param includeTrialMeans TRUE or FALSE (default) for whether to include the
#'   range of trial means for a given parameter. Note: This is calculated from
#'   individual values rather than being pulled directly from the output.
#' @param concatVariability TRUE (default) or FALSE for whether to concatenate
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
#' @param conc_units What concentration units should be used in the table?
#'   Default is NA to leave the units as is, but if you set the concentration
#'   units to something else, this will attempt to convert the units to match
#'   that. This only adjusts only the simulated values, since we're assuming
#'   that that's the most likely problem and that observed units are relatively
#'   easy to fix, and it also only affects AUC and Cmax values. If you leave
#'   this as NA, the units in the 1st simulation will be used as the units for
#'   \emph{all} the simulations for consistency and clarity. Acceptable input is
#'   any concentration unit
#'   listed in the Excel form for PE data entry, e.g. \code{conc_units =
#'   "ng/mL"} or \code{conc_units = "uM"}. Molar concentrations will be
#'   automatically converted using the molecular weight of whatever you set for
#'   \code{compoundToExtract}.
#' @param include_dose_num NA (default), TRUE, or FALSE on whether to include
#'   the dose number when listing the PK parameter. By default, the parameter
#'   will be labeled, e.g., "Dose 1 Cmax ratio" or "Last dose AUCtau ratio", if
#'   you have PK data for both the first dose and the last dose. Also by
#'   default, if you have data only for the first dose or only for the last
#'   dose, the dose number will be omitted and it will be labeled, e.g., "AUCtau
#'   ratio" or "Cmax ratio". Set this to TRUE or FALSE as desired to override
#'   the default behavior and get exactly what you want.
#' @param add_header_for_DDI TRUE (default) or FALSE for whether to add an extra
#'   header row to the top of your table denoting when the PK are for baseline,
#'   with a perpetrator, or are the geometric mean ratios.
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
#'   \emph{also} want to use the results from \code{pksummary_mult} to make
#'   forest plots, which requires numbers that are \emph{not} rounded.}}
#' @param prettify_columns TRUE (default) or FALSE for whether to make easily
#'   human-readable column names. TRUE makes pretty column names such as "AUCinf
#'   (h*ng/mL)" whereas FALSE leaves the column with the R-friendly name from
#'   \code{\link{extractPK}}, e.g., "AUCinf_dose1".
#' @param prettify_compound_names TRUE (default) or FALSE on whether to make
#'   compound names prettier in the prettified column titles and in any Word
#'   output files. This was designed for simulations where the substrate and any
#'   metabolites, perpetrators, or perpetrator metabolites are among the
#'   standard options for the simulator, and leaving \code{prettify_compound_names =
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
#'   will only work for simulations that included a perpetrator. All PK
#'   parameters in any forest data will be for the victim compound listed. The
#'   data.frame also includes columns for all the compounds that were included
#'   for the simulation, but all PK are for the victim compound listed.
#' @param checkDataSource TRUE or FALSE (default) for whether to include in the
#'   output a data.frame that lists exactly where the data were pulled from the
#'   simulator output file. Useful for QCing.
#' @param highlight_gmr_colors optionally specify a set of colors to use for
#'   highlighting geometric mean ratios for DDIs. Options are "yellow to red",
#'   "green to red" or a vector of 4 colors of your choosing. If left as NA, no
#'   highlighting for GMR level will be done.
#' @param highlight_so_cutoffs optionally specify cutoffs for highlighting any
#'   simulated-to-observed ratios in Word file output. Anything that is above
#'   those values or below the inverse of those values will be highlighted. To
#'   figure out what cells to highlight, this looks for a column titled
#'   "Statistic" or "Stat", then looks for what row contains "S/O" or "simulated
#'   (something something) observed" (as in, we'll use some wildcards to try to
#'   match your specific text). Next, it looks for any values in that same row
#'   that are above those cutoffs. This overrides anything else you specified
#'   for highlighting. The default is NA, for \emph{not} highlighting based on
#'   S/O value. Acceptable input for, say, highlighting values that are > 125\%
#'   or < 80\% of the observed and also, with a second color, values that are >
#'   150\% or < 66\% would be: \code{highlight_so_cutoffs = c(1.25, 1.5)}. If
#'   you would like the middle range of values to be highlighted, include 1 in
#'   your cutoffs. For example, say you would like everything that's < 80\% or >
#'   125\% to be highlighted red but you'd like the "good" values from 80\% to
#'   125\% to be green, you can get that by specifying
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
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.docx" or "My table.csv", depending on whether you'd prefer to have
#'   the table saved as a Word or csv file.  Do not include any slashes, dollar
#'   signs, or periods in the file name. (You can also save the table to a Word
#'   file later with the function \code{\link{formatTable_Simcyp}}.) If you
#'   supply only the file extension, e.g., \code{save_table = "docx"}, the name
#'   of the file will be "PK summary table" with that extension. If you supply
#'   something other than just "docx" or just "csv" for the file name but you
#'   leave off the file extension, we'll assume you want it to be ".csv". All PK
#'   info will be included in a single Word or csv file, and, if
#'   \code{checkDataSource = TRUE}, that will be saved in a single csv file.
#' @param name_clinical_study optionally specify the name(s) of the clinical
#'   study or studies for any observed data. This only affects the caption of
#'   the graph. For example, specifying \code{name_clinical_study = "101, fed
#'   cohort"} will result in a figure caption that reads in part "clinical study
#'   101, fed cohort". If you have more than one study, that's fine; we'll take
#'   care of stringing them together appropriately. Just list them as a
#'   character vector, e.g., \code{name_clinical_study = c("101",
#'   "102", "103")} will become "clinical studies 101, 102, and 103."
#' @param shading_column If you would like to alternate the shading of the rows
#'   in the output table, supply here the unquoted name of the column to check
#'   for when to change the shading; every time that column's value changes, the
#'   shading will alternate between white and light gray. By default, we will
#'   alternate the shading based on the simulation file name. Setting this
#'   argument can be a little bit tricky because we'll be looking for a column
#'   that's present in the \emph{output} from this function, something you might
#'   not know until you run it. If you specify something and the shading doesn't
#'   show up as expected, double check what the final output column names are
#'   and make sure you're using one of those.
#' @param single_table TRUE (default) or FALSE for whether to save all the PK
#'   data in a single table or break the data up by tissue, compound ID, and
#'   file into multiple tables. This only applies to the Word output.
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" (default) or "landscape"
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#' @param ... used under-the-hood to check for mis-specified arguments
#' @param return_PK_pulled TRUE or FALSE (default) for whether to return as a
#'   list item what PK parameters were pulled. This is used internally for
#'   writing table headings later.
#' @param convert_conc_units SOON TO BE DEPRECATED. Please use the argument
#'   "conc_units" instead.
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' # none yet
#' 
pk_table <- function(PKparameters = NA,
                     sim_data_files = NA, 
                     compoundsToExtract = "substrate",
                     tissues = "plasma", 
                     sheet_PKparameters = NA, 
                     existing_exp_details = NA, 
                     mean_type = NA, 
                     use_median_for_tmax = TRUE, 
                     includeCV = TRUE,
                     includeSD = FALSE,
                     includeConfInt = TRUE,
                     includeMedian = FALSE, 
                     includeRange = FALSE,
                     includePerc = FALSE, 
                     includeTrialMeans = FALSE, 
                     concatVariability = TRUE, 
                     variability_format = "to",
                     conc_units = NA, 
                     include_dose_num = NA,
                     PKorder = "default", 
                     file_order = "as is", 
                     add_header_for_DDI = TRUE, 
                     rounding = NA,
                     prettify_columns = TRUE, 
                     prettify_compound_names = TRUE, 
                     name_clinical_study = NA, 
                     extract_forest_data = FALSE, 
                     checkDataSource = FALSE, 
                     save_table = NA, 
                     highlight_gmr_colors = NA, 
                     highlight_so_cutoffs = NA, 
                     highlight_so_colors = "yellow to red", 
                     shading_column, 
                     single_table = TRUE,
                     page_orientation = "portrait", 
                     fontsize = 11, 
                     return_PK_pulled = FALSE, 
                     return_caption = FALSE, 
                     ..., 
                     convert_conc_units = NA){
   
   # Error catching ----------------------------------------------------------
   
   ## General error catching --------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Checking whether they've supplied pksummary_table args instead of
   # pksummary_mult args
   if("sim_data_file" %in% names(match.call()) &
      "sim_data_files" %in% names(match.call()) == FALSE){
      sim_data_files <- sys.call()$sim_data_file
   }
   
   if("compoundToExtract" %in% names(match.call()) &
      "compoundsToExtract" %in% names(match.call()) == FALSE){
      compoundsToExtract <- sys.call()$compoundToExtract
   }
   
   if("tissue" %in% names(match.call()) &
      "tissues" %in% names(match.call()) == FALSE){
      tissues <- sys.call()$tissue
   }
   
   if("convert_conc_units" %in% names(match.call())){
      if("conc_units" %in% names(match.call()) == FALSE){
         conc_units <- convert_conc_units
         warning(wrapn("You have used the argument 'convert_conc_units' to indicate which units to use in your table, and we're deprecating that argument in favor of the argument 'conc_units'. Please use 'conc_units' going forward."), 
                 call. = FALSE)
      } else {
         if(all(convert_conc_units == conc_units, na.rm = T) == FALSE){
            warning(wrapn("You have used both the argument 'conc_units' and the argument 'convert_conc_units' to indicate which units to use in your table, and they do not match. We're deprecating 'convert_conc_units' in favor of 'conc_units', so that is what we will use for your table. Please use 'conc_units' going forward."), 
                    call. = FALSE)
         }
      }
   }
   
   if("observed_PK" %in% names(match.call())){
      if("PKparameters" %in% names(match.call()) == FALSE){
         warning(wrapn("You have supplied an argument called 'observed_PK', which is what the older, soon-to-be-deprecated functions pksummary_table and pksummary_mult used to get observed data. For the pk_table function, we need to have the observed data included with the argument 'PKparameters'. Please check the help file to see how this should be set up. You can see examples for how to supply this by running `make_PK_example_input()`. For now, we will set the argument 'PKparameters' to what you supplied for 'observed_PK'."), 
                 call. = FALSE)
         PKparameters <- sys.call()$observed_PK
      } else {
         warning(wrapn("You have supplied an argument called 'observed_PK', which is what the older, soon-to-be-deprecated functions pksummary_table and pksummary_mult used to get observed data. For the pk_table function, we need to have the observed data included with the argument 'PKparameters'. Please check the help file to see how this should be set up. You can see examples for how to supply this by running `make_PK_example_input()`. Since you did supply something for the argument 'PKparameters' already, we cannot include your observed data."), 
                 call. = FALSE)
      }
   }
   
   # sheet_PKparameters should be length 1 and not be named b/c, if they want
   # more than 1, they need to supply it to PKparameters.
   if(length(sheet_PKparameters) > 1){
      stop(str_wrap("The value for sheet_PKparameters must be only 1 item, and it looks like you have more than that. If you want to specify multiple sheets to use for PK parameters, please specify them by supplying a data.frame to the argument `PKparameters`. You can see examples for how to supply this by running `make_PK_example_input()`."), 
           call. = FALSE)
   }
   
   # If user requested their own order, then omit the DDI header b/c it won't
   # necessarily be clear.
   if(PKorder != "default"){
      add_header_for_DDI <- FALSE
   }
   
   
   ## Harmonizing PKparameters -------------------------------------------------
   
   PKparam_tidied <- tidy_input_PK(PKparameters = PKparameters, 
                                   sim_data_files = sim_data_files, 
                                   compoundsToExtract = compoundsToExtract, 
                                   sheet_PKparameters = sheet_PKparameters, 
                                   tissues = tissues, 
                                   existing_exp_details = existing_exp_details)
   
   # Check for any duplicate observed values b/c that messes up things
   # downstream.
   DupCheck <- PKparam_tidied$PKparameters %>% 
      group_by(File, Sheet, CompoundID, Tissue, PKparameter) %>% 
      summarize(N = n()) %>% 
      filter(N > 1)
   
   if(nrow(DupCheck) > 0){
      message(wrapn(paste0("Warning: There are some duplicate observed PK data included in your input for PKparameters. We can only manage one observed value for each PK parameter, tissue, and compound, so we will have to ignore any duplicates. Specifically, we will ignore the following observed PK:")))
      
      message(paste(paste(capture.output(as.data.frame(DupCheck)), collapse = "\n"), 
                    "\n"))
      
      DupCheck <- DupCheck %>% 
         mutate(ID = paste(File, Sheet, CompoundID, Tissue, PKparameter))
      
      PKparam_tidied$PKparameters <- PKparam_tidied$PKparameters %>% 
         mutate(ID = paste(File, Sheet, CompoundID, Tissue, PKparameter)) %>% 
         filter(!ID %in% DupCheck$ID) %>% 
         select(-ID)
      
   }
   
   existing_exp_details <- PKparam_tidied$existing_exp_details
   PKparameters <- PKparam_tidied$PKparameters %>% 
      mutate(FileExists = file.exists(File)) %>% 
      filter(FileExists == TRUE) %>% select(-FileExists)
   
   ## Misc arg error catching -------------------------------------------------
   
   PKorder <- tolower(PKorder)
   if(PKorder %in% c("default", "user specified") == FALSE){
      warning("You have not supplied a permissible value for the order of PK parameters. Options are `default` or `user specified`. The default PK parameter order will be used.", 
              call. = FALSE)
      PKorder <- "default"
   }
   
   # Possibilities here: 
   
   # 1. There could be only 1 sim, in which case it doesn't matter what they
   # list for file_order, so setting it to "as is".
   
   # 2. There could be more than 1 sim but they've only specified 1 thing for
   # "file_order" rather than specifically listing the order of sims they want.
   # If there's more than 1 sim and only 1 thing for file_order, then file_order
   # must be "as is".
   
   # 3. There is more than 1 sim and more than 1 file listed for file_order.
   # Check that they've got all the sims included and specified correctly, etc.
   
   FilesToInclude <- unique(PKparam_tidied$PKparameters$File)
   
   # Possibility 1
   if(length(sim_data_files) == 1 && 
      complete.cases(sim_data_files)){
      file_order <- FilesToInclude
   } else {
      
      # Possibility 2. Note that we've already established that there is more
      # than 1 item in sim_data_files.
      if(length(file_order) == 1){
         if(tolower(file_order) != "as is"){
            warning(wrapn("You specified something for 'file_order' that we can't interpret, so we'll leave the file order as is."), 
                    call. = FALSE)
         }
         file_order <- FilesToInclude
         
      } else {
         # Possibility 3. Dealing w/all possible ways this could go wrong.
         MissingSims <- setdiff(FilesToInclude, file_order)
         
         if(length(MissingSims) > 0){
            warning(paste0(wrapn("You specified a set of simulations for the argument 'file_order', but they don't include all of the simulations we found from 'sim_data_files'. We'll add the following simulations to the end of the order you specified:"), 
                           str_c(paste0("   ", MissingSims), collapse = "\n"), 
                           "\n"), 
                    call. = FALSE)
            
            file_order <- c(file_order, MissingSims)
         }
         
         ExtraSims <- setdiff(file_order, FilesToInclude)
         if(length(ExtraSims) > 0){
            warning(paste0(wrapn("You specified a set of simulations for the argument 'file_order', but not all of the simulations in 'file_order' are included in 'sim_data_files'. We won't be able to include the following simulations in the order requested because they're not included in 'sim_data_files' (possibly because they are not present):"), 
                           str_c(paste0("   ", ExtraSims), collapse = "\n"), 
                           "\n"), 
                    call. = FALSE)
         }
         
         # file_order should be correct now and include all the files in
         # sim_data_files in the order the user requested plus an missing sims.
      }
   }
   
   # Checking mean type input syntax
   if(complete.cases(mean_type)){
      if(mean_type %in% c("geometric", "arithmetic", 
                          "arithmetic for most, geometric for ratios") == FALSE){
         if(mean_type == "mean"){
            warning(str_wrap("Technically, the input for mean_type should be `geometric` (default), `arithmetic`, or `arithmetic for most, geometric for ratios`. You specified a mean type of `mean`, so we think you want arithmetic means. If that's incorrect, please set mean_type to `geometric`."),
                    call. = FALSE)
         }
         
         mean_type <- case_when(str_detect(tolower(mean_type), "geo") & 
                                   !(str_detect(tolower(mean_type), "arith") &
                                        str_detect(tolower(mean_type), "geo")) ~ "geometric", 
                                mean_type == "mean" ~ "arithmetic", 
                                str_detect(tolower(mean_type), "arith") &
                                   str_detect(tolower(mean_type), "geo") ~ "arithmetic for most, geometric for ratios")
         
         if(mean_type %in% c("geometric", "arithmetic", 
                             "arithmetic for most, geometric for ratios") == FALSE){
            warning("You specified something other than `geometric` (default) `arithmetic`, or `arithmetic for most, geometric for ratios` for the mean type, so we're not sure what you would like. We'll use the default of geometric means.\n", 
                    call. = FALSE)
            
            mean_type <- "geometric"
         }
      }
      
   } else {
      mean_type <- "geometric"
   }
   
   # Now that mean type input is harmonized, set mean type for all scenarios. 
   MeanType <- 
      ifelse(mean_type %in% c("arithmetic", 
                              "arithmetic for most, geometric for ratios"),
             "arithmetic", "geometric")
   GMR_mean_type <- 
      ifelse(mean_type %in% c("geometric", 
                              "arithmetic for most, geometric for ratios"),
             "geometric", "arithmetic")
   
   # Make sure that input to variability_format is ok
   if(variability_format %in% c("to", "hyphen", "brackets", "parentheses") == FALSE){
      warning("The input for variability_format is not among the acceptable options, which are `to`, `hyphen`, `brackets` for square brackets, or `parentheses` for the eponymous symbol if you're an American and a bracket if you're British. We'll use the default of `to`.\n", 
              call. = FALSE)
      variability_format <- "to"
   }
   
   # Checking rounding
   rounding <- tolower(rounding[1])
   rounding <- sub("signif ", "significant ", rounding)
   rounding <- ifelse(is.na(rounding), "consultancy", rounding)
   if(str_detect(rounding, "consultancy|none|significant|round|word only") == FALSE){
      warning(paste0(str_wrap("You have entered something for the rounding argument other than the available options. We'll set this to the default, `Consultancy`. Please check the help file for details."), 
                     "\n"), call. = FALSE)
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
   
   if(any(complete.cases(highlight_gmr_colors)) && 
      tolower(highlight_gmr_colors[1]) == "lisa"){highlight_gmr_colors = "traffic"}
   if(any(complete.cases(highlight_so_colors)) &&
      tolower(highlight_so_colors[1]) == "lisa"){highlight_so_colors = "traffic"}
   
   if(any(complete.cases(highlight_gmr_colors)) &&
      highlight_gmr_colors[1] %in% c("yellow to red", "green to red", "traffic") == FALSE){
      if(length(highlight_gmr_colors) != 4){
         warning("We need 4 colors for highlighting geometric mean ratios, one each for negligible, weak, moderate, and strong interactions, and you have provided a different number of colors. We'll use yellow to red values for highlighting these.\n", 
                 call. = FALSE)
         highlight_gmr_colors <- "yellow to red"
      } else if(tryCatch(is.matrix(col2rgb(highlight_gmr_colors)),
                         error = function(x) FALSE) == FALSE){
         warning("The values you used for highlighting geometric mean ratios are not all valid colors in R. We'll used the default colors instead.\n", 
                 call. = FALSE)
         highlight_gmr_colors <- "yellow to red"
      } 
   }
   
   if(any(complete.cases(highlight_so_colors)) &&
      highlight_so_colors[1] %in% c("yellow to red", "green to red", "traffic") == FALSE &&
      tryCatch(is.matrix(col2rgb(highlight_so_colors)),
               error = function(x) FALSE) == FALSE){
      warning("The values you used for highlighting S/O values are not all valid colors in R. We'll used the default colors instead.\n", 
              call. = FALSE)
      highlight_so_colors <- "yellow to red"
   } 
   
   # If they said "save_output" instead of "save_table", fix that.
   if("save_output" %in% names(match.call())){
      save_table <- sys.call()$save_output
   }
   
   page_orientation <- tolower(page_orientation)[1]
   if(page_orientation %in% c("portrait", "landscape") == FALSE){
      warning("You must specify `portrait` or `landscape` for the argument page_orientation, and you've specified something else. We'll use the default of `portrait`.\n", 
              call. = FALSE)
   }
   
   
   # Main body of function --------------------------------------------------
   
   # Checking what concentration units to use. Using the same units for ALL sims
   # for consistency, clarity, and ease of coding.
   if(is.na(conc_units)){
      conc_units <- existing_exp_details$MainDetails$Units_Cmax[1]
   }
   
   ## Getting simulated data ------------------------------------------------
   MyPKResults <- list()
   PKpulled <- list()
   PKrequested <- list()
   OutQC <- list()
   FD <- list()
   CheckDoseInt <- list()
   
   # Sheet is often NA, which messes up split. Temporarily filling in
   # NA values with "default".
   PKparameters$Sheet[is.na(PKparameters$Sheet)] <- "default"
   
   PKparameters <- split(PKparameters, 
                         f = list(PKparameters$File, 
                                  PKparameters$Sheet, 
                                  PKparameters$CompoundID, 
                                  PKparameters$Tissue))
   
   for(i in names(PKparameters)){
      
      if(nrow(PKparameters[[i]]) == 0){next}
      
      message(paste0(str_wrap(paste0(
         "Extracting PK data from simulation `", 
         unique(PKparameters[[i]]$File),
         "` ", 
         ifelse(is.na(unique(PKparameters[[i]]$Sheet)) ||
                   unique(PKparameters[[i]]$Sheet) == "default", 
                "", paste0("sheet `", 
                           unique(PKparameters[[i]]$Sheet), "` ")), 
         "for the ",
         unique(PKparameters[[i]]$CompoundID), 
         " in ", unique(PKparameters[[i]]$Tissue))), ".\n"))
      
      Deets <- existing_exp_details$MainDetails %>%
         filter(File == unique(PKparameters[[i]]$File))
      
      # Discovery simulations have only 1 tissue. Need to adjust for that. NB: I
      # considered checking for this in the tidy_input_PK function, but it's
      # much easier to include it here since it's specific to each simulation
      # and also not something that often applies AND also may require
      # re-extracting details.
      if(Deets$SimulatorUsed == "Simcyp Discovery"){
         if("PKTissue_Discovery" %in% names(Deets) == FALSE){
            Deets <- extractExpDetails(sim_data_file = i, 
                                       exp_details = "Summary and Input")
            Deets <- Deets[["MainDetails"]]
         }
         
         PKparameters[[i]] <- PKparameters[[i]] %>% 
            filter(Tissue %in% Deets$PKTissue_Discovery)
      }
      
      # message(paste("     for compound =", j))
      # for(k in names(PKparameters[[i]][[j]])){
      #    message(paste("          for tissue =", k))
      #    
      temp <- 
         pk_table_subfun(
            sim_data_file = unique(PKparameters[[i]]$File), 
            PKparameters = PKparameters[[i]], 
            existing_exp_details = existing_exp_details, 
            conc_units = conc_units,
            MeanType = MeanType, 
            GMR_mean_type = GMR_mean_type, 
            includeTrialMeans = includeTrialMeans, 
            use_median_for_tmax = use_median_for_tmax)
      
      if(length(temp) == 0){
         warning(paste0(str_wrap(
            paste0("There were no possible PK parameters to be extracted for the ",
                   unique(PKparameters[[i]]$CompoundID),
                   " in ", unique(PKparameters[[i]]$Tissue), 
                   " for the simulation `", i,
                   "` on the ", 
                   ifelse(is.na(unique(PKparameters[[i]]$Sheet)) || 
                             unique(PKparameters[[i]]$Sheet) == "default", 
                          "regular sheet for the 1st or last-dose PK", 
                          paste0("sheet `", unique(PKparameters[[i]]$Sheet), "`")), 
                   ". Please check your input for 'PKparameters'. For example, check that you have not requested steady-state parameters for a single-dose simulation.")),
            "\n"), call. = FALSE)
         next
      }
      
      # tmax variability stats need to be set differently b/c user will get
      # range if they have requested any variability stats at all.
      temp$PK <- temp$PK %>% 
         mutate(Stat = case_when(str_detect(PKParam, "tmax") & 
                                    Stat == "Minimum" & 
                                    any(c({{includeConfInt}}, 
                                          {{includeCV}}, 
                                          {{includePerc}}, 
                                          {{includeRange}}, 
                                          {{includeSD}})) ~ "tmaxmin", 
                                 
                                 str_detect(PKParam, "tmax") & 
                                    Stat == "Maximum" & 
                                    any(c({{includeConfInt}}, 
                                          {{includeCV}}, 
                                          {{includePerc}}, 
                                          {{includeRange}}, 
                                          {{includeSD}})) ~ "tmaxmax", 
                                 
                                 .default = Stat))
      
      # Formatting to account for variability preferences
      VarOptions <- c("CV" = includeCV & MeanType == "arithmetic", 
                      "GCV" = includeCV & MeanType == "geometric",
                      "CI90_lower" = includeConfInt,
                      "CI90_upper" = includeConfInt, 
                      "CI95_lower" = includeConfInt,
                      "CI95_upper" = includeConfInt, 
                      "Per5" = includePerc, 
                      "Per95" = includePerc, 
                      "MinMean" = includeTrialMeans, 
                      "MaxMean" = includeTrialMeans, 
                      "Minimum" = includeRange, 
                      "Maximum" = includeRange, 
                      "SD" = includeSD, 
                      "Median" = includeMedian, 
                      "Mean" = MeanType == "arithmetic", 
                      "Geomean" = MeanType == "geometric",
                      "tmaxmin" = any(c(includeConfInt, 
                                        includePerc, 
                                        includeRange)), 
                      "tmaxmax" = any(c(includeConfInt, 
                                        includePerc, 
                                        includeRange)), 
                      "S_O_TM_MaxMean" = includeTrialMeans, 
                      "S_O_TM_MinMean" = includeTrialMeans, 
                      "S_O" = TRUE)
      VarOptions <- names(VarOptions)[which(VarOptions)]
      
      MyPKResults[[i]] <- temp$PK %>% filter(Stat %in% VarOptions)
      
      PKpulled[[i]] <-
         data.frame(File = unique(PKparameters[[i]]$File), 
                    CompoundID = unique(PKparameters[[i]]$CompoundID), 
                    Tissue = unique(PKparameters[[i]]$Tissue), 
                    PKpulled = temp$PKpulled, 
                    Sheet = unique(PKparameters[[i]]$Sheet))
      
      PKrequested[[i]] <-
         data.frame(File = unique(PKparameters[[i]]$File), 
                    CompoundID = unique(PKparameters[[i]]$CompoundID), 
                    Tissue = unique(PKparameters[[i]]$Tissue), 
                    PKrequested = temp$PKrequested, 
                    Sheet = unique(PKparameters[[i]]$Sheet))
      
      if(checkDataSource){
         OutQC[[i]] <- temp$QC
      } 
      
      FD[[i]] <- temp$ForestData
      
      CheckDoseInt[[i]] <- temp$CheckDoseInt
      
   }
   
   if(length(MyPKResults) == 0){
      warning("No PK data could be found in the files ", 
              str_comma(paste0("`", sim_data_files, "`")), "\n",
              call. = FALSE)
      return(list())
   }
   
   PKparameters <- bind_rows(PKparameters) %>% 
      mutate(Sheet = ifelse(Sheet == "default", NA, Sheet))
   MyPKResults <- bind_rows(MyPKResults)
   PKpulled <- bind_rows(PKpulled)
   PKrequested <- bind_rows(PKrequested)
   OutQC <- bind_rows(OutQC)
   FD <- bind_rows(FD)
   CheckDoseInt <- list("message" = purrr::map(CheckDoseInt, "message") %>% 
                           unlist(), 
                        "interval" = purrr::map(CheckDoseInt, "interval") %>%
                           bind_rows())
   
   # Formatting --------------------------------------------------------------
   
   # Formatting and selecting only rows where there are data. Also removing any
   # PKparameters where we have only observed data, which can happen if user
   # specifies a PK parameter w/out a dose number but then does not specify a
   # custom interval tab. That messes up other formatting down the line
   # w/writing to Word.
   MyPKResults <- MyPKResults %>%
      mutate(Value = if_else(str_detect(Stat, "CV"), 
                             round_opt(100*Value, rounding),
                             round_opt(Value, rounding)))
   
   # Checking for any PK parameters where there are no simulated data.
   GoodPKParam <- MyPKResults %>% 
      filter(Stat == switch(MeanType, 
                            "geometric" = "Geomean", 
                            "arithmetic" = "Mean") &
                SorO == "Sim" &
                complete.cases(Value)) %>% pull(PKParam) %>% unique()
   
   MyPKResults <- MyPKResults %>%
      filter(PKParam %in% GoodPKParam &
                complete.cases(Value)) %>% unique() 
   
   TmaxStuff <- MyPKResults %>% 
      filter(str_detect(PKParam, "tmax") & 
                !Stat %in% c("Geomean", "Mean", "Median", "S_O")) %>% 
      mutate(Stat = case_when(Stat == "tmaxmin" &
                                 {{includeRange}} ~ "Minimum", 
                              str_detect(PKParam, "tmax") & 
                                 Stat == "tmaxmax" &
                                 {{includeRange}} ~ "Maximum", 
                              
                              Stat == "tmaxmin" &
                                 {{includeConfInt}} ~ "CI90_lower", 
                              
                              Stat == "tmaxmax" &
                                 {{includeConfInt}} ~ "CI90_upper", 
                              
                              Stat == "tmaxmin" &
                                 {{includePerc}} ~ "Per5", 
                              
                              Stat == "tmaxmax" &
                                 {{includePerc}} ~ "Per95")) %>% 
      filter(complete.cases(Stat)) %>% 
      unique()
   
   MyPKResults <- MyPKResults %>% 
      filter(!str_detect(PKParam, "tmax") |
                (str_detect(PKParam, "tmax") & 
                    Stat %in% c("Geomean", "Mean", "Median", "S_O"))) %>% 
      bind_rows(TmaxStuff) %>% 
      pivot_wider(names_from = PKParam, values_from = Value) %>%
      mutate(SorO = factor(SorO, levels = c("Sim", "Obs", "S_O", "S_O_TM")), 
             Stat = factor(Stat, levels = unique(
                AllStats$InternalColNames[
                   which(complete.cases(AllStats$InternalColNames))]))) %>% 
      arrange(File, SorO, Stat) %>% 
      filter(if_any(.cols = -c(Stat, SorO), .fns = complete.cases)) %>% 
      mutate(across(.cols = everything(), .fns = as.character)) 
   
   rm(GoodPKParam)
   
   # Putting trial means into appropriate format
   if(includeTrialMeans){
      TM <- MyPKResults %>% 
         filter(Stat %in% c("MinMean", "MaxMean")) %>%
         summarize(across(.cols = -c(Stat, SorO, Sheet, CompoundID, 
                                     Tissue, File),
                          .fns = function(x) {paste(x[1], "to", x[2])})) 
      
      MyPKResults <- MyPKResults %>%
         filter(Stat != "MaxMean")
      
      for(col in names(TM)){
         MyPKResults[which(MyPKResults$Stat == "MinMean"), col] <- 
            TM[1, col]
      }
      
      MyPKResults$Stat[which(MyPKResults$Stat == "MinMean")] <- "TrialMeanRange"
      
      TM_SO <- MyPKResults %>% 
         filter(Stat %in% c("S_O_TM_MinMean", "S_O_TM_MaxMean")) %>%
         summarize(across(.cols = -c(Stat, SorO, Sheet, CompoundID, 
                                     Tissue, File),
                          .fns = function(x) {paste(x[1], "to", x[2])})) %>% 
         mutate(across(.cols = everything(), 
                       .fns = function(x) ifelse(x == "NA to NA",
                                                 as.character(NA), x)))
      
      if(nrow(TM_SO) > 0){
         MyPKResults <- MyPKResults %>%
            filter(Stat != "S_O_TM_MaxMean")
         
         for(col in names(TM_SO)){
            MyPKResults[which(MyPKResults$Stat == "S_O_TM_MinMean"), col] <- 
               TM_SO[1, col]
         }
         
         MyPKResults$Stat[which(MyPKResults$Stat == "S_O_TM_MinMean")] <- "S_O_TM_Range"
      }
   }
   
   # Concatenating the rows w/lower and upper limits of variability when
   # requested
   if(concatVariability){
      
      # This must be done one file, sheet, compound, and tissue at a time.
      # First, replacing any NA values for Sheet w/"default" so that it won't
      # mess up splitting.
      MyPKResults$Sheet[is.na(MyPKResults$Sheet)] <- "default"
      
      MyPKResults <- split(MyPKResults, 
                           f = list(MyPKResults$File, 
                                    MyPKResults$Sheet, 
                                    MyPKResults$Tissue, 
                                    MyPKResults$CompoundID, 
                                    MyPKResults$SorO))
      
      for(i in names(MyPKResults)){
         if(nrow(MyPKResults[[i]]) == 0){next}
         
         # All possible places where we need to concatenate the variability
         VarRows <- list("CI90concat" = c("CI90_lower", "CI90_upper"), 
                         "CI95concat" = c("CI95_lower", "CI95_upper"),
                         "Per95concat" = c("Per5", "Per95"), 
                         "Rangeconcat" = c("Minimum", "Maximum"))
         # Only the ones that should be present in our data
         VarRows <- VarRows[sapply(VarRows, function(x) unlist(x[[1]])) %in% VarOptions]
         
         for(j in names(VarRows)){
            temp <- MyPKResults[[i]] %>%
               filter(Stat %in% as.character(unlist(VarRows[[j]])))
            
            if(nrow(temp) == 0){
               rm(temp)
               next
            }
            
            temp <- temp %>%
               mutate(across(.cols = !any_of(c("Stat", "SorO", "Statistic",
                                               "File", "Sheet", 
                                               "CompoundID", "Tissue")),
                             .fns = function(x) {
                                ifelse(all(complete.cases(c(x[1], x[2]))),
                                       switch(variability_format, 
                                              "to" = paste(x[1], "to", x[2]),
                                              "hyphen" = paste(x[1], "-", x[2]),
                                              "brackets" = paste0("[", x[1], ", ", x[2], "]"), 
                                              "parentheses" = paste0("(", x[1], ", ", x[2], ")")),
                                       NA)}),
                      Stat = j)
            
            MyPKResults[[i]][which(MyPKResults[[i]]$Stat == VarRows[[j]][1]), ] <-
               temp[1, ]
            MyPKResults[[i]] <- MyPKResults[[i]] %>% filter(Stat != VarRows[[j]][2])
            rm(temp)
         }
      }
      
      MyPKResults <- bind_rows(MyPKResults) %>% 
         mutate(Sheet = ifelse(Sheet == "default", NA, Sheet)) 
      
      if(includeConfInt == FALSE){
         MyPKResults <- MyPKResults %>% filter(!Stat %in% c("CI90concat", 
                                                            "CI95concat"))
      }
      
      if(includeRange == FALSE){
         MyPKResults <- MyPKResults %>% filter(!Stat %in% c("Rangeconcat"))
      }
      
      if(includePerc == FALSE){
         MyPKResults <- MyPKResults %>% filter(!Stat %in% c("Per95concat"))
      }
   }
   
   # StatNames <- c("Geomean" = "Simulated",
   #                "Mean" = "Simulated",
   #                "GCV" = "CV%",
   #                "CV" = "CV%",
   #                "SD" = "Standard deviation",
   #                "CI90_lower" = "90% CI - Lower",
   #                "CI90_upper" = "90% CI - Upper",
   #                "CI90concat" = "90% CI",
   #                "CI95_lower" = "95% CI - Lower",
   #                "CI95_upper" = "95% CI - Upper",
   #                "CI95concat" = "95% CI",
   #                "Per5" = "5th Percentile",
   #                "Per95" = "95th Percentile",
   #                "Per95concat" = "5th to 95th Percentile",
   #                "Minimum" = "Minimum", 
   #                "Maximum" = "Maximum",
   #                "Median" = "Median",
   #                "Rangeconcat" = "Range",
   #                
   #                "geomean_obs" = "Observed",
   #                "mean_obs" = "Observed", 
   #                "CV_obs" = "Observed CV%",
   #                "GCV_obs" = "Observed CV%", 
   #                "CIL_obs" = "observed CI - Lower",
   #                "CIU_obs" = "observed CI - Upper",
   #                "CIconcat_obs" = "Observed CI",
   #                "Rangeconcat_obs" = "Observed range", 
   #                
   #                "S_O" = "S/O",
   #                "S_O_TM_MinMean" = "S/O range for trial means",
   #                "MinMean" = "Range of trial means")
   
   MyPKResults <- MyPKResults %>%
      mutate(Statistic = ifelse(SorO == "Obs", 
                                paste0(Stat, "_obs"), Stat),
             Statistic = renameStats(Statistic, use = "report"), 
             Statistic = ifelse(SorO == "Obs" & Statistic == "Simulated", 
                                "Observed", Statistic), 
             SorO = factor(SorO, levels = c("Sim", "Obs", "S_O", "S_O_TM")), 
             Stat = factor(Stat, levels = 
                              unique(AllStats$InternalColNames[
                                 which(complete.cases(AllStats$InternalColNames))])), 
             File = factor(File, levels = file_order)) %>% 
      arrange(File, CompoundID, SorO, Stat) %>% 
      filter(if_any(.cols = -c(Stat, SorO), .fns = complete.cases)) %>% 
      mutate(across(.cols = everything(), .fns = as.character)) %>% 
      select(-Stat, -SorO) %>%
      select(Statistic, everything())
   
   # setting levels for PK parameters so that they're in a nice order. 
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
                      "user specified" = PKparameters$PKparameter)
   
   MyPKResults <- MyPKResults %>%
      select(any_of(c("Statistic", 
                      unique(as.character(PKlevels)))), 
             everything()) %>% 
      relocate(File, .after = last_col())
   
   # Checking for whether to include AUCinf, AUCt, or both for dose 1 based on
   # what user requested initially and whether there were any problems with
   # extrapolation. If there were problems with extrapolation for either
   # AUCinf_dose1 OR for AUCinf_dose1_withInhib, then we want only AUCt values
   # b/c need to be able to make the correct comparison. If there were no
   # problems, then we only want AUCinf values unless they speicifcally
   # requested AUCt.
   
   # Any time AUCinf_dose1 was requested, only retain any AUCt_X that were
   # specfically requested or when AUCinf could not be returned.
   if("AUCinf_dose1" %in% PKpulled$PKpulled & 
      "AUCt_dose1" %in% PKparam_tidied$PKparameters$PKparameter[
         PKparam_tidied$PKparameters$OriginallyRequested == TRUE] == FALSE){
      MyPKResults <- MyPKResults %>% select(-any_of("AUCt_dose1"))
      PKpulled <- PKpulled %>% filter(!PKpulled %in% "AUCt_dose1")
   }
   
   if("AUCinf_dose1_withInhib" %in% PKpulled$PKpulled & 
      "AUCt_dose1_withInhib" %in% PKparam_tidied$PKparameters$PKparameter[
         PKparam_tidied$PKparameters$OriginallyRequested == TRUE] == FALSE){
      MyPKResults <- MyPKResults %>% select(-any_of("AUCt_dose1_withInhib"))
      PKpulled <- PKpulled %>% filter(!PKpulled %in% "AUCt_dose1_withInhib")
   }
   
   if("AUCinf_ratio_dose1" %in% PKpulled$PKpulled & 
      "AUCt_ratio_dose1" %in% PKparam_tidied$PKparameters$PKparameter[
         PKparam_tidied$PKparameters$OriginallyRequested == TRUE] == FALSE){
      MyPKResults <- MyPKResults %>% select(-any_of("AUCt_ratio_dose1"))
      PKpulled <- PKpulled %>% filter(!PKpulled %in% "AUCt_ratio_dose1")
   }
   
   # Optionally adding final column names
   if(prettify_columns){
      
      # Step 1 for setting column names. Note that this does NOT YET account for
      # the possibility that there were multiple user-defined intervals.
      ColNames <- prettify_column_names(MyPKResults,
                                        return_which_are_PK = TRUE) %>% 
         mutate(# Checking position of columns with custom intervals.
            CustomInt = ColName %in% AllPKParameters$PKparameter_nodosenum)
      
      # Adding time interval to any data that came from custom AUC interval
      # sheets.
      if(any(complete.cases(PKparameters$Sheet)) &
         nrow(CheckDoseInt$interval) > 0){ 
         
         # Dealing with instances where we just don't have the info needed
         UselessCheckDoseInt <- CheckDoseInt$message %in% 
            c("can't check - missing file")
         UselessCheckDoseInt <- UselessCheckDoseInt[which(UselessCheckDoseInt)]
         
         if(length(UselessCheckDoseInt) > 0){
            UselessCheckDoseInt <- names(UselessCheckDoseInt)
            CheckDoseInt$interval <- CheckDoseInt$interval %>% 
               unite(col = "ID", File, Sheet, CompoundID, Tissue, sep = ".", 
                     remove = FALSE) %>% 
               filter(!ID %in% UselessCheckDoseInt)
         }
         
         IntToAdd <- CheckDoseInt$interval %>% 
            filter(Sheet %in% PKparameters$Sheet[complete.cases(PKparameters$Sheet)]) %>% 
            select(File, Sheet, Interval)
         
         # There could be multiple user-defined intervals; accounting for that.
         MyPKResults <- MyPKResults %>% 
            pivot_longer(cols = any_of(ColNames$ColName[ColNames$IsPK == TRUE]), 
                         names_to = "PKparameter", 
                         values_to = "Value") %>% 
            left_join(IntToAdd, by = join_by(Sheet, File)) %>% 
            mutate(PKparameter = ifelse(is.na(Interval), 
                                        PKparameter, 
                                        paste(PKparameter, Interval))) %>% 
            select(-Interval, -Sheet) %>%
            filter(complete.cases(Value)) %>% 
            pivot_wider(names_from = PKparameter,
                        values_from = Value)
         
         ColNames <- ColNames %>% 
            left_join(expand_grid(Interval = unique(IntToAdd$Interval), 
                                  ColName = ColNames$ColName) %>% 
                         mutate(ColName_int = paste(ColName, Interval)) %>% 
                         filter(ColName_int %in% names(MyPKResults)), 
                      by = join_by(ColName)) %>% 
            mutate(ColName = ifelse(is.na(ColName_int), ColName, ColName_int)) %>% 
            select(-ColName_int) %>% 
            mutate(
               UnitsToAdd = case_when(
                  CustomInt == TRUE ~ str_extract(PrettifiedNames, 
                                                  " \\(h\\)| \\(ng/mL(.h)?\\)| \\(L/h\\)"), 
                  .default = ""), 
               
               PrettifiedNames = case_when(
                  CustomInt == TRUE ~ str_replace(PrettifiedNames, " \\(h\\)| \\(ng/mL(.h)?\\)| \\(L/h\\)", ""), 
                  .default = PrettifiedNames),
               
               PrettifiedNames = case_when(
                  CustomInt == TRUE & complete.cases(UnitsToAdd) ~ 
                     paste0(PrettifiedNames, " for interval ", Interval, UnitsToAdd), 
                  CustomInt == TRUE & is.na(UnitsToAdd) ~ 
                     paste0(PrettifiedNames, " for interval ", Interval), 
                  .default = PrettifiedNames)) %>% 
            # We had to remove the sheet or this would have unnecessary rows. Remove that column name. 
            filter(ColName != "Sheet")
      }
      
      # Adjusting units as needed.
      ColNames$PrettifiedNames <- sub("\\(ng/mL.h\\)",
                                      paste0("(", conc_units, ".h)"), 
                                      ColNames$PrettifiedNames)
      
      ColNames$PrettifiedNames <- sub("\\(ng/mL\\)", 
                                      paste0("(", conc_units, ")"),
                                      ColNames$PrettifiedNames)
      
      ColNames$PrettifiedNames <- gsub("ug/mL", "µg/mL", ColNames$PrettifiedNames)
      
      MyPerpetrator <- determine_myperpetrator(existing_exp_details,
                                               prettify_compound_names)
      
      if(any(complete.cases(MyPerpetrator))){
         ColNames$PrettifiedNames <- sub("perpetrator", MyPerpetrator, ColNames$PrettifiedNames)
      }
      
      MyPKResults <- MyPKResults[, ColNames$ColName]
      names(MyPKResults) <- ColNames$PrettifiedNames
      
   } 
   
   include_dose_num_orig <- include_dose_num
   include_dose_num <- check_include_dose_num(MyPKResults, 
                                              include_dose_num)
   
   if(include_dose_num == FALSE){
      names(MyPKResults) <- sub("Dose 1 |Last dose |_dose1$|_last$", "", 
                                names(MyPKResults))
   }
   
   if(checkDataSource){
      
      ColsToInclude <- c("PKparam", "File", "Tab", 
                         switch(MeanType,
                                "arithmetic" = "mean",
                                "geometric" = "geomean"))
      
      if(length(PKpulled$PKpulled) > 0 && any(str_detect(PKpulled$PKpulled, "tmax"), na.rm = T)){
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
      
      OutQC <- OutQC %>% 
         select(PKparam, File, matches(ColsToInclude))
      
   }
   
   Out <- list("Table" = MyPKResults)
   
   
   # Setting up table caption ------------------------------------------------
   
   
   MyPerpetrator <- determine_myperpetrator(Deets = Deets, 
                                            prettify_compound_names = TRUE)
   
   DosesIncluded <- c("Dose1" = any(str_detect(PKparameters$PKparameter, "_dose1")),
                      "Last" = any(str_detect(PKparameters$PKparameter, "_last")), 
                      "User" = any(complete.cases(PKparameters$Sheet)))
   DosesIncluded <- str_c(names(DosesIncluded)[DosesIncluded], collapse = " ")
   
   Annotations <- make_table_annotations(
      MyPKResults = MyPKResults %>% purrr::discard(~all(is.na(.))), 
      MyFile = unique(MyPKResults$File), 
      MyCompoundID = unique(MyPKResults$CompoundID), 
      prettify_compound_names = prettify_compound_names,
      existing_exp_details = existing_exp_details, 
      mean_type = mean_type, 
      DosesIncluded = case_match(DosesIncluded, 
                                 "Dose1 User" ~ "Dose1 Last", 
                                 "Last User" ~ "Last", 
                                 .default = DosesIncluded), 
      tissue = unique(MyPKResults$Tissue), 
      name_clinical_study = name_clinical_study)
   
   if(return_caption){
      Out[["table_heading"]] <- Annotations$table_heading
      Out[["table_caption"]] <- Annotations$table_caption
   } 
   
   if(single_table == FALSE){
      
      Annotations <- list("table_heading" = "", 
                          "table_caption" = "")
   }
   
   
   # Saving --------------------------------------------------------------
   
   # May need to change the working directory temporarily, so
   # determining what it is now
   CurrDir <- getwd()
   
   
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
      }
      
      # Now that the file should have an appropriate extension, check what
      # the path and basename should be.
      OutPath <- dirname(save_table)
      
      if(OutPath == "."){
         OutPath <- getwd()
      }
      
      save_table <- basename(save_table)
      setwd(OutPath)
      
      if(str_detect(save_table, "docx")){ 
         # This is when they want a Word file as output
         
         FromCalcPKRatios <- FALSE
         TemplatePath <- switch(page_orientation, 
                                "landscape" = system.file("Word/landscape_report_template.dotx",
                                                          package="SimcypConsultancy"), 
                                "portrait" = system.file("Word/report_template.dotx",
                                                         package="SimcypConsultancy"))
         
         rmarkdown::render(
            system.file("rmarkdown/templates/pktable/skeleton/skeleton.Rmd", 
                        package="SimcypConsultancy"),
            output_format = rmarkdown::word_document(reference_docx = TemplatePath), 
            output_dir = OutPath, 
            output_file = save_table, 
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
      
      setwd(CurrDir)
      
   }
   
   if(checkDataSource){
      Out[["QC"]] <- OutQC
      
      if(complete.cases(save_table)){
         
         write.csv(OutQC, sub(".csv|.docx", " - QC.csv", save_table), row.names = F)
         
      }
   }
   
   if(extract_forest_data){
      Out[["ForestData"]] <- FD
      
      if(complete.cases(save_table)){ 
         write.csv(FD, sub(".csv|.docx", " - forest data.csv", save_table), row.names = F)
      }
   }
   
   if(return_PK_pulled){
      Out[["PKpulled"]] <- PKpulled
   }
   
   if(length(Out) == 1){
      Out <- Out[[1]]
   }
   
   return(Out)
   
}



