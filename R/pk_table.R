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
#'   specify what you need in terms of tissues, which compounds, which
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
#'   columns that will be read are "File" (same thing as the argument
#'   \code{sim_data_files}), "Sheet" (same thing as the argument
#'   \code{sheet_PKparameters}), "Tissue" (same as the argument \code{tissues}),
#'   "CompoundID" (same as the argument \code{compoundsToExtract}), "Value" for
#'   any observed data (no equivalent argument), and "Variability" for any
#'   observed variability (no equivalent argument here, either). If you
#'   omit any of those columns, whatever you supply for their respective
#'   arguments will be used instead. If you supply something for one of them
#'   in the data.frame or file and \emph{also} something for its argument, the
#'   argument will be ignored. Here is how to specify each of the possible
#'   inputs for Option 1:\describe{\item{a csv file}{list the file
#'   name, including the file extension ".csv" inside quotes, e.g., "PK needed.csv"}
#'
#'   \item{an Excel file}{list the file name, including the file extension
#'   ".xlsx", inside quotes, e.g., "PK needed.xlsx". We will be looking for a
#'   tab titled "PKparameters" (all one word and with the same capitalization).}
#'
#'   \item{a data.frame}{If you'd like to supply a data.frame with the same
#'   columns you would have had in the .csv or Excel file, that works just the
#'   same.}}}
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
#'   perpetrator -- will not be included.
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
#' @param mean_type What kind of means and CVs do you want listed in the output
#'   table? Options are "arithmetic" or "geometric" (default).
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
#' @param convert_conc_units Would you like to convert the units to something
#'   other than what was used in the simulation? Default is NA to leave the
#'   units as is, but if you set the concentration units to something else, this
#'   will attempt to convert the units to match that. This only adjusts only the
#'   simulated values, since we're assuming that that's the most likely problem
#'   and that observed units are relatively easy to fix, and it also only
#'   affects AUC and Cmax values. Acceptable input is any concentration unit
#'   listed in the Excel form for PE data entry, e.g. \code{convert_conc_units =
#'   "ng/mL"} or \code{convert_conc_units = "uM"}. Molar concentrations will be
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
#' @param extract_forest_data TRUE or FALSE (default) to get forest-plot data at
#'   the same time. This only applies when the compound to extract is the
#'   substrate or a substrate metabolite. If set to TRUE, this will return a
#'   list that includes data formatted for use with the function
#'   \code{\link{forest_plot}}. Since the \code{\link{forest_plot}} function
#'   only works with simulations with perpetrators (at least, for now), this
#'   will only work for simulations that included a perpetrator.
#' @param checkDataSource TRUE (default) or FALSE for whether to include in the
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
#' @param single_table TRUE (default) or FALSE for whether to save all the PK
#'   data in a single table or break the data up by tissue, compound ID, and
#'   file into multiple tables. This only applies to the Word output.
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" (default) or "landscape"
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#' @param ...
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
                     concatVariability = FALSE, 
                     variability_format = "to",
                     convert_conc_units = NA, 
                     include_dose_num = NA,
                     PKorder = "default", 
                     add_header_for_DDI = TRUE, 
                     rounding = NA,
                     prettify_columns = TRUE, 
                     prettify_compound_names = TRUE, 
                     extract_forest_data = FALSE, 
                     checkDataSource = TRUE, 
                     highlight_gmr_colors = NA, 
                     highlight_so_cutoffs = NA, 
                     highlight_so_colors = "yellow to red", 
                     save_table = NA, 
                     single_table = FALSE,
                     page_orientation = "portrait", 
                     fontsize = 11, 
                     return_PK_pulled = FALSE, 
                     ...){
   
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
   
   # sheet_PKparameters should be length 1 and not be named b/c, if they want
   # more than 1, they need to supply it to PKparameters.
   if(length(sheet_PKparameters) > 1){
      stop(str_wrap("The value for sheet_PKparameters must be only 1 item, and it looks like you have more than that. If you want to specify multiple sheets to use for PK parameters, please specify them by supplying a data.frame to the argument `PKparameters`. You can see examples for how to supply this by running `make_PK_example_input()`."), 
           call. = FALSE)
   }
   
   ## Harmonizing PKparameters -------------------------------------------------
   
   PKparam_tidied <- tidy_input_PK(PKparameters = PKparameters, 
                                   sim_data_files = sim_data_files, 
                                   compoundsToExtract = compoundsToExtract, 
                                   sheet_PKparameters = sheet_PKparameters, 
                                   existing_exp_details = existing_exp_details)
   
   existing_exp_details <- PKparam_tidied$existing_exp_details
   PKparameters <- PKparam_tidied$PKparameters
   
   ## Misc arg error catching -------------------------------------------------
   
   PKorder <- tolower(PKorder)
   if(PKorder %in% c("default", "user specified") == FALSE){
      warning("You have not supplied a permissible value for the order of PK parameters. Options are `default` or `user specified`. The default PK parameter order will be used.", 
              call. = FALSE)
      PKorder <- "default"
   }
   
   # Checking mean type input syntax
   if(complete.cases(mean_type)){
      if(mean_type %in% c("geometric", "arithmetic", 
                          "arithmetic for most, geometric for ratios") == FALSE){
         if(mean_type == "mean"){
            warning(paste0(str_wrap("Technically, the input for mean_type should be `geometric` (default), `arithmetic`, or `arithmetic for most, geometric for ratios`. You specified a mean type of `mean`, so we think you want arithmetic means. If that's incorrect, please set mean_type to `geometric`."),
                           "\n"), call. = FALSE)
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
   
   if(complete.cases(highlight_gmr_colors) && 
      tolower(highlight_gmr_colors[1]) == "lisa"){highlight_gmr_colors = "traffic"}
   if(complete.cases(highlight_so_colors) &&
      tolower(highlight_so_colors[1]) == "lisa"){highlight_so_colors = "traffic"}
   
   if(any(complete.cases(highlight_gmr_colors)) &&
      highlight_gmr_colors[1] %in% c("yellow to red", "green to red", "traffic") == FALSE){
      if(length(highlight_gmr_colors) != 4){
         warning("We need 4 colors for highlighting geometric mean ratios, one each for negligible, weak, moderate, and strong interactions, and you have provided a different number of colors. We'll use yellow to red values for highlighting these.\n", 
                 call. = FALSE)
         highlight_gmr_colors <- "yellow to red"
      } else if(is.matrix(col2rgb(highlight_gmr_colors)) == FALSE){
         warning("The values you used for highlighting geometric mean ratios are not all valid colors in R. We'll used the default colors instead.\n", 
                 call. = FALSE)
         highlight_gmr_colors <- "yellow to red"
      } 
   }
   
   if(any(complete.cases(highlight_so_colors)) &&
      highlight_so_colors[1] %in% c("yellow to red", "green to red", "traffic") == FALSE &&
      is.matrix(col2rgb(highlight_so_colors)) == FALSE){
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
      suppressWarnings(
         temp <- 
            pk_table_subfun(
               sim_data_file = unique(PKparameters[[i]]$File), 
               PKparameters = PKparameters[[i]], 
               existing_exp_details = existing_exp_details, 
               convert_conc_units = convert_conc_units,
               MeanType = MeanType, 
               GMR_mean_type = GMR_mean_type, 
               includeTrialMeans = includeTrialMeans, 
               use_median_for_tmax = use_median_for_tmax)
      )
      
      if(length(temp) == 0){
         warning(paste0(str_wrap(
            paste0("There were no possible PK parameters to be extracted for the ",
                   j, " in ", k, " for the simulation `", i,
                   "` on the ", 
                   ifelse(is.na(ss), 
                          "regular sheet for the 1st or last-dose PK", 
                          paste0("sheet `", ss, "`")), 
                   ". Please check your input for 'PKparameters'. For example, check that you have not requested steady-state parameters for a single-dose simulation.")),
            "\n"), call. = FALSE)
         next
      }
      
      # Formatting to account for variability preferences
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
                      "min" = includeRange | any(str_detect(temp$PK$PKParam, "tmax")), 
                      "max" = includeRange | any(str_detect(temp$PK$PKParam, "tmax")), 
                      "SD" = includeSD, 
                      "median" = includeMedian, 
                      "mean" = MeanType == "arithmetic", 
                      "geomean" = MeanType == "geometric", 
                      "S_O" = TRUE)
      VarOptions <- names(VarOptions)[which(VarOptions)]
      
      MyPKResults[[i]] <- temp$PK %>%
         filter(Stat %in% VarOptions)
      
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
   
   MyPKResults <- MyPKResults %>% 
      filter(Stat %in% c(case_match(MeanType, 
                                    "geometric" ~ "geomean",
                                    "arithmetic" ~ "mean", 
                                    "median" ~ "median"),
                         "CI90_low", "CI90_high", "CI95_low", "CI95_high",
                         "min", "max", "per5", "per95", 
                         case_match(MeanType, 
                                    "geometric" ~ "GCV",
                                    "arithmetic" ~ "CV", 
                                    "median" ~ NA),
                         "MinMean", "MaxMean", 
                         "S_O_TM_MinMean", "S_O_TM_MaxMean",
                         "S_O", "SD", "median"))
   
   # If there were any observed data that were a range, e.g., for tmax, then put
   # the range on a single line, even if user did not request concatVariability
   # b/c it just makes things so much easier.
   if(nrow(MyPKResults %>% filter(Stat == "min" & SorO == "Obs")) > 0){
      
      MyPKResults <- MyPKResults %>% 
         pivot_wider(names_from = Stat, 
                     values_from = Value) %>% 
         mutate(min = case_when(complete.cases(min) & 
                                   complete.cases(max) ~ 
                                   switch(variability_format, 
                                          "to" = paste(min, "to", max), 
                                          "hyphen" = paste(min, "-", max), 
                                          "brackets" = paste0("[", min, ", ", max, "]"),
                                          "parentheses" = paste0("(", min, ", ", max, ")")), 
                                TRUE ~ min)) %>% 
         select(-max) %>% 
         pivot_longer(cols = -c(PKParam, SorO), 
                      names_to = "Stat", 
                      values_to = "Value") %>% 
         mutate(Stat = ifelse(Stat == "min", 
                              switch(MeanType, 
                                     "geometric" = "GCV", 
                                     "arithmetic" = "CV"), 
                              Stat))
   }
   
   # Checking for any PK parameters where there are no simulated data.
   GoodPKParam <- MyPKResults %>% 
      filter(Stat == switch(MeanType, 
                            "geometric" = "geomean", 
                            "arithmetic" = "mean") &
                SorO == "Sim" &
                complete.cases(Value)) %>% pull(PKParam) %>% unique()
   
   MyPKResults <- MyPKResults %>%
      filter(PKParam %in% GoodPKParam &
                complete.cases(Value)) %>% unique() %>%
      pivot_wider(names_from = PKParam, values_from = Value) %>%
      mutate(SorO = factor(SorO, levels = c("Sim", "Obs", "S_O", "S_O_TM")), 
             Stat = factor(Stat, levels = c("mean", "geomean", "median",
                                            "CV", "GCV", 
                                            "min", "max",
                                            "CI90_low", "CI90_high", "CI95_low", 
                                            "CI95_high", "per5", "per95",
                                            "MinMean", "MaxMean", 
                                            "SD", "S_O", 
                                            "S_O_TM_MinMean", 
                                            "S_O_TM_MaxMean"))) %>% 
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
            MyPKResults[which(MyPKResults$Stat == "MinMean"), col] <- 
               TM_SO[1, col]
         }
      }
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
                  "S_O_TM_MinMean" = "S/O range for trial means",
                  "MinMean" = "Range of trial means")
   
   MyPKResults <- MyPKResults %>%
      mutate(Statistic = as.character(Stat),
             Statistic = StatNames[Statistic], 
             Statistic = ifelse(SorO == "Obs" & Statistic == "Simulated", 
                                "Observed", Statistic), 
             SorO = factor(SorO, levels = c("Sim", "Obs", "S_O", "S_O_TM")), 
             Stat = factor(Stat, levels = c("mean", "geomean", "median",
                                            "CV", "GCV", 
                                            "min", "max",
                                            "CI90_low", "CI90_high", "CI95_low", 
                                            "CI95_high", "per5", "per95",
                                            "MinMean", "MaxMean", 
                                            "SD", "S_O", 
                                            "S_O_TM_MinMean", 
                                            "S_O_TM_MaxMean"))) %>% 
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
      "AUCt_dose1" %in% PKrequested$PKrequested == FALSE){
      MyPKResults <- MyPKResults %>% select(-any_of("AUCt_dose1"))
      PKpulled <- PKpulled %>% filter(!PKpulled %in% "AUCt_dose1")
   }
   
   if("AUCinf_dose1_withInhib" %in% PKpulled$PKpulled & 
      "AUCt_dose1_withInhib" %in% PKrequested$PKrequested == FALSE){
      MyPKResults <- MyPKResults %>% select(-any_of("AUCt_dose1_withInhib"))
      PKpulled <- PKpulled %>% filter(!PKpulled %in% "AUCt_dose1_withInhib")
   }
   
   if("AUCinf_ratio_dose1" %in% PKpulled$PKpulled & 
      "AUCt_ratio_dose1" %in% PKrequested$PKrequested == FALSE){
      MyPKResults <- MyPKResults %>% select(-any_of("AUCt_ratio_dose1"))
      PKpulled <- PKpulled %>% filter(!PKpulled %in% "AUCt_ratio_dose1")
   }
   
   # Optionally adding final column names
   if(prettify_columns){
      
      ColNames <- data.frame(Orig = names(MyPKResults)) %>% 
         mutate(Pretty = prettify_column_names(PKtable = Orig, 
                                               prettify_compound_names = prettify_compound_names, 
                                               pretty_or_ugly_cols = "pretty"), 
                # Checking position of columns with custom intervals.
                CustomInt = Orig %in% AllPKParameters$PKparameter_nodosenum)
      
      # Adding time interval to any data that came from custom AUC interval
      # sheets.
      if(any(complete.cases(PKparameters$Sheet)) &
         nrow(CheckDoseInt$interval) > 0){ 
         
         IntToAdd <- CheckDoseInt$interval %>% 
            filter(Sheet %in% PKparameters$Sheet) %>% 
            mutate(Interval = paste("from", UserIntervalStart, "h to",
                                    UserIntervalEnd, "h")) %>% 
            pull(Interval)
         
         ColNames <- ColNames %>% 
            mutate(UnitsToAdd = ifelse(CustomInt, 
                                       str_extract(Pretty, 
                                                   " \\(h\\)| \\(ng/mL(.h)?\\)| \\(L/h\\)"), 
                                       ""), 
                   Pretty = ifelse(CustomInt, 
                                   sub(" \\(h\\)| \\(ng/mL(.h)?\\)| \\(L/h\\)", "", Pretty), 
                                   Pretty), 
                   Pretty = ifelse(CustomInt, 
                                   paste0(Pretty, 
                                          " for interval ", IntToAdd, 
                                          UnitsToAdd), 
                                   Pretty))
      }
      
      # Adjusting units as needed.
      if("Units_AUC" %in% names(Deets) && complete.cases(Deets$Units_AUC)){
         ColNames$Pretty <- sub("\\(ng/mL.h\\)", paste0("(", Deets$Units_AUC, ")"), ColNames$Pretty)
      }
      if("Units_CL" %in% names(Deets) && complete.cases(Deets$Units_CL)){
         ColNames$Pretty <- sub("\\(L/h\\)", paste0("(", Deets$Units_CL, ")"), ColNames$Pretty)
      }
      if("Units_Cmax" %in% names(Deets) && complete.cases(Deets$Units_Cmax)){
         ColNames$Pretty <- sub("\\(ng/mL\\)", paste0("(", Deets$Units_Cmax, ")"), ColNames$Pretty)
      }
      if("Units_tmax" %in% names(Deets) && complete.cases(Deets$Units_tmax)){
         ColNames$Pretty <- sub("\\(h\\)", paste0("(", Deets$Units_tmax, ")"), ColNames$Pretty)
      }
      ColNames$Pretty <- gsub("ug/mL", "µg/mL", ColNames$Pretty)
      
      MyPerpetrator <- determine_myperpetrator(existing_exp_details, prettify_compound_names)
      
      if(any(complete.cases(MyPerpetrator))){
         ColNames$Pretty <- sub("perpetrator", MyPerpetrator, ColNames$Pretty)
      }
      
      MyPKResults <- MyPKResults[, ColNames$Orig]
      names(MyPKResults) <- ColNames$Pretty
      
   } 
   
   include_dose_num_orig <- include_dose_num
   include_dose_num <- check_include_dose_num(MyPKResults, 
                                              include_dose_num)
   
   if(include_dose_num == FALSE){
      names(MyPKResults) <- sub("Dose 1 |Last dose ", "", names(MyPKResults))
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
         
         OutPath <- dirname(save_table)
         
         if(OutPath == "."){
            OutPath <- getwd()
         }
         
         FileName <- basename(save_table)
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
   
   return(Out)
   
}



