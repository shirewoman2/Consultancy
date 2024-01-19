#' Make PK summary tables from multiple simulator output files at once
#'
#' \code{pksummary_mult} creates tables of PK parameters for reports and
#' presentations, including reporting means, CVs, and confidence intervals or
#' percentiles and, optionally, comparisons to observed data. This function
#' automatically finds the correct tabs and the correct cells in a Simulator
#' output Excel file to obtain those data. \strong{Notes:} \itemize{\item{Please
#' see the notes at the bottom of this help file for how to supply observed data
#' in a standardized fashion that this function can read.} \item{For detailed
#' instructions and examples, please see the SharePoint file "Simcyp PBPKConsult
#' R Files - Simcyp PBPKConsult R Files/SimcypConsultancy function examples and
#' instructions/Making PK tables/PK-tables.docx". (Sorry, we are unable to
#' include a link to it here.)}}
#'
#' Because we need to have a standardized way to input observed data, setting up
#' the input for this function requires creating a data.frame of the observed PK
#' data or supplying a csv or Excel file with observed PK data.
#'
#' \strong{OPTION A: Supply a data.frame.} Use the column names to indicate
#' which PK parameter you want, and include a column titled "File" to indicate
#' which simulator output Excel file the observed PK in that row should be
#' compared to. If you have CV values for any observed data that you'd like to
#' include in the table, make the column name be the PK parameter with a suffix
#' of "_CV".
#'
#' An example of specifying a data.frame: \code{observed_PK = data.frame(File =
#' c("abc1a-25mg.xlsx", "abc1a-50mg.xlsx"), AUCinf_dose1 = c(60, 120),
#' AUCinf_dose1_CV = c(0.38, 0.42), Cmax_dose1 = c(22, 51), Cmax_dose1_CV =
#' c(0.24, 0.39))}
#'
#' \strong{OPTION B: Use a csv file of observed PK data.} In Excel, create a csv
#' file where the first row is the PK parameters you want and the second row
#' lists the values for each. Make sure to include one column titled "File" to
#' indicate which simulator output Excel file the observed PK in that row should
#' be compared to. This should look the same as the examples for Option A. To
#' see an example of how this should look, run this in the console and then open
#' the csv file:
#'
#' \code{write.csv(data.frame(File = c("abc1a-25mg.xlsx", "abc1a-50mg.xlsx"),
#' AUCinf_dose1 = c(60, 120), AUCinf_dose1_CV = c(0.38, 0.42), Cmax_dose1 =
#' c(22, 51), Cmax_dose1_CV = c(0.24, 0.39)), file = "Example observed PK
#' values.csv", row.names = FALSE)}
#'
#' When you call on \code{pksummary_table}, use the following syntax,
#' substituting your file name for the example: \code{observed_PK = "Example
#' observed PK values.csv"}
#'
#' \strong{OPTION C: Use a tab named "observed PK" in the compound data sheet
#' for your project.} Just as with the other examples, the first row lists the
#' PK parameters you want, and subsequent rows list the values for those
#' parameters. Include a column titled "File" and list the name of the
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
#' @param sim_data_files a character vector of simulator output files, each in
#'   quotes and encapsulated with \code{c(...)}, NA to extract PK data for
#'   \emph{all} the Excel files in the current folder, or "recursive" to extract
#'   PK data for all the Excel files in the current folder and all subfolders.
#' @param compoundsToExtract For which compound(s) do you want to extract PK
#'   data? Options are any combination of the following:
#'   \itemize{\item{"substrate" (default),} \item{"primary metabolite 1",}
#'   \item{"primary metabolite 2",} \item{"secondary metabolite",}
#'   \item{"inhibitor 1" -- this can be an inducer, inhibitor, activator, or
#'   suppresesor, but it's labeled as "Inhibitor 1" in the simulator,}
#'   \item{"inhibitor 2" for the 2nd inhibitor listed in the simulation,}
#'   \item{"inhibitor 1 metabolite" for the primary metabolite of inhibitor 1}
#'   \item{"all" for all possible compounds present in the simulations}} To
#'   specify multiple compounds, enclose the compound IDs with parentheses,
#'   e.g., \code{compoundsToExtract = c("substrate", "inhibitor 1")}
#' @param tissues For which tissue(s) would you like the PK parameters to be
#'   pulled? Options are any combination of "plasma" (default), "unbound
#'   plasma", "blood", or "unbound blood". For multiple tissues, enclose them
#'   with parentheses, e.g., \code{tissues = c("blood", "plasma")}
#' @param PKparameters (optional) the PK parameters to include as a character
#'   vector. \itemize{
#'
#'   \item{By default, if you have a single-dose simulation, the parameters will
#'   include AUC and Cmax for dose 1, and, if you have a multiple-dose
#'   simulation, AUC and Cmax for the last dose. Also by default, if you have a
#'   perpetrator present, the parameters will include the AUC and Cmax values with
#'   and without the perpetrator as well as those ratios.}
#'
#'   \item{Alternatively, you can specify a vector of any combination of
#'   specific, individual parameters, e.g., \code{c("Cmax_dose1",
#'   "AUCtau_last").} Be sure to encapsulate the parameters you want with
#'   \code{c(...)}! To see the full set of possible parameters to extract, enter
#'   \code{view(PKParameterDefinitions)} into the console.}
#'
#'   \item{If you would like PK pulled from a specific custom interval, please
#'   supply a named character vector where the names are the PK parameters and the
#'   values are the tabs. Example: \code{sheet_PKparameters = c("AUCinf_dose1" =
#'   NA, "AUCt" = "Int AUC userT(1)(Sub)(CPlasma)", "AUCtau_last" = NA)} Please
#'   note that we would like the PK parameters that are for either dose 1 or the
#'   last dose to have NA listed for the tab. It is also ok to supply this named
#'   character vector to the argument \code{sheet_PKparameters} instead, but
#'   please do not supply it to both.}
#'
#'   \item{If you supply observed data using either the argument
#'   \code{report_input_file} or the argument \code{observed_PK} and do not
#'   specify anything for \code{PKparameters}, the PK parameters will be those
#'   included for the observed data.}
#'
#'   \item{Parameters that don't make sense for your scenario -- such as asking
#'   for \code{AUCinf_dose1_withInhib} when your simulation did not include an
#'   inhibitor or perpetrator -- will not be included.}
#'
#'   \item{tmax will be listed as median, min, and max rather than mean, lower
#'   and higher confidence interval or percentiles. Similarly, if you request
#'   trial means, the values for tmax will be the range of medians for the
#'   trials rather than the range of means.}}
#'
#'   An example of acceptable input here: \code{PKparameters = c("AUCtau_last",
#'   "AUCtau_last_withInhib", "Cmax_last", "Cmax_last_withInhib",
#'   "AUCtau_ratio_last", "Cmax_ratio_last")}.
#' @param PKorder Would you like the order of the PK parameters to be the
#'   order specified in the Consultancy Report Template (default), or would you
#'   like the order to match the order you specified with the argument
#'   \code{PKparameters}? Options are "default" or "user specified".
#' @param sheet_PKparameters (optional) If you want the PK parameters to be
#'   pulled from a specific tab in the simulator output file, list that tab
#'   here. Otherwise, this should be left as NA. If you want some parameters
#'   from a custom-interval tab and others from the regular tabs, please supply
#'   a named character vector where the names are the PK parameters and the
#'   values are the tabs. Example: \code{sheet_PKparameters = c("AUCinf_dose1" =
#'   NA, "AUCt" = "Int AUC userT(1)(Sub)(CPlasma)", "AUCtau_last" = NA)} Please
#'   note that we would like the PK parameters that are for either dose 1 or the
#'   last dose to have NA listed for the tab. It is also ok to supply this named
#'   character vector to the argument \code{PKparameters} instead, but
#'   please do not supply it to both.
#' @param observed_PK (optional) If you have a data.frame, a named numeric
#'   vector, or an Excel or csv file with observed PK parameters, supply the
#'   full file name in quotes or supply the unquoted name of the the data.frame
#'   or vector here, and the simulated-to-observed mean ratios will be
#'   calculated. If you supply an Excel file, R will be looking to read a tab
#'   named \emph{exactly} "observed PK". The supplied data.frame or file
#'   \emph{must} be set up in one of two possible ways: \itemize{
#'
#'   \item{Columns for each of the PK parameters you would like to compare, and
#'   those column names \emph{must} be among the PK parameter options listed in
#'   \code{PKParameterDefinitions}. If you would like the output table to
#'   include the observed data CV for any of the parameters, add "_CV" to the
#'   end of the parameter name, e.g., "AUCinf_dose1_CV".}
#'
#'   \item{A column titled "PKparameter" and a column titled "Value". (It's fine
#'   if there are other columns as well.) All the items in the column
#'   "PKparameter" \emph{must} be among the PK parameter options listed in
#'   \code{PKParameterDefinitions}.}}
#'
#'   Additionally, if you have more than one set of PK parameters, you must
#'   include a column titled "File" that lists which simulator output file
#'   should be compared to that observed data. If there's only one set of PK
#'   parameters, you can omit the column "File" and the PK data will be compared
#'   to ALL the simulated files included in \code{sim_data_files}. Please see
#'   the "Example" section of this help file for examples of how to set this up.
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get all the details from the "Input
#'   Sheet" (e.g., when you ran extractExpDetails you said \code{exp_details =
#'   "Input Sheet"} or \code{exp_details = "all"}), you can save some processing
#'   time by supplying that object here, unquoted. If left as NA, this function
#'   will run \code{extractExpDetails} behind the scenes to figure out some
#'   information about your experimental set up.
#' @param mean_type What kind of means and CVs do you want listed in the output
#'   table? Options are "arithmetic" or "geometric" (default).
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
#' @param includeRange TRUE or FALSE (default) for whether to include the
#'   minimum and maximum values
#' @param includePerc TRUE or FALSE (default) for whether to include 5th to 95th
#'   percentiles
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
#'   \emph{also} want to use the results from \code{pksummary_mult} to make
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
#' @param ...
#'
#' @return Returns a data.frame with summary PK parameters from multiple
#'   simulator output files
#' @export
#'
#' @examples
#'
#' # Create PK summary tables for all the Simulator output files
#' # in your working directory with the default PK parameters
#' pksummary_mult(sim_data_files = NA)
#'
#' # Include a data.frame of observed data for S/O comparisons
#' pksummary_mult(
#'     sim_data_files = NA,
#'     observed_PK = data.frame(File = c("mdz-5mg-qd-keto-400mg-qd.xlsx",
#'                                       "mdz-5mg-qd-rif-600mg-qd.xlsx"),
#'                              AUCtau_last = c(55, 55),
#'                              Cmax_last = c(20, 20),
#'                              AUCtau_last_withInhib = c(800, 20),
#'                              Cmax_last_withInhib = c(100, 5)))
#' 

pksummary_mult <- function(sim_data_files = NA, 
                           compoundsToExtract = "substrate",
                           tissues = "plasma", 
                           PKparameters = NA,
                           PKorder = "default", 
                           sheet_PKparameters = NA, 
                           observed_PK = NA,
                           existing_exp_details = NA, 
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
                           extract_forest_data = FALSE, 
                           checkDataSource = TRUE, 
                           save_table = NA, 
                           fontsize = 11, 
                           highlight_gmr_colors = NA, 
                           highlight_so_cutoffs = NA, 
                           highlight_so_colors = "yellow to red", 
                           single_table = FALSE,
                           ...){
   
   # Error catching ----------------------------------------------------------
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
   
   # If they said "save_output" instead of "save_table", fix that.
   if("save_output" %in% names(match.call())){
      save_table <- sys.call()$save_output
   }
   
   # Check whether they supplied a named character vector for PKparameters
   # instead of for sheet_PKparameters.
   if(is.null(names(sheet_PKparameters)) & all(is.na(sheet_PKparameters)) & # sheet_PKparameters = NA
      is.null(names(PKparameters)) == FALSE){ # PKparameters = named character vector that includes sheet names
      # warning("You supplied a named character vector for the argument `PKparameters` rather than for the argument `sheet_PKparameters`, which is what we had intended but which we also understand has a really similar name. We think we can accommodate this anyway. If you run into trouble, though, please tell Laura Shireman.\n", 
      #         call. = FALSE)
      
      sheet_PKparameters <- PKparameters
      PKparameters <- NA
      
   }
   
   # Harmonizing PK parameter names
   PKparameters <- harmonize_PK_names(PKparameters)
   
   # Check for appropriate input for compound ID
   compoundsToExtract <- tolower(compoundsToExtract)
   
   PossCmpd <- c("substrate", "primary metabolite 1", "primary metabolite 2",
                 "secondary metabolite",
                 "inhibitor 1", "inhibitor 2", "inhibitor 1 metabolite",
                 "inhibitor 2 metabolite", "all")
   
   if(any(compoundsToExtract %in% PossCmpd == FALSE)){
      warning(paste0("The compound(s) ", 
                     str_comma(paste0("`", setdiff(compoundsToExtract, PossCmpd), "`")),
                     " is/are not among the possible componds to extract and will be ignored. The possible compounds to extract are only exactly these: ",
                     str_comma(paste0("`", PossCmpd, "`")), "
                     "), 
              call. = FALSE)
      compoundsToExtract <- intersect(compoundsToExtract, PossCmpd)
   }
   
   compoundsToExtract_orig <- compoundsToExtract
   if(any(complete.cases(compoundsToExtract)) && "all" %in% compoundsToExtract){
      compoundsToExtract <- c("substrate", "primary metabolite 1", "primary metabolite 2",
                              "secondary metabolite",
                              "inhibitor 1", "inhibitor 2", "inhibitor 1 metabolite",
                              "inhibitor 2 metabolite")
   }
   
   tissues <- tolower(tissues)
   if(any(tissues %in% c("plasma", "unbound plasma", "blood", "unbound blood") == FALSE)){
      warning("You have not supplied a permissible value for tissue. Options are `plasma`, `unbound plasma`, `blood`, or `unbound blood`. The PK parameters will be for plasma.\n", 
              call. = FALSE)
      tissues <- intersect(tissues, c("plasma", "unbound plasma", "blood", "unbound blood"))
   }
   
   if(extract_forest_data & includeConfInt == FALSE){
      warning("To get forest-plot data, we need the confidence interval, but you have set `includeConfInt = FALSE`. We're going to change that to TRUE so that we can get what we need for forest-plot data.\n", 
              call. = FALSE)
      includeConfInt <- TRUE
   }
   
   # Main body of function --------------------------------------------------
   
   # Noting sim_data_files input for later.
   sim_data_files_input <- sim_data_files
   
   # If user did not supply files, then extract all the files in the current
   # folder that end in "xlsx" or in all subfolders if they wanted it to be
   # recursive.
   
   if(length(sim_data_files) == 1 &&
      (is.na(sim_data_files) | sim_data_files == "recursive")){
      sim_data_files <- list.files(pattern = "xlsx$",
                                   recursive = (complete.cases(sim_data_files) &&
                                                   sim_data_files == "recursive"))
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   }
   
   ## Read obs data --------------------------------------------------------
   # Read in the observed_PK data if it's not already a data.frame. Note that
   # the class of observed_PK will be logical if left as NA.
   if(class(observed_PK)[1] == "character"){
      observed_PKDF <- switch(str_extract(observed_PK, "csv|xlsx"), 
                              "csv" = read.csv(observed_PK), 
                              "xlsx" = xlsx::read.xlsx(observed_PK, 
                                                       sheetName = "observed PK"))
      # If there's anything named anything like "File", use that for the
      # "File" column. This is useful to deal with capitalization mismatches
      # and also because, if the user saves the file as certain kinds of csv
      # files, R has trouble importing and will add extra symbols to the 1st
      # column name.
      names(observed_PKDF)[str_detect(tolower(names(observed_PKDF)), "file")][1] <- 
         "File"
      
      # If they supplied a specific set of sims to extract, then ignore any
      # extras, even if they're in observed PK
      if(all(is.na(sim_data_files_input)) |
         (all(complete.cases(sim_data_files_input)) & 
          all(sim_data_files_input == "recursive"))){
         sim_data_files <- union(sim_data_files, observed_PKDF$File)
         sim_data_files <- sim_data_files[complete.cases(sim_data_files)]
      }
      
   } else if("numeric" %in% class(observed_PK)){
      observed_PKDF <- as.data.frame(t(observed_PK))
   }
   
   if("data.frame" %in% class(observed_PK)){
      observed_PKDF <- unique(observed_PK)
      
      # Harmonizing col names
      names(observed_PKDF) <- tolower(names(observed_PKDF))
      names(observed_PKDF) <- sub("compoundid", "CompoundID", names(observed_PKDF))
      names(observed_PKDF) <- sub("tissue", "Tissue", names(observed_PKDF))
      names(observed_PKDF) <- sub("file", "File", names(observed_PKDF))
      names(observed_PKDF) <- sub("pkparameter", "PKparameter", names(observed_PKDF))
      names(observed_PKDF) <- sub("value", "Value", names(observed_PKDF))
      
   }
   
   if(exists("observed_PKDF", inherits = FALSE)){
      
      # Checking whether data in long or wide format. 
      Wide <- any(names(observed_PKDF) %in% c(AllPKParameters$PKparameter, 
                                              tolower(AllPKParameters$PKparameter)))
      
      if(Wide){
         
         if("File" %in% names(observed_PKDF) == FALSE){
            
            if(nrow(observed_PKDF) == 1){
               # If there is only one value for each PK parameter, then use that
               # set of PK data to compare to ALL of the simulated data.
               observed_PKDF <- bind_cols(observed_PKDF, "File" = sim_data_files)
            } else {
               # If there is more than one value for each PK parameter, though,
               # then we don't know what to compare. Give an error message and
               # omit the S/O rows.
               warning("You must either include a column titled 'File' with the observed PK so that this function knows which simulator output files to compare with these obseved data, or you must submit only one set of PK parameters and we'll compare that to all the simulated files. We don't know what to compare here, so we will omit the observed data.\n", 
                       call. = FALSE)
               observed_PKDF <- NA
            }
         }
         
      } else {
         # Error catching for long format here
         
         # Making names consistent
         if(any(c("pkparameter", "pkparam", "param", "parameter") %in%
                tolower(names(observed_PKDF)))){
            
            ColToUse <- which(tolower(names(observed_PKDF)) %in% 
                                 c("pkparameter", "pkparam", "param", 
                                   "parameter"))[1]
            names(observed_PKDF)[ColToUse] <- "PKparameter"
            rm(ColToUse)
         }
         
         if("Value" %in% names(observed_PKDF) == FALSE &&
            any(c("geomean", "gm_mean", "gmean", "mean") %in%
                tolower(names(observed_PKDF)))){
            
            ColToUse <- c(which(tolower(names(observed_PKDF)) == "geomean"), 
                          which(tolower(names(observed_PKDF)) == "gm_mean"), 
                          which(tolower(names(observed_PKDF)) == "gmean"), 
                          which(tolower(names(observed_PKDF)) == "mean"))[1]
            
            warning(paste0("We were looking for a column named `Value` in your observed data and did not find it, but we *did* find a column called `", 
                           names(observed_PKDF)[ColToUse],
                           "`, which we think is what you might want. We'll use the data that column for the observed values.\n"), 
                    call. = FALSE)
            
            names(observed_PKDF)[ColToUse] <- "Value"
            rm(ColToUse)
         }
         
         # If they've only supplied a single set of PK parameters -- i.e., all
         # values in PKparameters are unique -- then make all PK apply to all
         # files.
         if(any(duplicated(observed_PKDF$PKparameter)) == FALSE &
            ("File" %in% names(observed_PKDF) == FALSE ||
             all(is.na(observed_PKDF$File)))){
            
            observed_PKDF <- observed_PKDF %>% ungroup() %>% 
               select(-any_of("File")) %>% 
               left_join(expand.grid(File = sim_data_files, 
                                     PKparameter = observed_PKDF$PKparameter), 
                         by = "PKparameter")
         }
      }   
      
      # Now error catching for either long or wide obs data
      
      # If user has not included "xlsx" in file name, add that.
      if(any(str_detect(observed_PKDF$File, "xlsx$") == FALSE)){
         observed_PKDF$File[which(str_detect(observed_PKDF$File, "xlsx$") == FALSE)] <-
            paste0(observed_PKDF$File[which(str_detect(observed_PKDF$File, "xlsx$") == FALSE)], 
                   ".xlsx")
      }
      
      if(Wide == FALSE & 
         all(c("PKparameter", "Value") %in% names(observed_PKDF)) == FALSE){
         
         warning("None of the data in `observed_PK` seem to be PK data. Please check the help file for options for arranging observed data for comparison to simulated data here. For now, we can't calculate S/O ratios.\n", 
                 call. = FALSE)
         observed_PKDF <- NA
      }
      
      # Specifying CompoundID and Tissue in ObsPK if possible
      if(("CompoundID" %in% names(observed_PKDF) == FALSE ||
          all(is.na(observed_PKDF$CompoundID))) &
         length(sort(unique(compoundsToExtract))) == 1){
         observed_PKDF$CompoundID <- sort(unique(compoundsToExtract))
      }
      
      if(("Tissue" %in% names(observed_PKDF) == FALSE ||
          all(is.na(observed_PKDF$Tissue))) &
         length(sort(unique(tissues))) == 1){
         observed_PKDF$Tissue <- sort(unique(tissues))
      }
   }
   
   # If user has not included "xlsx" in file name, add that.
   sim_data_files[str_detect(sim_data_files, "xlsx$") == FALSE] <-
      paste0(sim_data_files[str_detect(sim_data_files, "xlsx$") == FALSE], 
             ".xlsx")
   
   # Making sure that we're only extracting each file once
   sim_data_files <- unique(sim_data_files)
   
   # Making sure that all the files exist before attempting to pull data
   if(all(complete.cases(sim_data_files)) && 
      any(file.exists(sim_data_files) == FALSE)){
      MissingSimFiles <- sim_data_files[
         which(file.exists(sim_data_files) == FALSE)]
      warning(paste0("The file(s) ", 
                     str_comma(paste0("`", MissingSimFiles, "`")), 
                     " is/are not present and thus will not be extracted.
                     "), 
              call. = FALSE)
      sim_data_files <- setdiff(sim_data_files, MissingSimFiles)
   }
   
   # Setting this for all files 
   prettify_compound_names <- c("inhibitor" = "perpetrator",
                                "substrate" = "substrate")
   
   ## Getting simulated data ------------------------------------------------
   MyPKResults <- list()
   PKpulled <- list()
   OutQC <- list()
   FD <- list()
   
   for(i in sim_data_files){
      
      MyPKResults[[i]] <- list()
      PKpulled[[i]] <- list()
      OutQC[[i]] <- list()
      FD[[i]] <- list()
      
      message(paste0("Extracting PK data from `", i, "`"))
      
      # Getting experimental details for the simulation(s) as needed. NB:
      # "Deets" in all pksummary functions means ONLY the experimental details
      # for the single file in question -- either the only file for
      # pksummary_table or the specific file we're dealing with in that
      # iteration of the loop in pksummary_mult. By contrast,
      # existing_exp_details will include ALL experimental details provided or
      # extracted inside the function.
      if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA
         existing_exp_details <- extractExpDetails(i, exp_details = "Summary and Input", 
                                                   annotate_output = FALSE)
      } else {
         existing_exp_details <- harmonize_details(existing_exp_details)
      }
      
      Deets <- existing_exp_details$MainDetails %>% filter(File == i)
      
      if(nrow(Deets) == 0){
         Deets <- 
            extractExpDetails(sim_data_file = i, 
                              exp_details = "Summary and Input", 
                              annotate_output = FALSE)[["MainDetails"]]
      }
      
      # Checking that the file is, indeed, a simulator output file.
      if(length(Deets) == 0){
         # Using "warning" instead of "stop" here b/c I want this to be able to
         # pass through to other functions and just skip any files that
         # aren't simulator output.
         warning(paste("The file", i,
                       "does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file.\n"), 
                 call. = FALSE)
         next()
      }
      
      # Only include compounds that are actually present.
      AllPossCompounds <- c(
         "substrate" = Deets$Substrate, 
         "primary metabolite 1" = ifelse("PrimaryMetabolite1" %in% names(Deets), 
                                         Deets$PrimaryMetabolite1, NA),
         "primary metabolite 2" = ifelse("PrimaryMetabolite2" %in% names(Deets),
                                         Deets$PrimaryMetabolite2, NA),
         "secondary metabolite" = ifelse("PrimaryMetabolite2" %in% names(Deets),
                                         Deets$SecondaryMetabolite, NA),
         "inhibitor 1" = ifelse("Inhibitor1" %in% names(Deets),
                                Deets$Inhibitor1, NA),
         "inhibitor 2" = ifelse("Inhibitor2" %in% names(Deets),
                                Deets$Inhibitor2, NA),
         "inhibitor 1 metabolite" = ifelse("Inhibitor1Metabolite" %in% names(Deets),
                                           Deets$Inhibitor1Metabolite, NA))
      
      AllPossCompounds <- names(AllPossCompounds[complete.cases(AllPossCompounds)])
      
      if(any(compoundsToExtract_orig == "all")){
         compoundsToExtract <- intersect(compoundsToExtract, AllPossCompounds)
      }
      
      for(j in compoundsToExtract){
         message(paste("Extracting data for compound =", j))
         
         MyPKResults[[i]][[j]] <- list()
         PKpulled[[i]][[j]] <- list()
         OutQC[[i]][[j]] <- list()
         FD[[i]][[j]] <- list()
         
         for(k in tissues){
            if(exists("observed_PKDF", inherits = FALSE) &&
               i %in% observed_PKDF$File){ 
               ObsPK_temp <- observed_PKDF %>% filter(File == i)
               
               if("Tissue" %in% names(ObsPK_temp)){
                  ObsPK_temp <- ObsPK_temp %>% 
                     filter(Tissue == k)
               }
               
               if("CompoundID" %in% names(ObsPK_temp)){
                  ObsPK_temp <- ObsPK_temp %>% 
                     filter(CompoundID == j)
               }
               
               if(nrow(ObsPK_temp) == 0){
                  ObsPK_temp <- NA
               }
            } else {
               ObsPK_temp <- NA
            }
            
            message(paste("Extracting data for tissue =", k))
            suppressWarnings(
               temp <- pksummary_table(
                  sim_data_file = i,
                  compoundToExtract = j,
                  tissue = k, 
                  observed_PK = ObsPK_temp,
                  PKparameters = PKparameters, 
                  PKorder = PKorder, 
                  sheet_PKparameters = sheet_PKparameters, 
                  return_PK_pulled = TRUE,
                  existing_exp_details = Deets,
                  mean_type = mean_type,
                  includeCV = includeCV,
                  includeSD = includeSD, 
                  includeMedian = includeMedian,
                  includeRange = includeRange,
                  includeConfInt = includeConfInt, 
                  includePerc = includePerc, 
                  includeTrialMeans = includeTrialMeans,
                  concatVariability = concatVariability,
                  variability_format = variability_format,
                  include_dose_num = TRUE, # will remove later if needed but we need this for some of the table heading info
                  rounding = rounding,
                  adjust_conc_units = adjust_conc_units,
                  prettify_columns = prettify_columns, 
                  extract_forest_data = extract_forest_data,
                  checkDataSource = checkDataSource,
                  prettify_compound_names = prettify_compound_names)
            )
            
            if(length(temp) == 0){
               rm(temp)
               next
            }
            
            MyPKResults[[i]][[j]][[k]] <- switch(as.character("list" %in% class(temp)), 
                                                 "TRUE" = temp$Table, 
                                                 "FALSE" = temp) %>% 
               mutate(File = i, 
                      CompoundID = j, 
                      Tissue = k)
            
            # Checking for when they requested AUCinf but there were problems
            # extrapolating. Giving a warning in that situation.
            if((all(is.na(PKparameters)) |
                all(complete.cases(PKparameters)) &
                any(str_detect(PKparameters, "AUCinf"))) &
               any(str_detect(names(MyPKResults[[i]][[j]][[k]]), "AUCinf")) == FALSE){
               
               warning(paste0("The ", k, # tissue
                              " AUCinf included NA values for the ", j, # CompoundID
                              " in the file `", 
                              i, 
                              "`, meaning that the Simulator had trouble extrapolating to infinity and thus making the AUCinf summary data unreliable. We will supply AUCt for this instead.\n"),
                       call. = FALSE)
            }
            
            PKpulled[[i]][[j]][[k]] <-
               data.frame(File = i, 
                          CompoundID = j, 
                          Tissue = k, 
                          PKpulled = setdiff(names(MyPKResults[[i]][[j]][[k]]), 
                                             c("Statistic", "File")))
            
            if(checkDataSource){
               OutQC[[i]][[j]][[k]] <- temp$QC
            } 
            
            if(extract_forest_data){
               FD[[i]][[j]][[k]] <- temp$ForestData
            }
            
            rm(temp, ObsPK_temp)
         }
         
         MyPKResults[[i]][[j]] <- bind_rows(MyPKResults[[i]][[j]])
         PKpulled[[i]][[j]] <- bind_rows(PKpulled[[i]][[j]])
         OutQC[[i]][[j]] <- bind_rows(OutQC[[i]][[j]])
         FD[[i]][[j]] <- bind_rows(FD[[i]][[j]])
      }
      
      MyPKResults[[i]] <- bind_rows(MyPKResults[[i]])
      PKpulled[[i]] <- bind_rows(PKpulled[[i]])
      OutQC[[i]] <- bind_rows(OutQC[[i]])
      FD[[i]] <- bind_rows(FD[[i]])
      
   }
   
   if(length(MyPKResults) == 0){
      warning("No PK data could be found in the files ", 
              str_comma(paste0("`", sim_data_files, "`")), "\n",
              call. = FALSE)
      return(list())
   }
   
   MyPKResults <- bind_rows(MyPKResults[
      sapply(MyPKResults, FUN = function(x) is.data.frame(x) && nrow(x) > 0)])
   PKpulled <- bind_rows(PKpulled[sapply(PKpulled, FUN = length) > 0])
   OutQC <- bind_rows(OutQC[
      sapply(OutQC, FUN = function(x) is.data.frame(x) && nrow(x) > 0)])
   
   if(extract_forest_data){
      # Need to deal with possible character data for custom dosing before row
      # binding for FD
      FD <- FD[which(sapply(FD, function(x) "list" %in% class(x) == FALSE))]
      
      suppressWarnings(
         FD <- map(FD, .f = function(x) x %>% 
                      mutate(across(.cols = any_of(c("Dose_sub", "Dose_inhib")), 
                                    .fns = as.numeric))))
      FD <- bind_rows(FD[sapply(FD, FUN = length) > 0])
   }
   
   if(extract_forest_data & # NOT SURE THIS IS NECESSARY
      any(str_detect(names(bind_rows(MyPKResults)), "ratio")) == FALSE){
      warning("You requested forest data, but none of the PK parameters included in the output include geometric mean ratios. At least for now, the forest_plot function has only been set up to graph GMRs, so no forest plot data can be extracted.\n", 
              call. = FALSE)
      
      extract_forest_data <- FALSE
   } 
   
   ## Formatting and arranging the data -------------------------------------
   if(PKorder == "default"){
      
      MyPKResults <- bind_rows(MyPKResults)
      
      if(nrow(MyPKResults) == 0){
         warning("None of the PK values you requested could be found.\n", 
                 call. = FALSE)
         
         return(data.frame())
      }
      
      suppressMessages(
         MyPKResults <- MyPKResults %>% 
            select(Statistic, 
                   any_of(data.frame(PrettifiedNames = names(MyPKResults)) %>%
                             left_join(AllPKParameters %>% select(PrettifiedNames, SortOrder)) %>% 
                             filter(complete.cases(SortOrder)) %>% 
                             arrange(SortOrder) %>% pull(PrettifiedNames) %>% unique()),
                   everything()) %>% 
            relocate(c(CompoundID, Tissue, File), .after = last_col())
      )
      
   } else {
      
      MyPKResults <- bind_rows(MyPKResults) %>% 
         select(Statistic, CompoundID, Tissue, everything()) %>% 
         relocate(c(CompoundID, Tissue, File), .after = last_col())
   }
   
   ## Saving --------------------------------------------------------------
   if(complete.cases(save_table)){
      
      # Checking whether they have specified just "docx" or just "csv" for
      # output b/c then, we'll use "PK summary table" as file name.
      if(str_detect(sub("\\.", "", save_table), "^docx$|^csv$")){
         OutPath <- "."
         save_table <- paste0("PK summary table.", sub("\\.", "", save_table))
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
      
      if(str_detect(save_table, "\\.csv")){
         
         MeanType <- ifelse(is.na(mean_type), "geometric", mean_type)
         
         # This is when they want a csv file as output. In this scenario,
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
         
      } else {
         # This is when they want a Word file as output
         
         OutPath <- dirname(save_table)
         
         if(OutPath == "."){
            OutPath <- getwd()
         }
         
         FileName <- basename(save_table)
         FromCalcPKRatios <- FALSE
         
         rmarkdown::render(
            system.file("rmarkdown/templates/pksummarymult/skeleton/skeleton.Rmd", 
                        package="SimcypConsultancy"),
            output_dir = OutPath, 
            output_file = FileName, 
            quiet = TRUE)
         # Note: The "system.file" part of the call means "go to where the
         # package is installed, search for the file listed, and return its
         # full path.
         
      }
      
      if(checkDataSource){
         
         if(complete.cases(save_table)){
            
            write.csv(OutQC, sub(".csv|.docx", " - QC.csv", save_table), row.names = F)
            
         }  
      }
      
      if(extract_forest_data){
         write.csv(bind_rows(FD), sub(".csv|.docx", " - forest data.csv", save_table), row.names = F)
      }
      
   }
   
   Out <- list("Table" = MyPKResults)
   
   if(checkDataSource){
      Out[["QC"]] <- OutQC
   }
   
   if(extract_forest_data){
      Out[["ForestData"]] <- bind_rows(FD)
   }
   
   if(length(Out) == 1){
      Out <- Out[["Table"]]
   }
   
   return(Out)
   
}


