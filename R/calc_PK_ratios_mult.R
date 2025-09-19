#' Calculate the ratio of PK parameters between multiple pairs of simulations
#'
#' \code{calc_PK_ratios_mult} matches PK data from pairs of simulator output
#' Excel files and calculates the mean and confidence intervals of the ratios of
#' the requested PK parameters. For detailed instructions and examples, please
#' see the SharePoint file "Simcyp PBPKConsult R Files - Simcyp PBPKConsult R
#' Files/SimcypConsultancy function examples and instructions/Calculating PK
#' ratios from separate simulations/Calculating-PK-ratios.docx". (Sorry, we are
#' unable to include a link to it here.)
#'
#' @param PKparameters the PK parameters to include. There are two main options
#'   for this: 1) supply a file to read or a data.frame (R speak for "a table")
#'   that lists which simulation files, compounds, tissues, and PK you want or
#'   2) supply a character vector of which PK parameters you want and then also
#'   specify what you need in terms of which tissue, which compound, which
#'   simulation files, and which tab to get the data from with the arguments
#'   \code{tissue}, \code{compoundToExtract}, \code{sim_data_file_numerator},
#'   \code{sim_data_file_denominator}, and \code{sheet_PKparameters}.
#'   \strong{Details on each option:} \describe{\item{\strong{Option 1: }a file to read or a data.frame}{This
#'   is the most versatile option and, we think, the clearest in terms of
#'   getting what you expected. Please try running \code{\link{make_example_PK_input}}
#'   to see examples for how to set up a csv or Excel file or data.frame to
#'   specify exactly which simulation file should get which PK parameter from
#'   which tissue and, when user-specified intervals are involved, from which
#'   tab in the Excel file those data should be pulled. Whatever you supply, the
#'   columns that will be read are: \describe{\item{"Numerator_File"}{this is the
#'   same thing as the argument \code{sim_data_file_numerator}}
#'
#'   \item{"Denominator_File"}{This is the same thing as the argument \code{sim_data_file_denominator})}
#'
#'   \item{"Numerator_Sheet" or "Denominator_Sheet"}{When it's a user-defined AUC interval you want,
#'   this specifies which sheet in the Simulator output Excel file to use for
#'   the corresponding PK parameter in the numerator or denominator,
#'   respectively. If it's a regular first-dose or last-dose PK parameter,
#'   leave this blank or as NA; we know which sheets to use for those
#'   values. You can specify as many different sheets as needed with one row
#'   for every new sheet.}
#'
#'   \item{"Numerator_Tissue" or "Denominator_Tissue"}{This is the same as the
#'   argument \code{tissue} except you can specify it separately for each set of PK parameters.}
#'
#'   \item{"Numerator_CompoundID" or "Denominator_CompoundID"}{This is the same
#'   as the argument \code{compoundToExtract} except that you can specify it
#'   separately for each set of PK parameters.}
#'
#'   If you omit any of those columns, whatever you supply for their respective
#'   arguments will be used instead. Note that the respective arguments will
#'   use the \emph{same value} for both the numerator and the denominator
#'   simulations. If you supply something for one of them
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
#'   \item{\strong{Option 2: }specify just the PK parameters you want}{This is
#'   a good option when you want the same information
#'   from all your simulations. List the PK parameters you want here and then,
#'   in the arguments
#'   \code{tissue}, \code{compoundToExtract}, and
#'   \code{sheet_PKparameters} specify what you want for each of those. If
#'   you're going this route, here are the two options
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
#' @param sim_data_file_pairs DEPRECATED. This argument was used in the past to
#'   specify which pairs of files to compare, but we are changing the way we're
#'   asking you to tell us that. Now, we'd like you to give us this information
#'   with the argument \code{PKarameters} and ONLY there. We apologize for the
#'   inconvenience, but, to be frank, this is a pretty complicated function to
#'   put together, and we needed to simplify things.
#' @param compoundToExtract For which compound do you want to extract PK data?
#'   Options are: \itemize{\item{"substrate" (default if left as NA),} \item{"primary
#'   metabolite 1",} \item{"primary metabolite 2",} \item{"secondary
#'   metabolite",} \item{"inhibitor 1" -- this can be an inducer, inhibitor,
#'   activator, or suppressor, but it's labeled as "Inhibitor 1" in the
#'   simulator,} \item{"inhibitor 2" for the 2nd perpetrator listed in the
#'   simulation,} \item{"inhibitor 1 metabolite" for the primary metabolite of
#'   inhibitor 1}} If you have want one compound for the numerator PK and a
#'   different one for the denominator PK, that must be specified in a
#'   data.frame or a csv file that you supply to the argument
#'   \code{PKparameters}.
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are \itemize{\item{"plasma" (default if left as NA)}
#'   \item{"unbound plasma"} \item{"blood"} \item{"unbound blood"}
#'   \item{"peripheral plasma"} \item{"peripheral blood"}} If you want one
#'   tissue for the numerator PK and a different one for the denominator PK,
#'   that must be specified in a data.frame or a csv file that you supply to the
#'   argument \code{PKparameters}.
#' @param sheet_PKparameters If you have a user-defined AUC interval and you
#'   want the PK parameters for to be pulled from that specific tab in the
#'   Simulator output Excel files, list that tab here. If you want standard
#'   first-dose or last-dose PK parameters, leave this as the default NA; we
#'   know where to find those. If you want one tab for the numerator simulation
#'   and a different tab for the denominator simulation, that must be specified
#'   in a data.frame or a csv file that you supply to the argument
#'   \code{PKparameters}.
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get information about how the
#'   simulations were set up, you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails_mult} behind the scenes to figure out some
#'   information about your experimental set up.
#' @param paired TRUE (default) or FALSE for whether the study design is paired,
#'   as in, the subjects are \emph{identical} between the two simulations.
#'   \strong{THIS IS AN IMPORTANT DISTINCTION AND WILL AFFECT HOW THE
#'   CALCULATIONS ARE PERFORMED!} An example of a paired study would be a DDI
#'   study where each subject has a measurement without the perpetrator of
#'   interest and then has a second measurement \emph{with} the perpetrator. The
#'   comparison is for repeated measurements of the \emph{same subject}. An
#'   example of an unpaired study design would be comparing healthy volunteers
#'   to subjects with hepatic impairment because those are measurements on
#'   \emph{different} subjects. For paired study designs, the order of
#'   operations is to calculate each subject's mean ratio and then to calculate
#'   the mean of those ratios. For unpaired study designs, the order of
#'   operations is to calculate the mean for the numerator simulation and then
#'   divide it by the mean for the denominator simulation. Would this be clearer
#'   if you could see the mathematical equations? We agree but can't easily
#'   include equations in the help file. However, if you run this and save the
#'   output to a Word file, the equations will be included in the output.
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
#' @param match_subjects_by For a paired study design, how would you like to
#'   match your subjects? Options are "individual and trial" (default), which
#'   matches by both the simulated individual ID number AND by the trial number,
#'   or "individual only", which matches only by the individual ID number. This
#'   will be ignored for unpaired study designs. Why are we even bothering with
#'   this, you ask? Just to be totally safe, we normally match simulated
#'   subjects by both the individual subject ID and by the trial number. We
#'   thought that this would always work and would be the safest option, but we
#'   discovered that, for some scenarios where you might expect the individuals
#'   to be \emph{exactly} the same between two simulations, they actually were
#'   randomly assigned to different trials. Mismatched subjects would lead to
#'   inaccurate calculations, so we want to avoid that. If you use the default
#'   setting of "individual and trial" but the trials are NOT the same between
#'   simulations, you'll get a warning and no results, which we think is vastly
#'   superior to getting \emph{incorrect} results.
#' @param distribution_type use a "t" distribution (default) or a "Z"
#'   distribution. Note: The Simcyp Simulator calculates geometric confidence
#'   intervals with a t distribution.
#' @param mean_type What kind of means and confidence intervals do you want
#'   listed in the output table? Options are "arithmetic" or "geometric"
#'   (default).
#' @param include_num_denom_columns TRUE (default) or FALSE for whether to
#'   include columns in the output table for the numerator data alone and
#'   columns for the denominator alone. For example, if you wanted to calculate
#'   the dose 1 AUC ratio for cancer patients compared to healthy volunteers,
#'   settting \code{include_num_denom_columns = TRUE} would give you that ratio
#'   and also a column with summary statistics on the AUC for cancer patients
#'   and a column with summary statistics on the AUC for healthy volunteers.
#'   Setting it to FALSE would give you only the ratios.
#' @param conf_int confidence interval to use; default is 90\%
#' @param includeConfInt TRUE (default) or FALSE for whether to include whatever
#'   confidence intervals were included in the simulator output file. Note that
#'   the confidence intervals are geometric since that's what the simulator
#'   outputs (see an AUC tab and the summary statistics; these values are the
#'   ones for, e.g., "90\% confidence interval around the geometric mean(lower
#'   limit)").
#' @param includeCV TRUE (default) or FALSE for whether to include rows for CV
#'   in the table
#' @param include_dose_num NA (default), TRUE, or FALSE on whether to include
#'   the dose number when listing the PK parameter. By default, the parameter
#'   will be labeled, e.g., "Dose 1 Cmax ratio" or "Last dose AUCtau ratio", if
#'   you have PK data for both the first dose and the last dose. Also by
#'   default, if you have data only for the first dose or only for the last
#'   dose, the dose number will be omitted and it will be labeled, e.g., "AUCtau
#'   ratio" or "Cmax ratio". Set this to TRUE or FALSE as desired to override
#'   the default behavior and get exactly what you want.
#' @param prettify_columns TRUE (default) or FALSE for whether to make easily
#'   human-readable column names. TRUE makes pretty column names such as "AUCinf
#'   (h*ng/mL)" whereas FALSE leaves the column with the R-friendly name from
#'   \code{\link{extractPK}}, e.g., "AUCinf_dose1". We're still tweaking this to
#'   make it look just right!
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
#'   different. For example, \code{prettify_compound_names = c("inhibitor" =
#'   "teeswiftavir", "substrate" = "superstatin")}. Please note that "inhibitor"
#'   includes \emph{all} the perpetrators and perpetrator metabolites present,
#'   so, if you're setting the perpetrator name, you really should use something
#'   like this
#'   if you're including perpetrator metabolites: \code{prettify_compound_names =
#'   c("inhibitor" = "teeswiftavir and 1-OH-teeswiftavir", "substrate" =
#'   "superstatin")}.
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
#' @param rounding option for what rounding to perform, if any. Options are:
#'   \describe{\item{NA or "Consultancy"}{All output will be rounded according
#'   to Simcyp Consultancy Team standards: to three significant figures when the
#'   value is < 100 or to the ones place if the value is >= 100. Please see the
#'   function \code{\link{round_consultancy}}, which does the rounding here.}
#'   \item{"none"}{No rounding will be performed.} \item{"significant X" where
#'   "X" is a number}{Output will be rounded to X significant figures. "signif
#'   X" also works fine.} \item{"round X" where "X" is a number}{Output will be
#'   rounded to X digits}}
#' @param checkDataSource TRUE (default) or FALSE for whether to include in the
#'   output a data.frame that lists exactly where the data were pulled from the
#'   simulator output file. Useful for QCing.
#' @param extract_forest_data TRUE or FALSE (default) to get forest-plot data at
#'   the same time. If set to TRUE, this will return a list that includes data
#'   formatted for use with the function \code{\link{forest_plot}}. This will
#'   assume that the denominator is the baseline or control scenario and the
#'   numerator is the comparison. In the output for this, the column "Dose_sub"
#'   will contain the dose of the substrate in the denominator simualtions, and
#'   the column "Dose_inhib" will contain the dose of the inhibitor (if there
#'   was one) in the numerator simulations or the dose of the substrate in the
#'   numerator simulations if there was not. If there was not an inhibitor, the
#'   column "Inhibitor 1" will contain the file names for the numerator sims.
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.docx" or "My table.csv", depending on whether you'd prefer to have
#'   the main PK table saved as a Word or csv file. Do not include any slashes,
#'   dollar signs, or periods in the file name. While the main PK table data
#'   will be in whatever file format you requested, if you set
#'   \code{checkDataSource = TRUE}, the QC data will be in a csv file on its own
#'   and will have "- QC" added to the end of the file name.
#' @param single_table TRUE (default) or FALSE for whether to save all the PK
#'   data in a single table or break the data up by tissue, compound ID, and
#'   file into multiple tables. This only applies to the Word output.
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" or "landscape" (default)
#' @param highlight_gmr_colors optionally specify a set of colors to use for
#'   highlighting geometric mean ratios for DDIs. Options are "yellow to red",
#'   "green to red" or a vector of 4 colors of your choosing. If left as NA, no
#'   highlighting for GMR level will be done.
#' @param conc_units What concentration units should be used in the table?
#'   Default is "ng/mL", but if you set the concentration units to something
#'   else, this will attempt to convert the units to match that. This adjusts
#'   only the simulated values, and it also only affects AUC and Cmax values.
#'   Acceptable input is any concentration unit listed in the Excel form for PE
#'   data entry, e.g. \code{conc_units = "ng/mL"} or \code{conc_units = "uM"}.
#' @param time_units What time units should be used in the table? Default is
#'   "hours", and "days" is the other acceptable option. This adjusts only the
#'   simulated values.
#'
#' @return A list or a data.frame of PK data that optionally includes where the
#'   data came from and data to use for making forest plots
#' @export
#' @examples
#' # No examples yet.
#' 
calc_PK_ratios_mult <- function(PKparameters = NA, 
                                compoundToExtract = NA,
                                tissue = NA, 
                                sheet_PKparameters = NA,
                                existing_exp_details = NA,
                                paired = TRUE,
                                match_subjects_by = "individual and trial", 
                                include_num_denom_columns = TRUE, 
                                conc_units = "ng/mL", 
                                time_units = "hours", 
                                mean_type = "geometric", 
                                conf_int = 0.9, 
                                distribution_type = "t",
                                includeConfInt = TRUE,
                                includeCV = TRUE, 
                                include_dose_num = NA,
                                extract_forest_data = FALSE, 
                                concatVariability = TRUE, 
                                variability_format = "to",
                                prettify_columns = TRUE,
                                prettify_compound_names = TRUE,
                                rounding = NA,
                                checkDataSource = TRUE, 
                                save_table = NA, 
                                name_clinical_study = NA, 
                                shading_column, 
                                highlight_gmr_colors = NA, 
                                single_table = TRUE,
                                page_orientation = "landscape", 
                                fontsize = 11, 
                                sim_data_file_pairs = "deprecated"){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(wrapn("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again."), 
           call. = FALSE)
   }
   
   if("character" %in% class(sim_data_file_pairs) == FALSE ||
      all(sim_data_file_pairs == "deprecated") == FALSE){
      stop(wrapn("The argument 'sim_data_file_pairs' has been deprecated. This argument was used in the past to specify which pairs of files to compare, but we are changing the way we're asking you to tell us that. Now, we'd like you to give us this information with the argument 'PKarameters' and ONLY there. We apologize for the inconvenience, but, to be frank, this is a pretty complicated function to put together and maintain, and we needed to simplify things."), 
           call. = FALSE)
   }
   
   # Only returning geometric means and CI's if they want unpaired data.
   # Uncertain how to set things up otherwise.
   if(paired == FALSE & mean_type != "geometric"){
      warning(wrapn("You have supplied unpaired data and requested something other than geometric means and confidence intervals. We have only set this function up for unpaired data with geometric means and confidence intervals, so that is what will be returned."), 
              call = FALSE)
      mean_type <- "geometric"
   }
   
   if(tolower(distribution_type) %in% c("z", "t")){
      distribution_type <- ifelse(tolower(distribution_type) == "z", 
                                  "Z", "t")
   } else {
      stop(wrapn("You have supplied a value for distribution_type that doesn't work. It must be either `t` (default and what the Simulator uses) or `Z`."), 
           call. = FALSE)
   }
   
   page_orientation <- tolower(page_orientation)[1]
   if(page_orientation %in% c("portrait", "landscape") == FALSE){
      warning(wrapn("You must specify `portrait` or `landscape` for the argument page_orientation, and you've specified something else. We'll use the default of `portrait`."), 
              call. = FALSE)
   }
   
   match_subjects_by <- tolower(match_subjects_by)
   if(match_subjects_by %in% c("individual and trial", 
                               "individual only") == FALSE & 
      paired == TRUE){
      warning(wrapn("You have specified that you would like us to match the subjects in your paired study design by something other than `individual and trial` or `individual only`, which are the only options. We'll use the default of `individual and trial`."), 
              call. = FALSE)
   }
   
   if(complete.cases(highlight_gmr_colors) && 
      tolower(highlight_gmr_colors[1]) == "lisa"){highlight_gmr_colors = "traffic"}
   
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
   
   # Checking rounding
   rounding <- tolower(rounding[1])
   rounding <- sub("signif ", "significant ", rounding)
   rounding <- ifelse(is.na(rounding), "consultancy", rounding)
   if(str_detect(rounding, "consultancy|none|significant|round") == FALSE){
      warning(wrapn("You have entered something for the rounding argument other than the available options. We'll set this to the default, `Consultancy`. Please check the help file for details."), 
              call. = FALSE)
   }
   
   
   # Main body of function -------------------------------------------------
   
   ## Tidying PKparameters ------------------------------------------------
   
   # If they are *only* supplying file pairs and no PK, then use that for
   # PKparameters b/c when it's tidied in the next step, it will work better.
   TEMP <- tidy_input_PK(PKparameters = PKparameters, 
                         compoundsToExtract = compoundToExtract, 
                         tissues = tissue, 
                         sheet_PKparameters = sheet_PKparameters, 
                         existing_exp_details = existing_exp_details)
   PKparameters <- TEMP$PKparameters %>% unique()
   FilePairs <- TEMP$FilePairs %>% unique()
   existing_exp_details <- TEMP$existing_exp_details
   existing_exp_details <- filter_sims(existing_exp_details, 
                                       c(FilePairs$Denominator_File, 
                                         FilePairs$Numerator_File), 
                                       "include")
   
   ## Looping through files ----------------------------------------------------
   
   MyTable <- list()
   QC <- list()
   ForestInfo <- list()
   
   # It's actually FilePairs that we want to supply to the mult version of
   # calc_PK_ratios b/c tidy_input_PK will convert that to the necessary
   # PKparameters and FilePairs is what keeps track of which things match. We'll
   # need one iteration of calc_PK_ratios for every pair of files, compoundIDs,
   # tissues, and sheets. Note that this does NOT include every pair of PK
   # parameters b/c calc_PK_ratios can manage multiple PK parameter but not
   # multiples of the other columns. At least, not multiples of the other
   # columns within the same sim.
   FilePairs <- FilePairs %>% 
      mutate(across(.cols = everything(), 
                    .fns = \(x) ifelse(x == "default", NA, x)), 
             ID = paste(Numerator_File, Denominator_File, 
                        Numerator_CompoundID, Denominator_CompoundID, 
                        Numerator_Tissue, Denominator_Tissue, 
                        Numerator_Sheet, Denominator_Sheet))
   
   FilePairs <- split(FilePairs, f = FilePairs$ID)
   
   for(i in names(FilePairs)){
      # Including a progress message
      message("Extracting data for file pair #", which(names(FilePairs) == i))
      
      TEMP <- calc_PK_ratios(
         PKparameters = FilePairs[[i]], 
         paired = paired, 
         mean_type = mean_type, 
         existing_exp_details = existing_exp_details,
         include_num_denom_columns = include_num_denom_columns, 
         conf_int = conf_int, 
         includeCV = includeCV, 
         # it's easiest for the code if CI's are present even if they don't want
         # them ultimately. Removing this lower in script if they did not ask
         # for CIs.
         includeConfInt = TRUE, 
         include_dose_num = TRUE, # will remove dose number at end if needed
         prettify_columns = FALSE, 
         prettify_compound_names = FALSE, 
         conc_units = conc_units, 
         rounding = "none", # will change this later as needed
         concatVariability = FALSE, # will change this later as needed
         checkDataSource = TRUE, 
         save_table = NA)
      
      MyTable[[i]] <- TEMP$Table
      QC[[i]] <- TEMP$QC
      
      if(is.null(MyTable[[i]])){ next }
      
      # Getting the name of the compound for which we have PK. Most of the time,
      # CmpdNum and CmpdDenom should be the same.
      CmpdNum <- existing_exp_details$MainDetails %>% 
         filter(File == unique(FilePairs[[i]]$Numerator_File)) %>% 
         pull(AllRegCompounds$DetailNames[AllRegCompounds$CompoundID == unique(FilePairs[[i]]$Numerator_CompoundID)])
      
      CmpdDenom <- existing_exp_details$MainDetails %>% 
         filter(File == unique(FilePairs[[i]]$Denominator_File)) %>% 
         pull(AllRegCompounds$DetailNames[AllRegCompounds$CompoundID == unique(FilePairs[[i]]$Denominator_CompoundID)])
      
      suppressWarnings(
         ForestInfo[[i]] <- data.frame(
            File = unique(TEMP$Table$File), 
            Dose_sub = NA, 
            Dose_inhib = NA, 
            Dose_inhib2 = NA, 
            Numerator_CompoundID = unique(FilePairs[[i]]$Numerator_CompoundID), 
            Denominator_CompoundID = unique(FilePairs[[i]]$Denominator_CompoundID), 
            Numerator_Compound = CmpdNum, 
            Denominator_Compound = CmpdDenom, 
            Numerator_File = unique(FilePairs[[i]]$Numerator_File), 
            Denominator_File = unique(FilePairs[[i]]$Denominator_File), 
            Numerator_Tissue = unique(FilePairs[[i]]$Numerator_Tissue), 
            Denominator_Tissue = unique(FilePairs[[i]]$Denominator_Tissue)) %>% 
            left_join(MyTable[[i]] %>% 
                         select(-any_of(c("CompoundID", "Compound", 
                                          "Tissue", "Sheet"))), by = join_by(File))
      )
      
      rm(TEMP, CmpdDenom, CmpdNum)
   }
   
   MyPKResults <- bind_rows(MyTable)
   
   if(nrow(MyPKResults) == 0){
      warning(wrapn("No results could be found."), 
              call. = FALSE)
      return()
   }
   
   # Concatenating and rounding as requested
   if(concatVariability){
      
      # B/c had to run calc_PK_ratios function 1st w/out rounding, data are now
      # wide by PKparameter. Converting to long format, converting to wide by
      # stat, concatenating, then converting back to wide.
      MyPKResults <- MyPKResults %>% 
         pivot_longer(cols = -any_of(c("Statistic", "CompoundID", "Compound", 
                                       "Tissue", "File", "Sheet", 
                                       "Interval_Numerator",
                                       "Interval_Denominator")), 
                      names_to = "PKparameter", 
                      values_to = "Value") %>% 
         filter(complete.cases(Value)) %>% 
         mutate(Value = round_opt(Value,  
                                  round_fun = rounding)) %>% 
         pivot_wider(names_from = Statistic, 
                     values_from = Value) %>% 
         mutate(`90% CI` = case_when(
            complete.cases(`90% CI - Lower`) & complete.cases(`90% CI - Upper`) ~
               switch(variability_format, 
                      "to" = paste(`90% CI - Lower`, "to", `90% CI - Upper`),
                      "hyphen" = paste(`90% CI - Lower`, "-", `90% CI - Upper`),
                      "brackets" = paste0("[", `90% CI - Lower`, ", ", `90% CI - Upper`, "]"), 
                      "parentheses" = paste0("(", `90% CI - Lower`, ", ", `90% CI - Upper`, ")")),
            .default = NA)) %>% 
         select(-`90% CI - Lower`, -`90% CI - Upper`) %>% 
         pivot_longer(cols = -c(CompoundID, Tissue, File, PKparameter, 
                                Interval_Numerator, Interval_Denominator), 
                      names_to = "Statistic", 
                      values_to = "Value") %>% 
         pivot_wider(names_from = PKparameter, 
                     values_from = Value)
   } else {
      MyPKResults <- MyPKResults %>% 
         # Pivoting longer to remove empty cells, get things in order, and more
         # easily round.
         pivot_longer(cols = -any_of(c("File", "Statistic", "Tissue",
                                       "CompoundID",
                                       "Interval_Numerator",
                                       "Interval_Denominator")), 
                      names_to = "PKparameter", 
                      values_to = "Value") %>% 
         separate_wider_delim(cols = PKparameter, delim = " ", 
                              names = c("PKparameter", "Type")) %>% 
         mutate(
            Value = round_opt(x = Value, 
                              round_fun = ifelse(complete.cases(rounding) & 
                                                    rounding == "Word only", 
                                                 "none", rounding)), 
            Type = factor(Type, 
                          levels = c("DenominatorSim", "NumeratorSim",
                                     "Ratio")), 
            PKparameter = factor(PKparameter, 
                                 levels = unique(c(
                                    AllPKParameters %>% 
                                       arrange(SortOrder) %>% 
                                       pull(PKparameter)), 
                                    AllPKParameters %>% 
                                       arrange(SortOrder) %>% 
                                       pull(PKparameter_nodosenum)))) %>% 
         arrange(File, Type, PKparameter) %>% 
         mutate(PKparameter_Type = paste(PKparameter, Type), 
                PKparameter_Type = factor(PKparameter_Type, 
                                          levels = unique(PKparameter_Type))) %>% 
         filter(complete.cases(Value)) %>% 
         select(-PKparameter, -Type) %>% 
         pivot_wider(names_from = PKparameter_Type, 
                     values_from = Value)
   }
   
   # Setting column order
   MyPKResults <- MyPKResults %>% 
      relocate(Interval_Numerator, Interval_Denominator, 
               CompoundID, Tissue, File, .after = last_col())
   
   if(includeConfInt == FALSE){
      MyPKResults <- MyPKResults %>% 
         filter(!str_detect(Statistic, "CI|confidence"))
   }
   
   if(prettify_columns){
      
      PrettyCol <- tibble(OrigName = names(MyPKResults), 
                          GoodCol = prettify_column_names(names(MyPKResults)))
      
      # Setting prettified names.
      MyPKResults <- MyPKResults[, PrettyCol$OrigName]
      names(MyPKResults) <- PrettyCol$GoodCol
      
   }
   
   
   # Setting up table caption ------------------------------------------------
   
   if(single_table){
      
      MyPerpetrator <- determine_myperpetrator(Deets = existing_exp_details, 
                                               prettify_compound_names = TRUE)
      
      DosesIncluded <- c("Dose1" = any(str_detect(PKparameters$PKparameter, "_dose1")),
                         "Last" = any(str_detect(PKparameters$PKparameter, "_last")), 
                         "User" = any(complete.cases(PKparameters$Sheet)))
      DosesIncluded <- str_c(names(DosesIncluded)[DosesIncluded], collapse = " ")
      
      Annotations <- make_table_annotations(
         MyPKResults = MyPKResults %>% purrr::discard(~all(is.na(.))), 
         # existing_exp_details has already been filtered to only contain the
         # sims we need. Using that for specifying which files were included
         # since they have the " / " in the middle, which is different from all
         # the other possible file names people might encounter. 
         MyFile = unique(basename(existing_exp_details$MainDetails$File)), 
         MyCompoundID = unique(MyPKResults$CompoundID), 
         prettify_compound_names = prettify_compound_names,
         existing_exp_details = switch(as.character("logical" %in% class(existing_exp_details)), 
                                       "TRUE" = data.frame(), 
                                       "FALSE" = existing_exp_details), 
         mean_type = mean_type, 
         DosesIncluded = case_match(DosesIncluded, 
                                    "Dose1 User" ~ "Dose1 Last", 
                                    "Last User" ~ "Last", 
                                    .default = DosesIncluded), 
         tissue = unique(MyPKResults$Tissue), 
         name_clinical_study = name_clinical_study)
      
   } else {
      
      return_caption <- FALSE
      
      Annotations <- list("table_heading" = "", 
                          "table_caption" = "")
   }
   
   
   # Saving --------------------------------------------------------------
   
   Out <- list(Table = MyPKResults)
   
   # Need to set this for saving b/c it's an option for pk_table but not here
   interval_in_columns <- FALSE
   
   if(complete.cases(save_table)){
      
      # May need to change the working directory temporarily, so determining
      # what it is now
      CurrDir <- getwd()
      
      # Rounding as necessary
      if(complete.cases(rounding) && rounding == "Word only"){
         MyPKResults <- MyPKResults %>% 
            mutate(across(.cols = where(is.numeric), 
                          .fns = round_opt, round_fun = "Consultancy")) %>% 
            select(-File, File)
      } 
      
      # Checking whether they have specified just "docx" or just "csv" for
      # output b/c then, we'll use "PK ratios" as file name. This allows us to
      # determine what the path should be, too.
      if(str_detect(sub("\\.", "", save_table), "^docx$|^csv$")){
         OutPath <- getwd()
         save_table <- sub("xlsx", 
                           # If they included "." at the beginning of the
                           # file extension, need to remove that here.
                           sub("\\.", "", save_table),
                           "PK ratios")
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
         
         FileName <- basename(save_table)
         
         # Storing some objects so they'll work with the markdown file
         PKToPull <- PKparameters
         MeanType <- mean_type
         FromCalcPKRatios <- TRUE
         TemplatePath <- switch(page_orientation, 
                                "landscape" = system.file("Word/landscape_report_template.dotx",
                                                          package="SimcypConsultancy"), 
                                "portrait" = system.file("Word/report_template.dotx",
                                                         package="SimcypConsultancy"))
         
         add_header_for_DDI <- FALSE
         include_dose_num_orig <- include_dose_num
         
         # Hacking CheckDoseInt, which is needed for the .Rmd file. Not checking
         # this for people here. Too complicated. May add a warning later,
         # though. Just using placeholder data for now.
         CheckDoseInt <- list("message" = c("placeholder.xlsx" = "good"), 
                              "interval" = tibble(File = "placeholder.xlsx", 
                                                  CompoundID = "substrate", 
                                                  Tissue = "plasma"))
         # Similarly, hacking "Sheet" column
         MyPKResults$Sheet <- NA
         
         rmarkdown::render(system.file("rmarkdown/templates/pktable/skeleton/skeleton.Rmd",
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
         write.csv(MyPKResults, paste0(OutPath, "/", save_table), row.names = F)
      }
      
   }
   
   if(extract_forest_data){
      
      GoodPKRatios <- c("Cmax_ratio",
                        "Cmax_ratio_last", 
                        "Cmax_nounits",
                        "Cmax_last_nounits", 
                        "AUCtau_ratio",
                        "AUCtau_ratio_last", 
                        "AUCtau_nounits",
                        "AUCtau_last_nounits",
                        "Cmax_ratio_dose1", 
                        "Cmax_dose1_nounits",
                        "AUC_ratio",
                        "AUCt_ratio",
                        "AUCt_ratio_dose1", 
                        "AUCt_nounits",
                        "AUCinf_nounits", 
                        "AUCinf_dose1_nounits",
                        "AUCinf_ratio",
                        "AUCinf_ratio_dose1")
      
      FD <- bind_rows(ForestInfo) %>% 
         select(File, Statistic,
                any_of(c(
                   "Dose_sub", "Inhibitor1", "Dose_inhib",
                   "Numerator_CompoundID", "Denominator_CompoundID", 
                   "Numerator_Compound", "Denominator_Compound", 
                   "Numerator_File", "Denominator_File",
                   "Numerator_Tissue", "Denominator_Tissue")), 
                matches(" / | Ratio")) %>% 
         filter(str_detect(Statistic, "Ratio|^Simulated$|Lower|Upper")) %>% 
         pivot_longer(cols = -any_of(c("Statistic", "File", "Dose_sub", "Dose_inhib", 
                                       "Substrate", "Inhibitor1", 
                                       "Numerator_CompoundID", "Denominator_CompoundID", 
                                       "Numerator_Compound", "Denominator_Compound", 
                                       "Numerator_File", "Denominator_File",
                                       "Numerator_Tissue", "Denominator_Tissue")), 
                      names_to = "PKparameter", 
                      values_to = "Value") %>% 
         mutate(Statistic = renameStats(Statistic, use = "report to R"),
                Statistic = case_when(Statistic == "Mean" & 
                                         mean_type == "geometric" ~ "Geomean", 
                                      Statistic == "Mean" & 
                                         mean_type == "median" ~ "Median", 
                                      .default = Statistic), 
                # If they used " / " in their specification of PK parameters and
                # used it the way I envision, then we know when we have, e.g.,
                # "AUCinf_ratio_dose1" or "Cmax_ratio_last". However, if they
                # just requested regular PK parameters, then "AUCinf_dose1" is
                # NOT "AUCinf_ratio_dose1" because it is NOT the ratio of AUCinf
                # with perpetrator / AUCinf baseline but the ratio of, e.g.,
                # AUCinf_dose1 with hepatic impairment / AUCinf_dose1 for
                # healthy volunteers or fed / fasted, etc. In any circumstances
                # beyond the ones we can know definitively are the "regular" PK
                # ratios, leave the name of the PK parameter as is. It will NOT
                # currently work with forest_plot, but that's OK. I'd want to
                # KNOW what they're trying to do before adapting the forest_plot
                # function to accommodate anything unusual.
                PKparameter = case_when(
                   PKparameter == "AUCinf_dose1_withInhib / AUCinf_dose1" ~ "AUCinf_ratio_dose1", 
                   PKparameter == "AUCt_dose1_withInhib / AUCt_dose1" ~ "AUCt_ratio_dose1", 
                   PKparameter == "AUCtau_last_withInhib / AUCtau_last" ~ "AUCtau_ratio_last", 
                   PKparameter == "Cmax_dose1_withInhib / Cmax_dose1" ~ "Cmax_ratio_dose1", 
                   PKparameter == "Cmax_last_withInhib / Cmax_last" ~ "Cmax_ratio_last",
                   PKparameter == "AUCinf_dose1 Ratio" ~ "AUCinf_ratio_dose1",
                   PKparameter == "Cmax_dose1 Ratio" ~ "Cmax_ratio_dose1",
                   PKparameter == "AUCinf Ratio" ~ "AUCinf_ratio",
                   PKparameter == "Cmax Ratio" ~ "Cmax_ratio",
                   # Not even sure you can *get* these next few, but including just in case
                   PKparameter == "Cmax_withInhib / Cmax" ~ "Cmax_ratio", 
                   PKparameter == "AUCinf_withInhib / AUCinf" ~ "AUCinf_ratio", 
                   PKparameter == "AUCt_withInhib / AUCt" ~ "AUCt_ratio", 
                   PKparameter == "AUCtau_withInhib / AUCtau" ~ "AUCtau_ratio", 
                   TRUE ~ PKparameter), 
                PKparameter = case_when(PKparameter %in% GoodPKRatios == FALSE & 
                                           str_detect(tolower(PKparameter), "aucinf.*ratio") ~ "AUCinf_ratio_dose1", 
                                        # NB: Cmax regex here MUST be in order
                                        # of _last or _dose1 and then generic
                                        # interval for regex to work correctly.
                                        PKparameter %in% GoodPKRatios == FALSE & 
                                           str_detect(tolower(PKparameter), "cmax.*last.*ratio") ~ "Cmax_ratio_last", 
                                        PKparameter %in% GoodPKRatios == FALSE & 
                                           str_detect(tolower(PKparameter), "cmax.*dose1.*ratio") ~ "Cmax_ratio_dose1", 
                                        PKparameter %in% GoodPKRatios == FALSE & 
                                           str_detect(tolower(PKparameter), "cmax.*ratio") ~ "Cmax_ratio", 
                                        PKparameter %in% GoodPKRatios == FALSE & 
                                           str_detect(tolower(PKparameter), "auctau.*ratio") ~ "AUCtau_ratio_last", 
                                        PKparameter %in% GoodPKRatios == FALSE & 
                                           str_detect(tolower(PKparameter), "auct[^a].*ratio") ~ "AUCt_ratio", 
                                        TRUE ~ PKparameter)) %>% 
         filter(str_detect(PKparameter, "AUCinf_[^P]|AUCt|Cmax")) %>% 
         pivot_wider(names_from = Statistic, values_from = Value)
      
      if(nrow(FD) == 0){
         warning(wrapn("The PK parameters selected don't work for forest plots, which can only take PK parameters for AUCinf, AUCt, AUCtau, and Cmax. We cannot return any forest-plot data."), 
                 call. = FALSE)
         FD <- data.frame()
         
      } 
      
      Out[["ForestData"]] <- FD
      
   }
   
   if(checkDataSource){
      Out[["QC"]] <- bind_rows(QC)
   }
   
   if(length(Out) == 1){
      return(Out[[1]])
   } else {
      return(Out)
   }
   
}

