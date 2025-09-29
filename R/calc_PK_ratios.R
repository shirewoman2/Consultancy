#' Calculate the ratio of PK parameters between two simulations
#'
#' \code{calc_PK_ratios} matches PK data from a pair of simulator output Excel
#' files and calculates the mean and confidence intervals of the ratios of the
#' requested PK parameters. To do this for multiple pairs of simulator output
#' files, please see the function \code{\link{calc_PK_ratios_mult}}. For
#' detailed instructions and examples, please see the SharePoint file "Simcyp
#' PBPKConsult R Files - Simcyp PBPKConsult R Files/SimcypConsultancy function
#' examples and instructions/Calculating PK ratios from separate
#' simulations/Calculating-PK-ratios.docx". (Sorry, we are unable to include a
#' link to it here.)
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
#'   \item{"Numerator_UserAUCSheet" or "Denominator_UserAUCSheet"}{When it's a
#'   user-defined AUC interval you want,
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
#' @param sim_data_file_numerator a simulator output Excel file that will
#'   provide the numerator for the calculated ratios.
#' @param sim_data_file_denominator a simulator output Excel file that will
#'   provide the denominator for the calculated ratios
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
#'   (default). This will give you arithmetic or geometric means for everything
#'   but tmax values, which will be medians.
#' @param conc_units What concentration units should be used in the table?
#'   Default is "ng/mL", but if you set the concentration units to something
#'   else, this will attempt to convert the units to match that. This adjusts
#'   only the simulated values, and it also only affects AUC and Cmax values.
#'   Acceptable input is any concentration unit listed in the Excel form for PE
#'   data entry, e.g. \code{conc_units = "ng/mL"} or \code{conc_units = "uM"}.
#' @param time_units What time units should be used in the table? Default is
#'   "hours"; other acceptable options: "minutes", "hours", "days", or "weeks".
#'   This adjusts only the simulated values.
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
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.docx" or "My table.csv", depending on whether you'd prefer to have
#'   the main PK table saved as a Word or csv file. Do not include any slashes,
#'   dollar signs, or periods in the file name. While the main PK table data
#'   will be in whatever file format you requested, if you set
#'   \code{checkDataSource = TRUE}, the QC data will be in a csv file on its own
#'   and will have "- QC" added to the end of the file name.
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" or "landscape" (default)
#' @param highlight_gmr_colors optionally specify a set of colors to use for
#'   highlighting geometric mean ratios for DDIs. Options are "yellow to red",
#'   "green to red" or a vector of 4 colors of your choosing. If left as NA, no
#'   highlighting for GMR level will be done.
#'
#' @return A list or a data.frame of PK data that optionally includes where the
#'   data came from
#' @export
#' @examples
#' # No examples yet.
#' 
calc_PK_ratios <- function(PKparameters = NA, 
                           sim_data_file_numerator = NA,
                           sim_data_file_denominator = NA, 
                           compoundToExtract = NA, 
                           tissue = NA, 
                           sheet_PKparameters = NA,
                           existing_exp_details = NA,
                           paired = TRUE,
                           match_subjects_by = "individual and trial", 
                           conc_units = "ng/mL", 
                           time_units = "hours", 
                           include_num_denom_columns = TRUE, 
                           mean_type = "geometric",
                           conf_int = 0.9, 
                           distribution_type = "t",
                           includeConfInt = TRUE,
                           includeCV = TRUE, 
                           include_dose_num = NA,
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
                           page_orientation = "landscape", 
                           fontsize = 11){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(wrapn("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again."), 
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
         warning(wrapn("We need 4 colors for highlighting geometric mean ratios, one each for negligible, weak, moderate, and strong interactions, and you have provided a different number of colors. We'll use yellow to red values for highlighting these."), 
                 call. = FALSE)
         highlight_gmr_colors <- "yellow to red"
      } else if(tryCatch(is.matrix(col2rgb(highlight_gmr_colors)),
                         error = function(x) FALSE) == FALSE){
         warning(wrapn("The values you used for highlighting geometric mean ratios are not all valid colors in R. We'll used the default colors instead."), 
                 call. = FALSE)
         highlight_gmr_colors <- "yellow to red"
      } 
   }
   
   # Make sure that input to variability_format is ok
   if(variability_format %in% c("to", "hyphen", "brackets", "parentheses") == FALSE){
      warning(wrapn("The input for variability_format is not among the acceptable options, which are `to`, `hyphen`, `brackets` for square brackets, or `parentheses` for the eponymous symbol if you're an American and a bracket if you're British. We'll use the default of `to`."), 
              call. = FALSE)
      variability_format <- "to"
   }
   
   # Checking rounding
   rounding <- tolower(rounding[1])
   rounding <- sub("signif ", "significant ", rounding)
   rounding <- ifelse(is.na(rounding), "consultancy", rounding)
   if(str_detect(rounding, "consultancy|none|significant|round") == FALSE){
      warning(wrapn("You have entered something for the rounding argument other than the available options. We'll set this to the default, `Consultancy`. Please check the help file for details."), 
              call. = FALSE)
   }
   
   conc_units <- ifelse(is.na(conc_units[1]), "ng/mL", conc_units[1])
   
   
   # Main body of function -------------------------------------------------
   
   ## Tidying PKparameters ------------------------------------------------
   
   # Noting original PK parameters b/c that matters when the user wants one
   # parameter for num and a different one for denom.
   
   PKparameters_orig_NA <- all(is.na(PKparameters))
   
   TEMP <- tidy_input_PK(PKparameters = PKparameters,
                         sim_data_files = NA,
                         sim_data_file_numerator = sim_data_file_numerator, 
                         sim_data_file_denominator = sim_data_file_denominator, 
                         compoundsToExtract = compoundToExtract,
                         tissues = tissue,
                         sheet_PKparameters = sheet_PKparameters, 
                         existing_exp_details = existing_exp_details)
   
   existing_exp_details <- TEMP %>% pluck("existing_exp_details")
   PKparameters <- TEMP %>% pluck("PKparameters")
   
   if(is.na(sim_data_file_denominator) | is.na(sim_data_file_numerator)){
      sim_data_file_denominator <- 
         unique(TEMP$PKparameters$File[TEMP$PKparameters$NorD == "Denominator"])
      sim_data_file_numerator <- 
         unique(TEMP$PKparameters$File[TEMP$PKparameters$NorD == "Numerator"])
   }
   
   if("NorD" %in% names(PKparameters) == FALSE){
      PKparameters <- PKparameters %>% 
         mutate(NorD = case_when(File == sim_data_file_numerator ~ "Numerator", 
                                 File == sim_data_file_denominator ~ "Denominator"))
   }
   
   if(PKparameters_orig_NA |
      # This 2nd option can happen when they originally provided just a
      # character vector of PK parameters s/a "AUCinf_dose1_withInhib /
      # AUCinf_dose1". 
      "FilePair" %in% names(TEMP$FilePairs) == FALSE){
      
      Comparisons <- data.frame(
         Denominator_File = sim_data_file_denominator, 
         Numerator_File = sim_data_file_numerator, 
         Numerator_Tissue = ifelse(is.na(tissue), 
                                   "plasma", tissue), 
         Numerator_Sheet = NA, 
         Numerator_CompoundID = ifelse(is.na(compoundToExtract), 
                                       "substrate", compoundToExtract), 
         Numerator_PKparameter = NA) %>% 
         
         mutate(Denominator_Tissue = Numerator_Tissue, 
                Denominator_Sheet = Numerator_Sheet, 
                Denominator_CompoundID = Numerator_CompoundID, 
                Denominator_PKparameter = Numerator_PKparameter, 
                FilePair = paste(Numerator_File, "/", Denominator_File))
      
      if("logical" %in% class(TEMP$FilePairs) == FALSE & 
         "FilePair" %in% names(TEMP$FilePairs) == FALSE){
         # This is when they've only supplied PK parameters as, e.g.,
         # "AUCinf_dose1_withInhib / AUCinf_dose1", and the problem with only
         # running the above code is that it sets the PK parameters to NA.
         # Filling those back in here.
         Comparisons <- Comparisons %>% 
            select(-Numerator_PKparameter, -Denominator_PKparameter) %>% 
            left_join(TEMP$FilePairs %>% 
                         mutate(FilePair = unique(Comparisons$FilePair)),
                      by = "FilePair", 
                      relationship = "one-to-many")
      }
      
   } else {
      Comparisons <- TEMP %>% pluck("FilePairs")
   }
   
   rm(TEMP)
   
   if("logical" %in% class(Comparisons)){
      # This is when they have not provided a data.frame of PKparameters. In
      # that case, sim_data_file_numerator and sim_data_file_denominator MUST be
      # complete.cases. 
      
      if(any(c(is.na(sim_data_file_denominator),
               is.na(sim_data_file_numerator)))){
         stop(wrapn("You have not provided the simulation file names for the numerator and denominator files. You must either provide a data.frame to the argument PKparameters that includes the numerator and denominator simulation file names or you must supply those file names to the arguments 'sim_data_file_numerator' and 'sim_data_file_denominator'."), 
              call. = FALSE)
      }
      
      Comparisons <- PKparameters %>% 
         mutate(NorD = case_when(File %in% sim_data_file_numerator ~ "Numerator_File", 
                                 File %in% sim_data_file_denominator ~ "Denominator_File")) %>% 
         pivot_wider(names_from = NorD, values_from = File) %>% 
         mutate(Numerator_PKparameter = PKparameter, 
                Denominator_PKparameter = PKparameter, 
                FilePair = paste(Numerator_File, "/", Denominator_File)) %>%
         select(-PKparameter)
   }
   
   if(all(complete.cases(Comparisons$Numerator_PKparameter)) & 
      all(complete.cases(Comparisons$Denominator_PKparameter))){
      
      PKparameters <- PKparameters %>% 
         filter((File %in% Comparisons$Numerator_File & 
                    PKparameter %in% Comparisons$Numerator_PKparameter) |
                   (File %in% Comparisons$Denominator_File & 
                       PKparameter %in% Comparisons$Denominator_PKparameter))
      
   } else {
      PKparameters <- PKparameters %>% 
         filter(File %in% Comparisons$Numerator_File  |
                   File %in% Comparisons$Denominator_File)
      
      # Adding all possible PK parameters to Comparisons b/c we'll need them
      # later. If user did not supply any PK parameters, then we're matching
      # like to like across sims, e.g., we're assuming that AUCinf_dose1 in
      # numerator sim should be compared to AUCinf_dose1 in denominator sim,
      # etc.
      
      PossiblePK <- intersect(PKparameters$PKparameter[PKparameters$NorD == "Numerator"], 
                              PKparameters$PKparameter[PKparameters$NorD == "Denominator"])
      
      if(length(PossiblePK) == 0){
         stop(wrapn(paste0("You have not supplied any specific PK parameters for the file pair '", 
                           unique(Comparisons$FilePair), 
                           "', and the default PK for these two simulations differs because they have different study designs. Please try again with a specific set of PK parameters.")), 
              call. = FALSE)
      }
      
      Comparisons <- Comparisons %>% 
         select(-Numerator_PKparameter, -Denominator_PKparameter) %>% 
         left_join(expand_grid(Denominator_File = unique(Comparisons$Denominator_File), 
                               Denominator_PKparameter = PossiblePK), 
                   by = "Denominator_File") %>% 
         mutate(Numerator_PKparameter = Denominator_PKparameter)
      
   }
   
   # Checking for file name issues
   CheckFileNames <- check_file_name(PKparameters$File)
   BadFileNames <- CheckFileNames[!CheckFileNames == "File name meets naming standards."]
   if(length(BadFileNames)> 0){
      BadFileNames <- paste0(names(BadFileNames), ": ", BadFileNames)
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"), "\n"),
              call. = FALSE)
   }
   
   
   ## Extracting PK ---------------------------------------------------------
   
   suppressWarnings(
      PKnumerator <- extractPK(
         sim_data_file = unique(PKparameters$File[PKparameters$NorD == "Numerator"]),
         compoundToExtract = unique(PKparameters$CompoundID[PKparameters$NorD == "Numerator"]), 
         tissue = unique(PKparameters$Tissue[PKparameters$NorD == "Numerator"]), 
         PKparameters = unique(PKparameters$PKparameter[PKparameters$NorD == "Numerator"]), 
         sheet = unique(PKparameters$Sheet[PKparameters$NorD == "Numerator"]),
         existing_exp_details = existing_exp_details,
         returnExpDetails = FALSE) 
   )
   
   if(length(PKnumerator) == 0){
      warning(wrapn(paste0("We couldn't find PK values matching the requested compound ID and tissue for the numerator simulation '",
                           unique(PKparameters$File[PKparameters$NorD == "Numerator"]), 
                           "', so we can't return any PK comparisons. If you have loaded previously saved output from extractExpDetails and supplied that here, please ensure that you have re-run extractExpDetails following any updates to the simulation results so that we can find the correct data.")), 
              call. = FALSE)
      return(data.frame())
   }
   
   suppressWarnings(
      PKdenominator <- extractPK(
         sim_data_file = unique(PKparameters$File[PKparameters$NorD == "Denominator"]), 
         compoundToExtract = unique(PKparameters$CompoundID[PKparameters$NorD == "Denominator"]), 
         tissue = unique(PKparameters$Tissue[PKparameters$NorD == "Denominator"]), 
         PKparameters = unique(PKparameters$PKparameter[PKparameters$NorD == "Denominator"]), 
         sheet = unique(PKparameters$Sheet[PKparameters$NorD == "Denominator"]),
         existing_exp_details = existing_exp_details,
         returnExpDetails = FALSE)
   )
   
   if(length(PKdenominator) == 0){
      warning(wrapn(paste0("We couldn't find PK values matching the requested compound ID and tissue for the denominator simulation '",
                           unique(PKparameters$File[PKparameters$NorD == "Denominator"]), 
                           "', so we can't return any PK comparisons. If you have loaded previously saved output from extractExpDetails and supplied that here, please ensure that you have re-run extractExpDetails following any updates to the simulation results so that we can find the correct data.")), 
              call. = FALSE)
      return(data.frame())
   }
   
   # For now, using the numerator existing_exp_details as the default.
   existing_exp_details_denom <- existing_exp_details %>%
      filter_sims(unique(PKparameters$File[PKparameters$NorD == "Denominator"]), 
                  "include")
   existing_exp_details <- existing_exp_details %>% 
      filter_sims(unique(PKparameters$File[PKparameters$NorD == "Numerator"]),
                  "include")
   
   
   ## Determining column format & arrangement ---------------------------------
   
   # Keeping track of which columns came from where and the desired order
   # in the output
   ColNames <- data.frame(
      ValType = c(rep("NumeratorSim", 
                      length(unique(PKnumerator$aggregate$PKparameter))), 
                  rep("DenominatorSim", 
                      length(unique(PKdenominator$aggregate$PKparameter)))), 
      OrigName = c(unique(PKnumerator$aggregate$PKparameter), 
                   unique(PKdenominator$aggregate$PKparameter))) %>% 
      mutate(PKparameter = OrigName) %>% 
      arrange(ValType, OrigName) %>% 
      mutate(OrigName_ValType = paste(OrigName, ValType))
   
   # !!!!!!!!!!!!!!! CHANGING COL NAMES FOR DENOMINATOR !!!!!!!!!!!!!!!!!!!!!!
   # Because we need to match parameters when joining, renaming PK parameters
   # in PKdenominator to match the names in PKnumerator even though they're not
   # *really* the same PK parameters necessarily. We'll deal with that
   # difference later.
   
   # Need to filter to get only PK parameters that are actually present. If
   # there were issues with AUCinf extrapolation, then it won't be.
   GoodPKparam <- Comparisons %>% 
      filter(Denominator_PKparameter %in% PKdenominator$aggregate$PKparameter & 
                Numerator_PKparameter %in% PKnumerator$aggregate$PKparameter) %>% 
      select(Denominator_PKparameter, Numerator_PKparameter)
   
   PKdenominator$individual <- PKdenominator$individual %>% 
      filter(PKparameter %in% GoodPKparam$Denominator_PKparameter)
   PKdenominator$aggregate <- PKdenominator$aggregate %>% 
      filter(PKparameter %in% GoodPKparam$Denominator_PKparameter)
   
   PKnumerator$individual <- PKnumerator$individual %>% 
      filter(PKparameter %in% GoodPKparam$Numerator_PKparameter)
   PKnumerator$aggregate <- PKnumerator$aggregate %>% 
      filter(PKparameter %in% GoodPKparam$Numerator_PKparameter)
   
   
   # !!! IMPORTANT PKPARAMETER NAME-CHANGE STEP HERE !!! ----------------------
   
   # Setting this up to match things later. This is a hack to make PKparameter
   # match even if it doesn't really b/c user wanted to compare, e.g.,
   # AUCinf_dose1 to AUCtau_last or something.
   Comparisons$Denominator_PKparameterREVISED <- Comparisons$Numerator_PKparameter
   
   PKreplace <- Comparisons$Denominator_PKparameterREVISED
   names(PKreplace) <- Comparisons$Denominator_PKparameter
   
   PKdenominator$individual$PKparameter <- 
      PKreplace[PKdenominator$individual$PKparameter]
   PKdenominator$aggregate$PKparameter <- 
      PKreplace[PKdenominator$aggregate$PKparameter]
   
   # Dealing with units
   if(existing_exp_details$MainDetails$Units_Cmax != conc_units | 
      existing_exp_details$MainDetails$Units_tmax != time_units){
      
      PKnumerator <- convert_unit_subfun(
         PKlist = PKnumerator, 
         existing_exp_details = existing_exp_details, 
         conc_units = conc_units, 
         time_units = time_units)
      
   }
   
   if(existing_exp_details_denom$MainDetails$Units_Cmax != conc_units | 
      existing_exp_details_denom$MainDetails$Units_tmax != time_units){
      
      PKdenominator <- convert_unit_subfun(
         PKlist = PKdenominator, 
         existing_exp_details = existing_exp_details_denom, 
         conc_units = conc_units, 
         time_units = time_units)
      
   }
   
   # For all individual data, need to remove any columns for anything that might
   # *intentionally* not match, e.g., if they wanted to calculate ratios for
   # blood vs. plasma, etc. For that reason, removing all but the Trial,
   # Individual, PKparameter, and Value columns.
   PKnumerator$individual <- PKnumerator$individual %>% 
      select(Trial, Individual, PKparameter, Value) %>%
      rename(NumeratorSim = Value)
   
   PKdenominator$individual <- PKdenominator$individual %>% 
      select(Trial, Individual, PKparameter, Value) %>%
      rename(DenominatorSim = Value)
   
   
   ## Making paired comparisons -----------------------------------------
   
   if(paired){
      
      if(match_subjects_by == "individual and trial"){
         
         MyPKResults <- PKnumerator$individual %>% 
            full_join(PKdenominator$individual, 
                      join_by(Individual, Trial, PKparameter))
         
      } else if(match_subjects_by == "individual only"){
         MyPKResults <- PKnumerator$individual %>% 
            full_join(PKdenominator$individual %>% 
                         select(-Trial), 
                      join_by(Individual, PKparameter))
      }
      
      MyPKResults <- MyPKResults %>% 
         mutate(Ratio = NumeratorSim / DenominatorSim, 
                ID = paste(Individual, Trial), 
                MatchProblem = !str_detect(PKparameter, "AUCinf") &
                   ((complete.cases(NumeratorSim) & 
                        is.na(DenominatorSim)) |
                       (complete.cases(DenominatorSim) & 
                           is.na(NumeratorSim))))
      
      # Making sure that subjects were matched between numerator and
      # denominator
      if(any(MyPKResults$MatchProblem)){
         warning(wrapn("You do not appear to have perfectly matched subjects in your numerator and denominator simulations. Since you have requested calculations for a paired study design, something is amiss. We don't want to give you *incorrect* results, so we are returning *no results* here. If you have the same individuals but they're not in the same trials, please try setting `match_subjects_by = 'individual only'`. If you actually have an unpaired study design, in which case this mismatch is not a problem, please change `paired` to `FALSE` and try again."), 
                 call. = FALSE)
         
         # Using warning rather than stop so that it doesn't crash
         # calc_PK_ratios_mult.
         return(list())
      }
      
      if(include_num_denom_columns == FALSE){
         MyPKResults <- MyPKResults %>% select(-NumeratorSim, -DenominatorSim)
      } 
      
      MyPKResults <- MyPKResults %>% 
         pivot_longer(cols = switch(as.character(include_num_denom_columns), 
                                    "TRUE" = c("NumeratorSim", "DenominatorSim", "Ratio"), 
                                    "FALSE" = "Ratio"),
                      names_to = "ValType", 
                      values_to = "Value") %>% 
         left_join(data.frame(ValType = "DenominatorSim", 
                              PKparameter = Comparisons$Denominator_PKparameterREVISED,
                              CorrectName = Comparisons$Denominator_PKparameter), 
                   by = join_by(PKparameter, ValType)) %>% 
         mutate(CorrectName = ifelse(is.na(CorrectName), PKparameter, CorrectName)) %>% 
         select(-PKparameter) %>% rename(PKparameter = CorrectName) %>% 
         group_by(PKparameter, ValType) %>% 
         summarize(
            Mean = switch(
               mean_type, 
               "geometric" = round_opt(gm_mean(Value), rounding), 
               "arithmetic" = round_opt(mean(Value, na.rm = T), rounding)), 
            
            CV = switch(
               mean_type, 
               "geometric" = round_opt(100*gm_CV(Value), rounding),
               "arithmetic" = round_opt(100*sd(Value, na.rm = T) /
                                           mean(Value, na.rm = T), rounding)),
            
            CI90_lower = switch(
               mean_type, 
               # NB: As of 2023-06-15, the default statistic for gm_conf is a t
               # statistic, which matches the output from the Simulator. -LSh
               "geometric" = round_opt(gm_conf(Value, CI = conf_int, 
                                               distribution_type = distribution_type)[1], rounding),
               "arithmetic" = round_opt(confInt(Value, CI = conf_int, 
                                                distribution_type = distribution_type)[1], rounding)),
            
            CI90_upper = switch(
               mean_type, 
               "geometric" = round_opt(gm_conf(Value, CI = conf_int, 
                                               distribution_type = distribution_type)[2], rounding),
               "arithmetic" = round_opt(confInt(Value, CI = conf_int, 
                                                distribution_type = distribution_type)[2], rounding)), 
            
            Median = round_opt(median(Value), rounding), 
            Minimum = round_opt(min(Value), rounding), 
            Maximum = round_opt(max(Value), rounding)) %>% 
         ungroup()
      
      # Need to deal with any tmax values since those should be medians
      MyPKResults <- MyPKResults %>% 
         mutate(Mean = case_when(str_detect(PKparameter, "tmax") ~ Median, 
                                 .default = Mean), 
                CV = case_when(str_detect(PKparameter, "tmax") ~ NA, 
                               .default = CV), 
                CI90_lower = case_when(str_detect(PKparameter, "tmax") ~ Minimum, 
                                       .default = CI90_lower), 
                CI90_upper = case_when(str_detect(PKparameter, "tmax") ~ Maximum, 
                                       .default = CI90_upper)) %>% 
         select(-Median, -Minimum, -Maximum)
      
      if(concatVariability){
         MyPKResults <- MyPKResults %>% 
            mutate(CI90concat = case_when(
               all(complete.cases(c(CI90_lower, CI90_upper))) ~
                  switch(variability_format, 
                         "to" = paste(CI90_lower, "to", CI90_upper),
                         "hyphen" = paste(CI90_lower, "-", CI90_upper),
                         "brackets" = paste0("[", CI90_lower, ", ", CI90_upper, "]"), 
                         "parentheses" = paste0("(", CI90_lower, ", ", CI90_upper, ")")),
               .default = NA)) %>% 
            select(-CI90_lower, -CI90_upper)
      }
      
      if(mean_type == "geometric"){
         MyPKResults <- MyPKResults %>% 
            rename(Geomean = Mean, 
                   GCV = CV)
      }
      
      MyPKResults <- MyPKResults %>% 
         pivot_longer(cols = -c("PKparameter", "ValType"), 
                      names_to = "Statistic", 
                      values_to = "Value") %>% 
         left_join(ColNames %>% select(ValType, OrigName, PKparameter), 
                   by = join_by(PKparameter, ValType)) %>% 
         mutate(PKparameter = case_when(is.na(OrigName) ~ PKparameter, 
                                        complete.cases(OrigName) ~ OrigName),
                PKparameter = paste(PKparameter, ValType)) %>% 
         select(-ValType, -OrigName) %>% 
         pivot_wider(names_from = PKparameter, values_from = Value)
      
      
      ## Making unpaired comparisons -----------------------------------------
      
   } else {
      
      if(mean_type == "arithmetic"){
         warning(wrapn("This function has been set up to calculate geometric mean ratios and has not been set up for arithmetic mean ratios. We will return geometric mean ratios only."),
                 call. = FALSE)
      }
      
      # Using calculations recommended by Frederic Bois for the confidence
      # interval for UNPAIRED comparisons. (Note to self: See email from March
      # 3, 2023. -LSh)
      geomratio_stats <- function(x_num, 
                                  x_denom, 
                                  distribution_type = distribution_type, 
                                  rounding = rounding, 
                                  conf_int = conf_int){
         
         # Log transforming individual data
         logx_num <- log(x_num)
         logx_denom <- log(x_denom)
         
         # Calculating the difference of the means of the log-transformed
         # data; this is equivalent to the ratio of the geometric means,
         # i.e., gm_mean(x1) / gm_mean(x2) = exp(mean(logx1) - mean(logx2))
         LogGeomeanRatio <- mean(logx_num) - mean(logx_denom)
         
         # Variance of each vector
         Var_num <- var(logx_num)/length(x_num)
         Var_denom <- var(logx_denom)/length(x_denom)
         
         # Using that to calculate the variance of the ratio and then the
         # standard deviation
         Var_delta <- sum(Var_num, Var_denom)
         SD_delta <- sqrt(Var_delta)
         
         # Z distribution. This is generally fine but is not what the Simulator
         # uses.
         if(distribution_type == "Z"){
            suppressWarnings(
               CI_lower_delta <- LogGeomeanRatio - qnorm(1-(1-conf_int)/2)*SD_delta)
            
            suppressWarnings(
               CI_upper_delta <- LogGeomeanRatio + qnorm(1-(1-conf_int)/2)*SD_delta)
            
         } else if(distribution_type == "t"){
            # t distribution, which is what the Simulator uses
            suppressWarnings(
               CI_lower_delta <- LogGeomeanRatio -
                  qt(p = 1-(1-conf_int)/2, 
                     df = (min(c(length(x_num) - 1, length(x_denom) - 1)))) * SD_delta)
            
            suppressWarnings(
               CI_upper_delta <- LogGeomeanRatio + 
                  qt(p = 1-(1-conf_int)/2, 
                     df = (min(c(length(x_num) - 1, length(x_denom) - 1)))) * SD_delta)
         }
         
         Out <- c("Mean" = round_opt(exp(LogGeomeanRatio), rounding), 
                  "CI90_lower" = round_opt(exp(CI_lower_delta), rounding),
                  "CI90_upper" = round_opt(exp(CI_upper_delta), rounding))
         
         return(Out)
         
      }
      
      MyPKResults <- list()
      for(param in unique(PKnumerator$individual$PKparameter)){
         if(str_detect(param, "tmax")){
            MyPKResults[[param]] <- c(
               # Calculating the ratio of medians here, but ranges would not
               # apply. Also not calculating CIs since the observed data are
               # censored and I don't know how to deal w/that. Not sure you
               # *can* deal with that well, so omitting.
               "Mean" =
                  round_opt(
                     median(PKnumerator$individual$NumeratorSim[
                        PKnumerator$individual$PKparameter == param]) / 
                        median(PKdenominator$individual$DenominatorSim[
                           PKdenominator$individual$PKparameter == param]), 
                     rounding), 
               "CI90_lower" = NA, 
               "CI90_upper" = NA)
            
         } else {
            
            MyPKResults[[param]] <- 
               geomratio_stats(
                  x_num = PKnumerator$individual$NumeratorSim[
                     PKnumerator$individual$PKparameter == param], 
                  
                  x_denom = PKdenominator$individual$DenominatorSim[
                     PKdenominator$individual$PKparameter == param],
                  
                  distribution_type = distribution_type, 
                  rounding = rounding, 
                  conf_int = conf_int)
         }
      }
      
      MyPKResults <- bind_rows(MyPKResults, .id = "PKparameter")
      
      if(concatVariability){
         MyPKResults <- MyPKResults %>% 
            mutate(CI90concat = case_when(
               all(complete.cases(c(CI90_lower, CI90_upper))) ~
                  switch(variability_format, 
                         "to" = paste(CI90_lower, "to", CI90_upper),
                         "hyphen" = paste(CI90_lower, "-", CI90_upper),
                         "brackets" = paste0("[", CI90_lower, ", ", CI90_upper, "]"), 
                         "parentheses" = paste0("(", CI90_lower, ", ", CI90_upper, ")")),
               .default = NA)) %>% 
            select(-CI90_lower, -CI90_upper)
      }
      
      if(mean_type == "geometric"){
         MyPKResults <- MyPKResults %>% 
            rename(Geomean = Mean)
      }
      
      # Getting this into a form that matches form from paired option.
      MyPKResults <- MyPKResults %>% 
         pivot_longer(cols = -PKparameter, 
                      names_to = "Statistic", values_to = "Value") %>% 
         mutate(ValType = "Ratio", 
                PKparameter = paste(PKparameter, ValType))
      
      if(include_num_denom_columns){
         
         PKnum_agg <- PKnumerator$aggregate %>%
            select(-any_of(c("File", "CompoundID", "Compound", "Inhibitor",
                             "Tissue", "Simulated", "Dose"))) %>% 
            pivot_longer(cols = -PKparameter, 
                         names_to = "Statistic",
                         values_to = "Value") %>% 
            mutate(ValType = "NumeratorSim", 
                   Value = round_opt(Value, rounding))
         
         # Earlier, had to change column names of denominator results for
         # matching w/numerator results when joining data.frames. Now, need to
         # change names back to what the values *actually are* so that things
         # don't get any further confused.
         PKdenom_agg <- PKdenominator$aggregate %>%
            select(-any_of(c("File", "CompoundID", "Compound", "Inhibitor",
                             "Tissue", "Simulated", "Dose"))) %>% 
            pivot_longer(cols = -PKparameter, 
                         names_to = "Statistic",
                         values_to = "Value") %>% 
            mutate(ValType = "DenominatorSim", 
                   Value = round_opt(Value, rounding)) %>% 
            left_join(Comparisons %>%
                         select(Numerator_PKparameter,
                                Denominator_PKparameter) %>% 
                         rename(PKparameter = Numerator_PKparameter), 
                      by = "PKparameter") %>% 
            mutate(PKparameter = Denominator_PKparameter) %>% 
            select(-Denominator_PKparameter) 
         
         # Binding the numerator and denominator data together and
         # formatting to match ratio data
         PKnum_denom_agg <- bind_rows(PKnum_agg, PKdenom_agg) %>% 
            mutate(
               Keep = 
                  (str_detect(PKparameter, "tmax") & 
                      Statistic %in% c("Median", "Minimum", "Maximum")) |
                  
                  (!str_detect(PKparameter, "tmax") & 
                      Statistic %in% switch(mean_type,
                                            "geometric" = c("Geomean", 
                                                            "CI90_lower", 
                                                            "CI90_upper"),
                                            # I'm not even sure the arithmetic means
                                            # would ever come up for the ratios for
                                            # the way I've set things up, so this is
                                            # probably moot.
                                            "arithmetic" = c("Mean", 
                                                             "CI90_lower", 
                                                             "CI90_upper")))) %>% 
            filter(Keep == TRUE) %>% select(-Keep) %>% 
            mutate(PKparameter = paste(PKparameter, ValType), 
                   Statistic = case_when(
                      str_detect(PKparameter, "tmax") & 
                         mean_type == "geometric" & 
                         Statistic == "Median" ~ "Geomean", 
                      
                      str_detect(PKparameter, "tmax") & 
                         mean_type == "geometric" & 
                         Statistic == "Minimum" ~ "CI90_lower", 
                      
                      str_detect(PKparameter, "tmax") & 
                         mean_type == "geometric" & 
                         Statistic == "Maximum" ~ "CI90_upper", 
                      
                      str_detect(PKparameter, "tmax") & 
                         mean_type == "arithmetic" & 
                         Statistic == "Median" ~ "Mean", 
                      
                      str_detect(PKparameter, "tmax") & 
                         mean_type == "arithmetic" & 
                         Statistic == "Minimum" ~ "CI90_lower", 
                      
                      str_detect(PKparameter, "tmax") & 
                         mean_type == "arithmetic" & 
                         Statistic == "Maximum" ~ "CI90_upper",
                      
                      .default = Statistic))
         
         if(concatVariability){
            PKnum_denom_agg <- PKnum_denom_agg %>% 
               pivot_wider(names_from = Statistic, 
                           values_from = Value) %>% 
               mutate(CI90concat = case_when(
                  all(complete.cases(c(CI90_lower, CI90_upper))) ~
                     switch(variability_format, 
                            "to" = paste(CI90_lower, "to", CI90_upper),
                            "hyphen" = paste(CI90_lower, "-", CI90_upper),
                            "brackets" = paste0("[", CI90_lower, ", ", CI90_upper, "]"), 
                            "parentheses" = paste0("(", CI90_lower, ", ", CI90_upper, ")")),
                  .default = NA)) %>% 
               select(-CI90_lower, -CI90_upper) %>% 
               pivot_longer(cols = -c(PKparameter, ValType), 
                            names_to = "Statistic", 
                            values_to = "Value")
         }
         
         MyPKResults <- MyPKResults %>% 
            bind_rows(PKnum_denom_agg) %>% 
            select(-ValType) %>% 
            pivot_wider(names_from = PKparameter,
                        values_from = Value)
      }
   }
   
   # Filtering ColNames for columns that are still present and then using
   # ColNames to set factor order
   ColNames <- ColNames %>%
      filter((ValType == "DenominatorSim" &
                 PKparameter %in% switch(
                    as.character(all(Comparisons$Numerator_PKparameter == 
                                        Comparisons$Denominator_PKparameter)), 
                    "TRUE" = Comparisons$Denominator_PKparameterREVISED, 
                    "FALSE" = Comparisons$Denominator_PKparameter)) |
                (ValType == "NumeratorSim" &
                    PKparameter %in% Comparisons$Numerator_PKparameter))
   ColOrder <- unique(c(ColNames$OrigName_ValType,
                        paste(unique(ColNames$PKparameter[
                           ColNames$ValType == "NumeratorSim"]), "Ratio")))
   
   MyPKResults <- MyPKResults[, c("Statistic", ColOrder)]
   
   # At this point, MyPKResults has columns for Statistic and then one
   # column for each combination of PKparameter and ValType, and the order
   # left-to-right is 1) all PK parameters for the denominator (control)
   # simulation, in alphabetical order, 2) all PK parameters for the
   # numerator (comparison) simulation, in alphabetical order, 3) all
   # ratios of numerator / denominator PK in alphabetical order. If user
   # only wanted ratios, then the only ValType is "Ratio". Otherwise,
   # ValType is "Ratio", "DenominatorSim", and "NumeratorSim".
   
   
   # Tidying up names of columns, etc. ------------------------------------
   
   # Any time AUCinf_dose1 was requested, only retain any AUCt_X that were
   # specifically requested or when AUCinf could not be returned.
   AUCOrigReq <- PKparameters %>% 
      mutate(ColName = paste(PKparameter, NorD)) %>% 
      filter(OriginallyRequested == TRUE & 
                str_detect(ColName, "AUC.*_dose1")) %>% 
      pull(ColName) %>% unique()
   
   AUCpulled <- prettify_column_names(
      PKtable = MyPKResults, 
      return_which_are_PK = T) %>% 
      filter(IsPKParam == TRUE & 
                str_detect(ColName, "AUC.*_dose1")) %>%
      pull(ColName) %>% unique()
   
   if(any(str_detect(PKparameters$PKparameter, "AUCinf")) & 
      (length(AUCOrigReq) == 0 | # this is when user left PKparameters as NA and accepted default ones 
       any(str_detect(AUCOrigReq, "AUCinf")))){
      
      # If any of the AUCs requested were AUCt, then retain ALL AUCt. If any of
      # the AUCinfs had NAs, then retain ALL AUCt.
      AUCtreqorig <- any(str_detect(AUCOrigReq, "AUCt_dose1"))
      ExtrapProbs_denom <-  
         PKdenominator$aggregate %>% 
         filter(str_detect(PKparameter, "AUCinf"))
      ExtrapProbs_denom <- nrow(ExtrapProbs_denom) == 0 |
         is.na(ExtrapProbs_denom$Geomean)
      ExtrapProbs_num <-  
         PKdenominator$aggregate %>% 
         filter(str_detect(PKparameter, "AUCinf"))
      ExtrapProbs_num <- nrow(ExtrapProbs_num) == 0 |
         is.na(ExtrapProbs_num$Geomean)
      
      if((AUCtreqorig | ExtrapProbs_denom | ExtrapProbs_num) == FALSE){
         MyPKResults <- MyPKResults %>% 
            select(-matches("AUCt_dose1"))
      }
   }
   
   MyPKResults$Statistic <- renameStats(MyPKResults$Statistic, use = "report")
   
   # Only including the variability measurements user requested
   if(includeCV == FALSE){
      MyPKResults <- MyPKResults %>% 
         filter(!Statistic %in% c("CV%", "GCV%"))
   }
   
   if(includeConfInt == FALSE){
      MyPKResults <- MyPKResults %>% 
         filter(!Statistic %in% c("90% CI - Lower", "90% CI - Upper", 
                                  "90% CI"))
   }
   
   if(prettify_columns){
      
      PrettyCol <- tibble(OrigName = names(MyPKResults), 
                          GoodCol = prettify_column_names(names(MyPKResults)))
      
      # Adjusting units as needed.
      PrettyCol <- PrettyCol %>% 
         mutate(GoodCol = sub("ng/mL.h", 
                              paste0(conc_units, ".", 
                                     case_match(time_units, 
                                                "minutes" ~ "min", 
                                                "hours" ~ "h", 
                                                "days" ~ "d", 
                                                "weeks" ~ "wk")), GoodCol), 
                GoodCol = sub("L/h", 
                              paste0("L/", 
                                     case_match(time_units, 
                                                "minutes" ~ "min", 
                                                "hours" ~ "h", 
                                                "days" ~ "d", 
                                                "weeks" ~ "wk")), GoodCol), 
                GoodCol = sub("ng/mL", conc_units, GoodCol), 
                GoodCol = sub("\\(h\\)", 
                              paste0("(", 
                                     case_match(time_units, 
                                                "minutes" ~ "min", 
                                                "hours" ~ "h", 
                                                "days" ~ "d", 
                                                "weeks" ~ "wk"), 
                                     ")"), GoodCol))
      
      # Setting prettified names.
      MyPKResults <- MyPKResults[, PrettyCol$OrigName]
      names(MyPKResults) <- PrettyCol$GoodCol
   }
   
   # Checking on possible perpetrators to prettify
   MyPerpetrator <- c(existing_exp_details$MainDetails$Inhibitor1, 
                      existing_exp_details$MainDetails$Inhibitor1Metabolite, 
                      existing_exp_details$MainDetails$Inhibitor2)
   if(length(MyPerpetrator) > 0){
      MyPerpetrator <- str_comma(MyPerpetrator[complete.cases(MyPerpetrator)])
      MyPerpetrator <- ifelse(MyPerpetrator == "", NA, MyPerpetrator)
   } else {
      MyPerpetrator <- NA
   }
   
   if(any(complete.cases(MyPerpetrator))){
      
      if(class(prettify_compound_names) == "logical" &&
         prettify_compound_names){
         MyPerpetrator <- prettify_compound_name(MyPerpetrator)
      }
      
      if(class(prettify_compound_names) == "character"){
         names(prettify_compound_names)[
            str_detect(tolower(names(prettify_compound_names)), 
                       "inhibitor")][1] <- "inhibitor"
         MyPerpetrator <- prettify_compound_names["inhibitor"]
      }
      
      # Prettifying perpetrator names as necessary
      names(MyPKResults) <- sub("perpetrator", MyPerpetrator, names(MyPKResults))
   }
   
   include_dose_num <- check_include_dose_num(PK = MyPKResults, 
                                              include_dose_num = include_dose_num)
   
   if(include_dose_num == FALSE){
      names(MyPKResults) <- sub("Dose 1 |Last dose |_dose1|_last", "",
                                names(MyPKResults))
   }
   
   # Noting compound ID, tissue, and Files
   if("Numerator_CompoundID" %in% names(Comparisons)){
      
      MyPKResults$CompoundID <- Comparisons %>% 
         select(Numerator_CompoundID, Denominator_CompoundID) %>% 
         unique() %>% 
         mutate(across(.cols = everything(), 
                       .fns = \(x) ifelse(is.na(x), "substrate", x))) %>% 
         mutate(AllCmpdID = ifelse(Numerator_CompoundID == Denominator_CompoundID, 
                                   Numerator_CompoundID, 
                                   paste(Numerator_CompoundID, "/", Denominator_CompoundID))) %>% 
         pull(AllCmpdID) %>% str_comma()
      
   } else {
      MyPKResults$CompoundID <- Comparisons %>% pull(CompoundID) %>% 
         unique() %>% str_comma()
   }
   
   if("Numerator_Tissue" %in% names(Comparisons)){
      
      MyPKResults$Tissue <- Comparisons %>% 
         select(Numerator_Tissue, Denominator_Tissue) %>% 
         unique() %>% 
         mutate(across(.cols = everything(), 
                       .fns = \(x) ifelse(is.na(x), "plasma", x))) %>% 
         mutate(AllCmpdID = ifelse(Numerator_Tissue == Denominator_Tissue, 
                                   Numerator_Tissue, 
                                   paste(Numerator_Tissue, "/", Denominator_Tissue))) %>% 
         pull(AllCmpdID) %>% str_comma()
      
   } else {
      MyPKResults$CompoundID <- Comparisons %>% pull(Tissue) %>% 
         unique() %>% str_comma()
   }
   
   MyPKResults$File <- Comparisons %>% pull(FilePair) %>% unique()
   MyPKResults$Interval_Numerator <- unique(PKnumerator$TimeInterval$Interval)
   MyPKResults$Interval_Denominator <- unique(PKdenominator$TimeInterval$Interval)
   
   # Setting column order 
   MyPKResults <- MyPKResults %>% 
      relocate(Interval_Numerator, Interval_Denominator, 
               any_of(c("CompoundID",
                        "CompoundID_Numerator",
                        "CompoundID_Denominator", 
                        "Tissue",
                        "Tissue_Numerator", 
                        "Tissue_Denominator", 
                        "File")), 
               .after = last_col())
   
   if(prettify_columns){
      names(MyPKResults) <- sub("NumeratorSim", "numerator simulation", names(MyPKResults))  
      names(MyPKResults) <- sub("DenominatorSim", "denominator simulation", names(MyPKResults))
      names(MyPKResults) <- sub("_Denominator", " denominator simulation", names(MyPKResults))  
      names(MyPKResults) <- sub("_Numerator", " numerator simulation", names(MyPKResults))  
   }
   
   # Saving --------------------------------------------------------------
   
   MyPKResults_out <- MyPKResults
   
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
      MyFile = c(sim_data_file_numerator, sim_data_file_denominator), 
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
   
   if(paired){
      Annotations$table_caption <- 
         sub("except for t~max~ values, which are medians and ranges.", 
             "except for t~max~ values, which are medians and ranges. t~max~ ratios are the median of individual ratios and the range of individual ratios. No confidence intervals are calculated since t~max~ values are censored and thus do not follow a normal distribution.", 
             Annotations$table_caption)
   } else {
      Annotations$table_caption <- 
         sub("except for t~max~ values, which are medians and ranges.", 
             "except for t~max~ values, which are medians and ranges. t~max~ ratios are the ratio of median t~max~ values for each group, and no confidence intervals are calculated since t~max~ values are censored and thus do not follow a normal distribution.", 
             Annotations$table_caption)
   }
   
   # Rounding as necessary
   if(complete.cases(rounding)){
      MyPKResults <- MyPKResults %>% 
         mutate(across(.cols = where(is.numeric), 
                       .fns = round_opt, round_fun = rounding))
      
   } 
   
   if(complete.cases(save_table)){
      
      FileName <- save_table
      if(str_detect(FileName, "\\.")){
         # Making sure they've got a good extension
         Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
         FileName <- sub(paste0(".", Ext), "", FileName)
         if(Ext %in% c("docx", "csv") == FALSE){
            warning(wrapn(paste0("You have requested the table's file extension be `", 
                                 Ext, "`, but we haven't set up that option. We'll save your graph as a Word file instead.")),
                    call. = FALSE)
            Ext <- "docx"
         }
         FileName <- paste0(FileName, ".", Ext)
      } 
      
      OutPath <- dirname(FileName)
      if(OutPath == "."){
         OutPath <- getwd()
      }
      
      FileName <- basename(FileName)
      
      if(Ext == "docx"){ 
         # This is when they want a Word file as output
         
         FileName <- basename(save_table)
         
         # Storing some objects so they'll work with the markdown file
         PKToPull <- PKparameters
         MeanType <- mean_type
         sim_data_file <- str_comma(c(basename(unique(Comparisons$Numerator_File)),
                                      basename(unique(Comparisons$Denominator_File))))
         highlight_so_cutoffs = NA
         highlight_so_colors = "yellow to red"
         prettify_columns <- TRUE
         
         FromCalcPKRatios <- TRUE
         
         PKpulled <- Comparisons$Denominator_PKparameter
         
         CheckDoseInt_1 <- list()
         
         for(i in c(unique(Comparisons$Numerator_File),
                    unique(Comparisons$Denominator_File))){
            
            suppressWarnings(
               CheckDoseInt_1[[i]] <- check_doseint(sim_data_file = i, 
                                                    existing_exp_details = existing_exp_details,
                                                    compoundID = compoundToExtract,
                                                    stop_or_warn_missing_file = "warn")
            )
         }
         
         suppressWarnings(
            CheckDoseInt <- list("message" = ifelse(any(map(CheckDoseInt_1, "message") == "mismatch"), 
                                                    "mismatch", "good"),
                                 "interval" = as.data.frame(lapply(bind_rows(
                                    map(CheckDoseInt_1, "interval")),
                                    FUN = function(x) str_c(unique(x), collapse = ", "))) %>% 
                                    mutate(across(.cols = -c(File, CompoundID), 
                                                  .fns = as.numeric)))
         )
         
         TemplatePath <- switch(page_orientation, 
                                "landscape" = system.file("Word/landscape_report_template.dotx",
                                                          package="SimcypConsultancy"), 
                                "portrait" = system.file("Word/report_template.dotx",
                                                         package="SimcypConsultancy"))
         
         add_header_for_DDI <- FALSE
         single_table <- TRUE # placeholder
         interval_in_columns <- FALSE
         
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
   
   Out <- list("Table" = MyPKResults_out)
   
   if(checkDataSource){
      Out[["QC"]] <- bind_rows(PKnumerator$QC, PKdenominator$QC)
      
      if(complete.cases(save_table)){ 
         write.csv(Out[["QC"]], sub(".csv|.docx", " - QC.csv", save_table), row.names = F)
      }
      
   }
   
   if(length(Out) == 1){
      return(Out[[1]])
   } else {
      return(Out)
   }
   
}

