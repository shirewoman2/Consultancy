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
#' @param sim_data_file_pairs a data.frame or a csv file with at least two
#'   columns: "Numerator", listing the files to use for the numerator of each
#'   calculation, and "Denominator", listing the files to use for the
#'   denominator of each calculation. Each row comprises one pair to use for
#'   calculating ratios. This data.frame or csv file can also be used to specify
#'   the sheets to use for extracting the numerator or denominator data. (Please
#'   see the arguments \code{sheet_PKparameters_num} and
#'   \code{sheet_PKparameters_denom}.)
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
#' @param compoundToExtract For which compound do you want to extract PK data?
#'   Options are: \itemize{\item{"substrate" (default),} \item{"primary
#'   metabolite 1",} \item{"primary metabolite 2",} \item{"secondary
#'   metabolite",} \item{"inhibitor 1" -- this can be an inducer, inhibitor,
#'   activator, or suppressor, but it's labeled as "Inhibitor 1" in the
#'   simulator,} \item{"inhibitor 2" for the 2nd perpetrator listed in the
#'   simulation,} \item{"inhibitor 1 metabolite" for the primary metabolite of
#'   inhibitor 1}} At some point, we may expand this to allow for extracting PK
#'   data for multiple compounds, but we have not done that at this point. Email
#'   Laura Shireman if that's something you really want.
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file. Options are: \describe{
#'
#'   \item{"all"}{all possible parameters}
#'
#'   \item{"AUC tab"}{only those parameters on the "AUC" tab (default). The
#'   "AUC_CI" tab or "AUC_SD" tab will be used if "AUC" tab is not present.}
#'
#'   \item{"Absorption tab"}{only those parameters on the "Absorption" tab.
#'   Please note that we haven't developed this function for output in the
#'   "Overall Fa Fg" tab for ADAM-model simulations yet.}
#'
#'   \item{a vector of any combination of specific, individual parameters, each
#'   surrounded by quotes and encapsulated with \code{c(...)}}{An example:
#'   \code{c("Cmax_dose1", "AUCtau_last")}. To see the full set of possible
#'   parameters to extract, enter \code{view(PKParameterDefinitions)} into the
#'   console. Not case sensitive. If you use "_first" instead of "_dose1", that
#'   will also work.}
#'
#'   \item{a vector of individual parameters with one parameter for the
#'   numerator and whatever parameter you want from the other file for the
#'   denominator, separated by "/"}{The previous options are all for when you
#'   want to take the ratio of the \emph{same} parameter for file 1 / file 2.
#'   However, if you want to compare one PK parameter from file 1 with a
#'   \emph{different} parameter for file 2, you can do that with this option.
#'   Here's an example of how to input the parameters so that you can calculate
#'   the dose 1 AUCinf with an inhibitor present for file 1 divided by the
#'   AUCinf for dose 1 with no inhibitor (baseline) for file 2:
#'   \code{PKparameters = c("AUCinf_dose1_withInhib / AUCinf_dose1")} Please
#'   note that the quotes are around \emph{both} of the two parameters!}}
#'
#'   Currently, the PK data are only for the substrate unless noted, although
#'   you can sometimes hack around this by supplying a specific sheet to extract
#'   for a compound other than the substrate, e.g. sheet = "AUC(Sub Pri Met1)".
#'   This has NOT been as well tested, though, so be sure to check that you're
#'   getting what you expected!
#' @param sheet_PKparameters_num (optional) If you want the PK parameters for
#'   the numerator to be pulled from a specific tab in
#'   \code{sim_data_file_numerator}, list that tab here. Most of the time, this
#'   should be left as NA.
#' @param sheet_PKparameters_denom (optional) If you want the PK parameters for
#'   the numerator to be pulled from a specific tab in
#'   \code{sim_data_file_denominator}, list that tab here. Most of the time,
#'   this should be left as NA.
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default), "unbound plasma", "blood", or "unbound
#'   blood".
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
#' @param conf_int confidence interval to use; default is 90\%
#' @param includeCV TRUE (default) or FALSE for whether to include rows for CV
#'   in the table
#' @param includeConfInt TRUE (default) or FALSE for whether to include whatever
#'   confidence intervals were included in the simulator output file. Note that
#'   the confidence intervals are geometric since that's what the simulator
#'   outputs (see an AUC tab and the summary statistics; these values are the
#'   ones for, e.g., "90\% confidence interval around the geometric mean(lower
#'   limit)").
#' @param include_dose_num NA (default), TRUE, or FALSE on whether to include
#'   the dose number when listing the PK parameter. By default, the parameter
#'   will be labeled, e.g., "Dose 1 Cmax ratio" or "Last dose AUCtau ratio", if
#'   you have PK data for both the first dose and the last dose. Also by
#'   default, if you have data only for the first dose or only for the last
#'   dose, the dose number will be omitted and it will be labeled, e.g., "AUCtau
#'   ratio" or "Cmax ratio". Set this to TRUE or FALSE as desired to override
#'   the default behavior and get exactly what you want.
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
#'   \emph{also} want to use the results from \code{calc_PK_ratios_mult} to make
#'   forest plots, which requires numbers that are \emph{not} rounded.}}
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get all the details from the "Input
#'   Sheet" (e.g., when you ran extractExpDetails_mult you said
#'   \code{exp_details = "Input Sheet"} or \code{exp_details = "all"}), you can
#'   save some processing time by supplying that object here, unquoted. If left
#'   as NA, this function will run \code{extractExpDetails} behind the scenes to
#'   figure out some information about your experimental set up.
#' @param checkDataSource TRUE (default) or FALSE for whether to include in the
#'   output a data.frame that lists exactly where the data were pulled from the
#'   simulator output file. Useful for QCing.
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.docx" or "My table.csv", depending on whether you'd prefer to have
#'   the main PK table saved as a Word or csv file.  Do not include any slashes,
#'   dollar signs, or periods in the file name. If you supply only the file
#'   extension, e.g., \code{save_table = "docx"}, the name of the file will be
#'   the file name plus "PK summary table" with that extension and output will
#'   be located in the same folder as \code{sim_data_file}. If you supply
#'   something other than just "docx" or just "csv" for the file name but you
#'   leave off the file extension, we'll assume you want it to be ".csv". While
#'   the main PK table data will be in whatever file format you requested, if
#'   you set \code{checkDataSource = TRUE}, the QC data will be in a csv file on
#'   its own and will have "- QC" added to the end of the file name.
#' @param single_table TRUE (default) or FALSE for whether to save all the PK
#'   data in a single table or break the data up by tissue, compound ID, and
#'   file into multiple tables. This only applies to the Word output.
#' @param highlight_so_cutoffs DOES NOT CURRENTLY APPLY. Will need to add option
#'   of including obs PK for this to work. -LSh optionally specify cutoffs for
#'   highlighting any simulated-to-observed ratios. Anything that is above those
#'   values or below the inverse of those values will be highlighted. To figure
#'   out what cells to highlight, this looks for a column titled "Statistic" or
#'   "Stat", then looks for what row contains "S/O" or "simulated (something
#'   something) observed" (as in, we'll use some wildcards to try to match your
#'   specific text). Next, it looks for any values in that same row that are
#'   above those cutoffs. This overrides anything else you specified for
#'   highlighting. The default is NA, for \emph{not} highlighting based on S/O
#'   value. Acceptable input for, say, highlighting values that are > 125\% or <
#'   80\% of the observed and also, with a second color, values that are > 150\%
#'   or < 66\% would be: \code{highlight_so_cutoffs = c(1.25, 1.5)}. If you
#'   would like the middle range of values to be highlighted, include 1 in your
#'   cutoffs. For example, say you would like everything that's < 80\% or >
#'   125\% to be highlighted red but you'd like the "good" values from 80\% to
#'   125\% to be green, you can get that by specifying
#'   \code{highlight_so_cutoffs = c(1, 1.25)} and \code{highlight_so_colors =
#'   c("green", "red")}. This only applies when you save the table as a Word file.
#' @param highlight_so_colors DOES NOT CURRENTLY APPLY. Will need to add option
#'   of including obs PK for this to work. -LSh optionally specify a set of
#'   colors to use in the Word file output for highlighting S/O values outside
#'   the limits you
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
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" or "landscape" (default)
#'
#' @return A list or a data.frame of PK data that optionally includes where the
#'   data came from and data to use for making forest plots
#' @export
#' @examples
#' # No examples yet.
#' 
calc_PK_ratios_mult <- function(sim_data_file_pairs,  
                                existing_exp_details = NA,
                                compoundToExtract = NA,
                                tissue = NA, 
                                PKparameters = NA, 
                                sheet_PKparameters_num = NA,
                                sheet_PKparameters_denom = NA,
                                paired = TRUE,
                                match_subjects_by = "individual and trial", 
                                distribution_type = "t",
                                mean_type = "geometric", 
                                include_num_denom_columns = TRUE, 
                                conf_int = 0.9, 
                                includeCV = TRUE, 
                                includeConfInt = TRUE,
                                rounding = NA,
                                checkDataSource = TRUE, 
                                include_dose_num = NA,
                                extract_forest_data = FALSE, 
                                save_table = NA, 
                                single_table = TRUE,
                                page_orientation = "portrait", 
                                fontsize = 11){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Only returning geometric means and CI's if they want unpaired data.
   # Uncertain how to set things up otherwise.
   if(paired == FALSE & mean_type != "geometric"){
      warning(wrapn("You have supplied unpaired data and requested something other than geometric means and confidence intervals. We have only set this function up for unpaired data with geometric means and confidence intervals, so that is what will be returned."), 
              call = FALSE)
      mean_type <- "geometric"
   }
   
   # FIXME - This shouldn't have to be a thing. 
   # if(extract_forest_data & includeConfInt == FALSE){
   #    warning(wrapn("To get forest-plot data, we need the confidence interval, but you have set `includeConfInt = FALSE`. We're going to change that to TRUE so that we can get what we need for forest-plot data."), 
   #            call. = FALSE)
   #    includeConfInt <- TRUE
   # }
   
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
   
   
   # Main body of function -------------------------------------------------
   
   # # Checking experimental details to only pull details that apply
   # if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA
   #    existing_exp_details <- 
   #       extractExpDetails_mult(sim_data_file = c(sim_data_file_numerator, 
   #                                                sim_data_file_denominator), 
   #                              exp_details = "Summary and Input")
   #    Deets <- existing_exp_details$MainDetails
   # } else {
   #    Deets <- harmonize_details(existing_exp_details)[["MainDetails"]] %>%
   #       filter(File %in% c(sim_data_file_numerator, 
   #                          sim_data_file_denominator))
   #    
   #    if(nrow(Deets) != 2){
   #       existing_exp_details <-
   #          extractExpDetails_mult(sim_data_file = c(sim_data_file_numerator, 
   #                                                   sim_data_file_denominator), 
   #                                 exp_details = "Summary and Input", 
   #                                 existing_exp_details = existing_exp_details)
   #       Deets <- existing_exp_details$MainDetails
   #    }
   #    
   #    if(nrow(Deets) != 2){
   #       warning(wrapn(paste0("We were attempting to find the simulation details for ", 
   #                            str_comma(c(sim_data_file_numerator, 
   #                                        sim_data_file_denominator)), 
   #                            " and failed to find them, so we cannot return information on these files.")), 
   #               call. = FALSE)
   #       return(data.frame())
   #    }
   # }
   
   
   ## Tidying PKparameters ------------------------------------------------
   
   TEMP <- tidy_input_PK(PKparameters = sim_data_file_pairs, 
                                 compoundsToExtract = compoundToExtract, 
                                 tissues = tissue, 
                                 existing_exp_details = existing_exp_details)
   
   existing_exp_details <- TEMP %>% pluck("existing_exp_details")
   PKparameters <- TEMP %>% 
      pluck("PKparameters") %>% 
      mutate(Sheet = case_when(is.na(Sheet) & NorD == "Numerator" ~ {{sheet_PKparameters_num}}, 
                               is.na(Sheet) & NorD == "Denominator" ~ {{sheet_PKparameters_denom}}, 
                               .default = Sheet))
   
   rm(TEMP)
   
   # Noting file pairs
   sim_data_file_pairs <- PKparameters %>% 
      select(FilePair, File, NorD) %>% unique() %>% 
      pivot_wider(names_from = NorD, values_from = File)
   
   
   ## Further error checking now that PKparameters are tidy -------------------
   
   # Check for appropriate input for arguments
   tissue <- tolower(tissue)
   
   if(any(complete.cases(PKparameters$Tissue))){
      if(any(is.na(PKparameters$Tissue))){
         stop(wrapn("You have supplied some values for the tissue with the argument 'PKparameters', but you've left some blank. We're not sure what you want when there's a mix of complete and missing values here. Please check your inputs and try again."), 
              call. = FALSE)
      }
   } else {
      PKparameters$Tissue <- ifelse(complete.cases(tissue), tissue, "plasma")
   }
   
   if(all(PKparameters$Tissue %in% 
          c("plasma", "blood", "unbound plasma", "unbound blood", 
            "peripheral plasma", "peripheral blood")) == FALSE){
      
      warning(wrapn("You have not supplied a permissible value for tissue. Options are 'plasma', 'blood', 'unbound plasma', 'unbound blood', 'peripheral plasma', or 'peripheral blood'. The PK parameters will be for plasma whenver you've supplied something else."), 
              call. = FALSE)
      
      PKparameters$Tissue[
         PKparameters$Tissue %in% 
            c("plasma", "blood", "unbound plasma", "unbound blood", 
              "peripheral plasma", "peripheral blood") == FALSE] <- "plasma"
   }
   
   if(any(complete.cases(PKparameters$CompoundID))){
      if(any(is.na(PKparameters$CompoundID))){
         stop(wrapn("You have supplied some values for the compoundID with the argument 'PKparameters', but you've left some blank. We're not sure what you want when there's a mix of complete and missing values here. Please check your inputs and try again."), 
              call. = FALSE)
      }
   } else {
      PKparameters$CompoundID <- ifelse(complete.cases(compoundID), compoundID, "substrate")
   }
   
   if(all(PKparameters$CompoundID %in% AllCompounds$CompoundID) == FALSE){
      
      warning(wrapn("You have not supplied a permissible value for compoundID. Options are 'substrate', 'primary metabolite 1', 'primary metabolite 2', 'secondary metabolite', 'inhibitor 1', 'inhibitor 2', or 'inhibitor 1 metabolite'. The PK parameters will be for the substrate whenver you've supplied something else."), 
              call. = FALSE)
      
      PKparameters$CompoundID[
         PKparameters$CompoundID %in% AllCompounds$CompoundID == FALSE] <- "substrate"
   }
   
   ## Looping through files ----------------------------------------------------
   
   MyTable <- list()
   QC <- list()
   ForestInfo <- list()
   
   for(i in 1:nrow(sim_data_file_pairs)){
      # Including a progress message
      message("Extracting data for file pair #", i)
      
      TEMP <- calc_PK_ratios(
         # sim_data_file_numerator = sim_data_file_pairs$Numerator[i], 
         # sim_data_file_denominator = sim_data_file_pairs$Denominator[i], 
         # compoundToExtract = compoundToExtract,
         PKparameters = PKparameters, 
         # sheet_PKparameters_num = sim_data_file_pairs$sheet_PKparameters_num[i], 
         # sheet_PKparameters_denom = sim_data_file_pairs$sheet_PKparameters_denom[i], 
         # tissue = tissue, 
         paired = paired, 
         mean_type = mean_type, 
         existing_exp_details = existing_exp_details,
         include_num_denom_columns = include_num_denom_columns, 
         conf_int = conf_int, 
         includeCV = includeCV, 
         includeConfInt = includeConfInt, 
         include_dose_num = include_dose_num, 
         prettify_columns = FALSE, 
         prettify_compound_names = FALSE, 
         rounding = "none", 
         checkDataSource = TRUE, 
         returnExpDetails = TRUE,
         save_table = NA)
      
      MyTable[[i]] <- TEMP$Table %>% 
         mutate(File = paste(sim_data_file_pairs$Numerator[i], "/", 
                             sim_data_file_pairs$Denominator[i]))
      QC[[i]] <- TEMP$QC
      
      ForestInfo[[i]] <- data.frame(
         File = paste(sim_data_file_pairs$Numerator[i], "/", 
                      sim_data_file_pairs$Denominator[i]), 
         Dose_sub = TEMP$ExpDetails_denom$Dose_sub, 
         Dose_inhib = switch(as.character(
            complete.cases(TEMP$ExpDetails_num$Inhibitor1)), 
            "TRUE" = TEMP$ExpDetails_num$Dose_inhib, 
            "FALSE" = TEMP$ExpDetails_num$Dose_sub), 
         Substrate = TEMP$ExpDetails_denom$Substrate, 
         Inhibitor1 = switch(as.character(
            complete.cases(TEMP$ExpDetails_num$Inhibitor1)), 
            "TRUE" = TEMP$ExpDetails_num$Inhibitor1, 
            "FALSE" = TEMP$ExpDetails_num$Substrate), 
         File_num = sim_data_file_pairs$Numerator[i], 
         File_denom = sim_data_file_pairs$Denominator[i]) %>% 
         left_join(MyTable[[i]], by = join_by(File))
      
      rm(TEMP)
   }
   
   MyPKResults <- bind_rows(MyTable)
   
   # Setting the rounding option
   round_opt <- function(x, round_fun){
      
      round_fun <- ifelse(is.na(round_fun), "consultancy", tolower(round_fun))
      round_fun <- ifelse(str_detect(tolower(round_fun), "word"), "none", round_fun)
      
      suppressWarnings(
         NumDig <- as.numeric(str_trim(sub("signif(icant)?|round", "", round_fun)))
      )
      
      if(str_detect(round_fun, "signif|round") & 
         !str_detect(round_fun, "[0-9]{1,}")){
         warning(wrapn("You appear to want some rounding, but we're not sure how many digits. We'll use 3 for now, but please check the help file for appropriate input for the argument `rounding`."), 
                 call. = FALSE)
         NumDig <- 3
      }
      
      round_fun <- str_trim(sub("[0-9]{1,}", "", round_fun))
      round_fun <- ifelse(str_detect(round_fun, "signif"), "signif", round_fun)
      
      Out <- switch(round_fun, 
                    "round" = round(x, digits = NumDig),
                    "signif" = signif(x, digits = NumDig), 
                    "consultancy" = round_consultancy(x), 
                    "none" = x)
      
      return(Out)
   }
   
   # Rounding as requested and setting column order
   MyPKResults <- MyPKResults %>% 
      mutate(across(.cols = where(is.numeric), 
                    .fns = round_opt, 
                    round_fun = ifelse(complete.cases(rounding) & 
                                          rounding == "Word only", 
                                       "none", rounding))) %>% 
      relocate(CompoundID, Tissue, File, .after = last_col())
   
   
   # Saving --------------------------------------------------------------
   
   Out <- list(Table = MyPKResults)
   
   
   if(complete.cases(save_table)){
      
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
         
         # May need to change the working directory temporarily, so
         # determining what it is now
         CurrDir <- getwd()
         
         OutPath <- dirname(save_table)
         if(OutPath == "."){
            OutPath <- getwd()
         }
         
         FileName <- basename(save_table)
         
         # Storing some objects so they'll work with the markdown file
         PKToPull <- PKparameters
         MeanType <- mean_type
         FromCalcPKRatios <- TRUE
         highlight_gmr_colors <- NA
         highlight_so_cutoffs = NA
         highlight_so_colors = "yellow to red"
         prettify_columns <- TRUE
         TemplatePath <- switch(page_orientation, 
                                "landscape" = system.file("Word/landscape_report_template.dotx",
                                                          package="SimcypConsultancy"), 
                                "portrait" = system.file("Word/report_template.dotx",
                                                         package="SimcypConsultancy"))
         
         add_header_for_DDI <- FALSE
         
         rmarkdown::render(system.file("rmarkdown/templates/pksummarymult/skeleton/skeleton.Rmd",
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
      
      StatNames <- unique(MyPKResults$Statistic[
         str_detect(MyPKResults$Statistic, "Mean Ratio|CI|^Simulated$")])
      names(StatNames) <- StatNames
      StatNames[which(str_detect(StatNames, "Ratio|^Simulated$"))] <- "GeoMean"
      StatNames[which(str_detect(StatNames, "CI - Lower"))] <- "CI_Lower"
      StatNames[which(str_detect(StatNames, "CI - Upper"))] <- "CI_Upper"
      
      FD <- bind_rows(ForestInfo) %>% 
         select(File, Statistic, Substrate, Dose_sub, Inhibitor1, Dose_inhib,
                matches(" / | Ratio")) %>% 
         filter(str_detect(Statistic, "Ratio|^Simulated$|Lower|Upper")) %>% 
         pivot_longer(cols = -c("Statistic", "File", "Dose_sub", "Dose_inhib", 
                                "Substrate", "Inhibitor1"), 
                      names_to = "PKparameter", 
                      values_to = "Value") %>% 
         mutate(Statistic = StatNames[Statistic],
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

