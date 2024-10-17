#' Make graphs comparing center statistic and variability in PK across trials
#' and, optionally, against observed PK data as well
#'
#' @description UNDER CONSTRUCTION. For now, must supply observed_PK as a
#'   data.frame.
#'
#'
#' @param sim_data_file name of the Excel file containing the simulator output,
#'   in quotes
#' @param sheet optionally specify the name of the sheet where you'd like to
#'   pull the PK data, in quotes; for example, specify the tab where you have a
#'   user-defined AUC integration. \emph{Note:} Unless you want a very specific
#'   Excel sheet that's not what the usual sheet name would be for a first or
#'   last dose, this function will work best if this is left as NA. Also, since
#'   we don't know which dose these data were for, you'll see that the output
#'   parameter names do not include the suffixes "_last" or "_dose1".
#' @param PKparameter PK parameter you want to extract from the simulator output
#'   file. To see the full set of possible parameters to extract, enter
#'   \code{view(PKParameterDefinitions)} into the console. Not case sensitive.
#'   If you use "_first" instead of "_dose1", that will also work.
#' @param compoundToExtract For which compound do you want to extract
#'   PK data? Options are: \itemize{\item{"substrate"
#'   (default),} \item{"primary metabolite 1",} \item{"primary metabolite 2",}
#'   \item{"secondary metabolite",} \item{"inhibitor 1" -- this can be an
#'   inducer, inhibitor, activator, or suppresesor, but it's labeled as
#'   "Inhibitor 1" in the simulator,} \item{"inhibitor 2" for the 2nd inhibitor
#'   listed in the simulation,} \item{"inhibitor 1 metabolite" for the primary
#'   metabolite of inhibitor 1}}
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default), "unbound plasma", "blood", "unbound blood",
#'   "peripheral plasma", or "peripheral blood". \strong{NOTE: PK for peripheral
#'   sampling is not as well tested as for other tissues and is only set up for
#'   V21+. Please check your results carefully.}
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#' @param mean_type What kind of means do you want to use for the center point
#'   on the graph? Options are "geometric" (default), "arithmetic", or "median".
#' @param variability_type What variability statistic to you want to use for the
#'   error bars? Options are "90% CI", "SD", "CV", "GCV" (for geometric CV), or
#'   "range".
#' @param lines_for_population_stats optionally include horizontal lines for the
#'   overall simulated population by specifying the desired line color and type.
#'   If left as "none" (default), no lines will be included. If set to, e.g.,
#'   "red solid dotted" or "gray50 dashed dashed" or even "#87A896 solid E2",
#'   then lines will be added to the graph at whatever centeral statistic you
#'   specified for "mean_type" and whatever variability statistic you specified
#'   for "variability_type". The first word must be a legitimate color in R (hex
#'   codes are fine), the second word, separated by a space, must be the line
#'   type you want for the central statistic, and the third word must be the
#'   line type you want for the variability statistic. If you add another space
#'   and then a fourth number, that will set the line width, which will be 0.5
#'   by default.
#' @param y_axis_limits_lin optionally set the Y axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as the default NA, the Y axis
#'   limits for the linear plot will be automatically selected. (Setting up
#'   semi-log plot y axis intervals manually is a bit tricky and is not
#'   currently supported.)
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get all the details from the "Input
#'   Sheet" (e.g., when you ran extractExpDetails you said \code{exp_details =
#'   "Summary and Input"} or \code{exp_details = "all"}), you can save some processing
#'   time by supplying that object here, unquoted. If left as NA, this function
#'   will run \code{extractExpDetails} behind the scenes anyway to figure out
#'   some information about your experimental set up.
#' @param observed_PK a data.frame of observed PK that you would like to
#'   compare. This must include the columns "ObsValue" for the center value for
#'   the observed data and "ObsVariability" for the error bars for the observed
#'   data. We'll assume that observed PK mean type and variability type match
#'   what's supplied here. This will probably be buggy if they don't. UNDER
#'   CONSTRUCTION. LIKELY SUPER BUGGY.
#' @param return_caption TRUE or FALSE (default) for whether to return any
#'   caption text to use with the graph. This works best if you supply something
#'   for the argument \code{existing_exp_details}. If set to TRUE, you'll get as
#'   output a list of the graph, the figure heading, and the figure caption.
#' @param prettify_compound_names TRUE (default), FALSE or a character vector:
#'   This is asking whether to make compound names prettier in the figure
#'   heading and caption. This was designed for simulations where the substrate
#'   and any metabolites, perpetrators, or perpetrator metabolites are among the
#'   standard options for the simulator, and leaving
#'   \code{prettify_compound_names = TRUE} will make the name of those compounds
#'   something more human readable. For example, "SV-Rifampicin-MD" will become
#'   "rifampicin", and "Sim-Midazolam" will become "midazolam". Setting this to
#'   FALSE will leave the compound names as is. For an approach with more
#'   control over what the compound names will look like in legends and Word
#'   output, set each compound to the exact name you  want with a named
#'   character vector where the names are "substrate" and, as applicable,
#'   "inhibitor 1", "primary metabolite 1", etc. and the values are the names
#'   you want, e.g.,
#'   \code{prettify_compound_names = c("inhibitor 1" = "teeswiftavir",
#'   "substrate" = "superstatin")}.
#' @param name_clinical_study optionally specify the name(s) of the clinical
#'   study or studies for any observed data. This only affects the caption of
#'   the graph. For example, specifying \code{name_clinical_study = "101, fed
#'   cohort"} will result in a figure caption that reads in part "clinical study
#'   101, fed cohort". If you have more than one study, that's fine; we'll take
#'   care of stringing them together appropriately. Just list them as a
#'   character vector, e.g., \code{name_clinical_study = c("101",
#'   "102", "103")} will become "clinical studies 101, 102, and 103."
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
#' # None yet
#' 
trial_means_plot <- function(sim_data_file, 
                             PKparameter = "Cmax_dose1", 
                             compoundToExtract = "substrate", 
                             tissue = "plasma", 
                             sheet = NA, 
                             mean_type = "geometric", 
                             variability_type = "90% CI", 
                             lines_for_population_stats = "none", 
                             y_axis_limits_lin = NA, 
                             include_dose_num = FALSE, 
                             existing_exp_details = NA, 
                             observed_PK = NA, 
                             return_caption = FALSE, 
                             prettify_compound_names = TRUE,
                             name_clinical_study = NA){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if(length(PKparameter) > 1){
      warning(wrapn("You have supplied more than 1 PK parameter, and we can only plot one at a time. We'll only plot the 1st one."), 
              call. = FALSE)
      PKparameter <- PKparameter[1]
   }
   
   if(is.na(PKparameter)){
      warning(wrapn("You must supply a PK parameter to plot. We'll plot the default of Cmax for dose 1."), 
              call. = F)
      PKparameter <- "Cmax_dose1"
   }
   
   if(PKparameter %in% c(AllPKParameters$PKparameter, 
                         AllPKParameters$PKparameter_nodosenum) == FALSE){
      warning(wrapn("You must supply a PK parameter to plot that is among the possible options, which you can see by running view(PKParameterDefinitions). We'll plot the default of Cmax for dose 1."), 
              call. = F)
      PKparameter <- "Cmax_dose1"
   }
   
   if(length(sim_data_file) > 1){
      warning(wrapn("You have supplied more than 1 simulation results file, and we can only plot one at a time. We'll only plot the 1st one."), 
              call. = FALSE)
      sim_data_file <- sim_data_file[1]
   }
   
   mean_type <- tolower(mean_type)[1]
   if(mean_type %in% c("arithmetic", "geometric", "median") == FALSE){
      warning(wrapn("The only possibilities for the mean type are 'geometric', 'arithmetic', or 'median', and you have entered something else. We'll use the default of 'geometric'."), 
              call. = FALSE)
      mean_type <- "geometric"
   }
   
   variability_type <- toupper(variability_type)[1]
   variability_type <- ifelse(str_detect(variability_type, "CV"), 
                              "CV", variability_type)
   if(variability_type %in% c("90% CI", "SD", "CV", "RANGE") == FALSE){
      warning(wrapn("The only possible variability types are '90% CI', 'SD', 'CV', or 'range', and you have entered something else. We'll use the default of 90% CI."), 
              call. = FALSE)
      variability_type <- "90% CI"
   }
   
   if(mean_type != "median" & str_detect(PKparameter, "tmax")){
      warning(wrapn(paste("You requested tmax for the PK parameter and then requested", 
                          mean_type, "means. Are you sure you didn't mean to request a mean_type of 'median' and a variability_type of 'range'?")), 
              call. = FALSE)
   }
   
   # Fixing mismatched stats
   if(mean_type == "geometric" & variability_type == "SD"){
      warning(wrapn("You have requested a geometric mean type but then an arithmetic variability type (SD), which does not make for a clear graph. We'll set the variability type to the geometric CV instead."), 
              call. = FALSE)
      variability_type <- "GCV"
   }
   if(mean_type == "geometric" & variability_type == "CV"){
      variability_type <- "GCV"
   }
   
   # If user wanted lines_for_population_stats added, check that they have
   # specified argument correctly and set up the character vector of preferences.
   LineAES <- str_split(lines_for_population_stats, pattern = " ")[[1]]
   if(length(LineAES) < 3 & any(complete.cases(lines_for_population_stats))){
      warning(wrapn("You requested that lines for the overall simulated population statistics be added to the graph, but you've supplied input that doesn't work for `lines_for_population_stats`. We'll set this to `gray solid dashed` for now, but please check the help file to get what you want."), 
              call. = FALSE)
      LineAES <- c("gray", "solid", "dashed", "0.5")
   }
   # This doesn't check that they've specified legit colors or linetypes, but
   # I'm hoping that ggplot2 errors will cover that.
   
   # Adding a linewidth to LineAES if user didn't specify one.
   if(length(LineAES) < 4){
      LineAES[4] <- "0.5"
   }
   
   
   # Main body of function -----------------------------------------------------
   
   PKdata <- extractPK(sim_data_file = sim_data_file, 
                       PKparameters = PKparameter, 
                       tissue = tissue, 
                       compoundToExtract = compoundToExtract, 
                       existing_exp_details = existing_exp_details, 
                       sheet = sheet, 
                       returnAggregateOrIndiv = "individual", 
                       returnExpDetails = TRUE)
   
   PK_long <- PKdata$individual %>% 
      pivot_longer(cols = -c(Individual, Trial), 
                   names_to = "PKparameter", 
                   values_to = "Value") %>% 
      filter(PKparameter == {{PKparameter}}) %>% 
      group_by(Trial) %>% 
      summarize(Mean = mean(Value, na.rm = T), 
                SD = sd(Value, na.rm = T), 
                Min = min(Value, na.rm = T), 
                Max = max(Value, na.rm = T), 
                Geomean = gm_mean(Value, na.rm = T), 
                GCV = gm_CV(Value, na.rm = T), 
                CI90_low = switch(mean_type, 
                                  "arithmetic" = confInt(Value, CI = 0.9, distribution_type = "t")[[1]], 
                                  "geometric" = gm_conf(Value, CI = 0.9, distribution_type = "t")[[1]], 
                                  "median" = NA), 
                CI90_high = switch(mean_type, 
                                   "arithmetic" = confInt(Value, CI = 0.9, distribution_type = "t")[[2]], 
                                   "geometric" = gm_conf(Value, CI = 0.9, distribution_type = "t")[[2]], 
                                   "median" = NA), 
                Median = median(Value, na.rm = T)) %>% 
      mutate(SorO = "simulated")
   
   # Getting observed PK for comparison
   if("File" %in% names(observed_PK)){
      observed_PK <- observed_PK %>% 
         filter(basename(File) == basename(sim_data_file))
   }
   
   if("CompoundID" %in% names(observed_PK)){
      observed_PK <- observed_PK %>% 
         filter(CompoundID == {{compoundToExtract}})
   }
   
   if("Tissue" %in% names(observed_PK)){
      observed_PK <- observed_PK %>% 
         filter(Tissue == {{tissue}})
   }
   
   if("logical" %in% class(observed_PK) == FALSE){
      suppressWarnings(
         observed_PK <- observed_PK %>%
            filter(PKparameter == {{PKparameter}}) %>% 
            separate(ObsVariability, into = c("Lower", "Upper"), 
                     sep = "( )?to( )?|( )?-( )?", remove = FALSE) %>% 
            rename(Center = ObsValue) %>% 
            mutate(SorO = "observed", 
                   Trial = "observed", 
                   across(.cols = c("Lower", "Upper"), .fns = as.numeric), 
                   SD = ifelse(is.na(Upper) & mean_type == "arithmetic" & 
                                  variability_type == "SD", 
                               Lower, NA), 
                   CV = ifelse(is.na(Upper) & mean_type == "arithmetic" & 
                                  variability_type == "CV", 
                               Lower, NA), 
                   GCV = ifelse(is.na(Upper) & mean_type == "geometric" & 
                                   variability_type == "GCV", 
                                Lower, NA), 
                   
                   Lower = case_when(is.na(Upper) & 
                                        mean_type == "arithmetic" & 
                                        variability_type == "SD" ~ Center - SD, 
                                     
                                     is.na(Upper) & mean_type == "arithmetic" & 
                                        variability_type == "CV" ~ Center - Center * CV, 
                                     
                                     is.na(Upper) & mean_type == "geometric" & 
                                        variability_type == "GCV" ~ Center - Center * GCV, 
                                     
                                     .default = Lower), 
                   
                   Upper = case_when(is.na(Upper) & 
                                        mean_type == "arithmetic" & 
                                        variability_type == "SD" ~ Center + SD, 
                                     
                                     is.na(Upper) & mean_type == "arithmetic" & 
                                        variability_type == "CV" ~ Center + Center * CV, 
                                     
                                     is.na(Upper) & mean_type == "geometric" & 
                                        variability_type == "GCV" ~ Center + Center * GCV, 
                                     
                                     .default = Upper))
      )
   } 
   
   PK_long <- PK_long %>% 
      mutate(
         Center = case_when(mean_type == "arithmetic" ~ Mean, 
                            mean_type == "geometric" ~ Geomean, 
                            mean_type == "median" ~ Median), 
         
         Lower = case_when(
            mean_type == "arithmetic" & 
               variability_type %in% c("SD", "CV") ~ Mean - SD, 
            
            mean_type == "geometric" & 
               variability_type == "GCV" ~ Geomean  - Geomean * GCV, 
            
            variability_type == "90% CI" ~ CI90_low, 
            
            variability_type == "RANGE" ~ Min), 
         
         Upper = case_when(
            mean_type == "arithmetic" & 
               variability_type %in% c("SD", "CV") ~ Mean + SD, 
            
            mean_type == "geometric" & 
               variability_type == "GCV" ~ Geomean  + Geomean * GCV, 
            
            variability_type == "90% CI" ~ CI90_high, 
            
            variability_type == "RANGE" ~ Max))
   
   
   if("logical" %in% class(observed_PK) == FALSE){
      PK_long <- PK_long %>% 
         bind_rows(observed_PK) %>% 
         mutate(Trial = factor(Trial, 
                               levels = c(as.character(1:length(
                                  unique(PKdata$individual$Trial))), 
                                  "observed")))
   } else {
      PK_long <- PK_long %>% 
         mutate(Trial = factor(Trial, 
                               levels = c(as.character(1:length(
                                  unique(PKdata$individual$Trial))))))
   }
   
   
   # Making graph -------------------------------------------------------------
   
   G <- ggplot(PK_long, aes(x = Trial, color = SorO, 
                            y = Center, ymin = Lower, ymax = Upper))
   
   if(lines_for_population_stats != "none"){
      
      AggStats <- PKdata$aggregate[[1]]
      names(AggStats) <- renameStats(names(PKdata$aggregate[[1]]))
      
      # Simulator does not output arithmetic CIs, so need to add those here.
      CI90_arith <- confInt(PKdata$individual %>% 
                               pull({{PKparameter}}),
                            CI = 0.9, 
                            distribution_type = "t")
      
      AggStats <- c(AggStats, CI90_arith_low = CI90_arith[[1]], 
                    CI90_arith_high = CI90_arith[[2]])
      
      G <- G + 
         geom_hline(yintercept = 
                       case_match(mean_type, 
                                  "arithmetic" ~ AggStats["mean"], 
                                  "geometric" ~ AggStats["geomean"], 
                                  "median" ~ AggStats["median"]), 
                    color = LineAES[1], 
                    linetype = LineAES[2], 
                    linewidth = as.numeric(LineAES[4])) +
         geom_hline(yintercept = 
                       case_when(
                          mean_type == "arithmetic" & 
                          variability_type ==  "SD" ~ AggStats["mean"] + AggStats["SD"], 
                          
                          mean_type == "arithmetic" & 
                             variability_type ==  "CV" ~ AggStats["mean"] + AggStats["SD"], 
                          
                          mean_type == "arithmetic" & 
                             variability_type ==  "90% CI" ~ AggStats["CI90_arith_high"], 
                          
                          mean_type == "geometric" & 
                             variability_type ==  "GCV" ~ AggStats["geomean"] + 
                             AggStats["geomean"] * AggStats["GCV"], 
                          
                          mean_type == "geometric" & 
                             variability_type ==  "90% CI" ~ AggStats["CI90_high"], 
                          
                          variability_type == "RANGE" ~ AggStats["max"]), 
                    
                    color = LineAES[1], 
                    linetype = LineAES[3], 
                    linewidth = as.numeric(LineAES[4])) +
         geom_hline(yintercept = 
                       case_when(
                          mean_type == "arithmetic" & 
                             variability_type ==  "SD" ~ AggStats["mean"] - AggStats["SD"], 
                          
                          mean_type == "arithmetic" & 
                             variability_type ==  "CV" ~ AggStats["mean"] - AggStats["SD"], 
                          
                          mean_type == "arithmetic" & 
                             variability_type ==  "90% CI" ~ AggStats["CI90_arith_low"], 
                          
                          mean_type == "geometric" & 
                             variability_type ==  "GCV" ~ AggStats["geomean"] - 
                             AggStats["geomean"] * AggStats["GCV"], 
                          
                          mean_type == "geometric" & 
                             variability_type ==  "90% CI" ~ AggStats["CI90_low"], 
                          
                          variability_type == "RANGE" ~ AggStats["min"]), 
                    
                    color = LineAES[1], 
                    linetype = LineAES[3], 
                    linewidth = as.numeric(LineAES[4]))
   }
   
   G <- G +
      geom_errorbar(width = 0.5) +
      geom_point() +
      scale_color_brewer(palette = "Set1") +
      theme_consultancy() +
      ylab(PKexpressions[[sub("_last|_dose1", "", PKparameter)]]) +
      theme(legend.position = "none")
   
   if(any(complete.cases(y_axis_limits_lin))){
      G <- G +
         scale_y_continuous(limits = y_axis_limits_lin)
   }
   
   Out <- list("graph" = G)
   
   if(return_caption){
      
      Legos <- make_text_legos(sim_data_file = basename(sim_data_file), 
                               existing_exp_details = PKdata[["ExpDetails"]], 
                               prettify_compound_names = prettify_compound_names)
      
      PKparameter_rmd <- case_match(sub("_dose1|_last", "", PKparameter), 
                                    "Cmax" ~ "C~max~", 
                                    "tmax" ~ "t~max~", 
                                    "AUCtau" ~ "AUC~tau~", 
                                    "AUCinf" ~ "AUC~inf~", 
                                    "AUCt" ~ "AUC~t~", 
                                    "CLt" ~ "CL/F", 
                                    "CLtau" ~ "CL/F", 
                                    "CLinf" ~ "CL/F")
      
      Caption <- 
         paste0("Figure shows ", 
                case_match(mean_type,
                           "arithmetic" ~ "arithmetic mean", 
                           "geometric" ~ "geometric mean", 
                           "median" ~ "median"), 
                " (point) and ", 
                case_match(variability_type, 
                           "SD" ~ "standard deviation", 
                           "CV" ~ "standard deviation", 
                           "GCV" ~ "standard deviation", 
                           "90% CI" ~ "90% confidence interval", 
                           "RANGE" ~ "range"), 
                " (error bars) for ", 
                PKparameter_rmd, 
                " for ",
                PKdata[["ExpDetails"]]$NumTrials, 
                " trials of ", 
                PKdata[["ExpDetails"]]$NumSubjTrial, 
                " subjects each (", 
                PKdata[["ExpDetails"]]$PercFemale * 100, "% female, ages ", 
                PKdata[["ExpDetails"]]$Age_min, " to ", 
                PKdata[["ExpDetails"]]$Age_max, 
                ") following ", 
                ifelse(
                   AllCompounds$DDIrole[AllCompounds$CompoundID == compoundToExtract] == "victim", 
                   Legos$DosingText_sub_lower, 
                   Legos$DosingText_inhib_lower), 
                ifelse(str_detect(PKparameter, "withInhib|ratio"), 
                       paste0(" following ", Legos$DosingText_inhib_lower), ""), 
                ifelse("logical" %in% class(observed_PK), 
                       "", 
                       paste0(". Observed data from clinical study ", 
                              ifelse(is.na(name_clinical_study), 
                                     "***XXX***", name_clinical_study))), 
                ". ", 
                ifelse(lines_for_population_stats == "none", 
                       "", 
                       paste0("Horizontal lines indicated the ", 
                              case_match(mean_type,
                                         "arithmetic" ~ "arithmetic mean", 
                                         "geometric" ~ "geometric mean", 
                                         "median" ~ "median"), 
                              " and ", 
                              case_match(variability_type, 
                                         "SD" ~ "standard deviation", 
                                         "CV" ~ "standard deviation", 
                                         "GCV" ~ "standard deviation", 
                                         "90% CI" ~ "90% confidence interval", 
                                         "RANGE" ~ "range"), 
                              " for the simulated population. ")), 
                "Source simulated data: ", sim_data_file, ".")
      
      Heading <- paste0("Simulated ", 
                        ifelse("logical" %in% class(observed_PK), 
                               "", "and observed "), 
                        "values for ", PKparameter_rmd,
                        " for ", 
                        PKdata[["ExpDetails"]] %>% 
                           filter(File == sim_data_file) %>% 
                           select(AllCompounds$DetailNames[
                              AllCompounds$CompoundID == compoundToExtract]), 
                        " comparing variability across trials.")
      
      Out[["figure_heading"]] <- Heading
      Out[["figure_caption"]]  <-  Caption
   } 
   
   if(length(Out) == 1){
      Out <- Out[[1]]
   }
   
   return(Out)
   
}



