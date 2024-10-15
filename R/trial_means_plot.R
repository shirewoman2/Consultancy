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
#'   on the graph? Options are "arithmetic" or "geometric" (default).
#' @param variability_type What variability statistic to you want to use for the
#'   error bars? Options are "90% CI", "SD", "CV", "GCV" (for geometric CV), or
#'   "range".
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
                             y_axis_limits_lin = NA, 
                             include_dose_num = FALSE, 
                             existing_exp_details = NA, 
                             observed_PK = NA, 
                             return_caption = FALSE, 
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
   if(mean_type %in% c("arithmetic", "geometric") == FALSE){
      warning(wrapn("The only possibilities for the mean type are 'arithmetic' or 'geometric', and you have entered something else. We'll use the default of 'geometric'."), 
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
   
   # Fixing mismatched stats
   if(mean_type == "geometric" & variability_type == "SD"){
      warning(wrapn("You have requested a geometric mean type but then an arithmetic variability type (SD), which does not make for a clear graph. We'll set the variability type to the geometric CV instead."), 
              call. = FALSE)
      variability_type <- "GCV"
   }
   if(mean_type == "arithmetic" & variability_type == "CV"){
      variability_type <- "GCV"
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
                                  "geometric" = gm_conf(Value, CI = 0.9, distribution_type = "t")[[1]]), 
                CI90_high = switch(mean_type, 
                                   "arithmetic" = confInt(Value, CI = 0.9, distribution_type = "t")[[2]], 
                                   "geometric" = gm_conf(Value, CI = 0.9, distribution_type = "t")[[2]]), 
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
      mutate(Center = case_when(!str_detect({{PKparameter}}, "tmax") &
                                   mean_type == "arithmetic" ~ Mean, 
                                
                                !str_detect({{PKparameter}}, "tmax") &
                                   mean_type == "geometric" ~ Geomean, 
                                
                                str_detect({{PKparameter}}, "tmax") ~ Median), 
             
             Lower = case_when(!str_detect({{PKparameter}}, "tmax") &
                                  mean_type == "arithmetic" & 
                                  variability_type == "SD" ~ Mean - SD, 
                               
                               !str_detect({{PKparameter}}, "tmax") &
                                  mean_type == "geometric" & 
                                  variability_type == "GCV" ~ GCV, 
                               
                               str_detect({{PKparameter}}, "tmax") ~ Min), 
             
             Upper = case_when(!str_detect({{PKparameter}}, "tmax") &
                                  mean_type == "arithmetic" ~ Mean + SD, 
                               
                               !str_detect({{PKparameter}}, "tmax") &
                                  mean_type == "geometric" ~ CI90_high, 
                               
                               str_detect({{PKparameter}}, "tmax") ~ Max))
   
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
   
   G <- 
      ggplot(PK_long,
             aes(x = Trial, color = SorO, 
                 y = Center, ymin = Lower, ymax = Upper)) +
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
                               prettify_compound_names = T)
      
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
         paste0("Figure shows ", mean_type, " mean (point) and ", 
                case_match(variability_type, 
                           "SD" ~ "standard deviation", 
                           "CV" ~ "standard deviation", 
                           "GCV" ~ "standard deviation", 
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
                ". Observed data from clinical study ", 
                ifelse(is.na(name_clinical_study), 
                       "***XXX***", name_clinical_study), ".")
      
      Heading <- paste0("Simulated ", 
                        ifelse("logical" %in% class(observed_PK), 
                               "", "and observed "), 
                        "values for ", PKparameter_rmd, " comparing variability across trials.")
      
      Out[["figure_heading"]] <- Heading
      Out[["figure_caption"]]  <-  Caption
   } 
   
   if(length(Out) == 1){
      Out <- Out[[1]]
   }
   
   return(Out)
   
}



