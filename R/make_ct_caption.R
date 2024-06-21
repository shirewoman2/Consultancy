#' INTERNAL - make caption for conc-time plots
#'
#' @param ct_dataframe ct_dataframe
#' @param single_or_multiple whether this is for a ct_plot graph (only 1
#'   compound, tissue, file, etc.) or a graph with multiple profiles (multipel
#'   compounds, tissues, files, etc.). Options are "single" (default) or
#'   "multiple".
#' @param existing_exp_details OR supply: a data.frame with the following
#'   columns: File, Population, Substrate, Inhibitor1, Inhibitor1Metabolite,
#'   Inhibitor2, ... If left as NA, we'll note some generic parameters and make
#'   them bold to note that they should be changed.
#' @param mean_type "arithmetic" (default) or "geometric" 
#' @param linear_or_log "linear" (default), "both", "both horizontal", "both
#'   vertical", "log", "semi-log", "horizontal and vertical"
#' @param tissue default is plasma
#' @param compoundID default is substrate
#' @param prettify_compound_names default is TRUE
#' @param figure_type default is percentiles
#' @param hline_position hline_position
#' @param hline_style hline_style
#' @param vline_position vline_position
#' @param vline_style vline_style
#'
#' @return a string of text for making a caption
#'
#' @examples
#' # none 
make_ct_caption <- function(ct_dataframe, 
                            single_or_multiple = "single", 
                            existing_exp_details = NA,
                            mean_type = "arithmetic", 
                            linear_or_log = "linear", 
                            tissue = "plasma", 
                            compoundID = "substrate",
                            figure_type = "percentiles", 
                            prettify_compound_names = TRUE, 
                            hline_position = NA, 
                            hline_style = "red dotted", 
                            vline_position = NA, 
                            vline_style = "red dotted"){
   
   
   if(single_or_multiple == "single"){
      
      # Getting general info and setting up heading ------------------------------
      
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          unique(ct_dataframe$File), 
                                          "include")
      
      Text1 <- switch(linear_or_log, 
                      "both" = "Linear (A) and log-linear (B)",
                      "both horizontal" ="Linear (A) and log-linear (B)",
                      "both vertical" = "Linear (A) and log-linear (B)",
                      "linear" = "Linear", 
                      "log" = "Log-linear",
                      "semi-log" = "Log-linear", 
                      "horizontal and vertical" = "Linear (A) and log-linear (B)")
      
      # Note that this defaults to "none" if nothing was supplied for
      # existing_exp_details.
      MyPerpetrator <- determine_myperpetrator(existing_exp_details, 
                                               prettify_compound_names = prettify_compound_names)
      
      MyCompound <- case_when(
         "logical" %in% class(prettify_compound_names) &&
            prettify_compound_names == TRUE ~ 
            prettify_compound_name(unique(ct_dataframe$Compound)), 
         
         "logical" %in% class(prettify_compound_names) &&
            prettify_compound_names == FALSE ~ 
            unique(ct_dataframe$Compound), 
         
         "character" %in% class(prettify_compound_names) & 
            compoundID %in% names(prettify_compound_names) ~ 
            prettify_compound_names[compoundID], 
         
         TRUE ~ unique(ct_dataframe$Compound)
      )
      
      if("logical" %in% class(existing_exp_details) ||
         unique(ct_dataframe$File) %in% existing_exp_details$MainDetails$File == FALSE){
         
         if("logical" %in% class(existing_exp_details) == FALSE && 
            unique(ct_dataframe$File) %in% existing_exp_details$MainDetails$File == FALSE){
            WarningText <- "**WARNING:** The object that you supplied for the argument `existing_exp_details` does not contain the file in your concentration-time data, so we can't use it."
         } else {
            WarningText <- NA
         }
         
         Pop <- "**healthy subjects / patients with [complaint]**"
         
         DosingText_sub_lower <- "a **single oral** dose of **XXX** mg **Drug XXX**"
         DosingText_inhib_lower <- "**multiple oral** doses of **XXX** mg **Perpetrator XXX**"
         DoseDay_ordinal <- "**XXXth**"
         NumDaysInhib <- "**XXX**"
         Dose_inhib <- "**XXX**"
         Units_dose_inhib <- "**mg**"
         DoseFreq_inhib <- "**QD/BID**"
         
         N_trials <- "**XXX**"
         N_subjpertrial <- "**XXX**"
         N_indiv <- "**XXX**"
         
      } else {
         
         WarningText <- NA
         
         TextPieces <- make_text_legos(sim_data_file = unique(ct_dataframe$File),
                                       existing_exp_details = existing_exp_details, 
                                       prettify_compound_names = prettify_compound_names)
         
         Pop <- TextPieces$Pop
         DosingText_sub_lower <- TextPieces$DosingText_sub_lower
         DosingText_inhib_lower <- TextPieces$DosingText_inhib_lower
         DoseDay_ordinal <- TextPieces$DoseDay_ordinal
         NumDaysInhib <- TextPieces$NumDaysInhib
         Dose_inhib <- existing_exp_details$MainDetails$Dose_inhib
         Units_dose_inhib <- existing_exp_details$MainDetails$Units_dose_inhib
         DoseFreq_inhib <- TextPieces$DoseFreq_inhib
         N_subjpertrial <- existing_exp_details$MainDetails$NumSubjTrial
         N_trials <- existing_exp_details$MainDetails$NumTrials
         N_indiv <- existing_exp_details$MainDetails$NumSubjTrial * existing_exp_details$MainDetails$NumTrials
         MyPerpetrator <- TextPieces$MyPerpetrator
         
      }
      
      DDI <- MyPerpetrator != "none"
      DDI <- ifelse(compoundID %in% c("inhibitor 1", "inhibitor 2", 
                                      "inhibitor 1 metabolite"), 
                    FALSE, DDI)
      
      DDItext1 <- ifelse(DDI, 
                         paste0("co-administered with ", MyPerpetrator, " "), 
                         "")
      
      Heading <- paste0(Text1, " simulated ", tissue, 
                        " concentration-time profiles of ", 
                        MyCompound, " ", DDItext1, "in ", 
                        Pop)
      
      
      # Setting up caption --------------------------------------------------------
      
      ObsIncluded <- nrow(ct_dataframe %>% filter(Simulated == FALSE)) > 0
      ObsShowsMean <- all(c(exists("obs_dataframe", inherits = FALSE), 
                            exists("check", inherits = FALSE))) &&
         nrow(obs_dataframe) > 0 && all(check$N == 1)
      
      if(DDI){
         
         if("logical" %in% class(existing_exp_details) == FALSE && 
            complete.cases(existing_exp_details$MainDetails$Inhibitor2)){
            warning("The figure heading and caption text have not been set up for when there are two perpetrators in a DDI simulation. Please check and adjust the text carefully.\n", 
                    call. = FALSE)
         }
         
         CapText1 <- paste0(
            "Depicted are ", tissue,
            " concentration-time profiles of ", 
            MyCompound, " following ", 
            case_when(compoundID %in% c("substrate", "primary metabolite 1", 
                                        "primary metabolite 2",
                                        "secondary metabolite") ~ DosingText_sub_lower, 
                      compoundID %in% c("inhibitor 1", 
                                        "inhibitor 1 metabolite") ~ DosingText_inhib_lower, 
                      TRUE ~ "**multiple oral doses of XXX**"), 
            " in the absence of ", MyPerpetrator, 
            " (solid line) and on the ", 
            DoseDay_ordinal, " day of ",
            NumDaysInhib, " days of dosing of ",
            MyPerpetrator, " ", Dose_inhib, " ",
            Units_dose_inhib, " ",
            DoseFreq_inhib, " (dashed line).")
         
      } else {
         
         CapText1 <- paste0(
            "Depicted are ", 
            ifelse(ObsIncluded, 
                   paste0("simulated (lines) and observed (circles;",  
                          ifelse(ObsShowsMean, 
                                 paste0(mean_type, 
                                        " mean of n = ", N_subjpertrial, 
                                        " individuals; "), 
                                 paste0(" n = ", N_subjpertrial, 
                                        " individuals; ")),
                          "Clinical Study **XXX**) "),
                   "simulated "), 
            tissue, " concentration-time profiles of ", 
            MyCompound, ".") 
         
      }
      
      CapText2 <- switch(
         figure_type, 
         "trial means" = 
            paste0("The grey lines represent ",
                   mean_type, 
                   " mean values of simulated individual trials and the black lines portray the ",
                   mean_type, 
                   " mean data of the simulated population (n = ", N_trials, " trials of ", N_subjpertrial, " subjects per trial)."), 
         "percentiles" = 
            paste0("The grey lines represent the 5^th^ and 95^th^ percentiles and the solid black line the ",
                   mean_type, 
                   " mean data for the simulated population (n = ", N_indiv, ")."), 
         "percentile ribbon" = 
            paste0("The shaded regions represent the 5^th^ to the 95^th^ percentiles and the solid black line the ",
                   mean_type, 
                   " mean data for the simulated population (n = ", N_indiv, ")."), 
         "means only" = 
            paste0("The solid black line represents the ", 
                   mean_type, 
                   " mean data for the simulated population (n = ", N_indiv, ")."), 
         "Freddy" = 
            paste0("The grey lines represent the ", 
                   mean_type, 
                   " mean values of simulated individual trials and the black lines portray the ",
                   mean_type, 
                   " mean data of the simulated population (n = ", N_indiv, "). The dashed lines represent the 5^th^ and 95^th^ percentiles."))
      
      if(any(complete.cases(hline_position))){
         CapText2 <- paste0(CapText2, "* ",
                            paste("*The", HLineAES[1], HLineAES[2], "horizontal",
                                  ifelse(length(hline_position) > 1, 
                                         "lines indicate*", "line indicates*"), 
                                  "***XXX.*** "))
      }
      
      if(any(complete.cases(vline_position))){
         CapText2 <- paste0(CapText2, ifelse(any(complete.cases(hline_position)), 
                                             " ", "* "),
                            paste("*The", VLineAES[1], VLineAES[2], "vertical",
                                  ifelse(length(hline_position) > 1, 
                                         "lines indicate*", "line indicates*"), 
                                  "***XXX.*** "))
      }
      
      if(any(complete.cases(hline_position)) |
         any(complete.cases(vline_position))){
         CapText2 <- paste0(CapText2, "*")
      } else {
         CapText2 <- paste0(CapText2, " ")
      }
      
      Caption <- paste0(CapText1, " ", CapText2, 
                        "Source simulated data: ", 
                        basename(unique(ct_dataframe$File[ct_dataframe$Simulated == TRUE])), 
                        ".")
      
      return(list("heading" = Heading, 
                  "caption" = Caption, 
                  "WarningText" = WarningText))
      
   }
}

