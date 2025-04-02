#' INTERNAL - make caption for conc-time plots
#'
#' @param ct_dataframe ct_dataframe
#' @param plot_type options are "concentration-time", "enzyme-abundance",
#'   "dissolution-profile", "release-profile". May change these to be prettier.
#' @param single_or_multiple_profiles whether this is for a ct_plot graph (only
#'   1 compound, tissue, file, etc.) or a graph with multiple profiles (multiple
#'   compounds, tissues, files, etc.). Options are "single" (default) or
#'   "multiple".
#' @param existing_exp_details OR supply: a data.frame with the following
#'   columns: File, Population, Substrate, Inhibitor1, Inhibitor1Metabolite,
#'   Inhibitor2, ... If left as NA, we'll note some generic parameters and make
#'   them bold to note that they should be changed.
#' @param mean_type "arithmetic" (default) or "geometric"
#' @param linear_or_log "linear" (default), "both", "both horizontal", "both
#'   vertical", "log", "semi-log", "horizontal and vertical"
#' @param prettify_compound_names default is TRUE
#' @param figure_type default is percentiles
#' @param hline_position hline_position
#' @param hline_style hline_style
#' @param vline_position vline_position
#' @param vline_style vline_style
#' @param name_clinical_study optionally specify the name(s) of the clinical
#'   study or studies for any observed data. This only affects the caption of
#'   the graph. For example, specifying \code{name_clinical_study = "101, fed
#'   cohort"} will result in a figure caption that reads in part "clinical study
#'   101, fed cohort". If you have more than one study, that's fine; we'll take
#'   care of stringing them together appropriately. Just list them as a
#'   character vector, e.g., \code{name_clinical_study = c("101",
#'   "102", "103")} will become "clinical studies 101, 102, and 103." 
#'
#' @return a string of text for making a caption
#' 
make_ct_caption <- function(ct_dataframe, 
                            single_or_multiple_profiles = "single", 
                            plot_type = "concentration-time", 
                            existing_exp_details = NA,
                            mean_type = "arithmetic", 
                            linear_or_log = "linear", 
                            figure_type = "percentiles", 
                            prettify_compound_names = TRUE, 
                            name_clinical_study = NA, 
                            hline_position = NA, 
                            hline_style = "red dotted", 
                            vline_position = NA, 
                            vline_style = "red dotted"){
   
   # Determining appropriate names to use for compounds -----------------------
   
   # For a "compound summary" figure type, you could conceivably have more than
   # one simulation file present for ct_plot, which would mess things up here.
   # To avoid that, this will assume that the figure caption info should come
   # from the simulated file only. If this is only observed data, though, then
   # set File to the only observed file.
   SimFile <- sort(unique(ct_dataframe$File[ct_dataframe$Simulated == TRUE]))
   
   if(length(SimFile) == 1){
      ct_dataframe$File <- SimFile
   } else {
      ObsFile <- sort(unique(ct_dataframe$File[ct_dataframe$Simulated == FALSE]))
      
      if(length(ObsFile) == 1){
         ct_dataframe$File <- ObsFile
      }
   }
   
   Tissue <- str_comma(as.character(sort(unique(ct_dataframe$Tissue))),
                       conjunction = "or")
   CompoundID <- as.character(sort(unique(ct_dataframe$CompoundID)))
   
   InhibPresent <- any(ct_dataframe$Inhibitor != "none")
   
   CmpdRoles <- case_when(plot_type %in% c("enzyme-abundance", "fm") &
                             InhibPresent == TRUE ~ "perpetrator", 
                          
                          plot_type %in% c("enzyme-abundance", "fm") &
                             InhibPresent == FALSE ~ "victim", 
                          
                          .default = str_c(sort(unique(AllRegCompounds$DDIrole[
                             AllRegCompounds$CompoundID %in% CompoundID])), collapse = "-"))
   
   # Adding one more plot_type to account for ADAM model data where it's not
   # really "concentration" but "amount".
   if(plot_type == "concentration-time" & 
      all(unique(ct_dataframe$Conc_units) %in% c("µM", "nM", "mg/L", "mg/mL",
                                                 "µg/L", "µg/mL", "ng/L",
                                                 "ng/mL") == FALSE)){
      plot_type <- "mass-time"
   }
   
   if(plot_type  %in% c("enzyme-abundance", "fm")){
      MyCompound <- str_comma(sort(unique(ct_dataframe$Enzyme)))
   } else {
      MyCompound <- case_when(
         "logical" %in% class(prettify_compound_names) &&
            prettify_compound_names == TRUE ~ 
            str_comma(sort(unique(prettify_compound_name(ct_dataframe$Compound)))), 
         
         "logical" %in% class(prettify_compound_names) &&
            prettify_compound_names == FALSE ~ 
            str_comma(sort(unique(ct_dataframe$Compound))), 
         
         "character" %in% class(prettify_compound_names) & 
            any(CompoundID %in% names(prettify_compound_names)) ~ 
            str_comma(sort(unique(prettify_compound_names[CompoundID]))), 
         
         .default = str_comma(sort(unique(ct_dataframe$Compound)))
      )
   }
   
   # Harmonizing and checking existing_exp_details -------------------------------
   
   # No or incorrect details supplied 
   if("logical" %in% class(existing_exp_details) ||
      all(unique(ct_dataframe$File) %in% existing_exp_details$MainDetails$File) == FALSE){
      
      if("logical" %in% class(existing_exp_details) == FALSE && 
         all(unique(ct_dataframe$File) %in%
             existing_exp_details$MainDetails$File == FALSE)){
         warning(wrapn("The object that you supplied for the argument `existing_exp_details` does not contain the file(s) in your concentration-time data, so we can't use it."), 
                 call. = FALSE)
         existing_exp_details <- NA
      } 
      
      # determine_myperpetrator defaults to "none" if nothing was supplied for
      # existing_exp_details, so that's why this is "none" automatically.
      # Checking for whatever inhibitor was present in the data.
      if("Inhibitor" %in% names(ct_dataframe) &&
         any(ct_dataframe$Inhibitor != "none")){
         
         AllPerpsInCT <- unique(as.character(ct_dataframe$Inhibitor[
            ct_dataframe$Inhibitor != "none"]))
         MyPerpetrator <- ifelse(length(AllPerpsInCT) > 1, 
                                 str_comma(sort(AllPerpsInCT), conjunction = "or"), 
                                 AllPerpsInCT)
      } else {
         MyPerpetrator <- "none"
      }
      
      DetailsAvailable <- FALSE
      
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
      # Details available 
      
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          unique(ct_dataframe$File), 
                                          "include")
      
      DetailsAvailable <- TRUE
      
      MyPerpetrator <- determine_myperpetrator(existing_exp_details, 
                                               prettify_compound_names = prettify_compound_names)
      
      TextPieces <- unique(ct_dataframe$File) %>% 
         set_names() %>% 
         map(.f = \(x) make_text_legos(sim_data_file = x, 
                                       existing_exp_details = existing_exp_details, 
                                       prettify_compound_names = prettify_compound_names))
      
      Pop <- map(TextPieces, "Pop") %>% unique() %>% unlist() %>% 
         ifelse(length(.) > 1, str_comma(., conjunction = "or"), .)
      
      DosingText_sub_lower <- map(TextPieces, "DosingText_sub_lower") %>% 
         unique() %>% unlist() %>% 
         ifelse(length(.) > 1, str_comma(., conjunction = "or"), .)
      
      DosingText_inhib_lower <- map(TextPieces, "DosingText_inhib_lower") %>% 
         unique() %>% unlist() %>% 
         ifelse(length(.) > 1, str_comma(., conjunction = "or"), .)
      
      DoseDay_ordinal <- map(TextPieces, "DoseDay_ordinal") %>% 
         unique() %>% unlist() %>% 
         ifelse(length(.) > 1, str_comma(., conjunction = "or"), .)
      
      NumDaysInhib <- map(TextPieces, "NumDaysInhib") %>% unique() %>% unlist() %>% 
         ifelse(length(.) > 1, str_comma(., conjunction = "or"), .)
      
      Dose_inhib <- 
         ifelse("Dose_inhib" %in% 
                   names(existing_exp_details$MainDetails), 
                existing_exp_details$MainDetails$Dose_inhib %>% 
                   ifelse(length(.) > 1, str_comma(., conjunction = "or"), .), 
                NA)
      
      Units_dose_inhib <- 
         ifelse("Units_dose_inhib" %in% 
                   names(existing_exp_details$MainDetails), 
                existing_exp_details$MainDetails$Units_dose_inhib %>% 
                   ifelse(length(.) > 1, str_comma(., conjunction = "or"), .), 
                NA)
      
      DoseFreq_inhib <- map(TextPieces, "DoseFreq_inhib") %>% 
         unique() %>% unlist() %>% 
         ifelse(length(.) > 1, str_comma(., conjunction = "or"), .)
      
      N_subjpertrial <- as.numeric(existing_exp_details$MainDetails$NumSubjTrial)
      N_trials <- as.numeric(existing_exp_details$MainDetails$NumTrials)
      N_indiv <- N_subjpertrial * N_trials
      
      N_subjpertrial <- N_subjpertrial %>% unique() %>% 
         ifelse(length(.) > 1, str_comma(., conjunction = "or"), .)
      N_trials <- N_trials %>% unique() %>% 
         ifelse(length(.) > 1, str_comma(., conjunction = "or"), .)
      N_indiv <- N_indiv %>% unique() %>% 
         ifelse(length(.) > 1, str_comma(., conjunction = "or"), .)
      
      MyPerpetrator <- map(TextPieces, "MyPerpetrator") %>% unique() %>% unlist() %>% 
         ifelse(length(.) > 1, str_comma(., conjunction = "or"), .)
      
   }
   
   # Setting up text for clinical study or studies ----------------------------
   
   if(length(name_clinical_study) > 1){
      MyClinStudies <- str_comma(name_clinical_study)
   } else if(complete.cases(name_clinical_study)){
      MyClinStudies <- paste("clinical study", name_clinical_study)
   } else {
      MyClinStudies <- "clinical study **XXX**"
   }
   
   
   # single profile -----------------------------------------------------------
   
   if(single_or_multiple_profiles == "single"){
      
      ## Getting general info and setting up heading ------------------------------
      
      Text1a <- switch(linear_or_log, 
                       "both" = "Linear (A) and log-linear (B)",
                       "both horizontal" ="Linear (A) and log-linear (B)",
                       "both vertical" = "Linear (A) and log-linear (B)",
                       "linear" = "Linear", 
                       "log" = "Log-linear",
                       "semi-log" = "Log-linear", 
                       "horizontal and vertical" = "Linear (A) and log-linear (B)")
      
      Text1 <- case_when(plot_type == "fm" ~ "", 
                         .default = paste0(Text1a, " simulated ", Tissue))
      
      DDI <- MyPerpetrator != "none"
      # If the plot is of a perpetrator, we actually want DDI to be FALSE b/c we
      # want to just use the regular caption info. Still checking this, though.
      DDI <- ifelse(all(CompoundID %in% c("inhibitor 1", "inhibitor 2", 
                                          "inhibitor 1 metabolite")), 
                    FALSE, DDI)
      
      DDItext1 <- ifelse(DDI, 
                         paste0("co-administered with ", MyPerpetrator, " "), 
                         "")
      
      Heading <- paste0(Text1, 
                        switch(plot_type, 
                               "enzyme-abundance" = " enzyme-abundance profiles of ",
                               "fm" = "Time-dependent fm values for ", 
                               "release-profile" = " release profiles of ",
                               "dissolution-profile" = " dissolution profiles of ",
                               "concentration-time" = " concentration-time profiles of ", 
                               "mass-time" = " profiles of "), 
                        MyCompound, " ", DDItext1, "in ", 
                        Pop, ".")
      
      
      ## Setting up caption --------------------------------------------------------
      
      ObsIncluded <- nrow(ct_dataframe %>% filter(Simulated == FALSE)) > 0
      ObsShowsMean <- all(c(exists("obs_dataframe", inherits = FALSE), 
                            exists("check", inherits = FALSE))) &&
         nrow(obs_dataframe) > 0 && all(check$N == 1)
      
      if(DDI){
         
         if(DetailsAvailable && 
            any(complete.cases(existing_exp_details$MainDetails$Inhibitor2))){
            warning("The figure heading and caption text have not been set up for when there are two perpetrators in a DDI simulation. Please check and adjust the text carefully.\n", 
                    call. = FALSE)
         }
         
         CapText1 <- paste0(
            "Depicted are ", 
            case_when(plot_type == "fm" ~ "", 
                      .default = Tissue),
            switch(plot_type, 
                   "enzyme-abundance" = " enzyme-abundance profiles of ",
                   "fm" = "time-dependent fm values for ", 
                   "release-profile" = " release profiles of ",
                   "dissolution-profile" = " dissolution profiles of ",
                   "concentration-time" = " concentration-time profiles of "), 
            MyCompound, " following ", 
            
            switch(CmpdRoles, 
                   
                   "perpetrator" = paste0(DosingText_inhib_lower, "."),
                   
                   "victim" = paste0(DosingText_sub_lower, 
                                     " at baseline (solid line) and ", 
                                     
                                     # FIXME: I think the below was incorrect, so I've commented out
                                     # for now the bits I think were wrong. Need to check that we
                                     # don't need this!!!
                                     
                                     # ifelse(DoseFreq_inhib == "single dose", 
                                     # single dose of perp 
                                     paste0("with ", 
                                            DosingText_inhib_lower, 
                                            " (dashed line).")#, 
                                     
                                     # # multiple doses of perp
                                     # paste0("on the ", 
                                     #        DoseDay_ordinal, " day of ",
                                     #        NumDaysInhib, " days of dosing of ",
                                     #        MyPerpetrator, " ", Dose_inhib, " ",
                                     #        Units_dose_inhib, " ",
                                     #        DoseFreq_inhib, " (dashed line)."))
                   )
            ))
         
      } else {
         
         CapText1 <- paste0(
            "Depicted are ", 
            case_when(ObsIncluded ~ 
                         paste0("simulated (lines) and observed (circles;",  
                                ifelse(ObsShowsMean, 
                                       paste0(mean_type, 
                                              " mean of n = ", N_subjpertrial, 
                                              " individuals; "), 
                                       paste0(" n = ", N_subjpertrial, 
                                              " individuals; ")),
                                MyClinStudies, ") "),
                      plot_type == "fm" ~ "", 
                      .default = "simulated "), 
            case_when(plot_type == "fm" ~ "", 
                      .default = Tissue), 
            switch(plot_type, 
                   "enzyme-abundance" = " enzyme-abundance profiles of ",
                   "fm" = "time-dependent fm values for ", 
                   "release-profile" = " release profiles of ",
                   "dissolution-profile" = " dissolution profiles of ",
                   "concentration-time" = " concentration-time profiles of "), 
            MyCompound, " following ", 
            case_when(CompoundID %in% AllRegCompounds$CompoundID[
               AllRegCompounds$DDIrole == "victim"] ~ DosingText_sub_lower, 
               .default = DosingText_inhib_lower), 
            ".") 
      }
      
      # Hack to deal w/"compound summary" and "Freddy" figure types looking like
      # "percentiles" figure type when it's a DDI sim.
      figure_type_mod <- case_when(figure_type %in% c("compound summary", 
                                                      "Freddy") &
                                      DDI == TRUE ~ "percentiles", 
                                   .default = figure_type)
      
      CapText2 <- switch(
         figure_type_mod, 
         "trial means" = 
            paste0("The lighter lines represent ",
                   mean_type, 
                   " mean values of simulated individual trials and the darker lines portray the ",
                   mean_type, 
                   " mean data of the simulated population (n = ", N_trials, " trials of ", N_subjpertrial, " subjects per trial)."), 
         "percentiles" = 
            paste0("The lighter lines represent the 5^th^ and 95^th^ percentiles and the solid darker line the ",
                   mean_type, 
                   " mean data for the simulated population (n = ", N_indiv, ")."), 
         "percentile ribbon" = 
            paste0("The shaded regions represent the 5^th^ to the 95^th^ percentiles and the solid darker line the ",
                   mean_type, 
                   " mean data for the simulated population (n = ", N_indiv, ")."), 
         "means only" = 
            paste0("The solid line represents the ", 
                   mean_type, 
                   " mean data for the simulated population (n = ", N_indiv, ")."), 
         "Freddy" = 
            paste0("The lighter lines represent the ", 
                   mean_type, 
                   " mean values of simulated individual trials and the darker lines portray the ",
                   mean_type, 
                   " mean data of the simulated population (n = ", N_indiv, "). The dashed lines represent the 5^th^ and 95^th^ percentiles."), 
         "compound summary" = 
            paste0("The lighter lines represent the ", 
                   mean_type, 
                   " mean values of simulated individual trials and the darker lines portray the ",
                   mean_type, 
                   " mean data of the simulated population (n = ", N_indiv, "). The dashed lines represent the 5^th^ and 95^th^ percentiles."))
      
      if(any(complete.cases(hline_position))){
         
         HLineAES <- str_split(hline_style, pattern = " ")[[1]]
         
         CapText2 <- paste0(CapText2, "* ",
                            paste("*The horizontal", HLineAES[1], HLineAES[2], 
                                  ifelse(length(hline_position) > 1, 
                                         "lines indicate*", "line indicates*"), 
                                  "***XXX.*** "))
      }
      
      if(any(complete.cases(vline_position))){
         
         VLineAES <- str_split(vline_style, pattern = " ")[[1]]
         
         CapText2 <- paste0(CapText2, ifelse(any(complete.cases(hline_position)), 
                                             " ", "* "),
                            paste("*The vertical", VLineAES[1], VLineAES[2],
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
                        str_comma(basename(unique(ct_dataframe$File[ct_dataframe$Simulated == TRUE]))), 
                        ".")
      
      return(list("heading" = Heading, 
                  "caption" = Caption))
      
   } else {
      # Multiple profiles -----------------------------------------------------
      
      ## Getting general info and setting up heading ---------------------------
      
      Text1 <- switch(linear_or_log, 
                      "both" = "Linear (A) and log-linear (B)",
                      "both horizontal" ="Linear (A) and log-linear (B)",
                      "both vertical" = "Linear (A) and log-linear (B)",
                      "linear" = "Linear", 
                      "log" = "Log-linear",
                      "semi-log" = "Log-linear")
      Text1 <- case_when(plot_type == "fm" ~ "", 
                         .default = Text1)
      
      Text2 <- switch(plot_type, 
                      "enzyme-abundance" = paste0(" simulated enzyme-abundance profiles of ",
                                                  str_comma(unique(ct_dataframe$Enzyme)), 
                                                  " in ", str_comma(unique(ct_dataframe$Tissue)), "."),
                      "fm" = paste0("Time-dependent fm profiles for ",
                                    str_comma(unique(ct_dataframe$Enzyme)), 
                                    " in ", str_comma(unique(ct_dataframe$Tissue)), "."), 
                      "release-profile" = paste0(" release profiles of ",
                                                 MyCompound, "."),
                      "dissolution-profile" = paste0(" dissolution profiles of ",
                                                     MyCompound, "."),
                      "concentration-time" = paste0(" ", 
                                                    str_comma(unique(ct_dataframe$Tissue)), 
                                                    " simulated concentration-time profiles of ", # keep 1st and last spaces here
                                                    MyCompound, "."))
      
      Heading <- paste0(Text1, Text2)
      
      ## Caption --------------------------------------------------------------
      
      CapText1 <-
         switch(
            paste(ifelse(InhibPresent, "InhibPresent", "NoInhibPresent"), 
                  CmpdRoles, plot_type), 
            
            # Note that when NoInhibPresent, CmpdRoles will ALWAYS be
            # "victim" and that when InhibPresent, CmpdRoles can be any of
            # "perpetrator", "perpetrator-victim", or "victim".
            
            "NoInhibPresent victim concentration-time" = ifelse(
               nrow(ct_dataframe %>% filter(Simulated == FALSE)) > 0,
               
               # obs data present
               paste0("simulated and observed data (circles; mean of n = ",
                      N_subjpertrial, " individuals; ",
                      MyClinStudies, ")"),  
               
               # no obs data present
               "simulated data"), 
            
            "NoInhibPresent victim enzyme-abundance" = "simulated data", 
            
            "NoInhibPresent victim release-profile" = "release profile data", 
            
            "NoInhibPresent victim dissolution-profile" = "dissolution profile data", 
            
            "InhibPresent victim concentration-time" = paste0(
               ifelse(nrow(ct_dataframe %>% filter(Simulated == FALSE)) > 0,
                      
                      # obs data present
                      paste0("simulated and observed data (circles; mean of n = ",
                             N_subjpertrial, " individuals; ",
                             MyClinStudies, ")"),
                      
                      # no obs data present       
                      ""), 
               " ", Tissue, " concentration-time profiles of ",
               MyCompound, " following ", DosingText_sub_lower, 
               " (solid line) and ",
               
               # FIXME: I think the below was incorrect, so I've commented out
               # for now the bits I think were wrong. Need to check that we
               # don't need this!!!
               
               # ifelse(DoseFreq_inhib == "single dose", 
               DosingText_inhib_lower, 
               # paste0("on the ", DoseDay_ordinal, 
               #        " day of ", NumDaysInhib, " days of dosing ", 
               #        MyPerpetrator)), 
               " (dashed line)"), 
            
            "InhibPresent perpetrator-victim concentration-time" = paste0(
               ifelse(nrow(ct_dataframe %>% filter(Simulated == FALSE)) > 0,
                      
                      # obs data present
                      paste0("simulated and observed data (circles; mean of n = ",
                             N_subjpertrial, " individuals; ",
                             MyClinStudies, ")"), 
                      
                      # no obs data present
                      ""), 
               " ", Tissue, " concentration-time profiles of ",
               MyCompound, " following ", DosingText_sub_lower, 
               " and ", DosingText_inhib_lower), 
            
            "InhibPresent perpetrator concentration-time" = paste0(
               ifelse(nrow(ct_dataframe %>% filter(Simulated == FALSE)) > 0,
                      
                      # obs data present
                      paste0("simulated and observed data (circles; mean of n = ",
                             N_subjpertrial, " individuals; ",
                             MyClinStudies, ")"), 
                      
                      # no obs data present
                      ""), 
               " ", Tissue, " concentration-time profiles of ",
               MyCompound, " following ", DosingText_inhib_lower), 
            
            # NB: I *think* that enzyme-abundance plots will only have the
            # DDI role be victim when there's no perpetrator present in the
            # simulation and so you'd only be interested in autoinduction or
            # autosuppression. This text should work for that.
            "InhibPresent victim enzyme-abundance" = paste0(
               "simulated enzyme-abundance profiles of ",
               str_comma(unique(ct_dataframe$Enzyme), conjunction = "or"), 
               " following ", DosingText_sub_lower),  
            
            # This is text for a regular enzyme-abundance plot +/- a
            # perpetrator.
            "InhibPresent perpetrator enzyme-abundance" = paste0(
               "simulated enzyme-abundance profiles of ",
               str_comma(unique(ct_dataframe$Enzyme), conjunction = "or"), 
               " in the absence or presence of ", MyPerpetrator, 
               " following ", DosingText_inhib_lower)
         )
      
      CapText2 <- switch(
         figure_type, 
         "trial means" = paste("The lighter lines represent the", 
                               mean_type, 
                               "mean values of simulated individual trials and the darker lines the",
                               mean_type, 
                               paste0("mean data for the simulated population (n = ", 
                                      N_indiv, ")")), 
         
         "percentiles" = paste("The lighter lines represent the 5^th^ and 95^th^ percentiles and the darker lines the",
                               mean_type, 
                               paste0("mean data for the simulated population (n = ", 
                                      N_indiv, ")")), 
         
         "percentile ribbon" = ifelse(
            plot_type %in% c("release-profile", 
                             "dissolution-profile"), 
            "The lines represent the arithmetic mean, and the shaded regions represent the mean plus or minus the standard deviation", 
            paste("The shaded regions represent the 5^th^ to the 95^th^ percentiles and the darker lines the",
                  mean_type, 
                  paste0("mean data for the simulated population (n = ", 
                         N_indiv, ")"))), 
         
         "means only" = ifelse(plot_type %in% c("release-profile", 
                                                "dissolution-profile"), 
                               "The lines represent the arithmetic mean", 
                               paste("The lines represent the",
                                     mean_type, 
                                     paste0("mean data for the simulated population (n = ", 
                                            N_indiv, ")"))), 
         
         "Freddy" = paste("The lighter lines represent",
                          mean_type, 
                          "mean values of simulated individual trials and the darker lines portray the",
                          mean_type, 
                          paste0("mean data for the simulated population (n = ", 
                                 N_indiv, ")"), 
                          ". The dashed lines represent the 5^th^ and 95^th^ percentiles"))
      
      Caption <- paste0("Depicted are ", CapText1, ". ", 
                        CapText2, ". Source simulated data: ",
                        str_comma(unique(ct_dataframe$File[ct_dataframe$Simulated == TRUE])))
      
      return(list("heading" = Heading, 
                  "caption" = Caption))
      
   }
}

