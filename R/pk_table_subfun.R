#' Subfunction for pk_table. FOR INTERNAL PACKAGE USE.
#'
#' @param sim_data_file sim_data_file
#' @param PKparameters data.frame that includes Tissue, CompoundID, and Sheet
#' @param existing_exp_details harmonized existing_exp_details
#' @param convert_conc_units how to convert conc units, if desired
#' @param MeanType main mean type
#' @param GMR_mean_type GMR mean type
#' @param includeTrialMeans determines whether to get individual data
#'   w/extractPK
#' @param use_median_for_tmax T or F
#'
#' @return a list of 1) "PK" - a data.frame of unformatted simulated and, when
#'   supplied, observed PK data with the columns Stat, PKParam, and Sim. NOTHING
#'   has been rounded. 2) "PKrequested" - which PK parameters were requested, 3)
#'   "PKpulled" - which PK parameters were successfully pulled, 4) "QC" - QC
#'   info, 5) "ForestData" - forest data formatted appropriately for the
#'   forest_plot function, and 6) "CheckDoseInt" - list of the message and
#'   dosing interval data
#'
#' @examples
#' # none
pk_table_subfun <- function(sim_data_file, 
                            PKparameters, 
                            existing_exp_details, 
                            convert_conc_units, 
                            MeanType, 
                            GMR_mean_type, 
                            includeTrialMeans, 
                            use_median_for_tmax){
   
   ## extracting PK ------------------------------------------------------------
   
   existing_exp_details <- filter_sims(existing_exp_details,
                                       sim_data_file, 
                                       "include")
   Deets <- existing_exp_details$MainDetails
   PerpPresent <- complete.cases(Deets$Inhibitor1)
   DoseRegimen <- Deets$Regimen_sub
   
   PKrequested <- PKparameters$PKparameter
   
   # If they requested AUCinf_dose1 but not AUCt_dose1, then add AUCt_dose1 to
   # the set of parameters to pull in case of trouble with extrapolation.
   
   # For checking whether AUCt_dose1 was already included: 
   PKparameters <- PKparameters %>% 
      unite(col = ID, c(File, Sheet, CompoundID, Tissue, PKparameter), 
            remove = FALSE)
   
   ToAdd <- PKparameters %>% filter(str_detect(PKparameter, "AUCinf")) %>% 
      mutate(PKparameter = sub("AUCinf", "AUCt", PKparameter), 
             Value = NA, 
             # Variability needs to be character to be able to combine w/other
             # variability options that user may have entered s/a "1 to 2".
             Variability = as.character(NA)) %>% 
      unite(col = ID, c(File, Sheet, CompoundID, Tissue, PKparameter),
            remove = FALSE) %>% 
      filter(ID %in% PKparameters$ID == FALSE)
   
   PKparameters <- PKparameters %>% 
      bind_rows(ToAdd) %>% 
      select(-ID)
   
   suppressWarnings(
      MyPKResults_all <- extractPK(
         sim_data_file = sim_data_file,
         PKparameters = PKparameters$PKparameter,
         tissue = unique(PKparameters$Tissue),
         compoundToExtract = unique(PKparameters$CompoundID),
         sheet = ifelse(unique(PKparameters$Sheet) == "default", 
                        NA, unique(PKparameters$Sheet)), 
         existing_exp_details = existing_exp_details,
         returnAggregateOrIndiv =
            switch(as.character(includeTrialMeans),
                   "TRUE" = c("aggregate", "individual"),
                   "FALSE" = "aggregate")))
   
   # If there were no PK parameters to be pulled, MyPKResults_all will have
   # length 0 and we can't proceed.
   if(length(MyPKResults_all) == 0){
      # warning(paste0("No PK results were found in the file `",
      #                sim_data_file, "` for ", unique(PKparameters$CompoundID), " in ", tissue,
      #                "."), 
      #         call. = FALSE)
      return()
   }
   
   CheckDoseInt <- check_doseint(
      sim_data_file = sim_data_file, 
      existing_exp_details = existing_exp_details,
      interval = ifelse(all(PKparameters$Sheet != "default"), 
                        unique(MyPKResults_all$TimeInterval$Interval), 
                        NA), 
      compoundID = unique(PKparameters$CompoundID),
      stop_or_warn_missing_file = "warn")
   
   if(CheckDoseInt$message == "mismatch last dose"){
      warning(wrapn(paste0("The time used for integrating the AUC for the last dose was not the same as the dosing interval for the simulation '", 
                           sim_data_file, "'.")), 
              call. = FALSE)
   } else if(CheckDoseInt$message == "mismatch user-defined interval"){
      warning(wrapn(paste0("The time used for integrating the AUC for the custom AUC interval was not the same as the dosing interval for the simulation'", 
                           sim_data_file, "'.")),
              call. = FALSE)
   }
   
   CheckDoseInt$interval$Sheet <- unique(PKparameters$Sheet)
   CheckDoseInt$interval$Tissue <- unique(PKparameters$Tissue)
   
   # Sometimes missing problems with extrapolation to infinity. Checking for
   # that here. I thought that there wouldn't be any values for AUCinf, but
   # there definitely are. If any of the AUCinf_X parameters have trouble with
   # extrapolation, the others won't be useful either. Checking for any NA
   # values in geomean, mean, or median. 
   ExtrapCheck <- MyPKResults_all$aggregate %>% 
      filter(Statistic %in% c("mean", "median", "geometric mean")) %>%
      select(matches("AUCinf")) %>% 
      summarize(across(.cols = everything(), .fns = function(x) any(is.na(x))))
   
   if(any(ExtrapCheck == TRUE)){
      MyPKResults_all$aggregate <- MyPKResults_all$aggregate %>% 
         select(-matches("AUCinf"))
   }
   
   # Changing units if user wants. 
   if(complete.cases(convert_conc_units) & is.na(Deets$Units_Cmax)){
      warning("You requested that we convert the concentration units, but we can't find what units were used in your simulation. (This is often the case for Discovery simulations in particular.) We won't be able to convert the concentration units.\n", 
              call. = FALSE)
      convert_conc_units <- NA
   }
   
   if(complete.cases(convert_conc_units)){
      # Only adjusting AUC and Cmax values and not adjusting time portion of
      # units -- only conc.
      if(Deets$Units_Cmax != convert_conc_units){
         ColsToChange <- names(MyPKResults_all$aggregate)[
            intersect(which(str_detect(names(MyPKResults_all$aggregate), "AUC|Cmax|Cmin")), 
                      which(!str_detect(names(MyPKResults_all$aggregate), "ratio")))
         ]
         
         for(i in ColsToChange){
            TEMP <- convert_units(
               MyPKResults_all$aggregate %>% 
                  rename(Conc = i) %>% 
                  mutate(CompoundID = unique(PKparameters$CompoundID), 
                         Conc_units = Deets$Units_Cmax, 
                         Time = 1, Time_units = "hours"),
               DF_with_good_units = list("Conc_units" = convert_conc_units, 
                                         "Time_units" = "hours"), 
               MW = c(compoundToExtract = 
                         switch(unique(PKparameters$CompoundID), 
                                "substrate" = Deets$MW_sub, 
                                "primary metabolite 1" = Deets$MW_met1, 
                                "primary metabolite 2" = Deets$MW_met2, 
                                "secondary metabolite" = Deets$MW_secmet, 
                                "inhibitor 1" = Deets$MW_inhib, 
                                "inhibitor 2" = Deets$MW_inhib2, 
                                "inhibitor 1 metabolite" = Deets$MW_inhib1met)))
            GoodRows <- which(!str_detect(TEMP$Statistic, "CV|cv|Skewness|Fold"))
            MyPKResults_all$aggregate[GoodRows, i] <- TEMP$Conc[GoodRows]
            rm(TEMP)
            
            if("individual" %in% names(MyPKResults_all)){
               TEMP <- convert_units(
                  MyPKResults_all$individual %>% 
                     rename(Conc = i) %>% 
                     mutate(CompoundID = unique(PKparameters$CompoundID), 
                            Conc_units = Deets$Units_Cmax, 
                            Time = 1, Time_units = "hours"),
                  DF_with_good_units = list("Conc_units" = convert_conc_units, 
                                            "Time_units" = "hours"), 
                  MW = c(compoundToExtract = 
                            switch(unique(PKparameters$CompoundID), 
                                   "substrate" = Deets$MW_sub, 
                                   "primary metabolite 1" = Deets$MW_met1, 
                                   "primary metabolite 2" = Deets$MW_met2, 
                                   "secondary metabolite" = Deets$MW_secmet, 
                                   "inhibitor 1" = Deets$MW_inhib, 
                                   "inhibitor 2" = Deets$MW_inhib2, 
                                   "inhibitor 1 metabolite" = Deets$MW_inhib1met)))
               MyPKResults_all$individual[, i] <- TEMP$Conc
               rm(TEMP)
            }
         }
         
         # Need to change units in Deets now to match.
         Deets$Units_AUC <- sub(Deets$Units_Cmax, convert_conc_units, Deets$Units_AUC)
         Deets$Units_Cmax <- convert_conc_units
      }
   }
   
   # Noting PK that were successfully pulled.
   PKpulled <- setdiff(names(MyPKResults_all$aggregate), "Statistic")
   
   # If they requested AUCinf but there was trouble with that extrapolation,
   # AUCinf won't be present in the data but AUCt will be. Check for that and
   # change PKpulled to reflect that change.
   if(any(str_detect(PKrequested, "AUCinf")) & 
      any(str_detect(PKpulled, "AUCinf")) == FALSE){
      warning(paste0("AUCinf included NA values in the file `", 
                     sim_data_file, 
                     "`, meaning that the Simulator had trouble extrapolating to infinity and thus making the AUCinf summary data unreliable. We will supply AUCt instead."),
              call. = FALSE)
      PKparameters <- PKparameters %>% 
         filter(!str_detect(PKparameter, "AUCinf"))
   }
   
   # If they requested multiple parameters but only some were present, need to
   # change PKpulled. 
   Missing <- setdiff(PKrequested, PKpulled)
   # If it was AUCinf that's missing b/c of extrapolation issues, that was
   # already addressed.
   Missing <- setdiff(Missing, "AUCinf_dose1")
   
   if(length(Missing) > 0){
      warning(wrapn(paste0("The following parameters were requested for the simulation '", 
                           sim_data_file, 
                           "' but not found in your simulator output file:",
                           str_comma(Missing))),
              call. = FALSE)
   }
   
   MyPKResults <- MyPKResults_all$aggregate
   
   # Accounting for when mean_type is arithmetic but the user requests that the
   # ratio for + perpetrator over - perpetrator be a GMR. This will replace the
   # arithmetic mean ratio data with geometric mean ratio data. However,
   # because later we need to join that with obs data and we need to use the
   # correct mean type throughout, this will be labeled as "mean" rather than
   # "geomean". Yes, that's confusing, so my apologies, but I couldn't come up
   # with a better way to do this. -LSh
   if(MeanType == "arithmetic" &&
      PerpPresent == TRUE &&
      complete.cases(GMR_mean_type) &&
      GMR_mean_type == "geometric"){
      
      MyPKResults[MyPKResults$Statistic == "mean",
                  str_detect(names(MyPKResults), "ratio")] <-
         MyPKResults[MyPKResults$Statistic == "geometric mean",
                     str_detect(names(MyPKResults), "ratio")]
   }
   
   # Adding trial means since they're not part of the default output
   if(includeTrialMeans){
      
      suppressWarnings(
         TrialMeans <- MyPKResults_all$individual %>%
            group_by(Trial) %>%
            summarize(across(.cols = -Individual,
                             .fns = list("geomean" = gm_mean, 
                                         "mean" = mean, 
                                         "median" = median), 
                             .names = "{.col}-{.fn}")) %>%
            ungroup() %>%
            pivot_longer(cols = -Trial, names_to = "Parameter",
                         values_to = "Value") %>%
            separate(col = Parameter, into = c("Parameter", "Stat"), 
                     sep = "-") 
      )
      
      if(use_median_for_tmax){
         TrialMeans <- TrialMeans %>% 
            filter((str_detect(Parameter, "tmax") & Stat == "median") |
                      (!str_detect(Parameter, "tmax") & 
                          Stat == switch(MeanType, "geometric" = "geomean", 
                                         "arithmetic" = "mean")))
      } else {
         TrialMeans <- TrialMeans %>% 
            filter(Stat == switch(MeanType, "geometric" = "geomean", 
                                  "arithmetic" = "mean"))
      }
      
      TrialMeans <- TrialMeans %>% 
         group_by(Parameter) %>%
         summarize(MinMean = min(Value),
                   MaxMean = max(Value)) %>%
         pivot_longer(cols = -Parameter,
                      names_to = "Statistic", values_to = "Value") %>%
         pivot_wider(names_from = Parameter, values_from = Value)
      
      MyPKResults <- MyPKResults %>% bind_rows(TrialMeans)
   }
   
   # FIXME - May make this bit a standalone function b/c I sometimes would like
   # to use it elsewhere. Low priority.
   
   # Renaming stats for ease of coding
   MyPKResults <- MyPKResults %>% mutate(Stat = renameStats(Statistic))
   
   # Most of the time, people want median for tmax, but, if they don't want the
   # median, don't change anything and skip this next section. 
   
   if(use_median_for_tmax & 
      any(str_detect(names(MyPKResults), "tmax"))){
      
      # Adjusting tmax values since the mean row will actually be the median, the
      # lower range of conf interval and percentiles will be the min, and the
      # upper range will be the max.
      if("tmax_dose1" %in% names(MyPKResults)){
         MyPKResults$tmax_dose1[
            which(MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean"))] <-
            MyPKResults$tmax_dose1[which(MyPKResults$Stat == "median")]
         
         MyPKResults$tmax_dose1[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
            MyPKResults$tmax_dose1[MyPKResults$Stat == "min"]
         
         MyPKResults$tmax_dose1[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
            MyPKResults$tmax_dose1[MyPKResults$Stat == "max"]
         
         if(PerpPresent & "tmax_dose1_withInhib" %in% names(MyPKResults)){
            MyPKResults$tmax_dose1_withInhib[
               MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
               MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "median"]
            
            MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
               MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "min"]
            
            MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
               MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "max"]
         }
      }
      
      if("tmax_last" %in% names(MyPKResults)){
         MyPKResults$tmax_last[
            MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
            MyPKResults$tmax_last[MyPKResults$Stat == "median"]
         MyPKResults$tmax_last[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
            MyPKResults$tmax_last[MyPKResults$Stat == "min"]
         MyPKResults$tmax_last[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
            MyPKResults$tmax_last[MyPKResults$Stat == "max"]
         
         if(PerpPresent & "tmax_last_withInhib" %in% names(MyPKResults)){
            MyPKResults$tmax_last_withInhib[
               MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
               MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "median"]
            MyPKResults$tmax_last_withInhib[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
               MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "min"]
            MyPKResults$tmax_last_withInhib[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
               MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "max"]
         }
         
      }
      
      # For scenario where user specifies which tab to get data from
      if("tmax" %in% names(MyPKResults)){
         MyPKResults$tmax[
            MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
            MyPKResults$tmax[MyPKResults$Stat == "median"]
         MyPKResults$tmax[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
            MyPKResults$tmax[MyPKResults$Stat == "min"]
         MyPKResults$tmax[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
            MyPKResults$tmax[MyPKResults$Stat == "max"]
         
         if(PerpPresent & "tmax_withInhib" %in% names(MyPKResults)){
            MyPKResults$tmax_withInhib[
               MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
               MyPKResults$tmax_withInhib[MyPKResults$Stat == "median"]
            MyPKResults$tmax_withInhib[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
               MyPKResults$tmax_withInhib[MyPKResults$Stat == "min"]
            MyPKResults$tmax_withInhib[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
               MyPKResults$tmax_withInhib[MyPKResults$Stat == "max"]
         }
         
      }
      
      # CV and SD should be NA for all tmax values b/c we're reporting medians and
      # range and NOT reporting a mean or geometric mean. Setting that.
      MyPKResults <- MyPKResults %>% 
         mutate(across(.cols = matches("tmax"), 
                       ~replace(., Stat %in% c("CV", "GCV", "SD"), NA)))
      
   }
   
   MyPKResults <- MyPKResults %>%
      select(-Statistic) %>%
      select(Stat, everything()) %>%
      pivot_longer(cols = -Stat, 
                   names_to = "PKParam",
                   values_to = "Sim")
   
   # Adding observed data ----------------------------------------------------
   
   # NB: Absolutely NO ROUNDING and formatting here. Only rounding AFTER
   # extracting forest data.
   if("Value" %in% names(PKparameters) && 
      any(complete.cases(PKparameters$Value))){
      
      if("Variability" %in% names(PKparameters) && 
         any(complete.cases(PKparameters$Variability))){
         
         ObsPK_var <- PKparameters %>% 
            filter(complete.cases(Value) &
                      complete.cases(Variability)) %>% 
            select(PKparameter, Variability)
         
         if(nrow(ObsPK_var) == 0){
            # placeholder
            ObsPK_var <- PKparameters %>% 
               select(PKparameter) %>% 
               mutate(Stat = switch(MeanType, 
                                    "geometric" = "GCV", 
                                    "arithmetic" = "CV"), 
                      Value = as.numeric(NA), 
                      SorO = "Obs")
         } else {
            
            suppressWarnings(
               ObsPK_var <- ObsPK_var %>% 
                  rename(PKParam = PKparameter) %>% 
                  separate(col = Variability, 
                           into = c("Variability1", "Variability2"), 
                           sep = "-|( )?to( )?") %>% 
                  mutate(across(.cols = c(Variability1, Variability2), .fn = as.numeric), 
                         Stat = case_when(complete.cases(Variability2) ~ "range", 
                                          is.na(Variability2) & MeanType == "arithmetic" ~ "CV", 
                                          is.na(Variability2) & MeanType == "geometric" ~ "GCV")) %>% 
                  pivot_longer(cols = c(Variability1, Variability2), 
                               names_to = "VariableType", 
                               values_to = "Value") %>% 
                  mutate(Stat = case_when(Stat == "range" & VariableType == "Variability1" ~ "min", 
                                          Stat == "range" & VariableType == "Variability2" ~ "max", 
                                          TRUE ~ Stat), 
                         SorO = "Obs") %>% 
                  filter(complete.cases(Value)) %>% 
                  select(-VariableType)
            )
         }
      } else {
         # placeholder
         ObsPK_var <- PKparameters %>% 
            select(PKparameter) %>% 
            mutate(Stat = switch(MeanType, 
                                 "geometric" = "GCV", 
                                 "arithmetic" = "CV"), 
                   Value = as.numeric(NA), 
                   SorO = "Obs") %>% 
            rename(PKParam = PKparameter)
      }
      
      # Pivoting main data longer by stat
      ObsPK <- PKparameters %>% 
         select(PKparameter, Value) %>% 
         mutate(Value = as.numeric(Value), 
                SorO = "Obs", 
                Stat = switch(MeanType, 
                              "geometric" = "geomean", 
                              "arithmetic" = "mean")) %>% 
         filter(complete.cases(Value)) %>% 
         rename(PKParam = PKparameter)
      
      # Calculating S/O
      suppressMessages(
         SOratios <- MyPKResults %>% 
            filter(Stat == switch(MeanType, 
                                  "geometric" = "geomean",
                                  "arithmetic" = "mean")) %>%
            left_join(ObsPK %>% 
                         rename(Obs = Value) %>% 
                         select(Stat, PKParam, Obs)) %>%
            mutate(Value = Sim / Obs,
                   Stat = "S_O", 
                   SorO = "S_O") %>%
            select(PKParam, Stat, Value, SorO) %>%  
            filter(complete.cases(Value))
      )
      
      if(includeTrialMeans){
         
         suppressMessages(
            SOratios_TM <- MyPKResults %>% 
               filter(Stat %in% c("MinMean", "MaxMean")) %>%
               left_join(ObsPK %>% 
                            filter(Stat == switch(MeanType, 
                                                  "geometric" = "geomean", 
                                                  "arithmetic" = "mean")) %>% 
                            select(-Stat)) %>%
               mutate(Value = Sim / Value) %>% 
               select(-Sim) %>% 
               mutate(Stat = paste0("S_O_TM_", Stat), 
                      SorO = "S_O_TM") %>%
               select(PKParam, Stat, Value, SorO)
         )
         
         SOratios <- bind_rows(SOratios, 
                               SOratios_TM)
         
      }
      
      MyPKResults <- MyPKResults %>% 
         rename(Value = Sim) %>% 
         mutate(SorO = "Sim") %>% 
         bind_rows(ObsPK, ObsPK_var, SOratios)
      
   } else {
      MyPKResults <- MyPKResults %>% 
         rename(Value = Sim) %>% 
         mutate(SorO = "Sim")
   }
   
   MyPKResults <- MyPKResults %>% 
      mutate(File = sim_data_file, 
             Sheet = ifelse(unique(PKparameters$Sheet) == "default", 
                            NA, unique(PKparameters$Sheet)), 
             CompoundID = unique(PKparameters$CompoundID), 
             Tissue = unique(PKparameters$Tissue))
   
   # Forest data ------------------------------------------------------------
   
   if(complete.cases(Deets$Inhibitor1)){
      
      # Extracting forest data only works when the compound to extract is the
      # substrate or a substrate metabolite. Could change that in the future
      # if there's call for it, but that's all for now.
      if(unique(PKparameters$CompoundID) %in% c("substrate", "primary metabolite 1", 
                                                "primary metabolite 2", 
                                                "secondary metabolite") == FALSE){
         warning("This function is currently only set up to extract forest data for the substrate or a substrate metabolite, so any other compounds will be skipped.", 
                 call. = FALSE)
         FD <- list()
         
      } else {
         
         # For forest data, only keeping ratios.
         FD <- MyPKResults %>% 
            filter(str_detect(PKParam, "ratio")) %>% 
            rename(PKparameter = PKParam) %>% 
            filter(str_detect(PKparameter, "AUCinf_[^P]|AUCt|Cmax")) %>% 
            mutate(File = sim_data_file, 
                   Victim = switch(unique(PKparameters$CompoundID), 
                                   "substrate" = Deets$Substrate, 
                                   "primary metabolite 1" = Deets$PrimaryMetabolite1, 
                                   "primary metabolite 2" = Deets$PrimaryMetabolite2, 
                                   "secondary metabolite" = Deets$SecondaryMetabolite), 
                   Dose_sub = Deets$Dose_sub, 
                   Perpetrator = case_when(complete.cases(Deets$Inhibitor1) & 
                                              is.na(Deets$Inhibitor1Metabolite) & 
                                              is.na(Deets$Inhibitor2) ~ Deets$Inhibitor1, 
                                           
                                           complete.cases(Deets$Inhibitor1) & 
                                              is.na(Deets$Inhibitor1Metabolite) & 
                                              complete.cases(Deets$Inhibitor2) ~ 
                                              str_comma(c(Deets$Inhibitor1, 
                                                          Deets$Inhibitor2)), 
                                           
                                           complete.cases(Deets$Inhibitor1) & 
                                              complete.cases(Deets$Inhibitor1Metabolite) & 
                                              is.na(Deets$Inhibitor2) ~ str_comma(c(Deets$Inhibitor1,
                                                                                    Deets$Inhibitor1Metabolite)), 
                                           
                                           complete.cases(Deets$Inhibitor1) & 
                                              complete.cases(Deets$Inhibitor1Metabolite) & 
                                              complete.cases(Deets$Inhibitor2) ~ 
                                              str_comma(c(Deets$Inhibitor1, 
                                                          Deets$Inhibitor1Metabolite,
                                                          Deets$Inhibitor2))),
                   
                   Dose_inhib = ifelse("Dose_inhib" %in% names(Deets),
                                       Deets$Dose_inhib, NA), 
                   Dose_inhib2 = ifelse("Dose_inhib2" %in% names(Deets),
                                        Deets$Dose_inhib2, NA), 
                   Substrate = Deets$Substrate, 
                   PrimaryMetabolite1 = Deets$PrimaryMetabolite1, 
                   PrimaryMetabolite2 = Deets$PrimaryMetabolite2, 
                   SecondaryMetabolite = Deets$SecondaryMetabolite, 
                   Inhibitor1 = Deets$Inhibitor1, 
                   Inhibitor2 = Deets$Inhibitor2, 
                   Inhibitor1Metabolite = Deets$Inhibitor1Metabolite) %>% 
            pivot_wider(names_from = Stat, values_from = Value) %>% 
            select(File, Victim, Dose_sub, Perpetrator, Dose_inhib, Dose_inhib2,
                   everything()) %>% 
            select(-CompoundID)
         
         # Need to deal with possible character data for custom dosing before
         # row binding for FD
         FD <- FD %>% mutate(Dose_sub = as.numeric(Dose_sub), 
                             Dose_inhib = as.numeric(Dose_inhib))
      }  
   } else {
      FD <- list()
   }
   
   
   # Out ---------------------------------------------------------------------
   
   return(list(
      "PK" = MyPKResults, 
      
      "PKrequested" = PKrequested, 
      
      "PKpulled" = PKpulled, 
      
      "QC" = MyPKResults_all$QC, 
      
      "ForestData" = FD, 
      
      "CheckDoseInt" = CheckDoseInt))
   
}


