#' Subfunction for pk_table. FOR INTERNAL PACKAGE USE.
#'
#' @param sim_data_file sim_data_file
#' @param PKparameters data.frame that includes Tissue, CompoundID, and Sheet
#' @param existing_exp_details harmonized existing_exp_details
#' @param conc_units how to convert conc units, if desired
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
                            conc_units, 
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
   
   if(nrow(ToAdd) > 0){
      PKparameters <- PKparameters %>% 
         bind_rows(ToAdd) %>% 
         select(-ID)
   }
   
   if(str_detect(sim_data_file, "xlsx$")){
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
                   "FALSE" = "aggregate"))
      
   } else if(str_detect(sim_data_file, "db$")){
      MyPKResults_all <- extractPK_DB(
         sim_data_file = sim_data_file,
         PKparameters = PKparameters$PKparameter,
         tissue = unique(PKparameters$Tissue),
         compoundToExtract = unique(PKparameters$CompoundID),
         existing_exp_details = existing_exp_details,
         returnAggregateOrIndiv =
            switch(as.character(includeTrialMeans),
                   "TRUE" = c("aggregate", "individual"),
                   "FALSE" = "aggregate"))
   }
   
   
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
      interval = list(ifelse(all(PKparameters$Sheet != "default"), 
                             unique(MyPKResults_all$TimeInterval$Interval), 
                             NA)), 
      compoundID = unique(PKparameters$CompoundID),
      stop_or_warn_missing_file = "warn")
   
   if(str_detect(CheckDoseInt$message, "mismatch")){
      warning(wrapn(paste0("For the simulation '", 
                           sim_data_file, "', ",
                           CheckDoseInt$message, "'.")), 
              call. = FALSE)
   } 
   
   if(CheckDoseInt$message == "custom dosing"){
      # This is just a pass-through when it's a custom dosing interval. It's OK
      # that there could be more than one interval.
      CheckDoseInt$interval$Interval <- 
         MyPKResults_all$TimeInterval$Interval %>% unique() %>%
         str_comma(conjunction = "or")
      
      CheckDoseInt$interval$Sheet <- 
         MyPKResults_all$TimeInterval$Sheet %>% unique() %>% 
         str_comma(conjunction = "or")
      
   } else {
      CheckDoseInt$interval <- CheckDoseInt$interval %>% 
         left_join(MyPKResults_all$TimeInterval %>% 
                      select(any_of(c("Interval", "Sheet"))) %>% unique(), 
                   by = "Interval")
   }
   CheckDoseInt$interval$Tissue <- unique(PKparameters$Tissue)
   
   # Sometimes missing problems with extrapolation to infinity. Checking for
   # that here. I thought that there wouldn't be any values for AUCinf, but
   # there definitely are. If any of the AUCinf_X parameters have trouble with
   # extrapolation, the others won't be useful either. Checking for any NA
   # values in geomean b/c they will definitely be NA if there's a problem. 
   ExtrapCheck <- MyPKResults_all$aggregate %>% 
      filter(str_detect(PKparameter, "AUCinf")) %>% 
      group_by(PKparameter) %>% 
      summarize(Check = any(is.na(Geomean)))
   # filter(Statistic %in% c("mean", "median", "geometric mean")) %>%
   # select(matches("AUCinf")) %>% 
   # summarize(across(.cols = everything(), .fns = function(x) any(is.na(x))))
   
   if(any(ExtrapCheck$Check == TRUE)){
      MyPKResults_all$aggregate <- MyPKResults_all$aggregate %>% 
         select(-matches("AUCinf"))
   }
   
   # Changing units if user wants. 
   if(complete.cases(conc_units) & is.na(Deets$Units_Cmax)){
      warning(wrapn("You requested that we convert the concentration units, but we can't find what units were used in your simulation. (This is often the case for Discovery simulations in particular.) We won't be able to convert the concentration units."), 
              call. = FALSE)
      conc_units <- NA
   }
   
   if(complete.cases(conc_units)){
      # Only adjusting AUC and Cmax values and not adjusting time portion of
      # units -- only conc.
      if(Deets$Units_Cmax != conc_units){
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
               DF_with_good_units = list("Conc_units" = conc_units, 
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
                  DF_with_good_units = list("Conc_units" = conc_units, 
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
         Deets$Units_AUC <- sub(Deets$Units_Cmax, conc_units, Deets$Units_AUC)
         Deets$Units_Cmax <- conc_units
      }
   }
   
   # Noting PK that were successfully pulled.
   PKpulled <- unique(MyPKResults_all$aggregate$PKparameter)
   
   # If they requested AUCinf but there was trouble with that extrapolation,
   # AUCinf won't be present in the data but AUCt will be. Check for that and
   # change PKpulled to reflect that change.
   if(any(str_detect(PKrequested, "AUCinf")) & 
      any(str_detect(PKpulled, "AUCinf")) == FALSE){
      warning(wrapn(paste0("AUCinf included NA values in the file `", 
                           sim_data_file, 
                           "`, meaning that the Simulator had trouble extrapolating to infinity and thus making the AUCinf summary data unreliable. We will supply AUCt instead.")),
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
                           "' but not found in your simulator output file: ",
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
      
      MyPKResults$Mean[str_detect(MyPKResults$PKparameter, "ratio")] <-
         MyPKResults$Geomean[str_detect(MyPKResults$PKparameter, "ratio")]
   }
   
   # Adding trial means since they're not part of the default output
   if(includeTrialMeans){
      suppressWarnings(
         TrialMeans <- MyPKResults_all$individual %>%
            group_by(Trial, PKparameter, File, Tissue, CompoundID, Compound, 
                     Simulated, Dose) %>%
            summarize(Geomean = gm_mean(Value), 
                      Mean = mean(Value), 
                      Median = median(Value)) %>%
            ungroup())
      
      if(use_median_for_tmax){
         TrialMeans <- TrialMeans %>% 
            mutate(
               Geomean = case_when(
                  {{MeanType}} == "geometric" & 
                     str_detect(PKparameter, "tmax") ~ Median, 
                  .default = Geomean), 
               
               Mean = case_when(
                  {{MeanType}} == "arithmetic" & 
                     str_detect(PKparameter, "tmax") ~ Median, 
                  .default = Geomean))
      } 
      
      TrialMeans <- TrialMeans %>% 
         group_by(PKparameter, File, Tissue, CompoundID, Compound, 
                  Simulated, Dose) %>%
         summarize(
            MinMean = case_when(
               {{MeanType}} == "geometric" ~ min(Geomean), 
               {{MeanType}} == "arithmetic" ~ min(Mean), 
               {{MeanType}} == "median" ~ min(Median)), 
            
            MaxMean = case_when(
               {{MeanType}} == "geometric" ~ max(Geomean), 
               {{MeanType}} == "arithmetic" ~ max(Mean), 
               {{MeanType}} == "median" ~ max(Median)))
      
      MyPKResults <- MyPKResults %>% 
         left_join(TrialMeans, by = c("File", "CompoundID", "Compound", 
                                      "Tissue", "Simulated",
                                      "Dose", "PKparameter"))
   }
   
   # FIXME - May make this bit a standalone function b/c I sometimes would like
   # to use it elsewhere. Low priority.
   
   # Making this wide by PKparameter and long by stat since that's how the
   # function was originally set up and there's no point in rewriting
   # everything.
   MyPKResults <- MyPKResults %>% 
      # Removing columns that mess up pivoting even though we'll add them back
      # later
      select(-any_of(c("Inhibitor", "Compound", "File", "CompoundID", 
                       "Tissue", "DoseNum", "Simulated", "Dose"))) %>% 
      pivot_longer(cols = any_of(sort(unique(AllStats$InternalColNames))), 
                   names_to = "Stat", 
                   values_to = "Value") %>% 
      pivot_wider(names_from = PKparameter, 
                  values_from = Value)
   
   # Most of the time, people want median for tmax, but, if they don't want the
   # median, don't change anything and skip this next section. 
   
   if(use_median_for_tmax & 
      any(str_detect(names(MyPKResults), "tmax"))){
      
      # Adjusting tmax values since the mean row will actually be the median, the
      # lower range of conf interval and percentiles will be the min, and the
      # upper range will be the max.
      if("tmax_dose1" %in% names(MyPKResults)){
         MyPKResults$tmax_dose1[
            which(MyPKResults$Stat == switch(MeanType, 
                                             "geometric" = "Geomean",
                                             "arithmetic" = "Mean"))] <-
            MyPKResults$tmax_dose1[which(MyPKResults$Stat == "Median")]
         
         MyPKResults$tmax_dose1[
            MyPKResults$Stat %in% c("Per5", "CI95_lower", "CI90_lower")] <-
            MyPKResults$tmax_dose1[MyPKResults$Stat == "Minimum"]
         
         MyPKResults$tmax_dose1[
            MyPKResults$Stat %in% c("Per95", "CI95_upper", "CI90_upper")] <-
            MyPKResults$tmax_dose1[MyPKResults$Stat == "Maximum"]
         
         if(PerpPresent & "tmax_dose1_withInhib" %in% names(MyPKResults)){
            MyPKResults$tmax_dose1_withInhib[
               MyPKResults$Stat == switch(MeanType,
                                          "geometric" = "Geomean", 
                                          "arithmetic" = "Mean")] <-
               MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "Median"]
            
            MyPKResults$tmax_dose1_withInhib[
               MyPKResults$Stat %in% c("Per5", "CI95_lower", "CI90_lower")] <-
               MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "Minimum"]
            
            MyPKResults$tmax_dose1_withInhib[
               MyPKResults$Stat %in% c("Per95", "CI95_upper", "CI90_upper")] <-
               MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "Maximum"]
         }
      }
      
      if("tmax_last" %in% names(MyPKResults)){
         MyPKResults$tmax_last[
            MyPKResults$Stat == switch(MeanType,
                                       "geometric" = "Geomean", 
                                       "arithmetic" = "Mean")] <-
            MyPKResults$tmax_last[MyPKResults$Stat == "Median"]
         
         MyPKResults$tmax_last[
            MyPKResults$Stat %in% c("Per5", "CI95_lower", "CI90_lower")] <-
            MyPKResults$tmax_last[MyPKResults$Stat == "Minimum"]
         
         MyPKResults$tmax_last[
            MyPKResults$Stat %in% c("Per95", "CI95_upper", "CI90_upper")] <-
            MyPKResults$tmax_last[MyPKResults$Stat == "Maximum"]
         
         if(PerpPresent & "tmax_last_withInhib" %in% names(MyPKResults)){
            MyPKResults$tmax_last_withInhib[
               MyPKResults$Stat == switch(MeanType, 
                                          "geometric" = "Geomean", 
                                          "arithmetic" = "Mean")] <-
               MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "Median"]
            
            MyPKResults$tmax_last_withInhib[
               MyPKResults$Stat %in% c("Per5", "CI95_lower", "CI90_lower")] <-
               MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "Minimum"]
            
            MyPKResults$tmax_last_withInhib[
               MyPKResults$Stat %in% c("Per95", "CI95_upper", "CI90_upper")] <-
               MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "Maximum"]
         }
         
      }
      
      # For scenario where user specifies which tab to get data from
      if("tmax" %in% names(MyPKResults)){
         MyPKResults$tmax[
            MyPKResults$Stat == switch(MeanType, 
                                       "geometric" = "Geomean",
                                       "arithmetic" = "Mean")] <-
            MyPKResults$tmax[MyPKResults$Stat == "Median"]
         
         MyPKResults$tmax[MyPKResults$Stat %in% c("Per5", "CI95_lower", "CI90_lower")] <-
            MyPKResults$tmax[MyPKResults$Stat == "Minimum"]
         
         MyPKResults$tmax[MyPKResults$Stat %in% c("Per95", "CI95_upper", "CI90_upper")] <-
            MyPKResults$tmax[MyPKResults$Stat == "Maximum"]
         
         if(PerpPresent & "tmax_withInhib" %in% names(MyPKResults)){
            MyPKResults$tmax_withInhib[
               MyPKResults$Stat == switch(MeanType, 
                                          "geometric" = "Geomean",
                                          "arithmetic" = "Mean")] <-
               MyPKResults$tmax_withInhib[MyPKResults$Stat == "Median"]
            
            MyPKResults$tmax_withInhib[MyPKResults$Stat %in% c("Per5", "CI95_lower", "CI90_lower")] <-
               MyPKResults$tmax_withInhib[MyPKResults$Stat == "Minimum"]
            
            MyPKResults$tmax_withInhib[MyPKResults$Stat %in% c("Per95", "CI95_upper", "CI90_upper")] <-
               MyPKResults$tmax_withInhib[MyPKResults$Stat == "Maximum"]
         }
         
      }
      
      # CV and SD should be NA for all tmax values b/c we're reporting medians and
      # range and NOT reporting a mean or geometric mean. Setting that.
      MyPKResults <- MyPKResults %>% 
         mutate(across(.cols = matches("tmax"), 
                       ~replace(., Stat %in% c("CV", "GCV", "SD"), NA)))
      
   }
   
   MyPKResults <- MyPKResults %>%
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
                         Stat = case_when(
                            complete.cases(Variability2) & 
                               str_detect(PKParam, "tmax") ~ "range", 
                            complete.cases(Variability2) & 
                               !str_detect(PKParam, "tmax") ~ "CI90", 
                            is.na(Variability2) & MeanType == "arithmetic" ~ "CV", 
                            is.na(Variability2) & MeanType == "geometric" ~ "GCV")) %>% 
                  pivot_longer(cols = c(Variability1, Variability2), 
                               names_to = "VariableType", 
                               values_to = "Value") %>% 
                  mutate(Stat = case_when(Stat == "range" & VariableType == "Variability1" ~ "Minimum", 
                                          Stat == "range" & VariableType == "Variability2" ~ "Maximum", 
                                          Stat == "CI90" & VariableType == "Variability1" ~ "CI90_lower", 
                                          Stat == "CI90" & VariableType == "Variability2" ~ "CI90_upper", 
                                          Stat == "CV" & VariableType == "Variability1" ~ "CV", 
                                          Stat == "GCV" & VariableType == "Variability1" ~ "GCV", 
                                          TRUE ~ Stat), 
                         SorO = "Obs") %>% 
                  filter(complete.cases(Value)) %>% 
                  select(-VariableType)
            )
            
            # FIXME - Need to set this up to detect what kind of variability
            # stat is being used for the observed data b/c, currently, if they
            # have supplied a SD, this thinks it might be a CV and multiplies it
            # by 100 and labels it as "CV". HACK FOR NOW: I'm adding rows for SD
            # and telling the user to select which it actually is.
            if(any(ObsPK_var$Stat %in% c("CV")) & MeanType != "geometric"){
               warning(wrapn(paste0(
                  "You've included some variability for the observed data that we're including in your table, but, tbh, we have not yet set up anything to detect what *kind* of variability it is. You've given us one number per PK parameter for this variability and you've requested ", 
                  case_match(MeanType, 
                             "arithmetic" ~ "arithmetic means", 
                             "median" ~ "medians"), 
                  ", so it could be a CV or it could be a standard deviation. We're going to add rows for both in your table. Please remove whichever does not apply. Please also note that CVs will be multiplied by 100.")), 
                  call. = FALSE)
               ObsPK_var <- bind_rows(ObsPK_var, 
                                      ObsPK_var %>% mutate(Stat = "SD"))
            }
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
                              "geometric" = "Geomean", 
                              "arithmetic" = "Mean")) %>% 
         filter(complete.cases(Value)) %>% 
         rename(PKParam = PKparameter)
      
      # Calculating S/O. NB: If use_median_for_tmax is TRUE, then the stat in
      # ObsPK for all tmax values will be the regular stat, e.g., Geomean, and
      # NOT median. Simulated PK, though, will still contain all stats, so we
      # need to filter simulated PK appropriately here but then retain whatever
      # is present in ObsPK for tmax.
      suppressMessages(
         SOratios <- MyPKResults %>% 
            mutate(Keep = 
                      case_when(
                         (use_median_for_tmax == FALSE |
                             !str_detect(PKParam, "tmax")) & 
                            MeanType == "geometric" & 
                            Stat == "Geomean" ~ TRUE, 
                         
                         (use_median_for_tmax == FALSE |
                             !str_detect(PKParam, "tmax")) & 
                            MeanType == "arithmetic" & 
                            Stat == "Mean" ~ TRUE, 
                         
                         MeanType == "median" & 
                            Stat == "Median" ~ TRUE, 
                         
                         use_median_for_tmax == TRUE & 
                            str_detect(PKParam, "tmax") & 
                            Stat == "Median" ~ TRUE, 
                         
                         .default = FALSE)) %>% 
            filter(Keep == TRUE) %>% 
            mutate(Stat = case_when(MeanType == "geometric" ~ "Geomean", 
                                    MeanType == "arithmetic" ~ "Mean", 
                                    MeanType == "median" ~ "Median")) %>% 
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
                                                  "geometric" = "Geomean", 
                                                  "arithmetic" = "Mean")) %>% 
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
      if(unique(PKparameters$CompoundID) %in% 
         c("substrate", "primary metabolite 1", 
           "primary metabolite 2", "secondary metabolite") == FALSE){
         warning(wrapn("This function is currently only set up to extract forest data for the substrate or a substrate metabolite, so any other compounds will be skipped."), 
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
         suppressWarnings(
            FD <- FD %>% mutate(Dose_sub = as.numeric(Dose_sub), 
                                Dose_inhib = as.numeric(Dose_inhib))
         )
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


