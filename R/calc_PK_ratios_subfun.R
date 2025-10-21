#' INTERNAL - subfunction for calc_PK_ratios and calc_PK_ratios_mult, which I
#' always should have made as just one function.
#'
#' This will compare PK for ONE set of data, where that set is defined as ONE
#' compoundID, ONE tissue, ONE simulation file, and ONE interval (1st dose, last
#' dose, or a user-defined interval) for the numerator and ONE of each of those
#' for the denominator.
#'
#' @param PKparameters PK parameters as a tibble
#' @param existing_exp_details existing_exp_details
#' @param paired TRUE (default) or FALSE
#' @param match_subjects_by "individual and trial" (default) or "individual
#'   only", which matches only by the individual ID number.
#' @param distribution_type use a "t" distribution (default) or a "Z"
#'   distribution. Note: The Simcyp Simulator calculates geometric confidence
#'   intervals with a t distribution.
#' @param mean_type "arithmetic" or "geometric" (default)
#' @param conf_int confidence interval to use; default 0.9
#' @param FilePairs_i file pairs for this iteration
#'
#' @return A list or a data.frame of PK data that optionally includes where the
#'   data came from
#' 
calc_PK_ratios_subfun <- function(
      FilePairs_i, 
      existing_exp_details,
      paired,
      match_subjects_by, 
      mean_type,
      conf_int, 
      distribution_type, 
      conc_units, 
      time_units){
   
   # Main body of function -------------------------------------------------
   
   ## Extracting PK ---------------------------------------------------------
   
   suppressWarnings(
      PKnumerator <- extractPK(
         sim_data_file = unique(FilePairs_i$Numerator_File),
         compoundToExtract = unique(FilePairs_i$Numerator_CompoundID), 
         tissue = unique(FilePairs_i$Numerator_Tissue), 
         PKparameters = unique(FilePairs_i$Numerator_PKparameter), 
         sheet = unique(FilePairs_i$Numerator_Sheet),
         existing_exp_details = existing_exp_details,
         returnExpDetails = FALSE) 
   )
   
   if(length(PKnumerator) == 0){
      warning(wrapn(paste0("We couldn't find PK values matching the requested compound ID and tissue for the numerator simulation '",
                           unique(FilePairs_i$Numerator_File), 
                           "', so we can't return any PK comparisons. If you have loaded previously saved output from extractExpDetails and supplied that here, please ensure that you have re-run extractExpDetails following any updates to the simulation results so that we can find the correct data.")), 
              call. = FALSE)
      return(data.frame())
   }
   
   suppressWarnings(
      PKdenominator <- extractPK(
         sim_data_file = unique(FilePairs_i$Denominator_File), 
         compoundToExtract = unique(FilePairs_i$Denominator_CompoundID), 
         tissue = unique(FilePairs_i$Denominator_Tissue), 
         PKparameters = unique(FilePairs_i$Denominator_PKparameter), 
         sheet = unique(FilePairs_i$Denominator_Sheet),
         existing_exp_details = existing_exp_details,
         returnExpDetails = FALSE)
   )
   
   if(length(PKdenominator) == 0){
      warning(wrapn(paste0("We couldn't find PK values matching the requested compound ID and tissue for the denominator simulation '",
                           unique(FilePairs_i$Denominator_File), 
                           "', so we can't return any PK comparisons. If you have loaded previously saved output from extractExpDetails and supplied that here, please ensure that you have re-run extractExpDetails following any updates to the simulation results so that we can find the correct data.")), 
              call. = FALSE)
      return(data.frame())
   }
   
   # For now, using the numerator existing_exp_details as the default.
   existing_exp_details_denom <- existing_exp_details %>%
      filter_sims(unique(FilePairs_i$Denominator_File), "include")
   existing_exp_details <- existing_exp_details %>% 
      filter_sims(unique(FilePairs_i$Numerator_File), "include")
   
   
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
   GoodPKparam <- FilePairs_i %>% 
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
   FilePairs_i$Denominator_PKparameterREVISED <- 
      FilePairs_i$Numerator_PKparameter
   
   PKreplace <- FilePairs_i$Denominator_PKparameterREVISED
   names(PKreplace) <- FilePairs_i$Denominator_PKparameter
   
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
      
      MyPKResults <- MyPKResults %>% 
         pivot_longer(cols = c("NumeratorSim", "DenominatorSim", "Ratio"),
                      names_to = "ValType", 
                      values_to = "Value") %>% 
         left_join(data.frame(ValType = "DenominatorSim", 
                              PKparameter = FilePairs_i$Denominator_PKparameterREVISED,
                              CorrectName = FilePairs_i$Denominator_PKparameter), 
                   by = join_by(PKparameter, ValType)) %>% 
         mutate(CorrectName = ifelse(is.na(CorrectName), PKparameter, CorrectName)) %>% 
         select(-PKparameter) %>% rename(PKparameter = CorrectName) %>% 
         group_by(PKparameter, ValType) %>% 
         summarize(
            Mean = switch(
               mean_type, 
               "geometric" = gm_mean(Value), 
               "arithmetic" = mean(Value, na.rm = T)), 
            
            CV = switch(
               mean_type, 
               "geometric" = gm_CV(Value),
               "arithmetic" = sd(Value, na.rm = T) / mean(Value, na.rm = T)),
            
            CI90_lower = switch(
               mean_type, 
               # NB: As of 2023-06-15, the default statistic for gm_conf is a t
               # statistic, which matches the output from the Simulator. -LSh
               "geometric" = gm_conf(Value, CI = conf_int, 
                                     distribution_type = distribution_type)[1],
               "arithmetic" = confInt(Value, CI = conf_int, 
                                      distribution_type = distribution_type)[1]),
            
            CI90_upper = switch(
               mean_type, 
               "geometric" = gm_conf(Value, CI = conf_int, 
                                     distribution_type = distribution_type)[2], 
               "arithmetic" = confInt(Value, CI = conf_int, 
                                      distribution_type = distribution_type)[2]), 
            
            Median = median(Value),  
            Minimum = min(Value),  
            Maximum = max(Value)) %>% 
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
      
      
   } else {
      ## Making unpaired comparisons -----------------------------------------
      
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
         
         Out <- c("Mean" = exp(LogGeomeanRatio), 
                  "CI90_lower" = exp(CI_lower_delta),
                  "CI90_upper" = exp(CI_upper_delta))
         
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
                  median(PKnumerator$individual$NumeratorSim[
                        PKnumerator$individual$PKparameter == param]) / 
                        median(PKdenominator$individual$DenominatorSim[
                           PKdenominator$individual$PKparameter == param]), 
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
                  conf_int = conf_int)
         }
      }
      
      MyPKResults <- bind_rows(MyPKResults, .id = "PKparameter")
      
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
      
      # Adding num and denom cols   
      PKnum_agg <- PKnumerator$aggregate %>%
         select(-any_of(c("File", "CompoundID", "Compound", "Inhibitor",
                          "Tissue", "Simulated", "Dose"))) %>% 
         pivot_longer(cols = -PKparameter, 
                      names_to = "Statistic",
                      values_to = "Value") %>% 
         mutate(ValType = "NumeratorSim")
      
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
         mutate(ValType = "DenominatorSim") %>% 
         left_join(FilePairs_i %>%
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
      
      MyPKResults <- MyPKResults %>% 
         bind_rows(PKnum_denom_agg) %>% 
         select(-ValType) %>% 
         pivot_wider(names_from = PKparameter,
                     values_from = Value)
      
   }
   
   Out <- list("Table" = MyPKResults %>% 
                  mutate(Numerator_Interval = unique(PKnumerator$TimeInterval$Interval), 
                         Denominator_Interval = unique(PKdenominator$TimeInterval$Interval)), 
               "QC" = bind_rows(PKnumerator$QC, PKdenominator$QC))
   
   return(Out)
   
}

