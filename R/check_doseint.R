#' INTERNAL check whether AUC interval ends on a dosing interval
#'
#' @param sim_data_file sim_data_file
#' @param existing_exp_details existing_exp_details
#' @param compoundID compoundID
#' @param interval optionally include a specific interval that you want to check
#' @param stop_or_warn_missing_file stop or just warn if no info available. NB:
#'   This is NOT stop or warn if the dose interval dosen't match.
#' @param warnings "silent" (default) or "show" (really, or anything else) to
#'   see warnings in this function
#'
#' @return list of 1) message: "custom dosing" (as in, we can't tell whether the
#'   interval matched), "good", "mismatch last dose", "mismatch user-defined
#'   interval", or "can't check - missing file" and 2) data.frame of File,
#'   CompoundID, SimDuration, DoseInt_X, StartHr_X, NumDoses_X, LastDoseTime,
#'   IntervalRemaining, OneDoseIntRemaining, UserIntervalStart, UserIntervalEnd,
#'   UserIntervalStartGood, UserIntervalEndGood. If info is not available, e.g.,
#'   custom dosing or missing file, then not all of those columns will be
#'   included in the data.frame.
#'
#' @examples
#' # none yet
check_doseint <- function(sim_data_file, 
                          existing_exp_details, 
                          compoundID, 
                          interval = NA, 
                          stop_or_warn_missing_file = "stop", 
                          warnings = "silent"){
   
   Deets <- harmonize_details(existing_exp_details)
   Deets <- filter_sims(Deets, sim_data_file, "include")
   
   if("CompoundID" %in% names(Deets$CustomDosing)){
      CustomDosing <- Deets$CustomDosing %>% filter(CompoundID == compoundID)
   } else {
      CustomDosing <- data.frame()
   }
   Deets <- Deets$MainDetails
   
   if(nrow(Deets) == 0){
      if(stop_or_warn_missing_file == "stop"){
         stop("sim_data_file is not included in existing_exp_details. We cannot check whether the dose interval is correct.", 
              call. = FALSE)   
      } else {
         warning("sim_data_file is not included in existing_exp_details. We cannot check whether the dose interval is correct.", 
                 call. = FALSE)   
         return(
            list("message" = "can't check - missing file", 
                 "interval" = data.frame(DoseInt_X = NA, 
                                         NumDoses_X = NA, 
                                         StartHr_X = NA, 
                                         File = sim_data_file))
         )
      }
   }
   
   # Adding some NA values to Deets as needed for the next bit to
   # work w/out generating a ton of warnings.
   MissingCols <- setdiff(paste0(rep(c("DoseInt", "StartHr", 
                                       "NumDoses"), each = 3), 
                                 c("_sub", "_inhib", "_inhib2")), 
                          names(Deets))
   
   if(length(MissingCols) > 0){
      Deets <- Deets %>% 
         bind_cols(as.data.frame(matrix(data = NA, 
                                        ncol = length(MissingCols),
                                        dimnames = list(NULL, MissingCols))))
   }
   
   # Dealing with custom dosing
   if(nrow(CustomDosing > 0)){
      
      return(
         list("message" = "custom dosing", 
              "interval" = data.frame(DoseInt_X = NA, 
                                      NumDoses_X = NA, 
                                      StartHr_X = NA, 
                                      File = sim_data_file))
      )
   }
   
   IntCheck <- Deets %>% 
      mutate(across(.cols = matches("DoseInt|NumDoses|StartHr"), 
                    .fns = as.numeric),
             DoseInt_X = case_match(compoundID, 
                                    "substrate" ~ DoseInt_sub,
                                    "primary metabolite 1" ~ DoseInt_sub,
                                    "primary metabolite 2" ~ DoseInt_sub, 
                                    "secondary metabolite" ~ DoseInt_sub,
                                    "inhibitor 1" ~ DoseInt_inhib,
                                    "inhibitor 1 metabolite" ~ DoseInt_inhib, 
                                    "inhibitor 2" ~ DoseInt_inhib2), 
             NumDoses_X = case_match(compoundID, 
                                     "substrate" ~ NumDoses_sub,
                                     "primary metabolite 1" ~ NumDoses_sub,
                                     "primary metabolite 2" ~ NumDoses_sub, 
                                     "secondary metabolite" ~ NumDoses_sub,
                                     "inhibitor 1" ~ NumDoses_inhib,
                                     "inhibitor 1 metabolite" ~ NumDoses_inhib, 
                                     "inhibitor 2" ~ NumDoses_inhib2), 
             StartHr_X = case_match(compoundID, 
                                    "substrate" ~ StartHr_sub,
                                    "primary metabolite 1" ~ StartHr_sub,
                                    "primary metabolite 2" ~ StartHr_sub, 
                                    "secondary metabolite" ~ StartHr_sub,
                                    "inhibitor 1" ~ StartHr_inhib,
                                    "inhibitor 1 metabolite" ~ StartHr_inhib,
                                    "inhibitor 2" ~ StartHr_inhib2)) %>% 
      select(File, SimDuration, DoseInt_X, StartHr_X, NumDoses_X) %>% 
      mutate(
         # For calculating LastDoseTime, note that it must be NumDoses_X - 1 b/c
         # the 1st dose doesn't start at DoseInt_x * 1 but at DoseInt_x * (1 -
         # 1) = 0.
         LastDoseTime = StartHr_X + DoseInt_X * (NumDoses_X - 1), 
         IntervalRemaining = SimDuration - LastDoseTime, 
         # If things were set up correctly, then there should be one
         # dosing interval of time left when the last dose is
         # administered OR there should be no time left.
         OneDoseIntRemaining = IntervalRemaining == DoseInt_X |
            IntervalRemaining == 0, 
         CompoundID = compoundID, 
         EndsOnLastDose = IntervalRemaining == 0 & OneDoseIntRemaining == TRUE, 
         LastDoseTime = ifelse(EndsOnLastDose, 
                               StartHr_X + (NumDoses_X - 1) * DoseInt_X, 
                               LastDoseTime), 
         NumDoses_X = ifelse(EndsOnLastDose, 
                             NumDoses_X - 1, NumDoses_X), 
         IntervalRemaining = ifelse(EndsOnLastDose, 
                                    DoseInt_X, IntervalRemaining)) %>% 
      select(-EndsOnLastDose) %>% 
      select(File, CompoundID, everything())
   
   # DoseInt_x will be NA when it's a single-dose simulation, in which case,
   # the dosing interval is fine.
   IntCheckMessage <- ifelse(IntCheck$OneDoseIntRemaining == TRUE |
                                is.na(IntCheck$DoseInt_X), 
                             "good", "mismatch last dose")
   
   
   # user-defined intervals ---------------------------------------------------
   
   # Not adding much error catching here since this is an internal function.
   # This should either be the text from the top of an AUC tab in the Excel
   # output, which is something like "36 to 48 h", or it will be a length-2
   # numeric vector like c(36, 48).
   
   if(any(complete.cases(interval))){
      if("character" %in% class(interval)){
         Interval <- str_split(gsub("from | h", "", interval), pattern = " to ")
         Interval <- as.numeric(Interval[[1]])
      } else if("numeric" %in% class(interval)){
         Interval <- sort(interval[1:2])
      }
      
      IntGood <- Interval %% IntCheck$DoseInt_X == 0
      MsgToAdd <- switch(str_c(IntGood, collapse = " "), 
                         "TRUE FALSE" = "The user-defined AUC interval starts on a dosing time but ends at a different time than the dosing interval.\n", 
                         "TRUE TRUE" = "", 
                         "FALSE TRUE" = "The user-defined AUC interval does not start on a dosing time (although it does end on one).\n", 
                         "FALSE FALSE" = "The user-defined AUC interval starts and ends at times other than dosing intervals.\n")
      
      IntCheck <- IntCheck %>% 
         mutate(UserIntervalStart = Interval[1], 
                UserIntervalEnd = Interval[2], 
                UserIntervalStartGood = IntGood[1], 
                UserIntervalEndGood = IntGood[2], 
                UserIntervalRemaining = Interval[2] - Interval[1])
      
      # If they supplied a user-defined interval, that's clearly important, so
      # modify the message based on whether that's ok.
      IntCheckMessage <- ifelse(
         complete.cases(IntCheck$UserIntervalEndGood) &&
            all(c(IntCheck$UserIntervalStartGood, 
                  IntCheck$UserIntervalEndGood)) == FALSE, 
         "mismatch user-defined interval", "good")
      
      if(IntCheckMessage == "mismatch user-defined interval" & warnings != "silent"){
         warning("The time used for integrating the AUC for the user-defined interval was not the same as the dosing interval.\n", 
                 call. = FALSE)
      }
      
   } else {
      IntCheck <- IntCheck %>% 
         mutate(UserIntervalStart = as.numeric(NA), 
                UserIntervalEnd = as.numeric(NA), 
                UserIntervalStartGood = as.logical(NA), 
                UserIntervalEndGood = as.logical(NA))
      
      if(IntCheckMessage == "mismatch last dose" & warnings != "silent"){
         warning("The time used for integrating the AUC for the last dose was not the same as the dosing interval.\n", 
                 call. = FALSE)
      }
   }
   
   return(list("message" = IntCheckMessage, 
               "interval" = IntCheck))
   
}

