#' Check whether an AUC interval ends on a dosing interval
#'
#' @param sim_data_file the simulation output Excel file to check
#' @param existing_exp_details the output from running
#'   \code{\link{extractExpDetails_mult}} or a data.frame with the following
#'   columns: \describe{
#'
#'   \item{File}{this should include \code{sim_data_file})}
#'
#'   \item{SimDuration}{the length of the simulation in hours}
#'
#'   \item{StartHr_sub, StartHr_inhib, and StartHr_inhib2}{the
#'   first dose time in hours since the start of the simulation for the
#'   substrate, inhibitor 1, and inhibitor 2, respectively. It's ok to skip
#'   this for any compounds that are not \code{compoundID}.}
#'
#'   \item{DoseInt_sub, DoseInt_inhib, and DoseInt_inhib2}{the dosing interval for the
#'   substrate, inhibitor 1, and inhibitor 2, respectively. It's ok to skip
#'   this for any compounds that are not \code{compoundID}.}
#'
#'   \item{DoseInt_sub, DoseInt_inhib, and DoseInt_inhib2}{the dosing interval for the
#'   substrate, inhibitor 1, and inhibitor 2, respectively. It's ok to skip
#'   this for any compounds that are not \code{compoundID}.}}
#'
#' @param compoundID the specific compoundID you're interested in. Options are
#'   "substrate", "primary metabolite 1", "primary metabolite 2", "secondary
#'   metabolite", "inhibitor 1", "inhibitor 2", or "inhibitor 1 metabolite".
#' @param interval optionally include any specific intervals that you want to
#'   check. This should be a list of the intervals where each item in that list
#'   is either a text string of the start to end times in h, e.g., "36 to 48 h"
#'   (this is not at all coincidentally \emph{exactly} the format of the the
#'   text that is listed at the top of Excel sheets for custom AUC intervals in
#'   Simulator output Excel files) or a numeric vector of the start and end
#'   times, e.g., c(36, 48). An example of how to supply multiple intervals to
#'   check: \code{interval = list(c(24, 48), c(168, 180))} These will be checked
#'   in addition to the first- and last-dose intervals, which are always
#'   checked.
#' @param stop_or_warn_missing_file stop or just warn (default) if no
#'   information on the dosing interval is available. NB: This is NOT stop or
#'   warn if the dose interval doesn't match; it's only asking whether you want
#'   this to pass through to another function (choose "warn") if the file
#'   doesn't exist or completely stop.
#' @param warnings "silent" (default) or "show" to see warnings in this function
#'
#' @return list of 1) message: "custom dosing" (as in, we can't tell whether the
#'   interval matched), "good", "mismatch last dose", "mismatch user-defined
#'   interval", or "can't check - missing file" and 2) data.frame of File,
#'   CompoundID, SimDuration, DoseInt, StartHr, NumDoses, StartHr_last,
#'   IntDuration_last, OneDoseIntRemaining, UserIntervalStart,
#'   UserIntervalEnd, UserIntervalStartGood, UserIntervalEndGood. If info is not
#'   available, e.g., custom dosing or missing file, then not all of those
#'   columns will be included in the data.frame.
#'
#' @export
#' @examples
#' # none yet
check_doseint <- function(sim_data_file, 
                          existing_exp_details, 
                          compoundID = "substrate", 
                          interval = NA, 
                          stop_or_warn_missing_file = "warn", 
                          warnings = "silent"){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if(length(sim_data_file) != 1){
      stop(wrapn("You must enter 1 and only 1 value for 'sim_data_file' for checking whether the dosing interval matches the AUC interval."), 
           call. = FALSE)
   }
   
   if(length(compoundID) != 1){
      stop(wrapn("You must enter 1 and only 1 value for 'compoundID' for checking whether the dosing interval matches the AUC interval."), 
           call. = FALSE)
   }
   
   if(compoundID %in% AllRegCompounds$CompoundID == FALSE){
      warning(wrapn(paste0("Acceptable values for the argument compoundID are ", 
                           str_comma(paste0("'", AllRegCompounds$CompoundID, "'"), 
                                     conjunction = "or"), 
                           " and you have entered something else. We'll assume you want to check the AUC interval for the substrate.")), 
              call. = FALSE)
      compoundID <- "substrate"
   }
   
   if(stop_or_warn_missing_file[1] %in% c("stop", "warn") == FALSE){
      warning(wrapn("You must enter 1 and only 1 value for 'stop_or_warn_missing_file' and it must be either 'stop' or 'warn'. You have entered something else. We'll use the default value of 'warn'."), 
              call. = FALSE)
   }
   
   if(warnings[1] %in% c("silent", "show") == FALSE){
      warning(wrapn("You must enter 1 and only 1 value for 'warnings' and it must be either 'silent' or 'show'. You have entered something else. We'll use the default value of 'silent'."), 
              call. = FALSE)
   }
   
   if(length(interval) > 0 && any(complete.cases(interval))){
      if("list" %in% class(interval) == FALSE){
         warning(wrapn("You have supplied something for the argument 'interval' that is not a list, so we're not sure how to interpret what intervals you want to check. The only acceptable input for 'interval' other than NA is a list of the intervals you want. Even if you only want 1 interval, we'll still be looking for a list; it's just that it should only have 1 item in it in that case. For now, we ignore anything supplied for 'interval'."), 
                 call. = FALSE)
         interval <- NA
      }
   } else {
      interval <- NA
   }
   
   # Main body of function -----------------------------------------------------
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
                 "interval" = data.frame(File = sim_data_file, 
                                         SimDuration = NA, 
                                         CompoundID = compoundID, 
                                         DoseInt = NA, 
                                         NumDoses = NA, 
                                         Interval = NA, 
                                         StartHr = NA, 
                                         EndHr = NA, 
                                         IntDuration = NA, 
                                         IntStartMatch = NA, 
                                         IntEndMatch = NA, 
                                         IntDurationMatch = NA))
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
   
   suppressWarnings(
      IntCheck <- Deets %>% 
         mutate(across(.cols = matches("DoseInt|NumDoses|StartHr"), 
                       .fns = as.numeric),
                DoseInt = case_match(compoundID, 
                                     "substrate" ~ DoseInt_sub,
                                     "primary metabolite 1" ~ DoseInt_sub,
                                     "primary metabolite 2" ~ DoseInt_sub, 
                                     "secondary metabolite" ~ DoseInt_sub,
                                     "inhibitor 1" ~ DoseInt_inhib,
                                     "inhibitor 1 metabolite" ~ DoseInt_inhib, 
                                     "inhibitor 2" ~ DoseInt_inhib2), 
                NumDoses = case_match(compoundID, 
                                      "substrate" ~ NumDoses_sub,
                                      "primary metabolite 1" ~ NumDoses_sub,
                                      "primary metabolite 2" ~ NumDoses_sub, 
                                      "secondary metabolite" ~ NumDoses_sub,
                                      "inhibitor 1" ~ NumDoses_inhib,
                                      "inhibitor 1 metabolite" ~ NumDoses_inhib, 
                                      "inhibitor 2" ~ NumDoses_inhib2), 
                StartHr = case_match(compoundID, 
                                     "substrate" ~ StartHr_sub,
                                     "primary metabolite 1" ~ StartHr_sub,
                                     "primary metabolite 2" ~ StartHr_sub, 
                                     "secondary metabolite" ~ StartHr_sub,
                                     "inhibitor 1" ~ StartHr_inhib,
                                     "inhibitor 1 metabolite" ~ StartHr_inhib,
                                     "inhibitor 2" ~ StartHr_inhib2)) %>% 
         select(File, SimDuration, DoseInt, StartHr, NumDoses) %>% 
         mutate(
            # For calculating StartHr_last, note that it must be NumDoses - 1 b/c
            # the 1st dose doesn't start at DoseInt * 1 but at DoseInt * (1 -
            # 1) = 0.
            StartHr_last = StartHr + DoseInt * (NumDoses - 1), 
            EndHr_last = SimDuration, 
            IntDuration_last = SimDuration - StartHr_last, 
            CompoundID = compoundID) %>% 
         select(File, SimDuration, CompoundID, everything())
   )
   
   # Noting DoseInt, StartHr, and NumDoses for use later.
   DoseInt <- IntCheck$DoseInt
   StartHr <- IntCheck$StartHr
   NumDoses <- IntCheck$NumDoses
   SimDuration <- IntCheck$SimDuration
   
   # Reshaping for rest of function
   IntCheck <- IntCheck %>% 
      pivot_longer(cols = matches("StartHr|EndHr|IntDuration"), 
                   names_to = "Param", 
                   values_to = "Val") %>% 
      mutate(Interval = ifelse(str_detect(Param, "_last"), "last", "dose1"), 
             Param = str_replace(Param, "_last", "")) %>% 
      pivot_wider(names_from = Param, values_from = Val)
   
   
   # user-defined intervals ---------------------------------------------------
   
   # This will be a list where items should either be the text from the top of
   # an AUC tab in the Excel output, which is something like "36 to 48 h", or it
   # will be a length-2 numeric vector like c(36, 48).
   
   if(any(complete.cases(interval))){
      for(i in 1:length(interval)){
         
         if("character" %in% class(interval[[i]])){
            Interval <- str_split(gsub("from | h", "", interval[[i]]), pattern = " to ")
            Interval <- as.numeric(Interval[[1]])
         } else if("numeric" %in% class(interval[[i]])){
            Interval <- sort(interval[[i]][1:2])
         }
         
         IntCheck <- bind_rows(
            IntCheck, 
            data.frame(File = sim_data_file, 
                       SimDuration = {{SimDuration}}, 
                       CompoundID = compoundID, 
                       DoseInt = {{DoseInt}}, 
                       Interval = paste("from", Interval[1], "h to", Interval[2], "h"), 
                       StartHr = Interval[1], 
                       EndHr = Interval[2], 
                       IntDuration = Interval[2] - Interval[1]))
         
         rm(Interval)
      }
      
   } 
   
   
   # Putting everything together and checking things --------------------------
   
   IntCheck <- unique(IntCheck) %>% 
      mutate(IntStartMatch = StartHr %% DoseInt == 0, 
             IntEndMatch = EndHr %% DoseInt == 0, 
             IntDurationMatch = IntDuration == DoseInt | IntDuration == 0)
   
   IntCheckMessage <- c(IntStart = ifelse(any(IntCheck$IntStartMatch == FALSE), 
                                          "there's a mismatch between the start of at least one dosing interval and the AUC interval", NA), 
                        IntEnd = ifelse(any(IntCheck$IntEndMatch == FALSE), 
                                        "there's a mismatch between the end of at least one dosing interval and the AUC interval", NA), 
                        IntDuration = ifelse(any(IntCheck$IntDurationMatch == FALSE), 
                                             "there's a mismatch between the length of at least one dosing interval and the AUC interval", NA))
   
   IntCheckMessage <- ifelse(all(is.na(IntCheckMessage)),
                             "good",
                             str_comma(IntCheckMessage))
   
   if(nrow(CustomDosing > 0)){
      IntCheckMessage <- "custom dosing"
      IntCheck <- data.frame(File = sim_data_file, 
                             SimDuration = SimDuration, 
                             CompoundID = compoundID, 
                             DoseInt = NA, 
                             NumDoses = NA, 
                             Interval = NA, 
                             StartHr = NA, 
                             EndHr = NA, 
                             IntDuration = NA, 
                             IntStartMatch = NA, 
                             IntEndMatch = NA, 
                             IntDurationMatch = NA)
   }
   
   return(list("message" = IntCheckMessage, 
               "interval" = IntCheck))
   
}

