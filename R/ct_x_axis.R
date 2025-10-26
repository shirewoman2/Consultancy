#' Set up x axis in a conc-time plot
#'
#' This function is specifically for setting options for the x axis in a
#' concentration-time graph and is NOT meant to be called on its own.
#'
#' @param Data the data.frame containing conc-time data; this is the output from
#'   extractConcTime
#' @param time_range the user-supplied input for time_range
#' @param t0 the user-supplied input for t0
#' @param MyCompoundID user-requested compound to extract/graph
#' @param pad_x_axis user-supplied value for padding x axis
#' @param EnzPlot T or F for whether this is an enzyme-abundance plut
#'
#' @return values to use for ct_plots
#' 
ct_x_axis <- function(Data, time_range, t0, 
                      MyCompoundID, pad_x_axis, EnzPlot){
   
   if(all(complete.cases(time_range)) && class(time_range) == "numeric" &
      length(time_range) != 2){
      warning(wrapn("You must enter a start and stop time for 'time_range', e.g., 'time_range = c(0, 24)' or enter which dose you want. Please see the help file. For now, we'll use the full time range."),
              call. = FALSE)
      time_range <- NA
   }
   
   if(all(complete.cases(time_range)) && class(time_range) == "character" &
      length(time_range) != 1){
      time_range <- time_range[1]
   }
   
   if(all(complete.cases(time_range)) && class(time_range) == "character" &
      !any(time_range %in% c("last dose", "first dose", "penultimate dose",
                             "all obs", "all observed", "last obs", 
                             "last observed", "last to last obs", 
                             "last dose to last observed", 
                             "last to end", "last dose to end", 
                             "last day", "first day")) &
      !any(str_detect(tolower(time_range), "^dose|^day"))){
      warning(wrapn("time_range must be 'first dose', 'last dose', 'penultimate dose', dose number(s) (this option must start with 'dose'), day number(s) (this option must start with 'day'), 'last day', 'all observed', 'last observed', 'last dose to end', or a numeric time range, e.g., c(12, 24). For now, we'll use the full time range."),
              call. = FALSE)
      time_range <- NA
   }
   
   t0 <- tolower(t0)
   t0_opts <- c("simulation start", "dose 1", "last dose", "penultimate dose")
   if(t0 %in% t0_opts == FALSE){
      warning(paste0("t0 must be set to ",
                     sub("and", "or", str_comma(t0_opts)), ". For now, we'll set t0 to the start of the simulation."),
              call. = FALSE)
      t0 <- "simulation start"
   }
   
   TimeUnits <- sort(unique(Data$Time_units))
   
   # Adjusting graph labels as appropriate for the observed data
   xlab <- switch(TimeUnits,
                  "hours" = "Time (h)",
                  "minutes" = "Time (min)", 
                  "days" = "Time (days)", 
                  "weeks" = "Time (weeks)")
   
   # Setting the breaks for the x axis
   tlast <- ifelse(all(complete.cases(time_range)) &
                      length(time_range) == 2,
                   time_range[2], max(Data$Time))
   
   time_range_input <- time_range
   
   # Checking that user has provided all complete.cases for DoseNum b/c
   # otherwise next step will break.
   if(any(is.na(Data$DoseNum)) & class(time_range_input) == "character"){
      warning(wrapn("You have missing values in your data for the dose number, so we cannot set the x axis in your graph based on this. We will give you the full time range instead."), 
              call. = FALSE)
      time_range <- as.logical(NA)
   }
   
   if(all(complete.cases(time_range)) &&
      class(time_range_input) == "character" | t0 != "simulation start"){
      
      if(complete.cases(time_range) && str_detect(time_range, "first dose")){
         # Things work most consistently if "first dose" becomes "dose 1". 
         time_range <- sub("first dose", "dose 1", time_range)
         time_range_input <- "dose 1"
      }
      
      if(complete.cases(time_range) && str_detect(time_range, "first day")){
         # Things work most consistently if "first day" becomes "day 1". 
         time_range <- sub("first day", "day 1", time_range)
         time_range_input <- "day 1"
      }
      
      if(EnzPlot){
         if(str_detect(time_range_input, "substrate")){
            time_range <- str_trim(sub("substrate", "", time_range))
            Data <- Data %>% rename(DoseNum = DoseNum_sub)
         }
         
         if(str_detect(time_range_input, "inhibitor 1")){
            time_range <- sub(" inhibitor 1", "", time_range)
            Data <- Data %>% rename(DoseNum = DoseNum_inhib1)
         }
         
         if(str_detect(time_range_input, "inhibitor 2")){
            time_range <- sub(" inhibitor 2", "", time_range)
            Data <- Data %>% rename(DoseNum = DoseNum_inhib2)
         }
      } 
      
      SingleDose <- Data %>% filter(DoseNum > 0) %>% pull(DoseNum) %>%
         unique()
      SingleDose <- length(SingleDose) == 1 && SingleDose == 1
      
      # Checking for multiple dosing intervals for the same time range (not
      # checking for whether this is multiple dose)
      MultDoseInt <- 
         Data %>% group_by(File, Compound, CompoundID, Inhibitor, DoseNum) %>% 
         summarize(DoseTime = round(min(Time))) %>%
         ungroup() %>% group_by(DoseNum) %>% 
         summarize(UniqDT = length(unique(DoseTime))) %>% 
         pull(UniqDT) 
      MultDoseInt <- any(MultDoseInt != 1)
      
      # Also have problems when something failed and there are no dose numbers
      # in the data.
      NoDoseNum <- SingleDose == FALSE & 
         any(complete.cases(Data$DoseNum)) == FALSE
      
      if(MultDoseInt == FALSE & NoDoseNum == FALSE){
         if(SingleDose){
            DoseTimes <- data.frame(
               FirstDoseStart = Data %>%
                  filter(CompoundID == MyCompoundID & 
                            DoseNum == 1) %>% 
                  summarize(Min = min(Time)) %>% pull(floor(Min)), 
               
               FirstDoseEnd = ceiling(max(Data$Time[Data$CompoundID == MyCompoundID])),
               
               PenultDoseStart = floor(min(Data$Time[Data$DoseNum == 1 &
                                                        Data$CompoundID == MyCompoundID])),
               LastDoseStart = floor(min(Data$Time[Data$DoseNum == 1 &
                                                      Data$CompoundID == MyCompoundID])))
         } else {
            DoseTimes1 <- Data %>% 
               filter(DoseNum > 0 & CompoundID == MyCompoundID) %>%
               group_by(DoseNum) %>%
               summarize(t0 = round(min(Time)), # Is it safe to round this to the nearest unit? Probably ok but may need to adjust this later.
                         tlast = ceiling(max(Time))) %>% 
               ungroup()
            
            MaxNumDoses <- max(Data$DoseNum[Data$CompoundID == MyCompoundID])
            DoseTimes <- data.frame(FirstDoseStart = DoseTimes1 %>% 
                                       filter(DoseNum == 1) %>% pull(t0),
                                    FirstDoseEnd = DoseTimes1 %>% 
                                       filter(DoseNum == 1) %>% pull(tlast),
                                    PenultDoseStart = DoseTimes1 %>% 
                                       filter(DoseNum == MaxNumDoses - 1) %>% pull(t0),
                                    LastDoseStart = DoseTimes1 %>% 
                                       filter(DoseNum == MaxNumDoses) %>% pull(t0))
            
            DoseDays <- Data %>% 
               mutate(Day = Time %/% 24 + 1) %>% 
               filter(CompoundID == MyCompoundID) %>%
               group_by(Day) %>%
               summarize(t0 = round(min(Time)), 
                         tlast = ceiling(max(Time))) %>% 
               ungroup() %>% 
               # removing from consideration the last time point if it's the
               # ONLY time point for that day, which can often happen
               filter(t0 != tlast)
            
         }
         
         if(all(complete.cases(time_range_input)) && 
            str_detect(time_range_input, "first dose|dose 1$")){
            time_range <- c(0, DoseTimes$FirstDoseEnd)
         }
         
         if(all(complete.cases(time_range_input)) &&
            str_detect(time_range_input, "penultimate dose")){
            if(SingleDose){
               time_range <-
                  c(DoseTimes$LastDoseStart, max(Data$Time))
            } else {
               time_range <-
                  DoseTimes %>% ungroup() %>%
                  select(PenultDoseStart, LastDoseStart) %>%
                  t() %>% as.numeric()
            }
         }
         
         if(SingleDose & 
            any(time_range_input %in% c("last dose", "penultimate dose"))){
            warning(paste0("You requested the ", time_range_input,
                           ", but the compound was administered as a single dose. The graph x axis will cover the substrate administration time until the end of the simulation."),
                    call. = FALSE)
         }
         
         if(all(complete.cases(time_range_input)) &&
            str_detect(time_range_input, "last dose")){
            time_range <- c(DoseTimes$LastDoseStart, max(Data$Time))
         }
         
         if(all(complete.cases(time_range_input)) &&
            str_detect(time_range_input, "last day")){
            time_range <- DoseDays %>% 
               filter(Day == max(Day)) %>% 
               select(t0, tlast) %>% t() %>% as.numeric()
         }
         
         if(all(complete.cases(time_range_input)) && 
            str_detect(tolower(time_range_input), "^dose")){
            
            if(str_detect(time_range_input, "to|-")){
               
               DoseNumToPull <- as.numeric(
                  str_trim(str_split(
                     gsub("dose(s)?|substrate|inhibitor [12]", "", 
                          time_range_input), "to|-")[[1]]))
               
            } else {
               
               DoseNumToPull <- as.numeric(
                  str_trim(gsub("dose(s)?|substrate|inhibitor [12]", "", 
                                time_range_input)))
               
            }
            
            if(any(DoseNumToPull < min(Data$DoseNum) |
                   DoseNumToPull > max(Data$DoseNum))){
               warning(paste0("You requested ", 
                              time_range_input, ", but that is outside the number of doses administered. The full time range will be returned."),
                       call. = FALSE)
               time_range <- range(Data$Time)
            } else {
               
               time_range <- Data %>% 
                  filter(DoseNum %in% DoseNumToPull &
                            CompoundID == MyCompoundID) %>% 
                  summarize(Min = round(min(Time)), 
                            Max = ceiling(max(Time))) %>% 
                  t() %>% as.numeric()
            }
         }
         
         if(all(complete.cases(time_range_input)) && 
            str_detect(tolower(time_range_input), "^day")){
            
            time_range <- convert_days_to_hours(time_range_input)
            
         }
         
         if(all(complete.cases(time_range_input)) &&
            time_range_input %in% c("last obs", "last observed",
                                    "last to last obs", "last dose to last observed")){
            suppressWarnings(
               time_range <- Data %>% 
                  filter(Simulated == FALSE & DoseNum == MaxNumDoses) %>% 
                  pull(Time) %>% range()
            )
            
            time_range[1] <- DoseTimes$LastDoseStart
            
            if(any(is.infinite(time_range))){
               warning("You requested 'last observed' or 'last dose to last observed' for the time range, but your data do not include any observed data in that time frame. The full time range will be returned.\n",
                       call. = FALSE)
               time_range <- Data %>% pull(Time) %>% range()
            }
         } else if(all(complete.cases(time_range_input)) &&
                   time_range_input %in% c("last dose to end", "last to end")){ 
            suppressWarnings(
               time_range <- Data %>% 
                  filter(DoseNum == MaxNumDoses) %>% 
                  pull(Time) %>% range()
            )
            
            time_range[1] <- DoseTimes$LastDoseStart
            
         }
         
      } else if(MultDoseInt){
         
         # Multiple compound scenario here
         if(any(str_detect(time_range_input, "dose|last obs"))){
            warning(paste0("You requested the ", time_range_input,
                           ", but this is a graph of multiple compounds, which may have different dose intervals. The graph x axis will cover the full time range of the simulation.\n"),
                    call. = FALSE)
            time_range <- Data %>% pull(Time) %>% range()
         }
      } else if(NoDoseNum){
         warning("You appear to have a multiple-dose simulation, but something has gone wrong and there are no dose numbers in your data. The graph x axis will cover the full time range of the simulation.\n", 
                 call. = FALSE)
         time_range <- Data %>% pull(Time) %>% range()
      }
      
      if(all(complete.cases(time_range_input)) &&
         str_detect(time_range_input, "all obs")){
         suppressWarnings(
            time_range <- Data %>% filter(Simulated == FALSE) %>% 
               pull(Time) %>% range()
         )
         if(any(is.infinite(time_range))){
            warning("You requested 'all observed' for the time range, but your data do not include any observed data. The full time range will be returned.",
                    call. = FALSE)
            time_range <- Data %>% pull(Time) %>% range()
         }
      }
   }
   
   # Setting the time range if it's NA
   if(all(is.na(time_range_input)) | all(is.na(time_range))){
      
      TEMP_range <- switch(as.character(EnzPlot), 
                           "TRUE" = range(Data$Time, na.rm = T),
                           "FALSE" = range(Data$Time[complete.cases(Data$Conc)], na.rm = T))
      
      time_range <- c(round_down_unit(TEMP_range[1], 1), 
                      round_up_unit(TEMP_range[2], 1))
      
   }
   
   # If t0 isn't "simulation start", need to adjust x axis.
   if(t0 != "simulation start"){
      
      t0_num <- switch(
         t0,
         "dose 1" = DoseTimes$FirstDoseStart,
         "last dose" = DoseTimes$LastDoseStart,
         "penultimate dose" = DoseTimes$PenultDoseStart)
      
      Data$Time_orig <- Data$Time
      Data$Time <- Data$Time - t0_num
      time_range_relative <- time_range - t0_num
   } else {
      t0_num <- 0
      Data$Time_orig <- Data$Time
      time_range_relative <- time_range
   }
   
   Out <- list("xlab" = xlab, 
               "Data" = Data, 
               "time_range" = time_range, 
               "time_range_relative" = time_range_relative,
               "t0" = t0_num, 
               "TimeUnits" = TimeUnits)
   return(Out)
   
}    


