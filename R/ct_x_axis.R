#' Set up x axis in a conc-time plot
#'
#' This function is specifically for setting options for the x axis in a
#' concentration-time graph and is NOT meant to be called on its own.
#'
#' @param Data the data.frame containing conc-time data; this is the output from
#'   extractConcTime
#' @param time_range the user-supplied input for time_range
#' @param t0 the user-supplied input for t0
#' @param x_axis_interval x axis interval to use
#' @param compoundToExtract user-requested compound to extract/graph
#' @param pad_x_axis user-supplied value for padding x axis
#' @param EnzPlot T or F for whether this is an enzyme-abundance plut
#'
#' @return
#' 
ct_x_axis <- function(Data, time_range, t0, x_axis_interval,
                      compoundToExtract, pad_x_axis, EnzPlot){
    
    if(all(complete.cases(time_range)) && class(time_range) == "numeric" &
       length(time_range) != 2){
        stop("You must enter a start and stop time for 'time_range', e.g., 'c(0, 24)' or enter 'last dose' to plot only the time range for the last dose.")
    }
    
    if(all(complete.cases(time_range)) && class(time_range) == "numeric" &
       time_range[1] >= time_range[2]){
        stop("The 1st value for 'time_range' must be less than the 2nd value.")
    }
    
    if(all(complete.cases(time_range)) && class(time_range) == "character" &
       length(time_range != 1)){
        time_range <- time_range[1]
    }
    
    if(all(complete.cases(time_range)) && class(time_range) == "character" &
       !any(time_range %in% c("last dose", "first dose", "penultimate dose")) &
       !any(str_detect(tolower(time_range), "^dose"))){
        stop("time_range must be 'first dose', 'last dose', 'penultimate dose', dose number(s) (this option must start with 'dose'), or a numeric time range, e.g., c(12, 24).")
    }
    
    t0 <- tolower(t0)
    t0_opts <- c("simulation start", "dose 1", "last dose", "penultimate dose")
    if(t0 %in% t0_opts == FALSE){
        stop(paste0("t0 must be set to ",
                    sub("and", "or", str_comma(t0_opts)), "."))
    }
    
    TimeUnits <- sort(unique(Data$Time_units))
    
    # A little more error catching
    if(all(complete.cases(time_range) & class(time_range) == "numeric") &
       (any(time_range < min(Data$Time[Data$Simulated == TRUE])) |
        any(time_range > max(Data$Time[Data$Simulated == TRUE])))){
        stop(paste0(
            "Both the values entered for the time range must be within the range of time simulated. The range of time in your simulation was ",
            min(Data$Time[Data$Simulated == TRUE]), " to ",
            max(Data$Time[Data$Simulated == TRUE]), " ", TimeUnits, "."))
    }
    
    # Adjusting graph labels as appropriate for the observed data
    xlab <- switch(TimeUnits,
                   "hours" = "Time (h)",
                   "minutes" = "Time (min)")
    
    # Setting the breaks for the x axis
    tlast <- ifelse(all(complete.cases(time_range)) &
                        length(time_range) == 2,
                    time_range[2], max(Data$Time))
    
    time_range_input <- time_range
    
    if(class(time_range_input) == "character" | t0 != "simulation start"){
        
        if(complete.cases(time_range) && str_detect(time_range, "dose 1$")){
            time_range <- sub("dose 1", "first dose", time_range)
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
        
        if(SingleDose){
            DoseTimes <- data.frame(
                FirstDoseStart = Data %>%
                    filter(CompoundID == compoundToExtract & 
                               DoseNum == 1) %>% 
                    summarize(Min = min(Time)) %>% pull(Min) %>% floor(), 
                
                FirstDoseEnd = ceiling(max(Data$Time[Data$CompoundID == compoundToExtract])),
                
                PenultDoseStart = floor(min(Data$Time[Data$DoseNum == 1 &
                                                          Data$CompoundID == compoundToExtract])),
                LastDoseStart = floor(min(Data$Time[Data$DoseNum == 1 &
                                                        Data$CompoundID == compoundToExtract])))
        } else {
            DoseTimes1 <- Data %>% 
                filter(DoseNum > 0 & CompoundID == compoundToExtract) %>%
                group_by(DoseNum) %>%
                summarize(t0 = round(min(Time)), # Is it safe to round this to the nearest unit? Probably ok but may need to adjust this later.
                          tlast = ceiling(max(Time))) %>% 
                ungroup()
            
            MaxNumDoses <- max(Data$DoseNum[Data$CompoundID == compoundToExtract])
            
            DoseTimes <- 
                data.frame(
                    FirstDoseStart = DoseTimes1 %>% 
                        filter(DoseNum == 1) %>% pull(t0),
                    FirstDoseEnd = DoseTimes1 %>% 
                        filter(DoseNum == 1) %>% pull(tlast),
                    PenultDoseStart = DoseTimes1 %>% 
                        filter(DoseNum == MaxNumDoses - 1) %>% pull(t0),
                    LastDoseStart = DoseTimes1 %>% 
                        filter(DoseNum == MaxNumDoses) %>% pull(t0))
        }
        
        if(SingleDose & time_range_input %in% c("last dose", "penultimate dose")){
            warning(paste0("You requested the ", time_range_input,
                           ", but the substrate was administered as a single dose. The graph x axis will cover the substrate administration time until the end of the simulation."))
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
        
        if(all(complete.cases(time_range_input)) &&
           str_detect(time_range_input, "last dose")){
            time_range <- c(DoseTimes$LastDoseStart, max(Data$Time))
        }
        
        if(all(complete.cases(time_range_input)) && 
           str_detect(tolower(time_range_input), "^dose")){
            
            if(str_detect(time_range_input, "to")){
                
                DoseNumToPull <- as.numeric(
                    str_trim(str_split(
                        gsub("dose(s)?|substrate|inhibitor [12]", "", 
                             time_range_input), "to")[[1]]))
                
            } else {
                
                DoseNumToPull <- as.numeric(
                    str_trim(gsub("dose(s)?|substrate|inhibitor [12]", "", 
                                  time_range_input)))
                
            }
            
            if(any(DoseNumToPull < min(Data$DoseNum) |
                   DoseNumToPull > max(Data$DoseNum))){
                warning(paste0("You requested ", 
                               time_range_input, ", but that is outside the number of doses administered. The full time range will be returned."))
                time_range <- range(Data$Time)
            } else {
                
                time_range <- Data %>% 
                    filter(DoseNum %in% DoseNumToPull &
                               CompoundID == compoundToExtract) %>% 
                    summarize(Min = round(min(Time)), 
                              Max = ceiling(max(Time))) %>% 
                    t() %>% as.numeric()
            }
        }
    }
    
    # Setting the x axis intervals using tlast doesn't work well if the time
    # range starts at something other than 0 or ends somewhere other than the
    # max time, so adjusting for that situation.
    if(all(complete.cases(time_range)) &
       (time_range[1] != 0 | time_range[2] != max(Data$Time))){
        
        tlast <- time_range[2] - time_range[1]
        LastDoseTime <- time_range[1]
        
    }
    
    # Setting the time range if it's not already set since we use it later.
    if(is.na(time_range_input[1])){
        time_range <- range(Data$Time, na.rm = T)
    }
    
    # If tlast is just a smidge over one of the possible breaks I've set, it
    # goes to the next one and doesn't look as nice on the graph. Rounding
    # tlast down to the nearest 4 for hours and nearest 15 for minutes.
    tlast <- ifelse(TimeUnits == "hours",
                    round_down_unit(tlast, 4),
                    round_down_unit(tlast, 15))
    
    if(TimeUnits == "hours"){
        
        PossBreaks <- data.frame(
            Tlast = c(12, 24, 48, 96, 168, 336, 360, 504, 672, Inf),
            BreaksToUse = c("12hr", "24hr", "48hr", "96hr", "1wk", "2wk",
                            "15d", "3wk", "4wk", "4wkplus"))
        
        BreaksToUse <- PossBreaks %>% filter(Tlast >= tlast) %>%
            slice(which.min(Tlast)) %>% pull(BreaksToUse)
        
        BreaksToUse <- ifelse(complete.cases(x_axis_interval),
                              "UserDefined", BreaksToUse)
        
        XBreaks <- switch(BreaksToUse,
                          "12hr" = seq(0, 12, 1),
                          "24hr" = seq(0, 24, 2),
                          "48hr" = seq(0, 48, 4),
                          "96hr" = seq(0, 96, 6),
                          "1wk" = seq(0, 168, 12),
                          "2wk" = seq(0, 336, 24),
                          "15d" = seq(0, 360, 24),
                          "3wk" = seq(0, 504, 36),
                          "4wk" = seq(0, 672, 48),
                          "4wkplus" = round_up_nice(seq(0, tlast,
                                                        length.out = 12)),
                          "UserDefined" = seq(0, max(Data$Time, na.rm = T),
                                              x_axis_interval/2))
        
    }
    
    if(TimeUnits == "minutes"){
        PossBreaks <- data.frame(Tlast = c(60, 240, 480, 720, 1440, Inf),
                                 BreaksToUse = c("1hr", "4hr",
                                                 "8hr", "12hr",
                                                 "24hr", "24hrplus"))
        
        BreaksToUse <- PossBreaks %>% filter(Tlast >= tlast) %>%
            slice(which.min(Tlast)) %>% pull(BreaksToUse)
        
        BreaksToUse <- ifelse(complete.cases(x_axis_interval),
                              "UserDefined", BreaksToUse)
        
        XBreaks <- switch(BreaksToUse,
                          "1hr" = seq(0, 60, 7.5),
                          "4hr" = seq(0, 240, 15),
                          "8hr" = seq(0, 480, 30),
                          "12hr" = seq(0, 720, 60),
                          "24hr" = seq(0, 1440, 120),
                          "24hrplus" = round_up_nice(seq(0, tlast,
                                                         length.out = 12)),
                          "UserDefined" = seq(0, max(Data$Time, na.rm = T),
                                              x_axis_interval/2))
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
        XBreaks <- XBreaks - t0_num
        time_range_relative <- time_range - t0_num
    } else {
        Data$Time_orig <- Data$Time
        time_range_relative <- time_range
    }
    
    # Adjusting the breaks when time_range[1] isn't 0
    if(all(complete.cases(time_range)) & time_range[1] != 0){
        XBreaks <- XBreaks + LastDoseTime
    }
    
    XLabels <- XBreaks
    XLabels[seq(2,length(XLabels),2)] <- ""
    XLabels[which(XBreaks == 0)] <- "0"
    
    # Adding padding if user requests it
    pad_x_num <- ifelse(class(pad_x_axis) == "logical",
                        ifelse(pad_x_axis, 0.02, 0), 
                        pad_x_axis)
    pad_x_axis <- pad_x_num != 0 # Making pad_x_axis logical again to work with code elsewhere
    
    # Assigning the variables created or changed here to the environment one
    # level up, e.g., probably the environment within the function that's
    # calling on *this* function.
    assign("pad_x_axis", pad_x_axis, envir = parent.frame())
    assign("pad_x_num", pad_x_num, envir = parent.frame())
    assign("XLabels", XLabels, envir = parent.frame())
    assign("XBreaks", XBreaks, envir = parent.frame())
    assign("xlab", xlab, envir = parent.frame())
    assign("Data", Data, envir = parent.frame())
    assign("time_range", time_range, envir = parent.frame())
    assign("time_range_input", time_range_input, envir = parent.frame())
    assign("TimeUnits", TimeUnits, envir = parent.frame())
    assign("time_range_relative", time_range_relative, envir = parent.frame())
}    
    