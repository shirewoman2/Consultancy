#' Set up y axis in a conc-time plot
#'
#' This function is specifically for setting options for the y axis in a
#' concentration-time graph and is NOT meant to be called on its own.
#'
#' @param ADAMorAdvBrain T or F for whether ADAM or advanced brain model used
#' @param Tissue_subtype which ADAM-model or advanced-brain-model subsection
#'   the data include
#' @param EnzPlot T or F for whether this was a plot of enzyme abundance
#' @param y_axis_limits_lin user input for y axis limits for a linear plot
#' @param y_axis_limits_log user input for y axis limits for a semi-log plot
#' @param y_axis_interval user input for y axis tick-mark interval
#' @param time_range user-specified time range
#' @param Ylim_data data used for determining y axis limits
#' @param pad_y_axis user-specified value for pad_y_axis
#' @param time_range_relative relative time range
#' @param prettify_compound_names prettify? 
#' @param normalize_by_dose T or F for whether to normalize concs by dose
#'
#' @return values to use for ct_plots

ct_y_axis <- function(ADAMorAdvBrain, 
                      Tissue_subtype, 
                      EnzPlot, 
                      y_axis_limits_lin, 
                      time_range,
                      y_axis_interval = NA,
                      prettify_compound_names = TRUE, 
                      normalize_by_dose = FALSE, 
                      y_axis_limits_log, Ylim_data, pad_y_axis,
                      time_range_relative){
   
   if(EnzPlot){
      ObsConcUnits <- "Relative abundance"
   } else {
      ObsConcUnits <- sort(unique(Ylim_data$Conc_units))
   }
   
   if(any(ADAMorAdvBrain, na.rm = TRUE)){
      
      # # ADAM options available (this is for my reference and was copied from ct_plot.R)
      # ADAMoptions <- c("undissolved compound", "enterocyte concentration",
      #                  "free compound in lumen", "total compound in lumen",
      #                  "Heff", "absorption rate",
      #                  "unreleased compound in faeces", 
      #                  "dissolved compound", "luminal CLint", 
      #                  "dissolution rate of solid state", 
      #                  "cumulative fraction of compound absorbed", 
      #                  "cumulative fraction of compound dissolved")
      
      if(class(prettify_compound_names) == "logical"){
         CompoundLab <- ifelse(prettify_compound_names == TRUE, 
                               prettify_compound_name(unique(Ylim_data$Compound)), 
                               unique(Ylim_data$Compound))
      } else {
         CompoundLab <- str_comma(
            unique(
               prettify_compound_names[
                  prettify_compound_names %in%
                     unique(Ylim_data$Compound[Ylim_data$CompoundID == "substrate"])]), 
            conjunction = "or")
      }
      
      if(length(unique(Tissue_subtype)) == 1 & 
         length(unique(Ylim_data$Conc_units)) == 1){
         
         ylab1 <- 
            switch(Tissue_subtype, 
                   
                   ## ADAM 
                   "dissolved compound" = 
                      paste0("Dissolved ", CompoundLab, "\nin ", unique(Ylim_data$Tissue)),
                   "undissolved compound" = 
                      paste0("Undissolved ", CompoundLab, "\nin ", unique(Ylim_data$Tissue)),
                   "enterocyte concentration" = 
                      paste("Enterocyte concentration\nof", CompoundLab),
                   "free compound in lumen" = 
                      paste0("Free ", CompoundLab, " in lumen"),
                   "total compound in lumen" = 
                      paste0("Total ", CompoundLab, " in lumen"),
                   "Heff" = expression("Particle"~H[eff]~(mu*m)), 
                   "absorption rate" = 
                      paste0("Absorption rate of ", CompoundLab, "\nin ", unique(Ylim_data$Tissue)),
                   "unreleased compound in faeces" = 
                      paste0("Unreleased ", CompoundLab, "\nin faeces"),
                   "luminal CLint" = expression("Luminal"~CL[int]), 
                   "dissolution rate of solid state" = 
                      paste0("Dissolution rate of ", CompoundLab, "\nin ", unique(Ylim_data$Tissue)), 
                   "cumulative fraction of compound absorbed" =
                      paste0("Cumulative fraction of\n", CompoundLab, " absorbed"), 
                   "cumulative fraction of compound dissolved" =
                      paste0("Cumulative fraction of\n", CompoundLab, " dissolved"), 
                   "cumulative fraction of compound released" = 
                      paste0("cumulative fraction of\n", CompoundLab, " released"), 
                   
                   ## AdvBrainModel
                   "intracranial" = "Intracranial\nconcentration",
                   "brain ICF" = "Brain intracellular fluid\nconcentration", 
                   "brain ISF" = "Brain intrastitial fluid\nconcentration",
                   "spinal CSF" = "Spinal cerebrospinal fluid\nconcentration", 
                   "cranial CSF" = "Cranial cerebrospinal fluid\nconcentration",
                   "total brain" = "Total brain\nconcentration",
                   "Kp,uu,brain" = expression(K[p*",uu,brain"]), #"Unbound-brain-to-unbound-plasma\ntissue partition coefficient", 
                   "Kp,uu,ICF" = expression(K[p*",uu,intracellular fluid"]), # "Unbound-intracellular-fluid-to-unbound-plasma\ntissue partition coefficient", 
                   "Kp,uu,ISF" = expression(K[p*",uu,interstitial fluid"]) #"Unbound-intrastitial-fluid-to-unbound-plasma\ntissue partition coefficient") 
            )
         
         ylab2 <- ifelse(is.na(unique(Ylim_data$Conc_units)), 
                         "", paste0("(", unique(Ylim_data$Conc_units), ")"))
         
         if("expression" %in% class(ylab1) | is.na(ylab2)){
            ylab <- ylab1
         } else {
            ylab <- paste(ylab1, ylab2)
         }
      } else {
         # This is when it IS AdvBrain or ADAM model data but there's more than
         # one type.
         ylab1 <- NA
         ylab <- "Advanced-brain-model or ADAM-model amount"
      }
      
   } else {
      # This is when it is NOT AdvBrain or ADAM model data. 
      
      ylab1 <- NA
      
      # PossConcUnits is slightly different between ADAM, AdvBrain, and regular
      # tissues, so do NOT interchange them in the code.
      
      if(normalize_by_dose){
         PossConcUnits <- list("mg/mL" = "Dose-normalized\nconcentration (mg/mL/mg)",
                               "µg/L" = "Dose-normalized\nconcentration (µg/L/mg)", 
                               "µg/mL" = "Dose-normalized\nconcentration (µg/mL/mg)",
                               "ng/mL" = "Dose-normalized\nconcentration (ng/mL/mg)",
                               "ng/L" = "Dose-normalized\nconcentration (ng/L/mg)",
                               "µM" = "Dose-normalized\nconcentration (µM/mg)",
                               "nM" = "Dose-normalized\nconcentration (nM/mg)",
                               "µg/mL" = "Dose-normalized\nconcentration (µg/mL/mg)")
      } else {
         PossConcUnits <- list("mg/mL" = "Concentration (mg/mL)",
                               "µg/L" = "Concentration (µg/L)", 
                               "µg/mL" = "Concentration (µg/mL)",
                               "ng/mL" = "Concentration (ng/mL)",
                               "ng/L" = "Concentration (ng/L)",
                               "µM" = "Concentration (µM)",
                               "nM" = "Concentration (nM)",
                               "mg" = "Amount (mg)",
                               "mg/h" = "Absorption rate (mg/h)",
                               "µg/mL" = "Concentration (µg/mL)",
                               "mL" = "Volume (mL)",
                               "PD response" = "PD response",
                               "Relative abundance" = "Relative abundance")
      }
      
      ylab <- ifelse(length(ObsConcUnits) > 0, 
                     PossConcUnits[[ObsConcUnits]], "Amount")
      
   }
   
   # Noting when to adjust graph border to add lines to y axis title. This may
   # be moot now that we're not using bquote.
   AdjustGraphBorder <- "expression" %in% class(ylab1) == FALSE &&
      complete.cases(ylab1) && str_detect(ylab1, "\\\n")
   
   # Per Hannah: If there are observed data included in the simulation, set the
   # y axis limits to show those data well. 
   
   # To do this, when observed data are present, filtering Ylim_data to only
   # include concentrations >= 0.8*min(observed conc). t0 point isn't included
   # in this calculation. 
   if(EnzPlot == FALSE && nrow(Ylim_data %>% filter(Simulated == FALSE)) > 0){
      ObsMin <- Ylim_data %>% filter(Simulated == FALSE & Conc > 0) %>% 
         pull(Conc) %>% min(na.rm = TRUE) 
      
      Ylim_data <- Ylim_data %>% filter(Conc >= ObsMin)
   }
   
   # One option LS discussed w/MBK and FC: We could also limit the semi-log y
   # axis when there are no observed data to show Cmax * 1.25 then round up to
   # nearest factor of 10 and then give 4 orders of magnitude below that.
   # Problem is that often people will want to see full simulation, and that
   # would not provide that. We're holding off on implementing that until we
   # hear from users that they want that.
   
   # Time cannot be NA for it to be included in Ylim calculations, and it *will*
   # be NA if there were values in the SD_SE column. Using placeholder values
   # for time; just making sure that they're within the time range.
   
   Ylim <- Ylim_data %>% 
      mutate(Time = ifelse(is.na(Time), time_range[1], Time), 
             Time_orig = ifelse(is.na(Time_orig), Time, Time_orig)) %>% 
      filter(Time_orig >= time_range[1] &
                Time_orig <= time_range[2] &
                complete.cases(Conc)) %>%
      pull(Conc) %>%
      range()
   
   if(all(Ylim == 0) && (any(is.na(y_axis_limits_lin) | any(is.na(y_axis_limits_log))))){
      stop("For the tissue and compound selected, all concentrations = 0. Please either 1) specify what y axis limits you'd like for what will be empty graphs (set both y_axis_limits_lin and y_axis_limits_log) or 2) select a different combination of tissue and compound to graph.",
           call. = FALSE)
   }
   
   if(any(complete.cases(y_axis_limits_lin))){
      Ylim <- y_axis_limits_lin[1:2]
   } else if(EnzPlot){
      # If it's an enzyme abundance plot, user will generally want the lower
      # limit for the y axis to be 0 rather than 100 in the case of induction.
      # Setting that here, but user can override w/y_axis_limits_lin argument.
      Ylim[1] <- 0
   }
   
   # Some users are sometimes getting Inf for possible upper limit of data,
   # although I haven't been able to reproduce this error. Trying to catch
   # that nonetheless.
   if(is.infinite(Ylim[2]) | is.na(Ylim[2])){
      Ylim[2] <- max(Ylim_data$Conc, na.rm = T)
   }
   
   PossYBreaks <- data.frame(Ymax = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50,
                                      100, 200, 500, 1000, 2000, 5000,
                                      10000, 20000, 50000, 100000,
                                      200000, 500000, Inf),
                             YBreaksToUse = c(0.02, 0.05, 0.1, 0.2, 0.5,
                                              1, 2, 5, 10, 20, 50, 100, 200,
                                              500, 1000, 2000, 5000, 10000,
                                              20000, 50000, 100000, 200000))
   
   YBreaksToUse <- PossYBreaks %>% filter(Ymax >= (Ylim[2] - Ylim[1])) %>%
      slice(which.min(Ymax)) %>% pull(YBreaksToUse)
   
   # If we've maxed out on the regular breaks, probably need to use something
   # that isn't already all set up and nice.
   if(YBreaksToUse == 2e+05){
      YInterval    <- switch(paste(is.na(y_axis_interval), EnzPlot), 
                             "TRUE FALSE" = Ylim[2] / 5, 
                             "FALSE FALSE" = y_axis_interval, 
                             "TRUE TRUE" = Ylim[2] / 5,
                             "FALSE TRUE" = 2*y_axis_interval)
      
   } else {
      YInterval    <- switch(paste(is.na(y_axis_interval), EnzPlot), 
                             "TRUE FALSE" = YBreaksToUse, 
                             "FALSE FALSE" = y_axis_interval, 
                             "TRUE TRUE" = YBreaksToUse,
                             "FALSE TRUE" = 2*y_axis_interval)
      
   }
   
   YmaxRnd      <- ifelse(is.na(y_axis_limits_lin[2]), 
                          round_up_unit(Ylim[2], YInterval),
                          y_axis_limits_lin[2])
   YBreaks      <- seq(ifelse(is.na(y_axis_limits_lin[1]), 
                              0, y_axis_limits_lin[1]),
                       YmaxRnd, YInterval/2)                    # create labels at major and minor points
   YLabels      <- format(YBreaks, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
   YLabels[seq(2,length(YLabels),2)] <- ""                         # add blank labels at every other point i.e. for just minor tick marks at every other point
   
   # If the user specified a y axis interval, make sure there's an axis label
   # for the top of the y axis
   if(any(complete.cases(y_axis_limits_lin))){
      YBreaks <- unique(c(YBreaks, y_axis_limits_lin[2]))
      if(length(YLabels) == length(YBreaks)){
         YLabels[length(YLabels)] <- 
            format(YBreaks[length(YBreaks)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      } else {
         YLabels <- c(YLabels, 
                      format(YBreaks[length(YBreaks)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE))
      }
   }
   
   # Adding padding if user requests it
   if(class(pad_y_axis) == "logical"){ # class is logical if pad_y_axis unspecified
      if(EnzPlot){
         if(pad_y_axis){
            pad_y_num <-  c(0.02, 0.1)
         } else {
            pad_y_num <- c(0, 0)
         }
         
      } else {
         if(pad_y_axis){
            pad_y_num <-  c(0.02, 0)
         } else {
            pad_y_num <- c(0, 0)
         }
      }
   } else {
      pad_y_num <- pad_y_axis
      if(length(pad_y_axis) == 1){
         pad_y_num <- c(pad_y_num, 0)
      } else {
         pad_y_num <- pad_y_axis[1:2]
      }
      
      pad_y_axis <- pad_y_num[1] != 0 # Making pad_y_axis logical again to work with code elsewhere
   }
   
   # Setting up y axis for semi-log graphs ----------------------------------
   
   if(all(is.na(time_range_relative))){
      time_range_relative <- range(Ylim_data$Time[
         complete.cases(Ylim_data$Conc)], na.rm = T)
   } 
   
   Ylim_log <- range(Ylim_data$Conc, na.rm = T)
   
   if(all(is.na(y_axis_limits_log))){ # Option to consider for the future: Allow user to specify only the upper limit, which would leave y_axis_limits_log[1] as NA?
      
      # If user did not specify limits, then we'll need to find them based on
      # what data are included, which includes figuring out which data are
      # within the time range they wanted.
      
      near_match <- function(x, t) {x[which.min(abs(t - x))]} # LS to HB: Clever solution to this problem! :-)
      
      Ylim_log[1] <- Ylim_data %>% 
         filter(Conc >= 0) %>%  # Not allowing BLQ values that were set below 0.
         filter(complete.cases(Conc)) %>% 
         mutate(TimeMatch = Time == near_match(Time, time_range_relative[2])) %>%
         filter(TimeMatch == TRUE) %>% 
         pull(Conc) %>% min()
      
   } else {
      
      # If user set Ylim_log[1] to 0, set it to 1/100th the higher value and
      # tell them we can't use 0.
      if(y_axis_limits_log[1] <= 0){
         y_axis_limits_log[1] <- y_axis_limits_log[2]/100
         warning("You requested a lower y axis limit of 0, which is undefined for log-transformed data. The lower y axis limit will be set to 1/100th the upper y axis limit instead.",
                 call. = FALSE)
      }
      
      Ylim_log <- y_axis_limits_log
      
   }
   
   Breaks <- make_log_breaks(data_range = Ylim_log, 
                             axis_limits_log = y_axis_limits_log) # just submit y range desired
   
   
   YLogBreaks <- Breaks$breaks
   YLogLabels <- Breaks$labels
   Ylim_log <- Breaks$axis_limits_log
   
   # return -------------------------------------------------------------------
   
   # Assigning the variables created or changed here to the environment one
   # level up, e.g., probably the environment within the function that's
   # calling on *this* function.
   
   Out <- list("ObsConcUnits" = ObsConcUnits,
               "ylab" = ylab,
               "YLabels" = YLabels,
               "YLogLabels" = YLogLabels,
               "YBreaks" = YBreaks, 
               "YLogBreaks" = YLogBreaks, 
               "Ylim_log" = Ylim_log,
               "YmaxRnd" = YmaxRnd, 
               "pad_y_num" = pad_y_num, 
               "pad_y_axis" = pad_y_axis, 
               "AdjustGraphBorder" = AdjustGraphBorder)
}

