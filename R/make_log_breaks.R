#' Make pretty breaks and labels for a log axis
#'
#' \code{make_log_breaks} is mainly meant for internal SimcypConsultancy package
#' use and is thus not terribly annotated in terms of warnings if you do
#' something wrong.
#'
#' @param y_axis_limits_log desired y axis limits for a semi-log
#'   concentration-time plot
#' @param ct_dataframe the concentration-time data.frame that's being plotted
#' @param time_range_to_use optionally specify a time range for the conc-time
#'   data you're plotting and we'll find min and max concs within only that time
#'   range. If left as NA, we'll use the min and max concs for the overall data. 
#'
#' @return a list of log axis breaks and log axis labels
#' @export
#'
#' @examples
#' # none
#'
#' 
make_log_breaks <- function(y_axis_limits_log = NA, 
                            ct_dataframe, 
                            time_range_to_use = NA){
   
   if(all(is.na(time_range_to_use))){
      time_range_relative <- range(ct_dataframe$Time[
         complete.cases(ct_dataframe$Conc)], na.rm = T)
   } else {
      time_range_relative <- time_range_to_use
   }
   
   near_match <- function(x, t) {x[which.min(abs(t - x))]} # LS to HB: Clever solution to this problem! :-)
   
   if(is.na(y_axis_limits_log[1])){ # Option to consider for the future: Allow user to specify only the upper limit, which would leave y_axis_limits_log[1] as NA?
      
      Ylim_log <- range(ct_dataframe$Conc, na.rm = T)
      
      Ylim_log[1] <- ct_dataframe %>%
         filter(Conc >= 0) %>%  # Not allowing BLQ values that were set below 0.
         filter(Time == near_match(ct_dataframe$Time[complete.cases(ct_dataframe$Conc)],
                                   time_range_relative[2])) %>%
         pull(Conc) %>% min()
      
      # If Ylim_log[1] is 0, which can happen when the concs are really low, that
      # is undefined for log transformations. Setting it to be max value / 100
      # when that happens.
      Ylim_log[1] <- ifelse(Ylim_log[1] == 0, 
                            Ylim_log[2]/100, Ylim_log[1])
      Ylim_log[1] <- round_down(Ylim_log[1])
      Ylim_log[2] <- round_up(max(ct_dataframe$Conc, na.rm = T))
      Ylim_log <- sort(Ylim_log)
      
   } else {
      # If user set Ylim_log[1] to 0, set it to 1/100th the higher value and
      # tell them we can't use 0.
      if(y_axis_limits_log[1] <= 0){
         y_axis_limits_log[1] <- y_axis_limits_log[2]/100
         warning("You requested a lower y axis limit that is undefined for log-transformed data. The lower y axis limit will be set to 1/100th the upper y axis limit instead.",
                 call. = FALSE)
      }
      
      Ylim_log <- y_axis_limits_log
      
   }
   
   YLogBreaks <- as.vector(outer(1:9, 10^(log10(Ylim_log[1]):log10(Ylim_log[2]))))
   YLogBreaks <- YLogBreaks[YLogBreaks >= Ylim_log[1] & YLogBreaks <= Ylim_log[2]]
   YLogLabels   <- rep("",length(YLogBreaks))
   
   if(is.na(y_axis_limits_log[1]) |
      # checking whether Ylim_log values are a factor of 10 b/c, if they are,
      # then just use the Ylim_log that we would have come up with using the
      # other method b/c that makes prettier breaks
      all(log10(Ylim_log) == round(log10(Ylim_log)))){
      
      # add labels at order of magnitude
      YLogLabels[seq(1, length(YLogLabels), 9)] <- 
         format(YLogBreaks[seq(1,length(YLogLabels),9)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      
   } else {
      
      # add labels for the 1st and last YLogBreaks and also 3 in between. The
      # odd fractions are b/c I want to have them spaced out somewhat
      # regularly, and that requires nonlinear intervals since it's log
      # transformed.
      YLogLabels[1] <- 
         format(YLogBreaks[1], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      Nbreaks <- length(YLogBreaks)
      YLogLabels[Nbreaks] <- 
         format(YLogBreaks[Nbreaks], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      YLogLabels[round(Nbreaks/4)] <- 
         format(YLogBreaks[round(Nbreaks/4)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      YLogLabels[round(2*Nbreaks/3)] <- 
         format(YLogBreaks[round(2*Nbreaks/3)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      YLogLabels[round(5*Nbreaks/6)] <- 
         format(YLogBreaks[round(5*Nbreaks/6)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      
   } 
   
   return(list("labels" = YLogLabels, 
               "breaks" = YLogBreaks, 
               "y_axis_limits_log" = Ylim_log))
   
}

   
   