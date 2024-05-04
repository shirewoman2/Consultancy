#' Make pretty breaks and labels for a log axis
#'
#' \code{make_log_breaks} is mainly meant for internal SimcypConsultancy package
#' use and is thus not terribly annotated in terms of warnings if you do
#' something wrong.
#'
#' @param axis_limits_log desired y axis limits for a semi-log
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
make_log_breaks <- function(axis_range, 
                            axis_limits_log = NA){
   
   axis_range <- sort(axis_range)
   
   # If user set axis_range[1] to 0, set it to 1/100th the higher value and
   # tell them we can't use 0.
   if(complete.cases(axis_limits_log[1]) && axis_limits_log[1] <= 0){
      axis_limits_log[1] <- axis_limits_log[2]/100
      warning("You requested a lower y axis limit of 0, which is undefined for log-transformed data. The lower y axis limit will be set to 1/100th the upper y axis limit instead.",
              call. = FALSE)
   }
   
   # If axis_range[1] is 0, which can happen when the concs are really low, that
   # is undefined for log transformations. Setting it to be max value / 100
   # when that happens.
   axis_range[1] <- ifelse(axis_range[1] == 0, 
                         axis_range[2]/100, axis_range[1])
   axis_range[1] <- round_down(axis_range[1])
   axis_range[2] <- round_up(axis_range[2])
   axis_range <- sort(axis_range) # sorting again just to be sure b/c sometimes can get weird mathematical artifacts
   
   YLogBreaks <- as.vector(outer(1:9, 10^(log10(axis_range[1]):log10(axis_range[2]))))
   YLogBreaks <- YLogBreaks[YLogBreaks >= axis_range[1] & YLogBreaks <= axis_range[2]]
   YLogLabels   <- rep("",length(YLogBreaks))
   
   if(is.na(axis_limits_log[1]) |
      # checking whether axis_range values are a factor of 10 b/c, if they are,
      # then just use the axis_range that we would have come up with using the
      # other method b/c that makes prettier breaks
      all(log10(axis_range) == round(log10(axis_range)))){
      
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
               "axis_limits_log" = axis_range))
   
}

   
   