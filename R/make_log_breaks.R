#' Make pretty breaks and labels for a log axis
#'
#' \code{make_log_breaks} creates a set of evenly spaced breaks and labels in
#' what will be a log-transformed axis. These are set up to be used with a
#' ggplot2 log-transformed axis like this, where "G" is whatever else you have
#' specified for a ggplot2 graph and "MyBreaks" is the output from running
#' \code{make_log_breaks}: \code{G + scale_y_log10(limits =
#' MyBreaks$axis_limits_log, breaks = MyBreaks$breaks, labels =
#' MyBreaks$labels)}  If the limits are powers of 10, this will put breaks at
#' all powers of 10, and, if not, it will guess at reasonably spaced intervals.
#' Note that this will make it so that there are unlabeled tick marks between
#' the labels; this is to help make it clearer visually that the axis has been
#' log transformed.
#'
#' @param data_range the range of values to be plotted on a log-transformed axis
#' @param axis_limits_log desired axis limits for an axis that will be log
#'   transformed. Leave this as NA if you'd like us to make a reasonable guess.
#'
#' @return a list of log axis breaks and log axis labels
#' @export
#'
#' @examples
#' # none
#'
#' 
make_log_breaks <- function(data_range, 
                            axis_limits_log = NA){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   if(length(data_range[which(complete.cases(data_range))]) != 2){
      stop("You must enter a lower and upper limit for data_range.", 
           call. = FALSE)
   }
   
   # Give a warning if they specified anything for axis_limits_log but did not
   # make it length 2
   if(length(axis_limits_log[
      which(complete.cases(axis_limits_log))]) %in% c(0, 2) == FALSE){
      warning("You must enter a lower and upper limit for axis_limits_log if you want to specify what the limits should be. Since you have not, we will make a reasonable guess.\n", 
              call. = FALSE)
      axis_limits_log <- NA
   }
   
   # Main body of function ----------------------------------------------------
   
   data_range <- sort(data_range)
   if(all(complete.cases(axis_limits_log))){
      axis_limits_log <- sort(axis_limits_log)
   }
   
   # If user set axis_limits_log[1] to 0, set it to 1/100th the higher value and
   # tell them we can't use 0.
   if(all(complete.cases(axis_limits_log)) && axis_limits_log[1] <= 0){
      axis_limits_log[1] <- axis_limits_log[2]/100
      warning("You requested a lower axis limit of 0, which is undefined for log-transformed data. The lower axis limit will be set to 1/100th the upper axis limit instead.",
              call. = FALSE)
   }
   
   # Same thing for if user did not specify axis_limits_log but data_range
   # includes 0. No need for warning, though; just pretend the data range was
   # 1/100 the max. If that's not true, user can specify what they want for
   # limits b/c impossible to guess. 
   if(all(complete.cases(data_range)) && data_range[1] <= 0){
      data_range[1] <- data_range[2]/100
   }
   
   # If user specified limits, then use those. If not, round to intervals that
   # are powers of 10.
   if(all(is.na(axis_limits_log))){
      axis_limits_log[1] <- round_down(data_range[1])
      axis_limits_log[2] <- round_up(data_range[2])
   }
   
   axis_limits_log <- sort(axis_limits_log) # sorting again just to be sure b/c sometimes can get weird mathematical artifacts
   
   LogBreaks <- as.vector(outer(1:9, 10^(log10(axis_limits_log[1]):
                                            log10(axis_limits_log[2]))))
   LogBreaks <- LogBreaks[LogBreaks >= axis_limits_log[1] & 
                             LogBreaks <= axis_limits_log[2]]
   LogLabels   <- rep("",length(LogBreaks))
   
   # checking whether axis_limits_log values are a factor of 10 b/c, if they
   # are, that doesn't require coming up with odd intervals. ONLY want magnitude
   # of 10 breaks in that case.
   if(all(log10(axis_limits_log) == round(log10(axis_limits_log)))){
      
      # add labels at order of magnitude
      LogLabels[seq(1, length(LogLabels), 9)] <- 
         format(LogBreaks[seq(1, length(LogLabels), 9)], 
                scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      
   } else {
      
      # add labels for the 1st and last LogBreaks and also 3 in between. The
      # odd fractions are b/c I want to have them spaced out somewhat
      # regularly, and that requires nonlinear intervals since it's log
      # transformed.
      LogLabels[1] <- 
         format(LogBreaks[1], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      Nbreaks <- length(LogBreaks)
      LogLabels[Nbreaks] <- 
         format(LogBreaks[Nbreaks], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      LogLabels[round(Nbreaks/4)] <- 
         format(LogBreaks[round(Nbreaks/4)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      LogLabels[round(2*Nbreaks/3)] <- 
         format(LogBreaks[round(2*Nbreaks/3)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      LogLabels[round(5*Nbreaks/6)] <- 
         format(LogBreaks[round(5*Nbreaks/6)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
      
   } 
   
   return(list("labels" = LogLabels, 
               "breaks" = LogBreaks, 
               "axis_limits_log" = axis_limits_log))
   
}

