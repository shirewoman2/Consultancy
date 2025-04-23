#' Automatically scale a ggplot graph x axis for time
#'
#' \code{scale_x_time} formats a ggplot graph x axis to have breaks that make
#' sense for the user-specified time units and adds a minor tick between the
#' labels. It optionally adds padding to the left and right sides of the x axis.
#'
#' @param time_range time range to show for the graph. Options: \describe{
#'
#'   \item{NA}{(default) entire time range of data}
#'
#'   \item{a start time and end time in hours}{only data in that range, e.g.
#'   \code{time_range = c(24, 48)}. Note that there are no quotes around numeric
#'   data.}}
#' @param impose_limits TRUE (default) or FALSE to actually \emph{set} the
#'   limits listed in \code{time_range}. Why on earth would you \emph{not}
#'   impose limits on the graph here? Behind the scenes, \code{scale_x_time}
#'   uses the ggplot2 function \code{\link{ggplot::scale_x_continuous}}, which
#'   filters out all data beyond the limits you specify. That means that you
#'   can, in some situations, get graphs where lines are cut off abruptly. This
#'   is set up with the idea that you can set the time range here to get
#'   reasonable x axis intervals, but then you can add a call to
#'   \code{\link{ggplot::coord_cartesian}} to limit the range without the abrupt
#'   cut off.
#' @param time_units the units of time in the graph. Options are "hours"
#'   (default), "minutes", "days", or "weeks".
#' @param x_axis_interval optionally set the x-axis major tick-mark interval.
#'   Acceptable input: any number or leave as NA to accept default values, which
#'   are generally reasonable guesses as to aesthetically pleasing and
#'   time-relevant intervals.
#' @param pad_x_axis optionally add a smidge of padding to the the x axis
#'   (default is TRUE, which includes some generally reasonable padding). If
#'   changed to FALSE, the y axis will be placed right at the beginning of your
#'   time range and all data will end \emph{exactly} at the end of the time
#'   range specified. If you want a \emph{specific} amount of x-axis padding,
#'   set this to a number; the default is \code{c(0.02, 0.04)}, which adds 2\%
#'   more space to the left side and 4\% more to the right side of the x axis.
#'   If you only specify one number, we'll assume that's the percent you want
#'   added to the left side.
#' @param x_breaks x-axis breaks to use if you want more control than the
#'   default values. Most of the time, you want this to be NA, but if the breaks
#'   the function is coming up with just are not working for your scenario, you
#'   can specify \emph{exactly} what you want here. Either specify the exact
#'   numbers you want, as in, \code{x_breaks = c(0, 4, 6, 10)} or specify the
#'   start, end, and interval you want as in \code{x_breaks = seq(from = 0, to =
#'   336, by = 10)}. As an additional convenience, you can let the function
#'   calculate a reasonable time interval and specify that with a named list of
#'   "from", "to", and "by" (analogous to the base R "seq" function), e.g.,
#'   \code{x_breaks = list(from = 0, to = 336, by = "default")} and the minimum
#'   and maximum values will be set manually but the interval will be
#'   calculated. This differs from the argument \code{time_range} in that it
#'   does NOT set the limits of the actual data -- only the way things are
#'   labeled on the x axis.
#'
#' @return a ggplot2 graph scale for the x axis (replaces scale_x_continuous in
#'   a graph)
#' @export
#'
#' @examples
#' MyData <- data.frame(Time = 0:168,
#'                      Conc = rnorm(n = 169, mean = 100))
#' ggplot(MyData, aes(x = Time, y = Conc)) +
#'     geom_point() + scale_x_time()
#'
#' ggplot(MyData, aes(x = Time, y = Conc)) +
#'     geom_point() + scale_x_time(time_range = c(24, 48))
#'
#' # You don't have to name the column with your x-axis data "Time".
#' MyAltData <- data.frame(Mango = 0:24,
#'                         Conc = rnorm(n = 25, mean = 100))
#'
#' ggplot(MyAltData, aes(x = Mango, y = Conc)) +
#'     geom_point() + scale_x_time()
#'
#' 
scale_x_time <- function(time_range = NA, 
                         impose_limits = TRUE,
                         time_units = "hours", 
                         x_axis_interval = NA, 
                         x_breaks = NA, 
                         pad_x_axis = TRUE){
   
   # Error catching --------------------------------------------------------
   if(all(complete.cases(time_range)) && class(time_range) == "numeric" &
      time_range[1] >= time_range[2]){
      warning("The 1st value for 'time_range' must be less than the 2nd value. We'll use the full time range instead.",
              call. = FALSE)
      time_range <- NA
   }
   
   # Main body of function ------------------------------------------------
   
   Setup <- scale_x_time_setup(time_range = time_range, 
                               time_units = time_units, 
                               x_axis_interval = x_axis_interval, 
                               pad_x_axis = pad_x_axis)
   
   # FIXME - Haven't added any error catching here yet
   if(any(complete.cases(x_breaks))){
      if("list" %in% class(x_breaks)){
         Setup$xbreaks <- 
            seq(from = as.numeric(x_breaks$from), 
                to = as.numeric(x_breaks$to), 
                by = suppressWarnings(
                   case_when(x_breaks$by == "default" ~ 
                                as.numeric(Setup$xbreaks[2]) - 
                                as.numeric(Setup$xbreaks[1]), 
                             .default = as.numeric(x_breaks$by))))
         
      } else {
         Setup$xbreaks <- x_breaks
      }
      
         Setup$xlabels <- as.character(Setup$xbreaks)
         Setup$xlabels[seq(2,length(Setup$xlabels), by = 2)] <- ""
         
   }
   
   return(scale_x_continuous(breaks = Setup$xbreaks, 
                             labels = Setup$xlabels, 
                             limits = switch(as.character(impose_limits), 
                                             "TRUE" = Setup$limits,
                                             "FALSE" = NULL),
                             expand = Setup$expand))
   
}



