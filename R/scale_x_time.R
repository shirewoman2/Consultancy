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
#' @param time_units the units of time in the graph. Defaults is "hours", and
#'   acceptable alternative input is "minutes" or "days".
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
#'
#' @return
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
                         time_units = "hours", 
                         x_axis_interval = NA, 
                         pad_x_axis = TRUE){
    
    # Error catching --------------------------------------------------------
    if(all(complete.cases(time_range)) && class(time_range) == "numeric" &
       time_range[1] >= time_range[2]){
        warning("The 1st value for 'time_range' must be less than the 2nd value. We'll use the full time range instead.",
                call. = FALSE)
        time_range <- NA
    }
    
    DataLayers <- which(sapply(last_plot()$layers,
                               FUN = function(x) "data.frame" %in% class(x$data)))
    GraphData <- bind_rows(lapply(last_plot()$layers[DataLayers], 
                                  FUN = function(x) x$data), 
                           last_plot()$data)
    names(GraphData)[which(names(GraphData) == as_label(last_plot()$mapping$x))] <- "Time"
    
    if(all(is.na(time_range))){
        time_range <- range(GraphData$Time, na.rm = TRUE)
    }
    
    time_units <- tolower(time_units)
    if(time_units %in% c("hours", "minutes", "days") == FALSE){
        warning("You have not supplied an acceptable value for time_units, which can be `hours`, `minutes`, or `days`. We'll use the default, `hours`.", 
                call. = FALSE)
        time_units <- "hours"
    }
    
    # Main body of function -------------------------------------------------
    
    # Setting range to start at 0 and then will adjust *back* to the correct t0
    # later. This will allow for setting good intervals for the range that's
    # actually covered.
    t0 <- min(time_range)
    time_range_adj <- time_range - t0
    
    # If tlast is just a smidge over one of the possible breaks, it goes to the
    # next one and doesn't look as nice on the graph. Rounding tlast down to the
    # nearest 4 for hours, nearest 15 for minutes, and nearest 2 for days.
    tlast <- switch(time_units, 
                    "hours" = round_unit(time_range_adj[2], 4),
                    "minutes" = round_unit(time_range_adj[2], 15), 
                    "days" = round_unit(time_range_adj[2], 2))
    
    if(tlast <= max(time_range_adj, na.rm = T) * 0.8){
        tlast <- switch(time_units, 
                        "hours" = round_up_unit(time_range_adj[2], 4),
                        "minutes" = round_up_unit(time_range_adj[2], 15), 
                        "days" = round_up_unit(time_range_adj[2], 2))
    }
    
    if(time_units == "hours"){
        
        PossBreaks <- data.frame(
            Tlast = c(2, 4, 8, 12, 24, 48, 96, 168, 336, 360, 504, 672, Inf),
            BreaksToUse = c("2hr", "4hr", "8hr", "12hr", "24hr", "48hr", "96hr",
                            "1wk", "2wk",
                            "15d", "3wk", "4wk", "4wkplus"))
        
        BreaksToUse <- PossBreaks %>% filter(Tlast >= tlast) %>%
            slice(which.min(Tlast)) %>% pull(BreaksToUse)
        
        BreaksToUse <- ifelse(complete.cases(x_axis_interval),
                              "UserDefined", BreaksToUse)
        
        XBreaks <- switch(BreaksToUse,
                          "2hr" = seq(0, 2, 0.25),
                          "4hr" = seq(0, 4, 0.5),
                          "8hr" = seq(0, 8, 0.5),
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
        
    } else if(time_units == "minutes"){
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
    } else if(time_units == "days"){
        PossBreaks <- data.frame(
            Tlast = c(7, 14, 21, 28, 35, 42, 49, 70, 105, 140, Inf),
            BreaksToUse = c("1wk", "2wk", "3wk", "4wk", "5wk", "6wk",
                            "7wk", "10wk", "15wk", "20wk", "20wkplus"))
        
        BreaksToUse <- PossBreaks %>% filter(Tlast >= tlast) %>%
            slice(which.min(Tlast)) %>% pull(BreaksToUse)
        
        BreaksToUse <- ifelse(complete.cases(x_axis_interval),
                              "UserDefined", BreaksToUse)
        
        XBreaks <- switch(BreaksToUse,
                          "1wk" = seq(0, 7, 1),
                          "2wk" = seq(0, 14, 2),
                          "3wk" = seq(0, 21, 3), 
                          "4wk" = seq(0, 28, 4), 
                          "5wk" = seq(0, 35, 5),
                          "6wk" = seq(0, 42, 6), 
                          "7wk" = seq(0, 49, 7), 
                          "10wk" = seq(0, 70, 7), 
                          "15wk" = seq(0, 105, 7),
                          "20wk" = seq(0, 140, 14),
                          "20wkplus" = round_up_nice(seq(0, tlast,
                                                         length.out = 12)),
                          "UserDefined" = seq(0, max(Data$Time, na.rm = T),
                                              x_axis_interval/2))
    }
    
    XLabels <- XBreaks + t0
    XLabels[seq(2,length(XLabels),2)] <- ""
    XLabels[which(XBreaks + t0 == 0)] <- "0"
    
    # Adding padding if user requests it
    if(class(pad_x_axis) == "logical"){ # class is logical if pad_x_axis unspecified
        if(pad_x_axis){
            pad_x_num <-  c(0.02, 0.04)
        } else {
            pad_x_num <- c(0, 0)
        }
    } else {
        pad_x_num <- pad_x_axis
        if(length(pad_x_axis) == 1){
            pad_x_num <- c(pad_x_num, 0.04)
        } else {
            pad_x_axis <- pad_x_axis[1:2]
        }
        
        pad_x_axis <- pad_x_num[1] != 0 # Making pad_x_axis logical again to work with code elsewhere
    }
    
    return(scale_x_continuous(breaks = XBreaks + t0, 
                              labels = XLabels, 
                              limits = range(time_range),
                              expand = expansion(mult = pad_x_num)))
    
}



