#' Create nice breaks for graphs of log-transformed data
#'
#' \code{log10_breaks} creates a vector of numbers fairly evenly spaced on a log 10 scale with 10
#' points. (They're more evenly spaced if "min" and "max" span 1 order of
#' magnitude.) The decimals aren't necessarily pretty, though, so you may want
#' to set "prettify" to TRUE if they're ugly.
#'
#' @param min minimum value to use for breaks
#' @param max maximum value to use for breaks
#'
#' @return
#' @export
#'
#' @examples
#' data("ConcTime")
#' ggplot(ConcTime, aes(x = Time, y = Conc, color = ID)) +
#'    geom_line() +
#'    scale_y_log10(breaks = log10_breaks(min = min(ConcTime$Conc[ConcTime$Conc > 0]),
#'                                        max = max(ConcTime$Conc)))
#'
#' ggplot(ConcTime, aes(x = Time, y = Conc, color = ID)) +
#'    geom_line() +
#'    scale_y_log10(breaks = log10_breaks(min = min(ConcTime$Conc[ConcTime$Conc > 0]),
#'                                        max = max(ConcTime$Conc),
#'                                        prettify = TRUE))
#'
#'
log10_breaks <- function(min, max, prettify = FALSE) {
    if(any(c(min, max) <= 0)){
        stop("Neither 'min' nor 'max' may be less than or equal to zero since log(0) and log(negative numbers) are undefined.",
             call. = FALSE)
    }
    
    if(length(log10(min):log10(max)) > 1){
        breaks <- as.vector(outer(1:9, 10^(log10(min):log10(max))))
    } else {
        breaks <- seq(min, max, length.out = 10)
    }
    
    breaks <- breaks[breaks >= min & breaks <= max]
    
    if(prettify){
        breaks <- round_up_nice(breaks)
    }
    
    if(length(breaks) > 10){
        breaks <- breaks[round(seq(1, length(breaks), length.out = 10))]
    }
    
    return(breaks)
}


