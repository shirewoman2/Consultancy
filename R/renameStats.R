#' Rename the statistics listed in Simulator output to R-friendly versions
#'
#' \code{renameStats} takes Simulator statistic names such as "90% confidence
#' interval around the geometric mean(lower limit)" and converts them to names
#' that would work for R column names, e.g., "CI90_low". This is mainly meant
#' for internal use in the SimcypConsultancy package to help make sure we're
#' naming things consistently.
#'
#' @param OrigStat the original statistic name
#'
#' @return an R-friendly version of that statistic name
#' @export
#'
#' @examples#' 
#' renameStats(c("Geometric Mean", "Min Val", "5th centile"))
#' 
renameStats <- function(OrigStat){
    StatNames <- c("Mean" = "mean", 
                   "mean" = "mean", 
                   "Geometric Mean" = "geomean",
                   "geometric mean" = "geomean",
                   "Median" = "median",
                   "median" = "median",
                   "Geometric CV" = "GCV",
                   "geometric cv" = "GCV",
                   "90% confidence interval around the geometric mean(lower limit)" = "CI90_low",
                   "90% confidence interval around the geometric mean(upper limit)" = "CI90_high",
                   "95% confidence interval around the geometric mean(lower limit)" = "CI95_low",
                   "95% confidence interval around the geometric mean(upper limit)" = "CI95_high",
                   "5th centile" = "per5",
                   "95th centile" = "per95",
                   "Skewness" = "skewness", 
                   "skewness" = "skewness", 
                   "cv" = "CV",
                   "Min Val" = "min",
                   "min val" = "min",
                   "Max Val" = "max",
                   "max val" = "max",
                   "Fold" = "fold",
                   "fold" = "fold",
                   "Std Dev" = "SD", 
                   "std dev" = "SD", 
                   # These next ones are only for inside the PKsummary_table
                   # function and apply to trial-mean min and trial-mean max
                   "MinMean" = "MinMean", 
                   "minmMean" = "MinMean", 
                   "maxmean" = "MaxMean", 
                   "MaxMean" = "MaxMean")
    
    StatNames[OrigStat]
}
