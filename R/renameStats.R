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
                   "Geometric Mean" = "geomean",
                   "Median" = "median",
                   "Geometric CV" = "GCV",
                   "90% confidence interval around the geometric mean(lower limit)" = "CI90_low",
                   "90% confidence interval around the geometric mean(upper limit)" = "CI90_high",
                   "95% confidence interval around the geometric mean(lower limit)" = "CI95_low",
                   "95% confidence interval around the geometric mean(upper limit)" = "CI95_high",
                   "5th centile" = "per5",
                   "95th centile" = "per95",
                   "Skewness" = "skewness", 
                   "cv" = "CV",
                   "Min Val" = "min",
                   "Max Val" = "max",
                   "Fold" = "fold",
                   "Std Dev" = "SD")
    StatNames[OrigStat]
}
