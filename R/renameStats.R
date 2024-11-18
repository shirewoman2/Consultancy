#' Rename the statistics listed in Simulator output to R-friendly versions
#'
#' \code{renameStats} takes Simulator statistic names such as "90% confidence
#' interval around the geometric mean(lower limit)" and converts them to names
#' that would work for R column names, e.g., "CI90_lower", or converts them to
#' names that comply with Simcyp Consultancy Team report templates. This is
#' mainly meant for internal use in the SimcypConsultancy package to help make
#' sure we're naming things consistently.
#'
#' @param OrigStat the original statistic name
#' @param use options are "internal" for internal use when renaming stats from
#'   Simulator-output names into R-friendly column names or "report" for
#'   renaming R columns into the values that show up in the Simcyp Consultancy
#'   report template tables of PK parameters.
#'
#' @return an R-friendly version of that statistic name
#' @export
#'
#' @examples
#' renameStats(c("Geometric Mean", "Min Val", "5th centile"))
#' 
renameStats <- function(OrigStat, 
                        use = "internal"){
   
   # NB: As of Nov. 2024, making the names be title case to avoid confusion with
   # the base R functions. Note that Geomean is title case and NOT camel case
   # and that minimum and maximum are spelled out.
   AllStats <- tibble(
      InternalColNames = c("Mean", "Mean", "Geomean", "Geomean", 
                           "Median", "Median", "GCV", "GCV", 
                           "CI90_lower", "CI90_upper", NA, # concatenated 90% CI
                           "CI95_lower", "CI95_upper", NA, # concatenated 95% CI
                           "Per5", "Per95", NA, # concatenated percentiles
                           "Skewness", "Skewness", 
                           "CV", "Minimum", "Minimum", "Maximum", "Maximum", NA, # range
                           "Fold", "Fold", "SD", "SD", 
                           "MinMean", "MinMean", "MaxMean", "MaxMean", # used in pk_table function internally 
                           "TrialMeanRange", 
                           # obs values
                           rep(NA, 10)), 
      
      SimulatorNames = c("Mean", "mean", "Geometric Mean", "geometric mean", 
                         "Median", "median", "Geometric CV", "geometric cv",
                         "90% confidence interval around the geometric mean(lower limit)", 
                         "90% confidence interval around the geometric mean(upper limit)", 
                         NA, # concatenated 90% CI
                         "95% confidence interval around the geometric mean(lower limit)", 
                         "95% confidence interval around the geometric mean(upper limit)", 
                         NA, # concatenated 95% CI
                         "5th centile", "95th centile", NA, # concatenated percentiles
                         "Skewness", "skewness",
                         "cv", "Min Val", "min val", "Max Val", "max val", NA, # range
                         "Fold", "fold", "Std Dev", "std dev", 
                         "MinMean", "minMean", "maxmean", "MaxMean", # used in pk_table function internally
                         "TrialMeanRange", 
                         # obs values
                         rep(NA, 10)), 
      
      ReportNames = c(rep("Simulated", 6), # mean through median
                      "CV%", "CV%", 
                      "90% CI - Lower", "90% CI - Upper", 
                      "90% CI", 
                      "95% CI - Lower", "95% CI - Upper", 
                      "95% CI", 
                      "5th Percentile", "95th Percentile", 
                      "5th to 95th Percentile", 
                      NA, NA, # params we don't report
                      "CV%", # this is the arithmetic CV
                      "Minimum", "Minimum", "Maximum", "Maximum", "Range", 
                      NA, NA, # params we don't report
                      "Standard deviation", "Standard deviation",  
                      NA, NA, NA, NA, "Range of trial means", 
                      
                      # obs PK 
                      "Observed", "Observed", "Observed CV%", "Observed CV%", 
                      "observed CI - Lower", "observed CI - Upper",
                      "Observed CI", "Observed range", "S/O", 
                      "S/O range for trial means"))
   
   if(use == "internal"){
      # This is when we're starting from Simulator names and going to R column
      # names
      AllStats <- AllStats %>% filter(SimulatorNames %in% OrigStat)
      StatNames <- AllStats$InternalColNames
      names(StatNames) <- AllStats$SimulatorNames
   } else {
      # This is when we're starting from R column names and going to report
      # names
      AllStats <- AllStats %>% filter(InternalColNames %in% OrigStat)
      StatNames <- AllStats$ReportNames
      names(StatNames) <- AllStats$InternalColNames
   }
   
   return(StatNames[OrigStat])
   
}

