#' INTERNAL: Rename the statistics listed in Simulator output to R-friendly
#' versions
#'
#' \code{renameStats} takes Simulator statistic names such as "90% confidence
#' interval around the geometric mean(lower limit)" and converts them to names
#' that would work for R column names, e.g., "CI90_low". This is mainly meant
#' for internal use in the SimcypConsultancy package to help make sure we're
#' naming things consistently.
#'
#' @param OrigStat the original statistic name
#' @param use options are "Simulator to R" for internal use when renaming stats
#'   from Simulator-output names into R-friendly column names, "report" for
#'   report template tables of PK parameters, or "report to R" when reversing
#'   from report-friendly names to R-friendly names.
#'
#' @return an R-friendly version of that statistic name
#'
#' @examples#' 
#' renameStats(c("Geometric Mean", "Min Val", "5th centile"))
#' 
renameStats <- function(OrigStat){
                        use = "Simulator to R"){
   use <- tolower(use)
   if(use == "simulator to r"){
      MyStats <- AllStats %>% filter(SimulatorNames %in% OrigStat)
      StatNames <- MyStats$InternalColNames
      names(StatNames) <- MyStats$SimulatorNames
   } else if(use == "report"){
      MyStats <- AllStats %>% filter(InternalColNames %in% OrigStat) %>% 
         select(InternalColNames, ReportNames) %>% unique()
      StatNames <- MyStats$ReportNames
      names(StatNames) <- MyStats$InternalColNames
   } else if(use == "report to r"){
      MyStats <- AllStats %>% filter(ReportNames %in% OrigStat) %>% 
         select(ReportNames, InternalColNames) %>% unique()
      StatNames <- MyStats$InternalColNames
      names(StatNames) <- MyStats$ReportNames
    
    StatNames[OrigStat]
}
