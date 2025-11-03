#' Tidy up messy observed concentration-time data. UNDER CONSTRUCTION!!!
#'
#' The function \code{clean_obs_data} is meant to perform the following
#' automated data cleaning steps: \enumerate{\item{Make all the main columns
#' that you probably want in concentration-time data -- Subject, Day, Time, DV,
#' Conttime, etc. -- be the 1st columns listed and have standardized
#' names.}\item{Remove "mg" or "mg/kg" or "mg/m2" from any column with the
#' dose info.} \item{If there's a column for the day and a column for the time, calculate
#' the continuous time.} \item{Set any times < 0 to 0.} \item{Split the data
#' into separate csv files or list items by user-specified columns.}} \strong{A note:}
#' When you specify which columns are which in the arguments, the names must
#' EXACTLY match what R reads in for an Excel file. This can be tricky if there
#' are any spaces or especially any carriage returns in the column headings in
#' an Excel file. Put tick marks around any column names with spaces or special
#' characters, e.g., `Subject ID`. \code{clean_obs_data} will not do
#' \emph{everything} you'll probably need since messy data are messy in their
#' own unique ways in every case, but it will make a start. Once your observed
#' concentration-time data are in good shape, we recommend checking out
#' \code{\link{format_obs_for_XML}} for getting the data into the shape
#' necessary for pasting into a Simcyp Simulator PE template Excel file,
#' including adding dosing rows automatically based your specifications for the
#' dosing interval.
#'
#' @param untidy_data a data.frame to be tidied or a csv file with the untidy
#'   data or an Excel file with the untidy data. This should be in quotes.
#' @param untidy_data_sheet if you supplied an Excel file with the untidy data,
#'   specify what sheet to read here. This should be in quotes.
#' @param cohort_column the column in the observed concentration-time data.frame
#'   that contains the cohorts, unquoted
#' @param subject_column the column in the observed concentration-time
#'   data.frame that contains the subject IDs, unquoted.
#' @param time_column the column in the untidy data.frame that contains times,
#'   unquoted.
#' @param DV_column the column in the untidy data.frame that contains
#'   concentration data, unquoted.
#' @param analyte_column the column in the untidy data.frame that contains the
#'   analytes, unquoted.
#' @param dose_column the column in the untidy data.frame that contains the
#'   doses, unquoted.
#' @param day_column the column in the untidy data.frame that contains the days,
#'   unquoted.
#' @param age_column the column in the untidy data.frame that contains the ages,
#'   unquoted.
#' @param weight_column the column in the untidy data.frame that contains the
#'   weights, unquoted.
#' @param height_column the column in the untidy data.frame that contains
#'   heights, unquoted.
#' @param sex_column the column in the untidy data.frame that contains sexes,
#'   unquoted.
#' @param SDSE_column the column in the untidy data.frame that contains the
#'   standard deviation or standard error, unquoted.
#' @param injection_site_column the column in the untidy data.frame that
#'   contains the injection site, unquoted.
#' @param dose_unit the unit of dosing. Options are "mg" (default), "mg/m2", or
#'   "mg/kg".
#' @param weighting weighting to use for parameter estimation. Defaults to 1.
#' @param split_columns Which columns should the data be split up by? This
#'   \emph{should} be quoted. Apologies for the difference here since the other
#'   column assignments are \emph{un}quoted, but, since this can have multiple
#'   columns, the coding is easiest to do if these are in quotes. We're trying
#'   to figure out how to get around that.
#' @param save_csv optionally specify a file name for saving the tidied data as
#'   a csv. If you have more than one dose level in the data, the files will be
#'   split up by dose level, and we'll include a suffix with the dose amount at
#'   the end of the file name.
#'
#' @return a list of tidied data, split by dose level
#' @export
#' @examples
#' # none yet
#'
#' 
clean_obs_data <- function(untidy_data, 
                           untidy_data_sheet = NA, 
                           cohort_column, 
                           subject_column, 
                           time_column, 
                           DV_column, 
                           analyte_column, 
                           dose_column, 
                           day_column, 
                           age_column, 
                           weight_column, 
                           height_column, 
                           sex_column, 
                           SDSE_column, 
                           injection_site_column, 
                           dose_unit = "mg", 
                           weighting = 1, 
                           split_columns = NA, 
                           save_csv = NA){
   
   # Error catching ---------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
      
   # Loading data ------------------------------------------------------------
   if(str_detect(untidy_data, "\\.csv$")){
      untidy_data <- read.csv(untidy_data)
   } else if(str_detect(untidy_data, "\\.xlsx")){
      untidy_data <- readxl::read_xlsx(untidy_data, 
                                       sheet = untidy_data_sheet)
   } else if("logical" %in% class(untidy_data)){
      stop(str_wrap("You either have not supplied anything for the argument `untidy_data` or you have supplied something we can't figure out how to use. Please check the help file and try again."), 
           call. = FALSE)
   }
   
   if("data.frame" %in% class(untidy_data) == FALSE){
      stop(str_wrap("You either have not supplied anything for the argument `untidy_data` or you have supplied something we can't figure out how to use. Please check the help file and try again."), 
           call. = FALSE)
   }
   
   
   # Main body of function ---------------------------------------------------
   
   cohort_column <- rlang::enquo(cohort_column)
   subject_column <- rlang::enquo(subject_column)
   time_column <- rlang::enquo(time_column)
   DV_column <- rlang::enquo(DV_column)
   analyte_column <- rlang::enquo(analyte_column)
   dose_column <- rlang::enquo(dose_column)
   day_column <- rlang::enquo(day_column)
   age_column <- rlang::enquo(age_column)
   weight_column <- rlang::enquo(weight_column)
   height_column <- rlang::enquo(height_column)
   sex_column <- rlang::enquo(sex_column)
   SDSE_column <- rlang::enquo(SDSE_column)
   injection_site_column <- rlang::enquo(injection_site_column)
   
   GoodCols <- c("Cohort" = as_label(cohort_column), 
                 "Subject" = as_label(subject_column), 
                 "Time" = as_label(time_column),
                 "DV" = as_label(DV_column),
                 "Analyte" = as_label(analyte_column),
                 "Dose" = as_label(dose_column),
                 "Day" = as_label(day_column),
                 "Age" = as_label(age_column), 
                 "Height" = as_label(height_column),
                 "Weight" = as_label(weight_column),
                 "Sex" = as_label(sex_column),
                 "SDSE" = as_label(SDSE_column),
                 "InjectionSite" = as_label(injection_site_column))
   GoodCols <- GoodCols[which(GoodCols != "<empty>")]
   
   tidy_data <- untidy_data
   
   for(col in names(GoodCols)){
      # Dealing with what could otherwise be replicate column names
      if(col %in% names(tidy_data) & GoodCols[col] != col){
         names(tidy_data)[which(names(tidy_data) == col)] <- 
            paste0(col, "_before_tidying")
      }
      
      names(tidy_data)[which(names(tidy_data) == GoodCols[col])] <- col
      
      if(col == "DV"){
         suppressWarnings(
            tidy_data <- tidy_data %>% 
               # Phoenix step: Clean data
               filter(complete.cases(DV)) %>% 
               mutate(DV = as.numeric(DV))
         )
      }
      
      if(col == "Dose"){
         suppressWarnings(
            tidy_data <- tidy_data %>% 
               mutate(Dose = as.numeric(sub("mg|mg/kg|mg/m2", "", Dose)))
         )
      }
      
      if(col == "Time"){
         suppressWarnings(
            tidy_data <- tidy_data %>% 
               mutate(Time = ifelse(as.numeric(Time) < 0, 0, as.numeric(Time)))
         )
      }
      
      if(col == "Day"){
         suppressWarnings(
            tidy_data <- tidy_data %>% 
               mutate(Day = as.numeric(Day))
         )
      }
   }
   
   # Need to note original and revised column names for determining which
   # columns to split by
   ColNames <- data.frame(Orig = names(untidy_data), 
                          Rev = names(tidy_data))
   
   if(all(c("Time", "Day") %in% names(tidy_data))){
      tidy_data <- tidy_data %>% 
         mutate(Conttime = 24 * Day - 24 + Time)
   }
   
   tidy_data <- tidy_data %>% 
      mutate(Weighting = weighting, 
             DoseUnits = dose_unit, 
             DVID = 1) %>% 
      select(any_of(c("Cohort", "Subject", "Day", "Time", "Conttime", "DV", 
                      "Analyte", "Dose", "Day", 
                      "Age", "Weight", "Height", "Sex", "SDSE", 
                      "InjectionSite", "Weighting", "DoseUnits")), 
             everything())
   
   
   ## Split by ... ----------------------------------------------------------
   
   # Finding the columns to split by since their names may have changed. This
   # will omit any columns not actually present in the data.
   split_columns_for_realsies <- ColNames$Rev[ColNames$Orig %in% split_columns]
   
   if(length(split_columns_for_realsies) > 0){
      # Create a list of vectors to split by
      split_list <- lapply(split_columns_for_realsies, function(col) tidy_data[[col]])
      
      # Split the data.frame
      tidy_data <- split(tidy_data, split_list)
   } else {
      tidy_data <- list("alldata" = tidy_data)
   }
   
   # Output ------------------------------------------------------------------
   
   if(complete.cases(save_csv)){
      # If they didn't include ".csv" at the end, add that.
      save_csv <- paste0(sub("\\.csv$|\\.xlsx$", "", save_csv), ".csv")
      
      for(ff in names(tidy_data)){
         if(ff == "alldata"){
            write.csv(tidy_data[[ff]], save_csv, row.names = F, na = "")
         } else {
            write.csv(tidy_data[[ff]], 
                      sub("\\.csv", 
                          paste0(" - ", ff, " ", dose_unit, ".csv"), 
                          save_csv), row.names = F, na = "")
         }
      }
   } else {
      return(tidy_data)
   }
}





