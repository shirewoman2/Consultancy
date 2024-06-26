#' Tidy up messy observed data. UNDER CONSTRUCTION!!!
#'
#' @param untidy_data a data.frame to be tidied or a csv file with the untidy
#'   data or an Excel file with the untidy data. This should be in quotes.
#' @param untidy_data_sheet if you supplied an Excel file with the untidy data,
#'   specify what sheet to read here. This should be in quotes.
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
                           save_csv = NA){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
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
   
   GoodCols <- c("Subject" = as_label(subject_column), 
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
   
   if(all(c("Time", "Day") %in% names(tidy_data))){
      tidy_data <- tidy_data %>% 
         mutate(Conttime = 24 * Day - 24 + Time)
   }
   
   
   tidy_data <- tidy_data %>% 
      mutate(Weighting = weighting, 
             DoseUnits = dose_unit, 
             DVID = 1) %>% 
      select(any_of(c("Subject", "Day", "Time", "Conttime", "DV", "Analyte", "Dose", "Day", 
                      "Age", "Weight", "Height", "Sex", "SDSE", 
                      "InjectionSite", "Weighting", "DoseUnits")), 
             everything())
   
   
   ## Split by dose ----------------------------------------------------------
   
   if("Dose" %in% names(tidy_data) && 
      length(unique(tidy_data$Dose)) > 1){
      tidy_data <- split(tidy_data, f = tidy_data$Dose)
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





