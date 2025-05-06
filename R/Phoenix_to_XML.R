#' Convert a Phoenix-formatted concentration-time-data file to the format needed
#' for making observed XML overlay files -- UNDER CONSTRUCTION
#'
#' @param Phoenix_file file name
#' @param subject_column the column in the observed concentration-time
#'   data.frame and in the optional demographic data.frame that contains the
#'   subject ID. Note that this means that the subject ID column in the
#'   concentration-time data.frame and in the demographic data.frame \emph{must
#'   match}.
#' @param cohort_column unquoted column name for the cohort (optional). If there
#'   is more than one cohort and you set \code{split_by_cohort} to TRUE, we will
#'   make separate .csv files for each cohort and include the cohort name as a
#'   suffix to the file name.
#' @param split_by_cohort TRUE or FALSE (default) for whether to split the data
#'   into separate .csv files by cohort
#' @param DVID_column the column in the observed concentration-time data.frame
#'   that indicates any time the DV number in the PE file should change. It's
#'   fine to omit this if everything should be "1". If you haven't already
#'   created a column for this with "1", "2", or "3" as the values for the DVID,
#'   you can specify which column you want to use and we'll assign the numbers
#'   for you. For example, if you have some data in plasma and some in blood and
#'   have a column called "Tissue", you'd set this as \code{DVID_column =
#'   Tissue}, and we'll assign one blood to "1" and plasma to "2" (numbers are
#'   automatically assigned alphabetically). If you have an inhibitor present in
#'   some cases but not others, set this to the column with the inhibitor data,
#'   and any time the value changes in \code{obs_dataframe}, the number in the
#'   "DV" column will change in the output. We'll include a message to indicate
#'   which value in the original data was used for which DV number.
#' @param time_column the column in the observed concentration-time data.frame
#'   that contains time data
#' @param conc_column the column in the observed concentration-time data.frame
#'   that contains concentration data
#' @param conc_sd_column the column in the observed concentration-time
#'   data.frame that contains the standard deviation of the concentration
#'   (optional). This will be mapped to the column "SD SE" in the PE data
#'   template file. This only applies for Simcyp Simulator versions >= 22 and
#'   also obviously only applies if you have reported mean concentrations for
#'   the dependent variable.
#' @param dose the amount of the dose. If this amount varies, please include one
#'   dose amount for each time. For example: \code{dose = c(100, 50, 50)} will
#'   generate doses of 100 mg for the first dose and then 50 mg for the next two
#'   doses. \strong{An R coding tip:} You don't \emph{have} to list everything
#'   multiple times; you can use the function \code{\link{rep}} to repeat
#'   elements. For example, here's how you could specify that the 1st dose
#'   should be 100 mg but the next 10 doses should be 50: \code{dose = c(100,
#'   rep(50, 10))}
#' @param dose_unit the unit of dosing. Options are "mg" (default), "mg/m2", or
#'   "mg/kg".
#' @param inf_duration the infusion duration (min) (optional)
#' @param dosing_start_time the start time of compound administration (h);
#'   default is 0.
#' @param set_t0_concs_to_0 TRUE (default) or FALSE for whether to set any
#'   concentrations at t0 for the 1st dose to 0.
#' @param missing_value_treatment How should missing values be dealt with? The
#'   Simcyp PE template will not allow any missing values for concentrations, so
#'   they will be removed from the data automatically. Options for dealing with
#'   missing values: \describe{\item{"as is" (default)}{leaves all data as is. Missing
#'   values will be removed.} \item{"impute X" where X is a number}{Missing
#'   values will be replaced with X.}} If you set \code{set_t0_concs_to_0 = TRUE},
#'   that overrides any options selected here, but only for t0 concentrations.
#' @param dose_route the route of administration. Options are "Oral" (default),
#'   "Intravenous", "Dermal", "Inhaled", "SC-First Order", "SC-Mechanistic", or
#'   "Auto-detect". Not case sensitive.
#' @param dose_interval the dosing interval in hours. Default is NA for a single
#'   dose. Set this to, e.g., 24 for a QD dosing regimen.
#' @param num_doses the number of doses to generate. If this is left as NA and
#'   you have specified the dose interval, then the value for \code{end_time}
#'   will be used to determine the number of doses administered. If this is NA
#'   and so is \code{dose_interval}, we'll assume you want a single dose.
#' @param end_time the end time of the study in hours. If \code{num_doses} is
#'   filled out, that value will be used preferentially. If \code{num_doses},
#'   \code{end_time}, and \code{custom_dosing_schedule} are all NA, we'll use
#'   the maximum time in the data as the end time.
#' @param custom_dosing_schedule a custom dosing schedule to be used for each
#'   subject in hours, e.g., \code{custom_dosing_schedule = c(0, 12, 24, 168,
#'   180, 192)}; if this is filled out, values in \code{dose_interval},
#'   \code{num_doses}, and \code{end_time} will all be ignored.
#' @param simulator_version the version of the simulator that will be used. This
#'   affects what columns will be included in the output.
#' @param compoundID specify the compound that's being dosed. Options are
#'   "Substrate" (default), "Inhibitor 1", "Inhibitor 2", or "Inhibitor 3". Not
#'   case sensitive. If you list more than one compound, you must also list more
#'   than one \code{dose_route}, \code{dose_unit}, and \code{dose} or list just
#'   one of each with the understanding that they will all be the same.
#' @param demog_dataframe a data.frame of demographic information (optional)
#' @param age_column the column in demog_dataframe that contains age data
#' @param weight_column the column in demog_dataframe that contains weight data
#' @param height_column the column in demog_dataframe that contains height data
#' @param sex_column the column in demog_dataframe that contains sex data
#' @param save_output the file name to use for saving the output as a csv; if
#'   left as NA, this will generate a data.frame in R but no output will be
#'   saved.
#' @param return_data TRUE or FALSE (default) for whether to return a data.frame
#'   of the output at the end. If you're only using this to save csv files to
#'   convert into XML overlay files, you probably don't need to get that object
#'   back here, so that's why this argument exists.
#'
#' @returns a data.frame
#' @export
#' 
Phoenix_to_XML <- function(Phoenix_file, 
                           cohort_column, 
                           split_by_cohort = FALSE, 
                           subject_column, 
                           DVID_column, 
                           time_column, 
                           conc_column, 
                           conc_sd_column, 
                           dose = NA, 
                           dose_unit = "mg",
                           inf_duration = NA,
                           dosing_start_time = 0,
                           set_t0_concs_to_0 = TRUE, 
                           missing_value_treatment = "as is",
                           dose_route = "Oral",
                           dose_interval = NA, 
                           num_doses = NA,
                           end_time = NA,
                           custom_dosing_schedule = NA,
                           simulator_version = 23,
                           compoundID = "Substrate",
                           demog_dataframe, # should have same column name for subject IDs as in obs_dataframe.
                           age_column, 
                           weight_column, 
                           height_column, 
                           sex_column, 
                           save_output = NA, 
                           return_data = FALSE){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   suppressMessages(
      PhoenixFile <- readxl::read_excel(path = Phoenix_file, 
                                        col_names = FALSE)
   )
   
   UnitsCol <- any(t(PhoenixFile[2, ]) %in% 
                      c("ng/mL", "h", "mg/L", "mg/mL", "µM", "µg/mL"))
   
   PhoenixDF <- PhoenixFile[ifelse(UnitsCol, 3, 2):nrow(PhoenixFile), ]
   OrigNames <- t(PhoenixFile[1, ]) %>% as.character()
   names(PhoenixDF) <- OrigNames
   names(PhoenixFile) <- OrigNames
   
   # Setting things up for nonstandard evaluation ----------------------------
   
   subject_column <- rlang::enquo(subject_column)
   cohort_column <- rlang::enquo(cohort_column)
   # tissue_column <- rlang::enquo(tissue_column) # NOT USED IN XML FILE
   DVID_column <- rlang::enquo(DVID_column)
   time_column <- rlang::enquo(time_column)
   conc_column <- rlang::enquo(conc_column)
   conc_sd_column <- rlang::enquo(conc_sd_column)
   
   age_column <- rlang::enquo(age_column)
   weight_column <- rlang::enquo(weight_column)
   height_column <- rlang::enquo(height_column)
   sex_column <- rlang::enquo(sex_column)
   
   ColKey <- c("Individual", "Cohort", "Compound", 
               "Time", "Conc", "SD_SE")
   names(ColKey) <- c(as_label(subject_column), as_label(cohort_column),
                      as_label(DVID_column), as_label(time_column), 
                      as_label(conc_column), as_label(conc_sd_column))
   
   ColKey <- ColKey[complete.cases(names(ColKey))]
   
   PhoenixDF <- PhoenixDF %>% 
      select(any_of(c(as_label(cohort_column),
                      as_label(subject_column), 
                      as_label(DVID_column), as_label(time_column),
                      as_label(conc_column)))) %>%
      rename_with(~ str_replace_all(., ColKey)) %>% 
      mutate(across(.cols = any_of(c("Time", "Conc")), 
                    .fns = as.numeric), 
             Time_units = PhoenixFile[2, which(names(PhoenixFile) == 
                                                  as_label(time_column))] %>% 
                as.character(), 
             Conc_units = PhoenixFile[2, which(names(PhoenixFile) == 
                                                  as_label(conc_column))] %>% 
                as.character())
   
   if("Cohort" %in% names(PhoenixDF) == FALSE){
      PhoenixDF$Cohort <- "all"
   }
   
   if(split_by_cohort){
      PhoenixDF <- split(PhoenixDF, PhoenixDF$Cohort)
   } else {
      PhoenixDF <- list(PhoenixDF)
   }
   
   Out <- list()
   
   for(p in names(PhoenixDF)){
      
      if(length(names(PhoenixDF)) > 1 & split_by_cohort){
         message(paste0("Saving cohort '", p, "'"))
      }
      
      Out[[p]] <- format_obs_for_XML(
         obs_dataframe = PhoenixDF[[p]], 
         subject_column = Individual, 
         DVID_column = Compound, 
         time_column = Time, 
         conc_column = Conc, 
         dose = dose, 
         dose_unit = dose_unit, 
         inf_duration = inf_duration,
         dosing_start_time = dosing_start_time,
         set_t0_concs_to_0 = set_t0_concs_to_0, 
         missing_value_treatment = missing_value_treatment, 
         dose_route = dose_route, 
         dose_interval = dose_interval, 
         num_doses = num_doses,
         end_time = end_time,
         custom_dosing_schedule = custom_dosing_schedule,
         simulator_version = simulator_version,
         compoundID = compoundID,
         demog_dataframe = demog_dataframe, 
         age_column = !!age_column, 
         weight_column = !!weight_column, 
         height_column = !!height_column, 
         sex_column = !!sex_column, 
         save_output = ifelse(length(names(PhoenixDF)) > 1, 
                              sub("\\.csv", paste0(" - ", p, ".csv"), save_output), 
                              save_output), 
         return_data = return_data)
   }
   
   Out <- bind_rows(Out, .id = "Cohort")
   
   if(return_data){
      return(Out)
   }
   
}

