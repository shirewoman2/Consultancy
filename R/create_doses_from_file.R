#' Create a csv file of dosing rows for XML files using a study file to get
#' subject information
#'
#' \code{create_doses_from_file} uses an Excel or csv file with subject
#' information to generate a data.frame of dosing times and amounts -- one for
#' every subject if subject IDs are provided -- based on the dosing regimen
#' used. This is meant for generating XML files for use in Phoenix WNL. 
#'
#' @param dose_interval the dosing interval in hours. Default is 24 for a QD
#'   dosing regimen.
#' @param num_doses the number of doses to generate. If this is left as NA, then
#'   the value for \code{end_time} will be used to determine the number of doses
#'   administered.
#' @param end_time the end time of the dosing in hours. If \code{num_doses} is
#'   filled out, that value will be used preferentially.
#' @param custom_dosing_schedule a custom dosing schedule to be used for each
#'   subject in hours, e.g., \code{custom_dosing_schedule = c(0, 12, 24, 168,
#'   180, 192)}; if this is filled out, values in \code{dose_interval},
#'   \code{num_doses}, and \code{end_time} will all be ignored.
#' @param study_file optionally specify a file containing study information.
#'   This will be used for determining: \itemize{
#'
#'   \item{subject IDs,}
#'
#'   \item{compound IDs,}
#'
#'   \item{routes of administration,}
#'
#'   \item{dose units,}
#'
#'   \item{dose amounts,}
#'
#'   \item{ages,}
#'
#'   \item{weights,}
#'
#'   \item{heights,}
#'
#'   \item{and sexes}} for the subjects who will be dosed. If a study file is
#'   specified, the values for this information will \emph{only} be taken from
#'   the file, so you \emph{must} use unquoted column titles for the arguments
#'   for \code{subj_ID}, \code{compound}, \code{admin_route}, \code{dose_unit},
#'   \code{dose_amount}, \code{subj_age}, \code{subj_weight},
#'   \code{subj_height}, and \code{subj_sex}.
#' @param subj_ID optionally specify subject IDs as a) an unquoted column name
#'   in \code{study_file} or b) a vector, e.g., \code{subj_ID = c("101-001",
#'   "101-002", "101-003")}
#' @param compound optionally specify the compound that's being dosed as a) an
#'   unquoted column name in \code{study_file} or b) a vector. Options are
#'   "Substrate" (default if unspecified), "Inhibitor 1", "Inhibitor 2", or
#'   "Inhibitor 3". Not case sensitive.
#' @param admin_route optionally specify the route of administration as a) an
#'   unquoted column name in \code{study_file} or b) a vector. Options are
#'   "Oral" (default if unspecified), "Intravenous", "Dermal", "Inhaled",
#'   "SC-First Order", "SC-Mechanistic", or "Auto-detect". Not case sensitive.
#' @param dose_unit the unit of dosing. Options are "mg" (default), "mg/m2", or
#'   "mg/kg".
#' @param dose_amount the amount of the dose (units are not included). If this
#'   amount varies, please use the \code{custom_dosing_schedule} option and
#'   include one dose amount for each time. For example:
#'   \code{custom_dosing_schedule = c(0, 24, 48), dose_amount = c(100, 50, 50)}
#'   will generate doses of 100 mg at t = 0 and then 50 mg at t = 24 and 48
#'   hours.
#' @param subj_age age (years); either list them manually, enclosed in
#'   \code{c(...)} if there's more than one, or specify the unquoted name of the
#'   column in \code{study_file} to use. Example of input if you specify this
#'   manually: \code{subj_age = c(25, 67, 45)}. Example of input if you are
#'   instead specifying which column in study_file to use: \code{subj_age = Age}
#' @param subj_weight weight (kg); either specify -- unquoted -- which column in
#'   \code{study_file} to use or list specific numeric weights, enclosing
#'   multiple weights with \code{c(...)}. Example of input if you specify this
#'   manually: \code{subj_weight = c(72, 87, 91)}. Example of input if you are
#'   instead specifying which column in study_file to use: \code{subj_weight =
#'   SubjWt}
#' @param subj_height height (cm)
#' @param subj_sex sex; options are "F" or "M"
#' @param subjID_column which column in \code{study_file} contains the subject
#'   IDs; not quoted. This is the only column that matters in \code{study_file}.
#' @param save_output the file name to use for saving the output as a csv; if
#'   left as NA, this will generate a data.frame in R but no output will be
#'   saved.
#'
#' @return
#' @export
#'
#' @examples
#'
#' # QD dosing regimen of 100 mg
#' create_dose_rows(dose_interval = 24, num_doses = 4)
#'
#' # QD dosing regimen of 100 mg for subjects A, B, and C
#' create_dose_rows(dose_interval = 24, num_doses = 4,
#'                  subjIDs = c("A", "B", "C"))
#'
#' # QD dosing regimen of 100 mg for subjects A, B, and C and save output
#' create_dose_rows(dose_interval = 24, num_doses = 4,
#'                  subjIDs = c("A", "B", "C"),
#'                  save_output = "My doses.csv")
#'
#' # Custom dosing regimen for subjects A, B, and C
#' create_dose_rows(custom_dosing_schedule = c(0, 12, 24, 48, 92, 168),
#'                  subjIDs = c("A", "B", "C"))
#'
#'                   

create_dose_rows <- function(dose_interval = 24, 
                             num_doses = NA,
                             end_time = NA,
                             custom_dosing_schedule = NA,
                             study_file = NA,
                             subj_ID = NA,
                             compound = "Substrate",
                             admin_route = "Oral",
                             dose_unit = NA,
                             dose_amount = 100,
                             subj_age = NA,
                             subj_weight = NA, 
                             subj_height = NA,
                             subj_sex = NA,
                             subjIDs = NA,
                             save_output = NA){
    
    # Error catching ---------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
             call. = FALSE)
    }
    
    if(length(dose_interval) > 1){
        stop("Please supply a single value for `dose_interval`. If you need to have more than one dosing interval, please use the `custom_dosing_schedule` option.",
             call. = FALSE)
    }
    
    if(length(num_doses) > 1){
        stop("Please supply a single value for `num_doses`.",
             call. = FALSE)
    }
    
    if(length(end_time) > 1){
        stop("Please supply a single value for `end_time`.",
             call. = FALSE)
    }
    
    if(all(is.na(custom_dosing_schedule))){
        if(is.na(dose_interval)){
            stop("Please supply either a dose interval or a custom dosing schedule.",
                 call. = FALSE)
        } else {
            if(is.na(num_doses) & is.na(end_time)){
                stop("Please supply either the number of doses you want or the end time you want.", 
                     call. = FALSE)
            }
        }
    }
    
    if(complete.cases(num_doses) & complete.cases(end_time)){
        warning("You have supplied values for both `num_doses` and `end_time`. We will use the number of doses requested and ignore anything specified for the end time of dosing.")
    }
    
    # Fixing any case issues w/compound argument
    compound <- str_to_title(compound)
    
    if(any(compound %in% c("Substrate", "Inhibitor 1", "Inhibitor 2",
                           "Inhibitor 3") == FALSE)){
        stop("The entry for the argument `compound` is incorrect. The only options for compound are `Substrate`, `Inhiitor `, `Inhibitor 2`, or `Inhibitor 3`.", 
             call. = FALSE)
    }
    
    
    # Main body of function --------------------------------------------------
    
    ## Read in the existing subject data if the user provided a study_file -----
    if(complete.cases(study_file) && str_detect(study_file, "csv$")){
        StudyDF <- read.csv(study_file)
    } else if(complete.cases(study_file) && str_detect(study_file, "xlsx$")){
        StudyDF <- xlsx::read.xlsx(study_file, sheetIndex = 1)
    } else if(complete.cases(study_file)){
        warning("The `study_file` must be a csv or Excel file, which doesn't appear to be the case here. No subject IDs will be added to your dose rows.")
        StudyDF <- data.frame()
    } else {
        StudyDF <- data.frame()
    }
    
    ## Setting up subjID_column ---------------------------------------------
    subjID_column <- rlang::enquo(subjID_column)
    
    if(as_label(subjID_column) != "<empty>"){
        StudyDF <- StudyDF %>% mutate(subjID_column = {{subjID_column}})
        SubjIDs <- unique(StudyDF$subjID_column)
    } else {
        
        if(complete.cases(study_file)){
            warning("You supplied a file for `study_file` but did not specify which column contains the subject IDs. No subject IDs can be supplied in the output.")
        }
        
        if(any(complete.cases(subjIDs))){
            SubjIDs <- subjIDs
        } else {
            SubjIDs <- c()
        }
    }
    
    ## Generate new data.frame with dosing rows ----------------------------
    
    if(all(is.na(custom_dosing_schedule))){
        if(complete.cases(num_doses)){
            DoseTimes <- seq(0, (num_doses - 1) * dose_interval, by = dose_interval)
        } else {
            DoseTimes <- seq(0, end_time, by = dose_interval)
        }
    } else {
        DoseTimes <- custom_dosing_schedule
    }
    
    Out <- data.frame(Time = DoseTimes, 
                      Dose = dose_amount)
    
    if(length(SubjIDs) > 0){
        Out <- data.frame(SubjectID = SubjIDs,
                          Time = rep(DoseTimes, each = length(SubjIDs)), 
                          Dose = rep(dose_amount, each = length(SubjIDs)))
    }
    
    
    ## Saving & returning output ----------------------------------------------
    if(complete.cases(save_output)){
        
        if(str_detect(basename(save_output), "\\.")){
            # This is when they HAVE specified a file extension. If they
            # specified a file extension that wasn't csv, make that file
            # extension be .csv
            
            if(str_detect(save_output, "\\.csv") == FALSE){
                # Give a warning if they used any file extension other than csv
                # that their file will be saved as csv.
                warning(paste0("You supplied a file extension other than csv, but this function only supports csv output. Your file will be saved as `", 
                               sub("\\..*", ".csv", save_output), "`."), 
                        call. = FALSE)
            }
            
            save_output <- sub("\\..*", ".csv", save_output)
        } else {
            # If they didn't specify a file extension at all, make it .csv. 
            save_output <- paste0(save_output, ".csv")
        }
        
        write.csv(Out, save_output, row.names = FALSE)
        
    }
    
    return(Out)
    
}


