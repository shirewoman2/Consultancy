#' Create a csv file of dosing rows for XML files using a study file to get
#' subject information
#'
#' \code{create_doses_from_file} uses an Excel or csv file with subject
#' information to generate a data.frame of dosing times and amounts -- one for
#' every subject -- based on the dosing regimen specified. WARNING: This doesn't
#' work when there are spaces in the column names in the study file! (I'm
#' looking into whether there's a way around that. -LSh) This is meant for
#' generating XML files for use in Phoenix WNL.\strong{Special notes for when
#' you have more than one value for some items:} If you have multiple values for
#' anything having to do with the compound -- the compound ID, administration
#' route, dose unit, or dose amount (all have the prefix "compound_") -- then
#' all the other arguments having to do with compounds must have that same
#' number of values or must have only one value, which will be repeated as
#' needed. Really, this function will just be easier to use if you run it once
#' for each compound you want. (See the examples at the bottom of the help
#' file.) Any time you need to specify multiple values, you can  make use of the
#' R function \code{\link{rep}} to repeat elements of a vector. (See the R
#' coding tip for the argument \code{compound_dose_amount} for an example.)
#'
#' @param study_file optionally specify a file containing study information.
#'   This will be used for determining subject IDs, ages, weights, heights, and
#'   sexes for the subjects who will be dosed.
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
#' @param compoundID specify the compound that's being dosed. Options are
#'   "Substrate" (default), "Inhibitor 1", "Inhibitor 2", or "Inhibitor 3". Not
#'   case sensitive. If you list more than one compound, you must also list more
#'   than one \code{compound_dose_route}, \code{compound_dose_unit}, and
#'   \code{compound_dose_amount} or list just one of each with the understanding
#'   that they will all be the same.
#' @param compound_dosing_start_time the start time of compound administration
#'   (h); default is 0.
#' @param compound_dose_route the route of administration. Options are "Oral"
#'   (default), "Intravenous", "Dermal", "Inhaled", "SC-First Order",
#'   "SC-Mechanistic", or "Auto-detect". Not case sensitive.
#' @param compound_dose_unit the unit of dosing. Options are "mg" (default),
#'   "mg/m2", or "mg/kg".
#' @param compound_dose_amount the amount of the dose. If this amount varies,
#'   please include one dose amount for each time. For example:
#'   \code{compound_dose_amount = c(100, 50, 50)} will generate doses of 100 mg
#'   for the first dose and then 50 mg for the next two doses. \strong{An R
#'   coding tip:} You don't \emph{have} to list everything multiple times; you
#'   can use the function \code{\link{rep}} to repeat elements. For example,
#'   here's how you could specify that the 1st dose should be 100 mg but the
#'   next 10 doses should be 50: \code{compound_dose_amount = c(100, rep(50,
#'   10))}
#' @param compound_inf_duration the infusion duration (min) (optional)
#' @param subj_ID_column the name of the column in \code{study_file} that
#'   contains subject IDs, unquoted
#' @param subj_age_column the name of the column in \code{study_file} that
#'   contains the subject age (years), unquoted (optional)
#' @param subj_weight_column the name of the column in \code{study_file} that
#'   contains the subject weight (kg), unquoted (optional)
#' @param subj_height_column the name of the column in \code{study_file} that
#'   contains the subject height (cm), unquoted (optional)
#' @param subj_sex_column the name of the column in \code{study_file} that
#'   contains the subject sex, unquoted; options are "F" or "M" (optional)
#' @param save_output the file name to use for saving the output as a csv; if
#'   left as NA, this will generate a data.frame in R but no output will be
#'   saved.
#'
#' @return a data.frame
#' @export
#'
#' @examples
#'
#' # QD dosing regimen of 100 mg
#' create_dose_rows_from_file(study_file = "My dose info.csv",
#'                            dose_interval = 24, num_doses = 4,
#'                            subj_ID_column = Subject)
#'
#'
#' # If you have multiple compounds -- say you've got a DDI study with both a
#' # substrate and an effector -- this will probably be easiest to manage if you
#' # run \code{create_doses_from_file} once for each compound. It just gets pretty
#' # complicated pretty quickly to have a substrate with one dosing interval,
#' # start time, and amount and then an inhibitor with a \emph{different} dosing
#' # interval, start time, and amount. Here's an example of how you could do this
#' # but still get just one csv file at the end:
#'
#' # Substrate is dosed one time at 10 mg starting at t = 168 h.
#' Doses_sub <- create_doses(study_file = "Subject metadata.csv",
#'                           subj_ID_column = SubjectID,
#'                           subj_age_column = Age,
#'                           num_doses = 1, compoundID = "Substrate",
#'                           compound_dosing_start_time = 168,
#'                           compound_dose_amount = 10)
#'
#' # Inhibitor is dosed QD at 500 mg for 336 h starting at t = 0 h.
#' Doses_inhib <- create_doses(study_file = "Subject metadata.csv",
#'                             subj_ID_column = SubjectID,
#'                             subj_age_column = Age,
#'                             dose_interval = 24, end_time = 336,
#'                             compoundID = "Inhibitor 1",
#'                             compound_dosing_start_time = 0,
#'                             compound_dose_amount = 500)
#'
#' MyDoses <- bind_rows(Doses_sub, Doses_inhib)
#' write.csv(MyDoses, file = "Dose rows for sub and inhib.csv",
#'           row.names = FALSE)
#'
#' # Please see more examples with the function "create_doses".
#'
#'                   

create_doses_from_file <- function(study_file,
                                   dose_interval = 24, 
                                   num_doses = NA,
                                   end_time = NA,
                                   custom_dosing_schedule = NA,
                                   compoundID = "Substrate",
                                   compound_dosing_start_time = 0,
                                   compound_dose_route = "Oral",
                                   compound_dose_unit = "mg",
                                   compound_dose_amount = 100,
                                   compound_inf_duration = NA,
                                   subj_ID_column,
                                   subj_age_column,
                                   subj_weight_column, 
                                   subj_height_column,
                                   subj_sex_column,
                                   save_output = NA){
    
    # Error catching ---------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
             call. = FALSE)
    }
    
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
    
    # Main body of function --------------------------------------------------
    
    ## Read in the existing subject data if the user provided a study_file -----
    if(str_detect(study_file, "csv$")){
        StudyDF <- read.csv(study_file)
    } else if(str_detect(study_file, "xlsx$")){
        StudyDF <- xlsx::read.xlsx(study_file, sheetIndex = 1)
    } else {
        stop("You must supply a study file that ends in either .csv or .xlsx")
    }
    
    # Making things unique
    StudyDF <- unique(StudyDF)
    
    ## Setting up subject info ---------------------------------------------
    subj_ID_column <- rlang::enquo(subj_ID_column)
    if(as_label(subj_ID_column) != "<empty>"){
        subj_ID <- StudyDF %>% pull({{subj_ID_column}})
    } else {
            stop("You supplied a file for `study_file` but did not specify which column contains the subject IDs. No subject IDs can be supplied in the output.", 
                 call. = FALSE)
    }
    
    subj_age_column <- rlang::enquo(subj_age_column)
    if(as_label(subj_age_column) != "<empty>"){
        subj_age <- StudyDF %>% pull({{subj_age_column}})
    } else {
        subj_age <- NA
    }
    
    subj_weight_column <- rlang::enquo(subj_weight_column)
    if(as_label(subj_weight_column) != "<empty>"){
        subj_weight <- StudyDF %>% pull({{subj_weight_column}})
    } else {
        subj_weight <- NA
    }
    
    subj_height_column <- rlang::enquo(subj_height_column)
    if(as_label(subj_height_column) != "<empty>"){
        subj_height <- StudyDF %>% pull({{subj_height_column}})
    } else {
        subj_height <- NA
    }
    
    subj_sex_column <- rlang::enquo(subj_sex_column)
    if(as_label(subj_sex_column) != "<empty>"){
        subj_sex <- StudyDF %>% pull({{subj_sex_column}})
    } else {
        subj_sex <- NA
    }
    
    Out <- create_doses(dose_interval = dose_interval,
                        num_doses = num_doses,
                        end_time = end_time, 
                        custom_dosing_schedule = custom_dosing_schedule,
                        compoundID = compoundID,
                        compound_dosing_start_time = compound_dosing_start_time,
                        compound_dose_route = compound_dose_route, 
                        compound_dose_unit = compound_dose_unit, 
                        compound_dose_amount = compound_dose_amount,
                        compound_inf_duration = compound_inf_duration,
                        subj_ID = subj_ID, 
                        subj_age = subj_age,
                        subj_weight = subj_weight,
                        subj_height = subj_height,
                        subj_sex = subj_sex, 
                        save_output = save_output)
    
    return(Out)
    
}
