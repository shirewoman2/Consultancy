#' Create a csv file of dosing rows for XML files
#'
#' \code{create_doses} generates a data.frame of dosing times and amounts -- one
#' for every subject if subject IDs are provided -- based on the dosing regimen
#' specified. This is meant for generating XML files for use in Phoenix WNL.
#' \strong{Special notes for when you have more than one value for some items:}
#' If you have multiple values for anything having to do with the compound --
#' the compound ID, administration route, dose unit, or dose amount (all have
#' the prefix "compound_") -- then all the other arguments having to do with
#' compounds must have that same number of values or must have only one value,
#' which will be repeated as needed. Similarly, if you have multiple values for
#' anything having to do with the subject -- the subject ID, age, weight,
#' height, or sex (all have the prefix "subj_") -- then all the other arguments
#' having to do with subjects must have that same number of values or must have
#' only one value, which will be repeated as needed. Any time you need to
#' specify multiple values, you can  make use of the R function
#' \code{\link{rep}} here to repeat elements of a vector. (See the R coding tip
#' for the argument \code{compound_dose_amount} for an example.)
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
#' @param compound_ID specify the compound that's being dosed. Options are
#'   "Substrate" (default), "Inhibitor 1", "Inhibitor 2", or "Inhibitor 3". Not
#'   case sensitive. If you list more than one compound, you must also list more
#'   than one \code{compound_route}, \code{compound_dose_unit}, and
#'   \code{compound_dose_amount}.
#' @param compound_admin_route the route of administration. Options are "Oral"
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
#' @param subj_ID optionally specify subject IDs as, e.g., \code{subj_ID =
#'   c("101-001", "101-002", "101-003")}.
#' @param subj_age age (years) (optional)
#' @param subj_weight weight (kg) (optional)
#' @param subj_height height (cm) (optional)
#' @param subj_sex sex; options are "F" or "M" (optional)
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
#' create_doses(dose_interval = 24, num_doses = 4)
#'
#' # QD dosing regimen of 100 mg for subjects A, B, and C
#' create_doses(dose_interval = 24, num_doses = 4,
#'                  subj_ID = c("A", "B", "C"))
#'
#' # QD dosing regimen of 100 mg for subjects A, B, and C and save output
#' create_doses(dose_interval = 24, num_doses = 4,
#'                  subj_ID = c("A", "B", "C"),
#'                  save_output = "My doses.csv")
#'
#' # Custom dosing regimen for subjects A, B, and C
#' create_doses(custom_dosing_schedule = c(0, 12, 24, 48, 92, 168),
#'                  subj_ID = c("A", "B", "C"))
#'
#'                   

create_doses <- function(dose_interval = 24, 
                         num_doses = NA,
                         end_time = NA,
                         custom_dosing_schedule = NA,
                         compound_ID = "Substrate",
                         compound_route = "Oral",
                         compound_dose_unit = "mg",
                         compound_dose_amount = 100,
                         subj_ID = NA,
                         subj_age = NA,
                         subj_weight = NA, 
                         subj_height = NA,
                         subj_sex = NA,
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
    
    # Checking for disparate lengths of entered values b/c can result in user
    # NOT getting what they expected
    ArgLengths_subj <- c(length(subj_ID), length(subj_age), length(subj_weight),
                         length(subj_height), length(subj_sex))
    names(ArgLengths_subj) <- c("subj_ID", "subj_age", "subj_weight", 
                                "subj_height", "subj_sex")
    MaxLength_subj <- max(ArgLengths_subj)
    if(all(ArgLengths_subj %in% c(1, MaxLength_subj)) == FALSE){
        stop("There's something screwy with the lengths of the arguments you have specified for the subjects. For example, have you specified 3 subject IDs but then 2 subject weights? All the values for any argument starting with `subj_` must have either only 1 item or must have the same number of multiple items. Please check your input and try again.",
             call. = FALSE)
    }
    
    ArgLengths_cmpd <- c(length(compound_ID), length(compound_route), 
                         length(compound_dose_unit), length(compound_dose_amount))
    names(ArgLengths_cmpd) <- c("compound_ID", "compound_route", 
                                "compound_dose_unit", "compound_dose_amount")
    MaxLength_cmpd <- max(ArgLengths_cmpd)
    if(all(ArgLengths_cmpd %in% c(1, MaxLength_cmpd)) == FALSE){
        stop("There's something screwy with the lengths of the arguments you have specified for the compounds. For example, have you specified 2 compound IDs but then 3 dose administration routes? All the values for any argument starting with `compound_` must have either only 1 item or must have the same number of multiple items. Please check your input and try again.",
             call. = FALSE)
    }
    
    
    # Fixing any case issues
    compound_ID <- str_to_title(compound_ID)
    subj_sex <- str_to_upper(subj_sex)
    compound_route <- str_to_title(compound_route)
    compound_route <- sub("Auto-Detect", "Auto-detect", compound_route)
    
    # Checking for bad input
    if(all(compound_ID %in% c("Substrate", "Inhibitor 1", "Inhibitor 2",
                              "Inhibitor 3")) == FALSE){
        stop("The entry for the argument `compound_ID` is incorrect. The only options for compound_ID are `Substrate`, `Inhiitor `, `Inhibitor 2`, or `Inhibitor 3`.", 
             call. = FALSE)
    }
    
    if(all(compound_route %in% c("Oral", "Intravenous", "Dermal", "Inhaled",
                                 "SC-First Order", "SC-Mechanistic", 
                                 "Auto-detect")) == FALSE){
        stop("The entry for the argument `compound_route` is incorrect. Please check the help file for acceptable options.", 
             call. = FALSE)
    }
    
    if(all(compound_dose_unit %in% c("mg", "mg/m2", "mg/kg")) == FALSE){
        stop("The entry for the argument `compound_dose_unit` is incorrect. Please check the help file for acceptable options.", 
             call. = FALSE)
    }
    
    
    # Main body of function --------------------------------------------------
    
    ## Generate new data.frame with dosing rows ----------------------------
    
    if(all(is.na(custom_dosing_schedule))){
        if(complete.cases(num_doses)){
            DoseTimes <- seq(0, (num_doses - 1) * dose_interval, by = dose_interval)
        } else {
            DoseTimes <- seq(0, end_time, by = dose_interval)
            if(DoseTimes[length(DoseTimes)] %% dose_interval == 0){
                # There wouldn't be a dose at the end_time, so removing that 1.
                DoseTimes <- DoseTimes[1:(length(DoseTimes) - 1)]
            }
        }
    } else {
        DoseTimes <- custom_dosing_schedule
    }
    
    # If there are multiple items for anything dealing with compounds, the
    # number of values specified must be the same as the number of dosing times.
    CmpdTimeCheck <- names(ArgLengths_cmpd)[
        which(ArgLengths_cmpd %in% c(1, length(DoseTimes)) == FALSE)]
    if(length(CmpdTimeCheck) > 0){
        stop(paste0("You may not specify a different number of items for ", 
                    str_comma(CmpdTimeCheck), 
                    " than for the number of doses you have. You have specified ",
                    length(CmpdTimeCheck), " value(s) for ",
                    str_comma(CmpdTimeCheck), 
                    " but ", 
                    length(DoseTimes), " dosing times, based on your input."),
             call. = FALSE)
    }
    
    SubjInfo <- data.frame(Subj_ID = subj_ID, 
                           Subj_age = subj_age, 
                           Subj_weight = subj_weight, 
                           Subj_height = subj_height, 
                           Subj_sex = subj_sex)
    
    CmpdInfo <- data.frame(Compound_ID = compound_ID, 
                           Compound_route = compound_route,
                           Compound_dose_unit = compound_dose_unit,
                           Compound_dose_amount = compound_dose_amount) %>% 
        mutate(Time = DoseTimes)
    
    # Joining the two data.frames.
    Info <- expand_grid(SubjInfo, CmpdInfo)
    
    Out <- Info %>% 
        mutate(DV = "", DVID = "", Weighting = "", 
               InfDur = "", Period = "", 
               Compound_dose_unit = paste0("(", Compound_dose_unit, ")"), 
               Compound_dose_unit = sub("m2", "m²", Compound_dose_unit)) %>% 
        select(Subj_ID, Time, DV, DVID, Weighting, Compound_ID, Compound_route,
               Compound_dose_unit, Compound_dose_amount, 
               InfDur, Period, Subj_age, Subj_weight, Subj_height, Subj_sex) %>% 
        mutate(across(.cols = everything(), 
                      .fns = function(.) {ifelse(is.na(.), 
                                                 as.character(""),
                                                 as.character(.))})) %>% 
        rename("Route of administration" = Compound_route,
               Compound = Compound_ID,
               "DV ID" = DVID,
               "Infusion Duration (min)" = InfDur,
               "Dose Unit" = Compound_dose_unit,
               "Dose Amount" = Compound_dose_amount,
               "Age (year)" = Subj_age,
               Sex = Subj_sex,
               "Weight (kg)" = Subj_weight,
               "Height (cm)" = Subj_height)
    
    
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


