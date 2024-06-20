#' Create a csv file of dosing rows for XML files
#'
#' \code{create_doses} generates a data.frame of dosing times and amounts -- one
#' for every subject if subject IDs are provided -- based on the dosing regimen
#' specified. This is meant for generating XML files for use as observed data
#' overlays for the Simcyp Simulator. \strong{Special notes for when you have
#' more than one value for some items:} If you have multiple values for anything
#' having to do with the compound -- the compound ID, administration route, dose
#' unit, or dose amount (all have the prefix "compound_") -- then all the other
#' arguments having to do with compounds must have that same number of values or
#' must have only one value, which will be repeated as needed. Really, this
#' function will just be easier to use if you run it once for each compound you
#' want. (See the examples at the bottom of the help file.) Similarly, if you
#' have multiple values for anything having to do with the subject -- the
#' subject ID, age, weight, height, or sex (all have the prefix "subj_") -- then
#' all the other arguments having to do with subjects must have that same number
#' of values or must have only one value, which will be repeated as needed. Any
#' time you need to specify multiple values, you can make use of the R function
#' \code{\link{rep}} to repeat elements of a vector. (See the R coding tip for
#' the argument \code{compound_dose_amount} for an example.)
#'
#'
#' @param dose_interval the dosing interval in hours. Default is NA for a single
#'   dose. Set this to, e.g., 24 for a QD dosing regimen.
#' @param num_doses the number of doses to generate. If this is left as NA and
#'   you have specified the dose interval, then the value for \code{end_time}
#'   will be used to determine the number of doses administered. If this is NA
#'   and so is \code{dose_interval}, we'll assume you want a single dose.
#' @param end_time the end time of the dosing in hours. If \code{num_doses} is
#'   filled out, that value will be used preferentially.
#' @param custom_dosing_schedule a custom dosing schedule to be used for each
#'   subject in hours, e.g., \code{custom_dosing_schedule = c(0, 12, 24, 168,
#'   180, 192)}; if this is filled out, values in \code{dose_interval},
#'   \code{num_doses}, and \code{end_time} will all be ignored.
#' @param simulator_version the version of the simulator that will be used. This
#'   affects what columns will be included in hte output.
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
#' @return a data.frame
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
#'              subj_ID = c("A", "B", "C"),
#'              save_output = "My doses.csv")
#'
#' # Custom dosing regimen for subjects A, B, and C
#' create_doses(custom_dosing_schedule = c(0, 12, 24, 48, 92, 168),
#'              subj_ID = c("A", "B", "C"))
#'
#'
#' # If you have multiple compounds -- say you've got a DDI study with both a
#' # substrate and a perpetrator -- this will probably be easiest to manage if you
#' # run create_doses once for each compound. It just gets pretty
#' # complicated pretty quickly to have a substrate with one dosing interval,
#' # start time, and amount and then an inhibitor with a *different* dosing
#' # interval, start time, and amount. Here's an example of how you could do this
#' # but still get just one csv file at the end:
#'
#' # Substrate is dosed one time at 10 mg starting at t = 168 h.
#' Doses_sub <- create_doses(num_doses = 1, compoundID = "Substrate",
#'                           compound_dosing_start_time = 168,
#'                           compound_dose_amount = 10)
#'
#' # Inhibitor is dosed QD at 500 mg for 336 h starting at t = 0 h.
#' Doses_inhib <- create_doses(dose_interval = 24, end_time = 336,
#'                             compoundID = "Inhibitor 1",
#'                             compound_dosing_start_time = 0,
#'                             compound_dose_amount = 500)
#'
#' MyDoses <- bind_rows(Doses_sub, Doses_inhib)
#' write.csv(MyDoses, file = "Dose rows for sub and inhib.csv",
#'           row.names = FALSE)
#'
#'                   

create_doses <- function(dose_interval = NA, 
                         num_doses = NA,
                         end_time = NA,
                         custom_dosing_schedule = NA,
                         simulator_version = 22,
                         compoundID = "Substrate",
                         compound_dosing_start_time = 0,
                         compound_dose_route = "Oral",
                         compound_dose_unit = "mg",
                         compound_dose_amount = 100,
                         compound_inf_duration = NA,
                         subj_ID = NA,
                         subj_age = NA,
                         subj_weight = NA, 
                         subj_height = NA,
                         subj_sex = NA,
                         save_output = NA){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
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
   
   if(all(is.na(custom_dosing_schedule)) & 
      is.na(num_doses) & is.na(end_time)){
      stop("Please supply either the number of doses you want or the end time you want.", 
           call. = FALSE)
   }
   
   if(complete.cases(num_doses) & complete.cases(end_time)){
      warning(wrapn("You have supplied values for both `num_doses` and `end_time`. We will use the number of doses requested and ignore anything specified for the end time of dosing."), 
	  call. = FALSE)
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
   
   ArgLengths_cmpd <- c(length(compoundID), length(compound_dose_route), 
                        length(compound_dose_unit), length(compound_dose_amount))
   names(ArgLengths_cmpd) <- c("compoundID", "compound_dose_route", 
                               "compound_dose_unit", "compound_dose_amount")
   MaxLength_cmpd <- max(ArgLengths_cmpd)
   if(all(ArgLengths_cmpd %in% c(1, MaxLength_cmpd)) == FALSE){
      stop("There's something screwy with the lengths of the arguments you have specified for the compounds. For example, have you specified 2 compound IDs but then 3 dose administration routes? All the values for any argument starting with `compound_` must have either only 1 item or must have the same number of multiple items. Please check your input and try again.",
           call. = FALSE)
   }
   
   
   # Fixing any case issues
   compoundID <- str_to_title(compoundID)
   subj_sex <- str_sub(str_to_upper(subj_sex), 1, 1)
   compound_dose_route <- str_to_title(compound_dose_route)
   compound_dose_route <- sub("Auto-Detect", "Auto-detect", compound_dose_route)
   
   # Checking for bad input
   if(all(compoundID %in% c("Substrate", "Inhibitor 1", "Inhibitor 2",
                             "Inhibitor 3")) == FALSE){
      stop("The entry for the argument `compoundID` is incorrect. The only options for compoundID are `Substrate`, `Inhiitor `, `Inhibitor 2`, or `Inhibitor 3`.", 
           call. = FALSE)
   }
   
   if(all(compound_dose_route %in% c("Oral", "Intravenous", "Dermal", "Inhaled",
                                "SC-First Order", "SC-Mechanistic", 
                                "Auto-detect")) == FALSE){
      stop("The entry for the argument `compound_dose_route` is incorrect. Please check the help file for acceptable options.", 
           call. = FALSE)
   }
   
   if(all(compound_dose_unit %in% c("mg", "mg/m2", "mg/kg")) == FALSE){
      stop("The entry for the argument `compound_dose_unit` is incorrect. Please check the help file for acceptable options.", 
           call. = FALSE)
   }
   
   
   # Main body of function --------------------------------------------------
   
   ## Generate new data.frame with dosing rows ----------------------------
   
   if(all(is.na(custom_dosing_schedule))){
      if(complete.cases(num_doses) && num_doses > 1){
         DoseTimes <- seq(0, (num_doses - 1) * dose_interval, by = dose_interval)
      } else if(complete.cases(dose_interval)){
         DoseTimes <- seq(0, end_time, by = dose_interval)
      } else {
         DoseTimes <- 0
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
   
   CmpdInfo <- data.frame(Compound_ID = compoundID, 
                          Compound_route = compound_dose_route,
                          Compound_dose_unit = compound_dose_unit,
                          Compound_dose_amount = compound_dose_amount, 
                          Compound_inf_duration = compound_inf_duration) 
   if(nrow(CmpdInfo) == length(DoseTimes)){
      CmpdInfo <- CmpdInfo %>% mutate(Time = DoseTimes)
   } else {
      CmpdInfo <- expand_grid(CmpdInfo, data.frame(Time = DoseTimes))
   }
   
   # Dealing with possibly varying start times
   MyStartTimes <- data.frame(Compound_ID = compoundID,
                              Compound_start = compound_dosing_start_time) %>% 
      unique()
   
   # Checking that input is reasonable for the compound start times. There
   # should only be one start time for every compound ID.
   StartTimeCheck <- MyStartTimes %>% group_by(Compound_ID) %>% 
      summarize(Nrow = n())
   if(any(StartTimeCheck$Nrow != 1)){
      warning(wrapn("You have listed more than one start time for one of the compounds, so we're not sure which one to use. All start times will be 0."),
              call. = FALSE)
   }
   
   suppressMessages(CmpdInfo <- CmpdInfo %>% left_join(MyStartTimes) %>% 
                       mutate(Time = Time + Compound_start) %>% 
                       select(-Compound_start))
   
   if(complete.cases(end_time)){
      CmpdInfo <- CmpdInfo %>% filter(Time <= end_time)
   }
   
   # Joining the two data.frames.
   Info <- expand_grid(SubjInfo, CmpdInfo)
   
   # Figuring out which columns we need
   # Column names by Simulator version
   ColNames <- list("V22" = c("Subj_ID", "Time", "DV", "DVID", "Weighting",
                              "SD_SE",
                              "Compound_ID", "Compound_route",
                              "Compound_dose_unit", "Compound_dose_amount",
                              "Compound_inf_duration", "InjectionSite",
                              "Period", "Subj_age", "Subj_weight",
                              "Subj_height", "Subj_sex", "SerumCreatinine_umolL",
                              "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                              "SmokingStatus", "GestationalAge_wk", 
                              "PlacentaVol_L", "FetalWt_kg"), 
                    "V21" = c("Subj_ID", "Time", "DV", "DVID", "Weighting",
                              "Compound_ID", "Compound_route", 
                              "Compound_dose_unit", "Compound_dose_amount",
                              "Compound_inf_duration", "Period", 
                              "Subj_age", "Subj_weight",
                              "Subj_height", "Subj_sex", "SerumCreatinine_umolL",
                              "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                              "SmokingStatus", "GestationalAge_wk", 
                              "PlacentaVol_L", "FetalWt_kg"),
                    "V20" = c("Subj_ID", "Time", "DV", "DVID", "Weighting",
                              "Compound_ID", "Compound_route",
                              "Compound_dose_unit", "Compound_dose_amount",
                              "Compound_inf_duration", "Period",
                              "Subj_age", "Subj_weight",
                              "Subj_height", "Subj_sex", "SerumCreatinine_umolL",
                              "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                              "SmokingStatus", "GestationalAge_wk", 
                              "FetalWt_kg"),
                    "V19" = c("Subj_ID", "Time", "DV", "DVID", "Weighting",
                              "Compound_ID", "Compound_route",
                              "Compound_dose_unit", "Compound_dose_amount",
                              "Compound_inf_duration", "Period", 
                              "Subj_age", "Subj_weight",
                              "Subj_height", "Subj_sex", "SerumCreatinine_umolL",
                              "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                              "SmokingStatus")
   )
   
   ColNames <- ColNames[[paste0("V", simulator_version)]]
   
   Out <- Info %>% 
      mutate(across(.cols = everything(), 
                    .fns = function(.) {ifelse(is.na(.), 
                                               as.character(""),
                                               as.character(.))})) %>% 
      mutate(Compound_dose_unit = paste0("(", Compound_dose_unit, ")"), 
             Compound_dose_unit = sub("m2", "m²", Compound_dose_unit))
   
   Out[, setdiff(ColNames, names(Out))] <- ""
   
   # Syntax: currentname = newname
   NameKey <- c("Subj_ID" = "Subject ID",
                "Time" = "Time", 
                "DV" = "DV",
                "DVID" = "DV ID",
                "Weighting" = "Weighting",
                "SD_SE" = "SD SE",
                "Compound_ID" = "Compound",
                "Compound_route" = "Route of administration",
                "Compound_dose_unit" = "Dose Unit",
                "Compound_dose_amount" = "Dose Amount",
                "Compound_inf_duration" = "Infusion Duration (min)",
                "InjectionSite" = "Injection site",
                "Period" = "Period",
                "Subj_age" = "Age (year)",
                "Subj_weight" = "Weight (kg)",
                "Subj_height" = "Height (cm)",
                "Subj_sex" = "Sex",
                "SerumCreatinine_umolL" = "Serum Creatinine (umol/L)",
                "HSA_gL" = "HSA (g/L)",
                "Haematocrit" = "Haematocrit",
                "PhenotypeCYP2D6" = "Phenotype CYP2D6",
                "SmokingStatus" = "Smoking Status",
                "GestationalAge_wk" = "Gestational Age (weeks)", 
                "PlacentaVol_L" = "Placenta Volume (L)",
                "FetalWt_kg" = "Fetal Weight (kg)")
   
   GoodCols <- NameKey[which(names(NameKey) %in% ColNames)]
   Out <- Out[, names(GoodCols)]
   names(Out) <- GoodCols
   
   ## Saving & returning output ----------------------------------------------
   if(complete.cases(save_output)){
      
      if(str_detect(basename(save_output), "\\.")){
         # This is when they HAVE specified a file extension. If they
         # specified a file extension that wasn't csv, make that file
         # extension be .csv
         
         if(str_detect(save_output, "\\.csv") == FALSE){
            # Give a warning if they used any file extension other than csv
            # that their file will be saved as csv.
            warning(wrapn(paste0("You supplied a file extension other than csv, but this function only supports csv output. Your file will be saved as `", 
                           sub("\\..*", ".csv", save_output), "`.")), 
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


