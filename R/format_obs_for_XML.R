#' Format generic observed data for making an XML overlay file
#'
#' \code{format_obs_for_XML} will convert generic observed concentration-time
#' data into a format that works for pasting into the Simcyp Simulator PE data
#' template, which is then used to generate an observed data overlay XML file.
#' This should be used with one study cohort at at time, e.g., only one dose
#' level and one dosing regimen.
#'
#' @param obs_dataframe observed concentration-time data.frame
#' @param conc_column the column in the observed concentration-time data.frame
#'   that contains concentration data
#' @param time_column the column in the observed concentration-time data.frame
#'   that contains time data
#' @param subject_column the column in the observed concentration-time
#'   data.frame and in the optional demographic data.frame that contains the
#'   subject ID. Note that this means that the subject ID column in the
#'   concentration-time data.frame and in the demographic data.frame \emph{must
#'   match}.
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
#' @return a data.frame
#' @export
#'
#' @examples
#'
#' # None yet
#' 
format_obs_for_XML <- function(obs_dataframe, 
                               subject_column, 
                               DVID_column,
                               time_column, 
                               conc_column, 
                               conc_sd_column, 
                               dose = 100,
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
                               simulator_version = 22,
                               compoundID = "Substrate",
                               demog_dataframe, # should have same column name for subject IDs as in obs_dataframe.
                               age_column, 
                               weight_column, 
                               height_column, 
                               sex_column, 
                               save_output = NA, 
                               return_data = FALSE){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if(str_detect(missing_value_treatment, "impute")){
      ValToImpute <- as.numeric(str_split_1(missing_value_treatment, pattern = " ")[2])
      missing_value_treatment <- "impute"
      if(is.na(ValToImpute)){
         warning("The value you requested for replacing missing values is not a number, so we don't know what value to use. We'll leave missing values alone, which means they'll be removed from the data.\n",
                 call. = FALSE)
         missing_value_treatment <- "as is"
      }
      
   } else if(str_detect(missing_value_treatment, "as is") == FALSE){
      warning("You have requested an option for `missing_value_treatment` that is not `as is` (the default) or imputing a number, which are the only acceptable options. We'll leave missing values alone, which means they'll be removed from the data.\n", 
              call. = FALSE)
      missing_value_treatment <- "as is"
   }
   
   # Main body of function ---------------------------------------------------
   
   ## Setting things up for nonstandard evaluation -------------------------
   conc_column <- rlang::enquo(conc_column)
   conc_sd_column <- rlang::enquo(conc_sd_column)
   time_column <- rlang::enquo(time_column)
   subject_column <- rlang::enquo(subject_column)
   DVID_column <- rlang::enquo(DVID_column)
   age_column <- rlang::enquo(age_column)
   weight_column <- rlang::enquo(weight_column)
   height_column <- rlang::enquo(height_column)
   sex_column <- rlang::enquo(sex_column)
   
   # Converting everything to its non-NSE name b/c NSE is so hard to deal with.
   # Also b/c NSE is so hard to deal with, this next bit takes multiple steps.
   GoodNames_step1 <- c(Conc = rlang::as_label(conc_column),
                        ConcSD = rlang::as_label(conc_sd_column),
                        Time = rlang::as_label(time_column),
                        Subject = rlang::as_label(subject_column), 
                        DVID = rlang::as_label(DVID_column),
                        Age = rlang::as_label(age_column), 
                        Weight = rlang::as_label(weight_column), 
                        Height = rlang::as_label(height_column), 
                        Sex = rlang::as_label(sex_column))
   if(any(duplicated(GoodNames_step1[GoodNames_step1 != "<empty>"]))){
      
      DupCols <- GoodNames_step1[GoodNames_step1 != "<empty>"]
      DupCols <- c(DupCols[duplicated(DupCols)], 
                   DupCols[duplicated(DupCols, fromLast = TRUE)])
      DupCols <- names(DupCols)
      
      stop(paste0("You have assigned the same column to more than one thing, which won't work here. Specifically, you assigned the following columns to more than one thing: ", 
                  str_comma(paste0("`", DupCols, "`"))),
           call. = FALSE)
   }
   
   GoodNames <- names(GoodNames_step1)
   names(GoodNames) <- GoodNames_step1
   
   obs_dataframe <- obs_dataframe %>% 
      bind_rows() %>% 
      ungroup() %>% 
      select(any_of(c(rlang::as_label(conc_column),
                      rlang::as_label(conc_sd_column),
                      rlang::as_label(time_column),
                      rlang::as_label(subject_column), 
                      rlang::as_label(DVID_column),
                      rlang::as_label(age_column), 
                      rlang::as_label(weight_column), 
                      rlang::as_label(height_column), 
                      rlang::as_label(sex_column)))) %>% 
      rename_with(~str_replace_all(., GoodNames)) %>% 
      unique()
   
   ## General data arranging, imputing, etc. --------------------------------
   
   if("Sex" %in% names(obs_dataframe)){
      obs_dataframe <- obs_dataframe %>% 
         mutate(Sex = str_sub(str_to_upper(Sex), 1, 1))
   }
   
   # If they didn't include DVID column, it's probably b/c everything should
   # have DVID of 1.
   if(rlang::as_label(DVID_column) == "<empty>"){
      obs_dataframe$DVID <- 1
   }
   
   # If they didn't include subject column, it's probably b/c everything is mean
   # data.
   if(rlang::as_label(subject_column) == "<empty>"){
      obs_dataframe$Subject <- "mean"
   }
   
   # MissingCols <- setdiff(
   #    CheckNames[CheckNames != "<empty>" &
   #                  names(CheckNames) %in% c("concentration", "time", "subject")],
   #    names(obs_dataframe))
   # 
   # if(length(MissingCols) > 0){
   #    stop(paste0("The column you have listed for the ", 
   #                str_comma(MissingCols),
   #                " data is/are not present in your data.frame. Please check your input and try again."),
   #         call. = FALSE)
   # }
   
   if(missing(demog_dataframe)){
      
      DemogColsInCT <- any(c("Age", "Weight", "Height", "Sex") %in% 
                              names(obs_dataframe))
      
      if(DemogColsInCT){
         demog_dataframe <- obs_dataframe %>% 
            select(any_of(c("Subject", "Age", "Weight", "Height", "Sex"))) %>% 
            unique()
         
      } else {
         demog_dataframe <- 
            bind_cols(obs_dataframe %>% select(Subject) %>% unique(), 
                      as.data.frame(
                         matrix(data = NA, ncol = 4, 
                                dimnames = list(NULL, 
                                                c("Age", "Weight", "Height", "Sex")))))
      }
   } else {
      
      # Converting everything to its non-NSE name b/c NSE is so hard to deal with.
      demog_dataframe <- demog_dataframe %>% 
         select(any_of(c(rlang::as_label(subject_column), 
                         rlang::as_label(age_column), 
                         rlang::as_label(weight_column), 
                         rlang::as_label(height_column), 
                         rlang::as_label(sex_column)))) %>% 
         rename_with(~str_replace_all(., GoodNames)) %>% 
         unique()
      
      if("Sex" %in% names(demog_dataframe)){
         demog_dataframe <- demog_dataframe %>%  
            mutate(Sex = str_sub(str_to_upper(Sex), 1, 1))
      }
      
      if("subj_sex" %in% names(demog_dataframe)){
         demog_dataframe <- demog_dataframe %>%  
            mutate(Sex = str_sub(str_to_upper(subj_sex), 1, 1))
      }
   }
   
   MissingCols <- setdiff(c("Subject", "Age", "Weight", "Height", "Sex"), 
                          names(demog_dataframe))
   
   if(length(MissingCols) > 0){
      demog_dataframe <- demog_dataframe %>% 
         bind_cols(as.data.frame(matrix(data = NA, ncol = length(MissingCols), 
                                        dimnames = list(NULL, 
                                                        MissingCols))))
   }
   
   # Dealing with any imputating requested 
   if(missing_value_treatment == "impute"){
      obs_dataframe$Conc[is.na(obs_dataframe$Conc)] <- ValToImpute
   }
   
   if(set_t0_concs_to_0){
      obs_dataframe <- obs_dataframe %>% 
         mutate(Conc = ifelse(Time == dosing_start_time, 0, Conc))
   }
   
   # Setting up final column names
   NameKey <- c("Subject" = "Subject ID", 
                "Age" = "Age (year)", 
                "Weight" = "Weight (kg)", 
                "Height" = "Height (cm)", 
                "DVID" = "DV ID", 
                "Conc$" = "DV", 
                "ConcSD" = "SD/SE")
   
   FinalObsDF <- obs_dataframe %>% 
      # filtering to remove NAs here but could return to this to
      # allow user to choose what option they want instead. Could
      # make it either remove NAs or set to 0 or some other value
      # s/a 1/2 LLOQ.
      filter(complete.cases(Conc)) %>% 
      select(any_of(c("Subject", "Time", "Conc", "DVID", "ConcSD"))) %>% 
      left_join(demog_dataframe %>% 
                   select(Subject, Age, Height, Weight, Sex), 
                by = "Subject") %>% 
      mutate(across(.cols = everything(), .fns = as.character), 
             DVID = as.factor(DVID)) %>% 
      rename_with( ~ str_replace_all(., NameKey))
   
   if(all(is.na(FinalObsDF$`DV ID`))){
      FinalObsDF$`DV ID` <- 1
   } else if(rlang::as_label(DVID_column) != "<empty>"){
      MyDVIDs <- data.frame(DVIDvalue = levels(FinalObsDF$`DV ID`)) %>% 
         mutate(DVID = 1:nrow(.), 
                Message = paste(DVID, "=", DVIDvalue))
      
      message("DV IDs in the output file are:\n",
              str_c(MyDVIDs$Message, separate = "\n"))
   }
   
   # Only keeping demographic data for subjects who are included in the
   # conc-time data.
   demog_dataframe <- demog_dataframe %>% 
      filter(Subject %in% unique(FinalObsDF$`Subject ID`))
   
   # Including end time if not specified. In that case, just assuming it's the
   # last time in the data.
   if(all(is.na(custom_dosing_schedule)) & 
      is.na(num_doses) & is.na(end_time)){
      end_time <- max(as.numeric(FinalObsDF$Time))
   }
   
   Out <- create_doses(
      dose_interval = dose_interval,
      compound_dosing_start_time = dosing_start_time,
      num_doses = num_doses, 
      end_time = end_time, 
      compound_dose_amount = dose, 
      compound_dose_route = dose_route,
      compound_inf_duration = inf_duration,
      compound_dose_unit = dose_unit,
      subj_ID = demog_dataframe$Subject, 
      subj_age = demog_dataframe$Age, 
      subj_height = demog_dataframe$Height, 
      subj_weight = demog_dataframe$Weight,
      subj_sex = demog_dataframe$Sex, 
      simulator_version = simulator_version) %>% 
      bind_rows(FinalObsDF %>% 
                   mutate(`DV ID` = as.character(as.numeric(`DV ID`)))) %>% 
      mutate(across(.cols = everything(), .fns = as.character), 
             across(.cols = everything(), .fns = function(x) ifelse(is.na(x), "", x)))
   
   
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
   
   if(return_data){ 
      return(Out)
   }
   
}

