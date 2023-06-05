#' Extract observed concentration-time data from multiple Excel files
#'
#' Extract observed data from Excel files that follow the Simcyp Simulator
#' template for converting concentration-time data into an XML file.
#'
#' @param obs_data_files a character vector of the names of the Excel files
#'   containing the observed concentration-time data, each in quotes, NA
#'   (default) to extract data from all possible observed data Excel files in
#'   that folder, or "recursive" to extract data from all possible observed data
#'   Excel files in the current folder and any subfolders as well. These are the
#'   files that are \emph{ready} to be converted to XML files, not files that
#'   contain only the digitized time and concentration data and not the XML
#'   files themselves that you would include in a Simulator workspace for
#'   observed data. 
#' @param returnDosingInfo TRUE or FALSE (default) for whether to return a
#'   second data.frame with dosing and demographic information from the Excel
#'   file.
#' @param studyID either a single value for the study ID or a named character
#'   vector of which observed data files go with which study IDs. An example of
#'   acceptable input: \code{studyID = "HV 101"} or \code{studyID = c("HV 101" =
#'   "Observed CT 1.xlsx", "HV 102" = "Observed CT 2.xlsx")}. These are
#'   \emph{optional} and for your use only. We ask for them here because this
#'   information can be useful for graphing later.
#' @param study_arm either a single value for the study arm or cohort or a named
#'   character vector of which observed data files go with which arms An example
#'   of acceptable input: \code{study_arm = "SAD 01"} or \code{study_arm =
#'   c("SAD 01" = "Observed CT 1.xlsx", "MAD 05" = "Observed CT 2.xlsx")}. These
#'   are \emph{optional} and for your use only. We ask for them here because
#'   this information can be useful for graphing later.
#'
#' @return a data.frame with the following columns:
#'   \describe{\item{Individual}{the individual ID}
#'
#'   \item{CompoundID}{the compound ID listed in the observed file, e.g., "Sub
#'   Plasma", "Sub PM1 Plasma", "Sub (Inb) Plasma"}
#'
#'   \item{Tissue}{the tissue}
#'
#'   \item{Dose_sub, Dose_inhib, and/or Dose_inhib2}{the dose of the substrate
#'   and any effectors present}
#'
#'   \item{Time}{time since dosing}
#'
#'   \item{Conc}{concentration}
#'
#'   \item{Time_units}{the units of measurement for the time column}
#'
#'   \item{Conc_units}{the units of measurement for the concentration column}
#'
#'   \item{Period, Age, Weight_kg, Height_cm, Sex, SerumCreatinine_umolL,
#'   HSA_gL, Haematocrit, PhenotypeCYP2D6, SmokingStatus}{the columns in the
#'   template for "Period" and "Covariates" but with R-friendly names.
#'   (Currently, no dosing information is pulled because the data format is
#'   different from what we need for other functions related to
#'   concentration-time data.)}}
#'
#' @export
#' @examples
#' extractObsConcTime(obs_data_file = "My observed data.xlsx")
#' 
extractObsConcTime_mult <- function(obs_data_files, 
                                    returnDosingInfo = FALSE, 
                                    studyID = NA, 
                                    study_arm = NA){
   
   # Error catching ---------------------------------------------------
   
   # If user did not supply files, then extract all the files in the current
   # folder that end in "xlsx".
   # If user did not supply files, then extract all the files in the current
   # folder that end in "xlsx" or in all subfolders if they wanted it to be
   # recursive.
   if(length(sim_data_files) == 1 &&
      (is.na(sim_data_files) | sim_data_files == "recursive")){
      sim_data_files <- list.files(pattern = "xlsx$",
                                   recursive = (complete.cases(sim_data_files) &&
                                                   sim_data_files == "recursive"))
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   obs_data_files[str_detect(obs_data_files, "\\.xlsx$") == FALSE] <-
      paste0(obs_data_files[str_detect(obs_data_files, "\\.xlsx$") == FALSE], 
             ".xlsx")
   
   # Making sure that all the files exist before attempting to pull data
   if(any(file.exists(obs_data_files) == FALSE)){
      MissingSimFiles <- obs_data_files[
         which(file.exists(obs_data_files) == FALSE)]
      warning(paste0("The file(s) ", 
                     str_comma(paste0("`", MissingSimFiles, "`")), 
                     " is/are not present, so we cannot extract any information about the simulation experimental details."), 
              call. = FALSE)
      obs_data_files <- setdiff(obs_data_files, MissingSimFiles)
   }
   
   # The Consultancy Team's support group includes Excel files we can ignore.
   # Removing those from consideration.
   obs_data_files <- obs_data_files[!str_detect(obs_data_files, 
                                                "support-docs")]
   
   if(length(studyID) > 1){
      if(is.null(names(studyID))){
         warning("The value for `studyID` must either be NA, a single value, or a named character vector where the names match the observed data files, but you have entered something for `studyID` that lacks names, so we don't know which observed files should be assigned to which study ID.", 
                 call. = FALSE)
         studyID <- NA
      } else {
         studyID <- studyID[names(studyID %in% obs_data_files)]
         if(length(studyID) == 0){
            warning("None of the names of the study IDs matched the observed data files, so we don't know which study IDs to use for which observed data files. Please check your input and the help file.", 
                    call. = FALSE)
            studyID <- NA
         }
      }
   }
   
   if(length(study_arm) > 1){
      if(is.null(names(study_arm))){
         warning("The value for `study_arm` must either be NA, a single value, or a named character vector where the names match the observed data files, but you have entered something for `study_arm` that lacks names, so we don't know which observed files should be assigned to which study ID.", 
                 call. = FALSE)
         study_arm <- NA
      } else {
         study_arm <- study_arm[names(study_arm %in% obs_data_files)]
         if(length(study_arm) == 0){
            warning("None of the names of the study arms matched the observed data files, so we don't know which study arms to use for which observed data files. Please check your input and the help file.", 
                    call. = FALSE)
            study_arm <- NA
         }
      }
   }
   
   
   # Main body of function ---------------------------------------------------
   
   ObsData <- list()
   DosingInfo <- list()
   
   for(i in obs_data_files){
      TEMP <- extractObsConcTime(obs_data_file = i, 
                                 returnDosingInfo = TRUE)
      ObsData[[i]] <- TEMP$ObsCT
      DosingInfo[[i]] <- TEMP$ObsDosing
      
      rm(TEMP)
   }
   
   ObsData <- bind_rows(ObsData %>% unique())
   
   if(complete.cases(studyID)){
      if(length(studyID) == 1){
         ObsData$StudyID <- studyID
      } else {
         MyStudies <- data.frame(StudyID = studyID, 
                                 ObsFile = names(studyID))
         ObsData <- ObsData %>% 
            left_join(MyStudies, by = "ObsFile")
      }
   }
   
   if(complete.cases(study_arm)){
      if(length(study_arm) == 1){
         ObsData$study_arm <- study_arm
      } else {
         MyStudies <- data.frame(study_arm = study_arm, 
                                 ObsFile = names(study_arm))
         ObsData <- ObsData %>% 
            left_join(MyStudies, by = "ObsFile")
      }
   }
   
   Out <- list("ObsData" = ObsData)
   
   if(returnDosingInfo){
      Out[["ObsDosing"]] <- bind_rows(DosingInfo)
   } else {
      Out <- Out[["ObsData"]]
   }
   
   return(ObsData)
}





