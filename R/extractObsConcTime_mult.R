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
#' @param compound_name the name of the compound, e.g., "midazolam". If you have
#'   more than one compound that you want to specify -- for example, the data
#'   include both the substrate and primary metabolite 1 -- you can specify them
#'   with a named character vector
#'   like this: \code{compound_name = c("substrate" = "midazolam", "primary
#'   metabolite 1" = "OH-midazolam")}. All
#'   possible compound IDs permissible here: "substrate", "primary metabolite
#'   1", "primary metabolite 2", "secondary metabolite", "inhibitor 1",
#'   "inhibitor 2", or "inhibitor 1 metabolite".
#' @param perpetrator_name the name of the perpetrator, where applicable, e.g.,
#'   "itraconazole". This will be listed in the column "Inhibitor" in the
#'   output.
#' @param add_t0 TRUE or FALSE (default) for whether to add t0 points if they're
#'   missing. Sometimes, observed data do not include a measurement at t0
#'   because, presumably, the concentration should always be 0 at that time. If
#'   you're using these data to calculate PK, though, you'll miss that initial
#'   part of the AUC if t0 is missing. If \code{add_t0} is set to TRUE, this
#'   will add a concentration of 0 and time 0 for all of the individual
#'   concentration-time profiles.
#' @param returnDosingInfo TRUE or FALSE (default) for whether to return a
#'   second data.frame with dosing and demographic information from the Excel
#'   file.
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
#'   and any perpetrators present}
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
extractObsConcTime_mult <- function(obs_data_files = NA, 
                                    compound_name = NA, 
                                    perpetrator_name = NA,
                                    returnDosingInfo = FALSE){
   
   # Error catching ---------------------------------------------------
   
   obs_data_files_input <- obs_data_files
   
   # If user did not supply files, then extract all the files in the current
   # folder that end in "xlsx".
   # If user did not supply files, then extract all the files in the current
   # folder that end in "xlsx" or in all subfolders if they wanted it to be
   # recursive.
   if(length(obs_data_files) == 1 &&
      (is.na(obs_data_files_input) | obs_data_files_input == "recursive")){
      obs_data_files <- list.files(pattern = "xlsx$",
                                   recursive = (complete.cases(obs_data_files_input) &&
                                                   obs_data_files == "recursive"))
      obs_data_files <- obs_data_files[!str_detect(obs_data_files, "^~")]
   }
   
   # The Consultancy Team's support group includes Excel files we can ignore.
   # Removing those from consideration if we're collecting file names in an
   # automated fashion.
   if(all(is.na(obs_data_files_input)) || 
      all(obs_data_files_input == "recursive")){
      obs_data_files <- obs_data_files[!str_detect(obs_data_files, 
                                                   "support-docs")]
   }
   
   # If they didn't include ".xlsx" or ".xml" at the end, add ".xlsx" for now
   # b/c that's what we used historically and doesn't require the Simcyp
   # package.
   obs_data_files <- case_when(
      str_detect(obs_data_files, "\\.xml$|\\.xlsx$") == FALSE ~ 
         paste0(obs_data_files, ".xlsx"), 
      .default = obs_data_files)
   
   # Make this work for whoever the current user is, even if the XML obs file
   # path was for someone else. This will normalize paths ONLY when the full
   # path is present and starts w/"Users". Otherwise, keeping the original input
   # just b/c I don't want to change the input from basename to full path
   # unexpectedly.
   obs_data_files[which(str_detect(obs_data_files, "Users"))] <- 
      normalizePath(obs_data_files[which(str_detect(obs_data_files, "Users"))], 
                    winslash = "/", mustWork = FALSE)
   
   obs_data_files <- str_replace(obs_data_files, 
                                 "Users/(?<=\\/)[^\\/]+(?=\\/)", 
                                 paste0("Users/", Sys.info()["user"]))
   
   # Checking that the file exists.
   FileCheck <- data.frame(OrigFile = obs_data_files) %>% 
      mutate(Exists = file.exists(OrigFile), 
             File_xlsx = sub("\\.xml$", ".xlsx", OrigFile), 
             File_xml = sub("\\.xlsx$", ".xml", OrigFile), 
             Exists_xlsx = file.exists(File_xlsx), 
             Exists_xml = file.exists(File_xml), 
             FileToUse = case_when(Exists ~ OrigFile, 
                                   Exists_xlsx ~ File_xlsx, 
                                   Exists_xml ~ File_xml)) %>% 
      filter(complete.cases(OrigFile))
   
   if(any(is.na(FileCheck$FileToUse))){
      warning(wrapn(paste0(
         "The file(s) ", str_comma(
            paste0("'", FileCheck$OrigFile[which(is.na(FileCheck$FileToUse))], "'")), 
         " is/are not present and will be skipped.")), 
         call. = FALSE)
   }
   
   obs_data_files <- FileCheck$FileToUse
   
   # Main body of function ---------------------------------------------------
   
   ObsData <- list()
   DosingInfo <- list()
   
   for(i in obs_data_files){
      TEMP <- extractObsConcTime(obs_data_file = i, 
                                 compound_name = compound_name, 
                                 perpetrator_name = perpetrator_name,
                                 returnDosingInfo = TRUE)
      ObsData[[i]] <- TEMP$ObsCT
      DosingInfo[[i]] <- TEMP$ObsDosing
      
      rm(TEMP)
   }
   
   ObsData <- bind_rows(ObsData %>% unique())
   
   Out <- list("ObsData" = ObsData)
   
   if(returnDosingInfo){
      Out[["ObsDosing"]] <- bind_rows(DosingInfo)
   } else {
      Out <- Out[["ObsData"]]
   }
   
   return(ObsData)
}





