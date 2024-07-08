#' Extract simulated demographic data from the "Demographic Data" tab of a
#' Simcyp Simulator ouptut Excel file
#'
#' @param sim_data_files a character vector of simulator output files, each in
#'   quotes and encapsulated with \code{c(...)}, NA to extract
#'   concentration-time data for \emph{all} the Excel files in the current
#'   folder, or "recursive" to extract concentration-time data for \emph{all}
#'   the Excel files in the current folder and \emph{all} subfolders. Example of
#'   acceptable input: \code{c("sim1.xlsx", "sim2.xlsx")}. The path should be
#'   included with the file names if they are located somewhere other than your
#'   working directory. If some of your Excel files are not regular simulator
#'   output, e.g. they are sensitivity analyses or a file where you were doing
#'   some calculations, those files will be skipped.
#' @param demog_dataframe an existing data.frame of demographic data
#' @param overwrite TRUE or FALSE for whether to overwrite any existing data
#'
#' @return a data.frame of demographic data for simulated subjects. Columns
#'   included: File (simulation results file), Trial, Individual, Population,
#'   Sex (M or F), Age (years), Weight_kg (weight in kg), Height_cm (height in
#'   cm), BSA_m2 (body surface area in meters squared), BrainWt_g (brain weight
#'   in g), KidneyWt_g (kidney weight in g), LiverWt_g (liver weight in g),
#'   BMI_kgm2 (body mass index in kg/meter squared), CardiacOut (cardiac output
#'   in L/h), Haematocrit (percent), HSA_gL (human serum albumin in g/L), AGP_gL
#'   (alpha-1-acid glycoprotein in g/L), Other_uM (user-defined value in uM),
#'   Creatinine_umolL (creatinine in umol/L), GFR_mLminm2 (glomerular filtration
#'   rate in mL/min/m squared of body surface area), RenalFunction (the GFR
#'   divided by the reference GFR, which is 130 mL/min/m2 for male subjects and
#'   120 mL/min/m2 for female subjects), AllometricScalar (allometric scalar
#'   used), and Simulated (TRUE for simulated data).
#' @export
#'
#' @examples
#' # none yet
#' 
extractDemog <- function(sim_data_files = NA, 
                         demog_dataframe = NA, 
                         overwrite = FALSE){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
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
   
   # If user has not included "xlsx" in file name, add that.
   sim_data_files[str_detect(sim_data_files, "xlsx$") == FALSE] <-
      paste0(sim_data_files[str_detect(sim_data_files, "xlsx$") == FALSE], 
             ".xlsx")
   
   sim_data_files <- unique(sim_data_files)
   
   # If user supplied an unquoted object, this checks whether that object
   # exists. However, if they supplied NA or NULL, this throws an error. 
   Recode_demog_dataframe <- suppressWarnings(
      try(exists(deparse(substitute(demog_dataframe))) == FALSE, silent = TRUE))
   
   # If they got an error, then the class of Recode_X will be "try-error", and
   # then we want Recode_X to be TRUE.
   if(suppressWarnings("try-error" %in% class(Recode_demog_dataframe))){
      Recode_demog_dataframe <- TRUE
   }
   
   if(Recode_demog_dataframe){
      demog_dataframe <- "none"
   }
   
   # Checking for existing demographic data
   if(exists(deparse(substitute(demog_dataframe))) && 
      "logical" %in% class(demog_dataframe) == FALSE &&
      "data.frame" %in% class(demog_dataframe) && 
      nrow(demog_dataframe) > 0){
      
      if("File" %in% names(demog_dataframe) == FALSE){
         demog_dataframe$File <- "unknown file"
      }
      
      if(overwrite == FALSE){
         
         DataToFetch <- data.frame(File = sim_data_files) %>% 
            filter(!File %in% demog_dataframe$File)
         
         sim_data_files_topull <- unique(as.character(DataToFetch$File))
         
      } else {
         
         DataToFetch <- data.frame(File = sim_data_files)
         
         sim_data_files_topull <- unique(sim_data_files)
         demog_dataframe <- demog_dataframe %>%
            filter(!File %in% DataToFetch$File)
      }
      
   } else {
      
      # This is when there's no existing data, so we're just getting everything. 
      sim_data_files_topull <- sim_data_files
      demog_dataframe <- data.frame()
      
   }
   
   if(length(sim_data_files_topull) == 0){
      message("There are no data to pull that are not already present in your current data.frame. Returning current data.frame.")
      return(demog_dataframe)
   }
   
   # Making sure that all the files exist before attempting to pull data
   if(any(file.exists(sim_data_files_topull) == FALSE)){
      MissingSimFiles <- sim_data_files_topull[
         which(file.exists(sim_data_files_topull) == FALSE)]
      warning(paste0("The file(s) ", 
                     str_comma(paste0("`", MissingSimFiles, "`")), 
                     " is/are not present or have a file path that is too long and thus cannot be extracted.\n"), 
              call. = FALSE)
      sim_data_files_topull <- setdiff(sim_data_files_topull, MissingSimFiles)
   }
   
   
   # Main body of function --------------------------------------------------
   
   Demog <- list()
   
   for(ff in sim_data_files_topull){
      
      Demog.xl <- tryCatch(readxl::read_xlsx(ff, 
                                             skip = 20, # This should be the same every time. 
                                             sheet = "Demographic Data"), 
                           error = function(x) "glitch")
      
      if("character" %in% class(Demog.xl)){
         warning(paste0("The file `", 
                        ff, 
                        "` is not present or does not have a tab titled `Demographic Data`, so we cannot extract any data for it.\n"), 
                 call. = FALSE)
         next
      }
      
      ColNames <- c("Index" = "Individual", 
                    "Population" = "Population", 
                    "Trial" = "Trial", 
                    "Sex" = "Sex", 
                    "Sex Code" = "SexCode", 
                    "Age (Years)" = "Age", 
                    "Weight (kg)" = "Weight_kg",
                    "Height (cm)" = "Height_cm",
                    "BSA (m²)" = "BSA_m2", 
                    "Brain Weight (g)" = "BrainWt_g", 
                    "Kidney Weight (g)" = "KidneyWt_g",
                    "Liver Weight (g)" = "LiverWt_g", 
                    "BMI (kg/m²)" = "BMI_kgm2", 
                    "Cardiac Output (L/h)" = "CardiacOut", 
                    "Haematocrit (%)" = "Haematocrit", 
                    "HSA (g/L)" = "HSA_gL", 
                    "AGP (g/L)" = "AGP_gL",
                    "Other (µM)" = "Other_uM",
                    "Serum Creatinine (µmol/L)" = "Creatinine_umolL", 
                    "GFR (mL/min/1.73m²)" = "GFR_mLminm2",
                    "GFR (mL/min)" = "GFR_mLmin", 
                    "Renal Function" = "RenalFunction",
                    "Allometric Scalar" = "AllometricScalar", 
                    "Simulation Duration" = "SimDuration")
      
      ColNames <- ColNames[which(names(ColNames) %in% names(Demog.xl))]
      
      MissingColNames <- setdiff(names(Demog.xl), names(ColNames))
      if(length(MissingColNames) > 0){
         warning(paste0("The following column names are present in the demographic data for the file ", 
                        ff, "``, but are not accounted for in this function:\n", 
                        str_c(MissingColNames, collapse =  "\n")), 
                 call. = FALSE)
      }
      
      suppressWarnings(LastRow <- min(which(is.na(Demog.xl$Index))) - 1)
      LastRow <- ifelse(is.infinite(LastRow), nrow(Demog.xl), LastRow)
      
      Demog[[ff]] <- Demog.xl[1:LastRow, names(ColNames)]
      names(Demog[[ff]]) <- ColNames
      
      suppressWarnings(
         Demog[[ff]] <- Demog[[ff]] %>% 
            select(-SexCode, -SimDuration) %>% 
            mutate(across(.cols = any_of(setdiff(ColNames, "Sex")), 
                          .fns = as.numeric), 
                   File = ff, 
                   Simulated = TRUE, 
                   Individual = as.character(Individual), 
                   Trial = as.character(Trial))
      )
      
      rm(Demog.xl, LastRow, ColNames)
      
   }
   
   Out <- bind_rows(demog_dataframe, 
                    bind_rows(Demog)) %>% 
      filter(complete.cases(File)) %>% 
      select(File, Trial, Individual, Population, everything())
   
   return(Out)
   
}


