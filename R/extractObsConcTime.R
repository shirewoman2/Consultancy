#' Extract observed concentration-time data from an Excel file
#'
#' Extract observed data from an Excel file that follows the Simcyp Simulator
#' template for converting concentration-time data into an XML file.
#'
#' @param obs_data_file name of the Excel file containing the observed
#'   concentration-time data, in quotes. This is the file that is \emph{ready}
#'   to be converted to an XML file, not a file that contains only digitized
#'   time and concentration data and not the XML file itself that you would
#'   include in a Simulator workspace for observed data.
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
#'   "itraconazole". This will be listed in the column "Inhibitor" in the output.
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
#' @return a data.frame or a list of two data.frames. The observed
#'   concentration-time data.frame, which is named "ObsCT" if the output is a
#'   list, has the following columns:
#'   \describe{\item{Individual}{the individual ID}
#'
#'   \item{CompoundID}{the compound ID listed in the observed file, e.g., "Sub
#'   Plasma", "Sub PM1 Plasma", "Sub (Inb) Plasma" will become "substrate" and so on}
#'
#'   \item{Tissue}{the tissue}
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
#'   template for "Period" and "Covariates" but with R-friendly names.}}
#'   If the user requested dosing information, the second item in the list will
#'   be a data.frame titled "ObsDosing" and will include both dosing and
#'   demographic information from the Excel file.
#'
#' @export
#' @examples
#' extractObsConcTime(obs_data_file = "My observed data.xlsx")
#' 
extractObsConcTime <- function(obs_data_file, 
                               compound_name = NA, 
                               perpetrator_name = NA,
                               add_t0 = FALSE, 
                               returnDosingInfo = FALSE){
   
   # Error catching ---------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If they didn't include ".xlsx" at the end, add that. At the same time, if
   # they ended the files with XML, let's try just substituting xlsx for that.
   # We'll check whether that file exists in a moment.
   obs_data_file <- paste0(sub("\\.xml$|\\.xlsx$", "", obs_data_file), 
                           ".xlsx")
   
   # Checking that the file exists.
   if(file.exists(obs_data_file) == FALSE){
      # Using warning instead of stop so that this will pass through to mult function. 
      warning(paste0("The file `", obs_data_file, 
                     "` is not present, so it will be skipped.\n"))
      return(data.frame())
   }
   
   # Checking for file name issues
   CheckFileNames <- check_file_name(obs_data_file)
   BadFileNames <- CheckFileNames[!CheckFileNames == "File name meets naming standards."]
   if(length(BadFileNames)> 0){
      BadFileNames <- paste0(names(BadFileNames), ": ", BadFileNames)
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"), "\n"), 
              call. = FALSE)
   }
   
   # Main body of function -------------------------------------------------
   obs_data_xl <- suppressMessages(
      tryCatch(readxl::read_excel(path = obs_data_file, 
                                  sheet = "PK PD Profiles",
                                  col_names = FALSE),
               error = function(.){
                  warning(paste0("The file `", obs_data_file, 
                                 "` does not appear to be an Excel file of observed data that's ready to be converted to an XML file. We cannot extract any data."), 
                          call. = FALSE)
                  return(list())
               }))
   
   if(length(obs_data_xl) == 0){
      warning(paste("The file", obs_data_file, "does not appear to be an Excel file of observed data that's ready to be converted to an XML file. We cannot extract any data."), 
              call. = FALSE)
      return(list())
   }
   
   # Checking on whether this was animal data
   Animal <- str_detect(obs_data_xl[1, 1], "Animal")
   
   # Getting the meta data on this file
   MetaRowNum <- which(obs_data_xl$...1 == "For all subjects") + 1
   MetaRow <- as.character(obs_data_xl[MetaRowNum, ])
   MetaRow <- MetaRow[1:(which(is.na(MetaRow) | MetaRow == "NA")[1]-1)]
   
   TimeUnits <- tolower(as.character(
      obs_data_xl[MetaRowNum+1, which(MetaRow == "Time Units")]))
   
   CompoundCode <- c("1" = as.character(obs_data_xl[MetaRowNum+1, which(MetaRow == "DV")]),
                     "2" = as.character(obs_data_xl[MetaRowNum+2, which(MetaRow == "DV")]),
                     "3" = as.character(obs_data_xl[MetaRowNum+3, which(MetaRow == "DV")]))
   
   Smoke <- c("0" = as.character(obs_data_xl[MetaRowNum+1, 7]),
              "1" = as.character(obs_data_xl[MetaRowNum+2, 7]),
              "2" = as.character(obs_data_xl[MetaRowNum+3, 7]),
              "3" = as.character(obs_data_xl[MetaRowNum+4, 7]))
   
   # Converting to appropriate ObsConcUnits as necessary
   ObsConcUnits <- c("1" = as.character(obs_data_xl[MetaRowNum+1, which(MetaRow == "DV Unit")]),
                     "2" = as.character(obs_data_xl[MetaRowNum+2, which(MetaRow == "DV Unit")]),
                     "3" = as.character(obs_data_xl[MetaRowNum+3, which(MetaRow == "DV Unit")]))
   
   # Noting tissue
   Tissue <- 
      c(`ADC Plasma Free` = "plasma",
        `ADC Plasma Total` = "plasma", 
        `Adipose (Sub)` = "adipose", 
        `Conjugated Antibody Plasma Free` = "plasma", 
        `Conjugated Antibody Plasma Total` = "plasma",
        `Conjugated Drug Plasma Free` = "plasma", 
        `Conjugated Drug Plasma Total` = "plasma",
        `Conjugated Protein Plasma Free` = "plasma", 
        `Conjugated Protein Plasma Total` = "plasma",
        `Inh 1 Blood` = "blood", 
        `Inh 1 PD Response` = "PD response",
        `Inh 1 Plasma` = "plasma", 
        `Inh 1 Urine` = "urine",
        `Inh 2 Blood` = "blood",
        `Inh 2 Plasma` = "plasma", 
        `Inh1 Met Blood` = "blood",
        `Inh1 Met Plasma` = "plasma",
        `Met (Sub) Urine` = "urine", 
        `Met(Inh 1) Urine` = "urine",
        `Organ Conc` = "solid organ",
        `Organ Conc (Inb)` = "solid organ", 
        `PM1(Sub) PD Response` = "PD response",
        `Spinal CSF (Sub)` = "CSF", 
        `Sub (Inb) Blood` = "blood", 
        `Sub (Inb) PD Response` = "PD response", 
        `Sub (Inb) Plasma` = "plasma",
        `Sub (Inb) Urine` = "urine",
        `Sub Blood` = "blood", 
        `Sub PD Response` = "PD response",
        `Sub Placenta Whole Tissue Conc` = "placenta", 
        `Sub Plasma` = "plasma",
        `Sub Plasma Total Drug` = "plasma", 
        `Sub PM1 Blood` = "blood",
        `Sub PM1 Milk Conc` = "milk",
        `Sub PM1 Plasma` = "plasma", 
        `Sub PM2 Blood` = "blood",
        `Sub PM2 Plasma` = "plasma", `Sub SM blood` = "blood", 
        `Sub SM plasma` = "plasma",
        `Sub Unbound Plasma` = "plasma", 
        `Sub Urine` = "urine",
        `Total Protein Conjugate Plasma Free` = "plasma", 
        `Tumour Volume` = "tumour volume",
        `Tumour Volume (Inb)` = "tumour volume"
      )
   
   ObsCompoundIDs <- 
      c(`ADC Plasma Free` = NA, # "free antibody-drug conjugate",
        `ADC Plasma Total` = NA, # "total antibody-drug conjugate", 
        `Adipose (Sub)` = "substrate",
        `Conjugated Antibody Plasma Free` = NA, # "free conjugated antibody", 
        `Conjugated Antibody Plasma Total` = NA, # "total conjugated antibody", 
        `Conjugated Drug Plasma Free` = NA, # "free conjugated substrate", 
        `Conjugated Drug Plasma Total` = NA, # "total conjugated substrate",
        `Conjugated Protein Plasma Free` = NA, # "free conjugated protein", 
        `Conjugated Protein Plasma Total` = "conjugated protein", # Good as of 11/4/22
        `Inh 1 Blood` = "inhibitor 1", 
        `Inh 1 PD Response` = "inhibitor 1", 
        `Inh 1 Plasma` = "inhibitor 1", 
        `Inh 1 Urine` = "inhibitor 1", 
        `Inh 2 Blood` = "inhibitor 2", 
        `Inh 2 Plasma` = "inhibitor 2", 
        `Inh1 Met Blood` = "inhibitor 1 metabolite", 
        `Inh1 Met Plasma` = "inhibitor 1 metabolite", 
        `Met (Sub) Urine` = "substrate", 
        `Met(Inh 1) Urine` = "inhibitor 1 metabolite", 
        `Organ Conc` = "substrate", 
        `Organ Conc (Inb)` = "substrate", 
        `PM1(Sub) PD Response` = "primary metabolite 1", 
        `Spinal CSF (Sub)` = "substrate", 
        `Sub (Inb) Blood` = "substrate", 
        `Sub (Inb) PD Response` = "substrate", 
        `Sub (Inb) Plasma` = "substrate", 
        `Sub (Inb) Urine` = "substrate", 
        `Sub Blood` = "substrate", 
        `Sub PD Response` = "substrate", 
        `Sub Placenta Whole Tissue Conc` = "substrate", 
        `Sub Plasma` = "substrate", 
        `Sub Plasma Total Drug` = "substrate",
        `Sub Plasma Total Drug` = "total substrate", 
        `Sub PM1 Blood` = "primary metabolite 1", 
        `Sub PM1 Milk Conc` = "primary metabolite 1", 
        `Sub PM1 Plasma` = "primary metabolite 1", # This will become "released payload" if simulation involved an ADC
        `Sub PM2 Blood` = "primary metabolite 2", 
        `Sub PM2 Plasma` = "primary metabolite 2",
        `Sub SM blood` = "secondary metabolite", 
        `Sub SM plasma` = "secondary metabolite",
        `Sub Unbound Plasma` = "substrate", 
        `Sub Urine` = "substrate", 
        `Total Protein Conjugate Plasma Free` = "total protein", # Good as of 11/4/22
        `Tumour Volume` = "tumour volume",
        `Tumour Volume (Inb)` = "tumour volume"
      )
   
   ObsPerpetrators <- 
      c(`ADC Plasma Free` = "none",
        `ADC Plasma Total` = "none",
        `Adipose (Sub)` = "none", 
        `Conjugated Antibody Plasma Free` = "none",
        `Conjugated Antibody Plasma Total` = "none", 
        `Conjugated Drug Plasma Free` = "none",
        `Conjugated Drug Plasma Total` = "none",
        `Conjugated Protein Plasma Free` = "none", 
        `Conjugated Protein Plasma Total` = "none",
        `Inh 1 Blood` = "inhibitor", 
        `Inh 1 PD Response` = "inhibitor", 
        `Inh 1 Plasma` = "inhibitor",
        `Inh 1 Urine` = "inhibitor",
        `Inh 2 Blood` = "inhibitor", 
        `Inh 2 Plasma` = "inhibitor",
        `Inh1 Met Blood` = "inhibitor", 
        `Inh1 Met Plasma` = "inhibitor",
        `Met (Sub) Urine` = "none", 
        `Met(Inh 1) Urine` = "inhibitor",
        `Organ Conc` = "none",
        `Organ Conc (Inb)` = "inhibitor", 
        `PM1(Sub) PD Response` = "none",
        `Spinal CSF (Sub)` = "none", 
        `Sub (Inb) Blood` = "inhibitor",
        `Sub (Inb) PD Response` = "inhibitor", 
        `Sub (Inb) Plasma` = "inhibitor",
        `Sub (Inb) Urine` = "none", 
        `Sub Blood` = "none",
        `Sub PD Response` = "none",
        `Sub Placenta Whole Tissue Conc` = "none", 
        `Sub Plasma` = "none",
        `Sub Plasma Total Drug` = "none",
        `Sub Plasma Total Drug` = "none", 
        `Sub PM1 Blood` = "none",
        `Sub PM1 Milk Conc` = "none",
        `Sub PM1 Plasma` = "none", 
        `Sub PM2 Blood` = "none",
        `Sub PM2 Plasma` = "none",
        `Sub SM blood` = "none", 
        `Sub SM plasma` = "none",
        `Sub Unbound Plasma` = "none",
        `Sub Urine` = "none", 
        `Total Protein Conjugate Plasma Free` = "none",
        `Tumour Volume` = "none", 
        `Tumour Volume (Inb)` = "inhibitor")
   
   ## Saving code for making ObsDVoptions here for convenience. Leave this
   ## commented when running function.
   
   # ObsDVoptions <- data.frame(ID = names(Tissue),
   #                            Tissue = Tissue) %>%
   #     left_join(data.frame(ID = names(ObsCompoundIDs),
   #                          CompoundID = ObsCompoundIDs)) %>%
   #     left_join(data.frame(ID = names(ObsPerpetrators),
   #                          Perpetrator = ObsPerpetrators))
   # 
   # save(ObsDVoptions, file = "data/ObsDVoptions.RData")
   
   HeaderRowNum <- which(obs_data_xl$...1 == "Subject ID")
   MainColNames <- as.character(t(obs_data_xl[HeaderRowNum, ]))
   LastCol <- which(is.na(MainColNames) | MainColNames == "NA")[1]-1
   MainColNames <- MainColNames[1:(ifelse(is.na(LastCol), ncol(obs_data_xl), LastCol))]
   
   obs_data <- obs_data_xl[(HeaderRowNum+1):nrow(obs_data_xl),
                           1:length(MainColNames)]
   
   # Figuring version of simulator b/c col names differ based on that
   if(Animal){
      SimVersion <- "animal"
      names(obs_data) <- c("Individual", "Time", "Conc", "DVID", "Weighting", 
                           "DoseAmount", "InfDuration", "Weight_kg")
      obs_data$SmokingStatus <- NA
      obs_data$Species <- tolower(as.character(
         obs_data_xl[MetaRowNum+1, which(MetaRow == "Species")]))
      
   } else {
      
      if(any(str_detect(MainColNames, "Period"), na.rm = TRUE)){
         if(any(str_detect(MainColNames, "SD/SE"), na.rm = TRUE)){   
            SimVersion <- "V22"
            
            names(obs_data) <- c("Individual", "Time", "Conc", "DVID", "Weighting",
                                 "SD_SE",
                                 "Compound", "DoseRoute", "Dose_units", "DoseAmount",
                                 "InfDuration", "InjectionSite",
                                 "Period", "Age", "Weight_kg",
                                 "Height_cm", "Sex", "SerumCreatinine_umolL",
                                 "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                                 "SmokingStatus", "GestationalAge_wk", 
                                 "PlacentaVol_L", "FetalWt_kg")
            
         } else if(any(str_detect(MainColNames, "Placenta"), na.rm = TRUE)){
            SimVersion <- "V21"
            names(obs_data) <- c("Individual", "Time", "Conc", "DVID", "Weighting",
                                 "Compound", "DoseRoute", "Dose_units", "DoseAmount",
                                 "InfDuration", "Period", "Age", "Weight_kg",
                                 "Height_cm", "Sex", "SerumCreatinine_umolL",
                                 "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                                 "SmokingStatus", "GestationalAge_wk", 
                                 "PlacentaVol_L", "FetalWt_kg")
            
         } else {
            SimVersion <- "V20"
            if(any(str_detect(MainColNames, "Gestational Age"))){
               names(obs_data) <- c("Individual", "Time", "Conc", "DVID", "Weighting",
                                    "Compound", "DoseRoute", "Dose_units", "DoseAmount",
                                    "InfDuration", "Period", "Age", "Weight_kg",
                                    "Height_cm", "Sex", "SerumCreatinine_umolL",
                                    "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                                    "SmokingStatus", "GestationalAge_wk", 
                                    "FetalWt_kg")
               
            } else {
               
               SimVersion <- "V19"
               names(obs_data) <- c("Individual", "Time", "Conc", "DVID", "Weighting",
                                    "Compound", "DoseRoute", "Dose_units", "DoseAmount",
                                    "InfDuration", "Period", "Age", "Weight_kg",
                                    "Height_cm", "Sex", "SerumCreatinine_umolL",
                                    "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                                    "SmokingStatus")
            }
         }
      } else {
         SimVersion <- "preV19"
         names(obs_data) <- c("Individual", "Time", "Conc", "DVID", "Weighting",
                              "Compound", "DoseRoute", "Dose_units", "DoseAmount",
                              "InfDuration", "Age", "Weight_kg",
                              "Height_cm", "Sex", "SerumCreatinine_umolL",
                              "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                              "SmokingStatus")
         
      }
      
      obs_data$Species <- "human"
   }
   
   obs_data <- obs_data %>%
      mutate(across(.cols = any_of(c("Time", "Conc", "SD_SE")), .fns = as.numeric)) %>%
      mutate(CompoundID_obsfile = CompoundCode[as.character(DVID)],
             CompoundID = ObsCompoundIDs[CompoundID_obsfile],
             Inhibitor = ObsPerpetrators[CompoundID_obsfile],
             Simulated = FALSE,
             Trial = "obs",
             Tissue = Tissue[CompoundID_obsfile],
             ObsFile = obs_data_file,
             SmokingStatus = Smoke[SmokingStatus],
             Time_units = TimeUnits,
             Conc_units = ObsConcUnits[as.character(DVID)])
   
   dose_data <- obs_data %>% filter(complete.cases(Compound)) %>% 
      mutate(CompoundID = tolower(Compound), 
             Dose_units = gsub("\\(|\\)", "", Dose_units), 
             Dose = as.numeric(DoseAmount), 
             InfDuration = as.numeric(InfDuration), 
             Dose_sub = case_when(complete.cases(Dose) & 
                                     CompoundID == "substrate" ~ Dose), 
             Dose_inhib = case_when(complete.cases(Dose) & 
                                       CompoundID == "inhibitor 1" ~ Dose), 
             Dose_inhib2 = case_when(complete.cases(Dose) & 
                                        CompoundID == "inhibitor 2" ~ Dose), 
             InfDuration_sub = case_when(complete.cases(InfDuration) & 
                                            CompoundID == "substrate" ~ InfDuration),
             InfDuration_inhib = case_when(complete.cases(InfDuration) & 
                                              CompoundID == "inhibitor 1" ~ InfDuration),
             InfDuration_inhib2 = case_when(complete.cases(InfDuration) & 
                                               CompoundID == "inhibitor 2" ~ InfDuration)) %>% 
      select(any_of(c("Individual", "Species", "Age",
                      "Weight_kg", "Height_cm",
                      "Sex", "SerumCreatinine_umolL", "HSA_gL", 
                      "Haematocrit", "PhenotypeCYP2D6", "SmokingStatus", 
                      "GestationalAge_wk", "PlacentaVol_L", "FetalWt_kg", 
                      "ObsFile", "CompoundID", "Time", "Time_units",
                      "Dose_sub", "Dose_inhib", "Dose_inhib2", "Dose_units",
                      "InfDuration_sub", "InfDuration_inhib", "InfDuration_inhib2"))) 
   
   if(nrow(dose_data) > 0){
      
      obs_data <- obs_data %>%
         # Removing dosing rows b/c that's not conc time data. 
         filter(is.na(DoseAmount)) %>% 
         mutate(Simulated = FALSE) %>% 
         # Removing dose units column b/c it will be NA for all obs data. Need to
         # get that from dose_data.
         select(-Dose_units)
      
      DoseInts <- dose_data %>%
         select(Individual, CompoundID, Time, 
                Dose_sub, Dose_inhib, Dose_inhib2,
                InfDuration_sub, InfDuration_inhib, InfDuration_inhib2,
                Dose_units) %>% 
         rename(DoseTime = Time)
      
      # Adding dose info to conc-time data.frame one CompoundID at a time.
      obs_data <- split(obs_data, f = list(obs_data$CompoundID, 
                                           obs_data$Individual))
      DoseInts <- split(DoseInts, f = list(DoseInts$CompoundID, 
                                           DoseInts$Individual))
      
      for(i in names(obs_data)){
         
         i_split <- str_split_1(i, "\\.")
         
         # For metabolites, we need the parent compound dosing interval info.
         # Dealing with that here.
         ParentDrug <- switch(i_split[1], 
                              "substrate" = "substrate", 
                              "inhibitor 1" = "inhibitor 1", 
                              "inhibitor 2" = "inhibitor 2", 
                              "primary metabolite 1" = "substrate", 
                              "primary metabolite 2" = "substrate", 
                              "secondary metabolite" = "substrate", 
                              "inhibitor 1 metabolite" = "inhibitor 1")
         j <- paste(ParentDrug, i_split[2], sep = ".")
         
         if(j %in% names(DoseInts) && nrow(DoseInts[[j]]) > 0){
            DoseInts[[j]] <- DoseInts[[j]] %>% 
               mutate(Interval = cut(DoseTime, 
                                     breaks = c(unique(DoseInts[[j]]$DoseTime), Inf), 
                                     include.lowest = TRUE, right = FALSE))
            
            obs_data[[i]] <- obs_data[[i]] %>% 
               mutate(Interval = cut(Time, 
                                     breaks = c(unique(DoseInts[[j]]$DoseTime), Inf), 
                                     include.lowest = TRUE, right = FALSE)) %>% 
               left_join(DoseInts[[j]] %>% mutate(CompoundID = i_split[1]), 
                         by = join_by(CompoundID, Individual, Interval)) %>% 
               mutate(DoseNum = as.numeric(Interval))
         }
         
         rm(i_split, ParentDrug, j)
      }
   }
   
   obs_data <- bind_rows(obs_data) %>% 
      mutate(across(.cols = any_of(c("Age", "Weight_kg", "Height_cm",
                                     "SerumCreatinine_umolL", "HSA_gL", 
                                     "Haematocrit", "GestationalAge_wk", 
                                     "PlacentaVol_L", "FetalWt_kg")), 
                    .fns = as.numeric)) %>% 
      select(any_of(c("CompoundID", "Inhibitor", "Tissue", 
                      "Individual", "Trial",
                      "Simulated", "Time", "Conc", "SD_SE",
                      "Time_units",  "Conc_units", 
                      "Dose_sub", "Dose_inhib", "Dose_inhib2",
                      "InfDuration_sub", "InfDuration_inhib", "InfDuration_inhib2",
                      "Dose_units", "DoseNum",
                      "ObsFile", "Period", "Species", "Age", "Weight_kg",
                      "Height_cm", "Sex", "SerumCreatinine_umolL",
                      "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                      "SmokingStatus", "GestationalAge_wk", 
                      "PlacentaVol_L", "FetalWt_kg")))
   
   if(add_t0){
      ToAdd <- obs_data %>% 
         filter(DoseNum == 1) %>% 
         select(-Time, -Conc) %>% unique() %>% 
         mutate(Time = 0, 
                Conc = 0)
      
      obs_data <- obs_data %>% 
         bind_rows(ToAdd) %>% unique() %>% 
         arrange(CompoundID, Inhibitor, Tissue, Individual, Time, Trial, Simulated)
   }
   
   # Tidying compound names
   if(length(compound_name) == 1 & complete.cases(compound_name) & 
      is.null(names(compound_name))){
      compound_name <- c("substrate" = compound_name)
   }
   
   if(any(complete.cases(compound_name)) &&
      any(names(compound_name) %in% AllCompounds$CompoundID) == FALSE){
      warning("Some of the compound IDs used for naming the values for `compound_name` are not among the permissible compound IDs, so we won't be able to supply a compound name for any of the compound IDs listed. Please check the help file for what values are acceptable.\n", 
              call. = FALSE)
      
      compound_name <- rep(NA, each = nrow(AllCompounds))
      names(compound_name) <- AllCompounds$CompoundID
   } else {
      Missing <- setdiff(AllCompounds$CompoundID, names(compound_name))
      ToAdd <- rep(NA, each = length(Missing))
      names(ToAdd) <- Missing
      compound_name <- c(compound_name, Missing)
      rm(Missing, ToAdd)
   }
   
   obs_data$Compound <- compound_name[obs_data$CompoundID]
   
   if(complete.cases(perpetrator_name)){
      obs_data$Inhibitor[obs_data$Inhibitor != "none"] <- perpetrator_name
   }
   
   obs_data <- obs_data %>% 
      select(any_of(c("Compound", "CompoundID", "Inhibitor", "Simulated", 
                      "Tissue", "Individual", "Trial",
                      "Time", "Conc", "SD_SE", "Time_units", "Conc_units")), 
             everything())
   
   if(returnDosingInfo){
      Out <- list("ObsCT" = obs_data,
                  "ObsDosing" = dose_data)
   } else {
      Out <- obs_data
   }
   
   return(Out)
}

