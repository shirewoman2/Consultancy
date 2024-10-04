#' INTERNAL - Extract observed conc-time data from an xlsx file
#'
#' @param obs_data_file xlsx file used to make observed data overlay XML file
#'
#' @return a data.frame
#'
extractObsConcTime_xlsx <- function(obs_data_file){
   
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
      return(data.frame())
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
      
      SimVersion <- map(.x = ObsColNames, 
                        .f = \(x) all(MainColNames %in% x$PEColName))
      SimVersion <- SimVersion[which(SimVersion == TRUE)]
      SimVersion <- names(SimVersion)[1]
      
      names(obs_data) <- ObsColNames[[SimVersion]]$ColName
      obs_data$Species <- "human"
   }
   
   # If there's nothing filled in for DVID, you can still get that to work with
   # the simulator but it will break here. Fill in 1 for any NA values.
   obs_data$DVID[which(is.na(obs_data$DVID) & is.na(obs_data$CompoundID))] <- 1
   
   dose_data <- obs_data %>% filter(complete.cases(DoseRoute)) %>% 
      mutate(Dose_units = gsub("\\(|\\)", "", Dose_units), 
             ObsFile = obs_data_file, 
             CompoundID = tolower(CompoundID)) %>% 
      select(any_of(c("Individual", "ObsFile", "CompoundID", "Time", 
                      ObsColNames[[SimVersion]]$ColName[
                         ObsColNames[[SimVersion]]$DosingInfo == TRUE]))) %>% 
      mutate(across(.cols = everything(), .fns = as.character)) %>% 
      pivot_longer(cols = -c(Individual, ObsFile, CompoundID, DoseAmount, Time), 
                   names_to = "Parameter", 
                   values_to = "Value") %>% 
      mutate(ParameterCmpd = case_when(
         CompoundID %in% AllCompounds$CompoundID[
            AllCompounds$DosedCompoundID == "substrate"] ~ paste0(Parameter, "_sub"), 
         CompoundID %in% AllCompounds$CompoundID[
            AllCompounds$DosedCompoundID == "inhibitor 1"] ~ paste0(Parameter, "_inhib"), 
         CompoundID %in% AllCompounds$CompoundID[
            AllCompounds$DosedCompoundID == "inhibitor 2"] ~ paste0(Parameter, "_inhib2"))) %>% 
      select(-Parameter) %>% 
      pivot_wider(names_from = ParameterCmpd, 
                  values_from = Value) %>% 
      mutate(across(.cols = any_of(c(paste0(rep(c("DoseAmount", "InfDuration", 
                                                  "DoseVol", "DoseConc"), 
                                                each = 3), 
                                            c("_sub", "_inhib", "_inhib2")), 
                                     "Time")), 
                    .fns = as.numeric))
   
   obs_data <- obs_data %>%
      select(-CompoundID) %>% 
      mutate(across(.cols = any_of(c("Time", "Conc", "SD_SE")), .fns = as.numeric), 
             DVID_num = DVID, 
             DVID = CompoundCode[as.character(DVID)]) %>% 
      left_join(ObsDVoptions, by = "DVID") %>% 
      mutate(Simulated = FALSE,
             Trial = "obs",
             ObsFile = obs_data_file,
             SmokingStatus = Smoke[SmokingStatus],
             Time_units = TimeUnits,
             Conc_units = as.character(ObsConcUnits[DVID_num])) %>% 
      # Removing dosing rows b/c that's not conc time data. 
      filter(is.na(DoseAmount)) %>% 
      mutate(Simulated = FALSE) %>% 
      # Removing data that should ONLY be in dose_data so that they don't
      # mess up joining later
      select(-any_of(ObsColNames[[SimVersion]]$ColName[
         ObsColNames[[SimVersion]]$DosingInfo == TRUE]))
   
   
   return(list("obs_data" = obs_data, 
               "dose_data" = dose_data))
   
}

