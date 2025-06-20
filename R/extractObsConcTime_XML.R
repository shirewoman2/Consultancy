#' INTERNAL - Extract observed conc-time data from an xml file
#'
#' @param obs_data_file xml file used for observed data in a workspace
#'
#' @return a data.frame of observed conc time data
#'

extractObsConcTime_XML <- function(obs_data_file){
   
   SimcypInstalled <- length(find.package("Simcyp", quiet = TRUE)) > 0
   SimcypV23plus <- SimcypInstalled == TRUE && packageVersion("Simcyp") >= "23"
   
   if(SimcypV23plus == FALSE){
      warning(wrapn("We're trying to extract observed data from an XML file, but doing so requires that you have the version 23 or above Simcyp R package installed, which you do not. We cannot return any observed data from your XML file."), 
              call. = FALSE)
      
      return()
   }
   
   readobs.xl_subfun <- function(obs_data_file_from_error){
      if(file.exists(sub("\\.xml$", ".xlsx", obs_data_file))){
         Out <- extractObsConcTime_xlsx(obs_data_file = obs_data_file)
      } else {
         Out <- NULL
      }
      return(Out)
   }
   
   suppressWarnings(
      obs_data_xml <- tryCatch(Simcyp::ReadPEData(path = obs_data_file), 
                               error = readobs.xl_subfun))
   
   if(is.null(obs_data_xml)){
      
      if(packageVersion("Simcyp") >= "24"){
         warning(wrapn(paste0("The observed data file, '", 
                              obs_data_file, "', has non-numeric data for the subject IDs. Currently, a bug in version 24 of the Simcyp package, which we use to extract observed concentration-time data from an XML file, throws an error when this is the case, and that might be the problem here. We have tried to instead extract data from the .xlsx version of the observed data file, but we cannot find it. We will not be able to extract the observed data.")), 
                 call. = FALSE)
      } else {
         warning(wrapn(paste0("We're having trouble for some reason with extacting observed concentration-time data from '", 
                              obs_data_file, "'. If your observed data file does exist and is a typical PE data Excel file, please tell Laura Shireman you saw this message. We will not be able to return any observed data for now.")), 
                 call. = FALSE)
      }
      
      return(obs_data_xml)
   }
   
   obs_data <- obs_data_xml$Observations
   
   NewNames <- ObsColNames %>% bind_rows() %>% 
      select(PEColName, ColName) %>% unique() %>% 
      mutate(DBColName = str_replace_all(PEColName, " ", ""), 
             DBColName = case_match(DBColName, 
                                    "Age(year)" ~ "Age(years)", 
                                    "PhenotypeCYP2D6" ~ "Phenotype(CYP2D6)", 
                                    .default = DBColName)) %>% 
      filter(DBColName %in% names(obs_data))
   # WARNING: This will drop column names that are not matched. 
   obs_data <- obs_data[, NewNames$DBColName]
   names(obs_data) <- NewNames$ColName
   
   obs_data <- obs_data %>%
      left_join(obs_data_xml$DVInfo %>% 
                   mutate(DVID = as.numeric(DVID)), by = "DVID") %>% 
      mutate(DVID = DV, 
             TimeUnits = tolower(TimeUnits), 
             across(.cols = everything(), 
                    .fns = \(x) case_when(x == "" ~ NA, .default = x))) %>% 
      left_join(ObsDVoptions, by = "DVID") %>% 
      rename(Time_units = TimeUnits, 
             Conc_units = DVUnit) %>% 
      mutate(Simulated = FALSE,
             Trial = "obs",
             # SmokingStatus = Smoke[SmokingStatus], # Not available from Simcyp package, apparently
             ObsFile = obs_data_file)
   
   dose_data <-
      obs_data_xml$DosingEvent
   
   # Filling in some values when they're not present, which is apparently the
   # case for really old simulator versions
   if("Compound" %in% names(dose_data) == FALSE){
      dose_data$Compound <- "substrate" # just guessing here!!!
   }
   if("Dose_units" %in% names(dose_data) == FALSE){
      if("DoseUnit" %in% names(dose_data)){
         dose_data$Dose_units <- dose_data$DoseUnit
         dose_data$DoseUnit <- NULL
      } else {
         dose_data$Dose_units <- "mg" # just guessing here!!!
      }
   }
   
   # Need to rename things but account for the fact that some columns might not
   # be present
   NewNames_ch <- NewNames %>% filter(DBColName %in% names(dose_data)) %>% 
      pull(ColName)
   names(NewNames_ch) <- NewNames %>% filter(DBColName %in% names(dose_data)) %>% 
      pull(DBColName)
   
   dose_data <- dose_data %>% unique() %>% 
      rename_with(~ str_replace_all(., NewNames_ch)) %>% 
      mutate(ObsFile = obs_data_file, 
             CompoundID = tolower(Compound), 
             Dose_units = case_when(Dose_units == "mg/mL" ~ "mg", # Bug in Simcyp package
                                    .default = Dose_units),
             across(.cols = everything(), 
                    .fn = as.character)) %>% 
      pivot_longer(cols = any_of(c("DoseAmount", "DoseRoute", "Dose_units", 
                                   "InfDuration", "InjectionSite")), 
                   names_to = "Parameter", 
                   values_to = "Value") %>% 
      left_join(AllRegCompounds %>% select(CompoundID, Suffix), by = "CompoundID") %>% 
      mutate(Parameter = paste0(Parameter, Suffix)) %>% 
      select(-Suffix, -Compound) %>%
      pivot_wider(names_from = Parameter, 
                  values_from = Value) %>% 
      mutate(across(.cols = c(Time, matches("DoseAmount|InfDuration")), 
                    .fns = as.numeric))
   
   return(list("obs_data" = obs_data, 
               "dose_data" = dose_data))
   
}

