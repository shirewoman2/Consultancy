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
   
   # If they didn't include ".xlsx" or ".xml" at the end, add ".xlsx" for now
   # b/c that's what we used historically and doesn't require the Simcyp
   # package.
   obs_data_file <- case_when(
      str_detect(obs_data_file, "\\.xml$|\\.xlsx$") == FALSE ~ 
         paste0(obs_data_file, ".xlsx"), 
      .default = obs_data_file)
   
   # Make this work for whoever the current user is, even if the XML obs file
   # path was for someone else. This will normalize paths ONLY when the full
   # path is present and starts w/"Users". Otherwise, keeping the original input
   # just b/c I don't want to change the input from basename to full path
   # unexpectedly.
   obs_data_file[str_detect(obs_data_file, "Users")] <- 
      normalizePath(obs_data_file[str_detect(obs_data_file, "Users")], 
                    winslash = "/", mustWork = FALSE)
   
   obs_data_file <- str_replace(obs_data_file, 
                                "Users/(?<=\\/)[^\\/]+(?=\\/)", 
                                paste0("Users/", Sys.info()["user"]))
   
   # Checking that the file exists, that they have a version of Simcyp installed
   # that can possibly get the obs data, etc.
   ObsFileCheck <- data.frame(Orig = obs_data_file) %>% 
      mutate(xlsxFile = sub("\\.xml", ".xlsx", obs_data_file), 
             xmlFile = sub("\\.xlsx", ".xml", obs_data_file), 
             SimcypInstalled = length(find.package("Simcyp", quiet = TRUE)) > 0,
             SimcypV23plus = SimcypInstalled == TRUE && 
                packageVersion("Simcyp") >= "23", 
             xlsxExists = file.exists(xlsxFile), 
             xmlExists = file.exists(xmlFile), 
             # Preferentially using xlsx since that doesn't require Simcyp
             # package
             GoodFile = case_when(xlsxExists == TRUE ~ xlsxFile, 
                                  xlsxExists == FALSE & xmlExists == TRUE & 
                                     SimcypV23plus == TRUE ~ xmlFile, 
                                  .default = NA))
   
   if(is.na(ObsFileCheck$GoodFile)){
      # Using warning instead of stop so that this will pass through to mult
      # function.
      if(ObsFileCheck$xmlExists & ObsFileCheck$SimcypV23plus == FALSE){
         warning(wrapn(paste0("To extract data from the file `", obs_data_file, 
                              "`, you will need V23 or higher of the 'Simcyp' R package, which is not currently installed. We will have to skip this file.")), 
                 call. = FALSE)
         return(data.frame())
         
      } else {
         warning(wrapn(paste0("The file `", obs_data_file, 
                              "` is not present, so it will be skipped.")), 
                 call. = FALSE)
         return(data.frame())
      }
   }
   
   obs_data_file <- ObsFileCheck$GoodFile
   
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
   
   # Call on either xlsx or xml version of sub fun here. 
   if(str_detect(obs_data_file, "\\.xlsx$")){
      obs_data_untidy <- extractObsConcTime_xlsx(obs_data_file)
   } else {
      obs_data_untidy <- extractObsConcTime_XML(obs_data_file)
   }
   
   if(length(obs_data_untidy) == 0){
      warning(wrapn(paste0("The file '", obs_data_file, 
                           "' does not appear to contain observed concentration-time data.")), 
              call. = FALSE)
      return(data.frame())
   }
   
   obs_data <- obs_data_untidy$obs_data
   dose_data <- obs_data_untidy$dose_data
   
   DoseCols <- ObsColNames %>% bind_rows() %>% 
      select(PEColName, ColName, DosingInfo) %>% unique() %>% 
      filter(DosingInfo == TRUE) %>% 
      pull(ColName) %>% unique()
   DoseCols <- paste0(rep(DoseCols, each = 3), 
                      c("_sub", "_inhib", "_inhib2"))
   
   NonDoseCols <- ObsColNames %>% bind_rows() %>% 
      select(PEColName, ColName, DosingInfo) %>% unique() %>% 
      filter(DosingInfo == FALSE) %>% 
      pull(ColName) %>% unique()
   
   # Using dosing info to set DoseNum in conc time data
   if(nrow(dose_data) > 0){
      
      DoseInts <- dose_data %>%
         select(any_of(c("Individual", "CompoundID", "Time", DoseCols))) %>% 
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
                                     include.lowest = TRUE, right = FALSE), 
                      DoseNum = 1:nrow(.))
            
            obs_data[[i]] <- 
               obs_data[[i]] %>% 
               mutate(Interval = cut(Time, 
                                     breaks = c(unique(DoseInts[[j]]$DoseTime), Inf), 
                                     include.lowest = TRUE, right = FALSE)) %>% 
               left_join(DoseInts[[j]] %>% mutate(CompoundID = i_split[1]), 
                         by = join_by(CompoundID, Individual, Interval)) %>% 
               mutate(DoseNum = as.numeric(Interval), 
                      Dose_sub = ifelse("substrate" %in% DoseInts[[j]]$CompoundID, 
                                           DoseInts[[j]]$DoseAmount_sub, NA), 
                      Dose_inhib = ifelse("inhibitor 1" %in% DoseInts[[j]]$CompoundID, 
                                          DoseInts[[j]]$DoseAmount_inhib, NA), 
                      Dose_inhib2 = ifelse("inhibitor 2" %in% DoseInts[[j]]$CompoundID, 
                                           DoseInts[[j]]$DoseAmount_inhib2, NA))
         }
         
         rm(i_split, ParentDrug, j)
      }
   }
   
   obs_data <- bind_rows(obs_data)
   
   
   
   # Tidying and formatting --------------------------------------------------
   
   obs_data <- obs_data %>% 
      mutate(across(.cols = any_of(c("Age", "Weight_kg", "Height_cm",
                                     "SerumCreatinine_umolL", "HSA_gL", 
                                     "Haematocrit", "GestationalAge_wk", 
                                     "PlacentaVol_L", "FetalWt_kg")), 
                    .fns = as.numeric)) %>% 
      select(any_of(c(NonDoseCols, "Species", 
                      "Inhibitor", "Simulated", "Trial", "Tissue", "ObsFile", 
                      "Time_units", "Conc_units", "Dose_sub", "Dose_inhib", 
                      "Dose_inhib2", "DoseNum"))) 
   
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
      any(names(compound_name) %in% AllRegCompounds$CompoundID) == FALSE){
      warning("Some of the compound IDs used for naming the values for `compound_name` are not among the permissible compound IDs, so we won't be able to supply a compound name for any of the compound IDs listed. Please check the help file for what values are acceptable.\n", 
              call. = FALSE)
      
      compound_name <- rep(NA, each = nrow(AllRegCompounds))
      names(compound_name) <- AllRegCompounds$CompoundID
   } else {
      Missing <- setdiff(AllRegCompounds$CompoundID, names(compound_name))
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
      # Need IndivOrAgg to be a column in the data for downstream functions, but
      # we won't know whether obs data are individual or aggregate so setting
      # this to NA.
      mutate(IndivOrAgg = NA) %>% 
      select(any_of(c("Compound", "CompoundID", "Inhibitor", "IndivOrAgg", 
                      "Simulated", 
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

