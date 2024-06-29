#' Extract fm and fe valuesthat change with time from a Simulator output Excel
#' file
#'
#' \code{extractFmFe} extracts time-dependent fm and fe data from a simulator
#' output Excel file. A tab named something like "Time variant \%fm and fe" must
#' be present.
#'
#' @param sim_data_file name of the Excel file containing the simulated dynamic
#'   fm and fe data, in quotes
#' @param returnOnlyMaxMin TRUE (default) or FALSE for whether to return only
#'   maximum and minimum fm values -- basically, return the table in the upper
#'   left corner of the "Time variant \%fm and fe" tab.
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated fm
#'   and fe data? Options are "individual", "aggregate", or "both" (default).
#'   Aggregated data are not calculated here but are pulled from the simulator
#'   output rows labeled as "mean".
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#'
#' @return a data.frame
#'
#' @export
#' @examples
#' # None yet
#' 


extractFmFe <- function(sim_data_file,
                        returnOnlyMaxMin = TRUE, 
                        returnAggregateOrIndiv = "both", 
                        existing_exp_details = NA){
   
   # Error catching --------------------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   sim_data_file <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$", "", sim_data_file), ".xlsx")
   
   # Checking for file name issues
   CheckFileNames <- check_file_name(sim_data_file)
   BadFileNames <- CheckFileNames[!CheckFileNames == "File name meets naming standards."]
   if(length(BadFileNames)> 0){
      BadFileNames <- paste0(names(BadFileNames), ": ", BadFileNames)
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"), "\n"),
              call. = FALSE)
   }
   
   if(any(c(length(returnAggregateOrIndiv) < 1,
            length(returnAggregateOrIndiv) > 2,
            any(unique(returnAggregateOrIndiv) %in% c("aggregate", "individual", "both") == FALSE)))) {
      stop("returnAggregateOrIndiv must be 'aggregate', 'individual', or 'both'.",
           call. = FALSE)
   }
   
   
   # Main body of function ----------------------------------------------------------------
   
   # Getting summary data for the simulation(s)
   
   if("logical" %in% class(existing_exp_details)){
      Deets <- extractExpDetails(sim_data_file,
                                 exp_details = "Summary and Input")[["MainDetails"]]
      
   } else {
      
      Deets <- filter_sims(existing_exp_details, sim_data_file, "include")
      Deets <- harmonize_details(Deets)[["MainDetails"]] %>% 
         filter(File == sim_data_file)
      
      if(nrow(Deets) == 0){
         Deets <- extractExpDetails(sim_data_file,
                                    exp_details = "Summary and Input")[["MainDetails"]]
      }
   }
   
   if(Deets$PopRepSim == "Yes"){
      warning(paste0("The simulator file supplied, `", 
                     sim_data_file, 
                     "`, is for a population-representative simulation and thus doesn't have any aggregate data. Please be warned that some plotting functions will not work well without aggregate data.\n"),
              call. = FALSE)
   }
   
   # Figuring out which sheet to extract and dealing with case since that
   # apparently changes between Simulator versions.
   AllSheets <- readxl::excel_sheets(sim_data_file)
   SheetToExtract <- AllSheets[str_detect(tolower(AllSheets), "time variant .fm and fe")]
   
   if(length(SheetToExtract) == 0){
      warning(paste0("The simulator output file provided, `", 
                     sim_data_file, 
                     "``, does not appear to have a sheet titled `Time variance %fm and fe`, which is what we need for extracting dynamic fm and fe values.\n"),
              call. = FALSE)
      return(data.frame())
   }
   
   # Reading in simulated abundance-time profile data
   sim_data_xl <- suppressMessages(
      readxl::read_excel(path = sim_data_file,
                         sheet = SheetToExtract,
                         range = switch(as.character(returnOnlyMaxMin), 
                                        "TRUE" = "A1:E50", # Guessing that there wouldn't be more than 50 lines here... Reasonable? 
                                        "FALSE" = NULL),
                         col_names = FALSE))
   
   # Extracting only max and min --------------------------------------------
   
   if(returnOnlyMaxMin){
      
      EndRow <- which(is.na(sim_data_xl$...1[3:nrow(sim_data_xl)]))[1] + 1
      
      suppressMessages(
         Out <- sim_data_xl[3:EndRow, 1:5] %>% 
            rename(Max = ...2, 
                   tmax = ...3, 
                   Min = ...4, 
                   tmin = ...5) %>% 
            mutate(Tissue = str_extract(tolower(...1), "liver|kidney|renal"), 
                   Enzyme = str_extract(...1, "(CYP|UGT)[1-9][ABCDEFJ][1-9]{1,2}|Additional"), 
                   PerpPresent = str_detect(tolower(...1), "with inh"), 
                   Parameter = str_extract(...1, "fm|fe"), 
                   across(.cols = c(Max, tmax, Min, tmin), 
                          .fns = as.numeric), 
                   File = sim_data_file) %>% 
            select(Parameter, Tissue, Enzyme, PerpPresent, Max, tmax, Min, tmin, File)
      )
      
      return(Out)
   }
   
   # Extracting aggregate data ---------------------------------------------
   if(any(c("aggregate", "both") %in% returnAggregateOrIndiv)){
      
      StartRow_agg <- which(sim_data_xl$...1 == "Population Statistics")
      TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
      TimeRow <- TimeRow[TimeRow > StartRow_agg][1]
      
      # Figuring out which rows contain which data
      FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                              which(1:nrow(sim_data_xl) > TimeRow))[1]
      FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
      NamesToCheck <- sim_data_xl$...1[TimeRow[1]:(FirstBlank-1)]
      
      RowsToUse <- which(str_detect(tolower(NamesToCheck), 
                                    "\\((liver|kidney|renal)\\)"))
      
      IDs_agg <- data.frame(ColOrig = paste0("V", 1:(length(RowsToUse) + 1)), 
                            ID = c("Time", NamesToCheck[RowsToUse])) %>% 
         # NOTE: Just guessing for now what any inhibitors might be named b/c
         # I don't have an example file for that atm.
         mutate(CompoundID = str_extract(tolower(ID), "sub( met)?|sub( pri met[12])?|inhib"), 
                CompoundID = case_when(CompoundID == "sub" ~ "substrate", 
                                       CompoundID == "sub met" ~ "primary metabolite 1", 
                                       CompoundID == "sub pri met1" ~ "primary metabolite 1", 
                                       CompoundID == "sub pri met2" ~ "primary metabolite 2", 
                                       CompoundID == "inhib" ~ "inhibitor 1"),
                Tissue = gsub("\\(|\\)", "", 
                              str_extract(tolower(ID), 
                                          "\\((liver|kidney|renal)\\)")), 
                Enzyme = str_extract(ID,
                                     "(CYP|UGT)[1-9][ABCDEFJ][1-9]{1,2}"), 
                Trial = str_trim(str_extract(tolower(ID),
                                             "(geometric)? mean| 5(th)? percentile| 95(th)? percentile|median")), 
                Trial = case_when(Trial == "5th percentile" ~ "per5", 
                                  Trial == "95th percentile" ~ "per95", 
                                  Trial == "geometric mean" ~ "geomean",
                                  TRUE ~ Trial),
                PerpPresent = str_detect(ID, "with inh"), 
                Parameter = str_trim(str_extract(ID, " fe | fm ")))
      
      sim_data_mean <- sim_data_xl[c(TimeRow, RowsToUse + TimeRow - 1), ] %>% 
         t() %>%
         as.data.frame() %>% slice(-(1:3)) %>%
         mutate_all(as.numeric) %>% 
         rename(Time = V1) %>% 
         pivot_longer(names_to = "ColOrig", values_to = "Fraction", 
                      cols = -Time) %>%
         left_join(IDs_agg, by = "ColOrig")
   }
   
   
   # Extracting individual data --------------------------------------------
   if(any(c("individual", "both") %in% returnAggregateOrIndiv)){
      
      StartRow_indiv <- which(sim_data_xl$...1 == "Individual Statistics")
      TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
      TimeRow <- TimeRow[TimeRow > StartRow_indiv][1]
      
      # Figuring out which rows contain which data
      FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                              which(1:nrow(sim_data_xl) > TimeRow))[1]
      FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl) + 1, FirstBlank)
      RowsToUse <- which(str_detect(tolower(sim_data_xl$...1), 
                                    "\\((liver|kidney|renal)\\)"))
      RowsToUse <- RowsToUse[RowsToUse > StartRow_indiv & RowsToUse < FirstBlank]
      
      IDs_indiv <- data.frame(ColOrig = paste0("V", 1:(length(RowsToUse) + 1)), 
                              ID = c("Time", t(sim_data_xl[RowsToUse, 1])), 
                              Individual = sim_data_xl[c(TimeRow, RowsToUse), 2], 
                              Trial = sim_data_xl[c(TimeRow, RowsToUse), 3]) %>% 
         rename(Individual = ...2, 
                Trial = ...3) %>% 
         # NOTE: Just guessing for now what any inhibitors might be named b/c
         # I don't have an example file for that atm.
         mutate(CompoundID = str_extract(tolower(ID), "sub( met)?|sub( pri met[12])?|inhib"), 
                CompoundID = case_when(CompoundID == "sub" ~ "substrate", 
                                       CompoundID == "sub met" ~ "primary metabolite 1", 
                                       CompoundID == "sub pri met1" ~ "primary metabolite 1", 
                                       CompoundID == "sub pri met2" ~ "primary metabolite 2", 
                                       CompoundID == "inhib" ~ "inhibitor 1"),
                Tissue = gsub("\\(|\\)", "", 
                              str_extract(tolower(ID), 
                                          "\\((liver|kidney|renal)\\)")), 
                Enzyme = str_extract(ID,
                                     "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}"), 
                PerpPresent = str_detect(ID, "with inh"), 
                Parameter = str_trim(str_extract(ID, " fe | fm ")))
      
      sim_data_indiv <- sim_data_xl[c(TimeRow, RowsToUse), ] %>% 
         t() %>%
         as.data.frame() %>% slice(-(1:3)) %>%
         mutate_all(as.numeric) %>% 
         rename(Time = V1) %>% 
         pivot_longer(names_to = "ColOrig", values_to = "Fraction", 
                      cols = -Time) %>%
         left_join(IDs_indiv, by = "ColOrig")
      
   }
   
   # Putting everything together ------------------------------------------
   
   TimeUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "^Time"))][1]
   TimeUnits <- ifelse(TimeUnits == "Time (h)", "hours", "minutes")
   
   Data <- list()
   
   if(any(c("aggregate", "both") %in% returnAggregateOrIndiv)){
      Data[["agg"]] <- sim_data_mean %>%
         arrange(Trial, Time)
   }
   
   if(any(c("individual", "both") %in% returnAggregateOrIndiv)){
      Data[["indiv"]] <- sim_data_indiv %>%
         mutate(Individual = as.character(Individual),
                Trial = as.character(Trial)) %>%
         arrange(Individual, Time)
   }
   
   Data <- bind_rows(Data)
   
   if("individual" %in% returnAggregateOrIndiv){
      Data <- Data %>%
         mutate(Individual = ifelse(is.na(Individual), Trial, Individual))
   }
   
   # Noting which compound names belong to which compound IDs
   AllCompounds <-  c("substrate" = Deets$Substrate,
                      "primary metabolite 1" = Deets$PrimaryMetabolite1,
                      "primary metabolite 2" = Deets$PrimaryMetabolite2,
                      "secondary metabolite" = Deets$SecondaryMetabolite,
                      "inhibitor 1" = Deets$Inhibitor1,
                      "inhibitor 1 metabolite" = Deets$Inhibitor1Metabolite,
                      "inhibitor 2" = Deets$Inhibitor2)
   
   # Adding DoseNumber so that we can skip extractExpDetails in ct_plot when
   # the user requests a specific dose.
   MyIntervals <- 
      c("substrate" = Deets$DoseInt_sub,
        "primary metabolite 1" = Deets$DoseInt_sub,
        "primary metabolite 2" = Deets$DoseInt_sub,
        "secondary metabolite" = Deets$DoseInt_sub,
        "inhibitor 1" = ifelse(is.null(Deets$DoseInt_inhib),
                               NA, Deets$DoseInt_inhib),
        "inhibitor 1 metabolite" = ifelse(is.null(Deets$DoseInt_inhib),
                                          NA, Deets$DoseInt_inhib),
        "inhibitor 2" = ifelse(is.null(Deets$DoseInt_inhib2),
                               NA, Deets$DoseInt_inhib2))
   
   MyStartTimes <- 
      c("substrate" = Deets$StartHr_sub,
        "primary metabolite 1" = Deets$StartHr_sub,
        "primarymetabolite 2" = Deets$StartHr_sub,
        "secondary metabolite" = Deets$StartHr_sub,
        "inhibitor 1" = ifelse(is.null(Deets$StartHr_inhib), NA,
                               Deets$StartHr_inhib),
        "inhibitor 2" = ifelse(is.null(Deets$StartHr_inhib2), NA,
                               Deets$StartHr_inhib2),
        "inhibitor 1 metabolite" = ifelse(is.null(Deets$StartHr_inhib), NA,
                                          Deets$StartHr_inhib))
   
   MyMaxDoseNum <- 
      c("substrate" = ifelse(Deets$Regimen_sub == "Single Dose", 
                             1, Deets$NumDoses_sub),
        "primary metabolite 1" = ifelse(Deets$Regimen_sub == "Single Dose", 
                                        1, Deets$NumDoses_sub),
        "primarymetabolite 2" = ifelse(Deets$Regimen_sub == "Single Dose", 
                                       1, Deets$NumDoses_sub),
        "secondary metabolite" = ifelse(Deets$Regimen_sub == "Single Dose", 
                                        1, Deets$NumDoses_sub),
        "inhibitor 1" = ifelse(is.null(Deets$NumDoses_inhib), NA,
                               ifelse(Deets$Regimen_inhib == "Single Dose", 
                                      1, Deets$NumDoses_inhib)),
        "inhibitor 2" = ifelse(is.null(Deets$NumDoses_inhib2), NA,
                               ifelse(Deets$Regimen_inhib2 == "Single Dose", 
                                      1, Deets$NumDoses_inhib2)),
        "inhibitor 1 metabolite" = ifelse(is.null(Deets$NumDoses_inhib), NA,
                                          ifelse(Deets$Regimen_inhib == "Single Dose", 
                                                 1, Deets$NumDoses_inhib)))
   
   # Converting data to numeric while also retaining names
   suppressWarnings(
      MyIntervals <- sapply(MyIntervals, FUN = as.numeric))
   suppressWarnings(
      MyStartTimes <- sapply(MyStartTimes, FUN = as.numeric))
   suppressWarnings(
      MyMaxDoseNum <- sapply(MyMaxDoseNum, FUN = as.numeric))
   
   Data <- Data %>%
      mutate(StartHr_sub = MyStartTimes["substrate"],
             TimeSinceDose1_sub = Time - StartHr_sub,
             DoseInt_sub = MyIntervals["substrate"],
             MaxDoseNum_sub = MyMaxDoseNum["substrate"],
             DoseNum_sub = Time %/% DoseInt_sub + 1,
             # Taking care of possible artifacts
             DoseNum_sub = ifelse(DoseNum_sub < 0, 0, DoseNum_sub),
             DoseNum_sub = ifelse(DoseNum_sub > MaxDoseNum_sub, 
                                  MaxDoseNum_sub, DoseNum_sub),
             # If it was a single dose, make everything after StartHr dose
             # 1 and everything before StartHr dose 0. if it was a single
             # dose, then DoseInt is NA.
             DoseNum_sub = ifelse(is.na(DoseInt_sub),
                                  ifelse(TimeSinceDose1_sub < 0, 0, 1), DoseNum_sub),
             StartHr_inhib1 = MyStartTimes["inhibitor 1"],
             TimeSinceDose1_inhib1 = Time - StartHr_inhib1,
             DoseInt_inhib1 = MyIntervals["inhibitor 1"],
             MaxDoseNum_inhib1 = MyMaxDoseNum["inhibitor 1"],
             DoseNum_inhib1 = Time %/% DoseInt_inhib1 + 1,
             # Taking care of possible artifacts
             DoseNum_inhib1 = ifelse(DoseNum_inhib1 < 0, 0, DoseNum_inhib1),
             DoseNum_inhib1 = ifelse(DoseNum_inhib1 > MaxDoseNum_inhib1, 
                                     MaxDoseNum_inhib1, DoseNum_inhib1),
             # If it was a single dose, make everything after StartHr dose
             # 1 and everything before StartHr dose 0. if it was a single
             # dose, then DoseInt is NA.
             DoseNum_inhib1 = ifelse(is.na(DoseInt_inhib1),
                                     ifelse(TimeSinceDose1_inhib1 < 0, 0, 1), DoseNum_inhib1),
             StartHr_inhib2 = MyStartTimes["inhibitor 2"],
             TimeSinceDose1_inhib2 = Time - StartHr_inhib2,
             DoseInt_inhib2 = MyIntervals["inhibitor 2"],
             MaxDoseNum_inhib2 = MyMaxDoseNum["inhibitor 2"],
             DoseNum_inhib2 = Time %/% DoseInt_inhib2 + 1,
             # Taking care of possible artifacts
             DoseNum_inhib2 = ifelse(DoseNum_inhib2 < 0, 0, DoseNum_inhib2),
             DoseNum_inhib2 = ifelse(DoseNum_inhib2 > MaxDoseNum_inhib2, 
                                     MaxDoseNum_inhib2, DoseNum_inhib2),
             # If it was a single dose, make everything after StartHr dose
             # 1 and everything before StartHr dose 0. if it was a single
             # dose, then DoseInt is NA.
             DoseNum_inhib2 = ifelse(is.na(DoseInt_inhib2),
                                     ifelse(TimeSinceDose1_inhib2 < 0, 0, 1), DoseNum_inhib2))
   
   # Checking for when the simulation ends right at the last dose b/c
   # then, setting that number to 1 dose lower
   if(length(Data %>% filter(DoseNum_sub == max(Data$DoseNum_sub)) %>%
             pull(Time) %>% unique()) == 1){
      MyMaxDoseNum_sub <- max(Data$DoseNum_sub)
      Data <- Data %>%
         mutate(DoseNum_sub = ifelse(DoseNum_sub == MyMaxDoseNum_sub,
                                     MyMaxDoseNum_sub - 1, DoseNum_sub))
   }
   
   if(length(Data %>% filter(DoseNum_inhib1 == max(Data$DoseNum_inhib1)) %>%
             pull(Time) %>% unique()) == 1){
      MyMaxDoseNum_inhib1 <- max(Data$DoseNum_inhib1)
      Data <- Data %>%
         mutate(DoseNum_inhib1 = ifelse(DoseNum_inhib1 == MyMaxDoseNum_inhib1,
                                        MyMaxDoseNum_inhib1 - 1, DoseNum_inhib1))
   }
   
   if(length(Data %>% filter(DoseNum_inhib2 == max(Data$DoseNum_inhib2)) %>%
             pull(Time) %>% unique()) == 1){
      MyMaxDoseNum_inhib2 <- max(Data$DoseNum_inhib2)
      Data <- Data %>%
         mutate(DoseNum_inhib2 = ifelse(DoseNum_inhib2 == MyMaxDoseNum_inhib2,
                                        MyMaxDoseNum_inhib2 - 1, DoseNum_inhib2))
   }
   
   # Noting exactly what the perpetrators were
   AllPerpetrators <- c(Deets$Inhibitor1, Deets$Inhibitor2,
                     Deets$Inhibitor1Metabolite)
   AllPerpetrators <- AllPerpetrators[complete.cases(AllPerpetrators)]
   
   # Finalizing, tidying, selecting only useful columns
   Data <- Data %>%
      mutate(Time_units = tolower({{TimeUnits}}),
             File = sim_data_file,
             Inhibitor = ifelse(PerpPresent,
                                str_comma(AllPerpetrators), "none"), 
             Substrate = Deets$Substrate, 
             Compound = AllCompounds[CompoundID]) %>%
      arrange(across(any_of(c("Parameter", "Tissue", "Enzyme",
                              "Substrate", "Inhibitor",
                              "Individual", "Trial", "Time")))) %>%
      select(any_of(c("Compound", "CompoundID", "Inhibitor", 
                      "Parameter", "Enzyme", "Tissue", 
                      "Individual", "Trial", "Time", "Fraction",
                      "Time_units", 
                      "DoseNum_sub", "DoseNum_inhib1", "DoseNum_inhib2", 
                      "File"))) %>% 
      purrr::discard(~all(is.na(.)))
   
   return(Data)
   
}


