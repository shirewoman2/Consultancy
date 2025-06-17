#' INTERNAL. Extract fm and fe values that change with time from a Simulator output Excel
#' file
#'
#' \code{extractFmFe} extracts fm and fe data from a simulator output Excel
#' file. A tab named "Time variant \%fm and fe" must be present to get dynamic
#' fm and fe values, and a tab named "\% fm and fe SS" must be present to get
#' static values. Dynamic fm and fe data are required if you want to use these
#' data with the function \code{\link{fm_plot}} to make a graph of how the fm
#' values change over time.
#'
#' @param sim_data_file name of the Excel file containing the simulated dynamic
#'   fm and fe data, in quotes
#' @param returnOnlyMaxMin TRUE (default) or FALSE for whether to return only
#'   maximum and minimum fm values for dynamic fm values -- basically, return
#'   the table in the upper left corner of the "Time variant \%fm and fe" tab.
#' @param static_or_dynamic get "static" or "dynamic" (default) fm values.
#'   "static" will retrieve information from the "\% fm and fe SS" tab, if
#'   present, and "dynamic" will retrieve it from the "Time variant \%fm and fe"
#'   tab.
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated fm
#'   and fe data? This currently only applies to the dynamic fms and has not yet
#'   been developed for the static fms, which will only return aggregate data at
#'   present. Options are "individual", "aggregate", or "both" (default).
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
extractFmFe_subfun <- function(sim_data_file,
                               static_or_dynamic = "dynamic", 
                               returnOnlyMaxMin = TRUE, 
                               returnAggregateOrIndiv = "both", 
                               existing_exp_details = NA){
   
   # NB: This is ONLY called internally from extractFmFe function. For that
   # reason, limited error catching here.
   
   # Error catching --------------------------------------------------------------------
   
   Deets <- existing_exp_details$MainDetails %>% 
      filter(File == sim_data_file)
   
   if(is.null(Deets)){
      # Skipping the warning b/c they will have already gotten it from
      # extractExpDetails.
      return(data.frame())
   }
   
   if(Deets$PopRepSim == "Yes"){
      warning(wrapn(paste0("The simulator file supplied, `", 
                           sim_data_file, 
                           "`, is for a population-representative simulation and thus doesn't have any aggregate data. Please be warned that some plotting functions will not work well without aggregate data.")),
              call. = FALSE)
   }
   
   # Figuring out which sheet to extract and dealing with case since that
   # apparently changes between Simulator versions.
   AllSheets <- str_split(Deets$SheetNames, pattern = "` `")[[1]]
   AllSheets <- gsub("`", "", AllSheets)
   SheetNames_dynamic <- AllSheets[str_detect(tolower(AllSheets), "time variant .fm and fe")]
   SheetNames_static <- AllSheets[which(AllSheets == "% fm and fe SS")]
   
   if(length(SheetNames_dynamic) == 0 & 
      length(SheetNames_static) == 0){
      warning(wrapn(paste0("The simulator output file provided, '", 
                           sim_data_file, 
                           "', does not appear to have a sheet titled 'Time variant %fm and fe', which is what we need for extracting dynamic fm and fe values, nor a sheet titled '% fm and fe SS', which is what we need for static fm and fe values. We cannot return any fm data.")),
              call. = FALSE)
      return(data.frame())
   }
   
   if(length(SheetNames_static) == 0 & 
      static_or_dynamic == "static"){
      warning(wrapn(paste0("The simulator output file provided, '", 
                           sim_data_file, 
                           "', does not appear to have a sheet titled '% fm and fe SS', which is what we need for extracting static fm and fe values. We cannot return any dynamic fm data.")),
              call. = FALSE)
      return(data.frame())
   }
   
   if(length(SheetNames_dynamic) == 0 & 
      static_or_dynamic == "dynamic"){
      warning(wrapn(paste0("The simulator output file provided, '", 
                           sim_data_file, 
                           "', does not appear to have a sheet titled 'Time variant %fm and fe', which is what we need for extracting dynamic fm and fe values. We cannot return any fm data.")),
              call. = FALSE)
      return(data.frame())
   }
   
   
   # Main body of function ----------------------------------------------------------------
   
   if(static_or_dynamic == "dynamic"){
      
      ## Dynamic fm data extraction -----------------------------------------------
      
      # Reading in simulated abundance-time profile data
      sim_data_xl <- suppressMessages(
         readxl::read_excel(path = sim_data_file,
                            sheet = SheetNames_dynamic,
                            range = switch(as.character(returnOnlyMaxMin), 
                                           "TRUE" = "A1:E50", # Guessing that there wouldn't be more than 50 lines here... Reasonable? 
                                           "FALSE" = NULL),
                            col_names = FALSE))
      
      ### Extracting only max and min --------------------------------------------
      
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
                      File = sim_data_file, 
                      Pathway = case_when(
                         is.na(Enzyme) ~ Tissue,
                         .default = paste(Tissue, Enzyme))) %>% 
               select(Parameter, Tissue, Enzyme, Pathway, PerpPresent, 
                      Max, tmax, Min, tmin, File)
         )
         
         return(Out)
      }
      
      ### Extracting aggregate data ---------------------------------------------
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
      
      
      ### Extracting individual data --------------------------------------------
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
      
      ### Putting everything together ------------------------------------------
      
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
      AllRegCompounds <-  c("substrate" = Deets$Substrate,
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
                Compound = AllRegCompounds[CompoundID], 
                Pathway = case_when(
                   is.na(Enzyme) ~ Tissue,
                   .default = paste(Tissue, Enzyme))) %>%
         arrange(across(any_of(c("Parameter", "Tissue", "Enzyme",
                                 "Substrate", "Inhibitor",
                                 "Individual", "Trial", "Time")))) %>%
         select(any_of(c("File", "Compound", "CompoundID", "Inhibitor", 
                         "Enzyme", "Tissue", "Pathway", "Individual", "Trial", 
                         "Parameter", "Time", "Fraction", "Time_units", 
                         "DoseNum_sub", "DoseNum_inhib1", "DoseNum_inhib2"))) %>% 
         purrr::discard(~all(is.na(.)))
      
      return(Data)
      
   } else {
      
      ## Static fm data extraction -----------------------------------------------
      
      # Reading in simulated abundance-time profile data
      sim_data_xl <- suppressMessages(
         readxl::read_excel(path = sim_data_file,
                            sheet = SheetNames_static,
                            col_names = FALSE))
      
      # Only doing aggregate data for now. Determining column positions.
      Row3 <- as.character(t(sim_data_xl[3, ]))
      StartCols_main <- which(str_detect(Row3, "Statistics"))
      
      StartCol_BL <- StartCols_main[1]
      EndCol_BL <- as.character(t(sim_data_xl[4, (StartCol_BL + 1):ncol(sim_data_xl)]))
      EndCol_BL <- which(is.na(EndCol_BL))[1] - 1 + StartCol_BL
      EndCol_BL <- ifelse(is.na(EndCol_BL), ncol(sim_data_xl), EndCol_BL)
      
      # These do not seem to change
      StartRow <- 5
      EndRow <- 18
      
      # BL fms
      Out <- sim_data_xl[StartRow:EndRow, StartCol_BL:EndCol_BL]
      names(Out) <- c("Statistic", 
                      t(as.character(sim_data_xl[StartRow - 1, (StartCol_BL+1):EndCol_BL])))
      
      suppressWarnings(
         Out <- Out %>% 
            mutate(across(.cols = -Statistic, 
                          .fns = as.numeric)) %>% 
            mutate(Statistic = renameStats(Statistic), 
                   DDI = FALSE, 
                   File = sim_data_file)
      )
      
      if(length(StartCols_main) == 2){
         
         StartCol_DDI <- StartCols_main[2]
         EndCol_DDI <- as.character(t(sim_data_xl[4, (StartCol_DDI + 1):ncol(sim_data_xl)]))
         EndCol_DDI <- which(is.na(EndCol_DDI))[1] + StartCol_DDI - 1
         EndCol_DDI <- ifelse(is.na(EndCol_DDI), ncol(sim_data_xl), EndCol_DDI)
         
         # DDI fms
         DDI <- sim_data_xl[StartRow:EndRow, StartCol_DDI:EndCol_DDI]
         names(DDI) <- c("Statistic", 
                         t(as.character(sim_data_xl[StartRow - 1, (StartCol_DDI+1):EndCol_DDI])))
         
         suppressWarnings(
            DDI <- DDI %>% 
               mutate(across(.cols = -Statistic, 
                             .fns = as.numeric)) %>% 
               mutate(Statistic = renameStats(Statistic), 
                      DDI = TRUE, 
                      File = sim_data_file)
         )
         
         Out <- bind_rows(Out, DDI) %>% 
            select(File, Statistic, everything())
         
      }
      
      # Formatting to make output better match dynamic output and get better
      # column names
      Out <- Out %>% 
         pivot_longer(cols = -c(Statistic, DDI, File), 
                      names_to = "Parameter", 
                      values_to = "Fraction") %>% 
         mutate(Compound = Deets$Substrate, 
                CompoundID = "substrate", 
                Inhibitor = case_when(DDI == TRUE ~ Deets$Inhibitor1, 
                                      DDI == FALSE ~ "none")) %>% 
         separate_wider_delim(cols = Parameter, delim = " ", 
                              names = c("Enzyme1", "Tissue1"), 
                              too_many = "merge", 
                              too_few = "align_end") %>% 
         mutate(
            Enzyme = case_when(
               Enzyme1 == "Additional" & Tissue1 == "HLM" ~ "additional HLM", 
               Enzyme1 == "Additional" & Tissue1 == "Systemic Clearance" ~ "additional systemic clearance", 
               Tissue1 == "Renal" & is.na(Enzyme1) ~ "renal clearance", 
               .default = Enzyme1), 
            Tissue = case_when(
               Enzyme1 == "Additional" & Tissue1 == "HLM" ~ "liver", 
               Enzyme1 == "Additional" & Tissue1 == "systemic clearance" ~ "systemic", 
               Tissue1 == "Renal" ~ "kidney", 
               .default = Tissue1), 
            Tissue = tolower(Tissue), 
            Parameter = case_when(Tissue == "kidney" ~ "fe", 
                                  .default = "fm"), 
            # Fraction must be divided by 100 to match the dynamic output
            # format
            Fraction = Fraction / 100,
            Pathway = case_when(
               is.na(Enzyme) ~ Tissue,
               Enzyme %in% c("additional HLM", 
                             "additional systemic clearance", 
                             "renal clearance") ~ Enzyme, 
               .default = paste(Tissue, Enzyme))) %>% 
         select(File, Compound, CompoundID, Inhibitor, 
                Enzyme, Tissue, Pathway, Statistic, Parameter, Fraction)
      
      return(Out)
      
   }
   
}