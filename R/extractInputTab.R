#' INTERNAL: Extract info from a tab formatted like the main "Input Sheet" tab
#' in Simulator output Excel files
#'
#' @param deets details to extract (the coded name)
#' @param sim_data_file sim_data_file
#' @param sheet what sheet to read 
#'
#' @returns list of details from a tab formatted like the main "Input Sheet" tab
#'   in Simulator output Excel files
extractInputTab <- function(deets = "all", 
                            sim_data_file, 
                            sheet, 
                            VBE = FALSE, 
                            CustomDosing = FALSE){
   
   if(deets[1] == "all"){
      MyInputDeets <- AllExpDetails$Detail[
         str_detect(AllExpDetails$DataSource, "Input Sheet")]
   } else {
      MyInputDeets <- deets
   }
   
   InputDeets_DF <- AllExpDetails %>% filter(DataSource == "Input Sheet")
   Out <- list()
   
   InputTab <- suppressMessages(tryCatch(
      readxl::read_excel(path = sim_data_file, sheet = sheet,
                         col_names = FALSE),
      error = openxlsx::read.xlsx(sim_data_file, sheet = sheet,
                                  colNames = FALSE)))
   # If openxlsx read the file, the names are different. Fixing.
   if(names(InputTab)[1] == "X1"){
      names(InputTab) <- paste0("...", 1:ncol(InputTab))
   }
   
   Out[["ExcelResultsTimeStamp"]] <- 
      timeConv(as.numeric(InputTab[2, 1]), dataSource = "Excel")
   
   # When Inhibitor 1 is not present, don't look for those values.
   if(any(str_detect(t(InputTab[5, ]), "Inhibitor 1"), na.rm = T) == FALSE){
      MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_inhib$|Inhibitor1")]
   }
   
   # When Inhibitor 2 is not present, don't look for those values.
   if(any(str_detect(t(InputTab[5, ]), "Inhibitor 2"), na.rm = T) == FALSE){
      MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_inhib2$|Inhibitor2")]
   }
   
   # When primary metabolite 1 is not present, don't look for those values.
   if(any(str_detect(t(InputTab[5, ]), "Sub Pri Metabolite1"), na.rm = T) == FALSE){
      MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_met1|PrimaryMetabolite1")]
   }
   
   # When primary metabolite 2 is not present, don't look for those values.
   if(any(str_detect(t(InputTab[5, ]), "Sub Pri Metabolite2"), na.rm = T) == FALSE){
      MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_met2|PrimaryMetabolite2")]
   }
   
   # When secondary metabolite is not present, don't look for those values.
   if(any(str_detect(t(InputTab[5, ]), "Sub Sec Metabolite"), na.rm = T) == FALSE){
      MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_secmet|SecondaryMetabolite")]
   }
   
   # When Inhibitor 1 metabolite is not present, don't look for those values.
   if(any(str_detect(t(InputTab[5, ]), "Inh 1 Metabolite"), na.rm = T) == FALSE){
      MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_inhib1met|Inhibitor1Metabolite")]
   }
   
   # We'll select details based on whether this was a Discovery simulation. 
   Out[["SimulatorUsed"]] <- ifelse(str_detect(InputTab[1, 1], "Discovery"), 
                                    "Simcyp Discovery", "Simcyp Simulator")
   
   DiscoveryCol <- switch(Out[["SimulatorUsed"]], 
                          "Simcyp Discovery" = c("Simulator and Discovery", 
                                                 "Discovery only"), 
                          "Simcyp Simulator" = c("Simulator only", 
                                                 "Simulator and Discovery"))
   
   # Need to filter to keep only details that we can possibly find based on
   # what type of simulator was used
   MyInputDeets <- intersect(MyInputDeets, 
                             AllExpDetails$Detail[
                                AllExpDetails$SimulatorAvailability %in% DiscoveryCol])
   InputDeets_DF <- InputDeets_DF %>% 
      filter(SimulatorAvailability %in% DiscoveryCol)
   
   # Looking for locations of columns.
   InputDeets_DF <- InputDeets_DF %>% 
      mutate(CompoundID = case_when(is.na(CompoundID) ~ "Trial Design",
                                    .default = CompoundID))
   
   if(VBE){
      # I don't think there would be more than one compound in a VBE sim, so
      # setting the compound ID to "substrate".
      InputDeets_DF <- InputDeets_DF %>% 
         filter(CompoundID %in% c("substrate", "Trial Design"))
   } 
   
   ColLocations <- c("substrate" = 1,
                     "Trial Design" = which(t(InputTab[5, ]) == "Trial Design"),
                     "inhibitor 1" = which(t(InputTab[5, ]) == "Inhibitor 1"),
                     "inhibitor 2" = which(t(InputTab[5, ]) == "Inhibitor 2"),
                     "primary metabolite 1" = which(t(InputTab[5, ]) == "Sub Pri Metabolite1"),
                     "primary metabolite 2" = which(t(InputTab[5, ]) == "Sub Pri Metabolite2"),
                     "secondary metabolite" = which(t(InputTab[5, ]) == "Sub Sec Metabolite"),
                     "inhibitor 1 metabolite" = which(t(InputTab[5, ]) == "Inh 1 Metabolite"))
   
   InputDeets_DF <- InputDeets_DF %>% 
      mutate(NameCol = ColLocations[CompoundID], 
             ValueCol = NameCol + 1) %>% 
      # If the NameCol is empty, it's b/c that compound ID or else a column with
      # "Trial Design" wasn't found on that tab, so remove any empty NameCol
      # rows.
      filter(complete.cases(NameCol))
   
   ## Main set of parameters -----------------------------------------
   
   # Dealing w/potential replicate values. CompoundType and pKa may be
   # replicated but will have the same value, so when we take the unique
   # value, that will drop away.
   
   # Checking for any ADAMI parameters. (May need to adapt this later for
   # other variations on ADAM models or anything else where there will be
   # multiple cells with identical labels.)
   ADAMIrow <- which(InputTab$...1 == "ADAMI Parameters")
   
   if(length(ADAMIrow) == 0){
      InputDeets_DF <- InputDeets_DF %>% 
         filter(!str_detect(Detail, "ADAMI"))
      ADAMIreps <- NA
      NonADAMIreps <- NA
      MyInputDeets <- intersect(MyInputDeets, InputDeets_DF$Detail)
   } else {
      ADAMIreps <- InputDeets_DF %>% 
         filter(str_detect(Detail, "ADAMI")) %>% 
         pull(Detail)
      NonADAMIreps <- sub("_ADAMI", "", ADAMIreps)
   }
   
   # sub function for finding correct cell
   pullValue <- function(deet){
      
      # Setting up regex to search
      ToDetect <- InputDeets_DF %>% 
         filter(Detail == deet) %>% pull(Regex_row)
      NameCol <- InputDeets_DF$NameCol[which(InputDeets_DF$Detail == deet)]
      Row <- which(str_detect(InputTab[, NameCol] %>% pull(), ToDetect)) +
         (InputDeets_DF %>% filter(Detail == deet) %>% pull(OffsetRows))
      
      if(length(Row) == 0){
         Val <- NA
      } else {
         if(deet %in% ADAMIreps){Row <- Row[Row > ADAMIrow]}
         if(deet %in% NonADAMIreps){Row <- Row[Row < ADAMIrow]}
         
         Val <- InputTab[Row,
                         InputDeets_DF$ValueCol[
                            which(InputDeets_DF$Detail == deet)]] %>% pull()
         
         # If it's a kp scalar value other than the main one, then we need to
         # 1st check whether the value listed is "User" and then get the value
         # in the cell right below that if it is.
         kpcheck <- str_detect(deet, "kp_scalar_") & 
            deet %in% paste0("kp_scalar", AllCompounds$Suffix) == FALSE
         if(kpcheck){
            if(complete.cases(Val) && Val == "Predicted"){
               Val <- NA
            } else {
               NameColBelow <- InputTab[Row + 1,
                                        InputDeets_DF$NameCol[
                                           which(InputDeets_DF$Detail == deet)]] %>% pull()
               if(str_detect(tolower(NameColBelow),
                             tolower(gsub(paste0("kp_scalar_|",
                                                 str_c(AllCompounds$Suffix, collapse = "|")),
                                          "", 
                                          sub("additional_organ", "Additional Organ", deet))))){
                  Val <- InputTab[Row + 1, InputDeets_DF$ValueCol[
                     which(InputDeets_DF$Detail == deet)]] %>% pull()
                  
               } else {
                  Val <- NA
               }
            }
         }
      }
      
      # If SimStartDayTime is not found, which will happen with animal
      # sims, it may be possible to piece together from other data. 
      if(length(Val) == 0 & deet == "SimStartDayTime"){
         StartDay <- as.character(InputTab[which(InputTab$...1 == "Start Day"), 2])
         StartTime <- as.character(InputTab[which(InputTab$...1 == "Start Time"), 2])
         StartTime <- str_split(sub("m", "", StartTime), "h")[[1]]
         Val <- 
            paste0("Day ", StartDay, ", ",
                   formatC(as.numeric(StartTime[1]), width = 2, flag = "0"), ":",
                   formatC(as.numeric(StartTime[2]), width = 2, flag = "0"))
         rm(StartDay, StartTime)
      }
      
      # Ontogeny profile along w/CompoundType and pKa are often listed twice
      # in output for some reason. Only keeping the unique set of values for
      # all deets. This will still throw a warning if there is more than one
      # value, but we'd want to know that anyway, so that's why I'm not just
      # keeping the 1st value listed.
      Val <- sort(unique(Val))
      
      # Accounting for when fu,p is scripted
      if(length(Val) > 0 && 
         (any(complete.cases(Val)) && 
          str_detect(deet, "^fu_") & any(str_detect(Val, "script")))){
         InputDeets_DF$Class[InputDeets_DF$Detail == deet] <- "character"
         assign("InputDeets_DF", InputDeets_DF, envir = parent.frame())
      }
      
      # MoleculeType may be NA b/c "Molecule Type" is not found. I think that
      # will only happen for small molecules that are not the substrate.
      if(str_detect(deet, "MoleculeType") & 
         length(Val) == 0 & !str_detect(deet, "_sub$")){
         Val <- "Small Molecule"
      }
      
      suppressWarnings(
         Val <- switch(InputDeets_DF$Class[InputDeets_DF$Detail == deet], 
                       "character" = as.character(Val),
                       "numeric" = as.numeric(Val))
      )
      
      if(length(Val) > 1){
         Val <- str_comma(Val)
      }
      
      # Tidying up some specific idiosyncracies of simulator output
      Val <- ifelse(length(Val) == 0 || 
                       (complete.cases(Val) & Val == "n/a"), NA, Val)
      Val <- ifelse(str_detect(deet, "^Unit"),
                    str_trim(gsub("\\(unbound\\)|\\(blood\\)|\\(unbound blood\\)|Dose \\(|\\)|CMax \\(|TMax \\(|AUC \\(|CL \\(Dose/AUC\\)\\(|\\(blood\\)",
                                  "", Val)), Val)
      
      return(Val)
   }
   
   # pullValue doesn't work for CL, so those are separate. Also need
   # to do StartDayTime_x and SimulatorVersion separately.
   MyInputDeets1 <-
      MyInputDeets[!str_detect(MyInputDeets, 
                               "CLint_|Interaction_|^StartDayTime|Transport_|SimulatorVersion|OrganTissue")]
   
   if(length(MyInputDeets1) > 0){
      for(i in MyInputDeets1){
         # print(i)
         Out[[i]] <- pullValue(i)
      }
   }
   
   # Adjusting a few parameters after pulling b/c correct value depends on a
   # different parameter value
   for(suffix in AllRegCompounds$Suffix){
      if(all(paste0(
         c("DisintegrationProfile_alpha", 
           "DisintegrationProfile_beta", 
           "DisintegrationProfile_Input"), suffix) %in% names(Out)) &&
         complete.cases(Out[[paste0("DisintegrationProfile_Input", suffix)]]) &&
         Out[[paste0("DisintegrationProfile_Input", suffix)]] != "Weibull"){
         
         Out[[paste0("DisintegrationProfile_alpha")]] <- NA
         Out[[paste0("DisintegrationProfile_beta")]] <- NA
      }
      
      if(all(paste0(
         c("DisintegrationProfile_Kd1", 
           "DisintegrationProfile_Input"), suffix) %in% names(Out)) && 
         complete.cases(Out[[paste0("DisintegrationProfile_Input", suffix)]]) &&
         Out[[paste0("DisintegrationProfile_Input", suffix)]] != "First Order"){
         
         Out[[paste0("DisintegrationProfile_Kd1")]] <- NA
      }
   }
   
   
   ## Some overall simulation details -----------------------------------
   
   # Checking simulator version
   Out[["SimulatorVersion"]] <- ifelse(str_detect(InputTab[1, 1], "Discovery"), 
                                       as.character(InputTab[1, 1]), 
                                       str_extract(as.character(InputTab[3, 1]),
                                                   "Version [12][0-9]"))
   
   ### Checking on release profiles -----------------------------------------
   if(Out[["SimulatorUsed"]] != "Simcyp Discovery" &&
      exists("InputTab", inherits = FALSE)){
      
      ReleaseProfs <- list()
      
      for(i in names(ColLocations)[!names(ColLocations) == "Trial Design"]){
         
         Suffix <- AllCompounds$Suffix[AllCompounds$CompoundID == i]
         
         if(any(str_detect(t(InputTab[, as.numeric(ColLocations[i])]), "Release Mean"),
                na.rm = TRUE)){
            
            StartRow <- which(str_detect(t(InputTab[, ColLocations[i]]), "CR/MR Input"))[1] + 1
            EndRow <- which(str_detect(t(InputTab[, ColLocations[i]]), "Release Mean"))
            EndRow <- EndRow[which.max(EndRow)] + 1 # Looking for last "Release Mean" row and then the next row will be the CV for that. 
            
            Release_temp <- InputTab[StartRow:EndRow, ColLocations[i]:(ColLocations[i]+1)]
            names(Release_temp) <- c("NameCol", "ValCol")
            
            # Older versions of simulator do not have CV. Checking. 
            ReleaseCV <- Release_temp$ValCol[which(str_detect(Release_temp$NameCol, "CV"))]
            if(all(is.null(ReleaseCV))){ReleaseCV <- NA}
            
            suppressWarnings(
               ReleaseProfs[[i]] <- data.frame(
                  CR_MR_input = Out[[paste0("CR_MR_Input", Suffix)]], 
                  Time = Release_temp$ValCol[which(str_detect(Release_temp$NameCol, "Time"))], 
                  Release_mean = Release_temp$ValCol[which(str_detect(Release_temp$NameCol, "Release Mean"))], 
                  Release_CV = ReleaseCV) %>% 
                  mutate(across(.cols = everything(), .fns = as.numeric), 
                         Release_CV = Release_CV / 100, # Making this a fraction instead of a number up to 100
                         File = sim_data_file, 
                         CompoundID = i, 
                         Compound = as.character(Out[AllCompounds$DetailNames[
                            AllCompounds$CompoundID == i]])) %>% 
                  select(File, CompoundID, Compound, Time, Release_mean, Release_CV)
            )
            
            rm(StartRow, EndRow, Release_temp)
            
         } else if(
            paste0("CR_MR_Input", 
                   AllCompounds$Suffix[AllCompounds$CompoundID == i]) %in% 
            names(Out) == TRUE &&
            complete.cases(Out[[paste0("CR_MR_Input", 
                                       AllCompounds$Suffix[AllCompounds$CompoundID == i])]]) &&
            Out[[paste0("CR_MR_Input", 
                        AllCompounds$Suffix[AllCompounds$CompoundID == i])]] == 
            "Weibull"){
            
            Suffix <- AllCompounds$Suffix[AllCompounds$CompoundID == i]
            
            ReleaseProfs <- data.frame(
               CR_MR_input = Out[[paste0("CR_MR_Input", Suffix)]], 
               Parameter = c("Fmax", "alpha", "beta", "lag"), 
               Value = c(Out[[paste0("ReleaseProfile_Fmax", Suffix)]], 
                         Out[[paste0("ReleaseProfile_alpha", Suffix)]], 
                         Out[[paste0("ReleaseProfile_beta", Suffix)]], 
                         Out[[paste0("ReleaseProfile_lag", Suffix)]]), 
               CV = c(Out[[paste0("ReleaseProfile_Fmax_CV", Suffix)]], 
                      Out[[paste0("ReleaseProfile_alpha_CV", Suffix)]], 
                      Out[[paste0("ReleaseProfile_beta_CV", Suffix)]], 
                      Out[[paste0("ReleaseProfile_lag_CV", Suffix)]])) %>% 
               mutate(CV = CV / 100, # Making this a fraction instead of a number up to 100
                      File = sim_data_file, 
                      CompoundID = i, 
                      Compound = as.character(Out[AllCompounds$DetailNames[
                         AllCompounds$CompoundID == i]])) %>% 
               select(File, CompoundID, Compound, CR_MR_input, Parameter, Value, CV)
            
         } else {
            ReleaseProfs <- NULL
         }
      }
      
      ReleaseProfs <- bind_rows(ReleaseProfs)
      
      if(nrow(ReleaseProfs) == 0){ReleaseProfs <- NULL}
      
   } else {
      ReleaseProfs <- NULL
   }
   
   ### Checking on dissolution profiles ------------------------------------
   if(Out[["SimulatorUsed"]] != "Simcyp Discovery" &&
      exists("InputTab", inherits = FALSE) &&
      any(str_detect(unlist(c(InputTab[, ColLocations])), "Dissolution( Mean)? \\(\\%"),
          na.rm = TRUE)){
      
      DissoProfs <- list()
      
      for(i in names(ColLocations)[!names(ColLocations) == "Trial Design"]){
         # There may be more than one tissue. Checking for this. 
         DissoTissueRows <- which(str_detect(t(InputTab[, ColLocations[i]]),
                                             "^Dissolution Profile"))
         
         # If the results do not specify any tissues, then start rows will be
         # different. Last row will be the same, though.
         LastRow <- which(str_detect(t(InputTab[, ColLocations[i]]), "Dissolution( Mean)? \\(\\%"))
         LastRow <- LastRow[which.max(LastRow)] + 1 # Looking for last "Dissolution (%)" row and then the next row will be the CV for that. 
         
         if(length(DissoTissueRows) > 0){
            
            StartRows <- which(str_detect(t(InputTab[, ColLocations[i]]), "^Dissolution Profile")) + 1
            
            # It could be that one compound has dissolution profiles and another
            # compound does not. Checking that here since I did not check it in
            # the original "if" statement at the top of this section.
            if(all(is.na(StartRows))){
               next
            }
            
            if(length(StartRows) > 1){
               EndRows <- c(StartRows[2:length(StartRows)], NA) - 2
               EndRows[length(StartRows)] <- LastRow
            } else {
               EndRows <- LastRow
            }
            
            DissoTissues <- gsub("\\(|\\)", "", 
                                 str_extract(
                                    t(InputTab[, ColLocations[i]])[DissoTissueRows], 
                                    "\\(.*\\)"))
         } else {
            # If the tissue is not specified, then there will be only 1 set of values. 
            StartRows <- which(str_detect(t(InputTab[, ColLocations[i]]), "Dissolution( Mean)? \\(\\%"))[1] - 1
            DissoTissues <- "not specified"
            EndRows <- LastRow
            
            # It could be that one compound has dissolution profiles and another
            # compound does not. Checking that here since I did not check it in
            # the original "if" statement at the top of this section.
            if(all(is.na(StartRows))){
               next
            }
         }
         
         DissoProfs[[i]] <- list()
         
         for(tiss in 1:length(StartRows)){
            
            Disso_temp <- InputTab[StartRows[tiss]:EndRows[tiss],
                                   ColLocations[i]:(ColLocations[i]+1)]
            names(Disso_temp) <- c("NameCol", "ValCol")
            
            # Older versions of simulator do not have CV. Checking. 
            DissoCV <- Disso_temp$ValCol[which(str_detect(Disso_temp$NameCol, "CV"))]
            if(all(is.null(DissoCV))){DissoCV <- NA}
            
            suppressWarnings(
               DissoProfs[[i]][[tiss]] <- 
                  data.frame(
                     Time = Disso_temp$ValCol[which(str_detect(Disso_temp$NameCol, "Time"))], 
                     Dissolution_mean = Disso_temp$ValCol[which(str_detect(Disso_temp$NameCol, "Dissolution( Mean)? \\(\\%"))], 
                     Dissolution_CV = DissoCV) %>% 
                  mutate(across(.cols = everything(), .fns = as.numeric), 
                         Dissolution_CV = Dissolution_CV / 100, # Making this a fraction instead of a number up to 100
                         File = sim_data_file, 
                         Tissue = DissoTissues[[tiss]],
                         CompoundID = i, 
                         Compound = as.character(Out[AllCompounds$DetailNames[
                            AllCompounds$CompoundID == i]]))
            )
            
            rm(Disso_temp, DissoCV)
            
         }
         
         DissoProfs[[i]] <- bind_rows(DissoProfs[[i]])
         
      }
      
      DissoProfs <- bind_rows(DissoProfs) %>% 
         select(File, Tissue, CompoundID, Compound, Time,
                Dissolution_mean, Dissolution_CV)
      
   } else {
      DissoProfs <- NULL
   }
   
   ### Concentration-dependent fu -----------------------------------------
   if(Out[["SimulatorUsed"]] != "Simcyp Discovery" &&
      exists("InputTab", inherits = FALSE) &&
      any(str_detect(unlist(c(InputTab[, ColLocations + 1])), 
                     "Concentration-dependent fu profile"),
          na.rm = TRUE)){
      
      CDfupProfs <- list()
      
      for(i in names(ColLocations)[!names(ColLocations) == "Trial Design"]){
         StartRow <- which(str_detect(t(InputTab[, ColLocations[i] + 1]), 
                                      "Concentration-dependent fu profile"))[1] + 2
         EndRow <- which(str_detect(t(InputTab[, ColLocations[i]]), 
                                    "fu [0-9]"))
         EndRow <- EndRow[which.max(EndRow)]
         
         # It could be that one compound has conc-dependent fu,p profiles and
         # another compound does not. Checking that here since I did not check it
         # in the original if statement at the top of this section.
         if(is.na(StartRow)){
            next
         }
         
         CDfup_temp <- InputTab[StartRow:EndRow, ColLocations[i]:(ColLocations[i]+1)]
         names(CDfup_temp) <- c("NameCol", "ValCol")
         
         CDfupProfs[[i]] <- data.frame(
            Conc = CDfup_temp$ValCol[which(str_detect(CDfup_temp$NameCol, "Conc"))], 
            fup = CDfup_temp$ValCol[which(str_detect(CDfup_temp$NameCol, "fu [0-9]"))]) %>%  
            mutate(across(.cols = everything(), .fns = as.numeric), 
                   File = sim_data_file, 
                   CompoundID = i, 
                   Compound = as.character(Out[AllCompounds$DetailNames[
                      AllCompounds$CompoundID == i]])) %>% 
            select(File, CompoundID, Compound, Conc, fup)
         
         rm(StartRow, EndRow, CDfup_temp)
         
      }
      
      CDfupProfs <- bind_rows(CDfupProfs)
   } else {
      CDfupProfs <- NULL
   }
   
   
   ### Concentration-dependent B/P -----------------------------------------
   if(Out[["SimulatorUsed"]] != "Simcyp Discovery" &&
      exists("InputTab", inherits = FALSE) &&
      any(str_detect(unlist(c(InputTab[, ColLocations + 1])), 
                     "Concentration-dependent B/P profile"),
          na.rm = TRUE)){
      
      CDBPProfs <- list()
      
      for(i in names(ColLocations)[!names(ColLocations) == "Trial Design"]){
         StartRow <- which(str_detect(t(InputTab[, ColLocations[i] + 1]), 
                                      "Concentration-dependent B/P profile"))[1] + 2
         EndRow <- which(str_detect(t(InputTab[, ColLocations[i]]), 
                                    "B/P [0-9]"))
         EndRow <- EndRow[which.max(EndRow)]
         
         # It could be that one compound has conc-dependent B/P profiles and
         # another compound does not. Checking that here since I did not check it
         # in the original if statement at the top of this section.
         if(is.na(StartRow)){
            next
         }
         
         CDBP_temp <- InputTab[StartRow:EndRow, ColLocations[i]:(ColLocations[i]+1)]
         names(CDBP_temp) <- c("NameCol", "ValCol")
         
         CDBPProfs[[i]] <- data.frame(
            Conc = CDBP_temp$ValCol[which(str_detect(CDBP_temp$NameCol, "Conc"))], 
            BP = CDBP_temp$ValCol[which(str_detect(CDBP_temp$NameCol, "B/P [0-9]"))]) %>%  
            mutate(across(.cols = everything(), .fns = as.numeric), 
                   File = sim_data_file, 
                   CompoundID = i, 
                   Compound = as.character(Out[AllCompounds$DetailNames[
                      AllCompounds$CompoundID == i]])) %>% 
            select(File, CompoundID, Compound, Conc, BP)
         
         rm(StartRow, EndRow, CDBP_temp)
         
      }
      
      CDBPProfs <- bind_rows(CDBPProfs)
   } else {
      CDBPProfs <- NULL
   }
   
   
   ### pH-dependent luminal degradation -----------------------------------------
   if(Out[["SimulatorUsed"]] != "Simcyp Discovery" &&
      exists("InputTab", inherits = FALSE) &&
      any(str_detect(unlist(c(InputTab[, ColLocations])), 
                     "Degradation rate constant"), na.rm = TRUE)){
      
      pHLumDeg <- list()
      
      for(i in names(ColLocations)[!names(ColLocations) == "Trial Design"]){
         StartRow <- which(str_detect(t(InputTab[, ColLocations[i]]), 
                                      "Degradation rate constant"))[1] - 1
         EndRow <- which(str_detect(t(InputTab[, ColLocations[i]]), 
                                    "Degradation rate constant [0-9]{1,} 1.h"))
         EndRow <- EndRow[which.max(EndRow)]
         
         # It could be that one compound has pH-dependent fu,p profiles and
         # another compound does not. Checking that here since I did not check it
         # in the original if statement at the top of this section.
         if(length(StartRow) == 0 || is.na(StartRow)){
            next
         }
         
         pHLumDeg_temp <- InputTab[StartRow:EndRow, ColLocations[i]:(ColLocations[i]+1)]
         names(pHLumDeg_temp) <- c("NameCol", "ValCol")
         
         pHLumDeg[[i]] <- data.frame(
            pH = pHLumDeg_temp$ValCol[which(str_detect(pHLumDeg_temp$NameCol, "pH"))], 
            DegradationRateConstant = pHLumDeg_temp$ValCol[which(str_detect(pHLumDeg_temp$NameCol, "Degradation rate constant [0-9]{1,} 1.h"))]) %>%  
            mutate(across(.cols = everything(), .fns = as.numeric), 
                   File = sim_data_file, 
                   CompoundID = i, 
                   Compound = as.character(Out[AllCompounds$DetailNames[
                      AllCompounds$CompoundID == i]])) %>% 
            select(File, CompoundID, Compound, pH, DegradationRateConstant)
         
         rm(StartRow, EndRow, pHLumDeg_temp)
         
      }
      
      pHLumDeg <- bind_rows(pHLumDeg)
   } else {
      pHLumDeg <- NULL
   }
   
   
   ### pH-dependent solubility -----------------------------------------
   if(Out[["SimulatorUsed"]] != "Simcyp Discovery" &&
      exists("InputTab", inherits = FALSE) &&
      any(str_detect(unlist(c(InputTab[, ColLocations + 1])), 
                     "Solubility-pH profile|User defined pH-Solubility"),
          na.rm = TRUE)){
      
      pHSol <- list()
      
      for(i in names(ColLocations)[!names(ColLocations) == "Trial Design"]){
         StartRow <- which(str_detect(t(InputTab[, ColLocations[i] + 1]), 
                                      "Solubility-pH profile|User defined pH-Solubility"))[1] + 1
         EndRow <- which(str_detect(t(InputTab[, ColLocations[i]]), 
                                    "Entry [0-9]{1,} Solubility"))
         EndRow <- EndRow[which.max(EndRow)]
         
         # It could be that one compound has pH-dependent fu,p profiles and
         # another compound does not. Checking that here since I did not check it
         # in the original if statement at the top of this section.
         if(is.na(StartRow)){
            next
         }
         
         pHSol_temp <- InputTab[StartRow:EndRow, ColLocations[i]:(ColLocations[i]+1)]
         names(pHSol_temp) <- c("NameCol", "ValCol")
         
         pHSol[[i]] <- data.frame(
            pH = pHSol_temp$ValCol[which(str_detect(pHSol_temp$NameCol, "pH"))], 
            Solubility = pHSol_temp$ValCol[which(str_detect(pHSol_temp$NameCol, "Entry [0-9]{1,} Solubility"))]) %>%  
            mutate(across(.cols = everything(), .fns = as.numeric), 
                   File = sim_data_file, 
                   CompoundID = i, 
                   Compound = as.character(Out[AllCompounds$DetailNames[
                      AllCompounds$CompoundID == i]])) %>% 
            select(File, CompoundID, Compound, pH, Solubility)
         
         rm(StartRow, EndRow, pHSol_temp)
         
      }
      
      pHSol <- bind_rows(pHSol)
   } else {
      pHSol <- NULL
   }
   
   
   ## Pulling CL info ----------------------------------------------------
   MyInputDeets2 <- MyInputDeets[str_detect(MyInputDeets, "CLint_")]
   
   if(length(MyInputDeets2) > 0){
      
      for(j in MyInputDeets2){
         
         Suffix <- str_extract(j, "_sub$|_inhib$|_inhib2$|_met1$|_met2$|_secmet$|_inhib1met$")
         NameCol <- InputDeets_DF$NameCol[InputDeets_DF$Detail == j]
         ValueCol <- InputDeets_DF$ValueCol[InputDeets_DF$Detail == j]
         CLRows <- which(
            InputTab[ , NameCol] == "Enzyme" |
               str_detect(InputTab[ , NameCol] %>%
                             pull(),
                          "^Biliary (CLint|Clearance)") |
               str_detect(InputTab[ , NameCol] %>%
                             pull(),
                          "^Additional HLM CLint|^Additional Systemic Clearance|^CLcat \\(L/h|^Lymphatic Clearance \\(L/h|^Additional HKM CLint") |
               str_detect(InputTab[ , ValueCol] %>%
                             pull(),
                          "In Vivo Clear") |
               str_detect(InputTab[ , NameCol] %>%
                             pull(),
                          "(Liver|Intestine|Biliary) Clearance"))
         CLRows <- CLRows[complete.cases(InputTab[CLRows + 1, NameCol])]
         
         if(Out[["SimulatorUsed"]] == "Simcyp Discovery"){
            # Discovery sims have a slightly different setup on the Input
            # Sheet and only the 1st CLRows value should be used b/c that
            # section will contain all the info we need.
            CLRows <- CLRows[1]
            
            MyNames <- as.character(t(
               InputTab[CLRows:nrow(InputTab), NameCol]))
            
            DiscoveryDeets <- 
               data.frame(
                  Detail = c("CLiv_InVivoCL", 
                             "CLpo_InVivoCL", 
                             "CLint_biliary",
                             "CLint_biliary_fuinc", 
                             "CLrenal", 
                             "CL_AddSystemic", 
                             "CL_PercentAvailReabsorption"),
                  RegexRow = c("CL.*iv.*[(]mL", 
                               "CL.*po.*[(]mL", 
                               "Biliary Clearance ..L/min",
                               "Biliary fu inc", 
                               "CL R .mL", 
                               "^Additional Systemic Clearance", 
                               "^Percent.*re-absorption"))
            
            for(i in 1:nrow(DiscoveryDeets)){
               MyRow <- which(str_detect(MyNames, DiscoveryDeets$RegexRow[i]))
               if(length(MyRow) == 0){
                  Out[[paste0(DiscoveryDeets$Detail[i], Suffix)]] <- NA
               } else {
                  suppressWarnings(
                     Out[[paste0(DiscoveryDeets$Detail[i], Suffix)]] <-
                        as.numeric(InputTab[MyRow + CLRows - 1, ValueCol])
                  )
               }
               rm(MyRow)
            }
            
            # Liver and intestinal CL
            LivOrInt <- c("Liver", "Intestine")[
               c(any(str_detect(MyNames, "Liver")), any(str_detect(MyNames, "Intestine")))]
            
            LivIndCL <- 
               data.frame(
                  Detail = c("CL_XXX_Type", 
                             "CL_XXX__UseSaturableKinetics", 
                             "CLint_XXX", 
                             "CL_Km_XXX", 
                             "CL_Vmax_XXX", 
                             "CL_XXX_fuinc", 
                             "CL_XXX_UseMetabolite", 
                             "CL_XXX_MetabPerc", 
                             "CL_XXX_ScrapingsCorrectionFactor", 
                             "CL_XXX_ElutionCorrectionFactor"), 
                  RegexRow = c(paste(i, "Clearance Type"), 
                               "Use Saturable Kinetics", 
                               "CLint", 
                               "Km \\(", 
                               "Vmax \\(", 
                               "fu inc", 
                               "Use Metabolite", 
                               "Metabolite .%", 
                               "\\(scrapings\\) Correction Factor", 
                               "\\(elution\\) Correction Factor")
               )
            
            if(length(LivOrInt[complete.cases(LivOrInt)]) > 0){
               for(i in LivOrInt[complete.cases(LivOrInt)]){
                  OrganRows <- range(
                     which(str_detect(MyNames, i) |
                              str_detect(MyNames,
                                         paste0(str_sub(i, 1, 1), "M")))
                  )
                  
                  for(k in 1:nrow(LivIndCL)){
                     MyRow <- which(str_detect(MyNames[OrganRows[1]:OrganRows[2]],
                                               LivIndCL$RegexRow[k]))
                     if(length(MyRow) == 0){
                        Out[[paste0(sub("XXX", i, LivIndCL$Detail[k]), Suffix)]] <- NA
                     } else {
                        suppressWarnings(
                           Out[[paste0(sub("XXX", i, LivIndCL$Detail[k]), Suffix)]] <-
                              as.character(InputTab[MyRow + CLRows - 1, ValueCol])
                        )
                     }
                     rm(MyRow)
                  }
               }
            }
            
         } else {
            # Regular Simulator data extraction starts here. 
            
            # Checking for interaction data
            IntRowStart <- which(str_detect(
               InputTab[, NameCol] %>%
                  pull(), "Ind max|^Ki |^MBI|Interaction"))[1] - 1
            
            if(complete.cases(IntRowStart)){
               CLRows <- CLRows[CLRows < min(IntRowStart)]
            }
            
            for(i in CLRows){
               
               # CL for a specific enzyme
               if(str_detect(as.character(InputTab[i, NameCol]), "Enzyme")){
                  
                  LastRow_i <- which(is.na(InputTab[, NameCol]))
                  LastRow_i <- LastRow_i[LastRow_i > i][1] - 1
                  
                  Enzyme <- gsub(" ", "", InputTab[i, NameCol + 1])
                  Pathway <- gsub(" |-", "", InputTab[i - 1, NameCol + 1])
                  if(as.character(InputTab[i+1, NameCol]) == "Genotype"){
                     Genotype <- InputTab[i+1, NameCol + 1]
                     Genotype <- gsub("\\*", "star", Genotype)
                     Genotype <- gsub("/", "", Genotype)
                     Enzyme <- paste0(Enzyme, "_", Genotype)
                     CLrow <- i + 2
                  } else if((str_detect(Enzyme, "User") &
                             !str_detect(InputTab[i+1, NameCol], "CLint|Vmax")) |
                            str_detect(tolower(InputTab[i + 1, NameCol]), 
                                       "ontogeny")){
                     CLrow <- i + 2
                  } else {
                     CLrow <- i + 1
                  }
                  
                  CLType <- str_extract(InputTab[CLrow, NameCol],
                                        "CLint|Vmax|t1/2")
                  
                  Names_i <- InputTab[CLrow:LastRow_i, NameCol] %>% pull()
                  
                  if(CLType == "CLint"){
                     
                     # NOTE TO CODERS: I'd been including units for CYP-mediated
                     # CLint in the past, but I realized that I hadn't included
                     # units for other enzymes, which can differ, so I decided
                     # later to omit them. Keeping this bit of code, albeit
                     # commented out, so that we can add them back easily if we
                     # want.
                     
                     # Units <- str_extract(InputTab[CLrow, NameCol], 
                     #                      "\\(.*\\)")
                     # Units <- gsub("\\(|\\)", "", Units)
                     # Units <- gsub("/| ", "_", Units)
                     # # Dealing with mu since it's causing some problems
                     # # downstream when a symbol
                     # Units <- gsub(rlang::chr_unserialise_unicode("<U+00B5>"), 
                     #               "u", Units)
                     
                     suppressWarnings(
                        Out[[paste0(
                           paste("CLint", Enzyme, Pathway, # Units,
                                 sep = "_"),
                           Suffix)]] <-
                           as.numeric(InputTab[CLrow, NameCol + 1])
                     )
                     
                  } else if(CLType == "Vmax"){
                     # Sometimes, Vmax is listed twice. This definitely happens
                     # with user UGT elimination, but not sure about any other
                     # situations. Cannot extract data by index here. 
                     
                     suppressWarnings(
                        Out[[paste0(
                           paste("Vmax", Enzyme,
                                 Pathway, sep = "_"),
                           Suffix)]] <-
                           as.numeric(InputTab[
                              CLrow-1 + which(str_detect(Names_i, "Vmax"))[1],
                              NameCol + 1])
                     )
                     
                     suppressWarnings(
                        Out[[paste0(
                           paste("Km", Enzyme,
                                 Pathway, sep = "_"),
                           Suffix)]] <-
                           as.numeric(InputTab[
                              CLrow-1 + which(str_detect(Names_i, "Km"))[1],
                              NameCol + 1])
                     )
                     
                  } else if(CLType == "t1/2"){
                     suppressWarnings(
                        Out[[paste0(
                           paste("HalfLife", Enzyme,
                                 Pathway, sep = "_"),
                           Suffix)]] <-
                           as.numeric(InputTab[CLrow, NameCol + 1])
                     )
                  }
                  
                  if(any(str_detect(Names_i, "fu_mic"))){
                     suppressWarnings(
                        Out[[paste0(
                           paste("fu_mic", Enzyme,
                                 Pathway, sep = "_"),
                           Suffix)]] <-
                           as.numeric(InputTab[
                              CLrow-1 + which(str_detect(Names_i, "fu mic"))[1],
                              NameCol + 1])
                     )
                  }
                  
                  if(any(str_detect(Names_i, "ISEF"))){
                     suppressWarnings(
                        Out[[paste(CLType, "ISEF", Enzyme,
                                   Pathway, Suffix, sep = "_")]] <-
                           as.numeric(InputTab[
                              CLrow-1 + which(str_detect(Names_i, "ISEF"))[1],
                              NameCol + 1])
                     )
                  }
                  
                  # Check for any UGT-specific CL parameters
                  if(any(str_detect(Names_i, "rUGT"))){
                     
                     rUGTSysInfo <- 
                        InputTab[i:LastRow_i, c(NameCol, ValueCol)] %>% 
                        rename(Name = 1, Value = 2) %>% 
                        filter(str_detect(Name, "rUGT"))
                     
                     Out[[paste0(CLType, "_", Enzyme, "_", Pathway, 
                                 "_rUGTSystem", Suffix)]] <-
                        rUGTSysInfo[which(str_detect(
                           rUGTSysInfo$Name, "rUGTSystem")), ] %>% 
                        pull(Value)
                     
                     suppressWarnings(
                        Out[[paste0(CLType, "_", Enzyme, "_", Pathway,
                                    "_rUGTScalar_liver", Suffix)]] <-
                           rUGTSysInfo[which(
                              str_detect(tolower(rUGTSysInfo$Name), 
                                         "rugtscalar.*liver")), ] %>% 
                           pull(Value) %>% as.numeric())
                     
                     suppressWarnings(
                        Out[[paste0(CLType, "_", Enzyme, "_", 
                                    Pathway, "_rUGTScalar_intestine",
                                    Suffix)]] <-
                           rUGTSysInfo[which(
                              str_detect(tolower(rUGTSysInfo$Name), 
                                         "rugtscalar.*intestine")), ] %>% 
                           pull(Value) %>% as.numeric())
                     
                     suppressWarnings(
                        Out[[paste0(CLType, "_", Enzyme, "_", Pathway, "_rUGTScalar_kidney",
                                    Suffix)]] <-
                           rUGTSysInfo[which(
                              str_detect(tolower(rUGTSysInfo$Name), 
                                         "rugtscalar.*kidney")), ] %>% 
                           pull(Value) %>% as.numeric())
                     
                     rm(rUGTSysInfo)
                  }
                  
                  rm(Enzyme, Pathway, CLType)
                  
               } else if(str_detect(as.character(InputTab[i, NameCol]), "^Biliary (CLint|Clearance)")){
                  suppressWarnings(
                     # Biliary CL
                     Out[[paste0("CLint_biliary", Suffix)]] <-
                        as.numeric(InputTab[i, NameCol + 1])
                  )
                  
               } else if(str_detect(as.character(InputTab[i, NameCol]), "^Additional HLM CLint")){
                  # Other HLM CL
                  suppressWarnings(
                     Out[[paste0("CLint_additional_HLM", Suffix)]] <-
                        as.numeric(InputTab[i, NameCol + 1])
                  )
               } else if(str_detect(as.character(InputTab[i, NameCol]), "^Additional HKM CLint")){
                  # Other HKM CL
                  suppressWarnings(
                     Out[[paste0("CLint_additional_HKM", Suffix)]] <-
                        as.numeric(InputTab[i, NameCol + 1])
                  )
               } else if(str_detect(as.character(InputTab[i, NameCol]), "^CLcat \\(L/h")){
                  suppressWarnings(
                     Out[[paste0("CL_cat_nonspecific", Suffix)]] <-
                        as.numeric(InputTab[i, NameCol + 1])
                  )
               } else if(str_detect(as.character(InputTab[i, NameCol]), "^Lymphatic Clearance \\(L/h")){
                  suppressWarnings(
                     Out[[paste0("CL_lymphatic", Suffix)]] <-
                        as.numeric(InputTab[i, NameCol + 1])
                  )
               } else if(str_detect(as.character(InputTab[i, NameCol]), "^Additional Systemic Clearance \\(L/h\\)")){
                  suppressWarnings(
                     Out[[paste0("CL_additional_systemic", Suffix)]] <-
                        as.numeric(InputTab[i, NameCol + 1])
                  )
               } else if(str_detect(as.character(InputTab[i, ValueCol]),
                                    "In Vivo Clearance")){
                  # in vivo CL
                  MyNames <- as.character(t(
                     InputTab[i:min(
                        c(IntRowStart, CLRows[which(CLRows == i) + 1] - 1, 
                          nrow(InputTab)), na.rm = T), NameCol]))
                  
                  suppressWarnings(
                     Out[[paste0("CLiv_InVivoCL", Suffix)]] <- 
                        as.numeric(InputTab[
                           which(str_detect(MyNames,
                                            "CL.*iv.*[(](m)?L")) + i - 1,
                           ValueCol])
                  )
                  
                  suppressWarnings(
                     Out[[paste0("CLbiliary_InVivoCL", Suffix)]] <- 
                        as.numeric(InputTab[
                           which(str_detect(MyNames,
                                            "Biliary Clearance")) + i - 1,
                           ValueCol])
                  )
                  
                  suppressWarnings(
                     Out[[paste0("CLadditional_InVivoCL", Suffix)]] <- 
                        as.numeric(InputTab[
                           which(str_detect(MyNames,
                                            "Additional Systemic Clearance")) + i - 1,
                           ValueCol])
                  )
                  
                  suppressWarnings(
                     Out[[paste0("CLpo_InVivoCL", Suffix)]] <- 
                        as.numeric(InputTab[
                           which(str_detect(MyNames,
                                            "^CL .po.")) + i - 1,
                           ValueCol])
                  )
                  
               }
               
            }
            rm(CLRows, IntRowStart, NameCol, Suffix)
         }
      }
   } 
   
   ## Pulling interaction info -------------------------------------------
   MyInputDeets3 <- MyInputDeets[str_detect(MyInputDeets, "Interaction_")]
   
   if(length(MyInputDeets3) > 0){
      
      for(j in MyInputDeets3){
         
         Suffix <- str_extract(j, "_sub$|_inhib$|_inhib2$|_met1$|_secmet$|_inhib1met$")
         NameCol <- InputDeets_DF$NameCol[InputDeets_DF$Detail == j]
         ValueCol <- InputDeets_DF$ValueCol[InputDeets_DF$Detail == j]
         IntRows <- which(str_detect(InputTab[ , NameCol] %>% pull(),
                                     "^Enzyme$|^Transporter$"))
         IntRows <- IntRows[complete.cases(InputTab[IntRows + 1, NameCol])]
         
         # Only IntRows after the first instance of an
         # interaction type of term is found in NameCol. NB: I
         # thought it would work to just look for cells after
         # "interaction", but "interaction" hasn't always been
         # listed in the output files I've found.
         IntRowStart <- which(str_detect(InputTab[, NameCol] %>%
                                            pull(), "Ind [mM]ax|Ind [sS]lope|^Ki |^MBI"))[1] - 1
         TransporterTissues <- IntRows[which(
            str_detect(t(InputTab[IntRows, NameCol]), "Transporter"))]
         TransporterTissues <- TransporterTissues[which(
            str_detect(t(InputTab[TransporterTissues - 1, NameCol]), 
                       "Organ/Tissue"))] - 1
         TransporterTissues <- data.frame(
            Row = TransporterTissues, 
            Tissue = as.character(t(InputTab[TransporterTissues, ValueCol])))
         
         if(complete.cases(IntRowStart)){
            
            IntRows <- IntRows[IntRows >= IntRowStart]
            
            for(i in IntRows){
               Enzyme <- gsub(" |\\(|\\)|-|/", "_", InputTab[i, NameCol + 1])
               Enzyme <- gsub("_{2,}", "_", Enzyme)
               Enzyme <- sub("_$", "", Enzyme)
               NextEmptyCell <- which(is.na(InputTab[, NameCol + 1]))
               NextEmptyCell <- NextEmptyCell[NextEmptyCell > i][1]
               # If there's another interaction listed
               # before the next empty cell, need to
               # account for that.
               NextInt <- IntRows[which(IntRows == i) + 1] - 1
               NextInt <- ifelse(i == IntRows[length(IntRows)],
                                 nrow(InputTab), NextInt)
               ThisIntRows <- i:(c(NextEmptyCell, NextInt)[which.min(c(NextEmptyCell, NextInt))])
               ThisIntRows <- setdiff(ThisIntRows, NextEmptyCell)
               
               # Induction
               IndParam1stRow <- which(str_detect(InputTab[ThisIntRows, NameCol] %>% pull(),
                                                  "Ind max|Ind Slope"))
               if(length(IndParam1stRow) > 0){
                  IndModelCheck <- list(str_detect(t(InputTab[ThisIntRows, NameCol]), "Ind max"), 
                                        str_detect(t(InputTab[ThisIntRows, NameCol]), "Ind Slope"), 
                                        str_detect(t(InputTab[ThisIntRows, NameCol]), "Ind( )?C50"), 
                                        str_detect(t(InputTab[ThisIntRows, NameCol]), "fu inc"), 
                                        str_detect(t(InputTab[ThisIntRows, NameCol]), "\u03B3"))
                  IndModelCheck <- sapply(IndModelCheck, FUN = function(x) which(x == TRUE))
                  # Note: I can't seem to get regex to work for
                  # detecting a Greek character; I figured that the
                  # Hill coefficient gamma is the only time that an
                  # induction parameter name is only going to be one
                  # character long.
                  
                  suppressWarnings(
                     Out[[paste0(
                        paste("IndMax", Enzyme,
                              sep = "_"), Suffix)]] <-
                        as.numeric(InputTab[ThisIntRows[IndModelCheck[[1]][1]], NameCol + 1])
                  )
                  
                  suppressWarnings(
                     Out[[paste0(
                        paste("IndSlope", Enzyme,
                              sep = "_"), Suffix)]] <-
                        as.numeric(InputTab[ThisIntRows[IndModelCheck[[2]][1]], NameCol + 1])
                  )
                  
                  suppressWarnings(
                     Out[[paste0(
                        paste("IndC50", Enzyme,
                              sep = "_"), Suffix)]] <-
                        as.numeric(InputTab[ThisIntRows[IndModelCheck[[3]][1]], NameCol + 1])
                  )
                  
                  suppressWarnings(
                     Out[[paste0(
                        paste("Ind_fu_inc", Enzyme,
                              sep = "_"), Suffix)]] <-
                        as.numeric(InputTab[ThisIntRows[IndModelCheck[[4]][1]], NameCol + 1])
                  )
                  
                  suppressWarnings(
                     Out[[paste0(
                        paste("Ind_gamma", Enzyme,
                              sep = "_"), Suffix)]] <-
                        as.numeric(InputTab[ThisIntRows[IndModelCheck[[5]][1]], NameCol + 1])
                  )
                  
                  rm(IndModelCheck)   
               }
               
               # competitive inhibition
               Ki <- which(str_detect(InputTab[ThisIntRows, NameCol] %>% pull(),
                                      "Ki "))
               if(length(Ki) > 0){
                  
                  EnzTrans <- as.character(InputTab[i, NameCol])
                  
                  if(EnzTrans == "Transporter"){
                     Enzyme <-
                        paste0(Enzyme, "_",
                               # setting the tissue 
                               TransporterTissues %>% 
                                  filter(Row <= i) %>% 
                                  filter(Row == max(Row)) %>% 
                                  pull(Tissue))
                  }
                  
                  suppressWarnings(
                     Out[[paste0(
                        paste("Ki", Enzyme,
                              sep = "_"), Suffix)]] <-
                        as.numeric(InputTab[ThisIntRows[Ki], NameCol + 1])
                  )
                  
                  # fu mic or fu inc
                  IncType <- str_extract(InputTab[ThisIntRows[Ki+1], NameCol] %>%
                                            pull(),
                                         "inc|mic")
                  suppressWarnings(
                     Out[[paste0(
                        paste(switch(IncType,
                                     "inc" = "Ki_fu_inc",
                                     "mic" = "Ki_fu_mic"),
                              Enzyme,
                              sep = "_"), Suffix)]] <-
                        as.numeric(InputTab[ThisIntRows[Ki+1], NameCol + 1])
                  )
                  
                  rm(IncType, EnzTrans)
               }
               
               # MBI
               MBI <-  which(str_detect(InputTab[ThisIntRows, NameCol] %>% pull(),
                                        "MBI Kapp"))
               if(length(MBI) > 0){
                  suppressWarnings(
                     Out[[paste0(
                        paste("MBI_Kapp", Enzyme,
                              sep = "_"), Suffix)]] <-
                        as.numeric(InputTab[ThisIntRows[MBI], NameCol + 1])
                  )
                  
                  suppressWarnings(
                     Out[[paste0(
                        paste("MBI_kinact", Enzyme,
                              sep = "_"), Suffix)]] <-
                        as.numeric(InputTab[ThisIntRows[MBI+1], NameCol + 1])
                  )
                  
                  suppressWarnings(
                     Out[[paste0(
                        paste("MBI_fu_mic", Enzyme,
                              sep = "_"), Suffix)]] <-
                        as.numeric(InputTab[ThisIntRows[MBI+2], NameCol + 1])
                  )
               }
               
               rm(Enzyme, NextEmptyCell, NextInt,
                  ThisIntRows, IndParam1stRow, Ki, MBI)
            }
         }
         
         rm(Suffix, IntRows, IntRowStart, NameCol)
      }
   }
   
   ## Dealing with StartDayTime_x --------------------------------------
   MyInputDeets4 <- MyInputDeets[str_detect(MyInputDeets, "^StartDayTime")]
   
   if(length(MyInputDeets4) > 0){
      for(j in MyInputDeets4){
         
         NameCol <- InputDeets_DF$NameCol[which(InputDeets_DF$Detail == j)]
         ValueCol <- InputDeets_DF$ValueCol[InputDeets_DF$Detail == j]
         Row_day <- which(str_detect(InputTab[, NameCol] %>% pull(), "Start Day"))
         # If this is not present, which sometimes happens with a custom
         # dosing schedule, then will need to pull info from custom
         # dosing sheet lower in script.
         if(length(Row_day) == 0){
            CustomDosing <- c(CustomDosing, TRUE)
         } else {
            Val_day <- InputTab[Row_day, InputDeets_DF$ValueCol[
               which(InputDeets_DF$Detail == j)]] %>% pull()
            Row_time <- which(str_detect(InputTab[, NameCol] %>% pull(), "Start Time"))
            Val_time <- InputTab[Row_time, InputDeets_DF$ValueCol[
               which(InputDeets_DF$Detail == j)]] %>% pull()
            
            # Dealing with inconsistencies in time format
            Val_time <- sub("m", "", Val_time)
            Val_time <- str_split(Val_time, pattern = "h")[[1]]
            Val_time <- str_c(str_pad(Val_time, width = 2, pad = "0"),
                              collapse = ":")
            
            Out[[j]] <- paste(paste0("Day ", Val_day),
                              Val_time, sep = ", ")
         }
      }
   }
   
   
   ## Transport parameters ----------------------------------------------
   MyInputDeets5 <- MyInputDeets[str_detect(MyInputDeets, "Transport_")]
   MyInputDeets5 <- InputDeets_DF %>% 
      filter(Detail %in% MyInputDeets5 & complete.cases(NameCol)) %>%
      pull(Detail)
   
   if(length(MyInputDeets5) > 0){
      
      for(j in MyInputDeets5){
         
         Suffix <- str_extract(j, "_sub$|_inhib$|_inhib2$|_met1$|_secmet$|_inhib1met$")
         NameCol <- InputDeets_DF$NameCol[InputDeets_DF$Detail == j]
         ValueCol <- InputDeets_DF$ValueCol[InputDeets_DF$Detail == j]
         
         # There can be transporter interactions higher up on the tab,
         # but parameters that are actually just transport parameters all
         # come after the title "Transport".
         StartTrans <- which(InputTab[, NameCol] %>% pull() == "Transport") 
         
         if(length(StartTrans) > 0){
            TransRows <- which(str_detect(InputTab[ , NameCol] %>% pull(),
                                          "^Transporter"))
            TransRows <- TransRows[TransRows > StartTrans]
            
            if(length(TransRows) > 0){
               
               # Sometimes, the organ is only listed once and then not listed
               # again for subsequent transporters until it changes. 
               OrganRows <- which(str_detect(InputTab[, NameCol] %>% pull(), 
                                             "Organ/Tissue"))
               OrganRows <- OrganRows[OrganRows >= min(TransRows) - 1 & 
                                         OrganRows < max(TransRows)]
               
               # BBB transporter parameters are set up differently, so
               # removing any organ rows for those scenarios.
               BrainRows <- OrganRows + 1
               BrainRows <- BrainRows[which(str_detect(
                  InputTab[BrainRows, NameCol] %>% pull(), "BBB|BCSFB|ISF.ICF"))]
               
               OrganRows <- setdiff(OrganRows, BrainRows - 1)
               
               for(i in TransRows){
                  
                  # Last row always seems to contain RAF/REF or ISEF,T
                  TransRowLast <- which(str_detect(InputTab[ , NameCol] %>% pull(), 
                                                   "RAF/REF|ISEF"))
                  TransRowLast <- TransRowLast[TransRowLast > i]
                  TransRowLast <- ifelse(length(TransRowLast) == 0, 
                                         nrow(InputTab), TransRowLast[1])
                  TransRowNames <- InputTab[i:TransRowLast, NameCol] %>% pull(1)
                  Transporter <- gsub(" |\\(|\\)|-|/", "_", InputTab[i, NameCol + 1])
                  Transporter <- gsub("_{2,}", "_", Transporter)
                  Transporter <- sub("_$", "", Transporter)
                  Transporter <- case_match(Transporter, 
                                            "Apical_Efflux_Kidney" ~ "General_Apical_Efflux", 
                                            .default = Transporter)
                  
                  Location <- gsub(" |\\(|\\)|-|/", "", 
                                   InputTab[c(i:TransRowLast)[which(TransRowNames == "Location")],
                                            ValueCol] %>% pull(1))
                  Organ <- InputTab[max(OrganRows[OrganRows < i]), ValueCol] %>% 
                     pull() %>% str_comma() # This should have length 1, but adding str_comma just in case. 
                  
                  # Organ <- which(str_detect(as.character(t(
                  #    InputTab[(i-1):TransRowLast, NameCol])), "Organ/Tissue"))
                  # Organ <- ifelse(length(Organ) > 0, 
                  #                 as.character(InputTab[((i-1):TransRowLast)[Organ], ValueCol]), 
                  #                 "")
                  
                  ParamPrefix <- paste("Transporter", Organ, Transporter, Location, sep = "_")
                  
                  # Either CLint,T or Jmax and Km values will be listed
                  if(any(str_detect(TransRowNames, "CLint,T"))){
                     
                     suppressWarnings(
                        Out[[paste0(ParamPrefix, "_CLintT", Suffix)]] <- 
                           as.numeric(
                              InputTab[c(i:TransRowLast)[which(str_detect(TransRowNames, "CLint,T"))],
                                       ValueCol] %>% pull(1))
                     )
                     
                  } else if(any(str_detect(TransRowNames, "Jmax"))){
                     
                     suppressWarnings(
                        Out[[paste0(ParamPrefix, "_Jmax", Suffix)]] <- 
                           as.numeric(
                              InputTab[c(i:TransRowLast)[which(str_detect(TransRowNames, "Jmax"))],
                                       ValueCol] %>% pull(1))
                     )
                     
                     suppressWarnings(
                        Out[[paste0(ParamPrefix, "_Km", Suffix)]] <- 
                           as.numeric(
                              InputTab[c(i:TransRowLast)[which(str_detect(TransRowNames, "Km"))],
                                       ValueCol] %>% pull(1))
                     )
                     
                  }
                  
                  # Checking for fuinc values b/c they're not always there
                  fuinc <- as.numeric(
                     InputTab[c(i:TransRowLast)[which(str_detect(TransRowNames, "fuinc"))],
                              ValueCol] %>% pull(1))
                  
                  if(length(fuinc) > 0){
                     suppressWarnings(
                        Out[[paste0(ParamPrefix, "_fuinc", Suffix)]] <- fuinc
                     )
                  }
                  rm(fuinc)
                  
                  # Checking for RAF/REF values b/c they're not always there
                  RAFREF <- as.numeric(
                     InputTab[c(i:TransRowLast)[which(str_detect(TransRowNames, "ISEF|RAF|REF"))],
                              ValueCol] %>% pull(1))
                  
                  if(length(RAFREF) > 0){
                     suppressWarnings(
                        Out[[paste0(ParamPrefix, "_RAFREF", Suffix)]] <- RAFREF
                     )
                  }
                  rm(RAFREF)
                  
                  rm(TransRowLast, Transporter, TransRowNames, Location, 
                     ParamPrefix)
               }
            }
            
         }
      }
   }
   
   
   # Paediatric age bins --------------------------------------------------
   
   # This only applies when subjects are redefined over the course of the
   # simulation. 
   
   if(any(c("Bin 1 start", "Bin 1 frequency") %in%
          t(InputTab[, ColLocations["Trial Design"]]))){
      
      Out$Redefine_subjects_over_time <- "yes"
      
      StartRow <- which(t(InputTab[, ColLocations["Trial Design"]]) == "Bin 1 start")
      EndRow <- max(which(str_detect(
         t(InputTab[, ColLocations["Trial Design"]]), "Bin [0-9]{1,}")))
      
      AgeBins <- InputTab[StartRow:EndRow, c(ColLocations["Trial Design"], 
                                             ColLocations["Trial Design"] + 1)] 
      names(AgeBins) <- c("Orig", "Value")
      
      AgeBins <- AgeBins %>% 
         separate_wider_delim(cols = Orig, delim = " ", 
                              names = c(NA, "Bin", "Type")) %>% 
         pivot_wider(names_from = Type, 
                     values_from = Value) %>% 
         mutate(File = sim_data_file, 
                Bin = as.numeric(Bin), 
                Start = as.numeric(start), 
                End = as.numeric(end)) %>% 
         rename(Frequency = frequency) %>% 
         select(File, Bin, Start, End, Frequency)
      
   } else {
      Out$Redefine_subjects_over_time <- "no"
      AgeBins <- NULL
   }
   
   
   # Returning --------------------------------------------------------------
   Out <- Out[sort(names(Out))]
   Out[["ConcDependent_BP"]] <- CDBPProfs
   Out[["ConcDependent_fup"]] <- CDfupProfs 
   Out[["CustomDosing"]] <- any(CustomDosing, na.rm = T)
   Out[["DissolutionProfiles"]] <- DissoProfs
   Out[["Age_bins_redef_over_time"]] <- AgeBins
   Out[["pH_dependent_LumindalDegradation"]] <- pHLumDeg
   Out[["pH_dependent_solubility"]] <- pHSol
   Out[["ReleaseProfiles"]] <- ReleaseProfs
   
   return(Out)
   
}


