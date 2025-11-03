#' Extract simulation experimental details for Simcyp Simulator or Discovery
#' files
#'
#' @description \code{extractExpDetails} looks up experimental design details
#'   from a Simcyp Simulator output file.
#'
#'   For detailed instructions and examples, please see the SharePoint file
#'   "Simcyp PBPKConsult R Files - Simcyp PBPKConsult R Files/SimcypConsultancy
#'   function examples and instructions/Checking simulation experimental
#'   details/Checking-simulation-experimental-details.docx". (Sorry, we are
#'   unable to include a link to it here.)
#'
#' @param sim_data_file name of the Excel file containing the simulator output,
#'   in quotes. \strong{A note:} There are just a few items that we will attempt
#'   to extract from the matching workspace file; for that information, we will
#'   look for a workspace file that is named \emph{identically} to the Excel
#'   file except for the file extension. It will ignore the date/time stamp that
#'   the autorunner adds as long as that stamp is in a format like this: "myfile
#'   - 2023-10-31 07-23-15.xlsx".
#' @param exp_details Experiment details you want to extract from the simulator
#'   output file. Options are \describe{
#'
#'   \item{"Summary and Input"}{Extract details available from the "Summary tab"
#'   and the "Input Sheet" (default)}
#'
#'   \item{"population tab"}{Extract details about the population used (data
#'   come from the tab with the same name as the population simulated)}
#'
#'   \item{"Simcyp inputs"}{Extract all the details that you normally fill out
#'   on the "Simcyp inputs (and QC)" tab of a compound data sheet plus trial
#'   design information}
#'
#'   \item{"workspace"}{Extract a limited set of details directly
#'   from the Simcyp Simulator workspace files. The set of possible details may
#'   be viewed by entering \code{view(AllWorkspaceDetails)} in the console. This
#'   \emph{only} works if the workspace file name perfectly matches the Excel
#'   results file name and is located in the same folder. Otherwise, this step
#'   in the data extraction will be skipped.}
#'
#'   \item{"all"}{Extract all possible parameters}}
#'
#'   \strong{NOTES:} \enumerate{\item{The default pulls parameters from the
#'   "Summary" tab and the "Input Sheet" tab. Note that the
#'   "Summary" tab does not include information on any compounds beyond the
#'   substrate and inhibitor 1.} \item{There are a few places where
#'   requesting one item as input will get you multiple items as output:
#'   intrinsic clearance, interaction parameters, and transport parameters. For
#'   example, if you request intrinsic clearance values (ex: "CLint_sub"),
#'   you'll get all the intrinsic clearance values for that compound, and
#'   they'll be named according to which parameter it is, which enzyme it's for,
#'   etc. Same thing with requesting interaction parameters (ex:
#'   "Interaction_inhib" to get all the interaction parameters for inhibitor 1)
#'   and transporter parameters (ex: "Transport_sub").}}
#' @return Returns a named list of simulation experimental details for simulator
#'   files
#' @export
#'
#' @examples
#'
#' extractExpDetails(sim_data_file = "../Example simulator output.xlsx")
#' extractExpDetails(sim_data_file = "../Example simulator output MD + inhibitor.xlsx")
#' extractExpDetails(sim_data_file = "../Example simulator output.xlsx",
#'                   exp_details = "all")
#'
#'
#' 
extractExpDetails <- function(sim_data_file,
                              exp_details = "Summary and Input", 
                              sheet_names = NA){
   
   # Error catching ---------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
      
   # If they didn't include ".xlsx" at the end, add that.
   sim_data_file <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$", "", sim_data_file), ".xlsx")
   
   # Checking that the file is, indeed, a simulator output file.
   if(all(is.na(sheet_names))){
      SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                             error = openxlsx::getSheetNames(sim_data_file))
   } else {
      SheetNames <- sheet_names
   }
   
   if(all(c("Input Sheet", "Summary") %in% SheetNames) == FALSE){
      # Using "warning" instead of "stop" here b/c I want this to be able to
      # pass through to extractExpDetails_mult and just skip any files that
      # aren't simulator output.
      warning(wrapn(paste0("The file '", sim_data_file,
                           "' does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file.")), 
              call. = FALSE)
      return(list())
   }
   
   # Checking for file name issues
   CheckFileNames <- check_file_name(sim_data_file)
   BadFileNames <- CheckFileNames[!CheckFileNames == "File name meets naming standards."]
   if(length(BadFileNames)> 0){
      BadFileNames <- paste0(names(BadFileNames), ": ", BadFileNames)
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"),
                     "\n"), 
              call. = FALSE)
   }
   
   # Cleaning up possible problems w/how exp_details by tab might be inputted
   if(length(exp_details) != 1){
      warning("You can only enter one value for what set of details you want for the argument `exp_details`. We'll set this to `all` for now.\n", 
              call. = FALSE)
      exp_details <- "all"
   }
   
   exp_details <- tolower(exp_details[1])
   
   if(str_detect(exp_details, "summary|input")){
      exp_details <- "summary and input"
   } else if(str_detect(exp_details, "population")){
      exp_details <- "population tab"
   } else if(str_detect(exp_details, "worksp")){
      exp_details <- "workspace"
   } 
   
   if(exp_details %in% c("summary and input", "population tab", "workspace", 
                         "all", "simcyp inputs") == FALSE){
      warning(wrapn("The only options for the argument `exp_details` are `Summary and Input`, `population tab`, `workspace`, `Simcyp inputs`, `VBE` (only applies to VBE simulations), or `all` (not case sensitive), and you've supplied something else. We'll set this to `all` for now."), 
              call. = FALSE)
      exp_details <- "all"
   }
   
   # Noting exp_details requested for later
   exp_details_input <- exp_details
   
   
   # Main body of function ----------------------------------------------------
   
   # Noting which details are possible, which columns to search for their
   # names, which columns contain their values for substrates or inhibitors,
   # and what kind of data to format the output as at the end. Using data
   # object AllExpDetails.
   
   # Summary tab only includes info on Substrate & Inhibitor1
   SumDeets <- AllExpDetails %>% 
      filter(DataSource == "Summary" &
                (is.na(CompoundID) | 
                    CompoundID %in% c("substrate", "inhibitor 1")) &
                !Detail %in% c("PrimaryMetabolite1", 
                               "PrimaryMetabolite2", 
                               "SecondaryMetabolite", 
                               "Inhibitor2", 
                               "Inhibitor1Metabolite")) %>% 
      rename(Deet = Detail) %>% arrange(Deet)
   
   # If it's on input sheet but isn't for a specific compound, then it's about
   # the trial design b/c we haven't set this up to pull any information from
   # the "Simulation Toolbox" or "Software Version Detail" sections.
   InputDeets <- AllExpDetails %>% filter(DataSource == "Input Sheet") %>% 
      rename(Deet = Detail) %>% arrange(Deet) %>% 
      mutate(CompoundID = ifelse(is.na(CompoundID), "Trial Design", CompoundID))
   
   PopDeets <- AllExpDetails %>% filter(DataSource == "population") %>% 
      rename(Deet = Detail) %>% arrange(Deet)
   
   # Determining info to pull
   exp_details <- 
      switch(exp_details_input, 
             "all" = unique(AllExpDetails$Detail), 
             "summary tab" = SumDeets$Deet, 
             "input sheet" = InputDeets$Deet, 
             "summary and input" = c(SumDeets$Deet, InputDeets$Deet),
             "population tab" = PopDeets$Deet, 
             "simcyp inputs" = AllExpDetails %>% 
                filter(complete.cases(CDSInputMatch)) %>% 
                pull(Detail))
   
   # There are some details that we will just ALWAYS need to include b/c so
   # many downstream functions rely on them. Making sure that these are
   # included. 
   exp_details <- 
      unique(c(exp_details, 
               AllRegCompounds$DetailNames, 
               "Units_AUC", "Units_Cmax", "Units_CL", "Units_tmax",
               "PopRepSim", "SimulatorUsed",
               paste0(rep(c("StartHr", "StartDayTime", "Regimen", "MW",
                            "Dose", "NumDoses", "DoseInt", "DoseRoute", 
                            "ReleaseProfileAvailable"),
                          each = 3), 
                      c("_sub", "_inhib", "_inhib2"))))
   
   # This needs to exist for all scenarios, even if we're not checking for it.
   ReleaseProfs <- NULL
   
   # Need to note original exp_details requested b/c I'm adding to it if people
   # request info from other tabs than what they've originally got. Note that
   # this is different from "exp_details_input" and serves a different purpose.
   exp_details_orig <- exp_details
   
   # When user requests HSA or AGP, they actually want all the individual betas
   # for that. Adjusting to account for that here.
   if("HSA" %in% exp_details_orig){
      exp_details <- unique(c(exp_details, "HSA_C0_female",
                              "HSA_C0_male", "HSA_C1_female",
                              "HSA_C1_male", "HSA_C2_female",
                              "HSA_C2_male", "HSA_male", "HSA_female"))
      exp_details <- exp_details[!exp_details == "HSA"]
   }
   
   if("HSA_male" %in% exp_details_orig){
      exp_details <- unique(c(exp_details, "HSA_male", "HSA_C0_male",
                              "HSA_C1_male", "HSA_C2_male"))
      exp_details <- exp_details[!exp_details == "HSA_male"]
   }
   
   if("HSA_female" %in% exp_details_orig){
      exp_details <- unique(c(exp_details, "HSA_female", "HSA_C0_female",
                              "HSA_C1_female", "HSA_C2_female"))
      exp_details <- exp_details[!exp_details == "HSA_female"]
   }
   
   if("AGP" %in% exp_details_orig){
      exp_details <- unique(c(exp_details, "AGP_male", "AGP_female"))
      exp_details <- exp_details[!exp_details == "AGP"]
   }
   
   if(any(exp_details %in% AllExpDetails$Detail) == FALSE){
      Problem <- str_comma(unique(setdiff(exp_details,
                                          AllExpDetails$Detail)))
      warning(paste0("These study details are not among the possible options:\n",
                     str_c(Problem, collapse = "\n"), 
                     wrapn("They will be omitted. Please enter 'view(ExpDetailDefinitions)' into the console for all options.")),
              call. = FALSE)
      exp_details <- intersect(exp_details, AllExpDetails$Detail)
   }
   
   if(length(exp_details) == 0){
      stop("You must enter at least one study detail to extract.",
           call. = FALSE)
   }
   
   Out <- list()
   
   # Noting all sheet names. This saves time for later data extraction and
   # also helps with debugging and coding in general. 
   Out[["SheetNames"]] <- str_c(paste0("`", SheetNames, "`"), collapse = " ")
   
   if(any(exp_details %in% PopDeets$Deet)){
      exp_details <- c(exp_details, "Population")
      exp_details <- unique(exp_details)
   }
   
   # Need to note when to look for custom dosing tabs
   CustomDosing <- NA
   
   # Pulling details from the summary tab ----------------------------------
   MySumDeets <- sort(intersect(exp_details, SumDeets$Deet))
   
   if(exp_details_input[1] %in% c("population tab")){
      MySumDeets <- c("Population", "SimulatorVersion")
   }
   
   if(length(MySumDeets) > 0){
      
      # Long file names cause problems for readxl but not openxlsx, for
      # some reason. That's why there's the error function calling on
      # openxlsx.
      SummaryTab <- suppressMessages(tryCatch(
         readxl::read_excel(path = sim_data_file, sheet = "Summary",
                            col_names = FALSE),
         error = openxlsx::read.xlsx(sim_data_file, sheet = "Summary",
                                     colNames = FALSE)))
      # If openxlsx read the file, the names are different. Fixing.
      if(names(SummaryTab)[1] == "X1"){
         names(SummaryTab) <- paste0("...", 1:ncol(SummaryTab))
      }
      
      # We'll select details based on whether this was a Discovery simulation. 
      Out[["SimulatorUsed"]] <- ifelse(str_detect(SummaryTab[1, 1], "Discovery"), 
                                       "Simcyp Discovery", "Simcyp Simulator")
      DiscoveryCol <- switch(Out[["SimulatorUsed"]], 
                             "Simcyp Discovery" = c("Simulator and Discovery", 
                                                    "Discovery only"), 
                             "Simcyp Simulator" = c("Simulator only", 
                                                    "Simulator and Discovery"))
      Out[["ExcelResultsTimeStamp"]] <- 
         timeConv(as.numeric(SummaryTab[2, 1]), dataSource = "Excel")
      
      # Need to filter to keep only details that we can possibly find based on
      # what type of simulator was used
      SumDeets <- SumDeets %>% filter(SimulatorAvailability %in% DiscoveryCol)
      MySumDeets <- MySumDeets[MySumDeets %in% SumDeets$Deet]
      
      # sub function for finding correct cell
      pullValue <- function(deet){
         
         # Setting up regex to search
         ToDetect <- SumDeets %>% 
            filter(Deet == deet) %>% pull(Regex_row)
         NameCol <- SumDeets$NameCol[which(SumDeets$Deet == deet)]
         Row <- which(str_detect(SummaryTab[, NameCol] %>% pull(), ToDetect))
         if(length(Row) == 0 || all(is.null(Row)) || all(is.na(Row))){
            return(NA)
         }
         Val <- SummaryTab[Row, SumDeets$ValueCol[SumDeets$Deet == deet]] %>%
            pull()
         
         # Accounting for when fu,p is scripted
         if(length(Val) > 0 && 
            (any(complete.cases(Val)) && 
             str_detect(deet, "^fu_") & any(str_detect(Val, "script")))){
            SumDeets$Class[SumDeets$Deet == deet] <- "character"
            assign("SumDeets", SumDeets, envir = parent.frame())
         }
         
         suppressWarnings(
            Val <- switch(SumDeets$Class[SumDeets$Deet == deet], 
                          "character" = as.character(Val),
                          "numeric" = as.numeric(Val))
         )
         
         if(length(Val) > 1){
            Val <- str_comma(unique(Val))
         }
         
         # Tidying up some specific idiosyncracies of simulator output
         Val <- ifelse(complete.cases(Val) & Val == "n/a", NA, Val)
         Val <- ifelse(str_detect(deet, "^Unit"),
                       str_trim(gsub("\\(unbound\\)|\\(blood\\)|\\(unbound blood\\)|Dose \\(|\\)|CMax \\(|TMax \\(|AUC \\(|CL \\(Dose/AUC\\)\\(|\\(blood\\)",
                                     "", Val)), Val)
         Val <- ifelse(deet %in% c("SimDuration"),
                       as.numeric(Val), Val)
         Val <- ifelse(deet == "SimulatorVersion",
                       str_extract(Val, "Version [12][0-9]"),
                       Val)
         if(deet == "PKTissue_Discovery"){
            ConcUnit <- str_extract(Val, "(ng|mg|µg|µM|nM)(/)?(mL|L)?")
            Val <- case_match(Val, 
                              paste0("CMax (", ConcUnit, ")") ~ "plasma", 
                              paste0("CMax (", ConcUnit, ")(blood)") ~ "blood",
                              paste0("CMax (", ConcUnit, ")(unbound)") ~ "unbound plasma", 
                              paste0("CMax (", ConcUnit, ")(unbound blood)") ~ "unbound blood")
         }
         
         return(Val)
      }
      
      for(i in MySumDeets){
         Out[[i]] <- pullValue(i)
         
         if(i == "Population" & is.na(Out[[i]])){
            # This can happen when the simulator output is actually from Simcyp
            # Discovery or the Simcyp Animal Simulator. Look for
            # "species" in that case.
            Out[[i]] <- as.character(SummaryTab[which(SummaryTab$...1 == "Species"), 2])
         }
         
      }
      
      # Simcyp Discovery only allows simulations with a substrate or metabolite
      # 1 and no other compounds, so everything else will be NA or NULL.
      if(str_detect(SummaryTab[1, 1], "Discovery")){
         Out[c("Inhibitor1", "Inhibitor2", "Inhibitor1Metabolite", 
               "PrimaryMetabolite2", "SecondaryMetabolite")] <- NA
      }
      
      # Removing details that don't apply, e.g., _inhib parameters when there
      # was no inhibitor.
      if(length(Out$Inhibitor1) > 0 &&
         is.na(Out$Inhibitor1) & any(str_detect(names(Out), "_inhib$"))){
         Out <- Out[-which(str_detect(names(Out), "_inhib$"))]
      }
      
      if(length(Out$Inhibitor2) > 0 &&
         is.na(Out$Inhibitor2) & any(str_detect(names(Out), "_inhib2$"))){
         Out <- Out[-which(str_detect(names(Out), "_inhib2$"))]
      }
      
      if(length(Out$Inhibitor1Metabolite) > 0 &&
         is.na(Out$Inhibitor1Metabolite) & any(str_detect(names(Out), "_inhib1met$"))){
         Out <- Out[-which(str_detect(names(Out), "_inhib1met$"))]
      }
      
      if(length(Out$PrimaryMetabolite1) > 0 &&
         is.na(Out$PrimaryMetabolite1) & any(str_detect(names(Out), "_met$"))){
         Out <- Out[-which(str_detect(names(Out), "_met$"))]
      }
      
      if(length(Out$PrimaryMetabolite2) > 0 &&
         is.na(Out$PrimaryMetabolite2) & any(str_detect(names(Out), "_met2$"))){
         Out <- Out[-which(str_detect(names(Out), "_met2$"))]
      }
      
      if(length(Out$SecondaryMetabolite) > 0 &&
         is.na(Out$SecondaryMetabolite) & any(str_detect(names(Out), "secmet$"))){
         Out <- Out[-which(str_detect(names(Out), "_secmet$"))]
      }
   }
   
   # Pulling details from the Input Sheet tab ------------------------------
   
   MyInputDeets <- intersect(exp_details, InputDeets$Deet)
   # Not pulling the same info twice
   MyInputDeets <- setdiff(MyInputDeets, names(Out))
   
   if(length(MyInputDeets) > 0){
      
      MyInputDeets <- unique(c(MyInputDeets, AllCompounds$DetailNames))
      
      InputInfo <- extractInputTab(deets = MyInputDeets,
                                   sim_data_file = sim_data_file, 
                                   sheet = "Input Sheet", 
                                   CustomDosing = CustomDosing)
      
      CustomDosing <- InputInfo$CustomDosing
      Age_bins_redef_over_time <- InputInfo$Age_bins_redef_over_time
      
      Out <- c(Out, 
               InputInfo[setdiff(c(names(InputInfo), 
                                   "Age_bins_redef_over_time", 
                                   "CustomDosing"),
                                 names(Out))])
      
      if(Out[["SimulatorUsed"]] == "Simcyp Discovery"){
         # No DDIs with Discovery sims, and StartHr_sub not included b/c it's
         # always 0. Setting this.
         Out[["StartHr_sub"]] <- 0
         Out[["StartDayTime_sub"]] <- Out[["SimStartDayTime"]]
      }
      
   }
   
   # Dealing with custom dosing schedules ---------------------------------
   if(any(str_detect(exp_details, "^StartDayTime"), na.rm = TRUE) & 
      any(str_detect(names(Out), "^StartDayTime"), na.rm = TRUE) == FALSE |
      any(CustomDosing, na.rm = TRUE)){
      
      # When there's custom dosing for any of the substrate or inhibitors,
      # then the dosing start time should be pulled from a "Custom Dosing"
      # tab. Pulling any custom dosing sheets here.
      
      CustomDoseSheets <- SheetNames[str_detect(SheetNames, "Custom Dosing")]
      
      for(j in CustomDoseSheets){
         
         Suffix <- switch(str_extract(j, "Inh [12]|Sub"), 
                          "Inh 1" = "_inhib", 
                          "Inh 2" = "_inhib2", 
                          "Sub" = "_sub")
         
         CustomDose_xl <- suppressMessages(tryCatch(
            readxl::read_excel(path = sim_data_file, sheet = j,
                               col_names = FALSE),
            error = openxlsx::read.xlsx(sim_data_file, sheet = j,
                                        colNames = FALSE)))
         
         # If people have added anything to the sheet, that can mess up data
         # extraction. Here, we're at least checking for any columns that would
         # have NA values for the names b/c those are probably places where
         # people have added something extra off to the side of the data we
         # actually want.
         GoodCols <- t(CustomDose_xl[3, ]) %>% as.character()
         GoodCols <- which(complete.cases(GoodCols))
         
         CustomDosing_DF <- CustomDose_xl[4:nrow(CustomDose_xl), GoodCols]
         names(CustomDosing_DF) <- make.names(CustomDose_xl[3, GoodCols])
         CustomDosing_DF <- CustomDosing_DF %>% 
            rename(DoseNum = Dose.Number, 
                   Time1 = Time,
                   Dose_units = Dose.Units, 
                   DoseRoute = Route.of.Administration) %>% 
            mutate(Day = as.numeric(Day))
         
         TimeUnits <- names(CustomDosing_DF)[str_detect(names(CustomDosing_DF), "Offset")]
         names(CustomDosing_DF)[str_detect(names(CustomDosing_DF), "Offset")] <- "Time"
         
         MyCompoundID <- AllRegCompounds$CompoundID[AllRegCompounds$Suffix == Suffix]
         MyCompound <- as.character(Out[AllRegCompounds$DetailNames[AllRegCompounds$Suffix == Suffix]])
         
         CustomDosing_DF <- CustomDosing_DF %>% 
            # Removing any rows where Time is NA b/c those are likely places
            # where people have added some comments, etc. and not the main data
            # we want. The NA values mess up things downstream.
            filter(complete.cases(Time)) %>% 
            mutate(Time_units = ifelse(str_detect(TimeUnits, "\\.h\\.$"), 
                                       "h", "min"), 
                   File = sim_data_file, 
                   TimeOfDay = as.character(round_date(timeConv(
                      as.numeric(Time1)), unit = "minute")), 
                   TimeOfDay = sub("1899-12-30 ", "", TimeOfDay), 
                   CompoundID = MyCompoundID, 
                   Compound = MyCompound) %>% 
            mutate(across(.cols = matches("DoseNum|Time$|Dose$|^Day$"), 
                          .fns = as.numeric)) %>% 
            select(File, CompoundID, Compound, Day, TimeOfDay, 
                   Time, Time_units, DoseNum, 
                   Dose, Dose_units, DoseRoute)
         
         Out[[paste0("CustomDosing", Suffix)]] <- CustomDosing_DF
         Out[[paste0("Dose", Suffix)]] <- "custom dosing"
         Out[[paste0("StartDayTime", Suffix)]] <- "custom dosing"
         Out[[paste0("StartHr", Suffix)]] <- CustomDosing_DF$Time[CustomDosing_DF$DoseNum == 1]
         Out[[paste0("DoseRoute", Suffix)]] <- "custom dosing"
         Out[[paste0("DoseInt", Suffix)]] <- "custom dosing"
         Out[[paste0("Regimen", Suffix)]] <- "Multiple Dose"
         
         rm(CustomDosing_DF, Suffix, CustomDose_xl, MyCompoundID, MyCompound, TimeUnits)
         
      }
   }
   
   # Pulling details from the population tab -------------------------------
   MyPopDeets <- intersect(exp_details, PopDeets$Deet)
   
   # If user asks for population details, then function is set up to read both
   # what that population is and what the simulator version is by reading the
   # summary tab, so this next line should work and will not give them any
   # population details for Simcyp Discovery simulations until we set that up. 
   MyPopDeets <- intersect(MyPopDeets, 
                           (AllExpDetails %>% 
                               filter(SimulatorAvailability %in% DiscoveryCol) %>% 
                               pull(Detail)))
   
   if(length(MyPopDeets) > 0){
      # Getting name of that tab.
      if(exists("SheetNames", inherit = FALSE) == FALSE){
         SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                                error = openxlsx::getSheetNames(sim_data_file))
         
      }
      
      # Skipping this for now if it's multiple populations. 
      # FIXME: Set up something for dealing with multiple populations. 
      if(Out$Population != "Multiple populations"){
         
         # If user has requested that the population tab be annotated, which is an
         # option!, then there will be 2 matches to the population sheet name. We
         # want the 1st one.
         PopSheet <- SheetNames[str_detect(tolower(SheetNames),
                                           str_sub(tolower(Out$Population), 1, 20))][1]
         
         PopTab <- suppressMessages(tryCatch(
            readxl::read_excel(path = sim_data_file, sheet = PopSheet,
                               col_names = FALSE),
            error = openxlsx::read.xlsx(sim_data_file, sheet = PopSheet,
                                        colNames = FALSE)))
         # If openxlsx read the file, the names are different. Fixing.
         if(names(PopTab)[1] == "X1"){
            names(PopTab) <- paste0("...", 1:ncol(PopTab))
         }
         
         MyPopDeets <- intersect(exp_details, PopDeets$Deet)
         
         # User can change the name of user-defined cytosolic phenotypes for GI
         # tract, kidney, and liver. Changing this back to "Cyt1" to work for
         # regex, though. For now, only extracting data for Cyt1 and not any more
         # user-defined cytosolic phenotype parameters, so ignoring the others.
         # If name is changed in one, it's changed in all. Columns are 3, 5, and
         # 9.
         if(any(str_detect(PopTab$...3, "Cyt1"), na.rm = T) == FALSE){
            StartCytRow <- which(str_detect(PopTab$...3, "^User Cyt$"))[1]
            
            NewName <- gsub("Abundance : | Population Scalar", "", PopTab[StartCytRow + 1, 3])
            
            PopTab$...3 <- sub(NewName, "User Cyt1", PopTab$...3)
            PopTab$...5 <- sub(NewName, "User Cyt1", PopTab$...5)
            PopTab$...9 <- sub(NewName, "User Cyt1", PopTab$...9)
            
         }
         
         # sub function for finding correct cell
         pullValue <- function(deet){
            
            # Setting up regex to search
            ToDetect <- AllExpDetails %>% 
               filter(Detail == deet & DataSource == "population") %>% pull(Regex_row)
            NameCol <- PopDeets$NameCol[which(PopDeets$Deet == deet)]
            
            if(ncol(PopTab) < NameCol){
               # This happens when it's an animal simulation.
               return(NA)
            }
            Row <- which(str_detect(PopTab[, NameCol] %>% pull(), ToDetect))
            if(length(Row) == 0){
               Val <- NA
            } else {
               Val <- PopTab[Row, PopDeets$ValueCol[PopDeets$Deet == deet]] %>%
                  pull()
               Val <- sort(unique(Val))
            }
            
            suppressWarnings(
               Val <- switch(PopDeets$Class[PopDeets$Deet == deet], 
                             "character" = as.character(Val),
                             "numeric" = as.numeric(Val))
            )
            
            if(length(Val) == 0){Val <- NA}
            
            # Tidying up some specific idiosyncracies of simulator output
            Val <- ifelse(complete.cases(Val) & Val == "n/a",
                          NA, Val)
            
            Val <- case_when(deet == "GFR_pred_method" & Val == "Scripted" ~
                                "user defined", 
                             .default = as.character(Val))
            
            return(Val)
         }
         
         for(i in MyPopDeets){
            Out[[i]] <- pullValue(i)
         }
      }
   }
   
   
   # Pulling from workspace file -------------------------------------------
   if(any(c("workspace", "all") %in% exp_details_input)){
      
      # Checking that the workspace file is available. This will ignore the
      # date/time stamp on the Excel results if it's still there. 
      
      WkspFile <- c("Simulator" = sub("( - [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2})?\\.xlsx$",
                                      ".wksz", sim_data_file), 
                    "Discovery" = sub("( - [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2})?\\.xlsx$",
                                      ".dscw", sim_data_file))
      WkspFile <- WkspFile[which(file.exists(WkspFile))]
      
      if(length(WkspFile) > 0){
         
         WkSpDeets <- extractExpDetails_XML(
            sim_workspace_files = WkspFile, 
            compoundsToExtract = "all",
            exp_details = "all")
         
         WkSpDeets$MainDetails <- WkSpDeets$MainDetails %>% 
            # This currently removes anything that we already have from the
            # Excel file. May change that later to verify that Excel and
            # workspace match.
            select(!any_of(c("Substrate", "Inhibitor1", "Inhibitor2", 
                             "PrimaryMetabolite1", "PrimaryMetabolite2", 
                             "SecondaryMetabolite", "Inhibitor1Metabolite", 
                             paste0("DistributionModel",
                                    c("inhib1met", 
                                      "_met1", "_met2", "_secmet")))))
         
         Out <- c(Out,
                  WkSpDeets$MainDetails[
                     setdiff(names(WkSpDeets$MainDetails)[
                        names(WkSpDeets$MainDetails) != "Workspace"], 
                        names(Out))])
         
         if(is.na(Out$ObsOverlayFile) &
            "ObsOverlayFile" %in% names(WkSpDeets$MainDetails)){
            Out$ObsOverlayFile <- WkSpDeets$MainDetails$ObsOverlayFile
         }
         
         UserIntervals <- WkSpDeets$UserAUCIntervals
         
      } else {
         UserIntervals <- list()
      }
   } else {
      UserIntervals <- list()
   }
   
   if("Redefine_subjects_over_time_units" %in% names(Out) & 
      length(Age_bins_redef_over_time) > 0){
      Age_bins_redef_over_time$Time_units <- WkSpDeets$MainDetails$Redefine_subjects_over_time_units
   }
   
   
   # Calculated details & data cleanup ----------------------------------------
   
   # Other functions call on "Inhibitor1", etc., so we need those objects to
   # exist, even if they were not used in this simulation. Setting them to NA if
   # they don't exist.
   MissingCmpd <- setdiff(AllRegCompounds$DetailNames, names(Out))
   MissingCmpd_list <- as.list(rep(NA, length(MissingCmpd)))
   names(MissingCmpd_list) <- MissingCmpd
   Out <- c(Out, MissingCmpd_list)
   
   # There is a bug in at least V23 of the Simulator that can make the start
   # time of the substrate incorrect. If you had a workspace where you had a
   # perpetrator drug and the substrate did not start at t0 but then you remove
   # the perp and have just the substrate, the starting time from when there was
   # a perp present is sometimes still what is listed in the Summary and Input
   # tabs, even though that's NOT what is in the workspace and NOT what gets
   # simulated. Catching this and fixing it. The start time when it's a
   # substrate alone will ALWAYS be the simulation start time.
   if(is.na(Out$Inhibitor1) & 
      "StartDayTime_sub" %in% names(Out) &&
      Out$StartDayTime_sub != Out$SimStartDayTime){
      Out$StartDayTime_sub <- Out$SimStartDayTime
      Out$StartHr_sub <- 0
   }
   
   if("StartHr_sub" %in% exp_details && 
      "StartDayTime_sub" %in% names(Out) &&
      complete.cases(Out$StartDayTime_sub) && 
      Out$StartDayTime_sub != "custom dosing"){
      Out[["StartHr_sub"]] <- difftime_sim(time1 = Out$SimStartDayTime,
                                           time2 = Out$StartDayTime_sub)
   }
   
   if(all(c("Inhibitor1", "StartDayTime_inhib") %in% names(Out)) &&
      complete.cases(Out$StartDayTime_inhib) &&
      Out$StartDayTime_inhib != "custom dosing"){
      Out[["StartHr_inhib"]] <- difftime_sim(time1 = Out$SimStartDayTime,
                                             time2 = Out$StartDayTime_inhib)
   }
   
   if(all(c("Inhibitor2", "StartDayTime_inhib2") %in% names(Out)) && 
      complete.cases(Out$StartDayTime_inhib2) &&
      Out$StartDayTime_inhib != "custom dosing"){
      Out[["StartHr_inhib2"]] <- difftime_sim(time1 = Out$SimStartDayTime,
                                              time2 = Out$StartDayTime_inhib2)
   }
   
   # Always including the file name. 
   Out$File <- sim_data_file
   
   # Noting when workspace, if there is a matching one, was last changed.
   WorkspaceFile <- sub("xlsx", 
                        case_match(Out$SimulatorUsed, 
                                   "Discovery" ~ "dscw", 
                                   "Simcyp Simulator" ~ "wksz"), sim_data_file)
   # Removing the file timestamp if there was one b/c that won't be part of the
   # workspace file name.
   WorkspaceFile <- sub(" - [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2}", 
                        "", WorkspaceFile)
   
   Out$Workspace_date_last_saved <- 
      ifelse(file.exists(WorkspaceFile), 
             file.info(WorkspaceFile)$mtime %>% as.Date() %>% as.character(),
             NA)
   
   # Noting when this was run. 
   Out$expDetails_TimeStamp <- Sys.time()
   
   # Species should be lower case and not have "Sim" in front of it to work
   # more smoothly with other functions and also just look better. Setting
   # "beagle" to "dog" and setting it to "human" if it's missing, which it will
   # be for regular simulator output.
   if("Species" %in% names(Out)){
      Out$Species <- tolower(sub("Sim-", "", Out$Species))
      Out$Species <- ifelse(Out$Species == "beagle", "dog", Out$Species)
      
      if(is.na(Out$Species)){
         Out$Species <- "human"
      }
   }
   
   # Noting whether the simulation involved a large molecule
   if("ADCSimulation_sub" %in% names(Out) &&
      tolower(Out$ADCSimulation_sub) == "yes"){
      Out$LgMol_simulation <- TRUE
   } else if("MoleculeType_sub" %in% names(Out) && 
             tolower(Out$MoleculeType_sub) == "biologics integrated"){
      Out$LgMol_simulation <- TRUE
   } else {
      Out$LgMol_simulation <- FALSE
   }
   
   Out <- Out[sort(names(Out))]
   
   # Adding missing, necessary list items
   Missing1 <- setdiff(
      paste0(rep(c("DoseInt", "DoseRoute", "Regimen", "NumDoses"), each = 3), 
             c("_sub", "_inhib", "_inhib2")), 
      names(Out))
   
   if(length(Missing1) > 0){
      Missing <- as.list(matrix(data = NA, ncol = length(Missing1),
                                dimnames = list(NULL, Missing1)))
      names(Missing) <- Missing1
      
      Out <- c(Out, Missing)
   }
   
   # Fixing an issue that trips up other code down the line: Sometimes, the
   # user might specify a "multiple dose" regimen but then only administer
   # a single dose. That messes up, e.g., extractPK b/c it looks on the
   # wrong tab for the info it needs. When that happens, set the regimen to
   # "Single Dose".
   if(is.null(Out$Regimen_sub) == FALSE && 
      (complete.cases(Out$Regimen_sub) && Out$Regimen_sub == "Multiple Dose") &
      (complete.cases(Out$NumDoses_sub) && Out$NumDoses_sub == 1)){
      Out$Regimen_sub <- "Single Dose"
   }
   
   if(is.null(Out$Regimen_inhib) == FALSE && 
      (complete.cases(Out$Regimen_inhib) && Out$Regimen_inhib == "Multiple Dose") &
      (complete.cases(Out$NumDoses_inhib) && Out$NumDoses_inhib == 1)){
      Out$Regimen_inhib1 <- "Single Dose" 
   }
   
   if(is.null(Out$Regimen_inhib2) == FALSE && 
      (complete.cases(Out$Regimen_inhib2) && Out$Regimen_inhib2 == "Multiple Dose") &
      (complete.cases(Out$NumDoses_inhib2) && Out$NumDoses_inhib2 == 1)){
      Out$Regimen_inhib2 <- "Single Dose" 
   }
   
   # Simulator output makes NumDoses NA if single dose; setting to 1 in that
   # scenario
   Out$NumDoses_sub <- case_when(
      is.na(Out$NumDoses_sub) & tolower(Out$Regimen_sub) == "single dose" ~ 1, 
      .default = Out$NumDoses_sub)
   
   Out$NumDoses_inhib <- case_when(
      is.na(Out$NumDoses_inhib) & tolower(Out$Regimen_inhib) == "single dose" ~ 1, 
      .default = Out$NumDoses_inhib)
   
   Out$NumDoses_inhib2 <- case_when(
      is.na(Out$NumDoses_inhib2) & tolower(Out$Regimen_inhib2) == "single dose" ~ 1, 
      .default = Out$NumDoses_inhib2)
   
   # Making DoseInt_x and Dose_x numeric all the time. We'll get custom dosing
   # info from Regimen_x and DoseRoute_x.
   suppressWarnings(Out$DoseInt_sub <- as.numeric(Out$DoseInt_sub))
   suppressWarnings(Out$DoseInt_inhib <- as.numeric(Out$DoseInt_inhib))
   suppressWarnings(Out$DoseInt_inhib2 <- as.numeric(Out$DoseInt_inhib2))
   
   suppressWarnings(Out$Dose_sub <- as.numeric(Out$Dose_sub))
   suppressWarnings(Out$Dose_inhib <- as.numeric(Out$Dose_inhib))
   suppressWarnings(Out$Dose_inhib2 <- as.numeric(Out$Dose_inhib2))
   
   # At this point, DoseInt_x and Dose_x will be NA if it's a custom dosing
   # regimen. Setting the regimen to "multiple". We'll use that downstream for
   # checking for appropriate PK parameters, etc.
   
   Out$Regimen_sub <- ifelse(is.na(Out$DoseInt_sub) & 
                                (complete.cases(Out$DoseRoute_sub) && 
                                    Out$DoseRoute_sub == "custom dosing"), 
                             "Multiple Dose", Out$Regimen_sub)
   Out$Regimen_inhib <- ifelse(is.na(Out$DoseInt_inhib) & 
                                  (complete.cases(Out$DoseInt_inhib) && 
                                      Out$DoseRoute_inhib == "custom dosing"), 
                               "Multiple Dose", Out$Regimen_inhib)
   Out$Regimen_inhib2 <- ifelse(is.na(Out$DoseInt_inhib2) & 
                                   (complete.cases(Out$DoseInt_inhib2) && 
                                       Out$DoseRoute_inhib2 == "custom dosing"), 
                                "Multiple Dose", Out$Regimen_inhib2)
   
   # Splitting this up into main details -- a data.frame -- and then,
   # separately, whatever items need to be lists, e.g., custom dosing regimens
   # and dissolution profiles. 
   Main <- as_tibble(Out[which(sapply(Out, length) == 1)])
   # InputDF <- as_tibble(InputInfo[which(sapply(InputInfo, length) == 1)])
   # ColsToInclude <- setdiff(names(InputDF), names(Main))
   # Main <- cbind(Main, InputDF[, ColsToInclude])
   
   # Making absolutely sure that File included in Main. When we run
   # harmonize_details, it will add it to the other items whenever there is at
   # least 1 row, but we need it in Main to do that.
   Main$File <- sim_data_file 
   
   # Adding package version and R version
   Out$SimcypConsultancy_version <- packageVersion("SimcypConsultancy")
   Out$RVersion <- sessionInfo()[[1]]$version.string
   
   
   ## Dosing -----------------------------------------------------------------
   # Setting up Dosing data.frame to include ALL dosing info, so custom dosing
   # when appropriate and, for compounds and/or simulations w/out custom dosing,
   # then a data.frame of all dosing events filled in based on interval, amount,
   # etc.
   
   Out <- list(MainDetails = Main, 
               Age_bins_redef_over_time = Age_bins_redef_over_time, 
               CustomDosing = bind_rows(Out$CustomDosing_sub, 
                                        Out$CustomDosing_inhib, 
                                        Out$CustomDosing_inhib2), 
               
               DissolutionProfiles = 
                  switch(as.character("DissolutionProfiles") %in% 
                            names(InputInfo), 
                         "TRUE" = InputInfo$DissolutionProfiles,
                         "FALSE" = list()), 
               
               ReleaseProfiles = 
                  switch(as.character("ReleaseProfiles") %in% 
                            names(InputInfo), 
                         "TRUE" = InputInfo$ReleaseProfiles,
                         "FALSE" = list()), 
               
               ConcDependent_fup =
                  switch(as.character("ConcDependent_fup") %in% 
                            names(InputInfo), 
                         "TRUE" = InputInfo$ConcDependent_fup,
                         "FALSE" = list()), 
               
               ConcDependent_BP = 
                  switch(as.character("ConcDependent_BP") %in% 
                            names(InputInfo), 
                         "TRUE" = InputInfo$ConcDependent_BP,
                         "FALSE" = list()), 
               
               pH_dependent_solubility = 
                  switch(as.character("pH_dependent_solubility") %in% 
                            names(InputInfo), 
                         "TRUE" = InputInfo$pH_dependent_solubility,
                         "FALSE" = list()), 
               
               pH_dependent_LuminalDegradation = 
                  switch(as.character("pH_dependent_LuminalDegradation") %in% 
                            names(InputInfo), 
                         "TRUE" = InputInfo$pH_dependent_LuminalDegradation,
                         "FALSE" = list()), 
               
               UserAUCIntervals = UserIntervals)
   
   Out <- harmonize_details(Out)
   
   
   # Returning --------------------------------------------------------------
   
   return(Out)
   
}



