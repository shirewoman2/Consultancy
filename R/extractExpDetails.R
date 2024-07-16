#' Extract details about the experimental design
#'
#' \code{extractExpDetails} looks up experimental design details from a Simcyp
#' Simulator output file. For detailed instructions and examples, please see the
#' SharePoint file "Simcyp PBPKConsult R Files - Simcyp PBPKConsult R
#' Files/SimcypConsultancy function examples and instructions/Checking
#' simulation experimental
#' details/Checking-simulation-experimental-details.docx". (Sorry, we are unable
#' to include a link to it here.)
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
#' @param save_output optionally save the output by supplying a csv or Excel
#'   file name in quotes here, e.g., "Simulation details.csv" or "Simulation
#'   details.xlsx".  Do not include any slashes, dollar signs, or periods in the
#'   file name. If you leave off the file extension, it will be saved as a csv
#'   file.
#'
#' @return Returns a named list of the experimental details
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
                              save_output = NA){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   sim_data_file <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$", "", sim_data_file), ".xlsx")
   
   # Checking that the file is, indeed, a simulator output file.
   SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                          error = openxlsx::getSheetNames(sim_data_file))
   if(all(c("Input Sheet", "Summary") %in% SheetNames) == FALSE){
      # Using "warning" instead of "stop" here b/c I want this to be able to
      # pass through to extractExpDetails_mult and just skip any files that
      # aren't simulator output.
      warning(paste0("The file `", sim_data_file,
                     "` does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file.\n"), 
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
      warning("The only options for the argument `exp_details` are `Summary and Input`, `population tab`, `workspace`, `Simcyp inputs`, or `all` (not case sensitive), and you've supplied something else. We'll set this to `all` for now.\n", 
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
      filter(Sheet == "Summary" &
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
   InputDeets <- AllExpDetails %>% filter(Sheet == "Input Sheet") %>% 
      rename(Deet = Detail) %>% arrange(Deet) %>% 
      mutate(CompoundID = ifelse(is.na(CompoundID), "Trial Design", CompoundID))
   
   PopDeets <- AllExpDetails %>% filter(Sheet == "population") %>% 
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
               AllCompounds$DetailNames, 
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
   
   if(any(exp_details %in% AllExpDetails$Detail == FALSE)){
      Problem <- str_comma(unique(setdiff(exp_details,
                                          AllExpDetails$Detail)))
      warning(paste0("These study details are not among the possible options: ",
                     Problem,
                     ", so they will be omitted. Please enter 'view(ExpDetailDefinitions)' into the console for all options.\n"),
              call. = FALSE)
      exp_details <- intersect(exp_details, AllExpDetails$Detail)
   }
   
   if(length(exp_details) == 0){
      stop("You must enter at least one study detail to extract.",
           call. = FALSE)
   }
   
   Out <- list()
   
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
      
      # Need to filter to keep only details that we can possibly find based on
      # what type of simulator was used
      SumDeets <- SumDeets %>% filter(DiscoveryParameter %in% DiscoveryCol)
      MySumDeets <- MySumDeets[MySumDeets %in% SumDeets$Deet]
      
      # sub function for finding correct cell
      pullValue <- function(deet){
         
         # Setting up regex to search
         ToDetect <- SumDeets %>% 
            filter(Deet == deet) %>% pull(Regex_row)
         NameCol <- SumDeets$NameCol[which(SumDeets$Deet == deet)]
         Row <- which(str_detect(SummaryTab[, NameCol] %>% pull(), ToDetect))
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
            Val <- str_comma(Val)
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
      
      # Checking whether this was an ADC sim b/c have to do this differently. 
      MySumDeets <- setdiff(MySumDeets, "ADCSimulation_sub")
      Out[["ADCSimulation_sub"]] <- 
         any(str_detect(as.character(SummaryTab[, 1]), 
                        SumDeets %>% filter(Deet == "ADCSimulation_sub") %>% 
                           pull(Regex_row)), na.rm = T)
      
      for(i in MySumDeets){
         Out[[i]] <- pullValue(i)
         
         if(str_detect(i, "^StartDayTime") & is.na(Out[[i]])){
            CustomDosing <- c(CustomDosing, TRUE)
         }
         
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
      
      # Fixing an issue that trips up other code down the line: Sometimes, the
      # user might specify a "multiple dose" regimen but then only administer
      # a single dose. That messes up, e.g., extractPK b/c it looks on the
      # wrong tab for the info it needs. When that happens, set the regimen to
      # "Single Dose".
      if(is.null(Out$Regimen_sub) == FALSE && 
         (complete.cases(Out$Regimen_sub) &
          Out$Regimen_sub == "Multiple Dose" & Out$NumDoses_sub == 1)){
         Out$Regimen_sub <- "Single Dose"
      }
      
      if(is.null(Out$Regimen_inhib) == FALSE && 
         (complete.cases(Out$Inhibitor1) & 
          complete.cases(Out$Regimen_inhib)) && 
         (Out$Regimen_inhib == "Multiple Dose" & 
          Out$NumDoses_inhib == 1)){
         Out$Regimen_inhib <- "Single Dose" 
      }
      
      if(is.null(Out$Regimen_inhib2) == FALSE && 
         (complete.cases(Out$Inhibitor2) & 
          complete.cases(Out$Regimen_inhib2)) && 
         (Out$Regimen_inhib2 == "Multiple Dose" &
          Out$NumDoses_inhib2 == 1)){
         Out$Regimen_inhib2 <- "Single Dose" 
      }
   }
   
   # Pulling details from the Input Sheet tab ------------------------------
   MyInputDeets <- intersect(exp_details, InputDeets$Deet)
   # Not pulling the same info twice
   MyInputDeets <- setdiff(MyInputDeets, names(Out))
   
   if(length(MyInputDeets) > 0){
      
      InputTab <- suppressMessages(tryCatch(
         readxl::read_excel(path = sim_data_file, sheet = "Input Sheet",
                            col_names = FALSE),
         error = openxlsx::read.xlsx(sim_data_file, sheet = "Input Sheet",
                                     colNames = FALSE)))
      # If openxlsx read the file, the names are different. Fixing.
      if(names(InputTab)[1] == "X1"){
         names(InputTab) <- paste0("...", 1:ncol(InputTab))
      }
      
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
      InputDeets <- InputDeets %>% 
         filter(Deet %in% MyInputDeets & DiscoveryParameter %in% DiscoveryCol)
      MyInputDeets <- MyInputDeets[MyInputDeets %in% InputDeets$Deet]
      
      # Looking for locations of columns.
      ColLocations <- c("substrate" = 1,
                        "Trial Design" = which(t(InputTab[5, ]) == "Trial Design"),
                        "inhibitor 1" = which(t(InputTab[5, ]) == "Inhibitor 1"),
                        "inhibitor 2" = which(t(InputTab[5, ]) == "Inhibitor 2"),
                        "primary metabolite 1" = which(t(InputTab[5, ]) == "Sub Pri Metabolite1"),
                        "primary metabolite 2" = which(t(InputTab[5, ]) == "Sub Pri Metabolite2"),
                        "secondary metabolite" = which(t(InputTab[5, ]) == "Sub Sec Metabolite"),
                        "inhibitor 1 metabolite" = which(t(InputTab[5, ]) == "Inh 1 Metabolite"))
      
      InputDeets$NameCol <- ColLocations[InputDeets$CompoundID]
      InputDeets$ValueCol <- InputDeets$NameCol + 1
      
      ## Main set of parameters -----------------------------------------
      
      # Dealing w/potential replicate values. CompoundType and pKa may be
      # replicated but will have the same value, so when we take the unique
      # value, that will drop away.
      
      # Checking for any ADAMI parameters. (May need to adapt this later for
      # other variations on ADAM models or anything else where there will be
      # multiple cells with identical labels.)
      ADAMIrow <- which(InputTab$...1 == "ADAMI Parameters")
      
      if(length(ADAMIrow) == 0){
         InputDeets <- InputDeets %>% 
            filter(!str_detect(Deet, "ADAMI"))
         ADAMIreps <- NA
         NonADAMIreps <- NA
         MyInputDeets <- intersect(MyInputDeets, InputDeets$Deet)
      } else {
         ADAMIreps <- InputDeets %>% filter(str_detect(Deet, "ADAMI")) %>% 
            pull(Deet)
         NonADAMIreps <- sub("_ADAMI", "", ADAMIreps)
      }
      
      # sub function for finding correct cell
      pullValue <- function(deet){
         
         # Setting up regex to search
         ToDetect <- InputDeets %>% 
            filter(Deet == deet) %>% pull(Regex_row)
         NameCol <- InputDeets$NameCol[which(InputDeets$Deet == deet)]
         Row <- which(str_detect(InputTab[, NameCol] %>% pull(), ToDetect)) +
            (InputDeets %>% filter(Deet == deet) %>% pull(OffsetRows))
         
         if(length(Row) == 0){
            Val <- NA
         } else {
            if(deet %in% ADAMIreps){Row <- Row[Row > ADAMIrow]}
            if(deet %in% NonADAMIreps){Row <- Row[Row < ADAMIrow]}
            
            Val <- InputTab[Row,
                            InputDeets$ValueCol[
                               which(InputDeets$Deet == deet)]] %>% pull()
            
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
                                           InputDeets$NameCol[
                                              which(InputDeets$Deet == deet)]] %>% pull()
                  if(str_detect(tolower(NameColBelow),
                                tolower(gsub(paste0("kp_scalar_|",
                                                    str_c(AllCompounds$Suffix, collapse = "|")),
                                             "", 
                                             sub("additional_organ", "Additional Organ", deet))))){
                     Val <- InputTab[Row + 1, InputDeets$ValueCol[
                        which(InputDeets$Deet == deet)]] %>% pull()
                     
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
            InputDeets$Class[InputDeets$Deet == deet] <- "character"
            assign("InputDeets", InputDeets, envir = parent.frame())
         }
         
         suppressWarnings(
            Val <- switch(InputDeets$Class[InputDeets$Deet == deet], 
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
      # to do StartDayTime_x, SimulatorVersion, and ADCSimulation separately.
      MyInputDeets1 <-
         MyInputDeets[!str_detect(MyInputDeets, 
                                  "CLint_|Interaction_|^StartDayTime|Transport_|ADCSimulation|SimulatorVersion|OrganTissue")]
      
      if(length(MyInputDeets1) > 0){
         for(i in MyInputDeets1){
            Out[[i]] <- pullValue(i)
         }
      }
      
      
      ## Some overall simulation details -----------------------------------
      
      # Noting all sheet names. This saves time for later data extraction and
      # also helps with debugging and coding in general. 
      Out[["SheetNames"]] <- str_c(paste0("`", SheetNames, "`"), collapse = " ")
      
      # Checking whether this was an ADC sim. 
      if("ADCSimulation_sub" %in% MyInputDeets){
         Out[["ADCSimulation_sub"]] <- 
            any(str_detect(as.character(InputTab[, 1]), 
                           InputDeets %>% filter(Deet == "ADCSimulation_sub") %>%
                              pull(Regex_row)), na.rm = T)
      }
      
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
               
               rm(StartRow, EndRow, Release_temp)
               
            } else if(
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
      
      
      ### pH-dependent solubility -----------------------------------------
      if(Out[["SimulatorUsed"]] != "Simcyp Discovery" &&
         exists("InputTab", inherits = FALSE) &&
         any(str_detect(unlist(c(InputTab[, ColLocations + 1])), 
                        "Solubility-pH profile"),
             na.rm = TRUE)){
         
         pHSol <- list()
         
         for(i in names(ColLocations)[!names(ColLocations) == "Trial Design"]){
            StartRow <- which(str_detect(t(InputTab[, ColLocations[i] + 1]), 
                                         "Solubility-pH profile"))[1] + 1
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
            NameCol <- InputDeets$NameCol[InputDeets$Deet == j]
            ValueCol <- InputDeets$ValueCol[InputDeets$Deet == j]
            CLRows <- which(
               InputTab[ , NameCol] == "Enzyme" |
                  str_detect(InputTab[ , NameCol] %>%
                                pull(),
                             "^Biliary (CLint|Clearance)") |
                  str_detect(InputTab[ , NameCol] %>%
                                pull(),
                             "^Additional HLM CLint|^Additional Systemic Clearance") |
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
               IntRowStart <- which(str_detect(InputTab[, NameCol] %>%
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
                        Enzyme <- paste0(Enzyme, InputTab[i+1, NameCol + 1])
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
                                           "CLint|Vmax|t1/2|Ind max")
                     
                     if(CLType == "CLint"){
                        
                        Units <- str_extract(InputTab[CLrow, NameCol], 
                                             "\\(.*\\)")
                        Units <- gsub("\\(|\\)", "", Units)
                        Units <- gsub("/| ", "_", Units)
                        # Dealing with mu since it's causing some problems
                        # downstream when a symbol
                        Units <- gsub(rlang::chr_unserialise_unicode("<U+00B5>"), 
                                      "u", Units)
                        
                        suppressWarnings(
                           Out[[paste0(
                              paste("CLint", Enzyme,
                                    Pathway, Units, sep = "_"),
                              Suffix)]] <-
                              as.numeric(InputTab[CLrow, NameCol + 1])
                        )
                        
                        suppressWarnings(
                           Out[[paste0(
                              paste("fu_mic", Enzyme,
                                    Pathway, sep = "_"),
                              Suffix)]] <-
                              as.numeric(InputTab[CLrow+1, NameCol + 1])
                        )
                        
                        # Check for any UGT-specific CL parameters
                        if(str_detect(Enzyme, "UGT") & 
                           any(str_detect(t(InputTab[i:LastRow_i, NameCol]),
                                          "rUGT"))){
                           
                           rUGTSysInfo <- InputTab[i:LastRow_i, c(NameCol, ValueCol)] %>% 
                              rename(Name = 1, Value = 2) %>% 
                              filter(str_detect(Name, "rUGT"))
                           
                           Out[[paste0("CLint_", Enzyme, "_", Pathway, "_rUGTSystem",
                                       Suffix)]] <-
                              rUGTSysInfo[which(str_detect(rUGTSysInfo$Name, 
                                                           "rUGTSystem")), ] %>% 
                              pull(Value)
                           
                           suppressWarnings(
                              Out[[paste0("CLint_", Enzyme, "_", Pathway, "_rUGTScalar_liver",
                                          Suffix)]] <-
                                 rUGTSysInfo[which(
                                    str_detect(tolower(rUGTSysInfo$Name), 
                                               "rugtscalar.*liver")), ] %>% 
                                 pull(Value) %>% as.numeric())
                           
                           suppressWarnings(
                              Out[[paste0("CLint_", Enzyme, "_", Pathway, "_rUGTScalar_intestine",
                                          Suffix)]] <-
                                 rUGTSysInfo[which(
                                    str_detect(tolower(rUGTSysInfo$Name), 
                                               "rugtscalar.*intestine")), ] %>% 
                                 pull(Value) %>% as.numeric())
                           
                           suppressWarnings(
                              Out[[paste0("CLint_", Enzyme, "_", Pathway, "_rUGTScalar_kidney",
                                          Suffix)]] <-
                                 rUGTSysInfo[which(
                                    str_detect(tolower(rUGTSysInfo$Name), 
                                               "rugtscalar.*kidney")), ] %>% 
                                 pull(Value) %>% as.numeric())
                           
                           rm(rUGTSysInfo)
                        }
                        
                        rm(Enzyme, Pathway, CLType)
                        next
                        
                     }
                     
                     if(CLType == "Vmax"){
                        suppressWarnings(
                           Out[[paste0(
                              paste("Vmax", Enzyme,
                                    Pathway, sep = "_"),
                              Suffix)]] <-
                              as.numeric(InputTab[CLrow, NameCol + 1])
                        )
                        
                        suppressWarnings(
                           Out[[paste0(
                              paste("Km", Enzyme,
                                    Pathway, sep = "_"),
                              Suffix)]] <-
                              as.numeric(InputTab[CLrow+1, NameCol + 1])
                        )
                        
                        suppressWarnings(
                           Out[[paste0(
                              paste("fu_mic", Enzyme,
                                    Pathway, sep = "_"),
                              Suffix)]] <-
                              as.numeric(InputTab[CLrow+2, NameCol + 1])
                        )
                        
                        rm(Enzyme, Pathway, CLType)
                        next
                     }
                     
                     if(CLType == "t1/2"){
                        suppressWarnings(
                           Out[[paste0(
                              paste("HalfLife", Enzyme,
                                    Pathway, sep = "_"),
                              Suffix)]] <-
                              as.numeric(InputTab[CLrow, NameCol + 1])
                        )
                        
                        rm(Enzyme, Pathway, CLType)
                        next
                     }
                  } 
                  
                  # Biliary CL
                  if(str_detect(as.character(InputTab[i, NameCol]), "^Biliary (CLint|Clearance)")){
                     suppressWarnings(
                        Out[[paste0("CLint_biliary", Suffix)]] <-
                           as.numeric(InputTab[i, NameCol + 1])
                     )
                  }
                  
                  # Other HLM CL
                  if(str_detect(as.character(InputTab[i, NameCol]), "^Additional HLM CLint")){
                     suppressWarnings(
                        Out[[paste0("CLint_AddHLM", Suffix)]] <-
                           as.numeric(InputTab[i, NameCol + 1])
                     )
                  }
                  
                  # in vivo CL
                  if(str_detect(as.character(InputTab[i, ValueCol]),
                                "In Vivo Clearance")){
                     
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
            NameCol <- InputDeets$NameCol[InputDeets$Deet == j]
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
                  Enzyme <- gsub(" |\\(|\\)|-|/", "", InputTab[i, NameCol + 1])
                  NextEmptyCell <- which(is.na(InputTab[, NameCol + 1]))
                  NextEmptyCell <- NextEmptyCell[NextEmptyCell > i][1]
                  # If there's another interaction listed
                  # before the next empty cell, need to
                  # account for that.
                  NextInt <- IntRows[which(IntRows == i) + 1] - 1
                  NextInt <- ifelse(i == IntRows[length(IntRows)],
                                    nrow(InputTab), NextInt)
                  ThisIntRows <- i:(c(NextEmptyCell, NextInt)[which.min(c(NextEmptyCell, NextInt))])
                  
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
            
            NameCol <- InputDeets$NameCol[which(InputDeets$Deet == j)]
            Row_day <- which(str_detect(InputTab[, NameCol] %>% pull(), "Start Day"))
            # If this is not present, which sometimes happens with a custom
            # dosing schedule, then will need to pull info from custom
            # dosing sheet lower in script.
            if(length(Row_day) == 0){
               CustomDosing <- c(CustomDosing, TRUE)
            } else {
               Val_day <- InputTab[Row_day, InputDeets$ValueCol[
                  which(InputDeets$Deet == j)]] %>% pull()
               Row_time <- which(str_detect(InputTab[, NameCol] %>% pull(), "Start Time"))
               Val_time <- InputTab[Row_time, InputDeets$ValueCol[
                  which(InputDeets$Deet == j)]] %>% pull()
               
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
      MyInputDeets5 <- InputDeets %>% 
         filter(Deet %in% MyInputDeets5 & complete.cases(NameCol)) %>%
         pull(Deet)
      
      if(length(MyInputDeets5) > 0){
         
         for(j in MyInputDeets5){
            
            Suffix <- str_extract(j, "_sub$|_inhib$|_inhib2$|_met1$|_secmet$|_inhib1met$")
            NameCol <- InputDeets$NameCol[InputDeets$Deet == j]
            ValueCol <- InputDeets$ValueCol[InputDeets$Deet == j]
            
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
                     Transporter <- gsub(" |\\(|\\)|-|/", "", InputTab[i, NameCol + 1])
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
                        InputTab[c(i:TransRowLast)[which(str_detect(TransRowNames, "fuinc"))],
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
   }
   
   # Dealing with custom dosing schedules ---------------------------------
   if(any(str_detect(exp_details, "StartDayTime")) & 
      any(str_detect(names(Out), "StartDayTime")) == FALSE |
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
         
         Dose_xl <- suppressMessages(tryCatch(
            readxl::read_excel(path = sim_data_file, sheet = j,
                               col_names = FALSE),
            error = openxlsx::read.xlsx(sim_data_file, sheet = j,
                                        colNames = FALSE)))
         
         Dosing <- Dose_xl[4:nrow(Dose_xl), ]
         names(Dosing) <- make.names(Dose_xl[3,])
         Dosing <- Dosing %>% 
            rename(DoseNum = Dose.Number, 
                   Time1 = Time,
                   Dose_units = Dose.Units, 
                   DoseRoute = Route.of.Administration)
         
         TimeUnits <- names(Dosing)[str_detect(names(Dosing), "Offset")]
         names(Dosing)[str_detect(names(Dosing), "Offset")] <- "Time"
         
         MyCompoundID <- AllCompounds$CompoundID[AllCompounds$Suffix == Suffix]
         MyCompound <- as.character(Out[AllCompounds$DetailNames[AllCompounds$Suffix == Suffix]])
         
         Dosing <- Dosing %>% 
            mutate(Time_units = ifelse(str_detect(TimeUnits, "\\.h\\.$"), 
                                       "h", "min"), 
                   File = sim_data_file, 
                   TimeOfDay = as.character(round_date(timeConv(
                      as.numeric(Time1)), unit = "minute")), 
                   TimeOfDay = sub("1899-12-30 ", "", TimeOfDay), 
                   CompoundID = MyCompoundID, 
                   Compound = MyCompound) %>% 
            mutate(across(.cols = matches("DoseNum|Time$|Dose$"), 
                          .fns = as.numeric)) %>% 
            select(File, CompoundID, Compound, Day, TimeOfDay, 
                   Time, Time_units, DoseNum, 
                   Dose, Dose_units, DoseRoute)
         
         Out[[paste0("CustomDosing", Suffix)]] <- Dosing
         Out[[paste0("Dose", Suffix)]] <- "custom dosing"
         Out[[paste0("StartDayTime", Suffix)]] <- "custom dosing"
         Out[[paste0("StartHr", Suffix)]] <- Dosing$Time[Dosing$DoseNum == 1]
         Out[[paste0("DoseRoute", Suffix)]] <- "custom dosing"
         Out[[paste0("DoseInt", Suffix)]] <- "custom dosing"
         Out[[paste0("Regimen", Suffix)]] <- "custom dosing"
         
         rm(Dosing, Suffix, Dose_xl, MyCompoundID, MyCompound, TimeUnits)
         
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
                               filter(DiscoveryParameter %in% DiscoveryCol) %>% 
                               pull(Detail)))
   
   if(length(MyPopDeets) > 0){
      # Getting name of that tab.
      if(exists("SheetNames", inherit = FALSE) == FALSE){
         SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                                error = openxlsx::getSheetNames(sim_data_file))
         
      }
      
      # If user has requested that the population tab be annotated, which is an
      # option!, then there will be 2 matches to the population sheet name. We
      # want the 1st one.
      PopSheet <- SheetNames[str_detect(SheetNames, str_sub(Out$Population, 1, 20))][1]
      
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
            filter(Detail == deet & Sheet == "population") %>% pull(Regex_row)
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
         
         # Tidying up some specific idiosyncracies of simulator output
         Val <- ifelse(complete.cases(Val) & Val == "n/a",
                       NA, Val)
         
         return(Val)
      }
      
      for(i in MyPopDeets){
         Out[[i]] <- pullValue(i)
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
         
         TEMP <- extractExpDetails_XML(
            sim_workspace_files = WkspFile, 
            compoundsToExtract = "all",
            exp_details = "all")
         
         TEMP$MainDetails <- TEMP$MainDetails %>% 
            # This currently removes anything that we already have from the
            # Excel file. May change that later to verify that Excel and
            # workspace match.
            select(!any_of(c("Substrate", "Inhibitor1", "Inhibitor2", 
                             "PrimaryMetabolite1", "PrimaryMetabolite2", 
                             "SecondaryMetabolite", "Inhibitor1Metabolite", 
                             paste0("DistributionModel",
                                    c("inhib1met", 
                                      "_met1", "_met2", "_secmet")))))
         
         # Note: Currently, we are not extracting anything from the workspace
         # that would be its own separate list. When we DO do that, we'll need
         # to adjust this code to bind the MainDetails and whatever that list
         # is.
         Out <- c(Out,
                  TEMP$MainDetails[
                     setdiff(names(TEMP$MainDetails)[
                        names(TEMP$MainDetails) != "Workspace"], 
                        names(Out))])
         
         rm(TEMP)
      }
   }
   
   
   # Calculated details & data cleanup ----------------------------------------
   
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
   
   if("StartHr_inhib" %in% exp_details & 
      all(c("StartDayTime_inhib", "SimStartDayTime") %in% names(Out)) &&
      complete.cases(Out$StartDayTime_inhib) &&
      Out$StartDayTime_inhib != "custom dosing"){
      Out[["StartHr_inhib"]] <- difftime_sim(time1 = Out$SimStartDayTime,
                                             time2 = Out$StartDayTime_inhib)
   }
   
   if("StartHr_inhib2" %in% exp_details & 
      all(c("SimStartDayTime", "StartDayTime_inhib2") %in% names(Out)) &&
      complete.cases(Out$StartDayTime_inhib2) &&
      Out$StartDayTime_inhib2 != "custom dosing"){
      Out[["StartHr_inhib2"]] <- difftime_sim(time1 = Out$SimStartDayTime,
                                              time2 = Out$StartDayTime_inhib2)
   }
   
   # Removing StartDayTime_sub and SimStartDayTime if the user
   # did not request them.
   if("StartDayTime_sub" %in% exp_details_orig == FALSE){
      Out[["StartDayTime_sub"]] <- NULL
   }
   if("StartDayTime_inhib" %in% exp_details_orig == FALSE){
      Out[["StartDayTime_inhib"]] <- NULL
   }
   if("StartDayTime_inhib2" %in% exp_details_orig == FALSE){
      Out[["StartDayTime_inhib2"]] <- NULL
   }
   if("SimStartDayTime" %in% exp_details_orig == FALSE){
      Out[["SimStartDayTime"]] <- NULL
   }
   
   # Other functions call on "Inhibitor1", etc., so we need those objects to
   # exist. If user pulled data from Input Sheet, they might not, so setting
   # them to NA if they don't exist.
   if("Inhibitor1" %in% names(Out) == FALSE){
      Out$Inhibitor1 <- NA
   }
   if("Inhibitor2" %in% names(Out) == FALSE){
      Out$Inhibitor2 <- NA
   }
   if("PrimaryMetabolite1" %in% names(Out) == FALSE){
      Out$PrimaryMetabolite1 <- NA
   }
   if("PrimaryMetabolite2" %in% names(Out) == FALSE){
      Out$PrimaryMetabolite2 <- NA
   }
   if("SecondaryMetabolite" %in% names(Out) == FALSE){
      Out$SecondaryMetabolite <- NA
   }
   if("Inhibitor1Metabolite" %in% names(Out) == FALSE){
      Out$Inhibitor1Metabolite <- NA
   }
   
   # Always including the file name. It's just a good practice and it makes
   # extractExpDetails output more like extractExpDetails_mult.
   Out$File <- sim_data_file
   
   # Noting when workspace, if there is a matching one, was last changed.
   WorkspaceFile <- sub("xlsx", ifelse(Out$SimulatorUsed == "Discovery", 
                                       "dscw", "wksz"), sim_data_file)
   # Removing the file timestamp if there was one b/c that won't be part of the
   # workspace file name.
   WorkspaceFile <- sub(" - [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2}", 
                        "", WorkspaceFile)
   
   Out$Workspace_TimeLastModified <- 
      ifelse(file.exists(WorkspaceFile), 
             as.character(file.info(WorkspaceFile)$mtime), NA)
   
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
   
   Out <- Out[sort(names(Out))]
   
   # Fixing an issue that trips up other code down the line: Sometimes, the
   # user might specify a "multiple dose" regimen but then only administer
   # a single dose. That messes up, e.g., extractPK b/c it looks on the
   # wrong tab for the info it needs. When that happens, set the regimen to
   # "Single Dose".
   if(is.null(Out$Regimen_sub) == FALSE && 
      complete.cases(Out$Regimen_sub) &&
      Out$Regimen_sub == "Multiple Dose" && Out$NumDoses_sub == 1){
      Out$Regimen_sub <- "Single Dose"
   }
   
   if(is.null(Out$Regimen_inhib1) == FALSE && 
      (complete.cases(Out$Inhibitor1) & 
       complete.cases(Out$Regimen_inhib1) && 
       (Out$Regimen_inhib1 == "Multiple Dose" & Out$NumDoses_inhib1 == 1))){
      Out$Regimen_inhib1 <- "Single Dose" 
   }
   
   if(is.null(Out$Regimen_inhib2) == FALSE && 
      (complete.cases(Out$Inhibitor2) & 
       complete.cases(Out$Regimen_inhib2) && 
       (Out$Regimen_inhib2 == "Multiple Dose" & Out$NumDoses_inhib2 == 1))){
      Out$Regimen_inhib2 <- "Single Dose" 
   }
   
   # Splitting this up into main details -- a data.frame -- and then,
   # separately, whatever items need to be lists, e.g., custom dosing regimens
   # and dissolution profiles. 
   Main <- as.data.frame(Out[which(sapply(Out, length) == 1)])
   
   Out <- list(MainDetails = Main, 
               CustomDosing = bind_rows(Out$CustomDosing_sub, 
                                        Out$CustomDosing_inhib, 
                                        Out$CustomDosing_inhib2), 
               DissolutionProfiles = DissoProfs,
               ReleaseProfiles = ReleaseProfs, 
               ConcDependent_fup = CDfupProfs, 
               ConcDependent_BP = CDBPProfs, 
               pH_dependent_solubility = pHSol)
   
   for(j in names(Out)[unlist(lapply(Out, is.null)) == FALSE]){
      Out[[j]] <- Out[[j]] %>% 
         mutate(File = sim_data_file) %>% 
         select(File, everything())
   }
   
   if(complete.cases(save_output)){
      write.csv(Main, FileName, row.names = F)
   }
   
   return(Out)
   
}



