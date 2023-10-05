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
#'   \item{"Summary tab"}{Extract details available from the "Summary tab"
#'   (default)}
#'
#'   \item{"Input Sheet"}{Extract details available from the "Input Sheet" tab}
#'
#'   \item{"population tab"}{Extract details about the population used (data
#'   come from the tab with the same name as the population simulated)}
#'
#'   \item{"Simcyp inputs"}{Extract all the details that you normally fill out
#'   on the "Simcyp inputs (and QC)" tab of a compound data sheet plus trial
#'   design information}
#'
#'   \item{"workspace"}{Extract an extremely limited set of details directly
#'   from the Simcyp Simulator workspace files. The set of possible details may
#'   be viewed by entering \code{view(AllWorkspaceDetails)} in the console. This
#'   \emph{only} works if the workspace file name perfectly matches the Excel
#'   results file name and is located in the same folder. Otherwise, this step
#'   in the data extraction will be skipped. UNDER CONSTRUCTION.}
#'
#'   \item{"all"}{Extract all possible parameters}
#'
#'   \item{a string of the specific parameters you want, each in quotes and
#'   encapsulated with \code{c(...)}}{For a complete list of options:
#'   \code{view(ExpDetailDefinitions)} Parameters are reported with a suffix
#'   depending on which compound they pertain to: "_sub" for the substrate,
#'   "_met1" for the primary metabolite, "_met2" for the second primary
#'   metabolite, "_secmet" for the secondary metabolite, "_inhib" for the 1st
#'   inhibitor or inducer listed, "_inhib2" for the 2nd inhibitor or inducer
#'   listed, or "_inhib1met" for the inhibitor 1 metabolite. An example of
#'   acceptable input: \code{c("pKa1_sub", "fa_inhib2", "Regimen_sub")}}}
#'
#'   \strong{NOTES:} \enumerate{\item{The default pulls only parameters that are
#'   listed on the "Summary" tab. If you want experimental details on a second
#'   inhibitor or more information on metabolites, try pulling them from the
#'   "Input sheet" instead of the "Summary" tab, which doesn't have as much
#'   information on those compounds.} \item{There are a few places where
#'   requesting one item as input will get you multiple items as output:
#'   intrinsic clearance, interaction parameters, and transport parameters. For
#'   example, if you request intrinsic clearance values (ex: "CLint_sub"),
#'   you'll get all the intrinsic clearance values for that compound, and
#'   they'll be named according to which parameter it is, which enzyme it's for,
#'   etc. Same thing with requesting interaction parameters (ex:
#'   "Interaction_inhib" to get all the interaction parameters for inhibitor 1)
#'   and transporter parameters (ex: "Transport_sub").} \item{We have limited
#'   experience with extracting these data when a custom dosing regimen was
#'   used, so it would be a good idea to carefully check that the data are being
#'   pulled correctly in that scenario.}}
#' @param annotate_output TRUE or FALSE (default) on whether to transpose the
#'   rows and columns in the output, making the output table longer instead of
#'   wider, and adding columns to the output for a) which compound the
#'   information pertains to (substrate, inhibitor, etc.), b) which section of
#'   the Simcyp Simulator this detail is found in (physchem, absorption,
#'   distribution, etc.), c) notes describing what the detail is, and d) which
#'   sheet in the Excel file the information was pulled from. Setting this to
#'   "TRUE" runs the function \code{\link{annotateDetails}} behind the scenes.
#'   Please see \code{annotateDetails} for further ways to sift through and
#'   organize this output to find what you need.
#' @param save_output optionally save the output by supplying a csv or Excel
#'   file name in quotes here, e.g., "Simulation details.csv" or "Simulation
#'   details.xlsx".  Do not include any slashes, dollar signs, or periods in the file name. If you leave off the file extension, it will be saved as a
#'   csv file.
#'
#' @return Returns a named list of the experimental details
#' @import tidyverse
#' @import readxl
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
                              exp_details = "Summary tab", 
                              annotate_output = FALSE, 
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
   
   # Cleaning up possible problems w/how exp_details by tab might be inputted
   if(str_detect(tolower(exp_details[1]), "summary")){exp_details <- "Summary tab"}
   if(str_detect(tolower(exp_details[1]), "input") &
      !str_detect(tolower(exp_details[1]), "simcyp")){exp_details <- "Input sheet"}
   if(str_detect(tolower(exp_details[1]), "population")){exp_details <- "population tab"}
   
   # Noting exp_details requested for later
   exp_details_input <- tolower(exp_details)
   
   
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
   
   if(exp_details_input[1] == "all"){
      exp_details <- unique(AllExpDetails$Detail)
   }
   
   if(exp_details_input[1] == "summary tab"){
      exp_details <- c(SumDeets$Deet, paste0("StartHr", c("_sub", "_inhib")))
      # Note that StartHr_inhib2, even if there were an inhibitor 2, is
      # not available from the Summary Sheet. It IS available from the
      # Input Sheet, though.
   }
   
   if(exp_details_input[1] == "input sheet"){
      exp_details <- c(InputDeets$Deet, paste0("StartHr", 
                                               c("_sub", "_inhib", 
                                                 "_inhib2")))
   }
   
   if(exp_details_input[1] == "population tab"){
      exp_details <- PopDeets$Deet
   }
   
   if(tolower(exp_details_input[1]) == "simcyp inputs"){
      exp_details <- sort(unique(
         c("Substrate", "Inhibitor1", "Inhibitor2", "Metabolite1",
           "Metabolite2", "SecondaryMetabolite", "Inhibitor1Metabolite",
           AllExpDetails %>% 
              filter(complete.cases(CDSInputMatch)) %>% 
              pull(Detail))))
   }
   
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
   
   # Since StartHr values are calculated from StartDayTime, those values must be
   # included in exp_details to extract. If the user wanted "Input Sheet"
   # details, we can actually calculate this without reading the summary tab,
   # which takes more time, so omitting in that instance.
   if(length(setdiff(paste0("StartHr", c("_sub", "_inhib", "_inhib2")), 
                     exp_details)) > 0 &
      exp_details_input[1] != "input sheet"){
      exp_details <- unique(c(exp_details, 
                              paste0("StartDayTime", c("_sub", "_inhib", "_inhib2")),
                              "SimStartDayTime"))
   }
   
   if(any(exp_details %in% AllExpDetails$Detail == FALSE)){
      Problem <- str_comma(unique(setdiff(exp_details,
                                          AllExpDetails$Detail)))
      warning(paste0("These study details are not among the possible options: ",
                     Problem,
                     ", so they will be omitted. Please enter 'data(ExpDetailDefinitions)' into the console for all options.\n"),
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
   # If all of the details are from one of the other sheets, then don't
   # bother reading this sheet b/c that takes more processing time. (Some
   # details show up on multiple sheets, so there are redundancies in this
   # function to deal with that.)
   if(exp_details_input[1] %in% c("input sheet")){
      MySumDeets <- intersect("A", "B")
   }
   
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
                       gsub("Dose \\(|\\)|CMax \\(|TMax \\(|AUC \\(|CL \\(Dose/AUC\\)\\(|\\(blood\\)",
                            "", Val), Val)
         Val <- ifelse(deet %in% c("SimDuration"),
                       as.numeric(Val), Val)
         Val <- ifelse(deet == "SimulatorVersion",
                       str_extract(Val, "Version [12][0-9]"),
                       Val)
         
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
      
      # Simcyp Discovery only allows simulations with a substrate or metaboltie
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
   # If all of the details are from one of the other sheets, then don't
   # bother reading this sheet b/c that takes more processing time. (Some
   # details show up on multiple sheets, so there are redundancies in this
   # function to deal with that.)
   if(exp_details_input[1] %in% c("population tab", "summary tab")){
      MyInputDeets <- intersect("A", "B")
   }
   
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
         
         return(Val)
      }
      
      # pullValue doesn't work for CL, so those are separate. Also need
      # to do StartDayTime_x, SimulatorVersion, and ADCSimulation separately.
      MyInputDeets1 <-
         MyInputDeets[!str_detect(MyInputDeets, 
                                  "CLint_|Interaction_|^StartDayTime|Transport_|ADCSimulation|SimulatorVersion")]
      
      if(length(MyInputDeets1) > 0){
         for(i in MyInputDeets1){
            Out[[i]] <- pullValue(i)
         }
      }
      
      
      ## Some overall simulation details -----------------------------------
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
                                               "CL.*iv.*[(]mL")) + i - 1,
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
                                                  pull(), "Ind max|Ind slope|^Ki |^MBI"))[1] - 1
               
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
                                     tolower(as.character(InputTab[i-1, NameCol + 1])))
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
         
         
         ## Transport parameters -----------------------------------------------
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
                  OrganRows <- which(str_detect(InputTab[ , NameCol] %>% pull(),
                                                "^Organ/Tissue"))
                  TransRows <- which(str_detect(InputTab[ , NameCol] %>% pull(),
                                                "^Transporter"))
                  TransRows <- TransRows[TransRows > StartTrans]
                  
                  if(length(TransRows) > 0){
                     for(i in TransRows){
                        
                        # Last row always seems to contain RAF/REF or ISEF,T
                        TransRowLast <- which(str_detect(InputTab[ , NameCol] %>% pull(), 
                                                         "RAF/REF|ISEF"))
                        TransRowLast <- TransRowLast[which(TransRowLast > i)][1]
                        
                        Transporter <- gsub(" |\\(|\\)|-|/", "", InputTab[i, NameCol + 1])
                        
                        Organ <- OrganRows[which(OrganRows < TransRowLast)]
                        Organ <- Organ[length(Organ)]
                        Organ <- gsub(" |\\(|\\)|-|/", "", InputTab[Organ, NameCol + 1])
                        
                        TransRowNames <- InputTab[i:TransRowLast, NameCol] %>% pull(1)
                        
                        Location <- gsub(" |\\(|\\)|-|/", "", 
                                         InputTab[c(i:TransRowLast)[which(TransRowNames == "Location")],
                                                  ValueCol] %>% pull(1))
                        
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
                        
                        rm(TransRowLast, Transporter, Organ, TransRowNames, Location, 
                           ParamPrefix)
                     }
                  }
                  
                  rm(Suffix, NameCol, OrganRows, TransRows)
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
                      Dose = Dose, 
                      Dose_units = Dose.Units) 
            names(Dosing)[names(Dosing) == "Dose"] <-
               paste0("Dose", Suffix)
            names(Dosing)[names(Dosing) == "Route.of.Administration"] <-
               paste0("DoseRoute", Suffix)
            TimeUnits <- names(Dosing)[str_detect(names(Dosing), "Offset")]
            names(Dosing)[str_detect(names(Dosing), "Offset")] <- "Time"
            Dosing <- Dosing %>% 
               mutate(Time_units = ifelse(str_detect(TimeUnits, "\\.h\\.$"), 
                                          "h", "min")) %>% 
               mutate(across(.cols = matches("DoseNum|Time$|Dose_sub|Dose_inhib|Dose_inhib2"), 
                             .fns = as.numeric)) %>% 
               select(any_of(c("Time", "Time_units", "DoseNum", "Dose_sub", "Dose_inhib", 
                               "Dose_inhib2", "Dose_units", "DoseRoute_sub", 
                               "DoseRoute_inhib", "DoseRoute_inhib2")))
            
            Out[[paste0("CustomDosing", Suffix)]] <- Dosing
            Out[[paste0("Dose", Suffix)]] <- "custom dosing"
            Out[[paste0("StartDayTime", Suffix)]] <- "custom dosing"
            Out[[paste0("StartHr", Suffix)]] <- Dosing$Time[Dosing$DoseNum == 1]
            Out[[paste0("DoseRoute", Suffix)]] <- "custom dosing"
            Out[[paste0("DoseInt", Suffix)]] <- "custom dosing"
            Out[[paste0("Regimen", Suffix)]] <- "custom dosing"
            
            rm(Dosing, Suffix, Dose_xl)
            
         }
      }
   }
   
   # Pulling details from the population tab -------------------------------
   MyPopDeets <- intersect(exp_details, PopDeets$Deet)
   # If all of the details are from one of the other sheets, then don't
   # bother reading this sheet b/c that takes more processing time. (Some
   # details show up on multiple sheets, so there are redundancies in this
   # function to deal with that.)
   if(exp_details_input[1] %in% c("summary tab", "input sheet")){
      MyPopDeets <- intersect("A", "B")
   }
   
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
            exp_details = "all") %>% 
            # This currently removes anything that we already have from the
            # Excel file. May change that later to verify that Excel and
            # workspace match.
            select(!any_of(c("Substrate", "Inhibitor1", "Inhibitor2", 
                             "PrimaryMetabolite1", "PrimaryMetabolite2", 
                             "SecondaryMetabolite", "Inhibitor1Metabolite", 
                             paste0("DistributionModel",
                                    c("inhib1met", 
                                      "_met1", "_met2", "_secmet"))))) %>% 
            as.list()
         
         Out <- c(Out, TEMP[setdiff(names(TEMP)[names(TEMP) != "Workspace"], 
                                    names(Out))])
         
         rm(TEMP)
      }
   }
   
   
   # Calculated details & data cleanup ----------------------------------------
   if("StartHr_sub" %in% exp_details && 
      "StartDayTime_sub" %in% names(Out) &&
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
   
   if(annotate_output){
      Out <- annotateDetails(Out)
   } 
   
   if(complete.cases(save_output)){
      FileName <- save_output
      if(str_detect(FileName, "\\.")){
         # Making sure they've got a good extension
         Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
         FileName <- sub(paste0(".", Ext), "", FileName)
         Ext <- ifelse(Ext %in% c("csv", "xlsx"), 
                       Ext, "csv")
         FileName <- paste0(FileName, ".", Ext)
      } else {
         FileName <- paste0(FileName, ".csv")
         Ext <- "csv"
      }
      
      switch(Ext, 
             "csv" = write.csv(as.data.frame(Out), FileName, row.names = F), 
             "xlsx" = formatXL(
                as.data.frame(Out), 
                FileName, 
                sheet = "Simulation experimental details",
                styles = list(
                   list(columns = which(names(Out) == "Notes"), 
                        textposition = list(wrapping = TRUE)),
                   list(rows = 0, font = list(bold = TRUE),
                        textposition = list(alignment = "middle",
                                            wrapping = TRUE)), 
                   list(columns = which(str_detect(names(Out), "All files have this value")),
                        fill = "#E7F3FF"), 
                   list(rows = 0, columns = which(str_detect(names(Out), "All files have this value")), 
                        font = list(bold = TRUE), 
                        textposition = list(alignment = "middle",
                                            wrapping = TRUE), 
                        fill = "#E7F3FF"))))
   }
   
   return(Out)
}



