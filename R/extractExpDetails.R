#' Extract details about the experimental design
#'
#' \code{extractExpDetails} looks up experimental design details from a Simcyp
#' simulator output file.
#'
#' @param sim_data_file name of the Excel file containing the simulator output,
#'   in quotes
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
#'   on the "Simcyp inputs (and QC)" tab of a compound data sheet}
#'
#'   \item{"all"}{Extract all possible parameters}
#'
#'   \item{a string of the specific parameters you want, each in quotes and
#'   encapsulated with \code{c(...)},}{For a complete list, type
#'   \code{data(ExpDetailDefinitions); view(ExpDetailDefinitions)} into the
#'   console. Parameters are reported with a suffix depending on which compound
#'   they pertain to: "_sub" for the substrate, "_met1" for the primary
#'   metabolite, "_met2" for the second primary metabolite, "_secmet" for the
#'   secondary metabolite, "_inhib" for the 1st inhibitor or inducer listed,
#'   "_inhib2" for the 2nd inhibitor or inducer listed, or "_inh1met" for the
#'   inhibitor 1 metabolite. An example of acceptable input: \code{c("pKa1_sub",
#'   "fa_inhib2", "Regimen_sub")}}}
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
#' @param save_output optionally save the output by supplying a file name in
#'   quotes here, e.g., "My experimental details.csv". If you leave off ".csv",
#'   it will still be saved as a csv file.
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
                              save_output = NA){
    
    # Error catching ---------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    # If they didn't include ".xlsx" at the end, add that.
    sim_data_file <- ifelse(str_detect(sim_data_file, "xlsx$"), 
                            sim_data_file, paste0(sim_data_file, ".xlsx"))
    
    # Noting exp_details requested for later
    exp_details_input <- tolower(exp_details)
    
    # Cleaning up possible problems w/how exp_details by tab might be inputted
    if(str_detect(tolower(exp_details[1]), "summary")){exp_details <- "Summary tab"}
    if(str_detect(tolower(exp_details[1]), "input")){exp_details <- "Input sheet"}
    if(str_detect(tolower(exp_details[1]), "population")){exp_details <- "population tab"}
    
    # Main body of function ----------------------------------------------------
    
    # Noting which details are possible, which columns to search for their
    # names, which columns contain their values for substrates or inhibitors,
    # and what kind of data to format the output as at the end. Using data
    # object AllExpDetails.
    
    # Still need to add info for searching for some details. Removing those
    # from consideration for now.
    AllExpDetails <- AllExpDetails %>% filter(!str_detect(Sheet, "NEED TO ADD"))
    
    SumDeets <- AllExpDetails %>% filter(Sheet == "Summary") %>% 
        rename(Deet = Detail)
    PopDeets <- AllExpDetails %>% filter(Sheet == "population") %>% 
        rename(Deet = Detail)
    InputDeets <- AllExpDetails %>% filter(Sheet == "Input Sheet") %>% 
        rename(Deet = Detail)
    
    if(exp_details_input[1] == "all"){
        exp_details <- unique(AllExpDetails$Detail)
    }
    
    if(exp_details_input[1] == "summary tab"){
        exp_details <- c(SumDeets$Deet, "StartHr_sub", "StartHr_inhib")
        # Note that StartHr_inhib2, even if there were an inhibitor 2, is
        # not available from the Summary Sheet. It IS available from the
        # Input Sheet, though.
    }
    
    if(exp_details_input[1] == "input sheet"){
        exp_details <- c(InputDeets$Deet, "StartHr_sub", "StartHr_inhib",
                         "StartHr_inhib2")
    }
    
    if(exp_details_input[1] == "population tab"){
        exp_details <- PopDeets$Deet
    }
    
    if(exp_details_input[1] == "Simcyp inputs"){
        exp_details <- c("Substrate", "Inhibitor1",
                         paste0(rep(each = 2, 
                                    c("MW", "logP", "CompoundType", "pKa1", "pKa2",
                                      "BPratio", "fu", "Abs_model", 
                                      "Papp_Caco", "Papp_MDCK", "Papp_calibrator",
                                      "Qgut", "fu_gut", "ka", "fa", "tlag",
                                      "ModelType", "VssPredMeth", "Vss_input",
                                      "kp_scalar", "kin_sac", "kout_sac",
                                      "Vsac", "CLint", "CLrenal", "Interaction")), 
                                c("_sub", "_inhib")))
    }
    
    # Need to note original exp_details requested b/c I'm adding to it if
    # people request info from population tab. Note that this is different
    # from "exp_details_input" and serves a different purpose.
    exp_details_orig <- exp_details
    
    # Since StartHr_sub and StartHr_inhib are calculated from StartDayTime_sub
    # and StartDayTime_inhib, those must be included in exp_details to
    # extract. If the user wanted "Input Sheet" details, we can actually
    # calculate this without reading the summary tab, which takes more time,
    # so omitting in that instance.
    if("StartHr_sub" %in% exp_details & exp_details_input[1] != "input sheet"){
        exp_details <- unique(c(exp_details, "StartDayTime_sub",
                                "SimStartDayTime"))
    }
    if("StartHr_inhib" %in% exp_details & exp_details_input[1] != "input sheet"){
        exp_details <- unique(c(exp_details, "StartDayTime_inhib",
                                "SimStartDayTime"))
    }
    if("StartHr_inhib2" %in% exp_details & exp_details_input[1] != "input sheet"){
        exp_details <- unique(c(exp_details, "StartDayTime_inhib2",
                                "SimStartDayTime"))
    }
    
    if(any(exp_details %in% AllExpDetails$Detail == FALSE)){
        Problem <- str_comma(setdiff(exp_details,
                                     AllExpDetails$Detail))
        stop(paste0("These study details are not among the possible options: ",
                    Problem,
                    ". The study details to extract must be among the options listed. Please enter 'data(ExpDetailDefinitions)' into the console for all options."),
             call. = FALSE)
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
    MySumDeets <- intersect(exp_details, SumDeets$Deet)
    # If all of the details are from one of the other sheets, then don't
    # bother reading this sheet b/c that takes more processing time. (Some
    # details show up on multiple sheets, so there are redundancies in this
    # function to deal with that.)
    if(exp_details_input[1] %in% c("input sheet")){
        MySumDeets <- intersect("A", "B")
    }
    
    if(exp_details_input[1] %in% c("population tab")){
        MySumDeets <- "Population"
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
        
        # sub function for finding correct cell
        pullValue <- function(deet){
            
            # Setting up regex to search
            ToDetect <- switch(deet,
                               "Substrate" = "Compound Name",
                               "PrimaryMetabolite1" = "Sub Pri Metabolite1",
                               "PrimaryMetabolite2" = "Sub Pri Metabolite2",
                               "SecondaryMetabolite" = "Sub Sec Metabolite",
                               "Inhibitor1Metabolite" = "Inh 1 Metabolite",
                               "Inhibitor1" = "Compound Name",
                               "Inhibitor2" = "Inhibitor 2",
                               "MW_sub" = "Mol Weight",
                               "MW_inhib" = "Mol Weight",
                               "logP_sub" = "log P",
                               "logP_inhib" = "log P",
                               "CompoundType_sub" = "Compound Type",
                               "CompoundType_inhib" = "Compound Type",
                               "pKa1_sub" = "pKa 1",
                               "pKa1_inhib" = "pKa 1",
                               "pKa2_sub" = "pKa 2",
                               "pKa2_inhib" = "pKa 2",
                               "BPratio_sub" = "B/P",
                               "BPratio_inhib" = "B/P",
                               "Hematocrit" = "Haematocrit",
                               "Haematocrit" = "Haematocrit",
                               "fu_sub" = "^fu$",
                               "fu_inhib" = "^fu$",
                               "ModelType_sub" = "Distribution Model",
                               "ModelType_inhib" = "Distribution Model",
                               "Population" = "Population Name",
                               "PopSize" = "Population Size",
                               "NumTrials" = "Number of Trials",
                               "NumSubjTrial" = "No. of Subjects per Trial",
                               "SimStartDayTime" = "Start Day/Time",
                               "SimEndDayTime" = "End Day/Time",
                               "SimulatorVersion" = "Simcyp Version",
                               "SimDuration" = "Study Duration",
                               "PrandialSt_sub" = "Prandial State",
                               "PrandialSt_inhib" = "Prandial State",
                               "DoseRoute_sub" = "Route",
                               "DoseRoute_inhib" = "Route",
                               "Units_dose_sub" = "Dose Units",
                               "Units_dose_inhib" = "Dose Units",
                               "Dose_sub" = "^Dose$",
                               "Dose_inhib" = "^Dose$",
                               "PercDoseInhaled_sub" = "Amount of dose inhaled",
                               "PercDoseSwallowed_sub" = "Amount of dose swallowed",
                               "PercDoseInhaled_inhib" = "Amount of dose inhaled",
                               "PercDoseSwallowed_inhib" = "Amount of dose swallowed",
                               "StartDayTime_sub" = "Start Day/Time",
                               "StartDayTime_inhib" = "Start Day/Time",
                               "Regimen_sub" = "Dosing Regimen",
                               "Regimen_inhib" = "Dosing Regimen",
                               "DoseInt_sub" = "Dose Interval",
                               "DoseInt_inhib" = "Dose Interval",
                               "NumDoses_sub" = "Number of Doses",
                               "NumDoses_inhib" = "Number of Doses",
                               "GIAbsModel_sub" = "GI Absorption Model",
                               "GIAbsModel_inhib" = "GI Absorption Model",
                               "Units_AUC" = "^AUC \\(",
                               "Units_Cmax" = "^CMax \\(",
                               "Units_tmax" = "^TMax \\(",
                               "Units_CL" = "CL \\(Dose/AUC",
                               "Vss_input_sub" = "^Vss \\(L/kg\\)$",
                               "Vss_input_inhib" = "^Vss \\(L/kg\\)$",
                               "VssPredMeth_sub" = "Prediction Method",
                               "VssPredMeth_inhib" = "Prediction Method")
            NameCol <- SumDeets$NameCol[which(SumDeets$Deet == deet)]
            Row <- which(str_detect(SummaryTab[, NameCol] %>% pull(), ToDetect))
            Val <- SummaryTab[Row, SumDeets$ValueCol[SumDeets$Deet == deet]] %>%
                pull()
            
            suppressWarnings(
                Val <- ifelse(SumDeets$Class[SumDeets$Deet == deet] == "character",
                              Val, as.numeric(Val))
            )
            
            # Tidying up some specific idiosyncracies of simulator output
            Val <- ifelse(complete.cases(Val) & Val == "n/a", NA, Val)
            Val <- ifelse(str_detect(deet, "^Unit"),
                          gsub("Dose \\(|\\)|CMax \\(|TMax \\(|AUC \\(|CL \\(Dose/AUC\\)\\(",
                               "", Val), Val)
            Val <- ifelse(deet %in% c("SimDuration"),
                          as.numeric(Val), Val)
            Val <- ifelse(deet == "SimulatorVersion",
                          str_extract(Val, "Version [12][0-9]"),
                          Val)
            
            return(Val)
        }
        
        for(i in MySumDeets){
            Out[[i]] <- pullValue(i)
            if(str_detect(i, "^StartDayTime") & is.na(Out[[i]])){
                CustomDosing <- c(CustomDosing, TRUE)
            }
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
        
        # Looking for locations of columns.
        ColLocations <- c("Substrate" = 1,
                          "Trial Design" = which(t(InputTab[5, ]) == "Trial Design"),
                          "Inhibitor 1" = which(t(InputTab[5, ]) == "Inhibitor 1"),
                          "Inhibitor 2" = which(t(InputTab[5, ]) == "Inhibitor 2"),
                          "Sub Pri Metabolite1" = which(t(InputTab[5, ]) == "Sub Pri Metabolite1"),
                          "Sub Pri Metabolite2" = which(t(InputTab[5, ]) == "Sub Pri Metabolite2"),
                          "Sub Sec Metabolite" = which(t(InputTab[5, ]) == "Sub Sec Metabolite"),
                          "Inh 1 Metabolite" = which(t(InputTab[5, ]) == "Inh 1 Metabolite"))
        
        InputDeets$NameCol <- ColLocations[InputDeets$NameColDetect]
        InputDeets$ValueCol <- InputDeets$NameCol + 1
        
        # sub function for finding correct cell
        pullValue <- function(deet){
            
            # Setting up regex to search
            ToDetect <-
                switch(sub("_sub|_inhib|_met1|_met2|_secmet|_inhib1met|_inhib2",
                           "", deet),
                       "Abs_model" = "Absorption Model",
                       "Abs_scalars" = "Absorption Scalars .no units",
                       "Abs_scalar_colon" = "Absorption Scalar Colon",
                       "Abs_scalar_SI" = "Absorption Scalar SI Global",
                       "Age_min" = "Minimum Age",
                       "Age_max" = "Maximum Age",
                       "CLrenal" = "CL R \\(L/h",
                       "DLMPartHandModel" = "DLM Particle Handling Model",
                       "DoseInt" = "Dose Interval",
                       "fa" = "^fa$",
                       "ka" = "^ka \\(",
                       "kp_scalar" = "Kp Scalar",
                       "tlag" = "lag time \\(",
                       "fluid_intake" = "Fluid intake with dose",
                       "fu_gut" = "fu\\(Gut\\)$",
                       "Ontogeny" = "Ontogeny Profile",
                       "Papp_MDCK" = "MDCK\\(10E-06 cm/s\\)",
                       "Papp_Caco" = "PCaco-2",
                       "Papp_calibrator" = "Reference Compound Value \\(10E-06 cm/s\\)",
                       "Peff_cap" = "Peff.*man Cap \\(10",
                       "Peff" = "Peff.*man \\(10-4 cm/s",
                       "Peff_type" = "Peff.*[Tt]ype",
                       "PercFemale" = "Propn. of Females",
                       "UserAddnOrgan" = "User-defined Additional",
                       "SimulatorVersion" = "Version number",
                       "SimStartDayTime" = "Start Day/Time",
                       "Qgut" = "Q\\(Gut\\) \\(L/h",
                       "Regimen" = "Dosing Regimen",
                       "NumDoses" = "Number of Doses",
                       "kin_sac" = "SAC kin",
                       "kout_sac" = "SAC kout",
                       "Vsac" = "Volume .Vsac",
                       "Substrate" = "Compound Name",
                       "PrimaryMetabolite1" = "Compound Name",
                       "PrimaryMetabolite2" = "Compound Name",
                       "SecondaryMetabolite" = "Compound Name",
                       "Inhibitor1" = "Compound Name",
                       "Inhibitor2" = "Compound Name",
                       "Inhibitor1Metabolite" = "Compound Name", 
                       "BindingProtein" = "Reference Binding Component",
                       "Ptrans" = "Intrinsic Transcellular Permeability \\(",
                       "ParticleRadius" = "Mono[Dd]ispersed Radius \\(",
                       "ParticleDensity" = "Particle density \\(",
                       "IntrinsicSol" = "Intrinsic Solubility \\(",
                       "CritSupersatRatio" = "CSR value",
                       "PrecipRateConst" = "PRC \\(", 
                       "InputForm" = "Input Form",
                       "Formulation" = "Formulation",
                       "SegregatedTransTimeModel" = "Segregated transit time model",
                       "ParticleSizeDist" = "Particle Size Distribution",
                       "DispersionType" = "Dispersion Type"
                       
                )
            
            NameCol <- InputDeets$NameCol[which(InputDeets$Deet == deet)]
            Row <- which(str_detect(InputTab[, NameCol] %>% pull(), ToDetect))
            Val <- InputTab[Row,
                            InputDeets$ValueCol[
                                which(InputDeets$Deet == deet)]] %>% pull()
            
            # Ontogeny profile is listed twice in output for some reason.
            # Only keeping the 1st value. Really, keeping only the unique
            # set of values for all deets. This will still throw an error
            # if there is more than one value, but we'd want to know that
            # anyway, so not just keeping the 1st value listed.
            Val <- sort(unique(Val))
            
            suppressWarnings(
                Val <- ifelse(InputDeets$Class[InputDeets$Deet == deet] == "character",
                              Val, as.numeric(Val))
            )
            
            # Tidying up some specific idiosyncracies of simulator output
            Val <- ifelse(complete.cases(Val) & Val == "n/a", NA, Val)
            
            return(Val)
        }
        
        # pullValue doesn't work for CL, so those are separate. Also need
        # to do StartDayTime_x separately.
        MyInputDeets1 <-
            MyInputDeets[!str_detect(MyInputDeets, 
                                     "CLint_|Interaction_|^StartDayTime|Transport_")]
        
        if(length(MyInputDeets1) > 0){
            for(i in MyInputDeets1){
                Out[[i]] <- pullValue(i)
            }
        }
        
        # Pulling CL info
        MyInputDeets2 <- MyInputDeets[str_detect(MyInputDeets, "CLint_")]
        
        if(length(MyInputDeets2) > 0){
            
            for(j in MyInputDeets2){
                
                Suffix <- str_extract(j, "_sub$|_inhib$|_inhib2$|_met1$|_secmet$|_inh1met$")
                NameCol <- InputDeets$NameCol[InputDeets$Deet == j]
                ValueCol <- InputDeets$ValueCol[InputDeets$Deet == j]
                CLRows <- which(
                    InputTab[ , NameCol] == "Enzyme" |
                        str_detect(InputTab[ , NameCol] %>%
                                       pull(),
                                   "^Biliary CLint") |
                        str_detect(InputTab[ , NameCol] %>%
                                       pull(),
                                   "^Additional HLM CLint") |
                        str_detect(InputTab[ , ValueCol] %>%
                                       pull(),
                                   "In Vivo Clear"))
                CLRows <- CLRows[complete.cases(InputTab[CLRows + 1, NameCol])]
                
                # Checking for interaction data
                IntRowStart <- which(str_detect(InputTab[, NameCol] %>%
                                                    pull(), "Ind max|^Ki |^MBI|Interaction"))[1] - 1
                
                if(complete.cases(IntRowStart)){
                    CLRows <- CLRows[CLRows < min(IntRowStart)]
                }
                
                for(i in CLRows){
                    if(str_detect(as.character(InputTab[i, NameCol]), "Enzyme")){
                        
                        Enzyme <- gsub(" ", "", InputTab[i, NameCol + 1])
                        Pathway <- gsub(" |-", "", InputTab[i - 1, NameCol + 1])
                        if(as.character(InputTab[i+1, NameCol]) == "Genotype"){
                            Enzyme <- paste0(Enzyme, InputTab[i+1, NameCol + 1])
                            CLrow <- i + 2
                        } else if(str_detect(Enzyme, "User")){
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
                    
                    if(str_detect(as.character(InputTab[i, NameCol]), "^Biliary CLint")){
                        # biliary CL
                        suppressWarnings(
                            Out[[paste0("CLint_biliary", Suffix)]] <-
                                as.numeric(InputTab[i, NameCol + 1])
                        )
                    }
                    
                    if(str_detect(as.character(InputTab[i, NameCol]), "^Additional HLM CLint")){
                        # Other HLM CL
                        suppressWarnings(
                            Out[[paste0("CLint_AddHLM", Suffix)]] <-
                                as.numeric(InputTab[i, NameCol + 1])
                        )
                    }
                    
                    if(str_detect(as.character(InputTab[i, ValueCol]),
                                  "In Vivo Clear")){
                        suppressWarnings(
                            Out[[paste0("CLint_InVivo", Suffix)]] <- 
                                as.numeric(InputTab[i, NameCol + 1])
                        )
                    }
                }
                
                rm(CLRows, IntRowStart, NameCol, Suffix)
            }
        }
        
        # Pulling interaction info
        MyInputDeets3 <- MyInputDeets[str_detect(MyInputDeets, "Interaction_")]
        
        if(length(MyInputDeets3) > 0){
            
            for(j in MyInputDeets3){
                
                Suffix <- str_extract(j, "_sub$|_inhib$|_inhib2$|_met1$|_secmet$|_inh1met$")
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
                                                    pull(), "Ind max|^Ki |^MBI"))[1] - 1
                
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
                        ThisIntRows <- i:(c(NextEmptyCell, NextInt)[
                            which.min(c(NextEmptyCell, NextInt))])
                        
                        # induction
                        IndMax <- which(str_detect(InputTab[ThisIntRows, NameCol] %>% pull(),
                                                   "Ind max"))
                        if(length(IndMax) > 0){
                            suppressWarnings(
                                Out[[paste0(
                                    paste("IndMax", Enzyme,
                                          sep = "_"), Suffix)]] <-
                                    as.numeric(InputTab[ThisIntRows[IndMax], NameCol + 1])
                            )
                            
                            suppressWarnings(
                                Out[[paste0(
                                    paste("IndC50", Enzyme,
                                          sep = "_"), Suffix)]] <-
                                    as.numeric(InputTab[ThisIntRows[IndMax+3], NameCol + 1])
                            )
                            
                            suppressWarnings(
                                Out[[paste0(
                                    paste("Ind_fu_inc", Enzyme,
                                          sep = "_"), Suffix)]] <-
                                    as.numeric(InputTab[ThisIntRows[IndMax+5], NameCol + 1])
                            )
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
                           ThisIntRows, IndMax, Ki, MBI)
                    }
                }
                
                rm(Suffix, IntRows, IntRowStart, NameCol)
            }
        }
        
        # Dealing with StartDayTime_x
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
        
        
        # Transport parameters
        MyInputDeets5 <- MyInputDeets[str_detect(MyInputDeets, "Transport_")]
        MyInputDeets5 <- InputDeets %>% 
            filter(Deet %in% MyInputDeets5 & complete.cases(NameCol)) %>%
            pull(Deet)
        
        if(length(MyInputDeets5) > 0){
            
            for(j in MyInputDeets5){
                
                Suffix <- str_extract(j, "_sub$|_inhib$|_inhib2$|_met1$|_secmet$|_inh1met$")
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
                            
                            suppressWarnings(
                                Out[[paste0(ParamPrefix, "_CLintT", Suffix)]] <- 
                                    as.numeric(
                                        InputTab[c(i:TransRowLast)[which(str_detect(TransRowNames, "CLint,T"))],
                                                 ValueCol] %>% pull(1))
                            )
                            
                            suppressWarnings(
                                Out[[paste0(ParamPrefix, "_fuinc", Suffix)]] <- 
                                    as.numeric(
                                        InputTab[c(i:TransRowLast)[which(str_detect(TransRowNames, "fuinc"))],
                                                 ValueCol] %>% pull(1))
                            )
                            
                            suppressWarnings(
                                Out[[paste0(ParamPrefix, "_RAFREF", Suffix)]] <- 
                                    as.numeric(
                                        InputTab[c(i:TransRowLast)[which(str_detect(TransRowNames, "fuinc"))],
                                                 ValueCol] %>% pull(1))
                            )
                            
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
        
        SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                               error = openxlsx::getSheetNames(sim_data_file))
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
    
    # Pulling details from the population tab -------------------------------
    MyPopDeets <- intersect(exp_details, PopDeets$Deet)
    # If all of the details are from one of the other sheets, then don't
    # bother reading this sheet b/c that takes more processing time. (Some
    # details show up on multiple sheets, so there are redundancies in this
    # function to deal with that.)
    if(exp_details_input[1] %in% c("summary tab", "input sheet")){
        MyPopDeets <- intersect("A", "B")
    }
    
    if(length(MyPopDeets) > 0){
        # Getting name of that tab.
        if(exists("SheetNames", inherit = FALSE) == FALSE){
            SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                                   error = openxlsx::getSheetNames(sim_data_file))
            
        }
        
        PopSheet <- SheetNames[str_detect(SheetNames, str_sub(Out$Population, 1, 20))]
        
        PopTab <- suppressMessages(tryCatch(
            readxl::read_excel(path = sim_data_file, sheet = PopSheet,
                               col_names = FALSE),
            error = openxlsx::read.xlsx(sim_data_file, sheet = PopSheet,
                                        colNames = FALSE)))
        # If openxlsx read the file, the names are different. Fixing.
        if(names(PopTab)[1] == "X1"){
            names(PopTab) <- paste0("...", 1:ncol(PopTab))
        }
        
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
        }
        
        if("HSA_female" %in% exp_details_orig){
            exp_details <- unique(c(exp_details, "HSA_female", "HSA_C0_female",
                                    "HSA_C1_female", "HSA_C2_female"))
        }
        
        if("AGP" %in% exp_details_orig){
            exp_details <- unique(c(exp_details, "AGP_male", "AGP_female"))
            exp_details <- exp_details[!exp_details == "AGP"]
        }
        
        MyPopDeets <- intersect(exp_details, PopDeets$Deet)
        
        # sub function for finding correct cell
        pullValue <- function(deet){
            
            # Setting up regex to search
            ToDetect <- switch(deet,
                               "HSA_female" = "HSA : Female",
                               "HSA_male" = "HSA : Male",
                               "HSA_C0_female" = "HSA C0 : Female",
                               "HSA_C0_male" = "HSA C0 : Male",
                               "HSA_C1_female" = "HSA C1 : Female",
                               "HSA_C1_male" = "HSA C1 : Male",
                               "HSA_C2_female" = "HSA C2 : Female",
                               "HSA_C2_male" = "HSA C2 : Male",
                               "AGP_male" = "AGP Mean : Male",
                               "AGP_female" = "AGP Mean : Female",
                               "Hematocrit_male" = "Haematocrit Mean : Male",
                               "Hematocrit_female" = "Haematocrit Mean : Female",
                               "Haematocrit_male" = "Haematocrit Mean : Male",
                               "Haematocrit_female" = "Haematocrit Mean : Female", 
                               "Abund_CYP1A2_mean" = "Abundance : CYP1A2 EM Mean",
                               "Abund_CYP1A2_CV" = "Abundance : CYP1A2 EM CV",
                               "Abund_CYP2A6_mean" = "Abundance : CYP2A6 EM Mean",
                               "Abund_CYP2A6_CV" = "Abundance : CYP2A6 EM CV",
                               "Abund_CYP2B6_mean" = "Abundance : CYP2B6 EM Mean",
                               "Abund_CYP2B6_CV" = "Abundance : CYP2B6 EM CV",
                               "Abund_CYP2C9_mean" = "Abundance : CYP2C9 EM Mean",
                               "Abund_CYP2C9_CV" = "Abundance : CYP2C9 EM CV",
                               "Abund_CYP2C18_mean" = "Abundance : CYP2C18 EM Mean",
                               "Abund_CYP2C18_CV" = "Abundance : CYP2C18 EM CV",
                               "Abund_CYP2C19_mean" = "Abundance : CYP2C19 EM Mean",
                               "Abund_CYP2C19_CV" = "Abundance : CYP2C19 EM CV",
                               "Abund_CYP2D6_mean" = "Abundance : CYP2D6 EM Mean",
                               "Abund_CYP2D6_CV" = "Abundance : CYP2D6 EM CV",
                               "Abund_CYP2E1_mean" = "Abundance : CYP2E1 EM Mean",
                               "Abund_CYP2E1_CV" = "Abundance : CYP2E1 EM CV",
                               "Abund_CYP2J2_mean" = "Abundance : CYP2J2 EM Mean",
                               "Abund_CYP2J2_CV" = "Abundance : CYP2J2 EM CV",
                               "Abund_CYP3A4_mean" = "Abundance : CYP3A4 EM Mean",
                               "Abund_CYP3A4_CV" = "Abundance : CYP3A4 EM CV",
                               "Abund_CYP3A5_mean" = "Abundance : CYP3A5 EM Mean",
                               "Abund_CYP3A5_CV" = "Abundance : CYP3A5 EM CV",
                               "Abund_CYP3A7_mean" = "Abundance : CYP3A7 EM Mean",
                               "Abund_CYP3A7_CV" = "Abundance : CYP3A7 EM CV")
            
            NameCol <- PopDeets$NameCol[which(PopDeets$Deet == deet)]
            Row <- which(str_detect(PopTab[, NameCol] %>% pull(), ToDetect))
            Val <- PopTab[Row, PopDeets$ValueCol[PopDeets$Deet == deet]] %>%
                pull()
            Val <- sort(unique(Val))
            
            suppressWarnings(
                Val <- ifelse(PopDeets$Class[PopDeets$Deet == deet] == "character",
                              Val, as.numeric(Val))
            )
            
            # Tidying up some specific idiosyncracies of simulator output
            Val <- ifelse(complete.cases(Val) & Val == "n/a", NA, Val)
            
            return(Val)
        }
        
        for(i in MyPopDeets){
            Out[[i]] <- pullValue(i)
        }
    }
    
    # Calculated details ------------------------------------------------
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
    
    Out <- Out[sort(names(Out))]
    
    if(complete.cases(save_output)){
        if(str_detect(save_output, "\\.")){
            FileName <- sub("\\..*", ".csv", save_output)
        } else {
            FileName <- paste0(save_output, ".csv")
        }
        write.csv(as.data.frame(Out), FileName, row.names = F)
    }
    
    return(Out)
}



