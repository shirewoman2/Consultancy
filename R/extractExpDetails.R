#' Extract details about the experimental design
#'
#' \code{extractExpDetails} looks up experimental design details from the
#' "Summary" or "Input Sheet" tabs of a Simcyp simulator output file.
#'
#'
#' @param sim_data_file name of the Excel file containing the simulator output
#' @param exp_details Experiment details you want to extract from the simulator
#'   output file "Summary" or "Input Sheet" tabs or the tab containing
#'   information about that population (the tab with the same name as the
#'   population simulated). Options are "Summary tab" to extract details only
#'   from the "Summary tab" (default), "Input Sheet" to extract details only
#'   from the "Input Sheet" tab, "all" to extract all possible parameters, or
#'   any combination of the following:
#'
#'   \describe{
#'
#'   \item{AbsorptionModel}{absorption model used, e.g., "1st order"}
#'
#'   \item{Age_min, Age_max}{Minimum or maximum age in simulated population}
#'
#'   \item{AGP_male or AGP_female}{AGP mean value for males or females. Values
#'   are pulled from the tab with information on the population simulated.
#'   Specifying "AGP" will return data for both sexes.}
#'
#'   \item{BPratio_sub or BPratio_inhib}{blood-to-plasma ratio}
#'
#'   \item{CLint}{intrinsic clearance, Vmax, Km, fu_mic, and/or half life values
#'   used for any CYPs, UGTs, or other enzymes listed. Output will be labeled
#'   for each enzyme and pathway as, e.g., "CLint_CYP3A4_1-OH" or
#'   "Vmax_UGT1A1_Pathway1". Specify "CLint" and all the other values (Vmax, Km,
#'   fu,mic, half life) will also be returned.}
#'
#'   \item{Dose_sub or Dose_inhib}{dose administered}
#'
#'   \item{DoseInt_sub or DoseInt_inhib}{dosing interval}
#'
#'   \item{DoseRoute_sub or DoseRoute_inhib}{dose route, e.g. oral}
#'
#'   \item{fa_input}{user input value for the fraction absorbed}
#'
#'   \item{fu_gut_input}{user input value for the fraction escaping gut
#'   metabolism}
#'
#'   \item{fu_sub or fu_inhib}{fraction unbound}
#'
#'   \item{Hematocrit or Haematocrit}{the hematocrit listed on the "Summary"
#'   tab}
#'
#'   \item{Hematocrit_male, Hematocrit_female, Haematocrit_male, or
#'   Haematocrit_female}{the hematocrit listed on the tab with population info.
#'   Note from LS: Can someone tell me how this differs from the "Haematocrit"
#'   value listed on the Summary tab?}
#'
#'   \item{HSA_male or HSA_female}{the HSA value for the sex specified --
#'   including C0, C1, and C2, where appropriate -- listed on the tab with
#'   population info. Specifying "HSA" will return all possible HSA details.}
#'
#'   \item{GIAbsModel_sub or GIAbsModel_inhib}{GI absorption model used}
#'
#'   \item{Inhibitor}{inhibitor used, if applicable}
#'
#'   \item{ka_input}{user input value for the absorption constant ka}
#'
#'   \item{kin_sac and kout_sac}{k in and k out for SAC (1/hr)}
#'
#'   \item{kp_scalar}{kp scalar}
#'
#'   \item{tlag_input}{user input value for the lag time}
#'
#'   \item{logP_sub or logP_inhib}{logP of substrate or inhibitor}
#'
#'   \item{ModelType_sub or ModelType_inhib}{the type of model, e.g., full PBPK
#'   model}
#'
#'   \item{MW_sub or MW_inhib}{molecular weight of substrate or inhibitor}
#'
#'   \item{NumDoses_sub or NumDoses_inhib}{number of doses}
#'
#'   \item{NumSubjTrial}{number of subjects per trial}
#'
#'   \item{NumTrials}{the number of trials performed}
#'
#'   \item{Ontogeny}{ontogeny profile used}
#'
#'   \item{Papp_MDCK}{Papp as determined in MDCKII cells (10^-6 cm/s)}
#'
#'   \item{Papp_calibrator}{Papp of the calibrator compound (10^-6 cm/s)}
#'
#'   \item{PercFemale}{Percent of females in simulated population}
#'
#'   \item{pKa1_sub, pKa2_sub, pKa1_inhib, or pKa2_inhib}{the pertinent pKa}
#'
#'   \item{Pop}{the population modeled}
#'
#'   \item{PopSize}{the size of population modeled}
#'
#'   \item{PrandialSt_sub or PrandialSt_inhib}{prandial state upon dosing}
#'
#'   \item{Qgut}{Qgut (L/hr)}
#'
#'   \item{Regimen_sub or Regimen_inhib}{dosing regimen}
#'
#'   \item{SimEndDayTime}{ending day and time of the simulation}
#'
#'   \item{SimStartDayTime}{starting day and time of the simulation}
#'
#'   \item{SimulatorVersion}{Simulator version number}
#'
#'   \item{StartDayTime_sub or StartDayTime_inhib}{Starting day and time for
#'   administering the substrate or inhbitor}
#'
#'   \item{StudyDuration}{study duration}
#'
#'   \item{Substrate}{the substrate used}
#'
#'   \item{Type_sub or Type_inhib}{type of compound, e.g., monoprotic base}
#'
#'   \item{Units_dose_sub, Units_dose_inhib, Units_AUC, Units_Cmax, Units_tmax,
#'   Units_CL}{Units for substrate dose, inhibitor dose, AUC, Cmax, tmax, or CL}
#'
#'   \item{UserAddnOrgan}{yes or no: Was a user-defined additional organ
#'   included?}
#'
#'   \item{Vsac}{V sac (L/kg)}
#'
#'   \item{Vss_input_sub or Vss_input_inhib}{Vss used}
#'
#'   \item{VssPredMeth_sub or VssPredMeth_inhib}{method used for predicting Vss}
#'
#'   } \emph{NOTE:} The default only pulls parameters that are listed on the
#'   "Summary" tab. (There are ~50 of them, so I'm not listing them here for
#'   brevity. -LS)
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
                              exp_details = "Summary tab"){

      # Noting which details are possible, which columns to search for their
      # names, which columns contain their values for substrates or
      # inhibitors, and what kind of data to format the output as at the end
      AllDeets <- data.frame(
            Deet = c("Substrate", "Inhibitor",
                     "MW_sub", "MW_inhib",
                     "logP_sub", "logP_inhib",
                     "Type_sub", "Type_inhib",
                     "pKa1_sub", "pKa1_inhib",
                     "pKa2_sub", "pKa2_inhib",
                     "BPratio_sub", "BPratio_inhib",
                     "Hematocrit", "Haematocrit",
                     "fu_sub", "fu_inhib",
                     "Pop", "PopSize", "NumTrials",
                     "NumSubjTrial", "SimStartDayTime",
                     "SimEndDayTime", "StudyDuration",
                     "Units_AUC", "Units_Cmax", "Units_tmax", "Units_CL",


                     "ModelType_sub", "ModelType_inhib",
                     "PrandialSt_sub", "PrandialSt_inhib",
                     "DoseRoute_sub", "DoseRoute_inhib",
                     "Units_dose_sub", "Units_dose_inhib",
                     "Dose_sub", "Dose_inhib",
                     "StartDayTime_sub", "StartDayTime_inhib",
                     "Regimen_sub", "Regimen_inhib",
                     "DoseInt_sub", "DoseInt_inhib",
                     "NumDoses_sub", "NumDoses_inhib",
                     "GIAbsModel_sub", "GIAbsModel_inhib",
                     "Vss_input_sub", "Vss_input_inhib",
                     "VssPredMeth_sub", "VssPredMeth_inhib",
                     "SimulatorVersion"),
            NameCol = c(rep(1, 29), rep(5, 24), 1),
            ValueCol = c(rep(2, 25), rep(1, 4), rep(6, 24), 1),
            Sheet = "Summary") %>%
            mutate(ValueCol = ifelse(str_detect(tolower(Deet), "inhib") &
                                           ValueCol == 2,
                                     3, ValueCol),
                   ValueCol = ifelse(str_detect(Deet, "inhib") &
                                           ValueCol == 6,
                                     7, ValueCol),
                   Class = c(rep("character", 2), rep("numeric", 4),
                             rep("character", 2), rep("numeric", 10),
                             "character", rep("numeric", 3), rep("character", 15),
                             rep("numeric", 2),
                             rep("character", 4), rep("numeric", 4),
                             rep("character", 2),
                             rep("numeric", 2),
                             rep("character", 3)))

      InputDeets <- data.frame(
            Deet = c("Abs_model", "fa_input", "ka_input", "tlag_input",
                     "fu_gut_input", "Papp_MDCK", "Papp_calibrator",
                     "UserAddnOrgan",
                     "CLint", "Interaction",
                     "Qgut", "kin_sac", "kout_sac", "Vsac", "kp_scalar",
                     "PercFemale", "Age_min", "Age_max",
                     "Ontogeny"),
            NameCol = 1,
            ValueCol = 2,
            Class = c("character", rep("numeric", 17), "character"),
            Sheet = "Input Sheet") %>%
            mutate(NameCol = ifelse(Deet %in% c("PercFemale", "Age_min", "Age_max"),
                                    4, NameCol),
                   ValueCol = ifelse(Deet %in% c("PercFemale", "Age_min", "Age_max"),
                                     5, ValueCol))

      PopDeets <- data.frame(
            Deet = c("AGP", "AGP_female", "AGP_male",
                     "Haematocrit", "Hematocrit",
                     "Haematocrit_female", "Haematocrit_male",
                     "Hematocrit_female", "Hematocrit_male",
                     "HSA", "HSA_female", "HSA_male",
                     "HSA_CO_female", "HSA_CO_male",
                     "HSA_C1_female", "HSA_C1_male",
                     "HSA_C2_female", "HSA_C2_male"),
            NameCol = 15, ValueCol = 16,
            Class = "numeric", Sheet = "population")

      AllDeets <- bind_rows(AllDeets, InputDeets, PopDeets)

      if(exp_details[1] == "all"){
            exp_details <- AllDeets$Deet
      }

      if(tolower(exp_details[1]) == "summary tab"){
            exp_details <- AllDeets$Deet[AllDeets$Sheet == "Summary"]
      }

      if(tolower(exp_details[1]) == "input sheet"){
            exp_details <- AllDeets$Deet[AllDeets$Sheet == "Input Sheet"]
      }

      if(any(exp_details %in% AllDeets$Deet == FALSE)){
            Problem <- str_comma(setdiff(exp_details, AllDeets$Deet))
            stop(paste0("These study details are not among the possible options: ",
                        Problem,
                        ". The study details to extract must be among the options listed. (Please see help file for all options.)"))
      }

      if(length(exp_details) == 0){
            stop("You must enter at least one study detail to extract.")
      }

      Out <- list()

      # Need to note original exp_details requested b/c I'm adding to it if
      # people request info from population tab
      exp_details_orig <- exp_details

      if(any(exp_details %in% AllDeets$Deet[AllDeets$Sheet == "population"])){
            exp_details <- c(exp_details, "Pop")
            exp_details <- unique(exp_details)
      }


      # Pulling details from the summary tab
      SumDeets <- intersect(exp_details,
                            AllDeets$Deet[AllDeets$Sheet == "Summary"])

      if(length(SumDeets) > 0){

            SummaryTab <- suppressMessages(
                  readxl::read_excel(path = sim_data_file, sheet = "Summary",
                                     col_names = FALSE))

            # # Check whether an effector is present b/c that moves things around
            # EffectorPresent <-
            #       complete.cases(SummaryTab[
            #             which(str_detect(SummaryTab$...1, "Inhibitor"))[1], 2])

            # sub function for finding correct cell
            pullValue <- function(deet){

                  # Setting up regex to search
                  ToDetect <- switch(deet,
                                     "Substrate" = "Compound Name",
                                     "Inhibitor" = "Compound Name",
                                     "MW_sub" = "Mol Weight",
                                     "MW_inhib" = "Mol Weight",
                                     "logP_sub" = "log P",
                                     "logP_inhib" = "log P",
                                     "Type_sub" = "Compound Type",
                                     "Type_inhib" = "Compound Type",
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
                                     "Pop" = "Population Name",
                                     "PopSize" = "Population Size",
                                     "NumTrials" = "Number of Trials",
                                     "NumSubjTrial" = "No. of Subjects per Trial",
                                     "SimStartDayTime" = "Start Day/Time",
                                     "SimEndDayTime" = "End Day/Time",
                                     "SimulatorVersion" = "Simcyp Version",
                                     "StudyDuration" = "Study Duration",
                                     "PrandialSt_sub" = "Prandial State",
                                     "PrandialSt_inhib" = "Prandial State",
                                     "DoseRoute_sub" = "Route",
                                     "DoseRoute_inhib" = "Route",
                                     "Units_dose_sub" = "Dose Units",
                                     "Units_dose_inhib" = "Dose Units",
                                     "Dose_sub" = "^Dose$",
                                     "Dose_inhib" = "^Dose$",
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
                  NameCol <- AllDeets$NameCol[which(AllDeets$Deet == deet)]
                  Row <- which(str_detect(SummaryTab[, NameCol] %>% pull(), ToDetect))
                  Val <- SummaryTab[Row, AllDeets$ValueCol[AllDeets$Deet == deet]] %>%
                        pull()

                  suppressWarnings(
                        Val <- ifelse(AllDeets$Class[AllDeets$Deet == deet] == "character",
                                      Val, as.numeric(Val))
                  )

                  # Tidying up some specific idiosyncracies of simulator output
                  Val <- ifelse(complete.cases(Val) & Val == "n/a", NA, Val)
                  # Val <- ifelse(str_detect(deet, "DoseUnit"),
                  #               gsub("Dose \\(|\\)", "", Val), Val)
                  Val <- ifelse(str_detect(deet, "^Unit"),
                                gsub("Dose \\(|\\)|CMax \\(|TMax \\(|AUC \\(|CL \\(Dose/AUC\\)\\(",
                                     "", Val),
                                Val)
                  Val <- ifelse(deet == "SimulatorVersion",
                                str_extract(Val, "Version [12][0-9]"),
                                Val)

                  return(Val)
            }

            for(i in SumDeets){
                  Out[[i]] <- pullValue(i)
            }

      }

      # Pulling details from the Input Sheet tab
      InputDeets <- intersect(exp_details,
                              AllDeets$Deet[AllDeets$Sheet == "Input Sheet"])

      if(length(InputDeets) > 0){

            InputTab <- suppressMessages(
                  readxl::read_excel(path = sim_data_file, sheet = "Input Sheet",
                                     col_names = FALSE))

            # Check whether an effector is present b/c that moves things around
            EffectorPresent <- any(str_detect(InputTab$...3, "Inhibitor"), na.rm = TRUE)
            # !!! May need to adjust this further when there are TWO inhibitors
            # present!!!

            if(EffectorPresent){
                  AllDeets$ValueCol[AllDeets$Sheet == "Input Sheet" &
                                          AllDeets$ValueCol == 5] <- 7
                  AllDeets$NameCol[AllDeets$Sheet == "Input Sheet" &
                                          AllDeets$NameCol == 4] <- 6
            }

            # sub function for finding correct cell
            pullValue <- function(deet){

                  # Setting up regex to search
                  ToDetect <- switch(deet,
                                     "Abs_model" = "Absorption Model",
                                     "Age_min" = "Minimum Age",
                                     "Age_max" = "Maximum Age",
                                     "fa_input" = "^fa$",
                                     "ka_input" = "^ka \\(",
                                     "kp_scalar" = "Kp Scalar",
                                     "tlag_input" = "lag time \\(",
                                     "fu_gut_input" = "fu\\(Gut\\)$",
                                     "Ontogeny" = "Ontogeny Profile",
                                     "Papp_MDCK" = "MDCK\\(10E-06 cm/s\\)",
                                     "Papp_calibrator" = "Reference Compound Value \\(10E-06 cm/s\\)",
                                     "PercFemale" = "Propn. of Females",
                                     "UserAddnOrgan" = "User-defined Additional",
                                     "SimulatorVersion" = "Version number",
                                     "Qgut" = "Q\\(Gut\\) \\(L/h",
                                     "kin_sac" = "SAC kin",
                                     "kout_sac" = "SAC kout",
                                     "Vsac" = "Volume .Vsac")
                  NameCol <- AllDeets$NameCol[which(AllDeets$Deet == deet)]
                  Row <- which(str_detect(InputTab[, NameCol] %>% pull(), ToDetect))
                  Val <- InputTab[Row, AllDeets$ValueCol[AllDeets$Deet == deet]] %>%
                        pull()

                  # Ontogeny profile is listed twice in output for some reason.
                  # Only keeping the 1st value. Really, keeping only the unique
                  # set of values for all deets. This will still throw an error
                  # if there is more than one value, but we'd want to know that
                  # anyway, so not just keeping the 1st value listed.
                  Val <- sort(unique(Val))

                  suppressWarnings(
                        Val <- ifelse(AllDeets$Class[AllDeets$Deet == deet] == "character",
                                      Val, as.numeric(Val))
                  )

                  # Tidying up some specific idiosyncracies of simulator output
                  Val <- ifelse(complete.cases(Val) & Val == "n/a", NA, Val)

                  return(Val)
            }

            # pullValue doesn't work for CL, so those are separate.
            InputDeets1 <- InputDeets[!InputDeets == "CLint"]

            if(length(InputDeets1) > 0){
                  for(i in InputDeets1){
                        Out[[i]] <- pullValue(i)
                  }
            }

            # Pulling CL info
            InputDeets2 <- InputDeets[InputDeets == "CLint"]

            if(length(InputDeets2) > 0){

                  CLRows <- which(InputTab$...1 == "Enzyme" |
                                        str_detect(InputTab$...1, "^Biliary CLint"))
                  CLRows <- CLRows[complete.cases(InputTab$...1[CLRows + 1])]

                  for(i in CLRows){
                        if(str_detect(InputTab$...1[i], "Enzyme")){

                              Enzyme <- gsub(" ", "", InputTab$...2[i])
                              Pathway <- gsub(" ", "", InputTab$...2[i - 1])

                              CLType <- str_extract(InputTab$...1[i+1],
                                                    "CLint|Vmax|t1/2")

                              if(CLType == "CLint"){
                                    suppressWarnings(
                                          Out[[paste("CLint", Enzyme, Pathway, sep = "_")]] <-
                                                as.numeric(InputTab$...2[i+1])
                                    )

                                    suppressWarnings(
                                          Out[[paste("fu_mic", Enzyme, Pathway, sep = "_")]] <-
                                                as.numeric(InputTab$...2[i+2])
                                    )


                                    rm(Enzyme, Pathway, CLType)
                                    next

                              }

                              if(CLType == "Vmax"){
                                    suppressWarnings(
                                          Out[[paste("Vmax", Enzyme, Pathway, sep = "_")]] <-
                                                as.numeric(InputTab$...2[i+1])
                                    )

                                    suppressWarnings(
                                          Out[[paste("Km", Enzyme, Pathway, sep = "_")]] <-
                                                as.numeric(InputTab$...2[i+2])
                                    )

                                    suppressWarnings(
                                          Out[[paste("fu_mic", Enzyme, Pathway, sep = "_")]] <-
                                                as.numeric(InputTab$...2[i+3])
                                    )

                                    rm(Enzyme, Pathway, CLType)
                                    next
                              }

                              if(CLType == "t1/2"){
                                    suppressWarnings(
                                          Out[[paste("HalfLife", Enzyme, Pathway, sep = "_")]] <-
                                                as.numeric(InputTab$...2[i+1])
                                    )

                                    rm(Enzyme, Pathway, CLType)
                                    next
                              }

                        } else {
                              # biliary CL
                              suppressWarnings(
                                    Out[["CLint_biliary"]] <-
                                          as.numeric(InputTab$...2[i])
                              )
                        }
                  }
            }
      }

      # Pulling details from the Pop Sheet tab
      PopDeets <- intersect(exp_details,
                            AllDeets$Deet[AllDeets$Sheet == "population"])

      if(length(PopDeets) > 0){
            # Getting name of that tab.
            SheetNames <- readxl::excel_sheets(sim_data_file)
            PopSheet <- SheetNames[str_detect(SheetNames, str_sub(Out$Pop, 1, 20))]

            PopTab <- suppressMessages(
                  readxl::read_excel(path = sim_data_file, sheet = PopSheet,
                                     col_names = FALSE))

            # Removing population from output if the user didn't specifically request
            # it.
            if("Pop" %in% exp_details_orig == FALSE){
                  Out$Pop <- NULL
            }

            if("HSA" %in% exp_details_orig){
                  exp_details <- unique(c(exp_details, "HSA_CO_female",
                                          "HSA_CO_male", "HSA_C1_female",
                                          "HSA_C1_male", "HSA_C2_female",
                                          "HSA_C2_male", "HSA_male", "HSA_female"))
                  exp_details <- exp_details[!exp_details == "HSA"]
            }

            if("HSA_male" %in% exp_details_orig){
                  exp_details <- unique(c(exp_details, "HSA_male", "HSA_CO_male",
                                          "HSA_C1_male", "HSA_C2_male"))
            }

            if("HSA_female" %in% exp_details_orig){
                  exp_details <- unique(c(exp_details, "HSA_female", "HSA_CO_female",
                                          "HSA_C1_female", "HSA_C2_female"))
            }

            if("AGP" %in% exp_details_orig){
                  exp_details <- unique(c(exp_details, "AGP_male", "AGP_female"))
                  exp_details <- exp_details[!exp_details == "AGP"]
            }

            PopDeets <- intersect(exp_details, AllDeets$Deet[AllDeets$Sheet == "population"])

            # sub function for finding correct cell
            pullValue <- function(deet){

                  # Setting up regex to search
                  ToDetect <- switch(deet,
                                     "HSA_female" = "HSA : Female",
                                     "HSA_male" = "HSA : Male",
                                     "HSA_CO_female" = "HSA C0 : Female",
                                     "HSA_CO_male" = "HSA C0 : Male",
                                     "HSA_C1_female" = "HSA C1 : Female",
                                     "HSA_C1_male" = "HSA C1 : Male",
                                     "HSA_C2_female" = "HSA C2 : Female",
                                     "HSA_C2_male" = "HSA C2 : Male",
                                     "AGP_male" = "AGP Mean : Male",
                                     "AGP_female" = "AGP Mean : Female",
                                     "Hematocrit_male" = "Haematocrit Mean : Male",
                                     "Hematocrit_female" = "Haematocrit Mean : Female",
                                     "Haematocrit_male" = "Haematocrit Mean : Male",
                                     "Haematocrit_female" = "Haematocrit Mean : Female")

                  NameCol <- AllDeets$NameCol[which(AllDeets$Deet == deet)]
                  Row <- which(str_detect(PopTab[, NameCol] %>% pull(), ToDetect))
                  Val <- PopTab[Row, AllDeets$ValueCol[AllDeets$Deet == deet]] %>%
                        pull()
                  Val <- sort(unique(Val))

                  suppressWarnings(
                        Val <- ifelse(AllDeets$Class[AllDeets$Deet == deet] == "character",
                                      Val, as.numeric(Val))
                  )

                  # Tidying up some specific idiosyncracies of simulator output
                  Val <- ifelse(complete.cases(Val) & Val == "n/a", NA, Val)

                  return(Val)
            }

            for(i in PopDeets){
                  Out[[i]] <- pullValue(i)
            }

      }

      return(Out)
}



