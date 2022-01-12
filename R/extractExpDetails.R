#' Extract details about the experimental design
#'
#' \code{extractExpDetails} looks up experimental design details from the
#' "Summary" or "Input Sheet" tabs of a Simcyp simulator output file. This is
#' currently set up to extract details on the substrate, the first inhibitor,
#' primary metabolite 1, and/or secondary metabolite.
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
#'   \item{Abs_model_sub, Abs_model_inhib, Abs_model_met1,
#'   Abs_model_met2}{absorption model used, e.g., "1st order", for either the
#'   substrate, inhibitor, primary metabolite 1, or secondary metabolite}
#'
#'   \item{Age_min, Age_max}{Minimum or maximum age in simulated population}
#'
#'   \item{AGP_male or AGP_female}{AGP mean value for males or females. Values
#'   are pulled from the tab with information on the population simulated.
#'   Specifying "AGP" will return data for both sexes.}
#'
#'   \item{BPratio_sub, BPratio_inhib, BPratio_met1,
#'   BPratio_met2}{blood-to-plasma ratio}
#'
#'   \item{CLint_sub, CLint_inhib, CLint_met1, CLint_met2}{intrinsic clearance,
#'   Vmax, Km, fu_mic, and/or half life values used for any CYPs, UGTs, or other
#'   enzymes listed for the substrate, inhibitor, primary metabolite 1, or secondary metabolite.
#'   Output will be labeled for each enzyme and pathway as, e.g.,
#'   "CLint_sub_CYP3A4_1-OH" or "Vmax_sub_UGT1A1_Pathway1". Specify, e.g.,
#'   "CLint_sub" and all the other values (Vmax, Km, fu,mic, half life) will
#'   also be returned.}
#'
#'   \item{CLrenal_sub, CLrenal_inhib, CLrenal_met1, CLrenal_met2}{renal
#'   clearance (L/hr) for the substrate, inhibitor, primary metabolite 1, or metabolite
#'   2}
#'
#'   \item{Dose_sub or Dose_inhib}{dose administered}
#'
#'   \item{DoseInt_sub or DoseInt_inhib}{dosing interval}
#'
#'   \item{DoseRoute_sub or DoseRoute_inhib}{dose route, e.g. oral}
#'
#'   \item{fa_sub, fa_inhib, fa_met1, fa_met2}{user input value for the fraction
#'   absorbed for the substrate, inhibitor, primary metabolite 1, or secondary metabolite}
#'
#'   \item{fu_gut_sub, fu_gut_inhib, fu_gut_met1, fu_gut_met2}{user input value
#'   for the fraction escaping gut metabolism for the substrate, inhibitor,
#'   primary metabolite 1, or secondary metabolite}
#'
#'   \item{fu_sub, fu_inhib, fu_met1, fu_met2}{fraction unbound}
#'
#'   \item{GIAbsModel_sub, GIAbsModel_inhib, GIAbsModel_met1,
#'   GIAbsModel_met2}{GI absorption model used}
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
#'   \item{Inhibitor}{inhibitor used, if applicable}
#'
#'   \item{Interaction_sub, Interaction_inhib, Interaction_met1,
#'   Interaction_met2}{interaction parameters for any CYPs, UGTs, or other
#'   enzymes listed for the substrate, inhibitor, primary metabolite 1, or secondary metabolite.
#'   Output will be labeled for each enzyme and interaction type.}
#'
#'   \item{ka_sub, ka_inhib, ka_met1, ka_met2}{user input value for the
#'   absorption constant ka for the substrate, inhibitor, primary metabolite 1, or
#'   secondary metabolite}
#'
#'   \item{kin_sac_sub, kin_sac_inhib, kout_sac_sub, or kout_sac_inhib}{k in and
#'   k out for SAC (1/hr) for the substrate, inhibitor, primary metabolite 1, or
#'   secondary metabolite}
#'
#'   \item{kp_scalar_sub, kp_scalar_inhib, kp_scalar_met1, kp_scalar_met2}{kp
#'   scalar for the substrate, inhibitor, primary metabolite 1, or secondary metabolite}
#'
#'   \item{logP_sub, logP_inhib, logP_met1, logP_met2}{logP for the substrate,
#'   inhibitor, primary metabolite 1, or secondary metabolite}
#'
#'   \item{ModelType_sub, ModelType_inhib, ModelType_met1, ModelType_met2}{the
#'   type of model, e.g., full PBPK model}
#'
#'   \item{MW_sub, MW_inhib, MW_met1, MW_met2}{molecular weight}
#'
#'   \item{NumDoses_sub or NumDoses_inhib}{number of doses}
#'
#'   \item{NumSubjTrial}{number of subjects per trial}
#'
#'   \item{NumTrials}{the number of trials performed}
#'
#'   \item{Ontogeny}{ontogeny profile used}
#'
#'   \item{Papp_MDCK_sub, Papp_MDCK_inhib, Papp_MDCK_met1, Papp_MDCK_met2,
#'   Papp_Caco_sub, Papp_Caco_inhib, Papp_Caco_met1, Papp_Caco_met2}{Papp as
#'   determined in MDCKII or Caco-2 cells (10^-6 cm/s) for the substrate,
#'   inhibitor, primary metabolite 1, or secondary metabolite}
#'
#'   \item{Papp_calibrator_sub, Papp_calibrator_inhib, Papp_calibrator_met1,
#'   Papp_calibrator_met2}{Papp of the calibrator compound (10^-6 cm/s)}
#'
#'   \item{PercFemale}{Percent of females in simulated population}
#'
#'   \item{pKa1_sub, pKa2_sub, pKa1_inhib, pKa2_inhib, pKa1_met1, pKa1_met2,
#'   pKa2_met1, pKa2_met2}{the pertinent pKa}
#'
#'   \item{Pop}{the population modeled}
#'
#'   \item{PopSize}{the size of population modeled}
#'
#'   \item{PrandialSt_sub or PrandialSt_inhib}{prandial state upon dosing}
#'
#'   \item{Qgut_sub or Qgut_inhib}{Qgut_sub (L/hr) for the substrate or
#'   inhibitor}
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
#'   \item{StartHr_sub or StartHr_inhib}{Starting time in hours since the start
#'   of the simulation for administering the substrate or inhibitor.
#'   \strong{NOTE:} Unlike the other parameters listed here, this is
#'   \emph{calculated} from StartDayTime_sub or StartDayTime_inhib rather than
#'   being simply pulled from the simulator output file.}
#'
#'   \item{StudyDuration}{study duration in hours}
#'
#'   \item{Substrate}{the substrate used}
#'
#'   \item{tlag_sub or tlag_inhib}{user input value for the lag time for the
#'   substrate or inhibitor}
#'
#'   \item{Type_sub or Type_inhib}{type of compound, e.g., monoprotic base}
#'
#'   \item{Units_dose_sub, Units_dose_inhib, Units_AUC, Units_Cmax, Units_tmax,
#'   Units_CL}{Units for substrate dose, inhibitor dose, AUC, Cmax, tmax, or CL}
#'
#'   \item{UserAddnOrgan_sub or UserAddnOrgan_inhib}{yes or no: Was a
#'   user-defined additional organ included for the substrate, inhibitor,
#'   primary metabolite 1, or secondary metabolite?}
#'
#'   \item{Vsac_sub, Vsac_inhib, Vsac_met1, Vsac_met2}{V sac (L/kg) for the
#'   substrate, inhibitor, primary metabolite 1, or secondary metabolite}
#'
#'   \item{Vss_input_sub, Vss_input_inhib, Vss_input_met1, Vss_input_met2}{Vss
#'   used}
#'
#'   \item{VssPredMeth_sub, VssPredMeth_inhib, VssPredMeth_met1,
#'   VssPredMeth_met2}{method used for predicting Vss}
#'
#'   } \emph{NOTE:} The default pulls only parameters that are listed on the
#'   "Summary" tab. (There are ~50 of them, so I'm not listing them here for
#'   brevity. -LS) Also, if you want experimental details on a second inhibitor,
#'   try pulling them from the "Input sheet" instead of the "Summary" tab, which
#'   doesn't have much information on Inhibitor 2.
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
      SumDeets <- data.frame(
            Deet = c("SimulatorVersion", "Units_AUC", "Units_CL",
                     "Units_Cmax", "Units_tmax",

                     "Pop", "SimEndDayTime", "SimStartDayTime",
                     "StudyDuration", "Substrate", "Type_sub",

                     "Inhibitor", "Type_inhib", # NameCol 13

                     "DoseRoute_sub", "GIAbsModel_sub", "ModelType_sub",
                     "PrandialSt_sub", "Regimen_sub", "StartDayTime_sub",
                     "Units_dose_sub", "VssPredMeth_sub",

                     "DoseRoute_inhib", "GIAbsModel_inhib", "ModelType_inhib",
                     "PrandialSt_inhib", "Regimen_inhib", "StartDayTime_inhib",
                     "Units_dose_inhib", "VssPredMeth_inhib", # NameCol 16

                     "BPratio_sub", "fu_sub", "Haematocrit", "Hematocrit", "logP_sub",
                     "MW_sub", "NumSubjTrial", "NumTrials", "pKa1_sub", "pKa2_sub",
                     "PopSize",

                     "BPratio_inhib", "fu_inhib", "logP_inhib", "MW_inhib",
                     "pKa1_inhib", "pKa2_inhib", #NameCol 17

                     "Dose_sub", "DoseInt_sub", "NumDoses_sub", "Vss_input_sub",

                     "Dose_inhib", "DoseInt_inhib", "NumDoses_inhib", "Vss_input_inhib"),
            NameCol = c(rep(1, 13), rep(5, 16), rep(1, 17), rep(5, 8)),
            ValueCol = c(rep(1, 5), rep(2, 6), 3, 3, rep(6, 8), rep(7, 8),
                         rep(2, 11), rep(3, 6), rep(6, 4), rep(7, 4)),
            Sheet = "Summary",
            Class = c(rep("character", 29), rep("numeric", 25)))
      # !!!! There is almost no info on inhibitor 2 on the Summary tab!!!!

      InputDeets <- data.frame(
            Deet = c("Abs_model_sub", "fa_sub", "ka_sub", "tlag_sub",
                     "fu_gut_sub", "Papp_MDCK_sub", "Papp_calibrator_sub",
                     "Papp_Caco_sub", "UserAddnOrgan_sub", "CLrenal_sub",
                     "CLint_sub", "Interaction_sub", "Qgut_sub",
                     "kin_sac_sub", "kout_sac_sub", "Vsac_sub", "kp_scalar_sub",

                     # Trial Design details start here
                     "PercFemale", "Age_min", "Age_max",
                     "Ontogeny"),
            # NameCol and ValueCol change depending on whether there are
            # metabolites or inhibitors present, so noting what to detect using
            # RegEx in row 4 to determine position of those columns.
            NameColDetect = c(rep("Substrate", 17), rep("Trial Design", 4)),
            Class = c("character", rep("numeric", 19), "character"),
            Sheet = "Input Sheet")

      # Inhibitor 1 data
      InputDeets_inhib <- InputDeets %>% filter(str_detect(Deet, "_sub")) %>%
            mutate(Deet = sub("_sub", "_inhib", Deet),
                   NameColDetect = "Inhibitor 1")

      # Inhibitor 2 data
      InputDeets_inhib2 <- InputDeets_inhib %>%
            mutate(Deet = paste0(Deet, "2"),
                   NameColDetect = "Inhibitor 2")

      # Metabolite data
      InputDeets_met1 <- InputDeets_inhib %>%
            mutate(Deet = sub("_inhib", "_met1", Deet),
                   NameColDetect = "Sub Pri Metabolite1")
      InputDeets_met2 <- InputDeets_inhib %>%
            mutate(Deet = sub("_inhib", "_met2", Deet),
                   NameColDetect = "Sub Pri Metabolite2")

      InputDeets <- bind_rows(InputDeets, InputDeets_inhib, InputDeets_inhib2,
                              InputDeets_met1, InputDeets_met2) %>%
            bind_rows(data.frame(Deet = c("StartDayTime_sub",
                                          "StartDayTime_inhib",
                                          "StartDayTime_inhib2"),
                                 NameColDetect = c("Substrate",
                                                   "Inhibitor 1",
                                                   "Inhibitor 2"),
                                 Class = "numeric",
                                 Sheet = "Input Sheet"))

      PopDeets <- data.frame(
            Deet = c("AGP", "AGP_female", "AGP_male",
                     "Haematocrit_female", "Haematocrit_male",
                     "Hematocrit_female", "Hematocrit_male",
                     "HSA", "HSA_female", "HSA_male",
                     "HSA_C0_female", "HSA_C0_male",
                     "HSA_C1_female", "HSA_C1_male",
                     "HSA_C2_female", "HSA_C2_male"),
            NameCol = 15, ValueCol = 16,
            Class = "numeric", Sheet = "population")

      if(exp_details[1] == "all"){
            exp_details <- c(SumDeets$Deet, InputDeets$Deet, PopDeets$Deet,
                             "StartHr_sub", "StartHr_inhib", "StartHr_inhib2")
      }

      if(tolower(exp_details[1]) == "summary tab"){
            exp_details <- c(SumDeets$Deet, "StartHr_sub", "StartHr_inhib",
                             "StartHr_inhib2")
      }

      if(tolower(exp_details[1]) == "input sheet"){
            exp_details <- InputDeets$Deet
      }

      # Need to note original exp_details requested b/c I'm adding to it if
      # people request info from population tab
      exp_details_orig <- exp_details

      # Since StartHr_sub and StartHr_inhib are calculated from StartDayTime_sub
      # and StartDayTime_inhib, those must be included in exp_details to
      # extract.
      if("StartHr_sub" %in% exp_details){
            exp_details <- unique(c(exp_details, "StartDayTime_sub",
                                    "SimStartDayTime"))
      }
      if("StartHr_inhib" %in% exp_details){
            exp_details <- unique(c(exp_details, "StartDayTime_inhib",
                                    "SimStartDayTime"))
      }
      if("StartHr_inhib2" %in% exp_details){
            exp_details <- unique(c(exp_details, "StartDayTime_inhib2",
                                    "SimStartDayTime"))
      }

      if(any(exp_details %in% c(SumDeets$Deet, InputDeets$Deet,
                                PopDeets$Deet,
                                "StartHr_sub", "StartHr_inhib",
                                "StartHr_inhib2") == FALSE)){
            Problem <- str_comma(setdiff(exp_details,
                                         c(SumDeets$Deet, InputDeets$Deet, PopDeets$Deet)))
            stop(paste0("These study details are not among the possible options: ",
                        Problem,
                        ". The study details to extract must be among the options listed. (Please see help file for all options.)"))
      }

      if(length(exp_details) == 0){
            stop("You must enter at least one study detail to extract.")
      }

      Out <- list()

      if(any(exp_details %in% PopDeets$Deet)){
            exp_details <- c(exp_details, "Pop")
            exp_details <- unique(exp_details)
      }

      # Pulling details from the summary tab
      MySumDeets <- intersect(exp_details, SumDeets$Deet)

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
                  Val <- ifelse(deet %in% c("StudyDuration"),
                                as.numeric(Val), Val)
                  Val <- ifelse(deet == "SimulatorVersion",
                                str_extract(Val, "Version [12][0-9]"),
                                Val)

                  return(Val)
            }

            for(i in MySumDeets){
                  Out[[i]] <- pullValue(i)
            }

            # Dealing with the two calculated details
            if("StartHr_sub" %in% exp_details){
                  Out[["StartHr_sub"]] <- difftime_sim(time1 = Out$SimStartDayTime,
                                                       time2 = Out$StartDayTime_sub)
            }

            if("StartHr_inhib" %in% exp_details){
                  Out[["StartHr_inhib"]] <- difftime_sim(time1 = Out$SimStartDayTime,
                                                         time2 = Out$StartDayTime_inhib)
            }

            # Removing StartDayTime_sub and SimStartDayTime if the user
            # did not request them.
            if("StartDayTime_sub" %in% exp_details_orig == FALSE){
                  Out[["StartDayTime_sub"]] <- NULL
            }
            if("StartDayTime_inhib" %in% exp_details_orig == FALSE){
                  Out[["StartDayTime_inhib"]] <- NULL
            }
            if("SimStartDayTime" %in% exp_details_orig == FALSE){
                  Out[["SimStartDayTime"]] <- NULL
            }

      }

      # Pulling details from the Input Sheet tab
      MyInputDeets <- intersect(exp_details, InputDeets$Deet)

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

            # When effector 1 is not present, don't look for those values.
            if(any(str_detect(t(InputTab[5, ]), "Inhibitor 1"), na.rm = T) == FALSE){
                  MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_inhib$")]
            }

            # When effector 2 is not present, don't look for those values.
            if(any(str_detect(t(InputTab[5, ]), "Inhibitor 2"), na.rm = T) == FALSE){
                  MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_inhib$")]
            }

            # When primary metabolite 1 is not present, don't look for those values.
            if(any(str_detect(t(InputTab[5, ]), "Sub Pri Metabolite1"), na.rm = T) == FALSE){
                  MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_met1")]
            }

            # When secondary metabolite is not present, don't look for those values.
            if(any(str_detect(t(InputTab[5, ]), "Sub Pri Metabolite2"), na.rm = T) == FALSE){
                  MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_met2")]
            }

            # Looking for locations of columns.
            ColLocations <- c("Substrate" = 1,
                              "Trial Design" = which(t(InputTab[5, ]) == "Trial Design"),
                              "Inhibitor 1" = which(t(InputTab[5, ]) == "Inhibitor 1"),
                              "Inhibitor 2" = which(t(InputTab[5, ]) == "Inhibitor 2"),
                              "Sub Pri Metabolite1" = which(t(InputTab[5, ]) == "Sub Pri Metabolite1"),
                              "Sub Pri Metabolite2" = which(t(InputTab[5, ]) == "Sub Pri Metabolite2"))

            InputDeets$NameCol <- ColLocations[InputDeets$NameColDetect]
            InputDeets$ValueCol <- InputDeets$NameCol + 1

            # sub function for finding correct cell
            pullValue <- function(deet){

                  # Setting up regex to search
                  ToDetect <- switch(sub("_sub|_inhib$|_inhib2|_met1|_met2", "", deet),
                                     "Abs_model" = "Absorption Model",
                                     "Age_min" = "Minimum Age",
                                     "Age_max" = "Maximum Age",
                                     "CLrenal" = "CL R \\(L/h",
                                     "fa" = "^fa$",
                                     "ka" = "^ka \\(",
                                     "kp_scalar" = "Kp Scalar",
                                     "tlag" = "lag time \\(",
                                     "fu_gut" = "fu\\(Gut\\)$",
                                     "Ontogeny" = "Ontogeny Profile",
                                     "Papp_MDCK" = "MDCK\\(10E-06 cm/s\\)",
                                     "Papp_Caco" = "PCaco-2",
                                     "Papp_calibrator" = "Reference Compound Value \\(10E-06 cm/s\\)",
                                     "PercFemale" = "Propn. of Females",
                                     "UserAddnOrgan" = "User-defined Additional",
                                     "SimulatorVersion" = "Version number",
                                     "Qgut" = "Q\\(Gut\\) \\(L/h",
                                     "kin_sac" = "SAC kin",
                                     "kout_sac" = "SAC kout",
                                     "Vsac" = "Volume .Vsac")

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
            MyInputDeets1 <- MyInputDeets[!str_detect(MyInputDeets, "CLint_|Interaction_|StartDayTime")]

            if(length(MyInputDeets1) > 0){
                  for(i in MyInputDeets1){
                        Out[[i]] <- pullValue(i)
                  }
            }

            # Pulling CL info
            MyInputDeets2 <- MyInputDeets[str_detect(MyInputDeets, "CLint_")]

            if(length(MyInputDeets2) > 0){

                  for(j in MyInputDeets2){

                        Suffix <- str_extract(j, "_sub$|_inhib$|_inhib2$|_met1$|_met2$")
                        NameCol <- InputDeets$NameCol[InputDeets$Deet == j]
                        ValueCol <- InputDeets$ValueCol[InputDeets$Deet == j]
                        CLRows <- which(
                              InputTab[ , NameCol] == "Enzyme" |
                                    str_detect(InputTab[ , NameCol] %>%
                                                     pull(),
                                                         "^Biliary CLint") |
                                    str_detect(InputTab[ , ValueCol] %>%
                                                     pull(),
                                               "In Vivo Clear"))
                        CLRows <- CLRows[complete.cases(InputTab[CLRows + 1, NameCol])]

                        # Checking for interaction data
                        IntRowStart <- which(str_detect(InputTab[, NameCol] %>%
                                                              pull(), "Ind max|^Ki |^MBI"))[1] - 1

                        if(complete.cases(IntRowStart)){
                              CLRows <- CLRows[CLRows < min(IntRowStart)]
                        }

                        for(i in CLRows){
                              if(str_detect(as.character(InputTab[i, NameCol]), "Enzyme")){

                                    Enzyme <- gsub(" ", "", InputTab[i, NameCol + 1])
                                    Pathway <- gsub(" |-", "", InputTab[i - 1, NameCol + 1])
                                    if(InputTab[i+1, NameCol] == "Genotype"){
                                          Enzyme <- paste0(Enzyme, InputTab[i+1, NameCol + 1])
                                          CLrow <- i + 2
                                    } else {
                                          CLrow <- i + 1
                                    }

                                    CLType <- str_extract(InputTab[CLrow, NameCol],
                                                          "CLint|Vmax|t1/2|Ind max")

                                    if(CLType == "CLint"){
                                          suppressWarnings(
                                                Out[[paste0(
                                                      paste("CLint", Enzyme,
                                                           Pathway, sep = "_"),
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
                        }

                        if(str_detect(as.character(InputTab[i, NameCol]), "^Biliary CLint")){
                              # biliary CL
                              suppressWarnings(
                                    Out[[paste0("CLint_biliary", Suffix)]] <-
                                          as.numeric(InputTab[i, NameCol + 1])
                              )
                        }

                        if(str_detect(as.character(InputTab[i, ValueCol]),
                                      "In Vivo Clear")){
                              suppressWarnings(
                                    Out[[paste0("what should we call this?")]]
                                    # LEFT OFF HERE -- Need to look for CL (po), CL (iv), any active hepatic scalar, and CL R.
                              )
                        }


                        rm(CLRows, IntRowStart, NameCol, Suffix)
                  }
            }

            # Pulling interaction info
            MyInputDeets3 <- MyInputDeets[str_detect(MyInputDeets, "Interaction_")]

            if(length(MyInputDeets3) > 0){

                  for(j in MyInputDeets3){

                        Suffix <- str_extract(j, "_sub$|_inhib$|_met1$|_met2$")
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
      }

      # Pulling details from the Pop Sheet tab
      MyPopDeets <- intersect(exp_details, PopDeets$Deet)

      if(length(MyPopDeets) > 0){
            # Getting name of that tab.
            SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                                   error = openxlsx::getSheetNames(sim_data_file))
            PopSheet <- SheetNames[str_detect(SheetNames, str_sub(Out$Pop, 1, 20))]

            PopTab <- suppressMessages(tryCatch(
                  readxl::read_excel(path = sim_data_file, sheet = PopSheet,
                                     col_names = FALSE),
                  error = openxlsx::read.xlsx(sim_data_file, sheet = PopSheet,
                                              colNames = FALSE)))
            # If openxlsx read the file, the names are different. Fixing.
            if(names(PopTab)[1] == "X1"){
                  names(PopTab) <- paste0("...", 1:ncol(PopTab))
            }

            # Removing population from output if the user didn't specifically request
            # it.
            if("Pop" %in% exp_details_orig == FALSE){
                  Out$Pop <- NULL
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
                                     "Haematocrit_female" = "Haematocrit Mean : Female")

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

      Out <- Out[sort(names(Out))]

      return(Out)
}



