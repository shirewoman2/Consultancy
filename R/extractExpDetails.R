#' Extract details about the experimental design
#'
#' \code{extractExpDetails} looks up experimental design details from the
#' "Summary" or "Input Sheet" tabs of a Simcyp simulator output file.
#'
#'
#' @param sim_data_file name of the Excel file containing the simulator output
#' @param exp_details Experiment details you want to extract from the simulator
#'   output file, "Summary" tab. Options are any combination of the following:
#'
#'   \describe{
#'
#'   \item{AbsorptionModel}{absorption model used, e.g., "1st order"}
#'
#'   \item{BPratio_sub or BPratio_inhib}{blood-to-plasma ratio}
#'
#'   \item{Dose_sub or Dose_inhib}{dose administered}
#'
#'   \item{DoseInt_sub or DoseInt_inhib}{dosing interval}
#'
#'   \item{DoseRoute_sub or DoseRoute_inhib}{dose route, e.g. oral}
#'
#'   \item{DoseUnits_sub or DoseUnits_inhib}{dose units}
#'
#'   \item{fa_input}{user input value for the fraction absorbed}
#'
#'   \item{fu_gut_input}{user input value for the fraction escaping gut
#'   metabolism}
#'
#'   \item{fu_sub or fu_inhib}{fraction unbound}
#'
#'   \item{Hematocrit or Heamatocrit}{the hematocrit}
#'
#'   \item{GIAbsModel_sub or GIAbsModel_inhib}{GI absorption model used}
#'
#'   \item{Inhibitor}{inhibitor used, if applicable}
#'
#'   \item{ka_input}{user input value for the absorption constant ka}
#'
#'   \item{lag_input}{user input value for the lag time}
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
#'   \item{Peff}{Peff,man Cap(10^-4 cm/s)}
#'
#'   \item{pKa1_sub, pKa2_sub, pKa1_inhib, or pKa2_inhib}{the pertinent pKa}
#'
#'   \item{Pop}{the population modeled}
#'
#'   \item{PopSize}{the size of population modeled}
#'
#'   \item{PrandialSt_sub or PrandialSt_inhib}{prandial state upon dosing}
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
#'   \item{UserAddnOrgan}{yes or no: Was a user-defined additional organ
#'   included?}
#'
#'   \item{VssInput_sub or VssInput_inhib}{Vss used}
#'
#'   \item{VssPredMeth_sub or VssPredMeth_inhib}{method used for predicting Vss}
#'
#'   }
#'
#' @return Returns a named list of the experimental details
#'
#' @export
#'
#' @examples
#'
#' extractExpDetails(sim_data_file = "../Example simulator output.xlsx")
#' extractExpDetails(sim_data_file = "../Example simulator output MD + inhibitor.xlsx")
#'
#'
#'
extractExpDetails <- function(sim_data_file,
                              exp_details = c(
                                    "Abs_model",
                                    "BPratio_sub", "BPratio_inhib",
                                    "Dose_sub", "Dose_inhib",
                                    "DoseInt_sub", "DoseInt_inhib",
                                    "DoseRoute_sub", "DoseRoute_inhib",
                                    "DoseUnits_sub", "DoseUnits_inhib",
                                    "fa_input",
                                    "fu_gut_input",
                                    "fu_sub", "fu_inhib",
                                    "GIAbsModel_sub", "GIAbsModel_inhib",
                                    "Hematocrit", "Haematocrit",
                                    "Inhibitor",
                                    "ka_input", "lag_input",
                                    "logP_sub", "logP_inhib",
                                    "ModelType_sub", "ModelType_inhib",
                                    "MW_sub", "MW_inhib",
                                    "NumDoses_sub", "NumDoses_inhib",
                                    "NumTrials",
                                    "NumSubjTrial",
                                    "Peff",
                                    "pKa1_sub", "pKa1_inhib",
                                    "pKa2_sub", "pKa2_inhib",
                                    "Pop",
                                    "PopSize",
                                    "PrandialSt_sub", "PrandialSt_inhib",
                                    "Regimen_sub", "Regimen_inhib",
                                    "SimEndDayTime",
                                    "SimStartDayTime",
                                    "SimulatorVersion",
                                    "StartDayTime_sub", "StartDayTime_inhib",
                                    "StudyDuration",
                                    "Substrate",
                                    "Type_sub", "Type_inhib",
                                    "UserAddnOrgan",
                                    "VssInput_sub", "VssInput_inhib",
                                    "VssPredMeth_sub", "VssPredMeth_inhib")){


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

                     "ModelType_sub", "ModelType_inhib",
                     "PrandialSt_sub", "PrandialSt_inhib",
                     "DoseRoute_sub", "DoseRoute_inhib",
                     "DoseUnits_sub", "DoseUnits_inhib",
                     "Dose_sub", "Dose_inhib",
                     "StartDayTime_sub", "StartDayTime_inhib",
                     "Regimen_sub", "Regimen_inhib",
                     "DoseInt_sub", "DoseInt_inhib",
                     "NumDoses_sub", "NumDoses_inhib",
                     "GIAbsModel_sub", "GIAbsModel_inhib",
                     "VssInput_sub", "VssInput_inhib",
                     "VssPredMeth_sub", "VssPredMeth_inhib"),
            NameCol = c(rep(1, 25), rep(5, 24)),
            ValueCol = c(rep(2, 25), rep(6, 24)),
            Sheet = "Summary") %>%
            mutate(ValueCol = ifelse(str_detect(tolower(Deet), "inhib") &
                                           ValueCol == 2,
                                     3, ValueCol),
                   ValueCol = ifelse(str_detect(Deet, "inhib") &
                                           ValueCol == 6,
                                     7, ValueCol),
                   Class = c(rep("character", 2), rep("numeric", 4),
                             rep("character", 2), rep("numeric", 10),
                             "character", rep("numeric", 3), rep("character", 2),
                             "numeric", rep("character", 8), rep("numeric", 2),
                             rep("character", 4), rep("numeric", 4),
                             rep("character", 6)))

      InputDeets <- data.frame(
            Deet = c("Abs_model", "fa_input", "ka_input", "lag_input",
                     "fu_gut_input", "Peff", "UserAddnOrgan", "SimulatorVersion"),
            NameCol = c(rep(1, 7), 3),
            ValueCol = c(rep(2, 7), 4),
            Class = c("character", rep("numeric", 5), "character", "character"),
            Sheet = "Input Sheet"
      )

      AllDeets <- bind_rows(AllDeets, InputDeets)

      if(any(exp_details %in% AllDeets$Deet == FALSE)){
            Problem <- str_collapse(setdiff(exp_details, AllDeets$Deet), collapse = ", ")
            stop(paste0("These study details are not among the possible options: ",
                        Problem,
                        ". The study details to extract must be among the options listed. (Please see help file for all options.)"))
      }

      if(length(exp_details) == 0){
            stop("You must enter at least one study detail to extract.")
      }

      Out <- list()

      # Pulling details from the summary tab
      SumDeets <- intersect(exp_details,
                            AllDeets$Deet[AllDeets$Sheet == "Summary"])

      if(length(SumDeets) > 0){

            SummaryTab <- suppressMessages(
                  readxl::read_excel(path = sim_data_file, sheet = "Summary",
                                     col_names = FALSE))

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
                                     "StudyDuration" = "Study Duration",
                                     "PrandialSt_sub" = "Prandial State",
                                     "PrandialSt_inhib" = "Prandial State",
                                     "DoseRoute_sub" = "Route",
                                     "DoseRoute_inhib" = "Route",
                                     "DoseUnits_sub" = "Dose Units",
                                     "DoseUnits_inhib" = "Dose Units",
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
                                     "VssInput_sub" = "^Vss$",
                                     "VssInput_inhib" = "^Vss$",
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
                  Val <- ifelse(str_detect(deet, "DoseUnit"),
                                gsub("Dose \\(|\\)", "", Val), Val)

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

            # sub function for finding correct cell
            pullValue <- function(deet){

                  # Setting up regex to search
                  ToDetect <- switch(deet,
                                     "Abs_model" = "Absorption Model",
                                     "fa_input" = "^fa$",
                                     "ka_input" = "^ka \\(",
                                     "lag_input" = "lag time \\(",
                                     "fu_gut_input" = "fu\\(Gut\\)$",
                                     "Peff" = "Peff,man Cap",
                                     "UserAddnOrgan" = "User-defined Additional",
                                     "SimulatorVersion" = "Version number")
                  NameCol <- AllDeets$NameCol[which(AllDeets$Deet == deet)]
                  Row <- which(str_detect(InputTab[, NameCol] %>% pull(), ToDetect))
                  Val <- InputTab[Row, AllDeets$ValueCol[AllDeets$Deet == deet]] %>%
                        pull()

                  suppressWarnings(
                        Val <- ifelse(AllDeets$Class[AllDeets$Deet == deet] == "character",
                                      Val, as.numeric(Val))
                  )

                  # Tidying up some specific idiosyncracies of simulator output
                  Val <- ifelse(complete.cases(Val) & Val == "n/a", NA, Val)

                  return(Val)
            }

            for(i in InputDeets){
                  Out[[i]] <- pullValue(i)
            }

      }


      return(Out)

}



