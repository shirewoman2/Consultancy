#' extractExpDetails
#'
#' Extract details about the experimental design from the "Summary" tab of a
#' simulator output file.
#'
#'
#' @param sim_data_file name of the Excel file containing the simulator output
#' @param exp_details Experiment details you want to extract from the simulator
#'   output file, "Summary" tab. Options are any combination of the following:
#'
#'   \describe{
#'
#'   \item{Substrate}{the substrate used}
#'
#'   \item{Inhibitor}{inhibitor used, if applicable}
#'
#'   \item{MW_sub or MW_inhib}{molecular weight of substrate or inhibitor}
#'
#'   \item{logP_sub or logP_inhib}{logP of substrate or inhibitor}
#'
#'   \item{Type_sub or Type_inhib}{type of compound, e.g., monoprotic base}
#'
#'   \item{pKa1_sub, pKa2_sub, pKa1_inhib, or pKa2_inhib}{the pertinent pKa}
#'
#'   \item{BPratio_sub or BPratio_inhib}{blood-to-plasma ratio}
#'
#'   \item{Hematocrit or Heamatocrit}{the hematocrit}
#'
#'   \item{fu_sub or fu_inhib}{fraction unbound}
#'
#'   \item{ModelType_sub or ModelType_inhib}{the type of model, e.g., full PBPK
#'   model}
#'
#'   \item{Pop}{the population modeled}
#'
#'   \item{PopSize}{the size of population modeled}
#'
#'   \item{NumTrials}{the number of trials performed}
#'
#'   \item{NumSubjTrial}{number of subjects per trial}
#'
#'   \item{SimStartDayTime}{starting day and time of the simulation}
#'
#'   \item{SimEndDayTime}{ending day and time of the simulation}
#'
#'   \item{StudyDuration}{study duration}
#'
#'   \item{PrandialSt_sub or PrandialSt_inhib}{prandial state upon dosing}
#'
#'   \item{DoseRoute_sub or DoseRoute_inhib}{dose route, e.g. oral}
#'
#'   \item{DoseUnits_sub or DoseUnits_inhib}{dose units}
#'
#'   \item{Dose_sub or Dose_inhib}{dose administered}
#'
#'   \item{StartDayTime_sub or StartDayTime_inhib}{Starting day and time for
#'   administering the substrate or inhbitor}
#'
#'   \item{Regimen_sub or Regimen_inhib}{dosing regimen}
#'
#'   \item{DoseInt_sub or DoseInt_inhib}{dosing interval}
#'
#'   \item{NumDoses_sub or NumDoses_inhib}{number of doses}
#'
#'   \item{GIAbsModel_sub or GIAbsModel_inhib}{GI absorption model used}
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
                            exp_details = c("Substrate", "Inhibitor",
                                            "MW_sub", "MW_inhib",
                                            "logP_sub", "logP_inhib",
                                            "Type_sub", "Type_inhib",
                                            "pKa1_sub", "pKa1_inhib",
                                            "pKa2_sub", "pKa2_inhib",
                                            "BPratio_sub", "BPratio_inhib",
                                            "Hematocrit", "Haematocrit",
                                            "fu_sub", "fu_inhib",
                                            "ModelType_sub", "ModelType_inhib",
                                            "Pop", "PopSize", "NumTrials",
                                            "NumSubjTrial", "SimStartDayTime",
                                            "SimEndDayTime", "StudyDuration",
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
            ValueCol = c(rep(2, 25), rep(6, 24))) %>%
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

            Val <- ifelse(complete.cases(Val) & Val == "n/a", NA, Val)

            return(Val)
      }

      for(i in exp_details){
            Out[[i]] <- pullValue(i)
      }

      return(Out)

}



