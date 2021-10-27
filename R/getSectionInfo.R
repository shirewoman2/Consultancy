#' Get information about a section for a report
#'
#' \code{getSectionInfo} pulls information from an Excel file that is formatted
#' \emph{exactly} like the file "Report input template.xlsx" and organizes those
#' data plus some further data it pulls from the simulator output Excel file.
#' The output of this function serves as part of the input for the function
#' \code{so_table}.
#'
#' @param report_input_file the name of the Excel file formatted exactly like
#'   "Report input template.xlsx", including the path if it's in any other
#'   directory than the current one. This should be filled out with your
#'   specific project details.
#' @param sheet the sheet to read in that Excel file
#' @param report_input_DF a data.frame object that is laid out exactly like a
#'   tab from the "Report input template.xlsx" file
#'
#' @return a list object
#' @export
#'
#' @examples
#' # No examples yet.
#'
getSectionInfo <- function(report_input_file = NA,
                           sheet = NA,
                           report_input_DF = NA){

      if(all(c(complete.cases(report_input_file),
               complete.cases(sheet)))){
            InputXL <- suppressMessages(
                  readxl::read_excel(path = report_input_file,
                                     sheet = sheet,
                                     skip = 1) %>%
                        rename(RName = "Name in R code"))
      } else {
            InputXL <- report_input_DF
      }

      ClinStudy <- InputXL$Value[which(InputXL$RName == "ClinStudy")]

      SimFile <- InputXL$Value[which(InputXL$RName == "SimFile")]
      ObsFile_dose1 <- InputXL$Value[which(InputXL$RName == "ObsFile_dose1")]
      ObsFile_ss <- InputXL$Value[which(InputXL$RName == "ObsFile_ss")]
      ObsEffectorFile <- InputXL$Value[which(InputXL$RName == "ObsEffectorFile")]

      StatType <- InputXL$Value[which(InputXL$RName == "ArithOrGeom")]
      StatType <- ifelse(is.na(StatType),
                         ArithOrGeom, StatType)

      Deets <- extractExpDetails(sim_data_file = SimFile)

      # Tidying up the names used for populations so that they look nice in report
      Pop <- tidyPop(Deets$Pop)

      Dose <- Deets[["Dose_sub"]]
      DoseUnits <- Deets[["Units_dose_sub"]]

      NumSimSubj <- Deets[["NumSubjTrial"]] * Deets[["NumTrials"]]

      DoseRegimen <- InputXL$Value[which(InputXL$RName == "DoseRegimen")]
      DoseRegimen <- ifelse(DoseRegimen == "SD", "single dose", DoseRegimen)
      DoseRegimen <- ifelse(DoseRegimen == "MD", "multiple doses", DoseRegimen)

      DoseFreq <- switch(as.character(Deets[["DoseInt_sub"]]),
                         "12" = "BID",
                         "24" = "QD",
                         "8" = "TID")
      DoseFreq <- ifelse(is.null(DoseFreq),
                         DoseRegimen, DoseFreq)

      Inhib <- tolower(gsub(
            "SV-|Sim-|_EC|_SR|-MD|-SD|-[1-9]00 mg [QMSTBI]{1,2}D|_Fasted Soln|_Fed Capsule",
            "",
            Deets[["Inhibitor"]]))
      Dose_inhib <- Deets[["Dose_inhib"]]
      Units_dose_inhib <- Deets[["Units_dose_inhib"]]
      DoseFreq_inhib <- switch(as.character(Deets[["DoseInt_inhib"]]),
                               "12" = "BID",
                               "24" = "QD",
                               "8" = "TID")

      # Day substrate was administered
      StartDoseDay_sub <-
            as.numeric(sub("Day ", "",
                           str_split(Deets[["StartDayTime_sub"]], ", ")[[1]][1]))

      # Days inhibitor was administered
      StartDoseDay_inhib <- as.numeric(sub("Day ", "",
                                           str_split(Deets[["StartDayTime_inhib"]], ", ")[[1]][1]))

      LastDoseDay_inhib <-
            (Deets[["DoseInt_inhib"]] * Deets[["NumDoses_inhib"]])/24


      InfoList <- list(
            "InputXL" = InputXL,
            "StatType" = StatType,
            "ClinStudy" = ClinStudy,
            "SimFile" = SimFile,
            "ObsFile_dose1" = ObsFile_dose1, "ObsFile_ss" = ObsFile_ss,
            "ObsEffectorFile" = ObsEffectorFile,
            "Deets" = Deets, "Pop" = Pop, "NumSimSubj" = NumSimSubj,
            "Dose" = Dose, "DoseUnits" = DoseUnits, "DoseRegimen" = DoseRegimen,
            "DoseFreq" = DoseFreq,
            "Inhib" = Inhib, "Dose_inhib" = Dose_inhib,
            "Units_dose_inhib" = Units_dose_inhib, "DoseFreq_inhib" = DoseFreq_inhib,
            "StartDoseDay_sub" = StartDoseDay_sub,
            "StartDoseDay_inhib" = StartDoseDay_inhib,
            "LastDoseDay_inhib" = LastDoseDay_inhib)

      return(InfoList)
}
