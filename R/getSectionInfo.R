#' Get information about this section of the report
#'
#' \code{getSectionInfo} pulls information from an Excel file that is formatted
#' \emph{exactly} like the file "Report input template.xlsx" and organizes those
#' data plus some further data it pulls from the simulator output Excel file.
#' The output of this function serves as part of the input for the function
#' \code{so_table}.
#'
#' @param report_input_file the name of the filled-in Excel file formatted
#'   exactly like "Report input template.xlsx", including the path if it's in
#'   any other directory than the current one
#' @param sheet the sheet to read
#'
#' @return a list object
#' @export
#'
#' @examples
#' # No examples yet.
#'
getSectionInfo <- function(report_input_file, sheet){

      Info <- suppressMessages(
            readxl::read_excel(path = report_input_file,
                               sheet = sheet,
                               skip = 1) %>%
      rename(RName = "Name in R code"))

      ClinStudy <- Info$Value[which(Info$RName == "ClinStudy")]

      SimFile <- Info$Value[which(Info$RName == "SimFile")]

      StatType <- Info$Value[which(Info$RName == "ArithOrGeom")]
      StatType <- ifelse(is.na(StatType),
                         ArithOrGeom, StatType)

      Deets <- extractExpDetails(sim_data_file = SimFile)

      # Tidying up the names used for populations so that they look nice in report
      Pop <- tidyPop(Deets)

      Dose <- Deets[["Dose_sub"]]
      DoseUnits <- Deets[["Units_dose_sub"]]

      NumSimSubj <- Deets[["NumSubjTrial"]] * Deets[["NumTrials"]]

      DoseRegimen <- Info$Value[which(Info$RName == "DoseRegimen")]
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


      InfoList <- list(ClinStudy, SimFile, StatType,
                       Deets,
                       Pop, Dose, DoseUnits, NumSimSubj,
                       DoseRegimen, DoseFreq,
                       Inhib, Dose_inhib, Units_dose_inhib, DoseFreq_inhib,
                       StartDoseDay_sub, StartDoseDay_inhib, LastDoseDay_inhib)

      names(InfoList) <- c("ClinStudy", "SimFile", "StatType",
                           "Deets",
                           "Pop", "Dose", "DoseUnits", "NumSimSubj",
                           "DoseRegimen", "DoseFreq",
                           "Inhib", "Dose_inhib", "Units_dose_inhib", "DoseFreq_inhib",
                           "StartDoseDay_sub", "StartDoseDay_inhib", "LastDoseDay_inhib")

      return(InfoList)
}
