#' extractPK
#'
#' Extract PK data for specific parameters from a simulator output Excel file.
#'
#' @param sim_data_file name of the Excel file containing the simulator output
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file. Options are any combination of the following:
#'
#'   \describe{ \item{"AUCtau_dose1"}{AUC from 0 to tau for dose 1. Data are
#'   pulled from tab "AUC0(Sub)(CPlasma)", column titled, e.g., "AUC (mg/L.h)".
#'   IMPORTANT: This will be AUCtau for dose 1 if you have a done a
#'   multiple-dose simulation, but, if you have done a single-dose simulation,
#'   this will be the AUC from 0 to whatever time you stopped simulating.}
#'
#'   \item{"Cmax_dose1"}{Cmax for dose 1. Data are pulled from tab
#'   "AUC0(Sub)(CPlasma)", column titled, e.g., "CMax (mg/L)".}
#'
#'   \item{"tmax_dose1"}{tmax for dose 1. Data are pulled from tab
#'   "AUC0(Sub)(CPlasma)", column titled, e.g., "TMax (h)".}
#'
#'   \item{"AUCtau_lastdose_calc"}{AUC tau for the last dose. Data are pulled
#'   from tab "AUCX(Sub)(CPlasma)", where "X" is the largest dose for which
#'   there is a tab, from the column titled, e.g., "AUC (mg/L.h)". Nota bene:
#'   These data were calculated by the simulator from the beginning of the last
#'   dose to whenever the simulation ended, so that interval may not be tau if
#'   you did not set up the simulation that way. See the options for
#'   "AUCtau_lastdose_int" as an alternative.}
#'
#'   \item{AUCtau_lastdose_int}{AUC from 0 to tau for the last dose in the
#'   simulation. Data are pulled from tab "AUC" from the column titled, e.g.,
#'   "AUC (mg/L.h)" under the subheading "AUC integrated from X to Y". Nota
#'   bene: These data were integrated by the simulator using whatever sampling
#'   points you selected in the simulation. If you did not adequately sample the
#'   dosing interval, the estimate could be off. Also, if you did not set up
#'   your simulation to end at the end of the dosing interval, this will be the
#'   AUC from the last dosing time to the end of the simulation.}
#'
#'   \item{"AUCinf_dose1}{AUC from 0 to infinity for dose 1. Data are pulled
#'   from the tab "AUC", column titled, e.g., "AUC_INF (mg/L.h)")}
#'
#'   \item{"HalfLife_dose1"}{half life for dose 1. Data are pulled from the tab
#'   "AUC", column titled, e.g., "Half-life (h)")}
#'
#'   \item{"CL_dose1"}{Clearance as calculated by dose / AUCinf for dose 1. Data
#'   are pulled from the tab "AUC", column titled, e.g., "CL (Dose/AUC_INF)
#'   (L/h)")}
#'
#'   \item{"CL_lastdose_int"}{Clearance as calculated by dose / AUCtau for the
#'   last dose simulated. Data are pulled from the tab "AUC", column titled,
#'   e.g., "CL (Dose/AUC) (L/h)")}
#'
#'   \item{"CL_lastdose_calc"}{CL for the last dose calculated as Dose / AUCtau.
#'   Data are pulled from tab "AUCX(Sub)(CPlasma)", where "X" is the largest
#'   dose for which there is a tab, from the column titled, e.g., "AUC
#'   (mg/L.h)". Nota bene: These data were calculated by the simulator from the
#'   beginning of the last dose to whenever the simulation ended, so that
#'   interval may not be tau if you did not set up the simulation that way. See
#'   the options for "CL_lastdose_int" as an alternative.}
#'
#'   }
#'
#'
#'
#'
#' @return Depending on the options selected, returns a list of numerical
#'   vectors of whichever PK parameters were chosen. If only one PK parameter
#'   was requested, output is a numerical vector.
#'
#' @export
#'
#' @examples
#'
#' sim_data_file <- "../Example simulator output MD.xlsx"
#' extractPK(sim_data_file)
#' extractPK(sim_data_file, PKparameters = "AUCinf_dose1")
#'
#'
extractPK <- function(sim_data_file,
                      PKparameters = c("AUCtau_dose1", "AUCinf_dose1",
                                       "Cmax_dose1", "tmax_dose1",
                                       "AUCtau_lastdose_calc",
                                       "AUCtau_lastdose_int",
                                       "HalfLife_dose1",
                                       "CL_dose1",
                                       "CL_lastdose_calc")){

   AllSheets <- readxl::excel_sheets(path = sim_data_file)

   # Parameters to pull from the AUC tab
   Param_AUC <- c("AUCtau_lastdose_int",
                  "AUCinf_dose1", "HalfLife_dose1",
                  "CL_dose1", "CL_lastdose")

   # Parameters to pull from the "AUC0(Sub)(CPlasma)" tab
   Param_AUC0 <- c("AUCtau_dose1", "Cmax_dose1", "tmax_dose1")
   # Notes to self: AUCtau_dose1 appears to be the same in both
   # AUC0(Sub)(CPlasma) and AUCt0(Sub)(Plasma) tabs. Not sure if that ever
   # changes.

   # Parameters to pull from the AUCX(Sub)(CPlasma) tab, where X is the last dose
   Param_AUClast <- c("AUCtau_lastdose_calc", "CL_lastdose_calc")

   Out <- list()

   # Pulling data from the "AUC" tab
   if(any(PKparameters %in% Param_AUC)){

      PKparameters_AUC <- intersect(PKparameters, Param_AUC)

      # Error catching
      if("AUC" %in% AllSheets == FALSE){
         stop(paste0("The tab 'AUC' must be present in the Excel simulated data file to extract the PK parameters ",
                    PKparameters_AUC, "."))
      }

      AUC_xl <- suppressMessages(
         readxl::read_excel(path = sim_data_file, sheet = "AUC",
                            col_names = FALSE))

      EndRow <- which(AUC_xl$...2 == "Statistics") - 2

      findCol <- function(PKparam){

         ToDetect <- switch(PKparam,
                            "AUCtau_lastdose_int" = "AUCt\\(n\\) \\(",
                            "AUCinf_dose1" = "^AUC_INF",
                            "HalfLife_dose1" = "Half-life",
                            "CL_dose1" = "CL .Dose/AUC_INF",
                            "CL_lastdose" = "CL \\(Dose/AUC\\)")

         which(str_detect(as.vector(t(AUC_xl[3, ])), ToDetect) &
                  !str_detect(as.vector(t(AUC_xl[3, ])), "%"))
      }

      for(i in PKparameters_AUC){
         ColNum <- findCol(i)
         if(length(ColNum) == 0){
            message(paste("The column with information for", i,
                          "cannot be found."))
            rm(ColNum)
            next
         }

         # Need this to come from under the subheading "Truncated AUCt for the last dose"
         if(i %in% c("AUCtau_lastdose_int", "CL_lastdose")){
            CorrectColNum <- which(str_detect(as.vector(t(AUC_xl[2, ])),
                                              "Truncated AUCt for the last dose"))
            ColNum <- ColNum[ColNum > CorrectColNum]

         }

         Out[[i]] <- AUC_xl[4:EndRow, ColNum] %>% rename(Values = 1) %>%
            pull(Values) %>% as.numeric
         rm(ColNum)
      }

      rm(EndRow, findCol)
   }


   # Pulling data from the "AUC0(Sub)(CPlasma)" or "AUCt0(Sub)(CPlasma)" tabs
   if(any(PKparameters %in% Param_AUC0)){

      PKparameters_AUC0 <- intersect(PKparameters, Param_AUC0)

      # Error catching
      if(any(c("AUC0(Sub)(CPlasma)", "AUCt0(Sub)(CPlasma)") %in% AllSheets) == FALSE){
         stop(paste0("The tab 'AUC0(Sub)(CPlasma)' or 'AUCt0(Sub)(CPlasma)' must be present in the Excel simulated data file to extract the PK parameters ",
                     PKparameters_AUC0, "."))
      }

      Sheet <- intersect(c("AUC0(Sub)(CPlasma)", "AUCt0(Sub)(CPlasma)"), AllSheets)[1]

      AUC0_xl <- suppressMessages(
         readxl::read_excel(path = sim_data_file, sheet = Sheet,
                            col_names = FALSE))

      EndRow <- which(AUC_xl$...2 == "Statistics") - 3

      findCol <- function(PKparam){

         ToDetect <- switch(PKparam,
                            "AUCtau_dose1" = "AUC \\(",
                            "Cmax_dose1" = "CMax",
                            "tmax_dose1" = "TMax")

         which(str_detect(as.vector(t(AUC0_xl[2, ])), ToDetect))[1]
      }

       for(i in PKparameters_AUC0){
         ColNum <- findCol(i)
         if(length(ColNum) == 0){
            message(paste("The column with information for", i,
                          "cannot be found."))
            rm(ColNum)
            next
         }

         Out[[i]] <- AUC0_xl[3:EndRow, ColNum] %>% rename(Values = 1) %>%
            pull(Values) %>% as.numeric
         rm(ColNum)
      }

      rm(EndRow, findCol, Sheet)

   }

   # If user only wanted one parameter, make the output a vector instead of a
   # list
   if(length(Out) == 1){
      Out <- Out[[1]]
   }

   return(Out)
}


