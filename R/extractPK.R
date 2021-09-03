#' extractPK
#'
#' Extract PK data for specific parameters from a simulator output Excel file.
#'
#' @param sim_data_file name of the Excel file containing the simulator output
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file, most of which will be pulled from the "AUC" tab. Options are
#'   any combination of the following:
#'
#'   \describe{ \item{"AUCtau_dose1"}{AUC from 0 to tau for dose 1. This is the
#'   column "AUCt(0) (mg/L.h)". IMPORTANT: This will be AUCtau for dose 1 if you
#'   have a done a multiple-dose simulation, but, if you have done a single-dose
#'   simulation, this will be the AUC from 0 to whatever time you stopped
#'   monitoring.}
#'
#'   \item{"Cmax_dose1"}{Cmax for dose 1. This is pulled from the column titled,
#'   e.g., "CMax (mg/L)" on the tab "AUC0(Sub)(CPlasma)", so if that tab isn't
#'   included in your Excel file, you will get an error.}
#'
#'   \item{"tmax_dose1"}{tmax for dose 1. This is pulled from the column titled,
#'   e.g., "TMax (h)" on the tab "AUC0(Sub)(CPlasma)", so if that tab isn't
#'   included in your Excel file, you will get an error.}
#'
#'   \item{"AUCtau_lastdose"}{AUC from 0 to tau for the last dose in the
#'   simulation. This is the column "AUC (mg/L.h)" under the subheading "AUC
#'   integrated from X to Y".}
#'
#'
#'   \item{"AUCinf_dose1}{AUC from 0 to infinity for dose 1 (column: "AUC_INF
#'   (mg/L.h)")}
#'
#'   \item{"HalfLife_dose1"}{half life, estimated for dose 1 (column: "Half-life
#'   (h)")}
#'
#'   \item{"CL_dose1"}{Clearance as calculated by dose / AUCinf for dose 1
#'   (column: "CL (Dose/AUC_INF) (L/h)")}
#'
#'   \item{"CL_lastdose"}{Clearance as calculated by dose / AUCtau for the last
#'   dose simulated (column: "CL (Dose/AUC) (L/h)")} }
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
                                       "AUCtau_lastdose",
                                       "HalfLife_dose1",
                                       "CL_dose1", "CL_lastdose")){

   # Error catching
   if("AUC" %in% readxl::excel_sheets(sim_data_file) == FALSE){
      stop("The tab 'AUC' must be present in the Excel simulated data file to extract PK parameters.")
   }

   # Parameters to pull from the AUC tab
   Param_AUC <- c("AUCtau_dose1", "AUCtau_lastdose",
                  "AUCinf_dose1", "HalfLife_dose1",
                  "CL_dose1", "CL_lastdose")

   # Parameters to pull from the AUC0(Sub)(CPlasma) tab
   Param_AUC0 <- c("Cmax_dose1", "tmax_dose1")

   Out <- list()

   # Pulling data from the AUC tab
   if(any(PKparameters %in% Param_AUC)){

      AUC_xl <- suppressMessages(
         readxl::read_excel(path = sim_data_file, sheet = "AUC",
                            col_names = FALSE))

      PKparameters_AUC <- intersect(PKparameters, Param_AUC)

      EndRow <- which(AUC_xl$...2 == "Statistics") - 2

      findCol <- function(PKparam){

         ToDetect <- switch(PKparam,
                            "AUCtau_dose1" = "AUCt.0.",
                            "AUCtau_lastdose" = "AUC \\(",
                            "AUCinf_dose1" = "AUC_INF",
                            "HalfLife_dose1" = "Half-life",
                            "CL_dose1" = "CL .Dose/AUC_INF",
                            "CL_lastdose" = "CL \\(Dose/AUC\\)")

         which(str_detect(as.vector(t(AUC_xl[3, ])), ToDetect))[1]
      }

      for(i in PKparameters_AUC){
         ColNum <- findCol(i)
         if(length(ColNum) == 0){
            message(paste("The column with information for", i,
                          "cannot be found."))
            rm(ColNum)
            next
         }

         Out[[i]] <- AUC_xl[4:EndRow, ColNum] %>% rename(Values = 1) %>%
            pull(Values) %>% as.numeric
         rm(ColNum)
      }

      rm(EndRow)
   }


   # Pulling data from the AUCC0(Sub)(CPlasma) tab
   if(any(PKparameters %in% Param_AUC0)){

      AUC0_xl <- suppressMessages(
         readxl::read_excel(path = sim_data_file, sheet = "AUC0(Sub)(CPlasma)",
                            col_names = FALSE))

      PKparameters_AUC0 <- intersect(PKparameters, Param_AUC0)

      EndRow <- which(AUC_xl$...2 == "Statistics") - 3

      findCol <- function(PKparam){

         ToDetect <- switch(PKparam,
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

      rm(EndRow)

   }

   # If user only wanted one parameter, make the output a vector instead of a
   # list
   if(length(Out) == 1){
      Out <- Out[[1]]
   }

   return(Out)
}


