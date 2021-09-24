#' Extract PK data for specific parameters from a simulator output Excel file
#'
#' This is useful for pulling what parameters were used for setting up a
#' simulation in Simcyp and either checking the accuracy of those parameters or
#' including them in a report.
#'
#' @param sim_data_file name of the Excel file containing the simulator output
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file. Options are any combination of the following:
#'
#'   \describe{
#'
#'   \item{"AUCinf_dose1"}{AUC from 0 to infinity for dose 1. Data are pulled
#'   from the tab "AUC", column titled, e.g., "AUC_INF (mg/L.h)"}
#'
#'   \item{AUCinf_dose1_withEffector}{AUC from 0 to infinity for dose 1 in the
#'   presence of an effector (Inhibitor 1 in the simulator). Data are pulled
#'   from the tab "AUC", column titled, e.g., "AUC_INF_Inh (mg/L.h)" after the
#'   subheading "Extrapolated AUC_INF for the first dose in the presence of
#'   inhibitor"}
#'
#'   \item{"AUCtau_dose1"}{AUC from 0 to tau for dose 1. Data are pulled from
#'   tab "AUC0(Sub)(CPlasma)", column titled, e.g., "AUC (mg/L.h)". IMPORTANT:
#'   This will be AUCtau for dose 1 if you have a done a multiple-dose
#'   simulation, but, if you have done a single-dose simulation, this will be
#'   the AUC from 0 to whatever time you stopped simulating.}
#'
#'   \item{"AUCtau_lastdose"}{AUC from 0 to tau for the last dose in the
#'   simulation. Data are pulled from tab "AUC" from the column titled, e.g.,
#'   "AUC (mg/L.h)" under the subheading "Truncated AUCt for the last dose".
#'   Nota bene: These data were integrated by the simulator using whatever
#'   sampling points you selected in the simulation. If you did not adequately
#'   sample the dosing interval, the estimate could be off. To get this
#'   parameter, you must have checked the box for "AUCt for the last dose" to be
#'   calculated under Outputs-->Data Analysis-->AUC to be calculated}
#'
#'   \item{AUCtau_lastdose_withEffector}{AUC from 0 to tau for the last dose in
#'   the presence of an effector (Inhibitor 1 in the simulator). Data are pulled
#'   from the tab "AUC", column titled, e.g., "AUCt(n)_Inh (mg/L.h)" after the
#'   subheading "Truncated AUCt for the last dose in the presence of inhibitor"}
#'
#'   \item{"AUCtau_lastdoseToEnd"}{AUC from the last dose to the end of the
#'   simulation. Data are pulled from tab "AUCX(Sub)(CPlasma)", where "X" is the
#'   largest dose for which there is a tab, from the column titled, e.g., "AUC
#'   (mg/L.h)". \emph{Nota bene:} These data were calculated by the simulator
#'   from the beginning of the last dose to whenever the simulation ended, so
#'   that interval may not be tau if you did not set up the simulation that way.
#'   See the options for "AUCtau_lastdose" as an alternative.}
#'
#'   \item{"CL_dose1"}{Clearance as calculated by dose / AUCinf for dose 1. Data
#'   are pulled from the tab "AUC" and the column titled, e.g., "CL
#'   (Dose/AUC_INF) (L/h)", subheading "Extrapolated AUC_INF for the first
#'   dose".}
#'
#'   \item{"CL_dose1_withEffector"}{Clearance as calculated by dose / AUCinf for
#'   dose 1 in the presence of an effector (Inhibitor 1 in the simulator). Data
#'   are pulled from the tab "AUC" and the column titled, e.g., "CL
#'   (Dose/AUC_INF_Inh) (L/h)", subheading "Extrapolated AUC_INF for the first
#'   dose in the presence of inhibitor".}
#'
#'   \item{"CL_lastdose"}{Clearance as calculated by dose / AUCtau for the last
#'   dose simulated. Data are pulled from the tab "AUC", column titled, e.g.,
#'   "CL (Dose/AUC) (L/h)")}
#'
#'   \item{"CL_lastdose_withEffector"}{Clearance as calculated by dose / AUCtau
#'   for the last dose in the presence of an effector (Inhibitor 1 in the
#'   simulator). Data are pulled from the tab "AUC" and the column titled, e.g.,
#'   "CL (Dose/AUC_INF_Inh) (L/h)", subheading "Truncated AUCt for the last dose
#'   in the presence of inhibitor".}
#'
#'   \item{"CL_lastdoseToEnd"}{CL for the last dose calculated as Dose / AUCtau.
#'   Data are pulled from tab "AUCX(Sub)(CPlasma)", where "X" is the largest
#'   dose for which there is a tab, from the column titled, e.g., "AUC
#'   (mg/L.h)". \emph{Nota bene:} These data were calculated by the simulator
#'   from the beginning of the last dose to whenever the simulation ended, so
#'   that interval may not be tau if you did not set up the simulation that way.
#'   See the options for "CL_lastdose_int" as an alternative.}
#'
#'   \item{"Cmax_dose1"}{Cmax for dose 1. Data are pulled from tab
#'   "AUC0(Sub)(CPlasma)", column titled, e.g., "CMax (mg/L)".}
#'
#'   \item{"Cmax_dose1_withEffector"}{Cmax for the last dose in the presence of
#'   an inhibitor. Data are pulled from tab "AUC0(Sub)(CPlasma)", column titled,
#'   e.g., "CMaxinh (mg/L)".}
#'
#'   \item{"Cmax_lastdose"}{Cmax for the last dose. Data are pulled from tab
#'   "AUC", column titled, e.g., "CMax (mg/L)", under the subheading "Truncated
#'   AUCt for the last dose.}
#'
#'   \item{"HalfLife_dose1"}{half life for dose 1. Data are pulled from the tab
#'   "AUC", column titled, e.g., "Half-life (h)")}
#'
#'   \item{"tmax_dose1"}{tmax for dose 1. Data are pulled from tab
#'   "AUC0(Sub)(CPlasma)", column titled, e.g., "TMax (h)".}
#'
#'   \item{"tmax_lastdose"}{tmax for the last dose. Data are pulled from tab
#'   "AUCX(Sub)(CPlasma)", where "X" is the largest dose for which there is a
#'   tab, from the column titled, e.g., "TMax (h)".}
#'
#'   }
#' @param returnAggregateOrIndiv Return aggregate (geometric mean) and/or
#'   individual PK parameters? Options are "aggregate" or "individual".
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
#' sim_data_file <- "../Example simulator output MD + inhibitor.xlsx"
#' extractPK(sim_data_file)
#' extractPK(sim_data_file, PKparameters = "AUCinf_dose1")
#'
#'
extractPK <- function(sim_data_file,
                      PKparameters = c("AUCinf_dose1",
                                       "AUCtau_dose1",
                                       "AUCtau_lastdose",
                                       "AUCtau_lastdoseToEnd",
                                       "AUCinf_dose1_withEffector",
                                       "AUCtau_lastdose_withEffector",
                                       "CL_dose1",
                                       "CL_dose1_withEffector",
                                       "CL_lastdose",
                                       "CL_lastdose_withEffector",
                                       "CL_lastdoseToEnd",
                                       "Cmax_dose1",
                                       "Cmax_dose1_withEffector",
                                       "Cmax_lastdose",
                                       "Cmax_lastdose_withEffector",
                                       "HalfLife_dose1",
                                       "tmax_dose1"),
                      returnAggregateOrIndiv = "individual"){

   AllSheets <- readxl::excel_sheets(path = sim_data_file)

   # Error catching
   if(length(returnAggregateOrIndiv) != 1 |
      returnAggregateOrIndiv %in% c("aggregate", "individual") == FALSE){
      stop("You must return one or both of 'aggregate' or 'individual' data for the parameter 'returnAggregateOrIndiv'.")
   }

   # Parameters to pull from the AUC tab
   Param_AUC <- c("AUCtau_lastdose", "Cmax_lastdose", "Cmax_lastdose_withEffector",
                  "AUCinf_dose1", "HalfLife_dose1", "AUCinf_dose1_withEffector",
                  "AUCtau_lastdose_withEffector", "CL_dose1_withEffector",
                  "CL_dose1", "CL_lastdose", "CL_lastdose_withEffector")

   # Parameters to pull from the "AUC0(Sub)(CPlasma)" tab
   Param_AUC0 <- c("AUCtau_dose1", "Cmax_dose1", "tmax_dose1",
                   "Cmax_dose1_withEffector")
   # Notes to self: AUCtau_dose1 appears to be the same in both
   # AUC0(Sub)(CPlasma) and AUCt0(Sub)(Plasma) tabs. Not sure if that ever
   # changes.

   # Parameters to pull from the AUCX(Sub)(CPlasma) tab, where X is the last dose
   Param_AUCX <- c("AUCtau_lastdoseToEnd", "CL_lastdoseToEnd", "tmax_lastdose")

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


      # sub function for finding correct column
      findCol <- function(PKparam){

         ToDetect <- switch(PKparam,
                            "AUCinf_dose1" = "^AUC_INF",
                            "AUCinf_dose1_withEffector" = "^AUC_INF",
                            "AUCtau_lastdose" = "AUCt\\(n\\) \\(",
                            "AUCtau_lastdose_withEffector" = "AUCt\\(n\\)_Inh",
                            "Cmax_lastdose" = "^CMax",
                            "Cmax_lastdose_withEffector" = "^CMax",
                            "HalfLife_dose1" = "Half-life",
                            "CL_dose1" = "CL .Dose/AUC_INF",
                            "CL_dose1_withEffector" = "CL \\(Dose/AUC_INF_Inh\\)",
                            "CL_lastdose" = "CL \\(Dose/AUC\\)",
                            "CL_lastdose_withEffector" = "CL \\(Dose/AUC\\)")


         if(str_detect(PKparam, "_withEffector")){

            # If there is an effector involved, need to start looking for the
            # correct column after "for the Xth dose in the presence of
            # inhibitor".

            # dose1 data
            if(str_detect(PKparam, "_dose1_withEffector")){
               StartCol <-
                  which(str_detect(as.vector(t(AUC_xl[2, ])),
                                   "for the first dose in the presence of inhibitor"))
            }

            # lastdose data
            if(str_detect(PKparam, "_lastdose_withEffector")){
               StartCol <-
                  which(str_detect(as.vector(t(AUC_xl[2, ])),
                                   "for the last dose in the presence of inhibitor"))
            }

         } else {

            # first dose
            if(str_detect(PKparam, "_dose1")){

               StartCol <-  which(str_detect(as.vector(t(AUC_xl[2, ])),
                                             "^Extrapolated AUC_INF for the first dose$"))
            }

            # last dose
            if(str_detect(PKparam, "_lastdose")){

               StartCol <-  which(str_detect(as.vector(t(AUC_xl[2, ])),
                                             "^Truncated AUCt for the last dose$"))
            }
         }

         if(length(StartCol) == 0){

            OutCol <- StartCol

         } else {

            # Find the last column at the end of whatever subheading this was under
            EndCol <- which(complete.cases(as.vector(t(AUC_xl[2, ]))))
            EndCol <- EndCol[EndCol > StartCol][1]
            EndCol <- ifelse(is.na(EndCol), ncol(AUC_xl), EndCol)

            if(any(is.na(c(StartCol, EndCol)))){

               OutCol <- EndCol

            } else {

               PossCol <- StartCol:EndCol

               OutCol <- PossCol[
                  which(str_detect(as.vector(t(
                     AUC_xl[3, PossCol])), ToDetect) &
                        !str_detect(as.vector(t(AUC_xl[3, PossCol])), "%")) ]

            }


         }

         return(OutCol)
      }
      # end of subfunction



      # finding the PK parameters requested
      for(i in PKparameters_AUC){
         ColNum <- findCol(i)
         if(length(ColNum) == 0){
            message(paste("The column with information for", i,
                          "cannot be found."))
            rm(ColNum)
            next
         }

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

      EndRow <- which(AUC0_xl$...2 == "Statistics") - 3

      findCol <- function(PKparam){

         ToDetect <- switch(PKparam,
                            "AUCtau_dose1" = "AUC \\(",
                            "Cmax_dose1" = "CMax \\(",
                            "Cmax_dose1_withEffector" = "CMaxinh",
                            "tmax_dose1" = "TMax")

         which(str_detect(as.vector(t(AUC0_xl[2, ])), ToDetect))[1]
      }

      for(i in PKparameters_AUC0){
         ColNum <- findCol(i)
         if(length(ColNum) == 0 | is.na(ColNum)){
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

   # Pulling data from the AUCX(Sub)(CPlasma) tab
   if(any(PKparameters %in% Param_AUCX)){

      PKparameters_AUCX <- intersect(PKparameters, Param_AUCX)

      Tab_last <- AllSheets[str_detect(AllSheets, "AUC[0-9]{1,}")]
      LastDoseNum <- max(as.numeric(str_extract(Tab_last, "[0-9]{1,}")))
      Tab_last <- paste0("AUC", LastDoseNum, "(Sub)(CPlasma)")

      # Error catching
      if(LastDoseNum == 0 | length(LastDoseNum) == 0){
         stop(paste0("The tab 'AUCX(Sub)(CPlasma)', where 'X' is the last dose administered and is not dose 1, must be present in the Excel simulated data file to extract the PK parameters ",
                     PKparameters_AUCX, "."))
      }

      AUCX_xl <- suppressMessages(
         readxl::read_excel(path = sim_data_file, sheet = Tab_last,
                            col_names = FALSE))

      EndRow <- which(AUCX_xl$...2 == "Statistics") - 3

      findCol <- function(PKparam){

         ToDetect <- switch(PKparam,
                            "AUCtau_lastdoseToEnd" = "^AUC \\(",
                            "CL_lastdoseToEnd" = "CL \\(Dose/AUC",
                            "tmax_lastdose" = "^TMax")

         which(str_detect(as.vector(t(AUCX_xl[2, ])), ToDetect))[1]
      }

      for(i in PKparameters_AUCX){
         ColNum <- findCol(i)
         if(length(ColNum) == 0){
            message(paste("The column with information for", i,
                          "cannot be found."))
            rm(ColNum)
            next
         }

         Out[[i]] <- AUCX_xl[3:EndRow, ColNum] %>% rename(Values = 1) %>%
            pull(Values) %>% as.numeric
         rm(ColNum)
      }

      rm(EndRow, findCol)

   }

   # If user only wanted one parameter, make the output a vector instead of a
   # list
   if(length(Out) == 1){

      Out <- Out[[1]]

   } else {

      # Putting objects in alphabetical order
      ListItems <- names(Out)
      Out_temp <- Out
      Out <- list()
      for(i in sort(ListItems)){
         Out[[i]] <- Out_temp[[i]]
      }
   }

   if(returnAggregateOrIndiv == "aggregate"){
      if(class(Out) == "list"){
         Out <- sapply(Out, FUN = function(.) exp(mean(log(.), na.rm = TRUE)))
      } else {
         Out = mean(Out, na.rm = TRUE)
      }
   }

   return(Out)
}



