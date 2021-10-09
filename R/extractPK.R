#' Extract PK data for specific parameters from a simulator output Excel file
#'
#' This is useful for pulling what parameters were used in a simulation in
#' Simcyp and either checking the accuracy of those parameters or including them
#' in a report.
#'
#' @param sim_data_file name of the Excel file containing the simulator output
#' @param tab optionally specify the name of the tab where you'd like to pull
#'   the PK data. If left as NA, it will automatically be selected from,
#'   possibly, multiple tabs.
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file. Options are "all" for all possible parameters, "AUC tab" for
#'   only those parameters on the "AUC" tab (default), "Absorption tab" for only
#'   those parameters on the "Absorption" tab, or any combination of the
#'   following:
#'
#'   \describe{
#'
#'   \item{"AUCinf_dose1"}{AUC from 0 to infinity for dose 1. By default, data
#'   are pulled from the tab "AUC", column titled, e.g., "AUC_INF (mg/L.h)"}
#'
#'   \item{AUCinf_dose1_withEffector}{AUC from 0 to infinity for dose 1 in the
#'   presence of an effector (Inhibitor 1 in the simulator). By default, data
#'   are pulled from the tab "AUC", column titled, e.g., "AUC_INF_Inh (mg/L.h)"
#'   after the subheading "Extrapolated AUC_INF for the first dose in the
#'   presence of inhibitor"}
#'
#'   \item{"AUCtau_dose1"}{AUC from 0 to tau for dose 1. By default, data are
#'   pulled from tab "AUC0(Sub)(CPlasma)", column titled, e.g., "AUC (mg/L.h)".
#'   IMPORTANT: This will be AUCtau for dose 1 if you have a done a
#'   multiple-dose simulation, but, if you have done a single-dose simulation,
#'   this will be the AUC from 0 to whatever time you stopped simulating.}
#'
#'   \item{"AUCtau_ss"}{AUC from 0 to tau for the steady-state dose in the
#'   simulation. By default, data are pulled from tab "AUC" from the column
#'   titled, e.g., "AUC (mg/L.h)" under the subheading "Truncated AUCt for the
#'   last dose".}
#'
#'   \item{AUCtau_ss_withEffector}{AUC from 0 to tau for the steady-state dose in
#'   the presence of an effector (Inhibitor 1 in the simulator). By default,
#'   data are pulled from the tab "AUC", column titled, e.g., "AUCt(n)_Inh
#'   (mg/L.h)" after the subheading "Truncated AUCt for the last dose in the
#'   presence of inhibitor"}
#'
#'   \item{"AUCtau_lastdoseToEnd"}{AUC from the last dose to the end of the
#'   simulation. By default, data are pulled from tab "AUCX(Sub)(CPlasma)",
#'   where "X" is the largest dose for which there is a tab, from the column
#'   titled, e.g., "AUC (mg/L.h)". \emph{Nota bene:} These data were calculated
#'   by the simulator from the beginning of the last dose to whenever the
#'   simulation ended, so that interval may not be tau if you did not set up the
#'   simulation that way. See the options for "AUCtau_ss" as an
#'   alternative.}
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
#'   \item{"CL_ss"}{Clearance as calculated by dose / AUCtau for the last
#'   dose simulated. By default, data are pulled from the tab "AUC", column
#'   titled, e.g., "CL (Dose/AUC) (L/h)")}
#'
#'   \item{"CL_ss_withEffector"}{Clearance as calculated by dose / AUCtau
#'   for the last dose in the presence of an effector (Inhibitor 1 in the
#'   simulator). By default, data are pulled from the tab "AUC" and the column
#'   titled, e.g., "CL (Dose/AUC_INF_Inh) (L/h)", subheading "Truncated AUCt for
#'   the last dose in the presence of inhibitor".}
#'
#'   \item{"CL_lastdoseToEnd"}{CL for the last dose calculated as Dose / AUCtau.
#'   By default, data are pulled from tab "AUCX(Sub)(CPlasma)", where "X" is the
#'   largest dose for which there is a tab, from the column titled, e.g., "AUC
#'   (mg/L.h)". \emph{Nota bene:} These data were calculated by the simulator
#'   from the beginning of the last dose to whenever the simulation ended, so
#'   that interval may not be tau if you did not set up the simulation that way.
#'   See the options for "CL_ss_int" as an alternative.}
#'
#'   \item{"Cmax_dose1"}{Cmax for dose 1. By default, data are pulled from tab
#'   "AUC0(Sub)(CPlasma)", column titled, e.g., "CMax (mg/L)".}
#'
#'   \item{"Cmax_dose1_withEffector"}{Cmax for the last dose in the presence of
#'   an inhibitor. By default, data are pulled from tab "AUC0(Sub)(CPlasma)",
#'   column titled, e.g., "CMaxinh (mg/L)".}
#'
#'   \item{"Cmax_ss"}{Cmax for the last dose. By default, data are pulled
#'   from tab "AUC", column titled, e.g., "CMax (mg/L)", under the subheading
#'   "Truncated AUCt for the last dose.}
#'
#'   \item{"HalfLife_dose1"}{half life for dose 1. By default, data are pulled
#'   from the tab "AUC", column titled, e.g., "Half-life (h)")}
#'
#'   \item{"fa_sub" or "fa_inhib"}{fraction absorbed for the substrate or
#'   inhibitor 1. By default, data are pulled from the tab "Absorption".}
#'
#'   \item{"fg_sub" or "fg_inhib"}{fraction of substrate or inhibitor escaping
#'   gut metabolism. By default, data are pulled from the tab "Clearance Trials
#'   SS".}
#'
#'   \item{"fh_sub" or "fh_inhib"}{fraction of substrate or inhibitor escaping
#'   hepatic metabolism. By default, data are pulled from the tab "Clearance
#'   Trials SS".}
#'
#'   \item{"ka_sub" or "ka_inhib"}{absorption rate constant ka for the substrate
#'   or the 1st inhibitor. By default, data are pulled from the tab
#'   "Absorption".}
#'
#'   \item{"tlag_sub" or "tlag_inhib"}{lag time for the substrate or inhibitor 1.
#'   By default, data are pulled from the tab "Absorption".}
#'
#'   \item{"tmax_dose1"}{tmax for dose 1. By default, data are pulled from tab
#'   "AUC0(Sub)(CPlasma)", column titled, e.g., "TMax (h)".}
#'
#'   \item{"tmax_ss"}{tmax for the last dose. By default, data are pulled
#'   from tab "AUCX(Sub)(CPlasma)", where "X" is the largest dose for which
#'   there is a tab, from the column titled, e.g., "TMax (h)".}
#'
#'   } The default is only those parameters present on the "AUC" tab in the
#'   simulator output.
#' @param returnAggregateOrIndiv Return aggregate (geometric mean) and/or
#'   individual PK parameters? Options are "aggregate" or "individual".
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
#' extractPK(sim_data_file, PKparameters = "Absorption tab")
#' extractPK(sim_data_file, PKparameters = "AUCinf_dose1")
#'
#'
extractPK <- function(sim_data_file,
                      PKparameters = "AUC tab",
                      tab = NA,
                      returnAggregateOrIndiv = "individual"){

      AllSheets <- readxl::excel_sheets(path = sim_data_file)

      if(complete.cases(tab) & tab %in% AllSheets == FALSE){
            stop("The tab requested could not be found in the Excel file.")
      }

      # Error catching
      if(length(returnAggregateOrIndiv) != 1 |
         returnAggregateOrIndiv %in% c("aggregate", "individual") == FALSE){
            stop("You must return one or both of 'aggregate' or 'individual' data for the parameter 'returnAggregateOrIndiv'.")
      }

      if(PKparameters[1] == "all"){
            PKparameters <- c("AUCinf_dose1",
                              "AUCtau_dose1",
                              "AUCtau_ss",
                              "AUCtau_lastdoseToEnd",
                              "AUCinf_dose1_withEffector",
                              "AUCtau_ss_withEffector",
                              "CL_dose1",
                              "CL_dose1_withEffector",
                              "CL_ss",
                              "CL_ss_withEffector",
                              "CL_lastdoseToEnd",
                              "Cmax_dose1",
                              "Cmax_dose1_withEffector",
                              "Cmax_ss",
                              "Cmax_ss_withEffector",
                              "fa_sub", "fa_inhib",
                              "HalfLife_dose1",
                              "ka_sub", "ka_inhib",
                              "tlag_sub", "tlag_inhib",
                              "tmax_dose1")
      }

      if(PKparameters[1] == "AUC tab"){
            PKparameters <- c("AUCtau_ss", "Cmax_ss",
                              "Cmax_ss_withEffector",
                              "AUCinf_dose1", "HalfLife_dose1",
                              "AUCinf_dose1_withEffector",
                              "AUCtau_ss_withEffector",
                              "CL_dose1_withEffector",
                              "CL_dose1", "CL_ss",
                              "CL_ss_withEffector")
      }

      if(PKparameters[1] == "Absorption tab"){
            PKparameters <- c("ka_sub", "ka_inhib", "fa_sub", "fa_inhib",
                              "tlag_sub", "tlag_inhib")
      }

      # Checking experimental details to only pull details that apply
      Deets <- extractExpDetails(sim_data_file)

      if(is.na(Deets$Inhibitor)){
            PKparameters <- PKparameters[!str_detect(PKparameters, "Effector|inhib")]
      }

      if(is.na(Deets$Inhibitor) & Deets$Regimen_sub == "Single Dose"){
            PKparameters <- PKparameters[!str_detect(PKparameters, "ss")]
      }


      # Parameters to pull from the AUC tab
      Param_AUC <- c("AUCtau_ss", "Cmax_ss", "Cmax_ss_withEffector",
                     "AUCinf_dose1", "HalfLife_dose1", "AUCinf_dose1_withEffector",
                     "AUCtau_ss_withEffector", "CL_dose1_withEffector",
                     "CL_dose1", "CL_ss", "CL_ss_withEffector")

      # Parameters to pull from the "AUC0(Sub)(CPlasma)" tab
      Param_AUC0 <- c("AUCtau_dose1", "Cmax_dose1", "tmax_dose1",
                      "Cmax_dose1_withEffector")
      # Notes to self: AUCtau_dose1 appears to be the same in both
      # AUC0(Sub)(CPlasma) and AUCt0(Sub)(Plasma) tabs. Not sure if that ever
      # changes.

      # Parameters to pull from the AUCX(Sub)(CPlasma) tab, where X is the last dose
      Param_AUCX <- c("AUCtau_lastdoseToEnd", "CL_lastdoseToEnd", "tmax_ss")

      # Parameters to pull from the Absorption tab
      Param_Abs <- c("ka_sub", "ka_inhib", "fa_sub", "fa_inhib",
                     "tlag_sub", "tlag_inhib")

      Out <- list()

      # Pulling data from the "AUC" tab ------------------------------------------
      if(any(PKparameters %in% Param_AUC) & is.na(tab)){

            PKparameters_AUC <- intersect(PKparameters, Param_AUC)

            # Error catching
            if("AUC" %in% AllSheets == FALSE){
                  warning(paste0("The tab 'AUC' must be present in the Excel simulated data file to extract the PK parameters ",
                                 str_c(PKparameters_AUC, collapse = ", "),
                                 ". None of these parameters can be extracted."))
            } else {

                  AUC_xl <- suppressMessages(
                        readxl::read_excel(path = sim_data_file, sheet = "AUC",
                                           col_names = FALSE))

                  EndRow <- which(AUC_xl$...2 == "Statistics") - 2


                  # sub function for finding correct column
                  findCol <- function(PKparam){

                        ToDetect <- switch(PKparam,
                                           "AUCinf_dose1" = "^AUC_INF",
                                           "AUCinf_dose1_withEffector" = "^AUC_INF",
                                           "AUCtau_ss" = "AUCt\\(n\\) \\(|^AUC \\(",
                                           "AUCtau_ss_withEffector" = "AUCt\\(n\\)_Inh|AUCinh \\(",
                                           "Cmax_ss" = "^CMax",
                                           "Cmax_ss_withEffector" = "^CMax",
                                           "HalfLife_dose1" = "Half-life",
                                           "CL_dose1" = "CL .Dose/AUC_INF",
                                           "CL_dose1_withEffector" = "CL \\(Dose/AUC_INF_Inh\\)",
                                           "CL_ss" = "CL \\(Dose/AUC\\)",
                                           "CL_ss_withEffector" = "CL \\(Dose/AUC\\)")


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

                              # ss data
                              if(str_detect(PKparam, "_ss_withEffector")){
                                    StartCol <-
                                          which(str_detect(as.vector(t(AUC_xl[2, ])),
                                                           "for the last dose in the presence of inhibitor|^Inhibited$"))
                              }

                        } else {

                              # first dose
                              if(str_detect(PKparam, "_dose1")){

                                    StartCol <-  which(str_detect(as.vector(t(AUC_xl[2, ])),
                                                                  "^Extrapolated AUC_INF for the first dose$"))
                              }

                              # last dose
                              if(str_detect(PKparam, "_ss")){

                                    StartCol <- which(str_detect(as.vector(t(AUC_xl[2, ])),
                                                                 "^Truncated AUCt for the last dose$"))
                                    if(length(StartCol) == 0){
                                          StartCol <- which(str_detect(as.vector(t(AUC_xl[2, ])),
                                                                       "^AUC integrated from"))
                                    }
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

                        suppressWarnings(
                              Out[[i]] <- AUC_xl[4:EndRow, ColNum] %>% rename(Values = 1) %>%
                                    pull(Values) %>% as.numeric
                        )

                        rm(ColNum)
                  }

                  rm(EndRow, findCol)
            }
      }


      # Pulling data from the "AUC0(Sub)(CPlasma)" or "AUCt0(Sub)(CPlasma)" tabs -----------
      if(any(PKparameters %in% Param_AUC0) & is.na(tab)){

            PKparameters_AUC0 <- intersect(PKparameters, Param_AUC0)

            # Error catching
            if(any(c("AUC0(Sub)(CPlasma)", "AUCt0(Sub)(CPlasma)") %in% AllSheets) == FALSE){

                  warning(paste0("The tab 'AUC0(Sub)(CPlasma)' or 'AUCt0(Sub)(CPlasma)' must be present in the Excel simulated data file to extract the PK parameters ",
                                 str_c(PKparameters_AUC0, collapse = ", "),
                                 ". None of these parameters can be extracted."))
            } else {


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

                        suppressWarnings(
                              Out[[i]] <- AUC0_xl[3:EndRow, ColNum] %>% rename(Values = 1) %>%
                                    pull(Values) %>% as.numeric
                        )

                        rm(ColNum)
                  }

                  rm(EndRow, findCol, Sheet)
            }
      }

      # Pulling data from the AUCX(Sub)(CPlasma) tab ----------------------------
      if(any(PKparameters %in% Param_AUCX) & is.na(tab)){

            PKparameters_AUCX <- intersect(PKparameters, Param_AUCX)

            Tab_last <- AllSheets[str_detect(AllSheets, "AUC[0-9]{1,}")]
            ssNum <- max(as.numeric(str_extract(Tab_last, "[0-9]{1,}")))
            Tab_last <- paste0("AUC", ssNum, "(Sub)(CPlasma)")

            # Error catching
            if(ssNum == 0 | length(ssNum) == 0){
                  warning(paste0("The tab 'AUCX(Sub)(CPlasma)', where 'X' is the last dose administered and is not dose 1, must be present in the Excel simulated data file to extract the PK parameters ",
                                 str_c(PKparameters_AUCX, collapse = ", "),
                                 ". None of these parameters can be extracted."))
            } else {

                  AUCX_xl <- suppressMessages(
                        readxl::read_excel(path = sim_data_file, sheet = Tab_last,
                                           col_names = FALSE))

                  EndRow <- which(AUCX_xl$...2 == "Statistics") - 3

                  findCol <- function(PKparam){

                        ToDetect <- switch(PKparam,
                                           "AUCtau_lastdoseToEnd" = "^AUC \\(",
                                           "CL_lastdoseToEnd" = "CL \\(Dose/AUC",
                                           "tmax_ss" = "^TMax")

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

                        suppressWarnings(
                              Out[[i]] <- AUCX_xl[3:EndRow, ColNum] %>% rename(Values = 1) %>%
                                    pull(Values) %>% as.numeric
                        )

                        rm(ColNum)
                  }

                  rm(EndRow, findCol)

            }
      }
      # Pulling data from the "Absorption" tab -----------------------------------
      if(any(PKparameters %in% Param_Abs) & is.na(tab)){

            PKparameters_Abs <- intersect(PKparameters, Param_Abs)

            # Error catching
            if("Absorption" %in% AllSheets == FALSE){
                  warning(paste0("The tab 'Absorption' must be present in the Excel simulated data file to extract the PK parameters ",
                                 str_c(PKparameters_Abs, collapse = ", "),
                                 ". None of these parameters can be extracted."))
            } else {

                  Abs_xl <- suppressMessages(
                        readxl::read_excel(path = sim_data_file, sheet = "Absorption",
                                           col_names = FALSE))

                  SubCols <- which(as.character(Abs_xl[8, ]) == "Substrate")[1]
                  InhibCols <- which(as.character(Abs_xl[8, ]) == "Inhibitor 1")[1]

                  # SubCols <- SubCols:(SubCols + 2)
                  # InhibCols <- InhibCols:(InhibCols + 2)

                  # sub function for finding correct column
                  findCol <- function(PKparam){

                        ToDetect <- switch(PKparam,
                                           "ka_sub" = "^ka \\(1/",
                                           "ka_inhib" = "^ka \\(1/",
                                           "fa_sub" = "^fa$",
                                           "fa_inhib" = "^fa$",
                                           "tlag_sub" = "lag time \\(",
                                           "tlag_inhib" = "lag time \\(")


                        StartCol <- ifelse(str_detect(PKparam, "sub"),
                                           SubCols, InhibCols)
                        OutCol <- which(str_detect(
                              as.character(Abs_xl[9, StartCol:(StartCol+2)]),
                              ToDetect)) + StartCol - 1

                        return(OutCol)
                  }
                  # end of subfunction

                  # finding the PK parameters requested
                  for(i in PKparameters_Abs){
                        ColNum <- findCol(i)
                        if(length(ColNum) == 0){
                              message(paste("The column with information for", i,
                                            "cannot be found."))
                              rm(ColNum)
                              next
                        }

                        suppressWarnings(
                              Out[[i]] <- Abs_xl[10:nrow(Abs_xl), ColNum] %>% rename(Values = 1) %>%
                                    filter(complete.cases(Values)) %>% pull(Values) %>% as.numeric
                        )

                        rm(ColNum)
                  }

                  rm(findCol)
            }
      }

      # Pulling parameters from a user-specified tab --------------------------
      if(complete.cases(tab)){

            XL <- suppressMessages(
                  readxl::read_excel(path = sim_data_file, sheet = tab,
                                     col_names = FALSE))

            HeaderRow <- which(XL$...1 == "Index")[1]
            EndRow <- which(is.na(XL$...1))
            EndRow <- min(EndRow[EndRow > HeaderRow]) - 1

            # sub function for finding correct column
            findCol <- function(PKparam){

                  ToDetect <- switch(PKparam,
                                     "AUCinf_dose1" = "^AUC_INF",
                                     "AUCinf_dose1_withEffector" = "^AUC_INF",
                                     "AUCtau_dose1" = "AUC \\(",
                                     "AUCtau_ss" = "AUCt\\(n\\) \\(|^AUC \\(",
                                     "AUCtau_ss_withEffector" = "AUCt\\(n\\)_Inh|AUCinh \\(",
                                     "CL_dose1" = "CL .Dose/AUC_INF",
                                     "CL_dose1_withEffector" = "CL \\(Dose/AUC_INF_Inh\\)",
                                     "CL_ss" = "CL \\(Dose/AUC\\)",
                                     "CL_ss_withEffector" = "CL \\(Dose/AUC\\)",
                                     "Cmax_dose1" = "CMax \\(",
                                     "Cmax_dose1_withEffector" = "CMaxinh",
                                     "Cmax_ss" = "^CMax",
                                     "Cmax_ss_withEffector" = "^CMaxinh",
                                     "fa_sub" = "^fa$",
                                     "fa_inhib" = "^fa$",
                                     "HalfLife_dose1" = "Half-life",
                                     "ka_sub" = "^ka \\(1/",
                                     "ka_inhib" = "^ka \\(1/",
                                     "tlag_sub" = "lag time \\(",
                                     "tlag_inhib" = "lag time \\(",
                                     "tmax_dose1" = "TMax")

                  if(is.null(ToDetect)){
                        stop(paste("The parameter", PKparam, "could not be found."))
                  }

                  # If the header row is row 2, then there are no subheadings to read
                  # through and StartCol should be 1.
                  if(HeaderRow == 2){
                        StartCol <- 1
                  } else {

                        if(str_detect(PKparam, "Effector")){

                              # dose1 data
                              if(str_detect(PKparam, "_dose1_withEffector")){
                                    StartCol <-
                                          which(str_detect(as.vector(t(XL[2, ])),
                                                           "for the first dose in the presence of inhibitor"))
                              }

                              # lastdose data
                              if(str_detect(PKparam, "_lastdose_withEffector")){
                                    StartCol <-
                                          which(str_detect(as.vector(t(XL[2, ])),
                                                           "for the last dose in the presence of inhibitor|^Inhibited$"))

                                    if(length(StartCol) == 0){
                                          StartCol <- which(str_detect(as.vector(t(XL[2, ])),
                                                                       "^AUC integrated from"))
                                    }

                                    if(length(StartCol) == 0){
                                          StartCol <- which(str_detect(as.vector(t(XL[1, ])),
                                                                       "^AUC(.*)? integrated from"))
                                    }
                              }

                        } else {

                              # first dose
                              if(str_detect(PKparam, "_dose1")){
                                    StartCol <-  which(str_detect(as.vector(t(XL[2, ])),
                                                                  "^Extrapolated AUC_INF for the first dose$"))
                              }

                              # last dose
                              if(str_detect(PKparam, "_lastdose")){
                                    StartCol <- which(str_detect(as.vector(t(XL[2, ])),
                                                                 "^Truncated AUCt for the last dose$"))
                                    if(length(StartCol) == 0){
                                          StartCol <- which(str_detect(as.vector(t(XL[2, ])),
                                                                       "^AUC integrated from"))
                                    }
                                    if(length(StartCol) == 0){
                                          StartCol <- which(str_detect(as.vector(t(XL[1, ])),
                                                                       "^AUC(.*)? integrated from"))
                                    }
                              }
                        }
                  }

                  # Now should have StartCol. Finding OutCol.
                  if(length(StartCol) == 0){
                        StartCol <- 1
                  }

                  OutCol <- which(str_detect(as.vector(t(
                        XL[2, ])), ToDetect) &
                              !str_detect(as.vector(t(XL[2, ])), "%"))

                  return(OutCol)
            }
            # end of subfunction


            # finding the PK parameters requested
            for(i in PKparameters){
                  ColNum <- findCol(i)
                  if(length(ColNum) == 0){
                        message(paste("The column with information for", i,
                                      "cannot be found."))
                        rm(ColNum)
                        next
                  }

                  suppressWarnings(
                        Out[[i]] <- XL[(HeaderRow+1):EndRow, ColNum] %>% rename(Values = 1) %>%
                              pull(Values) %>% as.numeric
                  )

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
                  Out <- sapply(Out, FUN = gm_mean)
            } else {
                  Out = gm_mean(Out)
            }
      }

      return(Out)
}



