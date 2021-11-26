#' Extract PK data for specific parameters from a simulator output Excel file
#'
#' This is useful for pulling what parameters were used in a simulation in
#' Simcyp and either checking the accuracy of those parameters or including them
#' in a report.
#'
#' @param sim_data_file name of the Excel file containing the simulator output
#' @param sheet optionally specify the name of the sheet where you'd like to
#'   pull the PK data. If left as NA, it will automatically be selected and
#'   could come from multiple tabs.
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file. Options are "all" for all possible parameters, "AUC tab" for
#'   only those parameters on the "AUC" tab (default), "Absorption tab" for only
#'   those parameters on the "Absorption" tab, or any combination of the
#'   following:
#'
#'   \describe{
#'
#'   \item{"AUCinf_dose1"}{AUC from 0 to infinity for dose 1. By default, data
#'   are pulled from the sheet "AUC", column titled, e.g., "AUC_INF (mg/L.h)"}
#'
#'   \item{AUCinf_dose1_withEffector}{AUC from 0 to infinity for dose 1 in the
#'   presence of an effector (Inhibitor 1 in the simulator). By default, data
#'   are pulled from the sheet "AUC", column titled, e.g., "AUC_INF_Inh
#'   (mg/L.h)" after the subheading "Extrapolated AUC_INF for the first dose in
#'   the presence of inhibitor"}
#'
#'   \item{"AUCtau_dose1"}{AUC from 0 to tau for dose 1. By default, data are
#'   pulled from sheet "AUC0(Sub)(CPlasma)", column titled, e.g., "AUC
#'   (mg/L.h)". IMPORTANT: This will be AUCtau for dose 1 if you have a done a
#'   multiple-dose simulation, but, if you have done a single-dose simulation,
#'   this will be the AUC from 0 to whatever time you stopped simulating.}
#'
#'   \item{"AUCtau_ss"}{AUC from 0 to tau for the steady-state dose in the
#'   simulation. By default, data are pulled from sheet "AUC" from the column
#'   titled, e.g., "AUC (mg/L.h)" under the subheading "Truncated AUCt for the
#'   last dose".}
#'
#'   \item{AUCtau_ss_withEffector}{AUC from 0 to tau for the steady-state dose
#'   in the presence of an effector (Inhibitor 1 in the simulator). By default,
#'   data are pulled from the sheet "AUC", column titled, e.g., "AUCt(n)_Inh
#'   (mg/L.h)" after the subheading "Truncated AUCt for the last dose in the
#'   presence of inhibitor"}
#'
#'   \item{"AUCtau_lastdoseToEnd"}{AUC from the last dose to the end of the
#'   simulation. By default, data are pulled from sheet "AUCX(Sub)(CPlasma)",
#'   where "X" is the largest dose for which there is a sheet, from the column
#'   titled, e.g., "AUC (mg/L.h)". \emph{Nota bene:} These data were calculated
#'   by the simulator from the beginning of the last dose to whenever the
#'   simulation ended, so that interval may not be tau if you did not set up the
#'   simulation that way. See the options for "AUCtau_ss" as an alternative.}
#'
#'   \item{"CL_dose1"}{Clearance as calculated by dose / AUCinf for dose 1. Data
#'   are pulled from the sheet "AUC" and the column titled, e.g., "CL
#'   (Dose/AUC_INF) (L/h)", subheading "Extrapolated AUC_INF for the first
#'   dose".}
#'
#'   \item{"CL_dose1_withEffector"}{Clearance as calculated by dose / AUCinf for
#'   dose 1 in the presence of an effector (Inhibitor 1 in the simulator). Data
#'   are pulled from the sheet "AUC" and the column titled, e.g., "CL
#'   (Dose/AUC_INF_Inh) (L/h)", subheading "Extrapolated AUC_INF for the first
#'   dose in the presence of inhibitor".}
#'
#'   \item{"CL_ss"}{Clearance as calculated by dose / AUCtau for the last dose
#'   simulated. By default, data are pulled from the sheet "AUC", column titled,
#'   e.g., "CL (Dose/AUC) (L/h)")}
#'
#'   \item{"CL_ss_withEffector"}{Clearance as calculated by dose / AUCtau for
#'   the last dose in the presence of an effector (Inhibitor 1 in the
#'   simulator). By default, data are pulled from the sheet "AUC" and the column
#'   titled, e.g., "CL (Dose/AUC_INF_Inh) (L/h)", subheading "Truncated AUCt for
#'   the last dose in the presence of inhibitor".}
#'
#'   \item{"CL_lastdoseToEnd"}{CL for the last dose calculated as Dose / AUCtau.
#'   By default, data are pulled from sheet "AUCX(Sub)(CPlasma)", where "X" is
#'   the largest dose for which there is a sheet, from the column titled, e.g.,
#'   "AUC (mg/L.h)". \emph{Nota bene:} These data were calculated by the
#'   simulator from the beginning of the last dose to whenever the simulation
#'   ended, so that interval may not be tau if you did not set up the simulation
#'   that way. See the options for "CL_ss_int" as an alternative.}
#'
#'   \item{"CL_hepatic"}{I'm not actually positive that this is the total
#'   hepatic clearance, but I think that's what it is... This is the value
#'   listed on the Summary tab, under "PKPD Parameters" with the name "CL
#'   (L/h)". This is often in cell A41. Sorry for the uncertain explanation;
#'   I'll update this when I know better what this is! -LS}
#'
#'   \item{"Cmax_dose1"}{Cmax for dose 1. By default, data are pulled from sheet
#'   "AUC0(Sub)(CPlasma)", column titled, e.g., "CMax (mg/L)".}
#'
#'   \item{"Cmax_dose1_withEffector"}{Cmax for the last dose in the presence of
#'   an inhibitor. By default, data are pulled from sheet "AUC0(Sub)(CPlasma)",
#'   column titled, e.g., "CMaxinh (mg/L)".}
#'
#'   \item{"Cmax_ss"}{Cmax for the last dose. By default, data are pulled from
#'   sheet "AUC", column titled, e.g., "CMax (mg/L)", under the subheading
#'   "Truncated AUCt for the last dose.}
#'
#'   \item{"HalfLife_dose1"}{half life for dose 1. By default, data are pulled
#'   from the sheet "AUC", column titled, e.g., "Half-life (h)")}
#'
#'   \item{"F_sub"}{bioavailability (F) of substrate. By default, data are
#'   pulled from the sheet "Clearance Trials SS".}
#'
#'   \item{"fa_sub" or "fa_inhib"}{fraction absorbed for the substrate or
#'   inhibitor. By default, data are pulled from the sheet "Absorption".}
#'
#'   \item{"fg_sub"}{fraction of substrate escaping gut metabolism. By default,
#'   data are pulled from the sheet "Clearance Trials SS".}
#'
#'   \item{"fh_sub"}{fraction of substrate escaping hepatic metabolism. By
#'   default, data are pulled from the sheet "Clearance Trials SS".}
#'
#'   \item{"ka_sub" or "ka_inhib"}{absorption rate constant ka for the substrate
#'   or the 1st inhibitor. By default, data are pulled from the sheet
#'   "Absorption".}
#'
#'   \item{"tlag_sub" or "tlag_inhib"}{lag time for the substrate or inhibitor
#'   1. By default, data are pulled from the sheet "Absorption".}
#'
#'   \item{"tmax_dose1"}{tmax for dose 1. By default, data are pulled from sheet
#'   "AUC0(Sub)(CPlasma)", column titled, e.g., "TMax (h)".}
#'
#'   \item{"tmax_ss"}{tmax for the last dose. By default, data are pulled from
#'   sheet "AUCX(Sub)(CPlasma)", where "X" is the largest dose for which there
#'   is a sheet, from the column titled, e.g., "TMax (h)".}
#'
#'   } The default is only those parameters present on the "AUC" sheet in the
#'   simulator output.
#' @param returnAggregateOrIndiv Return aggregate and/or individual PK
#'   parameters? Options are "aggregate" and/or "individual". For aggregate
#'   data, values are pulled from simulator output -- not calculated -- and the
#'   output will be a data.frame with the PK parameters in columns and the
#'   statistics reported exactly as in the simulator output file.
#' @param includeTrialInfo TRUE or FALSE: Include which individual and trial the
#'   data describe? This only applies when \code{returnAggregateOrIndiv}
#'   includes "individual".
#'
#' @return Depending on the options selected, returns a list of numerical
#'   vectors or a list of data.frames.
#'
#' @import tidyverse
#' @import readxl
#' @export
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
                      sheet = NA,
                      returnAggregateOrIndiv = c("aggregate", "individual"),
                      includeTrialInfo = TRUE){

      AllSheets <- readxl::excel_sheets(path = sim_data_file)

      # Determining the name of the tab that contains PK data for the last dose
      Tab_last <- AllSheets[str_detect(AllSheets, "AUC[0-9]{1,}")]
      ssNum <- as.numeric(str_extract(Tab_last, "[0-9]{1,}"))
      # It's the highest dose number and it can't be 0 b/c that's dose 1.
      ssNum <- suppressWarnings(max(ssNum[ssNum != 0]))
      Tab_last <- paste0("AUC", ssNum, "(Sub)(CPlasma)")

      # If the user supplied a sheet but it's just one of the sheets built in,
      # then use *that* sheet instead b/c that code is more versatile than the
      # generic one.
      if(complete.cases(sheet) &
         sheet %in% c(Tab_last, "AUC", "AUC0(Sub)(CPlasma)", "AUCt0(Sub)(CPlasma)",
                      "Absorption", "Clearance Trials SS")){
            sheet <- NA
      }

      if(complete.cases(sheet) & sheet %in% AllSheets == FALSE){
            stop("The sheet requested could not be found in the Excel file.")
      }

      # Error catching
      if(length(returnAggregateOrIndiv) > 2 | length(returnAggregateOrIndiv) < 1 |
         all(returnAggregateOrIndiv %in% c("aggregate", "individual")) == FALSE){
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
                              "CL_hepatic",
                              "Cmax_dose1",
                              "Cmax_dose1_withEffector",
                              "Cmax_ss",
                              "Cmax_ss_withEffector",
                              "fa_sub",
                              "F_sub",
                              "fh_sub",
                              "fg_sub",
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

      # Parameters to pull from the "AUC0(Sub)(CPlasma)" sheet
      Param_AUC0 <- c("AUCtau_dose1", "Cmax_dose1", "tmax_dose1",
                      "Cmax_dose1_withEffector")
      # Notes to self: AUCtau_dose1 appears to be the same in both
      # AUC0(Sub)(CPlasma) and AUCt0(Sub)(Plasma) tabs. Not sure if that ever
      # changes.

      # Parameters to pull from the AUCX(Sub)(CPlasma) sheet, where X is the last dose
      Param_AUCX <- c("AUCtau_lastdoseToEnd", "CL_lastdoseToEnd", "tmax_ss")

      # Parameters to pull from the Absorption sheet
      Param_Abs <- c("ka_sub", "ka_inhib", "fa_sub", "fa_inhib",
                     "tlag_sub", "tlag_inhib")

      # Parameters to pull from the Clearance Trials SS tab
      Param_CLTSS <- c("F_sub", "fh_sub", "fg_sub", "CL_hepatic")

      Out_ind <- list()
      Out_agg <- list()

      # Pulling data from the "AUC" sheet ------------------------------------------
      if(any(PKparameters %in% Param_AUC) & is.na(sheet)){

            PKparameters_AUC <- intersect(PKparameters, Param_AUC)

            # Error catching
            if("AUC" %in% AllSheets == FALSE){
                  warning(paste0("The sheet 'AUC' must be present in the Excel simulated data file to extract the PK parameters ",
                                 str_c(PKparameters_AUC, collapse = ", "),
                                 ". None of these parameters can be extracted."))
            } else {

                  AUC_xl <- suppressMessages(
                        readxl::read_excel(path = sim_data_file, sheet = "AUC",
                                           col_names = FALSE))

                  EndRow_ind <- which(AUC_xl$...2 == "Statistics") - 2


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
                                           "CL_ss_withEffector" = "CL \\(Dose/AUC\\)|CLinh \\(Dose/AUC\\)")


                        if(str_detect(PKparam, "_withEffector")){

                              # If there is an effector involved, need to start
                              # looking for the correct column after "for the
                              # Xth dose in the presence of inhibitor".

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
                                                           "for the last dose in the presence of inhibitor|^Inhibited$"))[1]
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
                              EndCol <- EndCol[EndCol > StartCol][1] - 1
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
                  StartRow_agg <- which(AUC_xl$...2 == "Statistics") + 2
                  EndRow_agg <- which(is.na(AUC_xl$...2))
                  EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1

                  for(i in PKparameters_AUC){
                        ColNum <- findCol(i)
                        if(length(ColNum) == 0){
                              message(paste("The column with information for", i,
                                            "cannot be found."))
                              rm(ColNum)
                              next
                        }

                        suppressWarnings(
                              Out_ind[[i]] <- AUC_xl[4:EndRow_ind, ColNum] %>%
                                    pull(1) %>% as.numeric
                        )

                        suppressWarnings(
                              Out_agg[[i]] <- AUC_xl[StartRow_agg:EndRow_agg, ColNum] %>%
                                    pull(1) %>% as.numeric()
                        )
                        names(Out_agg[[i]]) <- AUC_xl[StartRow_agg:EndRow_agg, 2] %>%
                              pull(1)

                        rm(ColNum)
                  }

                  if(includeTrialInfo){
                        # Subject and trial info
                        SubjTrial_AUC <- AUC_xl[4:EndRow_ind, 1:2] %>%
                              rename("Individual" = ...1, "Trial" = ...2)

                        Out_ind[["AUCtab"]] <- cbind(SubjTrial_AUC,
                                                     as.data.frame(Out_ind[PKparameters_AUC]))
                  }

                  rm(EndRow_ind, findCol, StartRow_agg, EndRow_agg)
            }
      }


      # Pulling data from the "AUC0(Sub)(CPlasma)" or "AUCt0(Sub)(CPlasma)" tabs -----------
      if(any(PKparameters %in% Param_AUC0) & is.na(sheet)){

            PKparameters_AUC0 <- intersect(PKparameters, Param_AUC0)

            # Error catching
            if(any(c("AUC0(Sub)(CPlasma)", "AUCt0(Sub)(CPlasma)") %in% AllSheets) == FALSE){

                  warning(paste0("The sheet 'AUC0(Sub)(CPlasma)' or 'AUCt0(Sub)(CPlasma)' must be present in the Excel simulated data file to extract the PK parameters ",
                                 str_c(PKparameters_AUC0, collapse = ", "),
                                 ". None of these parameters can be extracted."))
            } else {


                  Sheet <- intersect(c("AUC0(Sub)(CPlasma)", "AUCt0(Sub)(CPlasma)"), AllSheets)[1]

                  AUC0_xl <- suppressMessages(
                        readxl::read_excel(path = sim_data_file, sheet = Sheet,
                                           col_names = FALSE))

                  EndRow_ind <- which(AUC0_xl$...2 == "Statistics") - 3

                  findCol <- function(PKparam){

                        ToDetect <- switch(PKparam,
                                           "AUCtau_dose1" = "AUC \\(",
                                           "Cmax_dose1" = "CMax \\(",
                                           "Cmax_dose1_withEffector" = "CMaxinh",
                                           "tmax_dose1" = "TMax")

                        which(str_detect(as.vector(t(AUC0_xl[2, ])), ToDetect))[1]
                  }
                  # end of subfunction

                  # finding the PK parameters requested
                  StartRow_agg <- which(AUC0_xl$...2 == "Statistics") + 2
                  EndRow_agg <- which(is.na(AUC0_xl$...2))
                  EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1

                  for(i in PKparameters_AUC0){
                        ColNum <- findCol(i)
                        if(length(ColNum) == 0 | is.na(ColNum)){
                              message(paste("The column with information for", i,
                                            "cannot be found."))
                              rm(ColNum)
                              next
                        }

                        suppressWarnings(
                              Out_ind[[i]] <- AUC0_xl[3:EndRow_ind, ColNum] %>%
                                    pull(1) %>% as.numeric
                        )

                        suppressWarnings(
                              Out_agg[[i]] <- AUC0_xl[StartRow_agg:EndRow_agg, ColNum] %>%
                                    pull(1) %>% as.numeric()
                        )
                        names(Out_agg[[i]]) <- AUC0_xl[StartRow_agg:EndRow_agg, 2] %>%
                              pull(1)

                        rm(ColNum)
                  }

                  if(includeTrialInfo){
                        # Subject and trial info
                        SubjTrial_AUC0 <- AUC0_xl[3:EndRow_ind, 1:2] %>%
                              rename("Individual" = ...1, "Trial" = ...2)

                        Out_ind[["AUC0tab"]] <- cbind(SubjTrial_AUC0,
                                                      as.data.frame(Out_ind[PKparameters_AUC0]))
                  }

                  rm(EndRow_ind, findCol, Sheet)
            }
      }

      # Pulling data from the AUCX(Sub)(CPlasma) sheet ----------------------------
      if(any(PKparameters %in% Param_AUCX) & is.na(sheet)){

            PKparameters_AUCX <- intersect(PKparameters, Param_AUCX)

            # Error catching
            if(ssNum == 0 | length(ssNum) == 0){
                  warning(paste0("The sheet 'AUCX(Sub)(CPlasma)', where 'X' is the tab for the last dose administered and is not dose 1, must be present in the Excel simulated data file to extract the PK parameters ",
                                 str_c(PKparameters_AUCX, collapse = ", "),
                                 ". None of these parameters can be extracted."))
            } else {

                  AUCX_xl <- suppressMessages(
                        readxl::read_excel(path = sim_data_file, sheet = Tab_last,
                                           col_names = FALSE))

                  EndRow_ind <- which(AUCX_xl$...2 == "Statistics") - 3

                  findCol <- function(PKparam){

                        ToDetect <- switch(PKparam,
                                           "AUCtau_lastdoseToEnd" = "^AUC \\(",
                                           "CL_lastdoseToEnd" = "CL \\(Dose/AUC",
                                           "tmax_ss" = "^TMax")

                        which(str_detect(as.vector(t(AUCX_xl[2, ])), ToDetect))[1]
                  }
                  # end subfunction

                  # finding the PK parameters requested
                  StartRow_agg <- which(AUCX_xl$...2 == "Statistics") + 2
                  EndRow_agg <- which(is.na(AUCX_xl$...2))
                  EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1

                  for(i in PKparameters_AUCX){
                        ColNum <- findCol(i)

                        if(length(ColNum) == 0){
                              message(paste("The column with information for", i,
                                            "cannot be found."))
                              rm(ColNum)
                              next
                        }

                        suppressWarnings(
                              Out_ind[[i]] <- AUCX_xl[3:EndRow_ind, ColNum] %>%
                                    pull(1) %>% as.numeric
                        )

                        suppressWarnings(
                              Out_agg[[i]] <- AUCX_xl[StartRow_agg:EndRow_agg, ColNum] %>%
                                    pull(1) %>% as.numeric()
                        )
                        names(Out_agg[[i]]) <- AUCX_xl[StartRow_agg:EndRow_agg, 2] %>%
                              pull(1)

                        rm(ColNum)
                  }

                  if(includeTrialInfo){
                        # Subject and trial info
                        SubjTrial_AUCX <- AUCX_xl[3:EndRow_ind, 1:2] %>%
                              rename("Individual" = ...1, "Trial" = ...2)

                        Out_ind[["AUCXtab"]] <- cbind(SubjTrial_AUCX,
                                                      as.data.frame(Out_ind[PKparameters_AUCX]))
                  }

                  rm(EndRow_ind, findCol, StartRow_agg, EndRow_agg)

            }
      }
      # Pulling data from the "Absorption" sheet -----------------------------------
      if(any(PKparameters %in% Param_Abs) & is.na(sheet)){

            PKparameters_Abs <- intersect(PKparameters, Param_Abs)

            # Error catching
            if("Absorption" %in% AllSheets == FALSE){
                  warning(paste0("The sheet 'Absorption' must be present in the Excel simulated data file to extract the PK parameters ",
                                 str_c(PKparameters_Abs, collapse = ", "),
                                 ". None of these parameters can be extracted."))
            } else {

                  Abs_xl <- suppressMessages(
                        readxl::read_excel(path = sim_data_file, sheet = "Absorption",
                                           col_names = FALSE))

                  SubCols <- which(as.character(Abs_xl[8, ]) == "Substrate")[1]
                  InhibCols <- which(as.character(Abs_xl[8, ]) == "Inhibitor 1")[1]

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
                              Out_ind[[i]] <- Abs_xl[10:nrow(Abs_xl), ColNum] %>% rename(Values = 1) %>%
                                    filter(complete.cases(Values)) %>% pull(Values) %>% as.numeric
                        )

                        rm(ColNum)
                  }

                  if(includeTrialInfo){
                        # Subject and trial info
                        SubjTrial_Abs <- Abs_xl[(which(Abs_xl$...1 == "Index") + 1):
                                                      nrow(Abs_xl), 1:2] %>%
                              rename("Individual" = ...1, "Trial" = ...2)

                        Out_ind[["Abstab"]] <- cbind(SubjTrial_Abs,
                                                     as.data.frame(Out_ind[PKparameters_Abs]))
                  }

                  rm(findCol)

                  # AGGREGATE VALUES: For the absorption tab, the aggregate
                  # values are stored in a COMPLETELY different place, so
                  # extracting those values completely separately.

                  # I think the data always start in column 20, but I'm not
                  # positive. The value in the column that starts the aggregate
                  # data is "Trial Groups", so looking for that. It's in the
                  # same row where the value is "Index" in column 1. The 1st
                  # instance of "Trial" is for the individual data, so it has to
                  # be the 2nd instance.
                  IndexRow <- which(Abs_xl$...1 == "Index")
                  StartCol_agg <- which(str_detect(t(Abs_xl[IndexRow, ]), "Trial"))[2]

                  # They are NOT LABELED (!!!!) as such, but the summary stats
                  # are for fa, ka, and lag time in order for 1) the substrate,
                  # 2) inhibitor 1, 3) inhibitor 2, and 4) inhibitor 3. Getting
                  # the appropriate columns.
                  SubCols <- StartCol_agg:(StartCol_agg + 2)
                  Inhib1Cols <- (StartCol_agg + 3):(StartCol_agg + 5)
                  Inhib2Cols <- (StartCol_agg + 6):(StartCol_agg + 8)
                  Inhib3Cols <- (StartCol_agg + 9):(StartCol_agg + 11)
                  # Note to self: I have only set this up for substrate and
                  # inhibitor 1 so far. Return to this later if/when we want
                  # more.

                  # "Statistics" is in the column before StartCol_agg, so looking
                  # for that next.
                  StartRow_agg <- which(Abs_xl[, StartCol_agg - 1] == "Statistics") + 1
                  EndRow_agg <- which(is.na(Abs_xl[, StartCol_agg - 1]))
                  EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1

                  # sub function for finding the correct column
                  findCol <- function(PKparam){

                        ToDetect <- switch(PKparam,
                                           "ka_sub" = "^ka \\(1/",
                                           "ka_inhib" = "^ka \\(1/",
                                           "fa_sub" = "^fa$",
                                           "fa_inhib" = "^fa$",
                                           "tlag_sub" = "lag time \\(",
                                           "tlag_inhib" = "lag time \\(")

                        MyCols <- switch(str_extract(PKparam, "sub|inhib"),
                                         "sub" = SubCols,
                                         "inhib" = Inhib1Cols)
                        OutCol <- MyCols[
                              which(str_detect(
                                    as.character(Abs_xl[StartRow_agg, MyCols]),
                                    ToDetect))]

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
                              Out_agg[[i]] <- Abs_xl[(StartRow_agg + 1):EndRow_agg, ColNum] %>%
                                    pull(1) %>% as.numeric
                        )

                        names(Out_agg[[i]]) <- Abs_xl[(StartRow_agg + 1):EndRow_agg, StartCol_agg - 1] %>%
                              pull(1)

                        rm(ColNum)
                  }
            }
      }

      # Pulling data from the "Clearance Trials SS" sheet ------------------------------------------
      if(any(PKparameters %in% Param_CLTSS) & is.na(sheet)){

            PKparameters_CLTSS <- intersect(PKparameters, Param_CLTSS)

            # Error catching
            if("Clearance Trials SS" %in% AllSheets == FALSE){
                  warning(paste0("The sheet 'Clearance Trials SS' must be present in the Excel simulated data file to extract the PK parameters ",
                                 str_c(PKparameters_CLTSS, collapse = ", "),
                                 ". None of these parameters can be extracted."))
            } else {

                  CLTSS_xl <- suppressMessages(
                        readxl::read_excel(path = sim_data_file, sheet = "Clearance Trials SS",
                                           col_names = FALSE))

                  # sub function for finding correct column
                  findCol <- function(PKparam){

                        ToDetect <- switch(PKparam,
                                           "CL_hepatic" = "CL \\(L",
                                           "F_sub" = "F\\(Sub",
                                           "fh_sub" = "Fh\\(Sub",
                                           "fg_sub" = "Fg\\(Sub")

                        OutCol <- which(str_detect(as.vector(t(CLTSS_xl[1, ])),
                                                   ToDetect))
                        return(OutCol)
                  }
                  # end of subfunction

                  # finding the PK parameters requested
                  for(i in PKparameters_CLTSS){
                        ColNum <- findCol(i)
                        if(length(ColNum) == 0){
                              message(paste("The column with information for", i,
                                            "cannot be found."))
                              rm(ColNum)
                              next
                        }

                        suppressWarnings(
                              Out_ind[[i]] <- CLTSS_xl[2:nrow(CLTSS_xl), ColNum] %>%
                                    rename(Values = 1) %>%
                                    pull(Values) %>% as.numeric
                        )
                        rm(ColNum)
                  }

                  if(includeTrialInfo){
                        # Subject and trial info
                        SubjTrial_CLTSS <- CLTSS_xl[2:nrow(CLTSS_xl), 1:2] %>%
                              rename("Individual" = ...1, "Trial" = ...2)

                        Out_ind[["CLTSStab"]] <- cbind(SubjTrial_CLTSS,
                                                       as.data.frame(Out_ind[PKparameters_CLTSS]))
                  }

                  rm(findCol)

                  # AGGREGATE VALUES: For the CLTSS tab, the aggregate values
                  # are stored in a COMPLETELY different place, so extracting
                  # those values completely separately. I *think* the aggregate
                  # values always start in column 10, but I'm not sure, so let's
                  # check each time.
                  StartCol_agg <- which(str_detect(t(CLTSS_xl[1, ]), "Total Systemic"))
                  StartRow_agg <- which(CLTSS_xl[, StartCol_agg] == "Statistics") + 1
                  EndRow_agg <- which(is.na(CLTSS_xl[, StartCol_agg]))
                  EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1

                  # sub function for finding correct column
                  findCol <- function(PKparam){

                        ToDetect <- switch(PKparam,
                                           "CL_hepatic" = "CL \\(L",
                                           "F_sub" = "F\\(Sub",
                                           "fh_sub" = "Fh\\(Sub",
                                           "fg_sub" = "Fg\\(Sub")

                        OutCol <- which(str_detect(as.vector(t(CLTSS_xl[StartRow_agg, ])),
                                                   ToDetect))
                        return(OutCol)
                  }
                  # end of subfunction

                  # finding the PK parameters requested
                  for(i in PKparameters_CLTSS){
                        ColNum <- findCol(i)
                        if(length(ColNum) == 0){
                              message(paste("The column with information for", i,
                                            "cannot be found."))
                              rm(ColNum)
                              next
                        }

                        suppressWarnings(
                              Out_agg[[i]] <- CLTSS_xl[(StartRow_agg + 1):EndRow_agg, ColNum] %>%
                                    pull(1) %>% as.numeric
                        )

                        names(Out_agg[[i]]) <- CLTSS_xl[(StartRow_agg + 1):EndRow_agg, StartCol_agg] %>%
                              pull(1)

                        rm(ColNum)
                  }


            }
      }


      # Pulling parameters from a user-specified sheet --------------------------
      if(complete.cases(sheet)){

            # WARNING: I have NOT written this to work for aggregate values that
            # are listed anywhere but right below all the individual values.
            # That's just b/c I'm not sure where to look for aggregate values in
            # that situation.
            if("aggregate" %in% returnAggregateOrIndiv &
               str_detect(sheet, "AUC") == FALSE){
                  warning(paste0("This function has not (yet) been set up to extract aggregate PK data from the sheet ",
                                 sheet, ". It can extract individual data only."))
            }

            XL <- suppressMessages(
                  readxl::read_excel(path = sim_data_file, sheet = sheet,
                                     col_names = FALSE))

            HeaderRow <- which(XL$...1 == "Index")[1]
            EndRow_ind <- which(is.na(XL$...1))
            EndRow_ind <- min(EndRow_ind[EndRow_ind > HeaderRow]) - 1

            StartRow_agg <- which(XL$...2 == "Statistics") + 2
            EndRow_agg <- which(is.na(XL$...2))
            EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1

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
                              if(str_detect(PKparam, "_ss_withEffector")){
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
                              if(str_detect(PKparam, "_ss")){
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
                        XL[HeaderRow, ])), ToDetect) &
                              !str_detect(as.vector(t(XL[HeaderRow, ])), "%"))

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
                        Out_ind[[i]] <- XL[(HeaderRow+1):EndRow_ind, ColNum] %>%
                              pull(1) %>% as.numeric
                  )

                  suppressWarnings(
                        Out_agg[[i]] <- XL[StartRow_agg:EndRow_agg, ColNum] %>%
                              pull(1) %>% as.numeric()
                  )
                  names(Out_agg[[i]]) <- XL[StartRow_agg:EndRow_agg, 2] %>%
                        pull(1)

                  rm(ColNum)
            }

            if(includeTrialInfo){
                  # Subject and trial info
                  IndexCol <- which(str_detect(as.character(XL[HeaderRow, ]),
                                               "Index"))
                  SubjTrial_XL <- XL[(HeaderRow + 1):EndRow_ind, IndexCol:(IndexCol + 1)]
                  names(SubjTrial_XL) <- c("Individual", "Trial")

                  Out_ind[["Xtab"]] <- cbind(SubjTrial_XL,
                                             as.data.frame(Out_ind[PKparameters]))
            }

            rm(EndRow_ind, findCol)
      }

      # If user only wanted one parameter and includeTrialInfo was FALSE, make
      # the output a vector instead of a list
      if(length(Out_ind) == 1 & includeTrialInfo == FALSE){

            Out_ind <- Out_ind[[1]]

      } else {

            # Putting objects in alphabetical order
            ListItems <- names(Out_ind)
            Out_temp <- Out_ind
            Out_ind <- list()
            for(i in sort(ListItems)){
                  Out_ind[[i]] <- Out_temp[[i]]
            }

            if(includeTrialInfo & "individual" %in% returnAggregateOrIndiv){
                  Out_ind <- Out_ind[ListItems[str_detect(ListItems, "tab$")]]
                  Out_ind <- bind_rows(Out_ind) %>%
                        pivot_longer(cols = -(c(Individual, Trial)),
                                     names_to = "Parameter",
                                     values_to = "Value") %>%
                        filter(complete.cases(Value)) %>%
                        arrange(Parameter, as.numeric(Trial),
                                as.numeric(Individual)) %>%
                        pivot_wider(names_from = Parameter,
                                    values_from = Value)
            } else {
                  Out_ind <- Out_ind[ListItems[!str_detect(ListItems, "tab$")]] %>%
                        as.data.frame()
            }
      }

      if("aggregate" %in% returnAggregateOrIndiv &
         # If the user only wanted 1 parameter, ok to leave Out_agg as is b/c
         # it's a named vector.
         length(Out_agg) > 1){

            for(i in names(Out_agg)){
                  Statistic_char <- names(Out_agg[[i]])
                  Out_agg[[i]] <- as.data.frame(Out_agg[[i]]) %>%
                        mutate(Statistic = Statistic_char,
                               Parameter = i)
                  names(Out_agg[[i]])[1] <- "Value"
            }

            Out_agg <- bind_rows(Out_agg) %>%
                  select(Parameter, Statistic, Value) %>%
                  pivot_wider(names_from = Parameter, values_from = Value,
                              id_cols = Statistic)
      }

      if(length(returnAggregateOrIndiv) == 1 &
         returnAggregateOrIndiv[[1]] == "individual"){
            Out <- Out_ind
      }

      if(length(returnAggregateOrIndiv) == 1 &
         returnAggregateOrIndiv[[1]] == "aggregate"){
            Out <- Out_agg

      }

      if(length(returnAggregateOrIndiv) == 2){
            Out <- list("Individual" = Out_ind,
                        "Aggregate" = Out_agg)
      }

      return(Out)
}



