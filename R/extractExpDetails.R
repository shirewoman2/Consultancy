#' Extract details about the experimental design
#'
#' \code{extractExpDetails} looks up experimental design details from the
#' "Summary" or "Input Sheet" tabs or the population tab of a Simcyp simulator
#' output file.
#'
#' @param sim_data_file name of the Excel file containing the simulator output
#' @param exp_details Experiment details you want to extract from the simulator
#'   output file. Options are \describe{
#'
#'   \item{"Summary tab"}{Extract details only from the "Summary tab" (default)}
#'
#'   \item{"Input Sheet"}{Extract details only from the "Input Sheet" tab}
#'
#'   \item{"population tab"}{Extract details about the population used (data
#'   come from the tab with the same name as the population simulated)}
#'
#'   \item{"all"}{Extract all possible parameters}
#'
#'   \item{a string of the specific parameters you want}{For a complete list,
#'   type \code{data(AllExpDetails)} into the console. Parameters are reported
#'   with a suffix depending on which compound they pertain to: "_sub" for the
#'   substrate, "_met1" for the primary metabolite, "_met2" for the second
#'   primary metabolite, "_secmet" for the secondary metabolite, "_inhib" for
#'   the 1st inhibitor or inducer listed, "_inhib2" for the 2nd inhibitor or
#'   inducer listed, or "_inh1met" for the inhibitor 1 metabolite. An example of
#'   acceptable input: \code{c("pKa1_sub", "fa_inhib2", "Regimen_sub")}}}
#'
#'   \emph{NOTE:} The default pulls only parameters that are listed on the
#'   "Summary" tab. If you want experimental details on a second inhibitor or
#'   more information on metabolites, try pulling them from the "Input sheet"
#'   instead of the "Summary" tab, which doesn't have as much information on
#'   those compounds.
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

      # Noting exp_details requested for later
      exp_details_input <- tolower(exp_details)
      # Noting which details are possible, which columns to search for their
      # names, which columns contain their values for substrates or
      # inhibitors, and what kind of data to format the output as at the end
      data(AllExpDetails)
      
      # Still need to add info for searching for some details. Removing those
      # from consideration for now.
      AllExpDetails <- AllExpDetails %>% filter(!str_detect(Sheet, "NEED TO ADD"))
      
      SumDeets <- AllExpDetails %>% filter(Sheet == "Summary") %>% 
          rename(Deet = Detail)
      PopDeets <- AllExpDetails %>% filter(Sheet == "population") %>% 
          rename(Deet = Detail)
      InputDeets <- AllExpDetails %>% filter(Sheet == "Input Sheet") %>% 
          rename(Deet = Detail)
      
      if(exp_details_input[1] == "all"){
            exp_details <- unique(AllExpDetails$Detail)
      }

      if(exp_details_input[1] == "summary tab"){
            exp_details <- c(SumDeets$Deet, "StartHr_sub", "StartHr_inhib")
            # Note that StartHr_inhib2, even if there were an inhibitor 2, is
            # not available from the Summary Sheet. It IS available from the
            # Input Sheet, though.
      }

      if(exp_details_input[1] == "input sheet"){
            exp_details <- c(InputDeets$Deet, "StartHr_sub", "StartHr_inhib",
                                         "StartHr_inhib2")
      }

      if(exp_details_input[1] == "population tab"){
            exp_details <- PopDeets$Deet
      }
      # Need to note original exp_details requested b/c I'm adding to it if
      # people request info from population tab. Note that this is different
      # from "exp_details_input" and serves a different purpose.
      exp_details_orig <- exp_details

      # Since StartHr_sub and StartHr_inhib are calculated from StartDayTime_sub
      # and StartDayTime_inhib, those must be included in exp_details to
      # extract. If the user wanted "Input Sheet" details, we can actually
      # calculate this without reading the summary tab, which takes more time,
      # so omitting in that instance.
      if("StartHr_sub" %in% exp_details & exp_details_input[1] != "input sheet"){
            exp_details <- unique(c(exp_details, "StartDayTime_sub",
                                    "SimStartDayTime"))
      }
      if("StartHr_inhib" %in% exp_details & exp_details_input[1] != "input sheet"){
            exp_details <- unique(c(exp_details, "StartDayTime_inhib",
                                    "SimStartDayTime"))
      }
      if("StartHr_inhib2" %in% exp_details & exp_details_input[1] != "input sheet"){
            exp_details <- unique(c(exp_details, "StartDayTime_inhib2",
                                    "SimStartDayTime"))
      }
      
      if(any(exp_details %in% AllExpDetails$Detail == FALSE)){
            Problem <- str_comma(setdiff(exp_details,
                                         AllExpDetails$Detail))
            stop(paste0("These study details are not among the possible options: ",
                        Problem,
                        ". The study details to extract must be among the options listed. (Please see help file for all options.)"))
      }

      if(length(exp_details) == 0){
            stop("You must enter at least one study detail to extract.")
      }
     
      Out <- list()

      if(any(exp_details %in% PopDeets$Deet)){
            exp_details <- c(exp_details, "Population")
            exp_details <- unique(exp_details)
      }

      # Pulling details from the summary tab ----------------------------------
      MySumDeets <- intersect(exp_details, SumDeets$Deet)
      # If all of the details are from one of the other sheets, then don't
      # bother reading this sheet b/c that takes more processing time. (Some
      # details show up on multiple sheets, so there are redundancies in this
      # function to deal with that.)
      if(exp_details_input[1] %in% c("input sheet")){
            MySumDeets <- intersect("A", "B")
      }

      if(exp_details_input[1] %in% c("population tab")){
            MySumDeets <- "Population"
      }

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
                                     "PrimaryMetabolite1" = "Sub Pri Metabolite1",
                                     "PrimaryMetabolite2" = "Sub Pri Metabolite2",
                                     "SecondaryMetabolite" = "Sub Sec Metabolite",
                                     "Inhibitor1Metabolite" = "Inh 1 Metabolite",
                                     "Inhibitor1" = "Compound Name",
                                     "Inhibitor2" = "Inhibitor 2",
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
                                     "Population" = "Population Name",
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
      }

      # Pulling details from the Input Sheet tab ------------------------------
      MyInputDeets <- intersect(exp_details, InputDeets$Deet)
      # Not pulling the same info twice
      MyInputDeets <- setdiff(MyInputDeets, names(Out))
      # If all of the details are from one of the other sheets, then don't
      # bother reading this sheet b/c that takes more processing time. (Some
      # details show up on multiple sheets, so there are redundancies in this
      # function to deal with that.)
      if(exp_details_input[1] %in% c("population tab", "summary tab")){
            MyInputDeets <- intersect("A", "B")
      }

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

            # When Inhibitor 1 is not present, don't look for those values.
            if(any(str_detect(t(InputTab[5, ]), "Inhibitor 1"), na.rm = T) == FALSE){
                  MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_inhib$|Inhibitor1")]
            }

            # When Inhibitor 2 is not present, don't look for those values.
            if(any(str_detect(t(InputTab[5, ]), "Inhibitor 2"), na.rm = T) == FALSE){
                  MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_inhib2$|Inhibitor2")]
            }

            # When primary metabolite 1 is not present, don't look for those values.
            if(any(str_detect(t(InputTab[5, ]), "Sub Pri Metabolite1"), na.rm = T) == FALSE){
                  MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_met1|PrimaryMetabolite1")]
            }

            # When primary metabolite 2 is not present, don't look for those values.
            if(any(str_detect(t(InputTab[5, ]), "Sub Pri Metabolite2"), na.rm = T) == FALSE){
                  MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_met2|PrimaryMetabolite2")]
            }

            # When secondary metabolite is not present, don't look for those values.
            if(any(str_detect(t(InputTab[5, ]), "Sub Sec Metabolite"), na.rm = T) == FALSE){
                  MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_secmet|SecondaryMetabolite")]
            }

            # When Inhibitor 1 metabolite is not present, don't look for those values.
            if(any(str_detect(t(InputTab[5, ]), "Inh 1 Metabolite"), na.rm = T) == FALSE){
                  MyInputDeets <- MyInputDeets[!str_detect(MyInputDeets, "_inhib1met|Inhibitor1Metabolite")]
            }

            # Looking for locations of columns.
            ColLocations <- c("Substrate" = 1,
                              "Trial Design" = which(t(InputTab[5, ]) == "Trial Design"),
                              "Inhibitor 1" = which(t(InputTab[5, ]) == "Inhibitor 1"),
                              "Inhibitor 2" = which(t(InputTab[5, ]) == "Inhibitor 2"),
                              "Sub Pri Metabolite1" = which(t(InputTab[5, ]) == "Sub Pri Metabolite1"),
                              "Sub Pri Metabolite2" = which(t(InputTab[5, ]) == "Sub Pri Metabolite2"),
                              "Sub Sec Metabolite" = which(t(InputTab[5, ]) == "Sub Sec Metabolite"),
                              "Inh 1 Metabolite" = which(t(InputTab[5, ]) == "Inh 1 Metabolite"))

            InputDeets$NameCol <- ColLocations[InputDeets$NameColDetect]
            InputDeets$ValueCol <- InputDeets$NameCol + 1

            # sub function for finding correct cell
            pullValue <- function(deet){

                  # Setting up regex to search
                  ToDetect <-
                        switch(sub("_sub|_inhib|_met1|_met2|_secmet|_inhib1met|_inhib2",
                                   "", deet),
                               "Abs_model" = "Absorption Model",
                               "Age_min" = "Minimum Age",
                               "Age_max" = "Maximum Age",
                               "CLrenal" = "CL R \\(L/h",
                               "DoseInt" = "Dose Interval",
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
                               "Regimen" = "Dosing Regimen",
                               "NumDoses" = "Number of Doses",
                               "kin_sac" = "SAC kin",
                               "kout_sac" = "SAC kout",
                               "Vsac" = "Volume .Vsac",
                               "Substrate" = "Compound Name",
                               "PrimaryMetabolite1" = "Compound Name",
                               "PrimaryMetabolite2" = "Compound Name",
                               "SecondaryMetabolite" = "Compound Name",
                               "Inhibitor1" = "Compound Name",
                               "Inhibitor2" = "Compound Name",
                               "Inhibitor1Metabolite" = "Compound Name")

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

                        Suffix <- str_extract(j, "_sub$|_inhib$|_inhib2$|_met1$|_secmet$|_inh1met$")
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

                        Suffix <- str_extract(j, "_sub$|_inhib$|_inhib2$|_met1$|_secmet$|_inh1met$")
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

            # Dealing with StartDayTime_x
            MyInputDeets4 <- MyInputDeets[str_detect(MyInputDeets, "StartDayTime")]

            if(length(MyInputDeets4) > 0){
                  for(j in MyInputDeets4){

                        NameCol <- InputDeets$NameCol[which(InputDeets$Deet == j)]
                        Row_day <- which(str_detect(InputTab[, NameCol] %>% pull(), "Start Day"))
                        Val_day <- InputTab[Row_day, InputDeets$ValueCol[
                              which(InputDeets$Deet == j)]] %>% pull()
                        Row_time <- which(str_detect(InputTab[, NameCol] %>% pull(), "Start Time"))
                        Val_time <- InputTab[Row_time, InputDeets$ValueCol[
                              which(InputDeets$Deet == j)]] %>% pull()
                        # Dealing with inconsistencies in time format
                        Val_time <- sub("m", "", Val_time)
                        Val_time <- str_split(Val_time, pattern = "h")[[1]]
                        Val_time <- str_c(str_pad(Val_time, width = 2, pad = "0"),
                                          collapse = ":")
                        Out[[j]] <- paste(paste0("Day ", Val_day),
                                          Val_time, sep = ", ")
                  }

            }
      }

      # Pulling details from the population tab -------------------------------
      MyPopDeets <- intersect(exp_details, PopDeets$Deet)
      # If all of the details are from one of the other sheets, then don't
      # bother reading this sheet b/c that takes more processing time. (Some
      # details show up on multiple sheets, so there are redundancies in this
      # function to deal with that.)
      if(exp_details_input[1] %in% c("summary tab", "input sheet")){
            MyInputDeets <- intersect("A", "B")
      }

      if(length(MyPopDeets) > 0){
            # Getting name of that tab.
            SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                                   error = openxlsx::getSheetNames(sim_data_file))
            PopSheet <- SheetNames[str_detect(SheetNames, str_sub(Out$Population, 1, 20))]

            PopTab <- suppressMessages(tryCatch(
                  readxl::read_excel(path = sim_data_file, sheet = PopSheet,
                                     col_names = FALSE),
                  error = openxlsx::read.xlsx(sim_data_file, sheet = PopSheet,
                                              colNames = FALSE)))
            # If openxlsx read the file, the names are different. Fixing.
            if(names(PopTab)[1] == "X1"){
                  names(PopTab) <- paste0("...", 1:ncol(PopTab))
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

      # Calculated details ------------------------------------------------
      if("StartHr_sub" %in% exp_details){
            if("SimStartDayTime" %in% names(Out)){
                  Out[["StartHr_sub"]] <- difftime_sim(time1 = Out$SimStartDayTime,
                                                       time2 = Out$StartDayTime_sub)
            } else {
                  if("Inhibitor1" %in% names(Out)){
                        if("Inhibitor2" %in% names(Out)){
                              TimeHr1 <- difftime_sim(Out$StartDayTime_inhib,
                                                      Out$StartDayTime_sub)
                              TimeHr2 <- difftime_sim(Out$StartDayTime_inhib2,
                                                      Out$StartDayTime_sub)
                              Out$StartHr_sub <- TimeHr
                              Out$StartHr_inhib <- -1*TimeHr1
                              Out$StartHr_inhib2 <- -1*TimeHr2

                        } else {
                              TimeHr <- difftime_sim(Out$StartDayTime_inhib,
                                                     Out$StartDayTime_sub)
                              Out$StartHr_sub <- TimeHr
                              Out$StartHr_inhib <- -1*TimeHr
                        }
                  } else {
                        # If no inhibitors were present, then the substrate should
                        # have been administered at t0 (would this ever not be the
                        # case?)
                        Out$StartHr_sub <- 0
                  }
            }
      }

      if("StartHr_inhib" %in% exp_details & "SimStartDayTime" %in% names(Out)){
            Out[["StartHr_inhib"]] <- difftime_sim(time1 = Out$SimStartDayTime,
                                                   time2 = Out$StartDayTime_inhib)
      }

      if("StartHr_inhib2" %in% exp_details & "SimStartDayTime" %in% names(Out)){
            Out[["StartHr_inhib2"]] <- difftime_sim(time1 = Out$SimStartDayTime,
                                                    time2 = Out$StartDayTime_inhib2)
      }

      # Removing StartDayTime_sub and SimStartDayTime if the user
      # did not request them.
      if("StartDayTime_sub" %in% exp_details_orig == FALSE){
            Out[["StartDayTime_sub"]] <- NULL
      }
      if("StartDayTime_inhib" %in% exp_details_orig == FALSE){
            Out[["StartDayTime_inhib"]] <- NULL
      }
      if("StartDayTime_inhib2" %in% exp_details_orig == FALSE){
            Out[["StartDayTime_inhib2"]] <- NULL
      }
      if("SimStartDayTime" %in% exp_details_orig == FALSE){
            Out[["SimStartDayTime"]] <- NULL
      }

      # Other functions call on "Inhibitor1", etc., so we need those objects to
      # exist. If user pulled data from Input Sheet, they might not, so setting
      # them to NA if they don't exist.
      if("Inhibitor1" %in% names(Out) == FALSE){
            Out$Inhibitor1 <- NA
      }
      if("Inhibitor2" %in% names(Out) == FALSE){
            Out$Inhibitor2 <- NA
      }
      if("PrimaryMetabolite1" %in% names(Out) == FALSE){
            Out$PrimaryMetabolite1 <- NA
      }
      if("PrimaryMetabolite2" %in% names(Out) == FALSE){
            Out$PrimaryMetabolite2 <- NA
      }
      if("SecondaryMetabolite" %in% names(Out) == FALSE){
            Out$SecondaryMetabolite <- NA
      }
      if("Inhibitor1Metabolite" %in% names(Out) == FALSE){
            Out$Inhibitor1Metabolite <- NA
      }

      Out <- Out[sort(names(Out))]

      return(Out)
}



