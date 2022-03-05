#' Extract PK data for specific parameters from a simulator output Excel file
#'
#' Pull calculated PK parameters from a Simcyp simulation output Excel file.
#' Note: Nearly all parameters are for the substrate.
#'
#' @param sim_data_file name of the Excel file containing the simulator output
#' @param sheet optionally specify the name of the sheet where you'd like to
#'   pull the PK data; for example, specify the tab where you have a
#'   user-defined AUC integration. If left as NA, it will automatically be
#'   selected and could come from multiple tabs. If you specify a sheet, you'll
#'   get all the possible parameters on that sheet, so no need to specify
#'   anything for \code{PKparameters}. \emph{Note:} Unless you want a very
#'   specific Excel sheet that's not what the usual sheet name would be for a
#'   first or last dose, this function will work best if this is left as NA.
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file. Options are "all" for all possible parameters, "AUC tab" for
#'   only those parameters on the "AUC" tab (default, "AUC_CI" tab will be used
#'   if "AUC" is not present), "Absorption tab" for only those parameters on the
#'   "Absorption" tab, or any combination of specific, individual parameters.
#'   Currently, the PK data are only for the substrate unless noted. To see the
#'   full set of possible parameters to extract, enter
#'   \code{data(AllPKParameters)} into the console.
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" or "blood".
#' @param returnAggregateOrIndiv Return aggregate and/or individual PK
#'   parameters? Options are "aggregate", "individual", or "both". For aggregate
#'   data, values are pulled from simulator output -- not calculated -- and the
#'   output will be a data.frame with the PK parameters in columns and the
#'   statistics reported exactly as in the simulator output file.
#' @param includeTrialInfo TRUE or FALSE: Include which individual and trial the
#'   data describe? This only applies when \code{returnAggregateOrIndiv}
#'   includes "individual".
#' @param checkDataSource TRUE or FALSE: Include in the output a data.frame that
#'   lists exactly where the data were pulled from the simulator output file.
#'   Useful for QCing.
#'
#' @return Depending on the options selected, returns a list of numerical
#'   vectors or a list of data.frames. If \code{checkDataSource} is TRUE, this
#'   will also return a data.frame indicating where in the simulator output file
#'   the data came from.
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
                      tissue = "plasma",
                      returnAggregateOrIndiv = "aggregate",
                      includeTrialInfo = TRUE,
                      checkDataSource = TRUE){
    
    # If they didn't include ".xlsx" at the end, add that.
    sim_data_file <- ifelse(str_detect(sim_data_file, "xlsx$"), 
                            sim_data_file, paste0(sim_data_file, ".xlsx"))
    
    if(returnAggregateOrIndiv[1] == "both"){
        returnAggregateOrIndiv <- c("aggregate", "individual")
    }
    
    AllSheets <- readxl::excel_sheets(path = sim_data_file)
    
    # Determining the name of the tab that contains PK data for the last dose
    # of the substrate (not the inhibitor... at least, not at this point).
    Tab_last <- AllSheets[str_detect(AllSheets, "AUC(t)?[0-9]{1,}") &
                              !str_detect(AllSheets, "Inh")]
    ssNum <- as.numeric(str_extract(Tab_last, "[0-9]{1,}"))
    # It's the highest dose number and it can't be 0 b/c that's dose 1.
    ssNum <- suppressWarnings(max(ssNum[ssNum != 0]))
    # If ssNum is now "-Inf" b/c it was all zeroes in the previous line but
    # there *is* a tab with "t" in the name, e.g., AUCt0(Sub)(CPlasma), then use
    # that one.
    Tab_last <- paste0("AUC(t)?", ssNum, "(_CI)?\\(Sub\\)\\(C",
                       str_to_title(tissue))
    Tab_last <- AllSheets[str_detect(AllSheets, Tab_last)]
    if(ssNum == -Inf){
        if(any(str_detect(AllSheets, "AUCt[0-9]{1,}") &
               !str_detect(AllSheets, "Inh"))){
            Tab_last <- AllSheets[str_detect(AllSheets, "AUCt[0-9]{1,}") &
                                      !str_detect(AllSheets, "Inh")]
        } else {
            Tab_last <- NA
        }
    }
    
    # Need to keep track of the original PK parameters requested so that we
    # don't waste time reading more sheets than necessary
    PKparameters_orig <- PKparameters
    
    # If the user supplied a sheet, temporarily set PKparameters to "all" to get
    # all the possible parameters from that sheet b/c they're named differently
    # since we don't know what dose that would be.
    if(complete.cases(sheet)){
        PKparameters <- "all"
    }
    
    if(PKparameters_orig[1] == "AUC tab" & 
       "AUC" %in% AllSheets == FALSE & 
       any(c("AUC0(Sub)(CPlasma)", "AUCt0(Sub)(CPlasma)") %in% AllSheets)){
        Sheet <- intersect(c("AUC0(Sub)(CPlasma)", "AUCt0(Sub)(CPlasma)"), AllSheets)[1]
        
        warning(paste0("You requested all the parameters from the 'AUC' sheet, but that sheet is not present in ",
                       sim_data_file, ". However, the tab ", Sheet, 
                       " *is* present; all PK parameters will be extracted from that sheet."))
        
        PKparameters <- "AUC0"
    }
    
    # Error catching
    if(complete.cases(sheet) & sheet %in% AllSheets == FALSE){
        stop("The sheet requested could not be found in the Excel file.")
    }
    
    if(length(returnAggregateOrIndiv) > 2 | length(returnAggregateOrIndiv) < 1 |
       all(returnAggregateOrIndiv %in% c("aggregate", "individual")) == FALSE){
        stop("You must return one or both of 'aggregate' or 'individual' data for the parameter 'returnAggregateOrIndiv'.")
    }
    
    ParamAUC <- AllPKParameters %>% filter(Sheet == "AUC") %>% 
        pull(PKparameter)
    
    ParamAbsorption <- AllPKParameters %>% filter(Sheet == "Absorption") %>% 
        pull(PKparameter)
    
    ParamAUC0 <- AllPKParameters %>% filter(Sheet == "AUC0") %>% 
        pull(PKparameter)
    
    ParamAUCX <- AllPKParameters %>% filter(Sheet == "AUCX") %>% 
        pull(PKparameter)
    
    ParamCLTSS <- AllPKParameters %>% filter(Sheet == "Clearance Trials SS") %>% 
        pull(PKparameter)
    
    ParamSummary <- AllPKParameters %>% filter(Sheet == "Summary") %>% 
        pull(PKparameter)
    
    if(PKparameters[1] == "all"){
        PKparameters <- unique(c(ParamAbsorption, ParamAUC, ParamAUC0,
                                 ParamAUCX, ParamCLTSS, ParamSummary))
    }
    
    if(PKparameters[1] == "AUC tab"){
        PKparameters <- ParamAUC
    }
    
    if(PKparameters[1] == "Absorption tab"){
        PKparameters <- ParamAbsorption
    }
    
    if(PKparameters[1] == "AUC0"){
        # This will happen if user requests PKparameters = "AUC" but "AUC" tab
        # is not present but a tab for AUC0 *is*.
        PKparameters <- ParamAUC0
        
    }
    
    MissingPKParam <- setdiff(PKparameters, AllPKParameters$PKparameter)
    if(length(MissingPKParam) > 0){
        warning(paste0("The parameter(s) ", str_comma(MissingPKParam),
                       " is/are not among the possible PK parameters and will not be extracted. Please see data(AllPKParameters) for all possible parameters."))
    }
    
    # Checking experimental details to only pull details that apply
    Deets <- extractExpDetails(sim_data_file)
    
    if(is.na(Deets$Inhibitor1)){
        PKparameters <- 
            PKparameters[PKparameters %in% 
                             AllPKParameters$PKparameter[AllPKParameters$AppliesOnlyWhenEffectorPresent == FALSE]]
    }
    
    if(Deets$Regimen_sub == "Single Dose"){
        PKparameters <- 
            PKparameters[PKparameters %in% 
                             AllPKParameters$PKparameter[AllPKParameters$AppliesToSingleDose == TRUE]]
    }
    
    # If it was a multiple-dose regimen, then the AUC tab will not include
    # certain parameters that WILL be able to be pulled from the AUC0 tab.
    # NOTE: I am NOT removing "AUCinf_ratio_dose1" from this list b/c it is
    # not available when the regimen is MD (at least, nowhere I've found in
    # the output). By NOT removing it, there will be a warning to the user
    # that that parameter was not found. Also, I'm removing some parameters
    # that are not completely clearly and unequivocably labeled so that they
    # can be pulled from sheets where they *are* so labeled.
    if(Deets$Regimen_sub == "Multiple Dose"){
        ParamAUC <- setdiff(ParamAUC,
                            c("AUCtau_ratio_dose1",
                              "Cmax_dose1", "Cmax_dose1_withInhib",
                              "Cmax_ratio_dose1", "tmax_dose1"))
    }
    
    PKparameters <- intersect(PKparameters, AllPKParameters$PKparameter)
    if(length(PKparameters) == 0){
        stop("There are no possible PK parameters to be extracted. Please check your input for 'PKparameters'. For example, check that you have not requested steady-state parameters for a single-dose simulation.")
    }
    
    # For the special cases when the user specified a sheet and did not leave
    # PKparameters as NA or as the default "AUC tab", then let's only return the
    # parameters that they asked for. 
    if(complete.cases(sheet) & complete.cases(PKparameters_orig[1]) &&
       PKparameters_orig[1] != "AUC tab"){
        PKparameters <- intersect(PKparameters, PKparameters_orig)
    }
    
    Out_ind <- list()
    Out_agg <- list()
    DataCheck <- data.frame(PKparam = as.character(NA),
                            Tab = as.character(NA),
                            StartColText = as.character(NA),
                            SearchText = as.character(NA),
                            Column = as.numeric(NA),
                            StartRow_agg = as.numeric(NA),
                            EndRow_agg = as.numeric(NA),
                            StartRow_ind = as.numeric(NA),
                            EndRow_ind = as.numeric(NA),
                            Note = as.character(NA))
    
    # Pulling data from the "AUC" sheet ------------------------------------------
    
    # Need to pull these parameters if either a) they requested a set of
    # parameters rather than asking for a set of parameters by sheet name (AUC
    # or Absorption tabs), did not specify an input sheet, and some of those
    # parameters are present on the AUC tab or b) the user requested the "AUC
    # tab" for PK parameters and either "AUC" or "AUC_CI" are among the sheets
    # in the file.
    if(is.na(sheet) && 
        # a)
        ((any(PKparameters %in% ParamAUC) & 
         PKparameters_orig[1] != "Absorption tab") |
        
        # b)
        (PKparameters_orig[1] == "AUC tab" & ("AUC" %in% AllSheets |
                                              "AUC_CI" %in% AllSheets)))){
        
        PKparameters_AUC <- intersect(PKparameters, ParamAUC)
        
        # Error catching
        if("AUC" %in% AllSheets == FALSE & "AUC_CI" %in% AllSheets == FALSE){
            warning(paste0("The sheet 'AUC' or 'AUC_CI' must be present in the Excel simulated data file to extract the PK parameters ",
                           str_c(PKparameters_AUC, collapse = ", "),
                           ". None of these parameters can be extracted."))
        } else {
            
            AUC_xl <- suppressMessages(
                readxl::read_excel(path = sim_data_file, 
                                   # If the user requested the "AUC" tab for PK
                                   # parameters, it's ok to use the tab "AUC_CI"
                                   # if "AUC" is not present.
                                   sheet = ifelse("AUC" %in% AllSheets == FALSE, 
                                                  "AUC_CI", "AUC"),
                                   col_names = FALSE))
            
            EndRow_ind <- which(AUC_xl$...2 == "Statistics") - 2
            
            # If tissue is blood, REMOVE the plasma columns entirely. I
            # think this will be easier to code. -LS
            if(tissue == "blood"){
                PlasmaCols <- c(which(str_detect(t(AUC_xl[1, ]), "CPlasma"))[1]:
                                    (which(str_detect(t(AUC_xl[1, ]), "CBlood"))[1] -1))
                if(length(PlasmaCols) > 0){
                    AUC_xl <- AUC_xl[, -PlasmaCols]
                }
            }
            
            # sub function for finding correct column
            findCol <- function(PKparam){
                
                ToDetect <- switch(PKparam,
                                   "AccumulationIndex" = "Accumulation Index$",
                                   "AccumulationRatio" = "Accumulation Ratio$",
                                   "AccumulationIndex_withInhib" = "Accumulation Index_Inh",
                                   "AccumulationRatio_withInhib" = "Accumulation Ratio_Inh",
                                   "AUCtau_dose1" = "^AUC \\(|AUCt.0. \\(",
                                   "AUCtau_dose1_withInhib" = "^AUCt.0._Inh \\(|^AUCinh \\(",
                                   "AUCtau_ratio_dose1" = "^AUC Ratio$",
                                   "AUCinf_dose1" = "^AUC_INF",
                                   "AUCinf_dose1_withInhib" = "^AUC_INF",
                                   "AUCinf_ratio_dose1" = "^AUC_INF ratio$",
                                   "AUCtau_ss" = "AUCt\\(n\\) \\(|^AUC \\(",
                                   "AUCtau_ss_withInhib" = "AUCt\\(n\\)_Inh|AUCinh \\(",
                                   "CL_dose1" = "CL .Dose/AUC_INF",
                                   "CL_dose1_withInhib" = "CL \\(Dose/AUC_INF_Inh\\)",
                                   "CL_ss" = "CL \\(Dose/AUC\\)",
                                   "CL_ss_withInhib" = "CL \\(Dose/AUC\\)|CLinh \\(Dose/AUC\\)",
                                   "Cmax_dose1" = "CMax \\(",
                                   "Cmax_dose1_withInhib" = "CMaxinh \\(",
                                   "Cmax_ratio_dose1" = "^CMax Ratio$",
                                   "Cmax_ss" = "^CMax",
                                   "Cmax_ss_withInhib" = "^CMax",
                                   "Cmax_ratio_ss" = "^CMax Ratio$",
                                   "HalfLife_dose1" = "Half-life",
                                   "tmax_dose1" = "^TMax \\(",
                                   "tmax_dose1_withInhib" = "^TMaxinh \\(",
                                   "tmax_ss" = "^TMax \\(", # RETURN TO THIS. Need to make sure it's going to pull the correct tmax.
                                   "tmax_ss_withInhib" = "TMaxinh \\("
                )
                
                # The AUC and Cmax ratios are listed with the parameters
                # with inhibitor present and need those columns to be
                # searched, even though "_withInhib" is not in the
                # name. Temporarily changing the parameter name to
                # search the appropriate columns. Note that this regex
                # will catch, e.g., "AUCinf_ratio_dose1" but NOT
                # "AccumulationRatio". That is by design.
                PKparam <- ifelse(str_detect(PKparam, "ratio"),
                                  paste0(PKparam, "_withInhib"),
                                  PKparam)
                
                # Dose 1 CL should be the clearance calculated using
                # AUCinf, not AUCtau for dose 1, so temporarily adding
                # "inf" to that parameter to check the correct columns.
                # Same thing with half life.
                PKparam <- ifelse(str_detect(PKparam, "CL.*_dose1|HalfLife_dose1"),
                                  sub("_dose1", "inf_dose1", PKparam), PKparam)
                
                # Dose 1 tmax and Cmax are only available for the 0 to tau columns, so
                # changing those parameter names temporarily. RETURN TO THIS: If the
                # user requests integration of the last dose, doesn't that show up here?
                # CHECK.
                PKparam <- ifelse(str_detect(PKparam, "[Ct]max"),
                                  sub("max", "maxtau", PKparam),
                                  PKparam)
                
                if(str_detect(PKparam, "_withInhib")){
                    
                    # If there is an inhibitor involved, need to start
                    # looking for the correct column after "for the
                    # Xth dose in the presence of inhibitor".
                    
                    # dose1 data
                    if(str_detect(PKparam, "dose1_withInhib")){
                        if(str_detect(PKparam, "inf")){
                            StartCol <-
                                which(str_detect(as.vector(t(AUC_xl[2, ])),
                                                 "for the first dose in the presence of inhibitor"))
                            StartColText <- "for the first dose in the presence of inhibitor"
                            
                        } else {
                            StartCol <-
                                which(str_detect(as.vector(t(AUC_xl[2, ])),
                                                 ifelse(Deets$Regimen_sub == "Single Dose",
                                                        "^Inhibited$",
                                                        "Truncated AUCt_inh for the first dose")))
                            StartColText <- ifelse(Deets$Regimen_sub == "Single Dose",
                                                   "^Inhibited$",
                                                   "Truncated AUCt_inh for the first dose")
                            
                        }
                    }
                    
                    # ss data
                    if(str_detect(PKparam, "_ss_withInhib")){
                        StartCol <-
                            which(str_detect(as.vector(t(AUC_xl[2, ])),
                                             "for the last dose in the presence of inhibitor|^Inhibited$"))[1]
                        StartColText <- "for the last dose in the presence of inhibitor|^Inhibited$"
                    }
                    
                } else {
                    
                    # first dose
                    if(str_detect(PKparam, "inf.*_dose1")){
                        StartCol <-  which(str_detect(as.vector(t(AUC_xl[2, ])),
                                                      "^Extrapolated AUC_INF for the first dose$"))
                        StartColText <- "^Extrapolated AUC_INF for the first dose$"
                    }
                    
                    if(str_detect(PKparam, "tau.*_dose1") &
                       Deets$Regimen_sub %in% c("Single Dose", "custom dosing")){
                        StartCol <-  which(str_detect(as.vector(t(AUC_xl[2, ])),
                                                      "^AUC integrated from"))
                        StartColText <- "^AUC integrated from"
                    }
                    
                    if(str_detect(PKparam, "tau.*_dose1") &
                       Deets$Regimen_sub %in% c("Multiple Dose")){
                        StartCol <- which(str_detect(as.vector(t(AUC_xl[2, ])),
                                                     "^Truncated AUCt for the first dose"))
                        StartColText <- "^Truncated AUCt for the first dose"
                    }
                    
                    # last dose
                    if(str_detect(PKparam, "_ss")){
                        
                        StartCol <- which(str_detect(as.vector(t(AUC_xl[2, ])),
                                                     "^Truncated AUCt for the last dose$"))
                        StartColText <- "^Truncated AUCt for the first dose"
                        if(length(StartCol) == 0){
                            StartCol <- which(str_detect(as.vector(t(AUC_xl[2, ])),
                                                         "^AUC integrated from"))
                            StartColText <- "^AUC integrated from"
                        }
                    }
                    
                    # accumulation index or other
                    if(str_detect(PKparam, "_dose1") == FALSE &
                       str_detect(PKparam, "_ss") == FALSE){
                        StartCol <- 1
                        StartColText <- NA
                    }
                }
                
                if(exists("StartCol", inherits = FALSE)){
                    StartCol <- StartCol[1]
                } else {
                    StartCol <- 1
                }
                
                if(length(StartCol) == 0){
                    
                    OutCol <- StartCol
                    
                } else {
                    
                    # Find the last column at the end of whatever subheading this was under
                    EndCol <- which(complete.cases(as.vector(t(AUC_xl[2, ]))))
                    EndCol <- EndCol[EndCol > StartCol][1] - 1
                    EndCol <- ifelse(is.na(EndCol), ncol(AUC_xl), EndCol)
                    
                    # accumulation index etc.
                    if(str_detect(PKparam, "_dose1") == FALSE &
                       str_detect(PKparam, "_ss") == FALSE){
                        EndCol <- ncol(AUC_xl)
                    }
                    
                    if(any(is.na(c(StartCol, EndCol)))){
                        
                        OutCol <- EndCol
                        
                    } else {
                        
                        PossCol <- StartCol:EndCol
                        
                        OutCol <- PossCol[
                            which(str_detect(as.vector(t(
                                AUC_xl[3, PossCol])), ToDetect) &
                                    !str_detect(as.vector(t(AUC_xl[3, PossCol])), "%")) ][1]
                    }
                }
                
                if(checkDataSource & complete.cases(OutCol)){
                    assign("SearchText4Col", value = StartColText,
                           pos = 1)
                    assign("SearchText", value = ToDetect,
                           pos = 1)
                }
                
                return(OutCol)
                
            }
            # end of subfunction
            
            # finding the PK parameters requested
            StartRow_agg <- which(AUC_xl$...2 == "Statistics") + 2
            EndRow_agg <- which(is.na(AUC_xl$...2))
            EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1
            EndRow_agg <- ifelse(is.na(EndRow_agg), nrow(AUC_xl), EndRow_agg)
            
            for(i in PKparameters_AUC){
                ColNum <- findCol(i)
                
                if(length(ColNum) == 0 | is.na(ColNum)){
                    message(paste("The column with information for", i,
                                  "cannot be found."))
                    suppressWarnings(rm(ColNum, SearchText4Col, SearchText))
                    PKparameters_AUC <- setdiff(PKparameters_AUC, i)
                    next
                }
                
                suppressWarnings(
                    Out_ind[[i]] <- AUC_xl[4:EndRow_ind, ColNum] %>%
                        pull(1) %>% as.numeric
                )
                
                if(any(is.na(Out_ind[[i]]) & str_detect(i, "inf"))){
                    # Simulator sometimes can't extrapolate to infinity well and you end
                    # up with NA values. If this happens, then AUCinf is NOT reliable and
                    # we SHOULD NOT use aggregated measures of it b/c they don't include
                    # all the data! Instead, pull AUCtau as well and give user a warning.
                    
                    NewParam <- sub("inf", "tau", i)
                    warning(paste0("The parameter ", i, " included some NA values, meaning that the Simulator had trouble extrapolating to infinity. No aggregate data will be returned for this parameter, and the parameter ", 
                                   NewParam, " will also be returned for use in place of ",
                                   i, " as you deem appropriate."))
                    
                    PKparameters_AUC <- unique(c(PKparameters_AUC, NewParam))
                    
                    ColNum_NewParam <- findCol(NewParam)
                    
                    if(length(ColNum) == 0 | is.na(ColNum_NewParam)){
                        message(paste("The column with information for", NewParam,
                                      "cannot be found."))
                        suppressWarnings(rm(ColNum_NewParam, SearchText4Col, SearchText))
                        PKparameters_AUC <- setdiff(PKparameters_AUC, NewParam)
                        next
                    }
                    
                    suppressWarnings(
                        Out_ind[[NewParam]] <- AUC_xl[4:EndRow_ind, ColNum_NewParam] %>%
                            pull(1) %>% as.numeric
                    )
                    
                    suppressWarnings(
                        Out_agg[[NewParam]] <- AUC_xl[StartRow_agg:EndRow_agg, ColNum_NewParam] %>%
                            pull(1) %>% as.numeric()
                    )
                    names(Out_agg[[NewParam]]) <- AUC_xl[StartRow_agg:EndRow_agg, 2] %>%
                        pull(1)
                    
                } else {
                    suppressWarnings(
                        Out_agg[[i]] <- AUC_xl[StartRow_agg:EndRow_agg, ColNum] %>%
                            pull(1) %>% as.numeric()
                    )
                    names(Out_agg[[i]]) <- AUC_xl[StartRow_agg:EndRow_agg, 2] %>%
                        pull(1)
                }
                
                if(checkDataSource){
                    DataCheck <- DataCheck %>%
                        bind_rows(data.frame(PKparam = i, 
                                             Tab = ifelse("AUC" %in% AllSheets == FALSE, 
                                                          "AUC_CI", "AUC"),
                                             StartColText = SearchText4Col,
                                             SearchText = SearchText,
                                             Column = ColNum,
                                             StartRow_agg = StartRow_agg,
                                             EndRow_agg = EndRow_agg,
                                             StartRow_ind = 4,
                                             EndRow_ind = EndRow_ind,
                                             Note = "StartColText is looking in row 2."))
                    
                    if(any(is.na(Out_ind[[i]]) & str_detect(i, "inf"))){
                        DataCheck <- DataCheck %>%
                            bind_rows(data.frame(PKparam = NewParam, 
                                                 Tab = ifelse("AUC" %in% AllSheets == FALSE, 
                                                              "AUC_CI", "AUC"),
                                                 StartColText = SearchText4Col,
                                                 SearchText = SearchText,
                                                 Column = ColNum_NewParam,
                                                 StartRow_agg = StartRow_agg,
                                                 EndRow_agg = EndRow_agg,
                                                 StartRow_ind = 4,
                                                 EndRow_ind = EndRow_ind,
                                                 Note = "StartColText is looking in row 2."))
                        rm(ColNum_NewParam, NewParam)
                    }
                    
                    suppressWarnings(rm(SearchText4Col, SearchText))
                }
                
                # end of iteration i
            }
            
            if(includeTrialInfo){
                # Subject and trial info
                SubjTrial_AUC <- AUC_xl[4:EndRow_ind, 1:2] %>%
                    rename("Individual" = ...1, "Trial" = ...2)
                
                Out_ind[["AUCtab"]] <- cbind(SubjTrial_AUC,
                                             as.data.frame(Out_ind[PKparameters_AUC]))
            }
            rm(findCol)
        }
    }
    
    
    # Pulling data from the "AUC0(Sub)(CPlasma)" or "AUCt0(Sub)(CPlasma)" tabs -----------
    PKparameters_AUC0 <- intersect(PKparameters, ParamAUC0)
    
    # Some PK parameters show up on multiple sheets. No need to pull
    # those here if they've already been pulled from another sheet.
    PKparameters_AUC0 <- setdiff(PKparameters_AUC0, names(Out_agg))
    
    if(complete.cases(sheet)){
        # How do you set something to have length 0? Hacking it for now.
        PKparameters_AUC0 <- intersect("A", "B")
    }
    
    if(length(PKparameters_AUC0) > 0 &
       (PKparameters_orig[1] %in% c("AUC tab", "Absorption tab") == FALSE |
        PKparameters_orig[1] == "AUC tab" & "AUC" %in% AllSheets == FALSE)){
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
                                   "AUCtau_dose1" = "^AUC \\(",
                                   "AUCtau_dose1_withInhib" = "^AUCinh \\(",
                                   "AUCtau_ratio_dose1" = "AUC Ratio",
                                   "Cmax_dose1" = "CMax \\(",
                                   "Cmax_dose1_withInhib" = "CMaxinh",
                                   "Cmax_ratio_dose1" = "^CMax Ratio$",
                                   "tmax_dose1" = "TMax",
                                   "tmax_dose1_withInhib" = "TMaxinh")
                
                if(checkDataSource){
                    assign("SearchText", value = ToDetect,
                           pos = 1)
                }
                
                return(which(str_detect(as.vector(t(AUC0_xl[2, ])),
                                        ToDetect))[1])
            }
            # end of subfunction
            
            # finding the PK parameters requested
            StartRow_agg <- which(AUC0_xl$...2 == "Statistics") + 2
            EndRow_agg <- which(is.na(AUC0_xl$...2))
            EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1
            EndRow_agg <- ifelse(is.na(EndRow_agg),
                                 nrow(AUC_xl), EndRow_agg)
            
            for(i in PKparameters_AUC0){
                ColNum <- findCol(i)
                if(length(ColNum) == 0 | is.na(ColNum)){
                    message(paste("The column with information for", i,
                                  "cannot be found."))
                    PKparameters_AUC0 <- setdiff(PKparameters_AUC0, i)
                    suppressWarnings(rm(ColNum, SearchText))
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
                
                if(checkDataSource){
                    DataCheck <- DataCheck %>%
                        bind_rows(data.frame(PKparam = i,
                                             Tab = Sheet,
                                             SearchText = SearchText,
                                             Column = ColNum,
                                             StartRow_agg = StartRow_agg,
                                             EndRow_agg = EndRow_agg,
                                             StartRow_ind = 3,
                                             EndRow_ind = EndRow_ind))
                }
                suppressWarnings(rm(ColNum, SearchText))
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
    PKparameters_AUCX <- intersect(PKparameters, ParamAUCX)
    
    # Some PK parameters show up on multiple sheets. No need to pull
    # those here if they've already been pulled from another sheet.
    PKparameters_AUCX <- setdiff(PKparameters_AUCX, names(Out_agg))
    
    if(complete.cases(sheet)){
        # How do you set something to have length 0? Hacking it for now.
        PKparameters_AUCX <- intersect("A", "B")
    }
    
    if(length(PKparameters_AUCX) > 0 &
       PKparameters_orig[1] %in% c("AUC tab", "Absorption tab") == FALSE){
        
        # Error catching
        if(ssNum %in% c(0, -Inf) | length(ssNum) == 0){
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
                                   "AUCtau_ss" = "AUC \\(",
                                   "AUCtau_ss_withInhib" = "AUCinh \\(",
                                   "AUCtau_ratio_ss" = "AUC Ratio",
                                   "CL_ss" = "CL \\(Dose/AUC",
                                   "CL_ss_withInhib" = "CLinh \\(Dose/AUCinh",
                                   "CL_ratio_ss" = "CL Ratio",
                                   "Cmax_ss" = "CMax \\(",
                                   "Cmax_ss_withInhib" = "CMaxinh \\(",
                                   "Cmax_ratio_ss" = "CMax Ratio",
                                   "Cmin_ss" = "CMin \\(",
                                   "Cmin_ss_withInhib" = "CMininh \\(",
                                   "Cmin_ratio_ss" = "CMin Ratio",
                                   "tmax_ss" = "^TMax \\(",
                                   "tmax_ss_withInhib" = "TMaxinh \\(")
                
                if(checkDataSource){
                    assign("SearchText", value = ToDetect,
                           pos = 1)
                }
                
                return(which(str_detect(as.vector(t(AUCX_xl[2, ])),
                                        ToDetect))[1])
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
                    PKparameters_AUCX <- setdiff(PKparameters_AUCX, i)
                    suppressWarnings(rm(ColNum, SearchText))
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
                
                if(checkDataSource){
                    DataCheck <- DataCheck %>%
                        bind_rows(data.frame(PKparam = i,
                                             Tab = Tab_last,
                                             SearchText = SearchText,
                                             Column = ColNum,
                                             StartRow_agg = StartRow_agg,
                                             EndRow_agg = EndRow_agg,
                                             StartRow_ind = 3,
                                             EndRow_ind = EndRow_ind))
                }
                suppressWarnings(rm(ColNum, SearchText))
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
    PKparameters_Abs <- intersect(PKparameters, ParamAbsorption)
    
    # Some PK parameters show up on multiple sheets. No need to pull
    # those here if they've already been pulled from another sheet.
    PKparameters_Abs <- setdiff(PKparameters_Abs, names(Out_agg))
    
    if(complete.cases(sheet)){
        # How do you set something to have length 0? Hacking it for now.
        PKparameters_Abs <- intersect("A", "B")
    }
    
    if(length(PKparameters_Abs) > 0 &
       PKparameters_orig[1] %in% c("AUC tab") == FALSE){
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
                
                if(checkDataSource){
                    assign("SearchText", value = ToDetect,
                           pos = 1)
                }
                
                return(OutCol)
            }
            # end of subfunction
            
            # finding the PK parameters requested
            for(i in PKparameters_Abs){
                ColNum <- findCol(i)
                if(length(ColNum) == 0){
                    message(paste("The column with information for", i,
                                  "cannot be found."))
                    PKparameters_Abs <- setdiff(PKparameters_Abs, i)
                    suppressWarnings(rm(ColNum, SearchText))
                    next
                }
                
                suppressWarnings(
                    Out_ind[[i]] <- Abs_xl[10:nrow(Abs_xl), ColNum] %>% rename(Values = 1) %>%
                        filter(complete.cases(Values)) %>% pull(Values) %>% as.numeric
                )
                
                if(checkDataSource){
                    DataCheck <- DataCheck %>%
                        bind_rows(data.frame(PKparam = i,
                                             Tab = "Absorption",
                                             SearchText = SearchText,
                                             Column = ColNum,
                                             StartRow_agg = NA,
                                             EndRow_agg = NA,
                                             StartRow_ind = 10,
                                             EndRow_ind = nrow(Abs_xl)))
                }
                
                suppressWarnings(rm(ColNum, SearchText))
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
                
                if(checkDataSource){
                    assign("SearchText", value = ToDetect,
                           pos = 1)
                }
                
                return(OutCol)
            }
            # end of subfunction
            
            # finding the PK parameters requested
            for(i in PKparameters_Abs){
                ColNum <- findCol(i)
                if(length(ColNum) == 0){
                    message(paste("The column with information for", i,
                                  "cannot be found."))
                    suppressWarnings(rm(ColNum, SearchText))
                    next
                }
                
                suppressWarnings(
                    Out_agg[[i]] <- Abs_xl[(StartRow_agg + 1):EndRow_agg, ColNum] %>%
                        pull(1) %>% as.numeric
                )
                
                names(Out_agg[[i]]) <- Abs_xl[(StartRow_agg + 1):EndRow_agg, StartCol_agg - 1] %>%
                    pull(1)
                
                if(checkDataSource){
                    DataCheck <- DataCheck %>%
                        bind_rows(data.frame(PKparam = i,
                                             Tab = "Absorption",
                                             SearchText = SearchText,
                                             Column = ColNum,
                                             StartRow_agg = StartRow_agg + 1,
                                             EndRow_agg = EndRow_agg,
                                             StartRow_ind = NA,
                                             EndRow_ind = NA))
                }
                suppressWarnings(rm(ColNum, SearchText))
            }
        }
    }
    
    # Pulling data from the "Clearance Trials SS" sheet ------------------------------------------
    PKparameters_CLTSS <- intersect(PKparameters, ParamCLTSS)
    
    # Some PK parameters show up on multiple sheets. No need to pull
    # those here if they've already been pulled from another sheet.
    PKparameters_CLTSS <- setdiff(PKparameters_CLTSS, names(Out_agg))
    
    if(complete.cases(sheet)){
        # How do you set something to have length 0? Hacking it for now.
        PKparameters_CLTSS <- intersect("A", "B")
    }
    
    if(length(PKparameters_CLTSS) > 0 &
       PKparameters_orig[1] %in% c("AUC tab", "Absorption tab") == FALSE){
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
                                   "CLpo" = "CLpo",
                                   "F_sub" = "F\\(Sub",
                                   "fh_sub" = "Fh\\(Sub",
                                   "fg_sub" = "Fg\\(Sub")
                
                OutCol <- which(str_detect(as.vector(t(CLTSS_xl[1, ])),
                                           ToDetect))
                
                if(checkDataSource){
                    assign("SearchText", value = ToDetect,
                           pos = 1)
                }
                
                return(OutCol)
            }
            # end of subfunction
            
            # finding the PK parameters requested
            for(i in PKparameters_CLTSS){
                ColNum <- findCol(i)
                if(length(ColNum) == 0){
                    message(paste("The column with information for", i,
                                  "cannot be found."))
                    PKparameters_CLTSS <- setdiff(PKparameters_CLTSS, i)
                    suppressWarninsg(rm(ColNum, SearchText))
                    next
                }
                
                suppressWarnings(
                    Out_ind[[i]] <- CLTSS_xl[2:nrow(CLTSS_xl), ColNum] %>%
                        rename(Values = 1) %>%
                        pull(Values) %>% as.numeric
                )
                
                
                if(checkDataSource){
                    DataCheck <- DataCheck %>%
                        bind_rows(data.frame(PKparam = i,
                                             Tab = "Clearance Trials SS",
                                             SearchText = SearchText,
                                             Column = ColNum,
                                             StartRow_agg = NA,
                                             EndRow_agg = NA,
                                             StartRow_ind = 2,
                                             EndRow_ind = nrow(CLTSS_xl)))
                }
                
                suppressWarnings(rm(ColNum, SearchText))
                
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
                                   "CLpo" = "CLpo",
                                   "F_sub" = "F\\(Sub",
                                   "fh_sub" = "Fh\\(Sub",
                                   "fg_sub" = "Fg\\(Sub")
                
                OutCol <- which(str_detect(as.vector(t(CLTSS_xl[StartRow_agg, ])),
                                           ToDetect))
                
                if(checkDataSource){
                    assign("SearchText", value = ToDetect,
                           pos = 1)
                }
                
                return(OutCol)
            }
            # end of subfunction
            
            # finding the PK parameters requested
            for(i in PKparameters_CLTSS){
                ColNum <- findCol(i)
                if(length(ColNum) == 0){
                    message(paste("The column with information for", i,
                                  "cannot be found."))
                    suppressWarnings(rm(ColNum, SearchText))
                    next
                }
                
                suppressWarnings(
                    Out_agg[[i]] <- CLTSS_xl[(StartRow_agg + 1):EndRow_agg, ColNum] %>%
                        pull(1) %>% as.numeric
                )
                
                names(Out_agg[[i]]) <- CLTSS_xl[(StartRow_agg + 1):EndRow_agg, StartCol_agg] %>%
                    pull(1)
                
                if(checkDataSource){
                    DataCheck <- DataCheck %>%
                        bind_rows(data.frame(PKparam = i,
                                             Tab = "Clearance Trials SS",
                                             SearchText = SearchText,
                                             Column = ColNum,
                                             StartRow_agg = StartRow_agg + 1,
                                             EndRow_agg = EndRow_agg,
                                             StartRow_ind = NA,
                                             EndRow_ind = NA))
                }
                
                suppressWarnings(rm(ColNum, SearchText))
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
        EndRow_agg <- ifelse(is.na(EndRow_agg), nrow(XL), EndRow_agg)
        
        # Not specifying which dose these parameters are for b/c we don't know.
        # Only pulling whatever AUC, Cmax, etc. are available.
        PKparameters <- unique(sub("_dose1|_ss", "", PKparameters))
        # Some parameters are not going to be present, so removing those. 
        PKparameters <- PKparameters[
            !PKparameters %in% c("fa_sub", "fa_inhib",
                                 "ka_sub", "ka_inhib",
                                 "tlag_sub", "tlag_inhib",
                                 "CL_hepatic", "CLpo",
                                 "F_sub", "F_inhib", "fg_sub", "fg_inhib",
                                 "fh_sub", "fh_inhib")]
        
        # sub function for finding correct column
        findCol <- function(PKparam){
            
            ToDetect <- switch(PKparam,
                               "AccumulationIndex" = "Accumulation Index$",
                               "AccumulationRatio" = "Accumulation Ratio$",
                               "AccumulationIndex_withInhib" = "Accumulation Index_Inh",
                               "AccumulationRatio_withInhib" = "Accumulation Ratio_Inh",
                               "AUCinf" = "^AUC_INF",
                               "AUCinf_withInhib" = "^AUC_INF",
                               "AUCinf_ratio" = "^AUC_INF ratio$",
                               "AUCtau" = "AUCt\\(n\\) \\(|^AUC \\(",
                               "AUCtau_withInhib" = "AUCt\\(n\\)_Inh|AUCinh \\(",
                               "AUCtau_ratio" = "AUC Ratio",
                               "CL" = "CL \\(Dose/AUC\\)",
                               "CL_withInhib" = "CL \\(Dose/AUC\\)|CLinh \\(Dose/AUC\\)",
                               "CL_ratio" = "CL Ratio",
                               "Cmax" = "CMax \\(",
                               "Cmax_withInhib" = "CMaxinh", 
                               "Cmax_ratio" = "CMax Ratio",
                               "Cmin" = "CMin \\(",
                               "Cmin_withInhib" = "CMininh \\(",
                               "Cmin_ratio" = "CMin Ratio",
                               "HalfLife" = "Half-life",
                               "tmax" = "TMax ", 
                               "tmax_withInhib" = "TMaxinh")
            
            if(is.null(ToDetect)){
                stop(paste("Extraction of the parameter", PKparam, "has not been set up correctly."))
            }
            
            OutCol <- which(str_detect(as.vector(t(
                XL[HeaderRow, ])), ToDetect) &
                    !str_detect(as.vector(t(XL[HeaderRow, ])), "%"))
            
            if(checkDataSource){
                assign("SearchText", value = ToDetect,
                       pos = 1)
            }
            
            return(OutCol)
        }
        # end of subfunction
        
        # finding the PK parameters requested
        for(i in PKparameters){
            ColNum <- findCol(i)
            if(length(ColNum) == 0){
                rm(ColNum)
                PKparameters <- PKparameters[!PKparameters == i]
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
            
            if(checkDataSource){
                DataCheck <- DataCheck %>%
                    bind_rows(data.frame(PKparam = i, Tab = sheet,
                                         SearchText = SearchText,
                                         Column = ColNum,
                                         StartRow_agg = StartRow_agg,
                                         EndRow_agg = EndRow_agg,
                                         StartRow_ind = HeaderRow+1,
                                         EndRow_ind = EndRow_ind,
                                         Note = "StartColText is looking in row 2."))
                
            }
            
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
    
    
    # Putting all data together ------------------------------------------
    # If user only wanted one parameter and includeTrialInfo was FALSE, make
    # the output a vector instead of a list
    if(length(Out_ind) == 1 & includeTrialInfo == FALSE){
        
        Out_ind <- Out_ind[[1]]
        
    } else {
        
        if(length(Out_ind) > 1){
            
            # Putting objects in alphabetical order
            Out_ind <- Out_ind[order(names(Out_ind))]
            
            if(includeTrialInfo & "individual" %in% returnAggregateOrIndiv){
                Out_ind <- Out_ind[names(Out_ind)[str_detect(names(Out_ind), "tab$")]]
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
                Out_ind <- Out_ind[names(Out_ind)[!str_detect(names(Out_ind), "tab$")]] %>%
                    as.data.frame()
            }
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
        
        Out_agg <- Out_agg[c("Statistic",
                             sort(setdiff(names(Out_agg), "Statistic")))]
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
        Out <- list("individual" = Out_ind,
                    "aggregate" = Out_agg)
    }
    
    if(checkDataSource){
        DataCheck <- DataCheck %>% filter(complete.cases(PKparam)) %>% 
            mutate(File = sim_data_file) %>% 
            select(PKparam, File, Tab, Column, StartRow_agg, EndRow_agg, 
                   StartRow_ind, EndRow_ind)
        
        if(class(Out)[1] == "list"){
            Out[["QC"]] <- unique(DataCheck)
        } else {
            # If Out is not a list, it is a single data.frame of whichever
            # the user wanted -- aggregate or individual information. Name
            # the items in Out accordingly.
            Out <- list(Out, DataCheck)
            names(Out) <- c(returnAggregateOrIndiv, "QC")
        }
    }
    
    return(Out)
}

