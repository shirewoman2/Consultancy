#' Extract PK data for specific parameters from a simulator output Excel file
#'
#' Pull calculated PK parameters from a Simcyp simulation output Excel file.
#' \strong{Note:} Nearly all parameters are for the SUBSTRATE We're still
#' validating this for extracting PK for an effector. \strong{A request for
#' assistance:} If you extract PK data for an effector by specifying an Excel
#' sheet for that compound, please check the values and tell Laura Shireman how
#' well it works! Also, absorption parameters are for first-order absorption
#' models only; we haven't developed this yet to pull ADAM-model absorption
#' parameters, but it's on our to-do list.
#'
#' @param sim_data_file name of the Excel file containing the simulator output,
#'   in quotes
#' @param sheet optionally specify the name of the sheet where you'd like to
#'   pull the PK data, in quotes; for example, specify the tab where you have a
#'   user-defined AUC integration. \emph{Note:} Unless you want a very specific
#'   Excel sheet that's not what the usual sheet name would be for a first or
#'   last dose, this function will work best if this is left as NA. Also, since
#'   we don't know which dose these data were for, you'll see that the output
#'   parameter names do not include the suffixes "_last" or "_dose1".
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file. Options are: \describe{
#'
#'   \item{"all"}{all possible parameters}
#'
#'   \item{"AUC tab"}{only those parameters on the "AUC" tab (default). The
#'   "AUC_CI" tab or "AUC_SD" tab will be used if "AUC" tab is not present.}
#'
#'   \item{"Absorption tab"}{only those parameters on the "Absorption" or
#'   "Overall Fa Fg" tab}
#'
#'   \item{"Regional ADAM"}{regional fraction absorbed and fraction metabolized
#'   from intestinal segments; only applies to ADAM models where the tab
#'   "Regional ADAM Fractions (Sub)" is included in the Excel file and currently
#'   only applies to substrate}
#'
#'   \item{a vector of any combination of specific, individual parameters, each
#'   surrounded by quotes and encapsulated with \code{c(...)}}{An example:
#'   \code{c("Cmax_dose1", "AUCtau_last")}. To see the full set of possible
#'   parameters to extract, enter \code{view(PKParameterDefinitions)} into the
#'   console. Not case sensitive. If you use "_first" instead of "_dose1", that
#'   will also work.}}
#'
#'   Currently, the PK data are only for the substrate unless noted, although
#'   you can sometimes hack around this by supplying a specific sheet to extract
#'   for a compound other than the substrate, e.g. sheet = "AUC(Sub Pri Met1)".
#'   This has NOT been as well tested, though, so be sure to check that you're
#'   getting what you expected!
#' @param compoundToExtract For which compound do you want to extract
#'   PK data? Options are: \itemize{\item{"substrate"
#'   (default),} \item{"primary metabolite 1",} \item{"primary metabolite 2",}
#'   \item{"secondary metabolite",} \item{"inhibitor 1" -- this can be an
#'   inducer, inhibitor, activator, or suppresesor, but it's labeled as
#'   "Inhibitor 1" in the simulator,} \item{"inhibitor 2" for the 2nd inhibitor
#'   listed in the simulation,} \item{"inhibitor 1 metabolite" for the primary
#'   metabolite of inhibitor 1}}
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default), "unbound plasma", "blood", or "unbound
#'   blood".
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#' @param returnAggregateOrIndiv return aggregate (default) and/or individual PK
#'   parameters? Options are "aggregate", "individual", or "both". For aggregate
#'   data, values are pulled from simulator output -- not calculated -- and the
#'   output will be a data.frame with the PK parameters in columns and the
#'   statistics reported exactly as in the simulator output file.
#' @param includeTrialInfo TRUE or FALSE (default) for whether to include which
#'   individual and trial the data describe. This only applies when
#'   \code{returnAggregateOrIndiv} is "individual" or "both".
#' @param returnExpDetails TRUE or FALSE: Since, behind the scenes, this
#'   function extracts experimental details from the "Summary" tab of the
#'   simulator output file, would you like those details to be returned here? If
#'   set to TRUE, output will be a list of 1. the requested PK data, 2. the
#'   experimental details provided on the Summary tab, and, if you have set
#'   checkDataSource to TRUE, 3. info for checking the data. (See
#'   "checkDataSource".)
#' @param checkDataSource TRUE (default) or FALSE for whether to include in the
#'   output a data.frame that lists exactly where the data were pulled from the
#'   simulator output file. Useful for QCing.
#'
#' @return Depending on the options selected, returns a list of numerical
#'   vectors or a list of data.frames: "individual" and "aggregate". If
#'   \code{checkDataSource} is TRUE, this will also return a data.frame named
#'   "QC" that indicates where in the simulator output file the data came from.
#'
#' @import tidyverse
#' @import readxl
#' @export
#' @examples
#'
#' sim_data_file <- "../Example simulator output MD + inhibitor.xlsx"
#' extractPK(sim_data_file)
#' extractPK(sim_data_file, PKparameters = "Absorption tab")
#' extractPK(sim_data_file, PKparameters = c("AUCinf_dose1", "Cmax_dose1"))
#'
#' 
extractPK <- function(sim_data_file,
                      PKparameters = "AUC tab",
                      sheet = NA,
                      compoundToExtract = "substrate",
                      tissue = "plasma",
                      existing_exp_details = NA, 
                      returnAggregateOrIndiv = "aggregate",
                      includeTrialInfo = TRUE,
                      returnExpDetails = FALSE, 
                      checkDataSource = TRUE){
    
    # Error catching ----------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    # If the user supplied "XXX_ss", change that to "XXX_last".
    PKparameters <- sub("_last", "_last", PKparameters)
    
    # If the user used "_first" instead of "_dose1", change that.
    PKparameters <- sub("_first", "_dose1", PKparameters)
    
    # If the user supplied "XXXtau_dose1", change that to "XXXt_dose1". 
    PKparameters <- sub("tau_dose1", "t_dose1", PKparameters)
    
    # If the user used AUCt_last instead of AUCtau_last, fix that for them.
    PKparameters <- sub("AUCt_last", "AUCtau_last", PKparameters)
    PKparameters <- sub("AUCt_ratio_last", "AUCtau_ratio_last", PKparameters)
    
    # If they didn't include ".xlsx" at the end, add that.
    sim_data_file <- ifelse(str_detect(sim_data_file, "xlsx$"), 
                            sim_data_file, paste0(sim_data_file, ".xlsx"))
    
    # Checking that the file is, indeed, a simulator output file.
    SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                           error = openxlsx::getSheetNames(sim_data_file))
    if(all(c("Input Sheet", "Summary") %in% SheetNames) == FALSE){
        # Using "warning" instead of "stop" here b/c I want this to be able to
        # pass through to other functions and just skip any files that
        # aren't simulator output.
        warning(paste("The file", sim_data_file,
                      "does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file."), 
                call. = FALSE)
        return(list())
    }
    
    if(complete.cases(sheet) & sheet %in% SheetNames == FALSE){
        warning(paste0("The sheet requested could not be found in the Excel file ",
                       sim_data_file, "."),
                call. = FALSE)
        return(list())
    }
    
    if(length(returnAggregateOrIndiv) > 2 | length(returnAggregateOrIndiv) < 1 |
       all(returnAggregateOrIndiv %in% c("aggregate", "both", "individual")) == FALSE){
        stop("Options for 'returnAggregateOrIndiv' are 'aggregate', 'individual', or 'both'.",
             call. = FALSE)
    }
    
    tissue <- tolower(tissue)
    if(tissue %in% c("plasma", "unbound plasma", "blood", "unbound blood") == FALSE){
        warning("You have not supplied a permissible value for tissue. Options are `plasma`, `unbound plasma`, `blood`, or `unbound blood`. The PK parameters will be for plasma.", 
                call. = FALSE)
        tissue <- "plasma"
    }
    
    
    # Main body of function ---------------------------------------------------
    
    # NOTE TO CODERS: This function calls on the data object AllPKParameters to
    # figure out which regex to use for which PK parameter. In AllPKParameters,
    # the sheet listed is "AUC" when it's a parameter that we're searching for
    # on a tab named "AUC", "AUC_SD", or "AUC_CI", which apparently only exists
    # with simulator versions earlier than V22 and even then does not *always*
    # exist. The "AUC" tab has a very specific format that DIFFERS from the more
    # generic PK tab layouts, so this matters! extractPK will call on the
    # unexported internal function extractAUCtab to get the data requested. 
    
    # By contrast, when the sheet listed is "AUC0", that's only for first-dose
    # data, and the structure of that Excel tab is the more generic layout of PK
    # data like in, e.g., "Int AUC 1st(Sub)(CPlasma)" or a similarly named tab.
    # Similarly, when the sheet is listed in AllPKParameters as "AUCX", that's a
    # tab that could be for a last dose or for a user-specified interval, but
    # its layout will be similar to the AUC0 layout, so extractPK will still
    # call on the internal function extractAUCXtab to get the requested info.
    
    # To add new PK parameters to extract, you should be able to just add them
    # to AllPKParameters, following the examples for other PK parameters as far
    # as what to put in each column. This is TRICKY for the AUC tab b/c you have
    # to check things in multiple rows to make sure you're extracting the
    # correct cells. Once you've added the new parameters to AllPKParameters,
    # save that RData object when you save the package, and then extractPK
    # *should* be able to find them. At least, I *hope* it will work that
    # simply! -LSh
    
    
    if(returnAggregateOrIndiv[1] == "both"){
        returnAggregateOrIndiv <- c("aggregate", "individual")
    }
    
    # Checking experimental details to only pull details that apply
    if(class(existing_exp_details) == "logical"){ # logical when user has supplied NA
        Deets <- extractExpDetails(sim_data_file = sim_data_file, 
                                   exp_details = "Summary tab")
    } else {
        Deets <- switch(as.character("File" %in% names(existing_exp_details)), 
                        "TRUE" = existing_exp_details, 
                        "FALSE" = deannotateDetails(existing_exp_details))
        
        if("data.frame" %in% class(Deets)){
            Deets <- Deets %>% filter(File == sim_data_file)
            
            if(nrow(Deets == 0)){
                Deets <- extractExpDetails(sim_data_file = sim_data_file, 
                                           exp_details = "Summary tab")
            }
        }
    }
    
    # We need to know the dosing regimen for whatever compound they
    # requested, but, if the compoundID is inhibitor 2, then that's listed
    # on the input tab, and we'll need to extract exp details for that, too.
    if("inhibitor 2" %in% compoundToExtract){
        DeetsInputSheet <- extractExpDetails(sim_data_file = i, 
                                             exp_details = "Input Sheet")
        Deets <- c(as.list(Deets), DeetsInputSheet)
    }
    
    # extractExpDetails will check whether the Excel file provided was, in
    # fact, a Simulator output file and return a list of length 0 if not.
    # Checking for that here.
    if(length(Deets) == 0){
        # warning(paste0("The file ", sim_data_file, 
        #                " is not a Simulator output file and will be skipped."))
        return(list())
    }
    
    # version <= 20: Has "AUC" tab for each compound that was integrated. Sheet
    # names are harder to decipher: AUC0 is 1st dose, but last dose will be on
    # whichever AUC tab has the highest number for that compoundID. AUC tab for
    # compounds than substrate will be labeled as, e.g., "AUC(Inh 1)". AUCinf is
    # ONLY found on AUC tab.
    
    # version == 21: Has "AUC" tab. Sheet names are "1st" or "last" for 1st or
    # last dose, respectively. AUC tab for compounds than substrate will be
    # labeled as, e.g., "AUC(Inh 1)". AUCinf is ONLY found on AUC tab.
    
    # verison >= 22: No AUC tab. Sheet names are "1st" or "last" for 1st or
    # last dose, respectively. 
    
    Tab_AUC <- switch(
        compoundToExtract, 
        "substrate" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?$")][1],
        "primary metabolite 1" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Sub Pri Met1\\)$")][1],
        "primary metabolite 2" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Sub Pri Met2\\)$")][1],
        "secondary metabolite" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Sub Sec Met\\)$")][1],
        "inhibitor 1" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Inh 1\\)$")][1],
        "inhibitor 2" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Inh 2\\)$")][1],
        "inhibitor 1 metabolite" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Inh 1 Met\\)$")][1])
    
    if(Deets$Species != "human" & compoundToExtract == "substrate"){
        # If it's from Simcyp Discovery, there's only one AUC sheet and
        # it's either for the only dose in a SD sim or the last dose for
        # a MD sim. Not sure about the regular Simcyp Animal, though. 
        Tab_AUC <- SheetNames[which(str_detect(SheetNames, "AUC"))]
    }
    
    SimV21plus <- as.numeric(str_extract(Deets$SimulatorVersion, "[0-9]{2}")) >= 21
    
    if(SimV21plus){
        Tab_first <- SheetNames[
            str_detect(SheetNames, 
                       paste0("Int AUC 1st", 
                              switch(compoundToExtract,
                                     "substrate" = "\\(Sub\\)", 
                                     "primary metabolite 1" = "\\(Sub Met\\)",
                                     "primary metabolite 2" = "\\(Sub Met2\\)",
                                     "secondary metabolite" = "\\(Sub SM\\)", 
                                     "inhibitor 1" = "\\(Inh 1\\)",
                                     "inhibitor 1 metabolite" = "\\(Inh1 M(et)?\\)", 
                                     "inhibitor 2" = "\\(Inh 2\\)"), 
                              switch(tissue, 
                                     "plasma" = "\\(CPl.*?\\)", # some sheet names have ellipses, e.g., "Int AUC 1st_SD(Sub Met)(CPl...)"
                                     "unbound plasma" = "\\(CuPlasma",
                                     "blood" = "\\(CBlood", 
                                     "unbound blood" = "\\(CuBlood")))][1]
        
        Tab_last <- SheetNames[
            str_detect(SheetNames, 
                       paste0("Int AUC last", 
                              switch(compoundToExtract,
                                     "substrate" = "\\(Sub\\)", 
                                     "primary metabolite 1" = "\\(Sub Met\\)",
                                     "primary metabolite 2" = "\\(Sub Met2\\)",
                                     "secondary metabolite" = "\\(Sub SM\\)", 
                                     "inhibitor 1" = "\\(Inh 1\\)",
                                     "inhibitor 1 metabolite" = "\\(Inh1 M(et)?\\)", 
                                     "inhibitor 2" = "\\(Inh 2\\)"), 
                              switch(tissue, 
                                     "plasma" = "\\(CPl.*?\\)", # some sheet names have ellipses, e.g., "Int AUC 1st_SD(Sub Met)(CPl...)"
                                     "unbound plasma" = "\\(CuPlasma",
                                     "blood" = "\\(CBlood", 
                                     "unbound blood" = "\\(CuBlood")))][1]
        
    } else {
        
        Tab_first <- SheetNames[
            str_detect(SheetNames, 
                       paste0("AUC0(_SD|_CI)?", 
                              switch(compoundToExtract,
                                     "substrate" = "\\(Sub\\)", 
                                     "primary metabolite 1" = "\\(Sub Met\\)",
                                     "primary metabolite 2" = "\\(Sub Met2\\)",
                                     "secondary metabolite" = "\\(Sub SM\\)", 
                                     "inhibitor 1" = "\\(Inh 1\\)",
                                     "inhibitor 1 metabolite" = "\\(Inh1 M(et)?\\)", 
                                     "inhibitor 2" = "\\(Inh 2\\)"), 
                              switch(tissue, 
                                     "plasma" = "\\(CPl.*?\\)", # some sheet names have ellipses, e.g., "Int AUC 1st_SD(Sub Met)(CPl...)"
                                     "unbound plasma" = "\\(CuPlasma",
                                     "blood" = "\\(CBlood", 
                                     "unbound blood" = "\\(CuBlood")))][1]
        
        # Determining the name of the tab that contains PK data for the last
        # dose. This will create a vector of ALL the AUC tabs for that tissue
        # and compound and we'll narrow down to the exact one we want next.
        Tab_last_check <- SheetNames[
            str_detect(SheetNames, 
                       paste0("AUC(t)?[1-9]{1,}(_SD|_CI)?", 
                              switch(compoundToExtract,
                                     "substrate" = "\\(Sub\\)", 
                                     "primary metabolite 1" = "\\(Sub Met\\)",
                                     "primary metabolite 2" = "\\(Sub Met2\\)",
                                     "secondary metabolite" = "\\(Sub SM\\)", 
                                     "inhibitor 1" = "\\(Inh 1\\)",
                                     "inhibitor 1 metabolite" = "\\(Inh1 M(et)?\\)", 
                                     "inhibitor 2" = "\\(Inh 2\\)"), 
                              switch(tissue, 
                                     "plasma" = "\\(CPl.*?\\)", # some sheet names have ellipses, e.g., "Int AUC 1st_SD(Sub Met)(CPl...)"
                                     "unbound plasma" = "\\(CuPlasma",
                                     "blood" = "\\(CBlood", 
                                     "unbound blood" = "\\(CuBlood")))]
        
        if(length(Tab_last_check) > 0){
            LastDoseNum <- data.frame(Tab_last = Tab_last_check) %>% 
                mutate(DoseNum = as.numeric(str_extract(Tab_last, "[0-9]{1,}"))) %>% 
                # It's the highest dose number and it can't be 0 b/c that's dose 1.
                filter(DoseNum == max(DoseNum) & DoseNum != 0)
            Tab_last <- LastDoseNum$Tab_last
        } else {
            # If there was no Tab_last found but there *is* a tab with "t0" in
            # the name, e.g., AUCt0(Sub)(CPlasma), then use that one.
            Tab_last <- Tab_last_check[str_detect(Tab_last_check, "t0")]
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
        # Checking formatting of the user-defined sheet b/c it's sometimes set up
        # the same way as the AUC tab and thus requires special fiddling.
        XL <- suppressMessages(
            readxl::read_excel(path = sim_data_file, sheet = sheet,
                               col_names = FALSE))
        if(which(XL$...1 == "Index")[1] == 3){
            # This is when the formatting is like the AUC tab. Instead of rewriting
            # the user-specified sheet section, I'm hacking this to make the
            # function think that this should be the AUC tab.
            UserAUC <- TRUE # A handle for checking whether to use XL instead of the regular AUC tab.
            
        } else {
            UserAUC <- FALSE
        }
        
    } else {
        UserAUC <- FALSE
    }
    
    ParamAUC <- AllPKParameters %>% filter(Sheet == "AUC") %>% 
        pull(PKparameter) %>% unique()
    
    ParamAbsorption <- AllPKParameters %>% filter(Sheet %in% c("Absorption",
                                                               "Overall Fa Fg")) %>% 
        pull(PKparameter) %>% unique()
    
    ParamRegADAM <- AllPKParameters %>% filter(Sheet %in% c("Regional ADAM Fractions (Sub)")) %>% 
        pull(PKparameter) %>% unique()
    
    ParamAUC0 <- AllPKParameters %>% filter(Sheet == "AUC0") %>% 
        pull(PKparameter) %>% unique()
    
    ParamAUClast <- AllPKParameters %>% filter(Sheet == "AUCX") %>% 
        pull(PKparameter) %>% unique()
    
    ParamCLTSS <- AllPKParameters %>% filter(Sheet == "Clearance Trials SS") %>% 
        pull(PKparameter) %>% unique()
    
    PKparameters <- 
        switch(tolower(PKparameters[1]), 
               "all" = unique(c(ParamAbsorption, ParamAUC, ParamAUC0,
                                ParamAUClast, ParamCLTSS)), 
               "auc tab" = unique(c(ParamAUC, ParamAUC0, ParamAUClast)),
               "absorption tab" = ParamAbsorption,
               "auc0" = ParamAUC0, # This will happen if user requests PKparameters = "AUC" but "AUC" tab is not present but a tab for AUC0 *is*.
               "regional adam" = ParamRegADAM)
    
    # Checking whether the user had supplied a vector of specific parameters
    # rather than a parameter set name and using those if so.
    if(is.null(PKparameters) & any(complete.cases(PKparameters_orig))){
        PKparameters <- PKparameters_orig
    }
    
    # Allowing for flexibility in case: Get the lower-case version of whatever
    # PKparameters user specified and match them to the correct PKparameters in
    # AllPKParameters.
    PKparameters <- AllPKParameters %>%
        mutate(PKparameter_lower = tolower(PKparameter)) %>% 
        filter(PKparameter_lower %in% tolower(PKparameters)) %>% 
        pull(PKparameter) %>% unique()
    
    MissingPKParam <- setdiff(PKparameters, AllPKParameters$PKparameter)
    if(length(MissingPKParam) > 0){
        warning(paste0("The parameter(s) ", str_comma(MissingPKParam),
                       " is/are not among the possible PK parameters and will not be extracted. Please see data(PKParameterDefinitions) for all possible parameters."),
                call. = FALSE)
    }
    
    # Filtering out irrelevant PK
    if(Deets$PopRepSim == "Yes"){
        warning(paste0("The simulator file supplied, `", 
                       sim_data_file, 
                       "`, is for a population-representative simulation and thus doesn't have any aggregate data. This function only really works with aggregate data, so this file will be skipped."),
                call. = FALSE)
        return(list())
    }
    
    if(is.na(Deets$Inhibitor1)){
        PKparameters <- 
            PKparameters[PKparameters %in% 
                             AllPKParameters$PKparameter[AllPKParameters$AppliesOnlyWhenEffectorPresent == FALSE]]
    }
    
    if((compoundToExtract %in% c("substrate", "primary metabolite 1", 
                          "primary metabolite 2", "secondary metabolite") &
        Deets$Regimen_sub == "Single Dose") |
       (complete.cases(Deets$Inhibitor1) && 
        compoundToExtract %in% c("inhibitor 1", "inhibitor 1 metabolite")
        && Deets$Regimen_inhib == "Single Dose") |
       (complete.cases(Deets$Inhibitor2) &&
        compoundToExtract %in% c("inhibitor 2") && 
        Deets$Regimen_inhib2 == "Single Dose")){
        
        PKparameters <- 
            PKparameters[PKparameters %in% 
                             AllPKParameters$PKparameter[
                                 AllPKParameters$AppliesToSingleDose == TRUE]]
    }
    
    # If it was a multiple-dose regimen, then the AUC tab will not include
    # certain parameters that WILL be able to be pulled from the AUC0 tab. NOTE:
    # I am NOT removing "AUCinf_ratio_dose1" from this list b/c it is not
    # available when the regimen is MD (at least, nowhere I've found in the
    # output). By NOT removing it, there will be a warning to the user that that
    # parameter was not found. Also, I'm removing some parameters that are not
    # completely clearly and unequivocably labeled so that they can be pulled
    # from sheets where they *are* so labeled. 
    if((compoundToExtract %in% c("substrate", "primary metabolite 1", 
                          "primary metabolite 2", "secondary metabolite") &
        Deets$Regimen_sub == "Multiple Dose") |
       (complete.cases(Deets$Inhibitor1) && 
        compoundToExtract %in% c("inhibitor 1", "inhibitor 1 metabolite")
        && Deets$Regimen_inhib == "Multiple Dose") |
       (complete.cases(Deets$Inhibitor2) &&
        compoundToExtract %in% c("inhibitor 2") && 
        Deets$Regimen_inhib2 == "Multiple Dose")){
        
        ParamAUC <- setdiff(ParamAUC,
                            c("AUCt_ratio_dose1", "AUCt_dose1", 
                              "AUCt_dose1_withInhib",
                              "Cmax_dose1", "Cmax_dose1_withInhib",
                              "Cmax_ratio_dose1", "tmax_dose1"))
    }
    
    if(length(PKparameters) == 0){
        warning("There are no possible PK parameters to be extracted. Please check your input for 'PKparameters'. For example, check that you have not requested steady-state parameters for a single-dose simulation.",
                call. = FALSE)
        return(list())
    }
    
    # For the special cases when the user specified a sheet and did not leave
    # PKparameters as NA or as the default "AUC tab", then let's only return the
    # parameters that they asked for. 
    if(complete.cases(sheet) & complete.cases(PKparameters_orig[1]) &&
       !PKparameters_orig[1] %in% c("all", "AUC tab")){
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
    
    
    # Pulling data from "AUC" sheet ------------------------------------------
    
    # Need to pull these parameters if either a) they requested a vector of
    # parameters rather than a set and some of those parameters are present on
    # the AUC tab or b) the user requested the "AUC tab" for PK parameters and
    # either "AUC", "AUC_CI", or "AUC_SD" are among the sheets in the file.
    if(length(Tab_AUC) > 0 && complete.cases(Tab_AUC)){
        
        PKparameters_AUC <- intersect(PKparameters, ParamAUC)
        
        if(UserAUC){ 
            AUC_xl <- XL
        } else {
            AUC_xl <- suppressMessages(
                readxl::read_excel(path = sim_data_file, 
                                   # If the user requested the "AUC" tab for PK
                                   # parameters, it's ok to use the tab "AUC_CI"
                                   # if "AUC" is not present.
                                   sheet = Tab_AUC,
                                   col_names = FALSE))
        }
        
        # Finding the last row of the individual data
        EndRow_ind <- which(AUC_xl$...2 == "Statistics")
        
        if(length(EndRow_ind) == 0){
            # Using "warning" instead of "stop" here b/c I want this to be
            # able to pass through to other functions.
            warning(paste0("It appears that you don't have any aggregate data in your simulator output file ",
                           sim_data_file, "; was this a population-representative simulation? This function only really works well when there are aggregate data present, so this file will be skipped."),
                    call. = FALSE)
            
            return(list())
            
        } 
        
        EndRow_ind <- max(which(complete.cases(AUC_xl$...2[1:(EndRow_ind-1)])))
        
        # REMOVE the columns for the un-requested tissues ENTIRELY. I think this
        # will be easier to code. -LSh
        ColStart <- c("plasma" = which(str_detect(t(AUC_xl[1, ]), "CPlasma"))[1], 
                      "unbound plasma" = which(str_detect(t(AUC_xl[1, ]), "CuPlasma"))[1], 
                      "blood" = which(str_detect(t(AUC_xl[1, ]), "CBlood"))[1], 
                      "unbound blood" = which(str_detect(t(AUC_xl[1, ]), "CuBlood"))[1])
        ColStart <- sort(ColStart)
        if(is.na(ColStart[tissue])){
            # Using "warning" instead of "stop" here b/c I want this to be
            # able to pass through to other functions.
            warning(paste0("You requested PK parameters for ", 
                           tissue, ", but that does not appear to be included in your output, so no PK data can be returned.", 
                           call. = FALSE))
            return(list())
        }
        
        if(length(ColStart) > 1){
            ColEnd <- ColStart
            ColEnd[1:(length(ColStart)-1)] <- as.numeric(ColStart[2:length(ColStart)] - 1)
            ColEnd[length(ColStart)] <- which(is.na(t(AUC_xl[3,])))[1] - 1
        } else {
            ColEnd <- which(is.na(t(AUC_xl[3,])))[1] - 1
            names(ColEnd) <- tissue
        }
        
        AUC_xl <- AUC_xl[, c(1, 2, ColStart[tissue]:ColEnd[tissue])]
        
        # Finding the aggregate data rows 
        StartRow_agg <- which(AUC_xl$...2 == "Statistics") + 2
        EndRow_agg <- which(is.na(AUC_xl$...2))
        EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1
        EndRow_agg <- ifelse(is.na(EndRow_agg), nrow(AUC_xl), EndRow_agg)
        
        # Looping through parameters and extracting values
        for(i in PKparameters_AUC){
            
            # Using regex to find the correct column. See
            # data(AllPKParameters) for all the possible parameters as well
            # as what regular expressions are being searched for each. For
            # the AUC tab specifically, you also have to make sure that
            # you're looking under the correct subheading, so that's what
            # the column in "AllPKParameters" called "AUC_StartColText" is
            # looking for.
            ToDetect <- AllPKParameters %>% 
                filter(Sheet == "AUC" & PKparameter == i) %>% 
                select(PKparameter, SearchText, AUCtab_StartColText)
            
            if(Deets$Species != "human" & i == "HalfLife_dose1"){
                ToDetect <- data.frame(PKparameter = "HalfLife_dose1", 
                                       SearchText = "t 1/2 ", 
                                       AUCtab_StartColText = "^AUC.*integrated from")
            }
            
            # Figuring out which rows to search for which text
            IndexRow <- which(AUC_xl$...1 == "Index")
            
            # Looking for the correct subheading 
            StartCol <- which(str_detect(as.vector(t(
                AUC_xl[IndexRow - 1, ])), 
                ToDetect$AUCtab_StartColText))[1]
            
            if(length(StartCol) == 0){
                StartCol <- 1
            }
            
            # Find the last column for this particular subheading
            EndCol <- which(complete.cases(as.vector(t(
                AUC_xl[IndexRow - 1, ]))))
            EndCol <- EndCol[EndCol > StartCol][1] - 1
            EndCol <- ifelse(is.na(EndCol), ncol(AUC_xl), EndCol)
            
            if(is.na(StartCol)){
                # If the subheading can't be found, then this parameter
                # isn't on the AUC tab (or, at least, we can't currently
                # find it). Removing that parameter from the parameters to
                # extract from the AUC tab.
                PKparameters_AUC <- PKparameters_AUC[!PKparameters_AUC == i]
                ColNum <- NA
            } else {
                
                # If the subheading CAN be found, look for the regular
                # expression specific to this parameter i. Since there are
                # often more than one column with the same title, we only
                # want the 1st one. (The second would be for the wrong
                # tissue, e.g., blood when user asked for plasma.)
                PossCol <- StartCol:EndCol
                ColNum <- PossCol[
                    which(str_detect(as.vector(t(
                        AUC_xl[IndexRow, PossCol])),
                        ToDetect$SearchText) &
                            !str_detect(as.vector(t(AUC_xl[3, PossCol])), "%")) ][1]
                
            }
            
            if(length(ColNum) == 0 | is.na(ColNum)){
                if(any(PKparameters_orig %in% c("all", "AUC tab")) == FALSE){
                    warning(paste0("The column with information for ", i,
                                   " on the tab 'AUC' cannot be found in the file ", 
                                   sim_data_file, "."), 
                            call. = FALSE)
                }
                suppressWarnings(suppressMessages(rm(ToDetect, StartCol, EndCol, PossCol, ColNum)))
                PKparameters_AUC <- setdiff(PKparameters_AUC, i)
                next
            }
            
            suppressWarnings(
                Out_ind[[i]] <- AUC_xl[(IndexRow + 1):EndRow_ind, ColNum] %>%
                    pull(1) %>% as.numeric
            )
            
            DataCheck <- DataCheck %>%
                bind_rows(data.frame(PKparam = i, 
                                     Tab = Tab_AUC,
                                     StartColText = ToDetect$AUCtab_StartColText,
                                     SearchText = ToDetect$SearchText,
                                     Column = ColNum - 2 + # "-2" accounts for index and trial columns
                                         ColStart[tissue] - 1, # "-1" accounts for the 1st column being 1 and not 0
                                     StartRow_agg = StartRow_agg,
                                     EndRow_agg = EndRow_agg,
                                     StartRow_ind = IndexRow + 1,
                                     EndRow_ind = EndRow_ind,
                                     Note = paste("StartColText is looking in row", IndexRow - 1)))
            
            if(any(is.na(Out_ind[[i]]) & str_detect(i, "inf"))){
                # Simulator sometimes can't extrapolate to infinity well and
                # you end up with NA values. If this happens, then AUCinf is
                # NOT reliable and we SHOULD NOT use aggregated measures of
                # it b/c they don't include all the data! Instead, pull
                # AUCtau as well and give user a warning.
                NewParam <- ifelse(str_detect(i, "dose1"), 
                                   sub("inf", "t", i), sub("inf", "tau", i))
                warning(paste0("For the file ", sim_data_file, 
                               ", the parameter ", i, 
                               " included some NA values, meaning that the Simulator had trouble extrapolating to infinity. No aggregate data will be returned for this parameter, and the parameter ", 
                               NewParam, " will be returned to use in place of ",
                               i, " as you deem appropriate."),
                        call. = FALSE)
                
                PKparameters_AUC <- unique(c(PKparameters_AUC, NewParam))
                
                suppressWarnings(rm(StartCol, EndCol, ColNum, ToDetect))
                
                ToDetect <- AllPKParameters %>% 
                    filter(Sheet == "AUC" & PKparameter == NewParam) %>% 
                    select(PKparameter, SearchText, AUCtab_StartColText)
                
                # !!! STARTING HERE, ALL TEXT IS SAME AS MAIN CODE ABOVE.
                
                # Looking for the correct subheading 
                StartCol <- which(str_detect(as.vector(t(
                    AUC_xl[IndexRow - 1, ])), 
                    ToDetect$AUCtab_StartColText))[1]
                
                if(length(StartCol) == 0){
                    StartCol <- 1
                }
                
                # Find the last column for this particular subheading
                EndCol <- which(complete.cases(as.vector(t(
                    AUC_xl[IndexRow - 1, ]))))
                EndCol <- EndCol[EndCol > StartCol][1] - 1
                EndCol <- ifelse(is.na(EndCol), ncol(AUC_xl), EndCol)
                
                if(is.na(StartCol)){
                    # If the subheading can't be found, then this parameter
                    # isn't on the AUC tab (or, at least, we can't currently
                    # find it). Removing that parameter from the parameters to
                    # extract from the AUC tab.
                    PKparameters_AUC <- PKparameters_AUC[!PKparameters_AUC == i]
                    ColNum <- NA
                } else {
                    
                    # If the subheading CAN be found, look for the regular
                    # expression specific to this parameter i. Since there are
                    # often more than one column with the same title, we only
                    # want the 1st one. (The second would be for the wrong
                    # tissue, e.g., blood when user asked for plasma.)
                    PossCol <- StartCol:EndCol
                    ColNum <- PossCol[
                        which(str_detect(as.vector(t(
                            AUC_xl[IndexRow, PossCol])),
                            ToDetect$SearchText) &
                                !str_detect(as.vector(t(AUC_xl[3, PossCol])), "%")) ][1]
                    
                }
                
                if(length(ColNum) == 0 | is.na(ColNum)){
                    if(PKparameters_orig %in% c("all", "AUC tab") == FALSE){
                        warning(paste0("The column with information for ", i,
                                       " on the tab 'AUC' cannot be found in the file ", 
                                       sim_data_file, "."), 
                                call. = FALSE)
                    }
                    suppressWarnings(suppressMessages(rm(ToDetect, StartCol, EndCol, PossCol, ColNum)))
                    PKparameters_AUC <- setdiff(PKparameters_AUC, i)
                    next
                }
                
                # !!! ENDING HERE FOR TEXT BEING SAME AS MAIN CODE. TEXT BELOW HERE
                # IS NO LONGER SAME AS MAIN CODE ABOVE.
                suppressWarnings(
                    Out_ind[[NewParam]] <- AUC_xl[(IndexRow+1):EndRow_ind, ColNum] %>%
                        pull(1) %>% as.numeric
                )
                
                suppressWarnings(
                    Out_agg[[NewParam]] <- AUC_xl[StartRow_agg:EndRow_agg,
                                                  ColNum] %>%
                        pull(1) %>% as.numeric()
                )
                names(Out_agg[[NewParam]]) <- AUC_xl[StartRow_agg:EndRow_agg, 2] %>%
                    pull(1)
                
                DataCheck <- DataCheck %>%
                    bind_rows(data.frame(PKparam = NewParam, 
                                         Tab = Tab_AUC,
                                         StartColText = ToDetect$AUCtab_StartColText,
                                         SearchText = ToDetect$SearchText,
                                         Column = ColNum - 2 + # "-2" accounts for index and trial columns
                                             ColStart[tissue] - 1, # "-1" accounts for the 1st column being 1 and not 0
                                         StartRow_agg = StartRow_agg,
                                         EndRow_agg = EndRow_agg,
                                         StartRow_ind = IndexRow + 1,
                                         EndRow_ind = EndRow_ind,
                                         Note = paste("StartColText is looking in row", IndexRow - 1)))
                
                suppressWarnings(rm(ToDetect, StartCol, EndCol, ColNum, PossCol))
                
            } else {
                
                # At this point in the code is where we're back to the
                # scenario where there were no NA values for extrapolating
                # to infinity.
                
                suppressWarnings(
                    Out_agg[[i]] <- AUC_xl[StartRow_agg:EndRow_agg, ColNum] %>%
                        pull(1) %>% as.numeric()
                )
                names(Out_agg[[i]]) <- AUC_xl[StartRow_agg:EndRow_agg, 2] %>%
                    pull(1)
            }
            
            suppressWarnings(rm(ToDetect, StartCol, EndCol, ColNum, PossCol))
        }
        
        if(includeTrialInfo & length(PKparameters_AUC) > 0){
            # Subject and trial info
            SubjTrial_AUC <- AUC_xl[(IndexRow + 1):EndRow_ind, 1:2] %>%
                rename("Individual" = ...1, "Trial" = ...2)
            
            Out_ind[["AUCtab"]] <- cbind(SubjTrial_AUC,
                                         as.data.frame(Out_ind[PKparameters_AUC]))
        }
        
    }
    
    # Pulling dose 1 data NOT present on AUC tab ------------------------------
    PKparameters_AUC0 <- intersect(PKparameters, ParamAUC0)
    
    # Some PK parameters show up on multiple sheets. No need to pull
    # those here if they've already been pulled from another sheet.
    PKparameters_AUC0 <- setdiff(PKparameters_AUC0, names(Out_agg))
    
    if(complete.cases(sheet)){
        # How do you set something to have length 0? Hacking it for now.
        PKparameters_AUC0 <- intersect("A", "B")
    }
    
    if(length(PKparameters_AUC0) > 0 & complete.cases(Tab_first) &
       (PKparameters_orig[1] %in% c("AUC tab", "Absorption tab") == FALSE |
        PKparameters_orig[1] == "AUC tab" & "AUC" %in% SheetNames == FALSE |
        PKparameters_orig[1] == "AUC tab" & compoundToExtract != "substrate")){
        # Error catching
        # if(any(str_detect(SheetNames, "AUC0(_CI)?\\(Sub\\)\\(CPlasma\\)|Int AUC 1st\\(Sub\\)\\(CPlasma\\)")) == FALSE){
        #     # IMPORTANT: The tab labelled "AUCt0(blah blah blah)" (note the "t")
        #     # is actually NOT for the 1st dose but for the 1st user-defined
        #     # interval! Do NOT use that tab unless/until we do something further
        #     # to check which dosing interval the data are for, e.g., read the
        #     # top line that says "something something integrated from time A to
        #     # time B".
        #     
        #     warning(paste0("A sheet with a name like 'AUC0(Sub)(CPlasma)' or 'Int AUC 1st(Sub)(CPlasma)' must be present in the Excel simulated data file to extract the PK parameters ",
        #                    sub("and", "or", str_comma(PKparameters_AUC0)),
        #                    ". None of these parameters can be extracted."),
        #             call. = FALSE)
        # } else {
        
        Out_AUC0 <- extractAUCXtab(PKparameters = PKparameters_AUC0, 
                                   PKparameters_orig = PKparameters_orig,
                                   sim_data_file = sim_data_file,
                                   Sheet = Tab_first, 
                                   PKset = "AUC0",
                                   UserSpecified = FALSE,
                                   Deets = Deets, 
                                   DataCheck = DataCheck,
                                   includeTrialInfo = includeTrialInfo)
        
        DataCheck <- DataCheck %>% bind_rows(Out_AUC0$DataCheck)
        Out_agg <- c(Out_agg, Out_AUC0$Out_agg)
        Out_ind <- c(Out_ind, Out_AUC0$Out_ind)
        
    }
    
    
    # Pulling last-dose data NOT present on AUC tab ------------------------------
    PKparameters_AUClast <- intersect(PKparameters, ParamAUClast)
    
    # Some PK parameters show up on multiple sheets. No need to pull
    # those here if they've already been pulled from another sheet.
    PKparameters_AUClast <- setdiff(PKparameters_AUClast, names(Out_agg))
    
    if(complete.cases(sheet)){
        # How do you set something to have length 0? Hacking it for now.
        PKparameters_AUClast <- intersect("A", "B")
    }
    
    if(length(PKparameters_AUClast) > 0 && complete.cases(Tab_last) &
       PKparameters_orig[1] %in% c("Regional ADAM", "Absorption tab") == FALSE){
        
        # # Error catching
        # if(length(Tab_last) == 0 | is.na(Tab_last)){
        #     warning(paste0("The sheet 'AUCX(Sub)(CPlasma)', where 'X' is the tab for the last dose administered and is not dose 1, must be present in the Excel simulated data file to extract the PK parameters ",
        #                    str_c(PKparameters_AUClast, collapse = ", "),
        #                    ". None of these parameters can be extracted."),
        #             call. = FALSE)
        # } else {
        
        Out_AUClast <- extractAUCXtab(PKparameters = PKparameters_AUClast, 
                                      PKparameters_orig = PKparameters_orig,
                                      sim_data_file = sim_data_file,
                                      Sheet = Tab_last, 
                                      PKset = "AUClast",
                                      UserSpecified = FALSE,
                                      Deets = Deets, 
                                      DataCheck = DataCheck,
                                      includeTrialInfo = includeTrialInfo)
        
        DataCheck <- DataCheck %>% bind_rows(Out_AUClast$DataCheck)
        Out_agg <- c(Out_agg, Out_AUClast$Out_agg)
        Out_ind <- c(Out_ind, Out_AUClast$Out_ind)
        
    }
    
    # Pulling parameters from a user-specified sheet --------------------------
    if(complete.cases(sheet) & UserAUC == FALSE && 
       sheet %in% SheetNames){
        
        # Not specifying which dose these parameters are for b/c we don't know.
        # Only pulling whatever AUC, Cmax, etc. are available.
        PKparameters <- unique(sub("_dose1|_last", "", PKparameters))
        # Some parameters are not going to be present, so removing those. 
        PKparameters <- PKparameters[
            !PKparameters %in% c("fa_sub", "fa_inhib",
                                 "ka_sub", "ka_inhib",
                                 "tlag_sub", "tlag_inhib",
                                 "CLinf", "CLinf_withInhib",
                                 "CL_hepatic", "CLpo",
                                 "F_sub", "F_inhib", "fg_sub", "fg_inhib",
                                 "fh_sub", "fh_inhib")]
        
        Out_AUCX <- extractAUCXtab(PKparameters = PKparameters, 
                                   PKparameters_orig = PKparameters_orig,
                                   sim_data_file = sim_data_file,
                                   Sheet = sheet, 
                                   PKset = "AUCX",
                                   UserSpecified = TRUE,
                                   Deets = Deets, 
                                   DataCheck = DataCheck,
                                   includeTrialInfo = includeTrialInfo)
        
        DataCheck <- DataCheck %>% bind_rows(Out_AUCX$DataCheck)
        Out_agg <- c(Out_agg, Out_AUCX$Out_agg)
        Out_ind <- c(Out_ind, Out_AUCX$Out_ind)
        
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
        if(any(c("Absorption", "Overall Fa Fg") %in% SheetNames) == FALSE){
            warning(paste0("A sheet called `Absorption` or `Overall Fa Fg` must be present in the Excel simulated data file to extract the PK parameters ",
                           str_c(PKparameters_Abs, collapse = ", "),
                           ". None of these parameters can be extracted."),
                    call. = FALSE)
        } else if(Deets$Species != "human"){
            warning("You have requested information from the Absorption tab from an animal simulation; we apologize, but we have not set up this function for animal data extraction from the Absorption tab yet.", 
                    call. = FALSE)
        } else {
            
            if("Overall Fa Fg" %in% SheetNames){
                
                PKparameters_Abs_ADAM <- intersect(PKparameters_Abs, 
                                                   c("fa_sub", "Fg_sub", "fa_apparent_sub"))
                
                FaFg_xl <- suppressMessages(
                    readxl::read_excel(path = sim_data_file, sheet = "Overall Fa Fg",
                                       col_names = FALSE))
                
                SubCols <- 3:5
                # This is IN THE PRESENCE OF AN EFFECTOR -- not the effector
                # itself. I haven't set that up yet. 
                WithInhibCols <- 6:10
                
                StartRow_agg <- 3
                EndRow_agg <- which(is.na(FaFg_xl$...1[3:nrow(FaFg_xl)]))[1] + 1
                StartRow_ind <- which(FaFg_xl$...1 == "Index")[1] + 1
                
                # Looping through parameters and extracting values
                for(i in PKparameters_Abs_ADAM){
                    
                    # Using regex to find the correct column. See
                    # data(AllPKParameters) for all the possible parameters as well
                    # as what regular expressions are being searched for each. 
                    ToDetect <- AllPKParameters %>% 
                        filter(Sheet == "Overall Fa Fg" & PKparameter == i) %>% 
                        select(PKparameter, SearchText)
                    
                    # Looking for the regular expression specific to this parameter
                    # i. For the absorption tab, there are columns for the substrate
                    # and columns for Inhibitor 1. (There are also columns for
                    # Inhibitor 2 and 3 but I've never seen them filled in. -LSh)
                    StartCol <- ifelse(str_detect(i, "sub"),
                                       SubCols, WithInhibCols)
                    
                    ColNum <- which(str_detect(
                        as.character(FaFg_xl[2, StartCol:(StartCol+2)]),
                        ToDetect$SearchText)) + StartCol - 1
                    
                    if(length(ColNum) == 0 | is.na(ColNum)){
                        if(PKparameters_orig %in% c("all", "Absorption tab") == FALSE){
                            warning(paste0("The column with information for ", i,
                                           " on the tab `Overall Fa Fg` cannot be found in the file ", 
                                           sim_data_file, "."), 
                                    call. = FALSE)
                        }
                        suppressMessages(rm(ToDetect, ColNum))
                        PKparameters_Abs_ADAM <- setdiff(PKparameters_Abs_ADAM, i)
                        next
                    }
                    
                    suppressWarnings(
                        Out_ind[[i]] <- FaFg_xl[StartRow_ind:nrow(FaFg_xl), ColNum] %>%
                            pull(1) %>% as.numeric
                    )
                    
                    suppressWarnings(
                        Out_agg[[i]] <- FaFg_xl[StartRow_agg:EndRow_agg, ColNum] %>%
                            pull(1) %>% as.numeric
                    )
                    names(Out_agg[[i]]) <- FaFg_xl[StartRow_agg:EndRow_agg, 1] %>%
                        pull(1)
                    
                    if(checkDataSource){
                        DataCheck <- DataCheck %>%
                            bind_rows(data.frame(PKparam = i,
                                                 Tab = "Overall Fa Fg",
                                                 SearchText = ToDetect$SearchText,
                                                 Column = ColNum,
                                                 StartRow_agg = StartRow_agg,
                                                 EndRow_agg = EndRow_agg,
                                                 StartRow_ind = StartRow_ind,
                                                 EndRow_ind = nrow(FaFg_xl)))
                    }
                }
                
                if(includeTrialInfo & length(PKparameters_Abs_ADAM) > 0){
                    # Subject and trial info
                    SubjTrial_Abs <- FaFg_xl[StartRow_ind:nrow(FaFg_xl), 1:2] %>%
                        rename("Individual" = ...1, "Trial" = ...2)
                    
                    Out_ind[["Abstab"]] <- cbind(SubjTrial_Abs,
                                                 as.data.frame(Out_ind[PKparameters_Abs_ADAM]))
                }
                
            } 
            
            if("Absorption" %in% SheetNames){
                
                PKparameters_Abs <- setdiff(PKparameters_Abs, PKparameters_Abs_ADAM)
                
                Abs_xl <- suppressMessages(
                    readxl::read_excel(path = sim_data_file, sheet = "Absorption",
                                       col_names = FALSE))
                
                SubCols <- which(as.character(Abs_xl[8, ]) == "Substrate")[1]
                InhibCols <- which(as.character(Abs_xl[8, ]) == "Inhibitor 1")[1]
                
                # Looping through parameters and extracting values
                for(i in PKparameters_Abs){
                    
                    # Using regex to find the correct column. See
                    # data(AllPKParameters) for all the possible parameters as well
                    # as what regular expressions are being searched for each. 
                    ToDetect <- AllPKParameters %>% 
                        filter(Sheet == "Absorption" & PKparameter == i) %>% 
                        select(PKparameter, SearchText)
                    
                    # Looking for the regular expression specific to this parameter
                    # i. For the absorption tab, there are columns for the substrate
                    # and columns for Inhibitor 1. (There are also columns for
                    # Inhibitor 2 and 3 but I've never seen them filled in. -LSh)
                    StartCol <- ifelse(str_detect(i, "sub"),
                                       SubCols, InhibCols)
                    
                    ColNum <- which(str_detect(
                        as.character(Abs_xl[9, StartCol:(StartCol+2)]),
                        ToDetect$SearchText)) + StartCol - 1
                    
                    if(length(ColNum) == 0 | is.na(ColNum)){
                        if(PKparameters_orig %in% c("all", "Absorption tab") == FALSE){
                            warning(paste0("The column with information for ", i,
                                           " on the tab 'Absorption' cannot be found in the file ", 
                                           sim_data_file, "."), 
                                    call. = FALSE)
                        }
                        suppressMessages(rm(ToDetect, ColNum))
                        PKparameters_Abs <- setdiff(PKparameters_Abs, i)
                        next
                    }
                    
                    suppressWarnings(
                        Out_ind[[i]] <- Abs_xl[10:nrow(Abs_xl), ColNum] %>%
                            pull(1) %>% as.numeric
                    )
                    
                    if(checkDataSource){
                        DataCheck <- DataCheck %>%
                            bind_rows(data.frame(PKparam = i,
                                                 Tab = "Absorption",
                                                 SearchText = ToDetect$SearchText,
                                                 Column = ColNum,
                                                 StartRow_agg = NA,
                                                 EndRow_agg = NA,
                                                 StartRow_ind = 10,
                                                 EndRow_ind = nrow(Abs_xl)))
                    }
                }
                
                if(includeTrialInfo & length(PKparameters_Abs) > 0){
                    # Subject and trial info
                    SubjTrial_Abs <- Abs_xl[10:nrow(Abs_xl), 1:2] %>%
                        rename("Individual" = ...1, "Trial" = ...2)
                    
                    Out_ind[["Abstab"]] <- cbind(SubjTrial_Abs,
                                                 as.data.frame(Out_ind[PKparameters_Abs]))
                }
                
                # AGGREGATE VALUES: For the absorption tab, the aggregate values are
                # stored in a COMPLETELY different place, so extracting those values
                # completely separately.
                IndexRow <- which(Abs_xl$...1 == "Index")
                StartCol_agg <- which(str_detect(t(Abs_xl[IndexRow, ]), "Trial"))[2]
                
                # They are NOT ALWAYS LABELED (!!!!) as such, but the summary stats
                # are for fa, ka, and lag time in order for 1) the substrate, 2)
                # inhibitor 1, 3) inhibitor 2, and 4) inhibitor 3. Getting the
                # appropriate columns.
                SubCols <- StartCol_agg:(StartCol_agg + 2)
                Inhib1Cols <- (StartCol_agg + 3):(StartCol_agg + 5)
                Inhib2Cols <- (StartCol_agg + 6):(StartCol_agg + 8)
                Inhib3Cols <- (StartCol_agg + 9):(StartCol_agg + 11)
                # Note: I have only set this up for substrate and inhibitor 1 so
                # far. Return to this later if/when we want more. -LSh
                
                # "Statistics" is in the column before StartCol_agg, so looking
                # for that next.
                StartRow_agg <- which(Abs_xl[, StartCol_agg - 1] == "Statistics") + 1
                EndRow_agg <- which(is.na(Abs_xl[, StartCol_agg - 1]))
                EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1
                EndRow_agg <- ifelse(is.na(EndRow_agg), nrow(Abs_xl), EndRow_agg)
                
                # finding the PK parameters requested
                for(i in PKparameters_Abs){
                    
                    MyCols <- switch(str_extract(i, "sub|inhib"), 
                                     "sub" = SubCols, 
                                     "inhib" = InhibCols)
                    
                    ColNum <- MyCols[which(str_detect(
                        as.character(Abs_xl[StartRow_agg, MyCols]), 
                        ToDetect$SearchText))]
                    
                    if(length(ColNum) == 0){
                        warning(paste0("The column with information for ", i,
                                       " on the tab 'Absorption' cannot be found in the file ", 
                                       sim_data_file, "."), 
                                call. = FALSE)
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
                                                 SearchText = ToDetect$SearchText,
                                                 Column = ColNum,
                                                 StartRow_agg = StartRow_agg + 1,
                                                 EndRow_agg = EndRow_agg,
                                                 StartRow_ind = NA,
                                                 EndRow_ind = NA))
                    }
                    suppressWarnings(rm(ColNum, MyCols))
                }
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
        if("Clearance Trials SS" %in% SheetNames == FALSE){
            
            ## Pulling data from the Summary tab -------------------------------
            
            warning(paste0("The sheet `Clearance Trials SS` was not present in the Excel simulated data file to extract the PK parameters ",
                           str_comma(PKparameters_CLTSS),
                           ", so only the aggregated data from the `PKPD Parameters` section on the `Summary` tab will be extracted."),
                    call. = FALSE)
            
            # Getting anything that we couldn't get if user didn't export
            # "Clearance Trials SS" tab
            Sum_xl <- suppressMessages(
                readxl::read_excel(path = sim_data_file, sheet = "Summary",
                                   col_names = FALSE))
            
            StartRow_agg <- which(Sum_xl$...1 == "PKPD Parameters") + 1
            EndRow_agg <- which(is.na(Sum_xl$...1[StartRow_agg:nrow(Sum_xl)]))[1] +
                StartRow_agg - 2
            
            TEMP <- Sum_xl[StartRow_agg:EndRow_agg, 1:6]
            names(TEMP) <- c("Parameter", as.character(Sum_xl[StartRow_agg - 1, 2:5]),
                             "CI_lo")
            TEMP <- TEMP %>% 
                mutate(Parameter = recode(Parameter, 
                                          "CL (L/h)" = "CL_hepatic", 
                                          "CLpo (L/h)" = "CL_po", 
                                          "F (Sub)" = "F_sub", 
                                          "Fg (Sub)" = "Fg_sub", 
                                          "Fh (Sub)" = "Fh_sub")) %>% 
                filter(Parameter %in% PKparameters_CLTSS) %>% 
                pivot_longer(cols = -Parameter, 
                             names_to = "Statistic", 
                             values_to = "Value") %>% 
                mutate(Value = as.numeric(Value),
                       Statistic = recode(Statistic, 
                                          "Confidence Interval" = "90% confidence interval around the geometric mean(lower limit)", 
                                          "CI_lo" = "90% confidence interval around the geometric mean(upper limit)"))
            TEMP <- split(TEMP, f = TEMP$Parameter)
            TEMP <- lapply(TEMP, function(x){
                Stat_names <- x$Statistic
                Stat_values <- x$Value
                names(Stat_values) <- Stat_names
                return(Stat_values)
            })
            
            Out_agg <- append(Out_agg, TEMP)
            
            DataCheck <- DataCheck %>%
                bind_rows(data.frame(PKparam = PKparameters_CLTSS,
                                     Tab = "Summary",
                                     SearchText = NA,
                                     Column = 1,
                                     StartRow_agg = StartRow_agg,
                                     EndRow_agg = EndRow_agg,
                                     StartRow_ind = NA,
                                     EndRow_ind = NA))
            
            rm(TEMP)
            
        } else {
            
            CLTSS_xl <- suppressMessages(
                readxl::read_excel(path = sim_data_file, sheet = "Clearance Trials SS",
                                   col_names = FALSE))
            
            # Looping through parameters and extracting values
            for(i in PKparameters_CLTSS){
                
                # Using regex to find the correct column. See
                # data(AllPKParameters) for all the possible parameters as well
                # as what regular expressions are being searched for each. 
                ToDetect <- AllPKParameters %>% 
                    filter(Sheet == "Clearance Trials SS" & PKparameter == i) %>% 
                    select(PKparameter, SearchText)
                
                # Looking for the regular expression specific to this parameter
                # i. 
                ColNum <- which(str_detect(as.vector(t(CLTSS_xl[1, ])),
                                           ToDetect$SearchText))
                
                if(length(ColNum) == 0 | is.na(ColNum)){
                    if(PKparameters_orig %in% c("all", "Absorption tab") == FALSE){
                        warning(paste0("The column with information for ", i,
                                       " on the tab 'Clearance Trials SS' cannot be found in the file ", 
                                       sim_data_file, "."), 
                                call. = FALSE)
                    }
                    suppressWarnings(suppressMessages(rm(ToDetect, StartCol, EndCol, PossCol, ColNum)))
                    PKparameters_CLTSS <- setdiff(PKparameters_CLTSS, i)
                    next
                }
                
                suppressWarnings(
                    Out_ind[[i]] <- CLTSS_xl[2:nrow(CLTSS_xl), ColNum] %>%
                        pull(1) %>% as.numeric
                )
                
                if(checkDataSource){
                    DataCheck <- DataCheck %>%
                        bind_rows(data.frame(PKparam = i, 
                                             Tab = "Clearance Trials SS",
                                             SearchText = ToDetect$SearchText,
                                             Column = ColNum, 
                                             StartRow_agg = NA,
                                             EndRow_agg = NA,
                                             StartRow_ind = 2,
                                             EndRow_ind = nrow(CLTSS_xl)))
                }
            }   
            
            if(includeTrialInfo & length(PKparameters_CLTSS) > 0){
                # Subject and trial info
                SubjTrial_CLTSS <- CLTSS_xl[2:nrow(CLTSS_xl), 1:2] %>%
                    rename("Individual" = ...1, "Trial" = ...2)
                
                Out_ind[["CLTSStab"]] <- cbind(SubjTrial_CLTSS,
                                               as.data.frame(Out_ind[PKparameters_CLTSS]))
            }
            
            # AGGREGATE VALUES: For the CLTSS tab, the aggregate values are
            # stored in a COMPLETELY different place, so extracting those values
            # completely separately. I *think* the aggregate values always start
            # in column 10, but I'm not sure, so let's check each time.
            StartCol_agg <- which(str_detect(t(CLTSS_xl[1, ]), "Total Systemic"))
            StartRow_agg <- which(CLTSS_xl[, StartCol_agg] == "Statistics") + 1
            EndRow_agg <- which(is.na(CLTSS_xl[, StartCol_agg]))
            EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1
            EndRow_agg <- ifelse(is.na(EndRow_agg), nrow(CLTSS_xl), EndRow_agg)
            
            # Looping through parameters and extracting values
            for(i in PKparameters_CLTSS){
                
                # Using regex to find the correct column. See
                # data(AllPKParameters) for all the possible parameters as well
                # as what regular expressions are being searched for each. 
                ToDetect <- AllPKParameters %>% 
                    filter(Sheet == "Clearance Trials SS" & PKparameter == i) %>% 
                    select(PKparameter, SearchText)
                
                # Looking for the regular expression specific to this parameter
                # i. 
                ColNum <-  which(str_detect(as.vector(t(CLTSS_xl[StartRow_agg, ])),
                                            ToDetect$SearchText))
                
                if(length(ColNum) == 0 | is.na(ColNum)){
                    if(PKparameters_orig %in% c("all", "Absorption tab") == FALSE){
                        warning(paste0("The column with information for ", i,
                                       " on the tab 'Clearance Trials SS' cannot be found in the file ", 
                                       sim_data_file, "."), 
                                call. = FALSE)
                    }
                    suppressWarnings(suppressMessages(rm(ToDetect, StartCol, EndCol, PossCol, ColNum)))
                    PKparameters_CLTSS <- setdiff(PKparameters_CLTSS, i)
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
                                             SearchText = ToDetect$SearchText,
                                             Column = ColNum,
                                             StartRow_agg = StartRow_agg + 1,
                                             EndRow_agg = EndRow_agg,
                                             StartRow_ind = NA,
                                             EndRow_ind = NA))
                }
                
                # end of iteration i
                suppressWarnings(rm(ToDetect, ColNum))
            }
            
            suppressWarnings(rm(StartRow_agg, EndRow_agg, StartCol_agg, EndRow_ind))
        }
    }
    
    # Pulling data from RegADAM tab -------------------------
    PKparameters_RegADAM <- intersect(PKparameters, ParamRegADAM)
    
    # Some PK parameters show up on multiple sheets. No need to pull
    # those here if they've already been pulled from another sheet.
    PKparameters_RegADAM <- setdiff(PKparameters_RegADAM, names(Out_agg))
    
    if(complete.cases(sheet)){
        # How do you set something to have length 0? Hacking it for now.
        PKparameters_RegADAM <- intersect("A", "B")
    }
    
    if(length(PKparameters_RegADAM) > 0){
        # Error catching
        if("Regional ADAM Fractions (Sub)" %in% SheetNames == FALSE){
            warning(paste0("The sheet `Regional ADAM Fractions (Sub)` must be present in the Excel simulated data file to extract the PK parameters ",
                           sub("and", "or", str_comma(PKparameters_RegADAM)),
                           ". None of these parameters can be extracted."),
                    call. = FALSE)
        } else {
            
            Sheet <- "Regional ADAM Fractions (Sub)"
            
            RegADAM_xl <- suppressMessages(
                readxl::read_excel(path = sim_data_file, sheet = Sheet,
                                   col_names = FALSE))
            
            # Finding the last row of the individual data
            StartRow_ind <- ifelse(complete.cases(RegADAM_xl[1, 1]) &&
                                       str_detect(RegADAM_xl[1, 1], "Fa greater than 1"), 
                                   4, 3)
            EndRow_ind <- which(is.na(RegADAM_xl$...2[StartRow_ind:nrow(RegADAM_xl)]))[1] +
                StartRow_ind - 2
            
            if(length(EndRow_ind) == 0){
                # Using "warning" instead of "stop" here b/c I want this to be
                # able to pass through to other functions and just skip any
                # files that aren't simulator output.
                warning(paste0("It appears that you don't have any aggregate data in your simulator output file ",
                               sim_data_file, "; was this a population-representative simulation? This function only really works well when there are aggregate data present, so this file will be skipped."),
                        call. = FALSE)
                return(list())
            } 
            
            # Finding the aggregate data rows 
            StartRow_agg <- which(RegADAM_xl$...2 == "Statistics") + 2
            EndRow_agg <- which(is.na(RegADAM_xl$...2))
            EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1] - 1
            EndRow_agg <- ifelse(is.na(EndRow_agg), nrow(RegADAM_xl), EndRow_agg)
            
            # Looping through parameters and extracting values
            for(i in PKparameters_RegADAM){
                
                # Using regex to find the correct column. See
                # data(AllPKParameters) for all the possible parameters as well
                # as what regular expressions are being searched for each. 
                ToDetect <- AllPKParameters %>% 
                    filter(Sheet == "Regional ADAM Fractions (Sub)" & PKparameter == i) %>% 
                    select(PKparameter, SearchText)
                
                # Looking for the regular expression specific to this parameter
                # i. 
                ColNum <- which(str_detect(as.vector(t(RegADAM_xl[StartRow_ind - 1, ])),
                                           ToDetect$SearchText))
                # fa values 1st, fm values 2nd for this sheet
                ColNum <- ifelse(str_detect(i, "fa"), ColNum[1], ColNum[2])
                
                if(length(ColNum) == 0 | is.na(ColNum)){
                    if(PKparameters_orig %in% c("all", "Regional ADAM") == FALSE){
                        warning(paste0("The column with information for ", i,
                                       " on the tab `Regional ADAM Fractions (Sub)` cannot be found in the file ", 
                                       sim_data_file, "."), 
                                call. = FALSE)
                    }
                    suppressMessages(rm(ToDetect, ColNum))
                    PKparameters_RegADAM <- setdiff(PKparameters_RegADAM, i)
                    next
                }
                
                suppressWarnings(
                    Out_ind[[i]] <- RegADAM_xl[StartRow_ind:EndRow_ind, ColNum] %>%
                        pull(1) %>% as.numeric
                )
                
                suppressWarnings(
                    Out_agg[[i]] <- RegADAM_xl[StartRow_agg:EndRow_agg, ColNum] %>%
                        pull(1) %>% as.numeric()
                )
                names(Out_agg[[i]]) <- RegADAM_xl[StartRow_agg:EndRow_agg, 2] %>%
                    pull(1)
                
                if(checkDataSource){
                    DataCheck <- DataCheck %>%
                        bind_rows(data.frame(PKparam = i, 
                                             Tab = Sheet,
                                             SearchText = ToDetect$SearchText,
                                             Column = ColNum, 
                                             StartRow_agg = StartRow_agg,
                                             EndRow_agg = EndRow_agg,
                                             StartRow_ind = StartRow_ind,
                                             EndRow_ind = EndRow_ind))
                }
            }   
            
            if(includeTrialInfo & length(PKparameters_RegADAM) > 0){
                # Subject and trial info
                SubjTrial_RegADAM <- RegADAM_xl[StartRow_ind:EndRow_ind, 1:2] %>%
                    rename("Individual" = ...1, "Trial" = ...2)
                
                Out_ind[["RegADAMtab"]] <- cbind(SubjTrial_RegADAM,
                                                 as.data.frame(Out_ind[PKparameters_RegADAM]))
            }
            
            suppressWarnings(rm(StartRow_agg, EndRow_agg, EndRow_ind, Sheet))
        }
    }
    
    
    # Putting all data together ---------------------------------------------- 
    # If user only wanted one parameter and includeTrialInfo was FALSE and so
    # was returnExpDetails, make the output a vector instead of a list
    if(length(Out_ind) == 1 & includeTrialInfo == FALSE & 
       returnExpDetails == FALSE){
        
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
    
    if(any(c("aggregate", "both") %in% returnAggregateOrIndiv) & 
       length(Out_agg) == 0){
        warning(paste0("For the file ", sim_data_file, 
                       ", no PK parameters were found. Did you include PK info as part of your simulation output?"), 
                call. = FALSE)
        return(list())
    }
    
    if(any(c("individual", "both") %in% returnAggregateOrIndiv) &
       length(Out_ind) == 0){
        warning(paste0("For the file ", sim_data_file, 
                       ", no PK parameters were found. Did you include PK info as part of your simulation output?"), 
                call. = FALSE)
        return(list())
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
        XLCols <- c(LETTERS, paste0("A", LETTERS), paste0("B", LETTERS))
        DataCheck <- DataCheck %>% filter(complete.cases(PKparam)) %>% 
            mutate(File = sim_data_file, 
                   Column = XLCols[Column], 
                   Individual = paste0(Column, StartRow_ind, ":",
                                       Column, EndRow_ind), 
                   Individual = ifelse(is.na(StartRow_ind), NA, Individual)) %>% 
            filter(complete.cases(PKparam)) %>% unique()
        
        if(any(returnAggregateOrIndiv %in% c("both", "aggregate"))){
            
            if(length(Out_agg) > 1){
                StatNames <- renameStats(Out_agg$Statistic) %>% as.character()
            } else {
                StatNames <- renameStats(names(Out_agg[[1]]))
            }
            
            StatNum <- 1:length(StatNames)
            names(StatNum) <- StatNames
            
            suppressMessages(
                DataCheck_agg <- DataCheck %>% 
                    select(File, PKparam, Tab, Column, StartRow_agg, EndRow_agg) %>% 
                    left_join(data.frame(File = unique(DataCheck$File), 
                                         Stat = StatNames)) %>% 
                    mutate(Row_agg = StatNum[Stat] + StartRow_agg - 1, 
                           Cell = paste0(Column, Row_agg), 
                           Cell = ifelse(is.na(Row_agg), NA, Cell)) %>% 
                    select(File, PKparam, Tab, Column, Cell, Stat) %>% 
                    pivot_wider(names_from = Stat, values_from = Cell)
            )
            
            suppressMessages(
                DataCheck <- DataCheck %>% left_join(DataCheck_agg)
            )
        }
        
        DataCheck <- DataCheck %>% 
            select(-c(Column, StartRow_ind, EndRow_ind, StartRow_agg, EndRow_agg,
                      StartColText, SearchText, Note)) %>% 
            group_by(PKparam, File, Tab) %>%
            fill(everything(), .direction = "downup") %>%
            select(PKparam, File, Tab, Individual, everything()) %>% unique()
        
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
    
    if(returnExpDetails){
        if(class(Out)[1] == "list"){
            Out[["ExpDetails"]] <- Deets
        } else {
            Out <- list(Out)
            Out[[2]] <- Deets
            names(Out) <- c(returnAggregateOrIndiv, "ExpDetails")
        }
    }
    
    return(Out)
}

