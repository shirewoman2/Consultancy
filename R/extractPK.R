#' Extract PK data for specific parameters from a simulator output Excel file
#'
#' Pull calculated PK parameters from a Simcyp simulation output Excel file. A
#' data.frame of all possible PK parameters may be found by typing
#' \code{view(PKParameterDefinitions)} into the console.
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
#'   \item{"AUC tab"}{only those parameters on the "AUC" tab (default). For V21
#'   or later, this will look for tabs named something like
#'   "Int AUC 1st_CI(Sub)(CPlasma)" for the 1st dose of the substrate, for
#'   example, and pull all possible parameters there. For V21,
#'   and earlier, this will pull data from the tab literally named "AUC"
#'   or the "AUC_CI" tab or "AUC_SD" tab if a tab named "AUC"is not present.}
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
#' @param compoundToExtract For which compound do you want to extract
#'   PK data? Options are: \itemize{\item{"substrate"
#'   (default),} \item{"primary metabolite 1",} \item{"primary metabolite 2",}
#'   \item{"secondary metabolite",} \item{"inhibitor 1" -- this can be an
#'   inducer, inhibitor, activator, or suppresesor, but it's labeled as
#'   "Inhibitor 1" in the simulator,} \item{"inhibitor 2" for the 2nd inhibitor
#'   listed in the simulation,} \item{"inhibitor 1 metabolite" for the primary
#'   metabolite of inhibitor 1}}
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default), "unbound plasma", "blood", "unbound blood",
#'   "peripheral plasma", or "peripheral blood". \strong{NOTE: PK for peripheral
#'   sampling is not as well tested as for other tissues and is only set up for 
#'   V21+. Please check your results carefully.}
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#' @param includeTrialInfo TRUE or FALSE (default) for whether to include which
#'   individual and trial the data describe. 
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
#' @param ... other arguments passed through 
#'
#' @return Depending on the options selected, returns a list of numerical
#'   vectors or a list of data.frames: "individual" and "aggregate". If
#'   \code{checkDataSource} is TRUE, this will also return a data.frame named
#'   "QC" that indicates where in the simulator output file the data came from.
#'
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
                      PKparameters = NA,
                      sheet = NA,
                      compoundToExtract = "substrate",
                      tissue = "plasma",
                      existing_exp_details = NA, 
                      includeTrialInfo = TRUE,
                      returnExpDetails = FALSE, 
                      checkDataSource = TRUE, 
                      ...){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if("returnAggregateOrIndiv" %in% names(match.call())){
      warning(wrapn("You specified something for the soon-to-be-deprecated argument 'returnAggregateOrIndiv', which we will ignore and instead give you both individual and aggregate results. Please adjust your code to omit that argument going forward."), 
              call. = FALSE)
   }
   
   # Checking on any user-specified sheets b/c that also implies which PK
   # parameters to use. 
   if(any(complete.cases(sheet))){
      
      if(is.null(names(sheet)) & all(is.na(PKparameters))){
         # Scenario 1: User has supplied a specific sheet and has not specified any
         # PK parameters. They presumably want all available PK from that sheet.
         
         PKSheets <- data.frame(Sheet = sheet, 
                                PKparameter = "all")
         PKparameters <- "all" 
         
      } else if(is.null(names(sheet)) == FALSE & all(is.na(PKparameters))){
         # Scenario 2: User has supplied a named character vector of sheets to
         # use, which will include which PK they want, and has NOT specified
         # anything for PKparameters, which is what we want when they have
         # supplied that named character vector b/c that already includes which
         # PK they want and from which tab.
         
         PKSheets <- data.frame(Sheet = sheet, 
                                PKparameter = names(sheet)) %>% 
            unique()
         
         PKparameters <- PKSheets$PKparameter
         
      } else if(is.null(names(sheet)) & any(complete.cases(PKparameters))){
         # Scenario 3: User has supplied a single specific sheet and also a
         # specific set of PK parameters they want.
         
         PKSheets <- data.frame(Sheet = sheet, 
                                PKparameter = PKparameters) %>% 
            # If they supplied dose suffixes, remove those to make it consistent
            # w/using custom AUC interval elsewhere.
            mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
                   PKparameter = sub("AUCtau", "AUCt", PKparameter)) %>% 
            unique()
         
         PKparameters <- PKSheets$PKparameter
         
      } else if(is.null(names(sheet)) == FALSE & any(complete.cases(PKparameters))){
         # Scenario 4, which is problematic: User supplied named character
         # vector AND some specific PKparameters, which may or may not match and
         # which I don't want to have to write a ton of code to check whether
         # they do and figure out what to do with the mismatches. 
         
         stop("You have specified some PK parameters with the argument `PKparameters` and some with the argument `sheet`,\nand we're not sure which set we should use. Please only use one or the other.\n", 
              call. = FALSE)
      }
   } 
   
   # Harmonizing PK parameter names
   PKparameters <- harmonize_PK_names(PKparameters)
   
   # If they didn't include ".xlsx" at the end, add that.
   sim_data_file <- ifelse(str_detect(sim_data_file, "xlsx$"), 
                           sim_data_file, paste0(sim_data_file, ".xlsx"))
   
   tissue <- tolower(tissue)
   if(tissue %in% c("plasma", "unbound plasma", "blood", "unbound blood", 
                    "peripheral plasma", "peripheral blood") == FALSE){
      warning("You have not supplied a permissible value for tissue. Options are `plasma`, `unbound plasma`, `blood`, `unbound blood`, `peripheral plasma`, or `peripheral blood`. The PK parameters will be for plasma.", 
              call. = FALSE)
      tissue <- "plasma"
   }
   
   
   # Main body of function ---------------------------------------------------
   
   # NOTE TO CODERS: This function calls on the data object AllPKParameters to
   # figure out which regex to use for which PK parameter. In AllPKParameters,
   # the sheet listed is "AUC" when it's a parameter that we're searching for on
   # a tab named "AUC", "AUC_SD", or "AUC_CI", which apparently only exists with
   # simulator versions earlier than V22 and even then does not *always* exist.
   # The "AUC" tab has a very specific format that DIFFERS from the more generic
   # PK tab layouts, so this matters! 
   
   # By contrast, when the sheet listed is not plain "AUC" but "AUC0", that's
   # only for first-dose data, and the structure of that Excel tab is the more
   # generic layout of PK data like in, e.g., "Int AUC 1st(Sub)(CPlasma)" or a
   # similarly named tab. Similarly, when the sheet is listed in AllPKParameters
   # as "AUCX", that's a tab that could be for a last dose or for a
   # user-specified interval, but its layout will be similar to the AUC0 layout,
   # so extractPK will still call on the internal function extractAUCXtab to get
   # the requested info.
   
   # To add new PK parameters to extract, add them to AllPKParameters, following
   # the examples for other PK parameters as far as what to put in each column.
   # This is TRICKY for the AUC tab b/c you have to check things in multiple
   # rows to make sure you're extracting the correct cells. It's easier for the
   # AUCX tabs, which means that it gets easier when the version of the
   # simulator is 22+. Once you've added the new parameters to AllPKParameters,
   # save that RData object when you save the package, and then extractPK
   # *should* be able to find them. At least, that's how I've tried to design
   # it to work! -LSh
   
   
   ## Checking exp details --------------------------------------------------
   
   # Checking experimental details to only pull details that apply
   if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA
      Deets <- extractExpDetails(sim_data_file = sim_data_file, 
                                 exp_details = "Summary and Input")[["MainDetails"]]
   } else {
      Deets <- harmonize_details(existing_exp_details)[["MainDetails"]] %>% 
         filter(File %in% sim_data_file) 
      
      if(nrow(Deets) == 0){
         Deets <- extractExpDetails(sim_data_file = sim_data_file, 
                                    exp_details = "Summary and Input")[["MainDetails"]]
      }
   }
   
   
   ## A bit more error catching w/Deets ---------------------------------------
   
   # Can't do this check if it was actually a db file that we got
   # existing_exp_details on, so skipping this.
   if(str_detect(sim_data_file, "xlsx$")){
      
      # Checking that the file is, indeed, a simulator output file. 
      SheetNames <- gsub("`", "", str_split_1(Deets$SheetNames, "` `"))
      
      if(all(c("Input Sheet", "Summary") %in% SheetNames) == FALSE){
         # Using "warning" instead of "stop" here b/c I want this to be able to
         # pass through to other functions and just skip any files that
         # aren't simulator output.
         warning(wrapn(paste0("The file '", sim_data_file,
                              "' does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file.")), 
                 call. = FALSE)
         return(list())
      }
      
      if(any(complete.cases(sheet))){
         # Using SheetWasSpecified later for flow control. If they specified a
         # sheet name originally, even if that sheet name wasn't found, then only
         # attempt to get PK from the user-specified sheet.
         SheetWasSpecified <- TRUE 
         GoodSheets <- sheet[complete.cases(sheet) & sheet %in% SheetNames]
         BadSheets <- setdiff(sheet, GoodSheets)
         BadSheets <- BadSheets[complete.cases(BadSheets)]
         
         if(length(BadSheets) > 0){
            warning(paste0("Some of the sheets requested could not be found in the Simulator results file. We will skip PK parameters for the sheet `", 
                           BadSheets, "`.\n"), 
                    call. = FALSE)
            
            sheet <- GoodSheets
         }
      } else {
         SheetWasSpecified <- FALSE
      }
   } else {
      SheetWasSpecified <- FALSE
   }
   
   if(Deets$SimulatorUsed == "Simcyp Discovery"){
      if("PKTissue_Discovery" %in% names(Deets) == FALSE){
         Deets <- extractExpDetails(sim_data_file = sim_data_file)
         Deets <- Deets$MainDetails
      }
      
      if(tissue != Deets$PKTissue_Discovery){
         warning(paste0("You requested PK for ", 
                        tissue, 
                        ", but the tissue included in your Simcyp Discovery file is ", 
                        Deets$PKTissue_Discovery, ". We cannot return any of the requested PK.\n"), 
                 call. = FALSE)
         return(list())
      }
   }
   
   
   ## Figuring out correct tab names ---------------------------------------
   
   # version <= 20: Has "AUC" tab for each compound that was integrated. Sheet
   # names are harder to decipher: AUC0 is 1st dose, but last dose will be on
   # whichever AUC tab has the highest number for that compoundID. AUC tab for
   # compounds than substrate will be labeled as, e.g., "AUC(Inh 1)". AUCinf is
   # ONLY found on AUC tab.
   
   # version == 21: Has "AUC" tab. Sheet names are "1st" or "last" for 1st or
   # last dose, respectively. AUC tab for compounds than substrate will be
   # labeled as, e.g., "AUC(Inh 1)". AUCinf is ONLY found on AUC tab.
   
   # verison >= 22: No AUC tab. Sheet names are "1st" or "last" for 1st or last
   # dose, respectively. When it's a user-defined sheet, it will be listed in
   # the order that the user specified it, e.g., "Int AUC userT(1)(Sub)(CPlasma)" 
   # for the 1st user-defined interval specified for the substrate in plasma. 
   
   Tab_AUC <- switch(
      compoundToExtract, 
      "substrate" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?$")][1],
      "primary metabolite 1" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Sub Pri Met1\\)$")][1],
      "primary metabolite 2" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Sub Pri Met2\\)$")][1],
      "secondary metabolite" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Sub Sec Met\\)$")][1],
      "inhibitor 1" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Inh 1\\)$")][1],
      "inhibitor 2" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Inh 2\\)$")][1],
      "inhibitor 1 metabolite" = SheetNames[str_detect(SheetNames, "^AUC(_CI|_SD)?\\(Inh 1 Met\\)$")][1])
   
   if(Deets$SimulatorUsed == "Simcyp Discovery"){
      # If it's from Simcyp Discovery, there's only one AUC sheet and
      # it's either for the only dose in a SD sim or the last dose for
      # a MD sim. Not sure about the regular Simcyp Animal, though. 
      Tab_AUC <- SheetNames[which(str_detect(
         SheetNames, 
         paste0("AUC.*", switch(compoundToExtract, 
                                "substrate" = "Sub\\)", 
                                "primary metabolite 1" = "Sub Met"))))]
   }
   
   # An unusual situation: Alice had a multiple-dose simulation where the AUC
   # tab in V21 listed 1st dose PK data instead of last-dose PK data, like I've
   # seen *everywhere else but here*. For this reason, we're NOT going to pull
   # data from Tab_AUC when it's a custom-dosing regimen. It's unreliable.
   if(complete.cases(Deets$DoseInt_sub) &&
      Deets$DoseInt_sub == "custom dosing"){
      Tab_AUC <- NULL
   }
   
   SimV21plus <- as.numeric(str_extract(Deets$SimulatorVersion, "[0-9]{2}")) >= 21
   
   if(SimV21plus){
      Tab_first <- SheetNames[
         str_detect(SheetNames, 
                    paste0(ifelse(str_detect(tissue, "peripheral"), 
                                  "Periph ", ""), 
                           "Int AUC 1st(_CI|_SD)?", 
                           switch(compoundToExtract,
                                  "substrate" = "\\(Sub\\)", 
                                  "primary metabolite 1" = "\\(Sub Met\\)|\\(Sub Pri Met1\\)",
                                  "primary metabolite 2" = "\\(Sub Met2\\)|\\(Sub Pri Met2\\)",
                                  "secondary metabolite" = "\\(Sub S(ec )?M(et)?\\)", 
                                  "inhibitor 1" = "\\(Inh 1\\)",
                                  "inhibitor 1 metabolite" = "\\(Inh( )?1 M(et)?\\)", 
                                  "inhibitor 2" = "\\(Inh 2\\)"), 
                           switch(tissue, 
                                  "plasma" = "\\(CP", # some sheet names have ellipses, e.g., "Int AUC 1st_SD(Sub Met)(CPl...)"
                                  "peripheral plasma" = "\\(CP",
                                  "unbound plasma" = "\\(CuP",
                                  "blood" = "\\(CB", 
                                  "peripheral blood" = "\\(CB", 
                                  "unbound blood" = "\\(CuB")))][1]
      
      # Sometimes, the tab name gets clipped so far that you can't even tell
      # which tissue it is. In that situation, we need to check what tissue was
      # on any possible Tab_first sheets. We'd get an NA above even if that tab
      # were present if it was either clipped at the C or clipped at Cu. In both
      # of those situations, we need to figure out what the tissue is.
      if(is.na(Tab_first)){
         Tab_first_clipped <- 
            SheetNames[
               str_detect(SheetNames, 
                          paste0("Int AUC 1st(_CI|_SD)?", 
                                 switch(compoundToExtract,
                                        "substrate" = "\\(Sub\\)", 
                                        "primary metabolite 1" = "\\(Sub Met\\)",
                                        "primary metabolite 2" = "\\(Sub Met2\\)",
                                        "secondary metabolite" = "\\(Sub SM\\)", 
                                        "inhibitor 1" = "\\(Inh 1\\)",
                                        "inhibitor 1 metabolite" = "\\(Inh1 M(et)?\\)", 
                                        "inhibitor 2" = "\\(Inh 2\\)"), 
                                 "\\(C(u)?\\."))]
         
         
         if(any(complete.cases(Tab_first_clipped))){
            
            TissueCheck <- c()
            
            for(i in Tab_first_clipped){
               suppressMessages(
                  TEMP <- readxl::read_excel(path = sim_data_file, 
                                             sheet = i,
                                             range = "A1:H1",
                                             col_names = FALSE)
               )
               
               TissueCheck[i] <- 
                  any(str_detect(t(TEMP)[1], switch(tissue, 
                                                    "plasma" = "\\(CPlasma", 
                                                    "unbound plasma" = "CuPlasma", 
                                                    "blood" = "\\(CBlood", 
                                                    "unbound blood" = "CuBlood")))
               
               rm(TEMP)
            }
            
            Tab_first <- names(TissueCheck)[TissueCheck == TRUE][1]
         } 
      }
      
      Tab_last <- SheetNames[
         str_detect(SheetNames, 
                    paste0("Int AUC last(_CI|_SD)?", 
                           switch(compoundToExtract,
                                  "substrate" = "\\(Sub\\)", 
                                  "primary metabolite 1" = "\\(Sub Met\\)",
                                  "primary metabolite 2" = "\\(Sub Met2\\)",
                                  "secondary metabolite" = "\\(Sub SM\\)", 
                                  "inhibitor 1" = "\\(Inh 1\\)",
                                  "inhibitor 1 metabolite" = "\\(Inh( )?1 M(et)?\\)", 
                                  "inhibitor 2" = "\\(Inh 2\\)"), 
                           switch(tissue, 
                                  "plasma" = "\\(CP", # some sheet names have ellipses, e.g., "Int AUC 1st_SD(Sub Met)(CPl...)"
                                  "unbound plasma" = "\\(CuP",
                                  "blood" = "\\(CB", 
                                  "unbound blood" = "\\(CuB")))][1]
      
      # Sometimes, the tab name gets clipped so far that you can't even tell
      # which tissue it is. In that situation, we need to check what tissue was
      # on any possible Tab_last sheets. We'd get an NA above even if that tab
      # were present if it was either clipped at the C or clipped at Cu. In both
      # of those situations, we need to figure out what the tissue is.
      if(is.na(Tab_last)){
         Tab_last_clipped <- 
            SheetNames[
               str_detect(SheetNames, 
                          paste0("Int AUC last(_CI|_SD)?", 
                                 switch(compoundToExtract,
                                        "substrate" = "\\(Sub\\)", 
                                        "primary metabolite 1" = "\\(Sub Met\\)",
                                        "primary metabolite 2" = "\\(Sub Met2\\)",
                                        "secondary metabolite" = "\\(Sub SM\\)", 
                                        "inhibitor 1" = "\\(Inh 1\\)",
                                        "inhibitor 1 metabolite" = "\\(Inh1 M(et)?\\)", 
                                        "inhibitor 2" = "\\(Inh 2\\)"), 
                                 "\\(C(u)?\\."))]
         
         
         if(any(complete.cases(Tab_last_clipped))){
            
            TissueCheck <- c()
            
            for(i in Tab_last_clipped){
               suppressMessages(
                  TEMP <- readxl::read_excel(path = sim_data_file, 
                                             sheet = i,
                                             range = "A1:H1",
                                             col_names = FALSE)
               )
               
               TissueCheck[i] <- 
                  any(str_detect(t(TEMP)[1], switch(tissue, 
                                                    "plasma" = "\\(CPlasma", 
                                                    "unbound plasma" = "CuPlasma", 
                                                    "blood" = "\\(CBlood", 
                                                    "unbound blood" = "CuBlood")))
               rm(TEMP)
            }
            
            Tab_last <- names(TissueCheck)[TissueCheck == TRUE][1]
         } 
      }
      
   } else {
      
      # Simulator versions earlier than V21
      
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
                                  "plasma" = "\\(CP", # some sheet names have ellipses, e.g., "Int AUC 1st_SD(Sub Met)(CPl...)"
                                  "unbound plasma" = "\\(CuP",
                                  "blood" = "\\(CB", 
                                  "unbound blood" = "\\(CuB")))][1]
      
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
                                  "plasma" = "\\(CP", # some sheet names have ellipses, e.g., "Int AUC 1st_SD(Sub Met)(CPl...)"
                                  "unbound plasma" = "\\(CuP",
                                  "blood" = "\\(CB", 
                                  "unbound blood" = "\\(CuB")))]
      
      if(length(Tab_last_check) > 0){
         LastDoseNum <- data.frame(Tab_last = Tab_last_check) %>% 
            mutate(DoseNum = as.numeric(str_extract(Tab_last, "[0-9]{1,}"))) %>% 
            # It's the highest dose number and it can't be 0 b/c that's dose 1.
            filter(DoseNum == max(DoseNum) & DoseNum != 0)
         Tab_last <- LastDoseNum$Tab_last[1]
      } else {
         # If there was no Tab_last found but there *is* a tab with "t0" in
         # the name, e.g., AUCt0(Sub)(CPlasma), then use that one.
         Tab_last <- Tab_last_check[str_detect(Tab_last_check, "t0")]
         
         # If there *still* isn't a tab last, set this to NA. Probably should
         # change how I set this up and 1st check whether this is a single-dose
         # simulation.
         Tab_last <- ifelse(length(Tab_last) == 0, NA, Tab_last)
      }
   }
   
   # Need to keep track of the original PK parameters requested so that we
   # don't waste time reading more sheets than necessary
   PKparameters_orig <- PKparameters
   
   if(tolower(PKparameters[1]) %in% c("all", "auc tab", "absorption tab", 
                                      "auc0", "regional adam")){
      PKparameters <- 
         switch(tolower(PKparameters[1]), 
                "all" = AllPKParameters %>% pull(PKparameter) %>% unique(), 
                "auc tab" = AllPKParameters %>% 
                   filter(Sheet %in% c("AUC", "AUC0", "AUCX")) %>% 
                   pull(PKparameter) %>% unique(),
                "absorption tab" = AllPKParameters %>% 
                   filter(Sheet %in% c("Absorption", "Overall Fa Fg")) %>% 
                   pull(PKparameter) %>% unique(),
                "auc0" = AllPKParameters %>% 
                   filter(Sheet %in% c("AUC0", "AUCX")) %>% 
                   pull(PKparameter) %>% unique(), # This will happen if user requests PKparameters = "AUC" but "AUC" tab is not present but a tab for AUC0 *is*.
                "regional adam" = AllPKParameters %>% 
                   filter(Sheet %in% c("Regional ADAM Fractions (Sub)")) %>% 
                   pull(PKparameter) %>% unique())
   }
   
   # This shouldn't be necessary b/c we already dealt w/this up higher
   
   # # For when user specified a sheet but not the parameters they want
   # if(all(is.null(names(sheet))) & 
   #    all(complete.cases(sheet))){
   #    PKparameters <- unique(sub("_dose1|_last", "", PKparameters))
   # }
   
   # Checking whether the user had supplied a vector of specific parameters
   # rather than a parameter set name and using those if so.
   if(is.null(PKparameters) & any(complete.cases(PKparameters_orig))){
      PKparameters <- PKparameters_orig
   }
   
   # Creating a data.frame that compiles where to look for each parameter. NB:
   # At this point, PKparameters should be not just a set name s/a "all" but a
   # vecctor of parameters.
   PKparamDF <- data.frame(PKparameter = PKparameters) %>% 
      mutate(SheetAUC = PKparameter %in% 
                (AllPKParameters %>% filter(Sheet == "AUC") %>% 
                    pull(PKparameter) %>% unique()), 
             
             SheetAbsorption = PKparameter %in% 
                (AllPKParameters %>% filter(Sheet %in% c("Absorption",
                                                         "Overall Fa Fg")) %>% 
                    pull(PKparameter) %>% unique()), 
             
             SheetRegADAM = PKparameter %in% 
                (AllPKParameters %>% 
                    filter(Sheet %in% c("Regional ADAM Fractions (Sub)")) %>% # FIXME - probably can expand this to inhibitor as well.
                    pull(PKparameter) %>% unique()), 
             
             SheetAUC0 = PKparameter %in% 
                (AllPKParameters %>% filter(Sheet == "AUC0") %>% 
                    pull(PKparameter) %>% unique()), 
             
             SheetAUClast = PKparameter %in% 
                (AllPKParameters %>% 
                    filter(Sheet == "AUCX" & str_detect(PKparameter, "_last")) %>% 
                    pull(PKparameter) %>% unique()), 
             
             SheetDrugPop = PKparameter %in% 
                (AllPKParameters %>% filter(Sheet == "Drug-Population Parameters") %>% 
                    pull(PKparameter) %>% unique()), 
             
             SheetCLTSS = PKparameter %in% 
                (AllPKParameters %>% filter(Sheet == "Clearance Trials SS") %>% 
                    pull(PKparameter) %>% unique()))
   
   # Adding user-defined sheet.
   if(any(complete.cases(sheet))){
      
      # If user only specified a sheet name w/out naming each one, then all the
      # other sheets should be false b/c we ONLY want to look on the one sheet
      # where they said to look.
      if(all(is.null(names(sheet))) & 
         all(complete.cases(sheet))){
         PKparamDF <- PKparamDF %>% 
            mutate(across(.cols = matches("Sheet"), 
                          .fns = function(x) FALSE), 
                   SheetUser = TRUE)
      } else {
         PKparamDF <- PKparamDF %>% 
            mutate(SheetUser = PKparameter %in% 
                      names(sheet[complete.cases(sheet)]))
      }
   } else {
      PKparamDF$SheetUser <- FALSE
   }
   
   MissingPKParam <- setdiff(PKparameters,
                             c(AllPKParameters$PKparameter, 
                               sub("_dose1|_last", "", AllPKParameters$PKparameter)))
   if(length(MissingPKParam) > 0){
      warning(paste0("The parameter(s) ", str_comma(MissingPKParam),
                     " is/are not among the possible PK parameters and will not be extracted. Please see data(PKParameterDefinitions) for all possible parameters."),
              call. = FALSE)
   }
   
   # Filtering out irrelevant PK ----------------------------------------------
   if(Deets$PopRepSim == "Yes"){
      warning(wrapn(paste0("The simulator file supplied, `", 
                           sim_data_file, 
                           "`, is for a population-representative simulation and thus doesn't have any aggregate data. This function only really works with aggregate data, so this file will be skipped.")),
              call. = FALSE)
      return(list())
   }
   
   # Filtering out DDI params when not a DDI sim 
   if(is.na(Deets$Inhibitor1)){
      NotDDIParams <- AllPKParameters$PKparameter[
         AllPKParameters$AppliesOnlyWhenPerpPresent == FALSE]
      
      PKparamDF <- PKparamDF %>% 
         filter(PKparameters %in% c(NotDDIParams, 
                                    sub("_last|_dose1", "", NotDDIParams)))
   }
   
   # Filtering out multiple dose params when SD sim 
   if((compoundToExtract %in% c("substrate", "primary metabolite 1", 
                                "primary metabolite 2", "secondary metabolite") &
       Deets$Regimen_sub == "Single Dose") |
      (complete.cases(Deets$Inhibitor1) && 
       (compoundToExtract %in% c("inhibitor 1", "inhibitor 1 metabolite") &&
        Deets$Regimen_inhib == "Single Dose")) |
      ("Inhibitor2" %in% names(Deets) && 
       (complete.cases(Deets$Inhibitor2) &&
        compoundToExtract %in% c("inhibitor 2") && 
        Deets$Regimen_inhib2 == "Single Dose"))){
      
      SDParams <- AllPKParameters$PKparameter[
         AllPKParameters$AppliesToSingleDose == TRUE]
      
      PKparamDF <- PKparamDF %>% 
         filter(PKparameter %in% c(SDParams, 
                                   sub("_last|_dose1", "", SDParams)))
   }
   
   # !!! IMPORTANT!!! If it was a custom-dosing regimen, then any parameters
   # that are not dose 1 parameters are not necessarily in their usual
   # locations! Previously, we did NOT report any PK for that scenario. Now, we
   # check that the dose interval and the AUC interval match, so this SHOULD be
   # ok. No longer removing this info. 
   
   # if(((compoundToExtract %in% c("substrate", "primary metabolite 1", 
   #                               "primary metabolite 2", "secondary metabolite") & 
   #      is.null(Deets$DoseInt_sub) == FALSE && 
   #      (complete.cases(Deets$DoseInt_sub) && Deets$DoseInt_sub == "custom dosing")) |
   #     
   #     (compoundToExtract %in% c("inhibitor 1", "inhibitor 1 metabolite") &
   #      is.null(Deets$DoseInt_inhib) == FALSE && 
   #      complete.cases(Deets$DoseInt_inhib) &&
   #      Deets$DoseInt_inhib == "custom dosing") |
   #     
   #     (compoundToExtract == "inhibitor 2" & 
   #      is.null(Deets$DoseInt_inhib2) == FALSE && 
   #      complete.cases(Deets$DoseInt_inhib2) &&
   #      Deets$DoseInt_inhib2 == "custom dosing")) &
   #    
   #    any(str_detect(PKparameters, "_last")) & all(is.na(sheet))){
   #    warning(paste0("The file `",
   #                   sim_data_file,
   #                   "` had a custom dosing regimen for the compound you requested or its parent, which means that PK data for the last dose are NOT in their usual locations.\nWe cannot pull any last-dose PK data for you unless you supply a specific tab using the argument `sheet`."), 
   #            call. = FALSE)
   #    
   #    PKparamDF <- PKparamDF %>% 
   #       filter(!str_detect(PKparameter, "_last"))
   # }
   
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
      ("Inhibitor1" %in% names(Deets) &&
       (complete.cases(Deets$Inhibitor1) &&
        compoundToExtract %in% c("inhibitor 1", "inhibitor 1 metabolite") &&
        Deets$Regimen_inhib == "Multiple Dose")) |
      ("Inhibitor2" %in% names(Deets) && 
       (complete.cases(Deets$Inhibitor2) &&
        compoundToExtract %in% c("inhibitor 2") && 
        Deets$Regimen_inhib2 == "Multiple Dose"))){
      
      PKparamDF <- PKparamDF %>% 
         mutate(SheetAUC = ifelse(PKparameter %in% 
                                     c("AUCt_ratio_dose1", "AUCt_dose1", 
                                       "AUCt_dose1_withInhib",
                                       "CLt_dose1", "CLt_dose1_withInhib", 
                                       "CLt_ratio_dose1",
                                       "Cmax_dose1", "Cmax_dose1_withInhib",
                                       "Cmax_ratio_dose1", 
                                       "tmax_dose1", "tmax_dose1_withInhib", 
                                       "tmax_ratio_dose1"), 
                                  FALSE, SheetAUC))
   }
   
   # Removing any rows where all sheets are labeled as FALSE, i.e., it is not
   # going to be found on any of those sheets.
   
   
   ## I think the snippet below isn't serving a purpose any more other than to
   ## filter out parameters that I do want.
   # if(SheetWasSpecified){
   #    PKparamDF <- PKparamDF %>% 
   #       mutate(across(.cols = c(SheetAUC, SheetAbsorption, SheetRegADAM, 
   #                               SheetAUC, SheetAUC0, SheetAUClast, 
   #                               SheetDrugPop, SheetCLTSS), 
   #                     .fns = function(x) FALSE))
   # } 
   
   PKparamDF <- PKparamDF %>% 
      mutate(AnyTRUE = any(SheetAUC, SheetAbsorption, SheetRegADAM, SheetDrugPop, 
                           SheetAUC0, SheetAUClast, SheetCLTSS, SheetUser)) %>% 
      filter(AnyTRUE == TRUE) %>% select(-AnyTRUE)
   
   if(nrow(PKparamDF) == 0){
      warning(paste0("There were no possible PK parameters to be extracted for ",
                     sim_data_file,
                     ". Please check your input for 'PKparameters'. For example, check that you have not requested steady-state parameters for a single-dose simulation.\n"),
              call. = FALSE)
      return(list())
   }
   
   Out_ind <- list()
   Out_agg <- list()
   TimeInterval <- list()
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
   
   # Need to pull these parameters if
   
   # a) they requested a vector of parameters rather than a set and some of
   # those parameters are present on the AUC tab or 
   
   # b) the user requested the "AUC tab" for PK parameters and either "AUC",
   # "AUC_CI", or "AUC_SD" are among the sheets in the file or
   
   # c) They requested a specific sheet for pulling the PK parameters and that
   # sheet was formatted like the AUC tab.
   
   PKparameters_AUC <- PKparamDF$PKparameter[PKparamDF$SheetAUC == TRUE]
   
   if((length(Tab_AUC) > 0 && complete.cases(Tab_AUC)) &
      length(PKparameters_AUC) > 0){
      
      AUC_xl <- suppressMessages(
         readxl::read_excel(path = sim_data_file, 
                            # If the user requested the "AUC" tab for PK
                            # parameters, it's ok to use the tab "AUC_CI"
                            # if "AUC" is not present.
                            sheet = Tab_AUC,
                            col_names = FALSE))
      
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
      if(Deets$SimulatorUsed == "Simcyp Simulator"){
         # Discovery files only have one tissue available at a time, so it's not
         # an issue.
         
         ColStart <- switch(tissue, 
                            "plasma" = which(str_detect(t(AUC_xl[1, ]), "CPlasma"))[1], 
                            "unbound plasma" = which(str_detect(t(AUC_xl[1, ]), "CuPlasma"))[1], 
                            "blood" = which(str_detect(t(AUC_xl[1, ]), "CBlood"))[1], 
                            "unbound blood" = which(str_detect(t(AUC_xl[1, ]), "CuBlood"))[1])
         if(is.na(ColStart)){
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
         }
         
         AUC_xl <- AUC_xl[, c(1, 2, ColStart:ColEnd)]
      } else {
         # Discovery files
         ColStart <- 3
      }
      
      # Finding the aggregate data rows 
      StartRow_agg <- which(AUC_xl$...2 == "Statistics")[1] + 2 # Need to only get the 1st instance b/c people will sometimes copy and paste additional data -- usually obs -- below!
      EndRow_agg <- which(AUC_xl$...2 == "Std Dev")
      EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1]
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
         ColsStart_subcol <- which(str_detect(as.vector(t(
            AUC_xl[IndexRow - 1, ])), 
            ToDetect$AUCtab_StartColText))[1]
         
         if(length(ColsStart_subcol) == 0){
            ColsStart_subcol <- 1
         }
         
         # Find the last column for this particular subheading
         EndCol <- which(complete.cases(as.vector(t(
            AUC_xl[IndexRow - 1, ]))))
         EndCol <- EndCol[EndCol > ColsStart_subcol][1] - 1
         EndCol <- ifelse(is.na(EndCol), ncol(AUC_xl), EndCol)
         
         if(is.na(ColsStart_subcol)){
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
            PossCol <- ColsStart_subcol:EndCol
            ColNum <- PossCol[which(str_detect(as.vector(t(
               AUC_xl[IndexRow, PossCol])), ToDetect$SearchText))][1]
         }
         
         if(length(ColNum) == 0 || is.na(ColNum)){
            if(any(PKparameters_orig %in% c("all", "AUC tab")) == FALSE){
               warning(wrapn(paste0("The column with information for ", i,
                                    " on the tab 'AUC' cannot be found in the file '", 
                                    sim_data_file, "'.")), 
                       call. = FALSE)
            }
            suppressWarnings(suppressMessages(rm(ToDetect, ColsStart_subcol, EndCol, PossCol, ColNum)))
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
                                    ColStart - 1, # "-1" accounts for the 1st column being 1 and not 0
                                 StartRow_agg = StartRow_agg,
                                 EndRow_agg = EndRow_agg,
                                 StartRow_ind = IndexRow + 1,
                                 EndRow_ind = EndRow_ind,
                                 Note = paste("StartColText is looking in row", IndexRow - 1)))
         
         if(any(is.na(Out_ind[[i]]) & str_detect(i, "inf_(ratio_)?dose1"))){
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
            
            suppressWarnings(rm(ColsStart_subcol, EndCol, ColNum, ToDetect))
            
            ToDetect <- AllPKParameters %>% 
               filter(Sheet == "AUC" & PKparameter == NewParam) %>% 
               select(PKparameter, SearchText, AUCtab_StartColText)
            
            # !!! STARTING HERE, ALL TEXT IS SAME AS MAIN CODE ABOVE.
            
            # Looking for the correct subheading 
            ColsStart_subcol <- which(str_detect(as.vector(t(AUC_xl[IndexRow - 1, ])), 
                                                 ToDetect$AUCtab_StartColText))[1]
            
            if(length(ColsStart_subcol) == 0){
               ColsStart_subcol <- 1
            }
            
            # Find the last column for this particular subheading
            EndCol <- which(complete.cases(as.vector(t(
               AUC_xl[IndexRow - 1, ]))))
            EndCol <- EndCol[EndCol > ColsStart_subcol][1] - 1
            EndCol <- ifelse(is.na(EndCol), ncol(AUC_xl), EndCol)
            
            if(is.na(ColsStart_subcol)){
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
               PossCol <- ColsStart_subcol:EndCol
               ColNum <- PossCol[which(str_detect(as.vector(t(
                  AUC_xl[IndexRow, PossCol])), ToDetect$SearchText))][1]
            }
            
            if(length(ColNum) == 0 || is.na(ColNum)){
               if(any(PKparameters_orig %in% c("all", "AUC tab") == FALSE)){
                  warning(wrapn(paste0("The column with information for ", i,
                                       " on the tab 'AUC' cannot be found in the file '", 
                                       sim_data_file, "'.")), 
                          call. = FALSE)
               }
               suppressWarnings(suppressMessages(rm(ToDetect, ColsStart_subcol, EndCol, PossCol, ColNum)))
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
            
            suppressWarnings(rm(ToDetect, ColsStart_subcol, EndCol, ColNum, PossCol))
            
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
         
         suppressWarnings(rm(ToDetect, ColsStart_subcol, EndCol, ColNum, PossCol))
      }
      
      if(includeTrialInfo & length(PKparameters_AUC) > 0){
         # Subject and trial info
         SubjTrial_AUC <- AUC_xl[(IndexRow + 1):EndRow_ind, 1:2] %>%
            rename("Individual" = ...1, "Trial" = ...2)
         
         Out_ind <- Out_ind[PKparameters_AUC] 
         
         Out_ind[["AUCtab"]] <-
            cbind(SubjTrial_AUC,
                  as.data.frame(Out_ind[which(sapply(Out_ind, length) > 0)]))
      }
      
      # NB: Time interval is in a different location on AUC tab than on AUCX
      # tab. This is correct.
      TimeInterval[[i]] <- data.frame(
         File = sim_data_file, 
         Sheet = Tab_AUC, 
         Interval = gsub("\\(|\\)", "", str_extract(AUC_xl[2, 3], "from.*"))) %>% 
         mutate(Interval = sub("\\.00 ", " ", Interval), 
                PKparameter = i)
      
      # The AUC tab should have included AUCinf if it were a 1st dose PK
      # parameeter, I think, so, if there was no AUCinf column at the end of all
      # the data extraction, that would be because the AUCinf col included NA
      # values. We WOULD want to warn the user of that in that case, so setting
      # things up to yes, get warnings, if the AUCinf column is not present in
      # the final data. There's a slight chance that this will result in
      # redundant warnings, but I think that's OK.
      Warn_if_noAUCinf_col <- TRUE
      
   }
   
   TimeInterval <- bind_rows(TimeInterval)
   
   # Pulling dose 1 data NOT present on AUC tab ------------------------------
   
   # Some PK parameters show up on multiple sheets. No need to pull
   # those here if they've already been pulled from another sheet.
   PKparameters_AUC0 <-
      setdiff(PKparamDF$PKparameter[PKparamDF$SheetAUC0 == TRUE], 
              names(Out_agg))
   
   if(length(PKparameters_AUC0) > 0 & complete.cases(Tab_first) &
      (any(PKparameters_orig %in% c("AUC tab", "Absorption tab")) == FALSE |
       (any(PKparameters_orig == "AUC tab") & "AUC" %in% SheetNames == FALSE) |
       (any(PKparameters_orig == "AUC tab") & compoundToExtract != "substrate"))){
      
      Out_AUC0 <- extractAUCXtab(PKparameters = PKparameters_AUC0, 
                                 PKparameters_orig = PKparameters_orig,
                                 compoundToExtract = compoundToExtract,
                                 sim_data_file = sim_data_file,
                                 Sheet = Tab_first, 
                                 PKset = "AUC0",
                                 UserSpecified = FALSE,
                                 Deets = Deets, 
                                 DataCheck = DataCheck,
                                 includeTrialInfo = includeTrialInfo)
      
      if(length(Out_AUC0) > 0){
         
         names(Out_AUC0$Out_ind)[which(names(Out_AUC0$Out_ind) == "AUCXtab")] <- 
            "AUC0tab"
         
         for(param in c("AUCinf_dose1", "AUCinf_dose1_withInhib", "AUCinf_ratio_dose1")){
            if(param %in% names(Out_AUC0[["Out_ind"]]) && 
               any(is.na(Out_AUC0[["Out_ind"]][[param]]))){
               Out_AUC0[["Out_agg"]][[param]] <- 
                  map(.x = Out_AUC0[["Out_agg"]][[param]], 
                      .f = \(x) NA) %>% unlist()
            }
         }
         
         DataCheck <- DataCheck %>% bind_rows(Out_AUC0$DataCheck)
         Out_agg <- c(Out_agg, Out_AUC0$Out_agg)
         Out_ind <- c(Out_ind, Out_AUC0$Out_ind)
         TimeInterval <- TimeInterval %>% 
            bind_rows(Out_AUC0$TimeInterval)
         
         Warn_if_noAUCinf_col <- Out_AUC0$WarnAUCinf
      }
   }
   
   
   # Pulling last-dose data NOT present on AUC tab ------------------------------
   
   # Some PK parameters show up on multiple sheets. No need to pull
   # those here if they've already been pulled from another sheet.
   PKparameters_AUClast <-
      setdiff(PKparamDF$PKparameter[PKparamDF$SheetAUClast == TRUE], 
              names(Out_agg))
   
   if(length(PKparameters_AUClast) > 0 && complete.cases(Tab_last) &
      any(PKparameters_orig %in% c("Regional ADAM", "Absorption tab")) == FALSE){
      
      Out_AUClast <- extractAUCXtab(PKparameters = PKparameters_AUClast, 
                                    PKparameters_orig = PKparameters_orig,
                                    sim_data_file = sim_data_file,
                                    Sheet = Tab_last, 
                                    PKset = "AUClast",
                                    UserSpecified = FALSE,
                                    Deets = Deets, 
                                    DataCheck = DataCheck,
                                    includeTrialInfo = includeTrialInfo)
      
      names(Out_AUClast$Out_ind)[which(names(Out_AUClast$Out_ind) == "AUCXtab")] <- 
         "AUClasttab"
      
      DataCheck <- DataCheck %>% bind_rows(Out_AUClast$DataCheck)
      Out_agg <- c(Out_agg, Out_AUClast$Out_agg)
      Out_ind <- c(Out_ind, Out_AUClast$Out_ind)
      TimeInterval <- TimeInterval %>% 
         bind_rows(Out_AUClast$TimeInterval)
   }
   
   # Pulling parameters from a user-specified sheet --------------------------
   
   ParamUserDef <-
      setdiff(PKparamDF$PKparameter[PKparamDF$SheetUser == TRUE], 
              names(Out_agg))
   
   if(length(ParamUserDef) > 0){
      
      # Some parameters are not going to be present, so removing those. 
      ParamUserDef <- ParamUserDef[!str_detect(
         ParamUserDef, "^fa_|^ka_|^tlag_|CLinf|CL_hepatic|CL_po|^F_|^Fg_|^Fh_|^fm_|^AUCinf|^HalfLife|^AUCtau|CLtau|Accumulation|BPratio|Qgut|^fu")]
      
      UserSheet <- unique(sheet[complete.cases(sheet)])
      
      Out_AUCX <- extractAUCXtab(PKparameters = ParamUserDef, 
                                 PKparameters_orig = PKparameters_orig,
                                 sim_data_file = sim_data_file,
                                 Sheet = UserSheet, 
                                 PKset = "AUCX",
                                 UserSpecified = TRUE,
                                 Deets = Deets, 
                                 DataCheck = DataCheck,
                                 includeTrialInfo = includeTrialInfo)
      
      DataCheck <- DataCheck %>% bind_rows(Out_AUCX$DataCheck)
      Out_agg <- c(Out_agg, Out_AUCX$Out_agg)
      Out_ind <- c(Out_ind, Out_AUCX$Out_ind)
      TimeInterval <- TimeInterval %>% 
         bind_rows(Out_AUCX$TimeInterval)
      
      rm(Out_AUCX)
      
   }
   
   # Pulling data from the "Absorption" sheet -----------------------------------
   
   # Some PK parameters show up on multiple sheets. No need to pull
   # those here if they've already been pulled from another sheet.
   PKparameters_Abs <-
      setdiff(PKparamDF$PKparameter[PKparamDF$SheetAbsorption == TRUE], 
              names(Out_agg))
   
   if(length(PKparameters_Abs) > 0 &
      any(PKparameters_orig %in% c("AUC tab")) == FALSE){
      
      PKparameters_Abs_ADAM <- intersect(PKparameters_Abs, 
                                         c("fa_sub", "Fg_sub", "fa_apparent_sub"))
      
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
            
            FaFg_xl <- suppressMessages(
               readxl::read_excel(path = sim_data_file, sheet = "Overall Fa Fg",
                                  col_names = FALSE))
            
            SubCols <- 3:5
            # This is IN THE PRESENCE OF AN PERPETRATOR -- not the perpetrator
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
               StartCol <- ifelse(str_detect(i, "withInhib"),
                                  WithInhibCols, SubCols)
               
               ColNum <- which(str_detect(
                  as.character(FaFg_xl[2, StartCol:(StartCol+2)]),
                  ToDetect$SearchText)) + StartCol - 1
               
               if(length(ColNum) == 0 || is.na(ColNum)){
                  if(any(PKparameters_orig %in% c("all", "Absorption tab")) == FALSE){
                     warning(wrapn(paste0("The column with information for ", i,
                                          " on the tab `Overall Fa Fg` cannot be found in the file '", 
                                          sim_data_file, "'.")), 
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
            
            PKparameters_Abs <- setdiff(PKparameters_Abs, unique(c(names(Out_ind), 
                                                                   names(Out_agg))))
            
            if(length(PKparameters_Abs) > 0){
               
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
                  
                  # fa_apparent_sub is not always available.
                  if(nrow(ToDetect) == 0){
                     suppressMessages(rm(ToDetect))
                     PKparameters_Abs <- setdiff(PKparameters_Abs, i)
                     next
                  }
                  
                  # Looking for the regular expression specific to this
                  # parameter i. For the absorption tab, there are columns for
                  # the substrate and columns for Inhibitor 1. (There are also
                  # columns for Inhibitor 2 and 3 but I've never seen them
                  # filled in. -LSh)
                  StartCol <- ifelse(str_detect(i, "sub"),
                                     SubCols, InhibCols)
                  
                  ColNum <- which(str_detect(
                     as.character(Abs_xl[9, StartCol:(StartCol+2)]),
                     ToDetect$SearchText)) + StartCol - 1
                  
                  if(length(ColNum) == 0 || is.na(ColNum)){
                     if(any(PKparameters_orig %in% c("all", "Absorption tab")) == FALSE){
                        warning(wrapn(paste0("The column with information for ", i,
                                             " on the tab 'Absorption' cannot be found in the file '", 
                                             sim_data_file, "'.")), 
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
                     warning(wrapn(paste0("The column with information for ", i,
                                          " on the tab 'Absorption' cannot be found in the file '", 
                                          sim_data_file, "'.")), 
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
   }
   
   
   # Pulling data from the "Drug-Population Parameters" tab --------------------
   
   # Some PK parameters show up on multiple sheets. No need to pull
   # those here if they've already been pulled from another sheet.
   PKparameters_DrugPop <-
      setdiff(PKparamDF$PKparameter[PKparamDF$SheetDrugPop == TRUE], 
              names(Out_agg))
   
   if(length(PKparameters_DrugPop) > 0 &
      any(PKparameters_orig %in% c("AUC tab", "Absorption tab")) == FALSE){
      
      if("Drug-Population Parameters" %in% SheetNames == FALSE){
         # Error catching
         warning(paste0("The sheet `Drug-Population Parameters` was not present in the Excel simulated data file to extract the parameters ",
                        str_comma(PKparameters_DrugPop), "."),
                 call. = FALSE)
      } else {
         
         DrugPop_xl <- suppressMessages(
            readxl::read_excel(path = sim_data_file, sheet = "Drug-Population Parameters",
                               col_names = FALSE))
         
         StartRow_ind <- which(DrugPop_xl$...1 == "Index")
         EndRow_agg <- which(is.na(DrugPop_xl$...3))[2] - 1
         
         # Looping through parameters and extracting values
         for(i in PKparameters_DrugPop){
            
            # Using regex to find the correct column. See
            # data(AllPKParameters) for all the possible parameters as well
            # as what regular expressions are being searched for each. 
            ToDetect <- AllPKParameters %>% 
               filter(Sheet == "Drug-Population Parameters" & PKparameter == i) %>% 
               select(PKparameter, SearchText)
            
            # Looking for the regular expression specific to this parameter
            # i. 
            ColNum <- which(str_detect(as.vector(t(DrugPop_xl[2, ])),
                                       ToDetect$SearchText))
            
            if(length(ColNum) == 0 || is.na(ColNum)){
               if(any(PKparameters_orig %in% c("all", "Absorption tab")) == FALSE){
                  warning(wrapn(paste0("The column with information for ", i,
                                       " on the tab 'Drug-Population Parameters' cannot be found in the file '", 
                                       sim_data_file, "'.")), 
                          call. = FALSE)
               }
               suppressWarnings(suppressMessages(rm(ToDetect, StartCol, EndCol, PossCol, ColNum)))
               PKparameters_DrugPop <- setdiff(PKparameters_DrugPop, i)
               next
            }
            
            suppressWarnings(
               Out_ind[[i]] <- DrugPop_xl[StartRow_ind:nrow(DrugPop_xl), ColNum] %>%
                  pull(1) %>% as.numeric
            )
            
            suppressWarnings(
               Out_agg[[i]] <- DrugPop_xl[3:EndRow_agg, ColNum] %>%
                  pull(1) %>% as.numeric
            )
            names(Out_agg[[i]]) <- DrugPop_xl[3:EndRow_agg, 3] %>%
               pull(1)
            
            
            if(checkDataSource){
               DataCheck <- DataCheck %>%
                  bind_rows(data.frame(PKparam = i, 
                                       Tab = "Drug-Population Parameters",
                                       SearchText = ToDetect$SearchText,
                                       Column = ColNum, 
                                       StartRow_agg = 3,
                                       EndRow_agg = EndRow_agg,
                                       StartRow_ind = StartRow_ind,
                                       EndRow_ind = nrow(DrugPop_xl)))
            }
         }   
         
         if(includeTrialInfo & length(PKparameters_DrugPop) > 0){
            # Subject and trial info
            SubjTrial_DrugPop <- DrugPop_xl[StartRow_ind:nrow(DrugPop_xl), 1:2] %>%
               rename("Individual" = ...1, "Trial" = ...2)
            
            Out_ind[["DrugPoptab"]] <- cbind(SubjTrial_DrugPop,
                                             as.data.frame(Out_ind[PKparameters_DrugPop]))
         }
         
         rm(ColNum, ToDetect)
         
      }
      
      suppressWarnings(rm(EndRow_agg, StartRow_ind))
   }
   
   
   # Pulling data from the "Clearance Trials SS" sheet ------------------------------------------
   
   # Some PK parameters show up on multiple sheets. No need to pull
   # those here if they've already been pulled from another sheet.
   PKparameters_CLTSS <-
      setdiff(PKparamDF$PKparameter[PKparamDF$SheetCLTSS == TRUE], 
              names(Out_agg))
   
   if(length(PKparameters_CLTSS) > 0 &
      any(PKparameters_orig %in% c("AUC tab", "Absorption tab")) == FALSE){
      # Error catching
      if("Clearance Trials SS" %in% SheetNames == FALSE){
         
         warning(wrapn(paste0("The sheet `Clearance Trials SS` was not present in the Excel simulated data file to extract the PK parameters ",
                              str_comma(PKparameters_CLTSS), ", so those will be skipped.")), 
                 call. = FALSE)
         
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
            RowNum <- which(str_detect(CLTSS_xl$...1, "Index"))[1]
            RowNum <- ifelse(length(RowNum) == 0 || is.na(RowNum), 
                             21, RowNum)
            
            ColNum <- which(str_detect(as.vector(t(CLTSS_xl[RowNum, ])),
                                       ToDetect$SearchText))
            
            if(length(ColNum) == 0 || is.na(ColNum)){
               if(any(PKparameters_orig %in% c("all", "Absorption tab")) == FALSE){
                  warning(wrapn(paste0("The column with information for ", i,
                                       " on the tab 'Clearance Trials SS' cannot be found in the file '", 
                                       sim_data_file, "'.")), 
                          call. = FALSE)
               }
               suppressWarnings(suppressMessages(rm(ToDetect, StartCol, EndCol, PossCol, ColNum)))
               PKparameters_CLTSS <- setdiff(PKparameters_CLTSS, i)
               next
            }
            
            suppressWarnings(
               Out_ind[[i]] <- CLTSS_xl[(RowNum + 1):nrow(CLTSS_xl), ColNum] %>%
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
                                       StartRow_ind = (RowNum + 1),
                                       EndRow_ind = nrow(CLTSS_xl)))
            }
         }   
         
         if(includeTrialInfo & length(PKparameters_CLTSS) > 0){
            # Subject and trial info
            SubjTrial_CLTSS <- CLTSS_xl[(RowNum + 1):nrow(CLTSS_xl), 1:2] %>%
               rename("Individual" = ...1, "Trial" = ...2)
            
            Out_ind[["CLTSStab"]] <- cbind(SubjTrial_CLTSS,
                                           as.data.frame(Out_ind[PKparameters_CLTSS]))
         }
         
         # AGGREGATE VALUES: Looping through parameters and extracting values
         for(i in PKparameters_CLTSS){
            
            # Using regex to find the correct column. See
            # data(AllPKParameters) for all the possible parameters as well
            # as what regular expressions are being searched for each. 
            ToDetect <- AllPKParameters %>% 
               filter(Sheet == "Clearance Trials SS" & PKparameter == i) %>% 
               select(PKparameter, SearchText)
            
            # Looking for the regular expression specific to this parameter
            # i. 
            RowNum <- which(str_detect(CLTSS_xl$...1, "Mean"))[1] - 1
            RowNum <- ifelse(length(RowNum) == 0 || is.na(RowNum), 
                             1, RowNum)
            
            ColNum <- which(str_detect(as.vector(t(CLTSS_xl[RowNum, ])),
                                       ToDetect$SearchText))
            
            if(length(ColNum) == 0 || is.na(ColNum)){
               if(any(PKparameters_orig %in% c("all", "Absorption tab")) == FALSE){
                  warning(wrapn(paste0("The column with information for ", i,
                                       " on the tab 'Clearance Trials SS' cannot be found in the file '", 
                                       sim_data_file, "'.")), 
                          call. = FALSE)
               }
               suppressWarnings(suppressMessages(rm(ToDetect, StartCol, EndCol, PossCol, ColNum)))
               PKparameters_CLTSS <- setdiff(PKparameters_CLTSS, i)
               next
            }
            
            # "Fold" should be the penultimate row. There's also "Std Dev", but
            # I think that's not always spelled/abbreviated consistently.
            EndRow_agg <- which(str_detect(tolower(CLTSS_xl$...1), "fold"))[1] + 1
            
            suppressWarnings(
               Out_agg[[i]] <- CLTSS_xl[(RowNum + 1):EndRow_agg, ColNum] %>%
                  pull(1) %>% as.numeric
            )
            names(Out_agg[[i]]) <- CLTSS_xl[(RowNum + 1):EndRow_agg, 1] %>%
               pull(1)
            
            if(checkDataSource){
               DataCheck <- DataCheck %>%
                  bind_rows(data.frame(PKparam = i, 
                                       Tab = "Clearance Trials SS",
                                       SearchText = ToDetect$SearchText,
                                       Column = ColNum, 
                                       StartRow_agg = RowNum + 1,
                                       EndRow_agg = EndRow_agg))
            }
         }  
      }
   }
   
   # Pulling data from RegADAM tab -------------------------
   
   # Some PK parameters show up on multiple sheets. No need to pull
   # those here if they've already been pulled from another sheet.
   PKparameters_RegADAM <-
      setdiff(PKparamDF$PKparameter[PKparamDF$SheetRegADAM == TRUE], 
              names(Out_agg))
   
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
            
            if(length(ColNum) == 0 || is.na(ColNum)){
               if(any(PKparameters_orig %in% c("all", "Regional ADAM")) == FALSE){
                  warning(wrapn(paste0("The column with information for ", i,
                                       " on the tab `Regional ADAM Fractions (Sub)` cannot be found in the file '", 
                                       sim_data_file, "'.")), 
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
   
   if(length(Out_agg) == 0){
      warning(wrapn(paste0("For the file ", sim_data_file, 
                           ", no PK parameters were found. Did you include PK info as part of your simulation output?")), 
              call. = FALSE)
      return(list())
   }
   
   Out_ind <- Out_ind[names(Out_ind)[str_detect(names(Out_ind), "tab$")]]
   
   Out_ind <- bind_rows(Out_ind) %>%
      pivot_longer(cols = -(c(Individual, Trial)),
                   names_to = "PKparameter",
                   values_to = "Value") %>%
      filter(complete.cases(Value)) %>%
      arrange(PKparameter, as.numeric(Trial),
              as.numeric(Individual)) 
   
   suppressWarnings(
      Out_ind <- Out_ind %>% 
         mutate(File = sim_data_file, 
                Tissue = tissue, 
                CompoundID = compoundToExtract, 
                Compound = as.character(Deets[
                   AllCompounds$DetailNames[AllCompounds$CompoundID == compoundToExtract]]), 
                Simulated = TRUE,
                # FIXME - Add DoseNum
                Dose = as.numeric(
                   Deets %>%
                      pull(any_of(paste0("Dose",
                                         AllCompounds$DosedCompoundSuffix[
                                            AllCompounds$CompoundID == compoundToExtract])))))
   )
   
   for(i in names(Out_agg)){
      Statistic_char <- tolower(names(Out_agg[[i]]))
      Out_agg[[i]] <- as.data.frame(Out_agg[[i]]) %>%
         mutate(Statistic = Statistic_char,
                PKparameter = i)
      names(Out_agg[[i]])[1] <- "Value"
   }
   
   suppressWarnings(
      Out_agg <- bind_rows(Out_agg) %>%
         select(PKparameter, Statistic, Value) %>%
         mutate(Statistic = renameStats(OrigStat = Statistic, 
                                        use = "simulator to r"), 
                File = sim_data_file, 
                Inhibitor = 
                   case_when(
                      str_detect(PKparameter, "_withInhib|_ratio") ~ 
                         determine_myperpetrator(Deets, 
                                                 prettify_compound_names = F), 
                      .default = "none"),
                Tissue = tissue, 
                CompoundID = compoundToExtract, 
                Compound = as.character(Deets[
                   AllCompounds$DetailNames[AllCompounds$CompoundID == compoundToExtract]]), 
                Simulated = TRUE,
                # FIXME - Add DoseNum
                Dose = as.numeric(
                   Deets %>%
                      pull(any_of(paste0("Dose",
                                         AllCompounds$DosedCompoundSuffix[
                                            AllCompounds$CompoundID == compoundToExtract])))), 
                # Units are always these for db files
                Units = case_when(str_detect(PKparameter, "AUC") ~ Deets$Units_AUC, 
                                  str_detect(PKparameter, "CL") ~ Deets$Units_CL, 
                                  str_detect(PKparameter, "Cmax") ~ Deets$Units_Cmax, 
                                  str_detect(PKparameter, "tmax") ~ Deets$Units_tmax)) %>% 
         pivot_wider(names_from = Statistic, 
                     values_from = Value) %>% 
         select(File, CompoundID, Compound, Inhibitor, Tissue, Simulated,
                Dose, # DoseNum, 
                PKparameter, # N, 
                matches("CI.*_lower"), matches("CI.*_upper"), 
                any_of(c("Geomean", "GCV",
                         "Per5", "Per95", 
                         "Mean", "SD", "CV", 
                         "Median", "Minimum", "Maximum")))
   )
   
   Out <- list("individual" = Out_ind,
               "aggregate" = Out_agg, 
               "TimeInterval" = TimeInterval)
   
   if(checkDataSource){
      XLCols <- c(LETTERS, paste0("A", LETTERS), paste0("B", LETTERS))
      DataCheck <- DataCheck %>% filter(complete.cases(PKparam)) %>% 
         mutate(File = sim_data_file, 
                Column = XLCols[Column], 
                Individual = paste0(Column, StartRow_ind, ":",
                                    Column, EndRow_ind), 
                Individual = ifelse(is.na(StartRow_ind), NA, Individual)) %>% 
         filter(complete.cases(PKparam)) %>% unique()
      
      StatNames <- c("Geomean", "GCV", "CI90_lower", "CI90_upper", 
                     "Mean", "SD", "Median", "Minimum", "Maximum")
      StatNum <- 1:length(StatNames)
      names(StatNum) <- StatNames
      
      suppressMessages(
         DataCheck_agg <- DataCheck %>% 
            select(File, PKparam, Tab, Column, StartRow_agg, EndRow_agg) %>% 
            left_join(data.frame(File = unique(DataCheck$File), 
                                 Stat = StatNames)) %>% 
            filter(complete.cases(StartRow_agg)) %>% 
            mutate(Row_agg = StatNum[Stat] + StartRow_agg - 1, 
                   Cell = paste0(Column, Row_agg), 
                   Cell = ifelse(is.na(Row_agg), NA, Cell)) %>% 
            select(File, PKparam, Tab, Column, Cell, Stat) %>% 
            pivot_wider(names_from = Stat, values_from = Cell)
      )
      
      suppressMessages(
         DataCheck <- DataCheck %>% left_join(DataCheck_agg)
      )
      
      DataCheck <- DataCheck %>% 
         select(-c(Column, StartRow_ind, EndRow_ind, StartRow_agg, EndRow_agg,
                   StartColText, SearchText, Note)) %>% 
         group_by(PKparam, File, Tab) %>%
         fill(everything(), .direction = "downup") %>%
         select(PKparam, File, Tab, Individual, everything()) %>% unique()
      
      if(class(Out)[1] == "list"){
         Out[["QC"]] <- unique(DataCheck)
      } 
   }
   
   if(exists("Warn_if_noAUCinf_col", inherits = FALSE) == FALSE){
      Warn_if_noAUCinf_col <- TRUE
   }
   
   Out[["WarnAUCinf"]] <- Warn_if_noAUCinf_col
   
   if(returnExpDetails){
      Out[["ExpDetails"]] <- Deets
   }
   
   return(Out)
}

