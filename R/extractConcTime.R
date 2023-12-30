#' Extract concentration-time data from a simulator output Excel file
#'
#' Extracts concentration-time data from simulator output Excel files and,
#' optionally, a separately specified observed data file, and puts all data into
#' a single, tidy data.frame. There are some nuances to how it deals with
#' observed data; please see the details at the bottom of this help file. Not
#' all substrate metabolites, inhibitors, or inhibitor metabolites are available
#' in all tissues. If it's not present in your Excel output, we can't extract it
#' here. For detailed instructions and examples, please see the SharePoint file
#' "Simcyp PBPKConsult R Files - Simcyp PBPKConsult R Files/SimcypConsultancy
#' function examples and instructions/Concentration-time plots 1 - one sim at a
#' time/Concentration-time-plot-examples-1.docx". (Sorry, we are unable to
#' include a link to it here.)
#'
#' \strong{A note on observed data:} When observed data are included in a
#' simulator output file, because the simulator output does not explicitly say
#' whether those observed data were in the presence of an inhibitor or
#' perpetrator, this function cannot tell the difference and will thus assume
#' all observed data included in the simulator output were for the substrate in
#' the \emph{absence} of any perpetrator. It will further assume that the
#' compound -- substrate or inhibitor 1 or primary metabolite 1 or whatever --
#' is the same as \code{compoundToExtract}. If \code{compoundToExtract} was an
#' inhibitor or inhibitor metabolite, the observed data from the simulator
#' output will NOT be pulled since it is unlikely to be inhibitor
#' concentrations.
#'
#' For best results, we recommend supplying an observed data file here, which
#' contains more information, to make sure the data are what you're expecting.
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   concentration-time data, in quotes; must be an output file from the Simcyp
#'   simulator
#' @param obs_data_file name of the Excel file containing the observed
#'   concentration-time data for the substrate or metabolite you're extracting,
#'   in quotes. If the observed data you want to plot were already included in
#'   the Excel output from the simulator, leave this as NA. Otherwise, this is
#'   the file that it is ready to be converted to an XML file, not the file that
#'   contains only the digitized time and concentration data.
#' @param adjust_obs_time TRUE or FALSE (default) for whether to adjust the time
#'   listed in the observed data file to match the last dose administered. This
#'   only applies to multiple-dosing regimens. If TRUE, the graph will show the
#'   observed data overlaid with the simulated data such that the dose in the
#'   observed data was administered at the same time as the last dose in the
#'   simulated data. If FALSE, the observed data will start at whatever times
#'   are listed in the Excel file.
#' @param tissue From which tissue should the desired concentrations be
#'   extracted? Default is plasma for typical plasma concentration-time data.
#'   Other options are "blood" or any tissues included in "Sheet Options",
#'   "Tissues" in the simulator. All possible options:\describe{
#'   \item{First-order absorption models}{"plasma", "blood", "unbound blood",
#'   "unbound plasma", "additional organ", "adipose", "bone", "brain",
#'   "feto-placenta", "gut tissue", "heart", "kidney", "liver", "lung", "muscle",
#'   "pancreas", "peripheral blood", "peripheral plasma", "peripheral unbound
#'   blood", "peripheral unbound plasma", "portal vein blood", "portal vein
#'   plasma", "portal vein unbound blood", "portal vein unbound plasma", "skin",
#'   or "spleen".} \item{ADAM-models}{"stomach", "duodenum", "jejunum I",
#'   "jejunum II", "ileum I", "ileum II", "ileum III", "ileum IV", "colon",
#'   "faeces", "gut tissue", "cumulative absorption", "cumulative fraction
#'   released", or "cumulative dissolution".} \item{ADC simulations}{NOT YET 
#'   SET UP. If you need this, please contact Laura Shireman.}} Not case sensitive.
#' @param compoundToExtract For which compound do you want to extract
#'   concentration-time data? Options are: \itemize{\item{"substrate"
#'   (default),} \item{"primary metabolite 1",} \item{"primary metabolite 2",}
#'   \item{"secondary metabolite",} \item{"inhibitor 1" -- this can be an
#'   inducer, inhibitor, activator, or suppresesor, but it's labeled as
#'   "Inhibitor 1" in the simulator,} \item{"inhibitor 2" for the 2nd inhibitor
#'   listed in the simulation,} \item{"inhibitor 1 metabolite" for the primary
#'   metabolite of inhibitor 1} \item{"conjugated protein" for DAR1-DARmax for
#'   an antibody-drug conjugate; observed data with DV listed as "Conjugated
#'   Protein Plasma Total" will match these simulated data,} \item{"total
#'   protein" for DAR0-DARmax for an ADC; observed data with DV listed as "Total
#'   Protein Conjugate Plasma Total" will match these simulated data,}
#'   \item{"released payload" for the released drug from an ADC, which shows up
#'   as primary metabolite 1 in Simulator output files.}} \strong{Note:} If your
#'   compound is a therapeutic protein or ADC, we haven't tested this very
#'   thoroughly, so please be extra careful to check that you're getting the
#'   correct data.
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated
#'   concentration-time data? Options are "aggregate", "individual", or "both"
#'   (default). Aggregated data are not calculated here but are pulled from the
#'   simulator output rows labeled as "Population Statistics".
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#' @param fromMultFunction INTERNAL USE ONLY. TRUE or FALSE on whether this is
#'   being called on by \code{\link{extractConcTime_mult}}.
#'
#' @return A data.frame of concentration-time data with the following columns:
#'   \describe{
#'
#'   \item{Compound}{the compound whose concentration is listed; this matches
#'   whatever you named your substrate or inhibitor in the simulator}
#'
#'   \item{CompoundID}{the generic name of the compound: "substrate", "inhibitor
#'   1", "primary metabolite 1", etc.}
#'
#'   \item{Inhibitor (as applicable)}{the inhibitor(s) or perpetrator(s) of
#'   interest; this matches whatever you named "Inhibitor 1", "Inhibitor 2", or
#'   "Inhibitor 1 metabolite" in the simulator and will be a concatenation of
#'   all the perpetrators present}
#'
#'   \item{Tissue}{the tissue}
#'
#'   \item{Individual}{the individual for the given profile, which will be a
#'   number for a simulated individual or will be "obs" or "obs+inhibitor" for
#'   observed data, "mean" for the mean data, "geomean" for the geometric mean
#'   data, or "per5" or "per95" for the 5th and 95th percentile data.}
#'
#'   \item{Trial}{the trial number for that set of simulations or "obs", "mean",
#'   etc. for the observed or aggregate data}
#'
#'   \item{Simulated}{TRUE or FALSE for whether the data were simulated}
#'
#'   \item{Time}{the time since the first dose}
#'
#'   \item{Conc}{concentration of the compound listed}
#'
#'   \item{Time_units}{units used for time}
#'
#'   \item{Conc_units}{units used for concentrations},
#'
#'   \item{subsection_ADAM}{type of concentration (only applies to ADAM model
#'   simulations), e.g., "undissolved compound", "free compound in lumen",
#'   "Heff", "absorption rate", "unreleased compound in faeces", "dissolved
#'   compound", "luminal CLint", "cumulative fraction of compound dissolved",
#'   "cumulative fraction of compound released", or "cumulative fraction of
#'   compound absorbed".}
#'
#'   \item{Dose_num}{the dose number}
#'
#'   \item{Dose_int}{the dosing interval. This will be NA for custom-dosing
#'   regimens.}
#'
#'   \item{File}{the simulator output Excel file that was used as the source for
#'   these data} }
#'
#' @import tidyverse
#' @import readxl
#' @export
#'
#' @examples
#' extractConcTime(sim_data_file = "../Example simulator output MD.xlsx")
#'
#' extractConcTime(sim_data_file = "../Example simulator output MD.xlsx",
#'                 returnAggregateOrIndiv = "individual")
#'
#' extractConcTime(sim_data_file = "../Example simulator output MD.xlsx",
#'                 obs_data_file = "../fig1-242-06-001-MD - for XML conversion.xlsx")
#'
#' extractConcTime(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'                 returnAggregateOrIndiv = c("aggregate", "individual"))
#'
#' extractConcTime(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'                 obs_data_file = "../fig1-242-06-001-MD - for XML conversion.xlsx",
#'                 returnAggregateOrIndiv = c("aggregate", "individual"))
#'
#' extractConcTime(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'                 tissue = "lung")
#'
#' 
extractConcTime <- function(sim_data_file,
                            obs_data_file = NA,
                            tissue = "plasma",
                            compoundToExtract = "substrate",
                            returnAggregateOrIndiv = "both",
                            adjust_obs_time = FALSE,
                            existing_exp_details = NA,
                            fromMultFunction = FALSE){
   
   # Error catching ------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if(returnAggregateOrIndiv[1] == "both"){
      returnAggregateOrIndiv <- c("aggregate", "individual")
   }
   
   if(any(c(length(returnAggregateOrIndiv) < 1,
            length(returnAggregateOrIndiv) > 2,
            any(unique(returnAggregateOrIndiv) %in% c("aggregate", "individual", "both") == FALSE)))) {
      stop("You must request 'aggregate', 'individual', or 'both' for the parameter 'returnAggregateOrIndiv'.",
           call. = FALSE)
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   sim_data_file <- ifelse(str_detect(sim_data_file, "xlsx$"), 
                           sim_data_file, paste0(sim_data_file, ".xlsx"))
   
   # If they used the American spelling of feces, change to the British version
   # for compatibility with simulator output
   tissue <- sub("feces", "faeces", tissue)
   
   if(length(tissue) != 1 & fromMultFunction == FALSE){
      stop("You must enter one and only one option for 'tissue'. (Default is plasma.)",
           call. = FALSE)
   }
   
   if(length(compoundToExtract) != 1 & fromMultFunction == FALSE){
      stop("You must enter one and only one option for 'compoundToExtract'. (Default is the substrate.)",
           call. = FALSE)
   }
   
   # Checking that the file is, indeed, a simulator output file.
   SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                          error = openxlsx::getSheetNames(sim_data_file))
   if(all(c("Input Sheet", "Summary") %in% SheetNames) == FALSE){
      # Using "warning" instead of "stop" here b/c I want this to be able to
      # pass through to other functions and just skip any files that
      # aren't simulator output.
      warning(paste("The file", sim_data_file,
                    "does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file.\n"), 
              call. = FALSE)
      return(data.frame())
   }
   
   
   # Main body of function ------------------------------------------------
   
   tissue <- tolower(tissue)
   PossTiss <- c("additional organ", "adipose", "blood", "bone", "brain",
                 "colon", "csf", "cumulative absorption",
                 "cumulative dissolution", "cumulative fraction released",
                 "duodenum", "faeces", "feto-placenta", 
                 "gi tissue", "gut tissue", "heart", 
                 "ileum i", "ileum ii", "ileum iii", "ileum iv",
                 "jejunum i", "jejunum ii", "kidney", "liver", "lung",
                 "milk", "muscle", "pancreas", 
                 "peripheral blood", "peripheral plasma", "peripheral unbound blood", 
                 "peripheral unbound plasma", "pd response",
                 "plasma", "portal vein blood", "portal vein plasma", 
                 "portal vein unbound blood", "portal vein unbound plasma", "skin", 
                 "solid organ", "spleen", "stomach",
                 "tumour volume", 
                 
                 # I thought I had this set up for ADCs, but, in looking at an
                 # example ADC sim from V22, it looks like the output may have
                 # changed. This is NOT ready to go. Set this up once someone
                 # asks for it.
                 
                 # "total protein", "conjugated protein", 
                 # "total antibody", "protein-conjugated antibody", 
                 "unbound blood", "unbound plasma", "urine")
   
   if(tissue %in% PossTiss == FALSE){
      stop("The requested tissue must be plasma, blood, or one of the options listed under 'Sheet Options', 'Tissues' in the Simulator or one of the ADAM model tissues. Please see the help file description for the 'tissue' argument.",
           call. = FALSE)
   }
   
   compoundToExtract <- tolower(compoundToExtract)
   
   MainCompoundIDs <- c("substrate", "primary metabolite 1", "primary metabolite 2",
                        "secondary metabolite",
                        "inhibitor 1", "inhibitor 2", "inhibitor 1 metabolite",
                        "inhibitor 2 metabolite")
   
   ADCCompoundIDs <- c(
      # "total protein"
      # , "conjugated protein"
      # , "released payload"
   )
   
   if(any(compoundToExtract %in% c(MainCompoundIDs, ADCCompoundIDs) == FALSE)){
      stop("The compound for which you requested concentration-time data was not one of the possible options. For 'compoundToExtract', please enter 'substrate', 'primary metabolite 1', 'secondary metabolite', 'inhibitor 1', 'inhibitor 2', or 'inhibitor 1 metabolite'.",
           call. = FALSE)
   }
   
   # Noting whether the tissue was from an ADAM model
   ADAM <- tissue %in% c("stomach", "duodenum", "jejunum i", "jejunum ii", "ileum i",
                         "ileum ii", "ileum iii", "ileum iv", "colon", "faeces", 
                         "cumulative absorption", "cumulative dissolution", 
                         "cumulative fraction released", "gut tissue")
   
   # Getting summary data for the simulation(s)
   if(fromMultFunction || ("logical" %in% class(existing_exp_details) == FALSE)){
      Deets <- harmonize_details(existing_exp_details)[["MainDetails"]] %>% 
         filter(File == sim_data_file)
      
      if(nrow(Deets) == 0){
         existing_exp_details <- extractExpDetails(sim_data_file = sim_data_file, 
                                                   exp_details = "Summary and Input")
         Deets <- existing_exp_details[["MainDetails"]]
      }
      
   } else {
      existing_exp_details <- extractExpDetails(sim_data_file = sim_data_file, 
                                                exp_details = "Summary and Input")
      Deets <- existing_exp_details[["MainDetails"]]
   } 
   
   if(is.null(Deets$PopRepSim) == FALSE && Deets$PopRepSim == "Yes" &
      "aggregate" %in% returnAggregateOrIndiv){
      
      if(all(returnAggregateOrIndiv == "aggregate")){
         warning(paste0("The simulator file supplied, `", 
                        sim_data_file, 
                        "`, is for a population-representative simulation and thus doesn't have any aggregate data. Since you requested only aggregate data, there are no data to return.\n"), 
                 call. = FALSE)
         return(data.frame())
      } else {
         warning(paste0("The simulator file supplied, `", 
                        sim_data_file, 
                        "`, is for a population-representative simulation and thus doesn't have any aggregate data. Please be warned that some plotting functions will not work well without aggregate data.\n"),
                 call. = FALSE)
      }
   }
   
   # Noting whether this was animal data
   Animal <- str_detect(tolower(Deets$Species), "monkey|rat|mouse|dog|beagle")
   Animal <- ifelse(is.na(Animal), FALSE, Animal)
   
   # Perpetrator present?
   PerpPresent <- complete.cases(Deets$Inhibitor1)
   if(PerpPresent == FALSE & any(str_detect(compoundToExtract, "inhibitor"))){
      # Giving user an empty data.frame rather than stopping so that this will
      # work with either single or multiple version of function.
      warning(paste0("There are no inhibitor data in `", 
                     sim_data_file, "`. Please either submit a different Simulator output file or request concentration-time data for a substrate or metabolite.\n"),
              call. = FALSE)
      return(data.frame())
   }
   
   AllPerpsPresent <- c(Deets$Inhibitor1, Deets$Inhibitor2, Deets$Inhibitor1Metabolite)
   AllPerpsPresent <- AllPerpsPresent[complete.cases(AllPerpsPresent)]
   
   # Noting all compounds present
   AllCompoundsPresent <- c("substrate" = Deets$Substrate,
                            "primary metabolite 1" = Deets$PrimaryMetabolite1, 
                            "primary metabolite 2" = Deets$PrimaryMetabolite2,
                            "secondary metabolite" = Deets$SecondaryMetabolite,
                            "inhibitor 1" = Deets$Inhibitor1,
                            "inhibitor 2" = Deets$Inhibitor2,
                            "inhibitor 1 metabolite" = Deets$Inhibitor1Metabolite)
   AllCompoundsPresent <- AllCompoundsPresent[complete.cases(AllCompoundsPresent)]
   AllCompoundsID <- names(AllCompoundsPresent)
   
   # For matching the correct perpetrator to the correct names in the output file.
   # NB: I made inhibitor 1 be "PERPETRATOR1INHIB" rather than just "PERPETRATOR1"
   # for ease of regex. If it's just "PERPETRATOR1" then, later, the regex will
   # match both "PERPETRATOR1" and "PERPETRATOR1METABOLITE".
   Eff_DF <- data.frame(CompoundID = c("inhibitor 1", "inhibitor 2",
                                       "inhibitor 1 metabolite"),
                        Compound = c(Deets$Inhibitor1,
                                     Deets$Inhibitor2,
                                     Deets$Inhibitor1Metabolite), 
                        COMPOUND = c("PERPETRATOR1INHIB", "PERPETRATOR2", 
                                     "PERPETRATOR1MET"))
   COMPOUND_regex <- str_c(Eff_DF %>% filter(complete.cases(Compound)) %>%
                              pull(COMPOUND), collapse = "|")
   
   # Extracting tissue or plasma/blood data? Sheet format differs.
   TissueType <- ifelse(str_detect(tissue, "plasma|blood|portal|peripheral"),
                        "systemic", "tissue")
   if(any(str_detect(compoundToExtract, "metabolite")) & TissueType == "tissue"){
      warning("You have requested metabolite concentrations in a solid tissue, which the simulator does not provide. Substrate or Inhibitor 1 concentrations will be provided instead, depending on whether you requested a substrate or inhibitor metabolite.\n",
              call. = FALSE)
      compoundToExtract <- compoundToExtract[!str_detect(compoundToExtract,
                                                         "metabolite")]
   }
   
   CompoundType <-
      data.frame(PossCompounds =
                    c("substrate", "inhibitor 1",
                      "inhibitor 2", "inhibitor 1 metabolite",
                      "primary metabolite 1",
                      "primary metabolite 2",
                      "secondary metabolite")) %>%
      mutate(Type = ifelse(PossCompounds %in%
                              c("substrate", "inhibitor 1",
                                "inhibitor 2", "inhibitor 1 metabolite"),
                           "substrate", PossCompounds)) %>%
      # bind_rows(data.frame(PossCompounds = ADCCompoundIDs, 
      #                      Type = "ADC")) %>% 
      filter(PossCompounds %in% compoundToExtract) %>% pull(Type) %>% 
      unique()
   
   if("SimulatorUsed" %in% names(Deets) && Deets$SimulatorUsed == "Simcyp Discovery"){
      if(compoundToExtract %in% c("substrate", 
                                  "primary metabolite 1") == FALSE){
         warning(paste0("This seems to be a Simcyp Discovery simulation, and the only compunds you can extract from that are `substrate` or `primary metabolite 1`, and you requested `", 
                        compoundToExtract, "`. We'll return substrate concentrations instead.\n"), 
                 call. = FALSE)
         compoundToExtract <- "substrate"
      }
      
      Sheet <- switch(compoundToExtract, 
                      "substrate" = 
                         switch(tissue, 
                                "plasma" = "Conc Profiles", 
                                "liver" = "Liver Conc Profiles", 
                                "portal vein" = "PV Conc Profiles"), 
                      "primary metabolite 1" =
                         switch(tissue, 
                                "plasma" = "Sub Pri Metab Conc Profiles"))
      
      if(is.null(Sheet)){
         warning("The combination of compound ID and tissue you requested is not availble for Simcyp Discovery files. Please contact the R Working Group if you think it should be.\n", 
                 call. = FALSE)
         
         return(data.frame())
      }
      
   } else if(TissueType == "systemic"){
      
      # Only looking for only sheets with conc-time data and not AUC, etc.
      PossSheets <- SheetNames[
         !str_detect(tolower(SheetNames), "auc|absorption|summary|ode state|demographic|fm and fe|input|physiology|cl profiles|^cl |^clint|clearance|clinical|cmax|cyp|ugt|population|pk( )?pd parameters|tmax|vss")
      ]
      
      if(CompoundType == "ADC"){
         PossSheets <- PossSheets[
            str_detect(PossSheets, 
                       switch(compoundToExtract, 
                              "conjugated protein" = "Conc Profiles C[Ss]ys|Protein Conc Trials", 
                              "total protein" = "Conc Profiles C[Ss]ys|Protein Conc Trials", 
                              "released payload" = paste0("Sub Pri Met1.*",
                                                          str_to_title(tissue))
                       ))]
         
         Sheet <- PossSheets[1]
         
      } else {
         
         # Searching for correct tissue
         PossSheets <- PossSheets[
            str_detect(tolower(PossSheets), 
                       switch(tissue, 
                              "plasma" = "cplasma",
                              "unbound plasma" = "cuplasma",
                              "peripheral plasma" = "cuplasma",
                              "peripheral unbound plasma" = "cuplasma",
                              "portal vein plasma" = "cplasma",
                              "portal vein unbound plasma" = "cuplasma",
                              
                              "blood" = "cblood",
                              "unbound blood" = "cublood",
                              "peripheral blood" = "cblood",
                              "peripheral unbound blood" = "cublood",
                              "portal vein blood" = "cblood",
                              "portal vein unbound blood" = "cublood"))]
         
         # add criteria for peripheral, pv when needed
         if(str_detect(tissue, "peripheral|portal vein")){
            Cond1 <- str_extract(tissue, "peripheral|portal vein")
            PossSheets <- PossSheets[
               str_detect(tolower(PossSheets), 
                          switch(Cond1,
                                 "peripheral" = "periph", 
                                 "portal vein" = "^pv"))]
         }
         
         # Searching for correct compound. Substrate, inhibitor 1, inhibitor 1
         # metabolite, and inhibitor 2 concentrations will all be on the main
         # concentration-time data tab, but other compounds will be on separate
         # tabs. 
         if(any(compoundToExtract %in%  c("primary metabolite 1",
                                          "primary metabolite 2",
                                          "secondary metabolite"))){
            PossSheets <- PossSheets[
               switch(compoundToExtract[1], 
                      "primary metabolite 1" = 
                         str_detect(tolower(PossSheets), "sub met|sub pri met1") & 
                         !str_detect(tolower(PossSheets), "sub met2"),
                      "primary metabolite 2" = 
                         str_detect(tolower(PossSheets), "sub met2|sub pri met2"), 
                      "secondary metabolite" = 
                         str_detect(tolower(PossSheets), "sub sm|sub sec met")
               )]
         }
         
         Sheet <- PossSheets[1]
      }
      
   } else {
      
      # when tissue is not systemic: 
      
      SheetToDetect <-
         switch(tissue,
                "gi tissue" = "Gut Tissue Conc",
                "gut tissue" = "Gut Tissue Conc", 
                "git" = "Gut Tissue Conc",
                "lung" = "Lung Conc",
                "additional organ" = "Additional Organ Conc",
                "adipose" = "Adipose Conc",
                "heart" = "Heart Conc",
                "muscle" = "Muscle Conc",
                "bone" = "Bone Conc",
                "kidney" = "Kidney Conc",
                "skin" = "Skin Conc",
                "pancreas" = "Pancreas Conc",
                "brain" = "Brain Conc",
                "liver" = "Liver Conc",
                "spleen" = "Spleen Conc",
                "feto-placenta" = "Feto-Placenta", # Need to check this one. I don't have an example output file for this yet!
                "stomach" = "Stomach Prof",
                "duodenum" = "Duodenum Prof",
                "jejunum i" = "Jejunum I Prof",
                "jejunum ii" = "Jejunum II Prof",
                "ileum i" = "Ileum I Prof",
                "ileum ii" = "Ileum II Prof",
                "ileum iii" = "Ileum III Prof",
                "ileum iv" = "Ileum IV Prof",
                "colon" = "Colon Prof",
                "faeces" = "Faeces Prof",
                "feces" = "Faeces Prof",
                "cumulative absorption" = "Cumulative Abs",
                "cumulative fraction released" = "CR Profile",
                "cumulative dissolution" = paste0("Cum.*Dissolution.*",
                                                  switch(CompoundType, # FIXME - I don't know where this info is or what sheet names to expect.
                                                         "substrate" = "Sub",
                                                         "inhibitor 1" = "Inhib")) # Need to check this for inhibitor 1 ADAM model data. This is just my guess as to what the sheet name will be!
         )
      
      if(tissue == "faeces"){ 
         SheetToDetect <- switch(compoundToExtract, 
                                 "inhibitor 1" = "Faeces Prof. .Inh 1", 
                                 # inhibitor 2 does not appear to be available
                                 "substrate" = "Faeces Prof. .Sub")
      }
      
      Sheet <- SheetNames[str_detect(SheetNames, SheetToDetect)][1]
      
   }
   
   if(length(Sheet) == 0 | is.na(Sheet) | Sheet %in% SheetNames == FALSE){
      warning(paste0("You requested data for ", str_comma(compoundToExtract),
                     " in ", tissue,
                     " from the file `",
                     sim_data_file, "``, but that compound and/or tissue or that combination of compound and tissue is not available in that file and will be skipped.\n"),
              call. = FALSE)
      return(data.frame())
   }
   
   # Reading in simulated concentration-time profile data
   sim_data_xl <- suppressMessages(
      readxl::read_excel(path = sim_data_file,
                         sheet = Sheet,
                         col_names = FALSE))
   
   # Setting up data for when the compound name might have regex-unfriendly
   # characters or they use a not-terribly-consistent way of coding which
   # perpetrator it is with a number. These steps depend on whether this is
   # ADAM-model or advanced brain-model data, which will only have one compound
   # ID per sheet, or regular data, which could have multiple compound IDs on
   # the same sheet. If the latter, need to do some data harmonizing to make
   # sure that regex works and picks up the correct CompoundID.
   
   AdvBrainModel <- any(str_detect(sim_data_xl$...1, "Intracranial"), na.rm = TRUE)
   
   # Harmonizing compound names
   if(ADAM == FALSE & AdvBrainModel == FALSE &
      ("SimulatorUsed" %in% names(Deets) && 
       Deets$SimulatorUsed == "Simcyp Simulator")){
      # If "interaction" or "Csys" or other similar strings are part of the name
      # of any of the compounds, that messes up the regex. Substituting to
      # standardize the compound names. Also need to consider the possibility
      # that user may have had to hack things and may have the same compound in
      # multiple positions. This is a MESS of code... Could I do this in some
      # better manner???
      
      # Scenario 1: Systemic tissue, no perpetrator
      # CSys will be listed for all concs. Each compound ID will be on its own tab.
      
      # Scenario 2: Systemic tissue, + perpetrator
      # Substrate: CSys for substrate alone, CSys + interaction for substrate + perp,
      # Each metabolite will be on its own tab. All concs labeled as CSys.
      # Perpetrators: ISys for inhibitor 1 concs, ISys 2 for inhibitor 2 concs, ISys and some other number for inhibitor metabolite concs. Not clear how they pick the number for the metabolite concs.
      
      # Scenario 3: Solid tissue, no perpetrator
      # Many concs possible. 
      # Adipose, Bone, Brain (except when AdvBrainModel), Gut, Heart, Kidney, Liver, Lung, Muscle, Skin, Spleen, Pancreas: CTissue
      
      # Scenario 4: Solid tissue, + perpetrator
      # Adipose, Bone, Brain (except when AdvBrainModel), Gut, Heart, Kidney, Liver, Lung, Muscle, Skin, Spleen, Pancreas: CTissue or CTissue + Interaction for substrate, ITissue(Inh X) for inhibitor. This will be 1 for inhibitor 1 but need to check on other perp compound IDs. 
      
      NApos <- which(is.na(sim_data_xl$...1))
      
      # Looking for all possible compounds. If there is an inhibitor, this will
      # include substrate alone as well as substrate + interaction.
      CmpdMatches1 <- sim_data_xl$...1[(NApos[1] + 1):(NApos[2]-1)] 
      CmpdMatches1 <- CmpdMatches1[!str_detect(CmpdMatches1, "Trial")]
      
      # If the compound is not on the same tab as the substrate, then removing
      # all the "Trial" rows removes all the rows with the compound name.
      # Adjusting for that. FIXME - Will this work for ADAM tissues???
      if(any(compoundToExtract %in% c("primary metabolite 1", "primary metabolite 2", 
                                      "secondary metabolite"))){
         CmpdMatches1 <- rep(AllCompoundsPresent[[compoundToExtract]], length(CmpdMatches1))
      }
      
      # Next, need to figure out which combination of CSys and ISys 1 or ISys 3 or
      # whatever number belongs to which actual compound. Looking for what
      # compounds were listed under "Population Statistics" b/c that's where they 
      # use that kind of coding. 
      StartRow <- which(str_detect(sim_data_xl$...1, "Population Statistics"))[1]
      EndRow <- which(str_detect(sim_data_xl$...1, "Individual Statistics"))[1]-1
      EndRow <- max(which(complete.cases(sim_data_xl$...1[1:EndRow])))
      CmpdMatches2 <- sim_data_xl$...1[StartRow:EndRow]
      CmpdMatches2 <- CmpdMatches2[which(str_detect(CmpdMatches2, "C(Sys|liver|pv)(.*interaction)?|I(Sys|liver|pv) [1-9]?"))]
      CmpdMatches2 <- str_trim(str_extract(CmpdMatches2, "C(Sys|liver|pv)(.*interaction)?|I(Sys|liver|pv) [1-9]?"))
      CmpdMatches2 <- CmpdMatches2[complete.cases(CmpdMatches2)]
      CmpdMatches2[str_detect(CmpdMatches2, "\\+( )?interaction")] <- 
         paste(str_extract(CmpdMatches2[str_detect(CmpdMatches2, "\\+( )?interaction")], "[CI](Sys|liver|pv)"), 
               "interaction")
      # Last step: Find the unique versions of the coding. 
      CmpdMatches2 <- unique(CmpdMatches2)
      
      if(PerpPresent == FALSE &
         tissue %in% c("plasma", "blood", "unbound plasma", "unbound blood") == FALSE){
         CmpdMatches2 <- CmpdMatches1
      }
      
      if(length(CmpdMatches1) != length(CmpdMatches2)){
         warning("PLEASE TELL LAURA SHIREMAN YOU SAW AN ERROR CALLED `COMPOUNDCODE` WHEN TRYING TO EXTRACT CONCENTRATION TIME DATA")
      }
      
      AllCompoundsInv <- names(AllCompoundsPresent)
      names(AllCompoundsInv) <- AllCompoundsPresent
      # This works fine as long as there are no duplicate compounds, e.g., Drug
      # X is both the primary metabolite 1 AND inhibitor 1, which CAN happen
      # when we need to hack things in the Simulator. Need to filter to retain
      # ONLY compounds in compoundsToExtract or the function glitches farther
      # down.
      AllCompoundsInv <- AllCompoundsInv[AllCompoundsInv %in% compoundToExtract]
      
      # Admittedly, this step here where we say that CmpdMatches1, which is the
      # actual compound names, is going to be in the same order as CmpdMatches2,
      # which lists the coded versions of the compounds, makes me nervous just
      # b/c it's coding by index and the two items aren't perfectly matched --
      # we had to remove a bunch of excess junk in between them. So far, though,
      # I haven't found an example of this failing. The order that compounds are
      # listed -- whether by their actual names or by their coded names -- seems
      # to be the same always.
      
      CmpdMatches <- data.frame(NamesInExcel = CmpdMatches1, 
                                CompoundCode = CmpdMatches2) %>% 
         mutate(CompoundName = sub("( )?\\+( )?[Ii]nteraction", "", NamesInExcel), 
                CompoundID = AllCompoundsInv[CompoundName], 
                CompoundID = ifelse(str_detect(CompoundCode, "I(Sys|liver|pv)") & 
                                       CompoundID %in% c("substrate", 
                                                         "primary metabolite 1", 
                                                         "primary metabolite 2", 
                                                         "secondary metabolite"), 
                                    AllCompoundsInv[str_detect(AllCompoundsInv, "inhibitor")][CompoundName], 
                                    CompoundID))
      
      # For V21 (and maybe others), when there's only a substrate present and
      # maybe when it's single dose, they don't list the compound name, only,
      # e.g., "CPlasma".
      if(all(is.na(CmpdMatches$CompoundID)) & 
         length(AllCompoundsPresent) == 1){
         CmpdMatches$CompoundID <- AllCompoundsID
      }
      
      rm(CmpdMatches1, CmpdMatches2, NApos, StartRow, EndRow, AllCompoundsInv)
      
      for(cmpd in compoundToExtract){
         
         if(complete.cases(Deets[AllCompounds$DetailNames[AllCompounds$CompoundID == cmpd]]) &
            cmpd %in% CmpdMatches$CompoundID){
            PossRows <- intersect(
               which(str_detect(sim_data_xl$...1, 
                                str_c(CmpdMatches$CompoundCode[
                                   which(CmpdMatches$CompoundID == cmpd)], 
                                   collapse = "|"))), 
               which(str_detect(sim_data_xl$...1, 
                                str_c(CmpdMatches$CompoundName[
                                   which(CmpdMatches$CompoundID == cmpd)], 
                                   collapse = "|"))))
            
            sim_data_xl$...1[PossRows] <- 
               sub(gsub("\\(|\\)|\\-", ".", 
                        Deets[AllCompounds$DetailNames[AllCompounds$CompoundID == cmpd]]),
                   switch(cmpd, 
                          "substrate" = "SUBSTRATE",
                          "primary metabolite 1" = "PRIMET1", 
                          "primary metabolite 2" = "PRIMET2", 
                          "secondary metabolite" = "SECMET", 
                          "inhibitor 1" = "PERPETRATOR1INHIB", 
                          "inhibitor 2" = "PERPETRATOR2", 
                          "inhibitor 1 metabolite" = "PERPETRATOR1MET"), 
                   sim_data_xl$...1[PossRows])
            
            # Weird extra sub step below will make things work right even if
            # inhibitor 1 metabolite name included the name of inhibitor 1, e.g.,
            # when inhibitor 1 is "Carbamazepine" and inhibitor 1 metabolite is
            # "Carbamazepine-10,11-epoxide". Probably should add this for other
            # possible similar situations w/other compound IDs. <-- NOPE. This is
            # NOT working. 
            # FIXME
            
            # if(cmpd == "inhibitor 1 metabolite"){
            #    sim_data_xl$...1[PossRows] <-
            #       sub(gsub("\\(|\\)|\\-", ".",
            #                sub(Deets$Inhibitor1, "PERPETRATOR1INHIB", Deets$Inhibitor1Metabolite)),
            #           "PERPETRATOR1MET", sim_data_xl$...1[PossRows])
            # }
            
            rm(PossRows)
            
         } else if(cmpd %in% compoundToExtract){
            compoundToExtract <- setdiff(compoundToExtract, cmpd)
         }
      }
   }
   
   # Determining concentration and time units. This will be NA for most ADAM
   # tissues. For ADC compounds, this will be NA here but we'll fix that later.
   SimConcUnits <- as.character(
      sim_data_xl[2, which(str_detect(as.character(sim_data_xl[2, ]),
                                      "CMax"))])[1]
   SimConcUnits <- gsub("CMax \\(|\\)", "", SimConcUnits)
   
   # # ADAM options available (this is for my reference and was copied from ct_plot.R)
   # ADAMoptions <- c("undissolved compound", "free compound in lumen",
   #                  "Heff", "absorption rate",
   #                  "unreleased compound in faeces", 
   #                  "dissolved compound", "luminal CLint", 
   #                  "cumulative fraction of compound absorbed", 
   #                  "cumulative fraction of compound dissolved")
   
   # ADAM and AdvBrainModel have different units depending on what parameter you
   # want.
   if(ADAM|AdvBrainModel){
      PopStatRow <- which(sim_data_xl$...1 == "Population Statistics")
      Blank1 <- which(is.na(sim_data_xl$...1))
      Blank1 <- Blank1[Blank1 > PopStatRow][1]
      
      # For cumulative release and for gut tissue, the tab in Excel is laid out
      # slightly differently (because YOLO so why not?), and the row that
      # contains the units is located elsewhere. Just setting it instead.
      SimConcUnits <- sim_data_xl[PopStatRow:(Blank1 -1), 1] %>% 
         rename(OrigVal = ...1) %>% 
         mutate(TypeCode = str_extract(
            OrigVal, 
            "^Ms|^Dissolution Rate Solid State|^C Lumen Free|^C Lumen Total|^Heff|^Absorption Rate|^Mur|^Md|^Inh Md|^Luminal CLint|CTissue|ITissue|dissolved|absorbed|^C Enterocyte|Release fraction|CIntracranial|CBrainI[CS]F|CCSF(Spinal|Cranial)|Kpuu_I[CS]F|Kpuu_BrainMass|CTotalBrain"), 
            Type = case_match(TypeCode, 
                              "Ms" ~ "undissolved compound", 
                              "Dissolution Rate Solid State" ~ "dissolution rate of solid state",
                              "C Lumen Free" ~ "free compound in lumen", 
                              "C Lumen Total" ~ "total compound in lumen", 
                              "Heff" ~ "Heff", 
                              "Absorption Rate" ~ "absorption rate", 
                              "Mur" ~ "unreleased compound in faeces", 
                              "Inh Mur" ~ "unreleased compound in faeces", 
                              "Md" ~ "dissolved compound", 
                              "Luminal CLint" ~ "luminal CLint", 
                              "absorbed" ~ "cumulative fraction of compound absorbed", 
                              "dissolved" ~ "cumulative fraction of compound dissolved", 
                              "C Enterocyte" ~ "enterocyte concentration", 
                              "CTissue" ~ "tissue concentration", 
                              "ITissue" ~ "tissue concentration", 
                              "CIntracranial" ~ "intracranial", 
                              "CBrainICF" ~ "brain ICF", # ICF = intracellular fluid?
                              "CBrainISF" ~ "brain ISF", # ISF = interstitial fluid?
                              "CCSFSpinal" ~ "spinal CSF", # CSF = cerebrospinal fluid, I assume
                              "CCSFCranial" ~ "cranial CSF", 
                              "CTotalBrain" ~ "total brain", 
                              "Kpuu_BrainMass" ~ "Kp,uu,brain", # unbound brain-to-plasma partition coefficient
                              "Kpuu_ICF" ~ "Kp,uu,ICF", 
                              "Kpuu_ISF" ~ "Kp,uu,ISF"), 
            ConcUnit = str_extract(OrigVal, "mg/h|mg/L|mg/mL|µg/L|µg/mL|ng/L|ng/mL|µM|nM|mg|µg|ng|mmol|µmol|nmol|mM|L/h|mg/h|Cumulative fraction"), 
            # Making "Cumulative" lower case
            ConcUnit = sub("Cumulative", "cumulative", ConcUnit), 
            # Heff unit is um but isn't included in the title
            ConcUnit = ifelse(TypeCode == "Heff", "µm", ConcUnit), 
            # Kpuu_BrainMass should be unitless, but it gets set to "nM" since those letters appear in that column. Fixing.
            ConcUnit = ifelse(TypeCode == "Kpuu_BrainMass", NA, ConcUnit)) %>% 
         group_by(TypeCode) %>% 
         fill(ConcUnit, .direction = "down") %>% ungroup() %>% 
         filter(complete.cases(TypeCode)) %>% select(-OrigVal) %>% unique()
      
      if("cumulative fraction released" %in% tissue){
         SimConcUnits <- bind_rows(SimConcUnits %>% 
                                      mutate(across(.fns = as.character)), 
                                   data.frame(ConcUnit = "cumulative fraction", 
                                              TypeCode = "Release Fraction",
                                              Type = "cumulative fraction released"))
      }
      
      if("gut tissue" %in% tissue){
         SimConcUnits$ConcUnit[SimConcUnits$Type == "tissue concentration"] <- 
            Deets$Units_Cmax
      }
   }
   
   SimTimeUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "^Time"))][1]
   SimTimeUnits <- ifelse(str_detect(SimTimeUnits, "Time.* \\(h\\)"), "hours", "days")
   
   
   # Extracting each compound ----------------------------------------------
   
   # Note: This is a loop for use by extractConcTime_mult. For just running
   # extractConcTime, this will only have a single iteration.
   
   sim_data_mean <- list()
   sim_data_ind <- list()
   
   for(cmpd in compoundToExtract){
      
      if(fromMultFunction){
         message(paste("Extracting data for compound ID =", cmpd))
      }
      
      # "NotAvail" is a hack to skip this iteration of the loop if it's ADAM
      # concentrations that the user requested but they're not available for
      # this tissue/compound combination.
      NotAvail <- FALSE
      
      if(ADAM & cmpd != "substrate"){ # May need to add AdvBrainModel. Not sure.
         if(tissue %in% c("faeces", "gut tissue")){
            if(cmpd != "inhibitor 1"){
               if(fromMultFunction){
                  warning(paste0(str_to_title(cmpd), 
                                 " concentrations are not available for ",
                                 tissue, " and thus will not be extracted.\n"),
                          call. = FALSE)
                  NotAvail <- TRUE
               } else {
                  stop(paste0(str_to_title(cmpd), 
                              " concentrations are not available for ",
                              tissue, " and thus cannot be extracted.\n"),
                       call. = FALSE)
               }
            }
         } else {
            if(fromMultFunction){
               warning(paste0(str_to_title(cmpd), 
                              " concentrations are not available for ",
                              tissue, " and thus will not be extracted.\n"),
                       call. = FALSE) 
               NotAvail <- TRUE
            } else {
               stop(paste0(str_to_title(cmpd), 
                           " concentrations are not available for ",
                           tissue, " and thus cannot be extracted.\n"),
                    call. = FALSE) 
            }
         }
      }
      
      # Here's where we're skipping this iteration if it's ADAM and that
      # tissue/compound combo isn't available.
      if(NotAvail){
         rm(NotAvail)
         next
      }
      
      MyCompound <- 
         switch(paste(cmpd, TissueType),
                "substrate systemic" = Deets$Substrate,
                "substrate tissue" = Deets$Substrate,
                "inhibitor 1 systemic" = Deets$Inhibitor1,
                "inhibitor 1 tissue" = Deets$Inhibitor1,
                "inhibitor 2 systemic" = Deets$Inhibitor2,
                "inhibitor 2 tissue" = Deets$Inhibitor2,
                "inhibitor 1 metabolite systemic" = Deets$Inhibitor1Metabolite,
                "inhibitor 2 systemic" = Deets$Inhibitor2,
                "inhibitor 2 tissue" = Deets$Inhibitor2,
                "primary metabolite 1 systemic" = Deets$PrimaryMetabolite1,
                "primary metabolite 2 systemic" = Deets$PrimaryMetabolite2,
                "secondary metabolite systemic" = Deets$SecondaryMetabolite,
                "primary metabolite 1 tissue" = Deets$PrimaryMetabolite1,
                "primary metabolite 2 tissue" = Deets$PrimaryMetabolite2,
                "secondary metabolite tissue" = Deets$SecondaryMetabolite,
                # inhibitor 1 metabolite concs aren't available in
                # tissues, are they? Giving the user the Inhibitor
                # instead b/c I don't think they are.
                "inhibitor 1 metabolite tissue" = Deets$Inhibitor1) %>%
         as.character()
      
      if(CompoundType == "ADC"){
         MyCompound <- switch(cmpd, 
                              "total protein" = paste("total", Deets$Substrate),
                              "conjugated protein" = paste("conjugated", Deets$Substrate),
                              "released payload" = Deets$PrimaryMetabolite1)
      } 
      
      if(PerpPresent){
         # When the simulator output is for an inhibitor, for reasons I
         # *cannot fathom*, they include a number after "ISys" to designate
         # which inhibitor the data pertain to and, *sometimes*, they will
         # list the name of the inhibitor and *sometimes* they will only list
         # that number *and* that number is not necessarily "1" for
         # "inhibitor 1" and "2" for "inhibitor 2". I cannot determine
         # exactly how they decide what that number will be, so we need to
         # figure out what that number is, assign it to the correct
         # inhibitor, and extract appropriately.
         TimeRow <- which(str_detect(sim_data_xl$...1,
                                     "^Time.*Inhibitor "))[1]
         if(is.na(TimeRow)){ # This occurs when the tissue is not systemic or maybe also in versions > 20
            TimeRow <- which(str_detect(sim_data_xl$...1,
                                        "Time "))
            TimeRow <- TimeRow[which(str_detect(sim_data_xl$...1[TimeRow + 1],
                                                "^I|^CTissue"))][1]
         }
         
         # If TimeRow is still NA, it doesn't apply so ok to skip the rest of
         # this section.
         if(complete.cases(TimeRow)){
            
            # Figuring out which rows contain which data
            FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                    which(1:nrow(sim_data_xl) > TimeRow))[1]
            FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
            NamesToCheck <- sim_data_xl$...1[(TimeRow+1):(FirstBlank-1)]
            
            temp <- data.frame(Name = NamesToCheck[
               which(str_detect(NamesToCheck, COMPOUND_regex))]) %>% 
               mutate(Number = str_extract(Name, "ISys [0-9]|I[a-z]* [0-9]|InhM"),
                      Inhibitor =
                         str_extract(Name, COMPOUND_regex)) %>%
               select(Number, Inhibitor) %>% unique()
            NumCheck <- temp$Number
            names(NumCheck) <- temp$Inhibitor
            
            # If this is a tissue, NumCheck will have length 0
            if(length(NumCheck) == 0){
               NumCheck <- "inhibitor 1"
               names(NumCheck) <- Deets$Inhibitor1
            }
            
            rm(temp, TimeRow, FirstBlank, NamesToCheck)
         } 
      }
      
      # ADAM and AdvBrainModel data include multiple types of concentrations.
      # Dealing with that and extracting each one.
      if(ADAM | AdvBrainModel){
         # Getting the correct ADAM-model subsections and also making "Cumulative"
         # for cumulative absorption/dissolution lower case. Using subsection_ADAM
         # also for AdvBrainModel tissues. May need to changes subsection_ADAM to
         # "TissueType" or something.
         subsection_ADAMs <- sub("Cumulative", "cumulative", SimConcUnits$Type)
      } else {
         subsection_ADAMs <- "regular"
      }
      
      ## aggregate data -------------------------------------------------------
      if("aggregate" %in% returnAggregateOrIndiv){
         
         sim_data_mean[[cmpd]] <- list()
         
         ### cmpd is substrate or substrate metabolite -----------
         if(str_detect(cmpd, "substrate|metabolite|released payload") & 
            !str_detect(cmpd, "inhibitor")){
            # released payload looks like primary metabolite 1, so
            # extracting those data here rather than with the rest of the
            # ADC data
            
            TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
            TimeRow <- TimeRow[TimeRow > which(sim_data_xl$...1 == "Population Statistics")][1]
            
            # Figuring out which rows contain which data
            FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                    which(1:nrow(sim_data_xl) > TimeRow))[1]
            FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
            NamesToCheck <- tolower(sim_data_xl$...1[TimeRow:(FirstBlank-1)])
            
            for(ss in subsection_ADAMs){
               
               # message(paste("subsection_ADAMs =", ss))
               # Some sheets have all compounds included, so need to narrow
               # down which rows to check. Others don't have metabolites
               # listed on the same sheet, so that's why there are these
               # options.
               if(str_detect(tissue, "portal") | TissueType == "tissue"){
                  if(ADAM|AdvBrainModel){
                     
                     temp_regex <- paste0(ifelse(str_detect(SimConcUnits$TypeCode[SimConcUnits$Type == ss],
                                                            "dissolved|absorbed"),
                                                 "", "^"), 
                                          SimConcUnits$TypeCode[SimConcUnits$Type == ss])
                     
                     # gut tissue tab is set up slightly differently, so need to
                     # adjust what regex to use.
                     if(tissue == "gut tissue"){
                        temp_regex <- ifelse(cmpd == "inhibitor 1", 
                                             "^ITissue", "^CTissue")
                     }
                     
                     Include <- which(str_detect(NamesToCheck, tolower(temp_regex)))
                     
                     if(tissue == "cumulative fraction released"){
                        Include <- 2:5
                     }
                     
                  } else {
                     Include <-
                        which(str_detect(
                           NamesToCheck,
                           switch(cmpd,
                                  "substrate" =
                                     paste0("^cpv|^ctissue|^c lumen free|^c", tolower(tissue)),
                                  "primary metabolite 1" =
                                     paste0("^mpv |^mpv\\+|^mtissue|^m", tolower(tissue)),
                                  "primary metabolite 2" =
                                     paste0("^pm2pv |^pm2pb\\+|^pm2tissue|^pm2", tolower(tissue)),
                                  "secondary metabolite" =
                                     paste0("^miipv|^miitissue|^mii", tolower(tissue)))))
                  }
                  
               } else {
                  Include <- which(str_detect(NamesToCheck, "^csys|therapeutic protein csys"))
               }
               
               RowsToUse <- c(
                  "mean" = intersect(
                     which(str_detect(NamesToCheck, "mean") &
                              !str_detect(NamesToCheck,
                                          "geometric|interaction")),
                     Include) + TimeRow-1,
                  "per5" = intersect(
                     which(str_detect(NamesToCheck," 5(th)? percentile|5th ptile|^5th percentile$") &
                              !str_detect(NamesToCheck, "interaction|95")),
                     Include) + TimeRow-1,
                  "per95" = intersect(
                     which(str_detect(NamesToCheck, " 95(th)? percentile|95th ptile|^95th percentile$") &
                              !str_detect(NamesToCheck, "interaction")),
                     Include) + TimeRow-1,
                  "per10" = intersect(
                     which(str_detect(NamesToCheck," 10(th)? percentile|10th ptile") &
                              !str_detect(NamesToCheck,
                                          "interaction")),
                     Include) + TimeRow-1,
                  "per90" = intersect(
                     which(str_detect(NamesToCheck, " 90(th)? percentile|90th ptile") &
                              !str_detect(NamesToCheck, "interaction")),
                     Include) + TimeRow-1,
                  "geomean" = intersect(
                     which(str_detect(NamesToCheck, "geometric mean") &
                              !str_detect(NamesToCheck, "interaction")),
                     Include) + TimeRow-1,
                  "median" = intersect(
                     which(str_detect(NamesToCheck, "median") &
                              !str_detect(NamesToCheck, "interaction")),
                     Include) + TimeRow-1)
               
               suppressWarnings(
                  sim_data_mean[[cmpd]][[ss]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                     t() %>%
                     as.data.frame() %>%
                     slice(-(1:3)) %>%
                     mutate_all(as.numeric)
               )
               
               names(sim_data_mean[[cmpd]][[ss]]) <- c("Time", names(RowsToUse))
               sim_data_mean[[cmpd]][[ss]] <- sim_data_mean[[cmpd]][[ss]] %>%
                  pivot_longer(names_to = "Trial", values_to = "Conc",
                               cols = -c(Time)) %>%
                  mutate(Compound = MyCompound,
                         CompoundID = cmpd,
                         Inhibitor = "none",
                         Time_units = SimTimeUnits,
                         Conc_units = ifelse(ADAM|AdvBrainModel, 
                                             SimConcUnits$ConcUnit[
                                                SimConcUnits$Type == ss],
                                             SimConcUnits), 
                         subsection_ADAM = ifelse(ADAM|AdvBrainModel, ss, NA))
               
               rm(RowsToUse, Include)
            }
            
            if(PerpPresent){
               
               for(ss in subsection_ADAMs){
                  # message(paste("subsection_ADAMs =", ss))
                  # Some sheets have all compounds included, so need to narrow
                  # down which rows to check. Others don't have metabolites
                  # listed on the same sheet, so that's why there are these
                  # options.
                  if(str_detect(tissue, "portal") | TissueType == "tissue"){
                     if(ADAM){
                        temp_regex <- paste0(ifelse(str_detect(SimConcUnits$TypeCode[SimConcUnits$Type == ss],
                                                               "dissolved|absorbed"),
                                                    "", "^"), 
                                             tolower(SimConcUnits$TypeCode[
                                                SimConcUnits$Type == ss]))
                        
                        # gut tissue tab is set up slightly differently, so need
                        # to adjust what regex to use.
                        if(tissue == "gut tissue"){
                           temp_regex <- ifelse(cmpd == "inhibitor 1", 
                                                "^ITissue", "^CTissue")
                        }
                        
                        Include <- which(str_detect(NamesToCheck, tolower(temp_regex)))
                        
                     } else {
                        Include <-
                           which(str_detect(
                              NamesToCheck,
                              switch(cmpd,
                                     "substrate" =
                                        paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                     "primary metabolite 1" =
                                        paste0("^mpv |^mpv\\+|^mtissue|^m", tolower(tissue)),
                                     "primary metabolite 2" =
                                        paste0("^pm2pv |^pm2pb\\+|^pm2tissue|^pm2", tolower(tissue)),
                                     "secondary metabolite" =
                                        paste0("^miipv|^miitissue|^mii", tolower(tissue)))))
                     }
                  } else {
                     Include <- which(str_detect(NamesToCheck, "^csys|therapeutic protein csys"))
                  }
                  
                  RowsToUse <- c(
                     "mean" = intersect(
                        which(str_detect(NamesToCheck, "mean") &
                                 str_detect(NamesToCheck, "interaction") &
                                 !str_detect(NamesToCheck, "geometric")),
                        Include) + TimeRow-1,
                     "per5" = intersect(
                        which(str_detect(NamesToCheck," 5(th)? percentile|5th ptile") &
                                 str_detect(NamesToCheck, "interaction") &
                                 !str_detect(NamesToCheck, "95")),
                        Include) + TimeRow-1,
                     "per95" = intersect(
                        which(str_detect(NamesToCheck, " 95(th)? percentile|95th ptile") &
                                 str_detect(NamesToCheck, "interaction")),
                        Include) + TimeRow-1,
                     "per10" = intersect(
                        which(str_detect(NamesToCheck," 10(th)? percentile|10th ptile") &
                                 str_detect(NamesToCheck, "interaction")),
                        Include) + TimeRow-1,
                     "per90" = intersect(
                        which(str_detect(NamesToCheck, " 90(th)? percentile|90th ptile") &
                                 str_detect(NamesToCheck, "interaction")),
                        Include) + TimeRow-1,
                     "geomean" = intersect(
                        which(str_detect(NamesToCheck, "geometric mean") &
                                 str_detect(NamesToCheck, "interaction")),
                        Include) + TimeRow-1,
                     "median" = intersect(
                        which(str_detect(NamesToCheck, "median") &
                                 str_detect(NamesToCheck, "interaction")),
                        Include) + TimeRow-1)
                  
                  if(length(RowsToUse) > 0){
                     
                     suppressWarnings(
                        sim_data_mean_SubPlusPerp <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                           t() %>%
                           as.data.frame() %>% slice(-(1:3)) %>%
                           mutate_all(as.numeric)
                     )
                     names(sim_data_mean_SubPlusPerp) <- c("Time", names(RowsToUse))
                     sim_data_mean_SubPlusPerp <- sim_data_mean_SubPlusPerp %>%
                        pivot_longer(names_to = "Trial", values_to = "Conc",
                                     cols = -c(Time)) %>%
                        mutate(Compound = MyCompound,
                               Inhibitor = str_comma(AllPerpsPresent),
                               CompoundID = cmpd,
                               Time_units = SimTimeUnits,
                               Conc_units = ifelse(ADAM|AdvBrainModel, 
                                                   SimConcUnits$ConcUnit[
                                                      SimConcUnits$Type == ss],
                                                   SimConcUnits), 
                               subsection_ADAM = ifelse(ADAM|AdvBrainModel, ss, NA))
                     
                     sim_data_mean[[cmpd]][[ss]] <- bind_rows(sim_data_mean[[cmpd]][[ss]],
                                                              sim_data_mean_SubPlusPerp)
                     
                  }
               }
            }
            
            sim_data_mean[[cmpd]] <- bind_rows(sim_data_mean[[cmpd]])
            
            rm(TimeRow, FirstBlank, NamesToCheck)
            
         }
         
         ### cmpd is an inhibitor or inhibitor metabolite -----------
         
         # Inhibitor concentrations are only present on tabs
         # w/substrate info for systemic tissues.
         if(cmpd %in% c("inhibitor 1", "inhibitor 2",
                        "inhibitor 1 metabolite") &
            length(AllPerpsPresent) > 0){
            
            TimeRow <- which(str_detect(sim_data_xl$...1,
                                        "^Time.*Inhibitor "))[1]
            if(is.na(TimeRow)){ # This occurs when the tissue is not systemic or it seems in versions > 20
               TimeRow <- which(str_detect(sim_data_xl$...1,
                                           "Time "))
               if(ADAM){
                  TimeRow <- TimeRow[1]
               } else {
                  TimeRow <- TimeRow[which(str_detect(sim_data_xl$...1[TimeRow + 1],
                                                      "^I|CTissue"))][1]
               }
            }
            
            # Figuring out which rows contain which data
            FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                    which(1:nrow(sim_data_xl) > TimeRow))[1]
            FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
            NamesToCheck <- sim_data_xl$...1[(TimeRow+1):(FirstBlank-1)]
            
            
            for(ss in subsection_ADAMs){
               
               # message(paste("subsection_ADAMs =", ss))
               # Some sheets have all compounds included, so need to narrow
               # down which rows to check. Others don't have metabolites
               # listed on the same sheet, so that's why there are these
               # options.
               if(str_detect(tissue, "portal") | TissueType == "tissue"){
                  if(ADAM){
                     
                     temp_regex <- paste0(ifelse(str_detect(SimConcUnits$TypeCode[SimConcUnits$Type == ss],
                                                            "dissolved|absorbed"),
                                                 "", "^"), 
                                          SimConcUnits$TypeCode[SimConcUnits$Type == ss])
                     
                     # gut tissue tab is set up slightly differently, so need to
                     # adjust what regex to use.
                     if(tissue == "gut tissue"){
                        temp_regex <- ifelse(cmpd == "inhibitor 1", 
                                             "^ITissue", "^CTissue")
                     }
                     
                     Include <- which(str_detect(NamesToCheck, temp_regex))
                     
                  } else {
                     Include <- which(str_detect(NamesToCheck, NumCheck)) 
                  }
               } else {
                  Include <- which(str_detect(NamesToCheck,
                                              NumCheck[switch(cmpd, 
                                                              "inhibitor 1" = "PERPETRATOR1INHIB", 
                                                              "inhibitor 2" = "PERPETRATOR2", 
                                                              "inhibitor 1 metabolite" = "PERPETRATOR1MET")]))
               }
               
               RowsToUse <- c(
                  "mean" = intersect(
                     which(str_detect(NamesToCheck, "Mean") &
                              !str_detect(NamesToCheck, "Geome(t)?ric")), # There's a spelling error in some simulator output, and geometric is listed as "geomeric".
                     Include) + TimeRow,
                  "per5" = intersect(
                     which(str_detect(NamesToCheck," 5(th)? percentile|5th ptile") &
                              !str_detect(NamesToCheck, "95")),
                     Include) + TimeRow,
                  "per95" = intersect(
                     which(str_detect(NamesToCheck, " 95(th)? percentile|95th ptile")),
                     Include) + TimeRow,
                  "per10" = intersect(
                     which(str_detect(NamesToCheck," 10(th)? percentile|10th ptile")),
                     Include) + TimeRow,
                  "per90" = intersect(
                     which(str_detect(NamesToCheck, " 90(th)? percentile|90th ptile")),
                     Include) + TimeRow,
                  "geomean" = intersect(
                     which(str_detect(NamesToCheck, "Geome(t)?ric Mean")),
                     Include) + TimeRow,
                  "median" = intersect(
                     which(str_detect(NamesToCheck, "Median")),
                     Include) + TimeRow)
               
               suppressWarnings(
                  sim_data_mean[[cmpd]][[ss]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                     t() %>%
                     as.data.frame() %>% slice(-(1:3)) %>%
                     mutate_all(as.numeric)
               )
               names(sim_data_mean[[cmpd]][[ss]]) <-
                  c("Time", names(RowsToUse))
               sim_data_mean[[cmpd]][[ss]] <- sim_data_mean[[cmpd]][[ss]] %>%
                  pivot_longer(names_to = "Trial", values_to = "Conc",
                               cols = -c("Time")) %>%
                  mutate(Compound = Eff_DF$Compound[which(Eff_DF$CompoundID == cmpd)],
                         CompoundID = cmpd,
                         Inhibitor = str_comma(AllPerpsPresent),
                         Time_units = SimTimeUnits,
                         Conc_units = ifelse(ADAM|AdvBrainModel, 
                                             SimConcUnits$ConcUnit[
                                                SimConcUnits$Type == ss],
                                             SimConcUnits), 
                         subsection_ADAM = ifelse(ADAM|AdvBrainModel, ss, NA))
               
               rm(RowsToUse, Include)
            }
            
            sim_data_mean[[cmpd]] <- bind_rows(sim_data_mean[[cmpd]])
            rm(NamesToCheck, TimeRow, FirstBlank)
         }
         
         ### cmpd is an ADC compound -----------
         
         # ADC compound sheets are set up differently
         if(CompoundType == "ADC" &
            all(compoundToExtract != "released payload") &
            length(AllPerpsPresent) == 0){
            TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))[1]
            
            # Figuring out which rows contain which data
            FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                    which(1:nrow(sim_data_xl) > TimeRow))[1]
            FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
            NamesToCheck <- tolower(sim_data_xl$...1[TimeRow:(FirstBlank-1)])
            
            # for(ss in ADCCompoundIDs){
            ss <- compoundToExtract
            
            Include <- which(str_detect(NamesToCheck, 
                                        switch(ss, 
                                               "total protein" = "protein total .dar0",
                                               "conjugated protein" = "conjugated protein .dar1", 
                                               "total antibody" = "cantibody total", 
                                               "protein-conjugated substrate" = "protein conjugated drug" # CHECK THIS ONE with an example; just guessing for now
                                        )))
            
            # if(length(Include) == 0){
            #     next
            # }
            
            RowsToUse <- c(
               "mean" = intersect(
                  which(str_detect(NamesToCheck, "mean") &
                           !str_detect(NamesToCheck,
                                       "geometric|interaction")),
                  Include) + TimeRow-1,
               "per5" = intersect(
                  which(str_detect(NamesToCheck," 5(th)? percentile|5th ptile") &
                           !str_detect(NamesToCheck, "interaction|95")),
                  Include) + TimeRow-1,
               "per95" = intersect(
                  which(str_detect(NamesToCheck, " 95(th)? percentile|95th ptile") &
                           !str_detect(NamesToCheck, "interaction")),
                  Include) + TimeRow-1,
               "per10" = intersect(
                  which(str_detect(NamesToCheck," 10(th)? percentile|10th ptile") &
                           !str_detect(NamesToCheck,
                                       "interaction")),
                  Include) + TimeRow-1,
               "per90" = intersect(
                  which(str_detect(NamesToCheck, " 90(th)? percentile|90th ptile") &
                           !str_detect(NamesToCheck, "interaction")),
                  Include) + TimeRow-1,
               "geomean" = intersect(
                  which(str_detect(NamesToCheck, "geometric mean") &
                           !str_detect(NamesToCheck, "interaction")),
                  Include) + TimeRow-1,
               "median" = intersect(
                  which(str_detect(NamesToCheck, "median") &
                           !str_detect(NamesToCheck, "interaction")),
                  Include) + TimeRow-1)
            
            suppressWarnings(
               sim_data_mean[[cmpd]][[ss]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric)
            )
            
            # Getting conc units here
            SimConcUnits <- sub("\\(DAR.-DARmax\\)", "", as.character(sim_data_xl[TimeRow + 1, 1]))
            SimConcUnits <- gsub("\\(|\\)|-|", "", str_extract(SimConcUnits, "\\(.*\\)"))
            
            names(sim_data_mean[[cmpd]][[ss]]) <- c("Time", names(RowsToUse))
            sim_data_mean[[cmpd]][[ss]] <- sim_data_mean[[cmpd]][[ss]] %>%
               pivot_longer(names_to = "Trial", values_to = "Conc",
                            cols = -c(Time)) %>%
               mutate(Compound = MyCompound,
                      CompoundID = ss,
                      Inhibitor = "none",
                      Time_units = SimTimeUnits,
                      Conc_units = ifelse(ADAM|AdvBrainModel, 
                                          SimConcUnits$ConcUnit[
                                             SimConcUnits$Type == ss],
                                          SimConcUnits), 
                      subsection_ADAM = ifelse(ADAM|AdvBrainModel, ss, NA))
            
            rm(RowsToUse, Include)
            # }
            
            sim_data_mean[[cmpd]] <- bind_rows(sim_data_mean[[cmpd]])
            rm(NamesToCheck, TimeRow, FirstBlank)
            
         } # RETURN TO THIS when there are perpetrators. Not set up yet. 
      }
      
      ## individual data ------------------------------------------------------
      if("individual" %in% returnAggregateOrIndiv){
         
         StartIndiv <- which(str_detect(sim_data_xl$...1, "Individual Statistics"))
         
         sim_data_ind[[cmpd]] <- list()
         
         ### cmpd is substrate or substrate metabolite -----------
         if(str_detect(cmpd, "substrate|metabolite|released payload") &
            !str_detect(cmpd, "inhibitor")){
            # released payload looks like primary metabolite 1, so
            # extracting those data here rather than with the rest of the
            # ADC data
            
            # substrate data
            TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
            TimeRow <- TimeRow[TimeRow > StartIndiv][1]
            
            # ADAM data include multiple types of concentrations. Dealing
            # with that and extracting each one.
            if(ADAM|AdvBrainModel){
               subsection_ADAMs <- SimConcUnits$Type
            } else {
               subsection_ADAMs <- "regular"
            }
            
            for(ss in subsection_ADAMs){
               # message(paste("subsection_ADAMs =", ss))
               if(ADAM|AdvBrainModel){
                  RowsToUse <- 
                     data.frame(Orig = sim_data_xl$...1) %>% 
                     mutate(TypeCode = str_extract(Orig,
                                                   "^Ms|^Dissolution Rate Solid State|^C Lumen Free|^C Lumen Total|^Heff|^Absorption Rate|^Mur|^Md|^Inh Md|^Luminal CLint|CTissue|dissolved|absorbed|^C Enterocyte|Release Fraction|CIntracranial|CBrainI[CS]F|CCSF(Spinal|Cranial)|Kpuu_I[CS]F|Kpuu_BrainMass|CTotalBrain"), 
                            TypeCode = ifelse(str_detect(Orig, "with interaction|Inh C Lumen"), 
                                              NA, TypeCode)) %>% 
                     left_join(SimConcUnits, by = "TypeCode")
                  RowsToUse <- which(RowsToUse$Type == ss)
                  
               } else {
                  
                  RowsToUse <- which(
                     str_detect(sim_data_xl$...1,
                                switch(ifelse(TissueType == "systemic",
                                              TissueType,
                                              paste(TissueType, cmpd)),
                                       "systemic" = "C(Sys|pv)|CPeripheral",
                                       "tissue substrate" =
                                          paste0("CTissue$|",
                                                 "C", tissue, " \\("),
                                       "tissue inhibitor 1" =
                                          paste0("CTissue$|",
                                                 "C", tissue, " \\("),
                                       "tissue primary metabolite 1" =
                                          paste0("M", tissue, " \\("),
                                       "tissue secondary metabolite" =
                                          paste0("PM2", tissue, " \\("))) &
                        !str_detect(sim_data_xl$...1, "interaction|After Inh"))
               }
               
               RowsToUse <- RowsToUse[RowsToUse > TimeRow]
               
               suppressWarnings(
                  sim_data_ind[[cmpd]][[ss]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                     t() %>%
                     as.data.frame() %>% slice(-(1:3)) %>%
                     mutate_all(as.numeric) %>%
                     rename(Time = "V1")
               )
               
               SubjTrial <- sim_data_xl[RowsToUse, 2:3] %>%
                  rename(Individual = ...2, Trial = ...3) %>%
                  mutate(SubjTrial = paste0("ID", Individual, "_", Trial))
               
               names(sim_data_ind[[cmpd]][[ss]])[2:ncol(sim_data_ind[[cmpd]][[ss]])] <- SubjTrial$SubjTrial
               
               sim_data_ind[[cmpd]][[ss]] <- sim_data_ind[[cmpd]][[ss]] %>%
                  pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                               cols = -Time) %>%
                  mutate(Compound = MyCompound,
                         CompoundID = cmpd,
                         Inhibitor = "none",
                         SubjTrial = sub("ID", "", SubjTrial),
                         Time_units = SimTimeUnits,
                         Conc_units = ifelse(ADAM|AdvBrainModel, 
                                             SimConcUnits$ConcUnit[
                                                SimConcUnits$Type == ss],
                                             SimConcUnits), 
                         subsection_ADAM = ifelse(ADAM|AdvBrainModel, ss, NA)) %>%
                  separate(SubjTrial, into = c("Individual", "Trial"),
                           sep = "_")
               
               rm(RowsToUse)
               
            }
            
            if(PerpPresent){
               
               for(ss in subsection_ADAMs){
                  # message(paste("subsection_ADAMs =", ss))
                  if(ADAM){
                     
                     temp_regex <- paste0(ifelse(str_detect(SimConcUnits$TypeCode[SimConcUnits$Type == ss],
                                                            "dissolved|absorbed"),
                                                 "", "^"), 
                                          tolower(SimConcUnits$TypeCode[
                                             SimConcUnits$Type == ss]))
                     
                     # gut tissue tab is set up slightly differently, so need to
                     # adjust what regex to use.
                     if(tissue == "gut tissue"){
                        temp_regex <- ifelse(cmpd == "inhibitor 1", 
                                             "^ITissue", "^CTissue")
                     }
                     
                     RowsToUse <- intersect(
                        which(
                           str_detect(sim_data_xl$...1, temp_regex)), 
                        which(
                           str_detect(sim_data_xl$...1, 
                                      "with interaction|Inh C Lumen")) )
                  } else {
                     
                     RowsToUse <- which(
                        str_detect(sim_data_xl$...1,
                                   switch(ifelse(TissueType == "systemic",
                                                 TissueType,
                                                 paste(TissueType, cmpd)),
                                          "systemic" = "C(Sys|pv) After Inh|C(Sys|pv).interaction",
                                          "tissue substrate" =
                                             paste0("CTissue . Interaction|",
                                                    "C", tissue, " After Inh"),
                                          "tissue inhibitor 1" =
                                             paste0("CTissue . Interaction|",
                                                    "C", tissue, " After Inh"),
                                          "tissue primary metabolite 1" =
                                             paste0("M", tissue, " After Inh"),
                                          "tissue secondary metabolite" =
                                             paste0("PM2", tissue, " After Inh"))
                        ))
                  }
                  
                  RowsToUse <- RowsToUse[RowsToUse > TimeRow]
                  
                  if(length(RowsToUse) > 0){
                     
                     suppressWarnings(
                        sim_data_ind_SubPlusPerp <-
                           sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                           t() %>%
                           as.data.frame() %>% slice(-(1:3)) %>%
                           mutate_all(as.numeric) %>%
                           rename(Time = "V1")
                     )
                     names(sim_data_ind_SubPlusPerp)[
                        2:ncol(sim_data_ind_SubPlusPerp)] <- SubjTrial$SubjTrial
                     sim_data_ind_SubPlusPerp <-
                        sim_data_ind_SubPlusPerp %>%
                        pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                                     cols = -Time) %>%
                        mutate(Compound = MyCompound,
                               CompoundID = cmpd,
                               Inhibitor = str_comma(AllPerpsPresent),
                               SubjTrial = sub("ID", "", SubjTrial),
                               Time_units = SimTimeUnits,
                               Conc_units = ifelse(ADAM|AdvBrainModel, 
                                                   SimConcUnits$ConcUnit[
                                                      SimConcUnits$Type == ss],
                                                   SimConcUnits), 
                               subsection_ADAM = ifelse(ADAM|AdvBrainModel, ss, NA)) %>%
                        separate(SubjTrial, into = c("Individual", "Trial"),
                                 sep = "_")
                     
                     sim_data_ind[[cmpd]][[ss]] <- bind_rows(sim_data_ind[[cmpd]][[ss]],
                                                             sim_data_ind_SubPlusPerp)
                  }
                  
                  rm(RowsToUse)
               }
            }
            
            sim_data_ind[[cmpd]] <- bind_rows(sim_data_ind[[cmpd]])
            
            rm(TimeRow)
         }
         
         ### cmpd is an inhibitor or inhibitor metabolite ----------------------
         
         # Inhibitor concentrations are only present on tabs
         # w/substrate info for systemic tissues.
         if(cmpd %in% c("inhibitor 1", "inhibitor 2",
                        "inhibitor 1 metabolite") &
            length(AllPerpsPresent) > 0){
            
            sim_data_ind[[cmpd]] <- list()
            
            TimeRow <- which(str_detect(sim_data_xl$...1, "^Time.*Inhibitor "))
            TimeRow <- TimeRow[TimeRow > StartIndiv]
            
            # TimeRow has length 0 when the tissue is not systemic or
            # w/portal vein
            if(length(TimeRow) == 0 || is.na(TimeRow)){ 
               TimeRow <- which(str_detect(sim_data_xl$...1,
                                           "Time "))
               TimeRow <- TimeRow[TimeRow > StartIndiv]
               
               if(ADAM){
                  TimeRow <- TimeRow[1]
               } else {
                  TimeRow <- TimeRow[which(str_detect(sim_data_xl$...1[TimeRow + 1],
                                                      "^I|^CTissue"))][1]
               }
            }
            
            # Figuring out which rows contain which data
            FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                    which(1:nrow(sim_data_xl) > TimeRow))[1]
            FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl) + 1, FirstBlank)
            NamesToCheck <- sim_data_xl$...1[(TimeRow+1):(FirstBlank-1)]
            
            for(ss in subsection_ADAMs){
               # message(paste("subsection_ADAMs =", ss))
               # Some sheets have all compounds included, so need to
               # narrow down which rows to check. Others don't have
               # metabolites listed on the same sheet, so that's why
               # there are these options.
               if(str_detect(tissue, "portal") | TissueType == "tissue"){
                  if(ADAM){
                     
                     temp_regex <- paste0(ifelse(str_detect(SimConcUnits$TypeCode[SimConcUnits$Type == ss],
                                                            "dissolved|absorbed"),
                                                 "", "^"), 
                                          tolower(SimConcUnits$TypeCode[
                                             SimConcUnits$Type == ss]))
                     
                     # gut tissue tab is set up slightly differently, so need to
                     # adjust what regex to use.
                     if(tissue == "gut tissue"){
                        temp_regex <- ifelse(cmpd == "inhibitor 1", 
                                             "^ITissue", "^CTissue")
                     }
                     
                     Include <- which(str_detect(NamesToCheck, temp_regex))
                     
                  } else {
                     Include <- which(str_detect(NamesToCheck, NumCheck[cmpd]))
                  }
               } else {
                  Include <- which(str_detect(NamesToCheck,
                                              NumCheck[switch(cmpd, 
                                                              "inhibitor 1" = "PERPETRATOR1INHIB", 
                                                              "inhibitor 2" = "PERPETRATOR2", 
                                                              "inhibitor 1 metabolite" = "PERPETRATOR1MET")]))
               }
               
               if(length(Include) == 0){
                  Include <- 1:length(NamesToCheck)
               }
               
               if(ADAM){
                  temp_regex <- paste0(ifelse(str_detect(SimConcUnits$TypeCode[SimConcUnits$Type == ss],
                                                         "dissolved|absorbed"),
                                              "", "^"), 
                                       tolower(SimConcUnits$TypeCode[
                                          SimConcUnits$Type == ss]))
                  
                  # gut tissue tab is set up slightly differently, so need to
                  # adjust what regex to use.
                  if(tissue == "gut tissue"){
                     temp_regex <- ifelse(cmpd == "inhibitor 1", 
                                          "^ITissue", "^CTissue")
                  }
                  
                  RowsToUse <- intersect(
                     which(
                        str_detect(sim_data_xl$...1, temp_regex)), 
                     which(
                        str_detect(sim_data_xl$...1, 
                                   "with interaction|Inh C Lumen")) )
               } else {
                  RowsToUse <- which(str_detect(
                     sim_data_xl$...1,
                     switch(TissueType,
                            "systemic" = paste0(
                               NumCheck[switch(cmpd, 
                                               "inhibitor 1" = "PERPETRATOR1INHIB", 
                                               "inhibitor 2" = "PERPETRATOR2", 
                                               "inhibitor 1 metabolite" = "PERPETRATOR1MET")],
                               " \\(|", cmpd),
                            "tissue" = paste0("ITissue\\(Inh 1|",
                                              "I", tissue, " 1 \\("))
                  ))
               }
               
               RowsToUse <- RowsToUse[which(RowsToUse > TimeRow)]
               
               suppressWarnings(
                  sim_data_ind[[cmpd]][[ss]] <-
                     sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                     t() %>%
                     as.data.frame() %>% slice(-(1:3)) %>%
                     mutate_all(as.numeric) %>%
                     rename(Time = "V1")
               )
               
               SubjTrial <- sim_data_xl[RowsToUse, 2:3] %>%
                  rename(Individual = ...2, Trial = ...3) %>%
                  mutate(SubjTrial = paste0("ID", Individual, "_", Trial))
               
               names(sim_data_ind[[cmpd]][[ss]])[2:ncol(sim_data_ind[[cmpd]][[ss]])] <-
                  SubjTrial$SubjTrial
               
               sim_data_ind[[cmpd]][[ss]] <-
                  sim_data_ind[[cmpd]][[ss]] %>%
                  pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                               cols = -Time) %>%
                  mutate(Compound = Eff_DF$Compound[which(Eff_DF$CompoundID == cmpd)],
                         CompoundID = cmpd,
                         Inhibitor = str_comma(AllPerpsPresent),
                         SubjTrial = sub("ID", "", SubjTrial),
                         Time_units = SimTimeUnits,
                         Conc_units = ifelse(ADAM|AdvBrainModel, 
                                             SimConcUnits$ConcUnit[
                                                SimConcUnits$Type == ss],
                                             SimConcUnits), 
                         subsection_ADAM = ifelse(ADAM|AdvBrainModel, ss, NA)) %>%
                  separate(SubjTrial, into = c("Individual", "Trial"),
                           sep = "_")
               
               rm(RowsToUse)
            }
            
            sim_data_ind[[cmpd]] <- bind_rows(sim_data_ind[[cmpd]])
         }
         ### cmpd is an ADC compound -----------
         if(CompoundType == "ADC" & 
            all(compoundToExtract != "released payload") &
            length(AllPerpsPresent) == 0){
            
            # substrate data
            TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
            TimeRow <- TimeRow[TimeRow > StartIndiv][1]
            
            # for(ss in ADCCompoundIDs){
            ss = compoundToExtract
            
            RowsToUse <- which(
               str_detect(tolower(sim_data_xl$...1),
                          switch(ss, 
                                 "total protein" = "protein total .dar0",
                                 "conjugated protein" = "conjugated protein .dar1", 
                                 "total antibody" = "cantibody total", 
                                 "protein-conjugated substrate" = "protein conjugated drug" # CHECK THIS ONE with an example; just guessing for now
                          )))
            
            RowsToUse <- RowsToUse[RowsToUse > TimeRow]
            
            # if(length(RowsToUse) == 0){
            #     next
            # }
            
            suppressWarnings(
               sim_data_ind[[cmpd]][[ss]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric) %>%
                  rename(Time = "V1")
            )
            
            SubjTrial <- sim_data_xl[RowsToUse, 2:3] %>%
               rename(Individual = ...2, Trial = ...3) %>%
               mutate(SubjTrial = paste0("ID", Individual, "_", Trial))
            
            names(sim_data_ind[[cmpd]][[ss]])[2:ncol(sim_data_ind[[cmpd]][[ss]])] <- SubjTrial$SubjTrial
            
            sim_data_ind[[cmpd]][[ss]] <- sim_data_ind[[cmpd]][[ss]] %>%
               pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                            cols = -Time) %>%
               mutate(Compound = MyCompound,
                      CompoundID = cmpd,
                      Inhibitor = "none",
                      SubjTrial = sub("ID", "", SubjTrial),
                      Time_units = SimTimeUnits,
                      Conc_units = ifelse(ADAM|AdvBrainModel, 
                                          SimConcUnits$ConcUnit[
                                             SimConcUnits$Type == ss],
                                          SimConcUnits), 
                      subsection_ADAM = ifelse(ADAM|AdvBrainModel, ss, NA)) %>%
               separate(SubjTrial, into = c("Individual", "Trial"),
                        sep = "_")
            
            rm(RowsToUse)
            
            # }
            
            sim_data_ind[[cmpd]] <- bind_rows(sim_data_ind[[cmpd]])
            
            rm(TimeRow)
         }
      } 
      
      ## observed data -------------------------------------------------------
      
      # This section of code ONLY applies when obs concs are NOT extracted
      # separately. This piece of code also ONLY applies to systemic concs. 
      
      # Setting up some names of observed data for use later
      ObsCompounds <-
         c("substrate" = Deets$Substrate,
           "inhibitor 1" = Deets$Inhibitor1, 
           "inhibitor 2" = Deets$Inhibitor2, 
           "primary metabolite 1" = Deets$PrimaryMetabolite1,
           "primary metabolite 2" = Deets$PrimaryMetabolite2,
           "secondary metabolite" = Deets$SecondaryMetabolite,
           "inhibitor 1 metabolite" = Deets$Inhibitor1Metabolite)
      
      if(CompoundType == "ADC"){
         ObsCompounds <- c(ObsCompounds, 
                           "conjugated protein" = paste("conjugated", Deets$Substrate),
                           "total protein" = paste("total", Deets$Substrate), 
                           "released payload" = Deets$PrimaryMetabolite1)
      }
      
      AllPerps_comma <- ifelse(length(AllPerpsPresent) == 0,
                               NA, str_comma(AllPerpsPresent))
      
      # Use the supplied obs file here if a) tissue is systemic and b) the
      # function was NOT called from the mult function OR the function WAS
      # called from the mult function but the user supplied an obs data file.
      if(TissueType == "systemic" &
         (fromMultFunction == FALSE | (fromMultFunction & complete.cases(obs_data_file)))){
         
         # If the user did not specify a file to use for observed data, use
         # the observed data that they included for the simulation. Note that
         # this will NOT pull the observed data if the user asked for an
         # inhibitor-related compound b/c it's unlikely that that's what
         # observed data they supplied when they set up their simulation.
         if(is.na(obs_data_file)){
            
            if(str_detect(compoundToExtract, "inhibitor") == FALSE){
               
               StartRow_obs <- which(sim_data_xl$...1 == "Observed Data") + 1
               
               if(length(StartRow_obs) != 0 && 
                  StartRow_obs - 1 != nrow(sim_data_xl)){
                  
                  obs_data <-
                     sim_data_xl[StartRow_obs:nrow(sim_data_xl), ] %>%
                     t() %>%
                     as.data.frame()
                  if(all(is.na(obs_data[,1]))){
                     
                     # Sometimes, there will be a single cell that says
                     # "Observed Data" but then nothing else for obs
                     # data. Removing the basically empty data.frame of
                     # obs data in that situation.
                     rm(obs_data)
                  } else {
                     warning("WARNING: This function is extracting observed data from simulator output, which does not contain information about the observed compound ID or whether the observed compound was in the presence of a perpetrator. The safer way to include observed data is to supply a separate file for 'obs_data_file'.\n",
                             call. = FALSE)
                     
                     # If subject names include special characters s/a "_", that
                     # messes up the regex below. Dealing with that here. Also,
                     # note that we're ignoring any numbers associated w/DV b/c
                     # simulator output file doesn't include any information
                     # about that. Everything will be assumed to be for the same
                     # compound and tissue as the simulated data and will be
                     # assumed to NOT have an inhibitor present.
                     NewNamesObs <- sim_data_xl[StartRow_obs:nrow(sim_data_xl), 1] %>% 
                        rename("OrigName" = 1) %>% 
                        mutate(Individual = 
                                  as.character(sapply(OrigName,
                                                      FUN = function(x) 
                                                         str_split(x, pattern = "Subject ")[[1]][2])), 
                               Individual = sub(" : DV [0-9]{1,}", "", Individual),
                               Indiv_code = paste0("Subject", 1:nrow(.)), 
                               Indiv_code = ifelse(str_detect(OrigName, "Time"),
                                                   NA, Indiv_code)) %>% 
                        fill(Indiv_code, .direction = "up") %>%  
                        fill(Individual, .direction = "up") %>% 
                        mutate(TempName = ifelse(str_detect(OrigName, "Time"), 
                                                 paste0(Indiv_code, "_Time"), 
                                                 paste0(Indiv_code, "_Conc")))
                     
                     names(obs_data) <- NewNamesObs$TempName
                     
                     suppressMessages(
                        suppressWarnings(
                           obs_data <- obs_data[4:nrow(obs_data), ] %>%
                              mutate_all(as.numeric) %>%
                              mutate(ID = 1:nrow(.)) %>%
                              pivot_longer(cols = -c(ID),
                                           names_to = "Param",
                                           values_to = "Val") %>%
                              separate(col = Param,
                                       into = c("Indiv_code", "TimeConc"),
                                       sep = "_") %>%
                              filter(complete.cases(Val)) %>% 
                              pivot_wider(names_from = TimeConc, 
                                          values_from = Val) %>% 
                              left_join(NewNamesObs %>% select(Indiv_code, Individual)) %>%
                              mutate(Trial = "obs",
                                     Inhibitor = "none",
                                     CompoundID = ifelse(tissue == "plasma" &
                                                            compoundToExtract == "substrate" &
                                                            all(AllCompoundsID == "substrate"),
                                                         cmpd, "UNKNOWN"),
                                     Compound = ifelse(tissue == "plasma" & 
                                                          compoundToExtract == "substrate" &
                                                          all(AllCompoundsID == "substrate"),
                                                       MyCompound, "UNKNOWN"), 
                                     ObsFile = NA,
                                     Species = ifelse(is.na(Deets$Species), 
                                                      "human", 
                                                      sub("sim-", "", 
                                                          tolower(Deets$Species))),
                                     Time_units = SimTimeUnits,
                                     Conc_units = SimConcUnits) %>%
                              select(-ID, -Indiv_code) %>% 
                              unique()
                        ))
                  }
               }
            }
            
         } else {
            # If the user did specify an observed data file, read in
            # observed data.
            obs_data <- extractObsConcTime(obs_data_file) %>%
               mutate(Compound = ObsCompounds[CompoundID], 
                      Inhibitor = ifelse(Inhibitor == "inhibitor" &
                                            complete.cases(AllPerps_comma),
                                         AllPerps_comma, Inhibitor))
            
            if(CompoundType == "ADC"){
               obs_data <- obs_data %>% 
                  mutate(CompoundID = ifelse(CompoundID == "primary metabolite 1", 
                                             "released payload", CompoundID))
            }
            
            obs_data <- obs_data %>% filter(CompoundID == cmpd)
            
            if(nrow(obs_data) == 0){
               rm(obs_data)
            } else {
               
               # If obs_data_file included compounds that were not present in
               # the simulation, don't include those and give the user a
               # warning.
               Missing <- setdiff(unique(obs_data$CompoundID), 
                                  names(ObsCompounds[complete.cases(ObsCompounds)])) 
               
               if(length(Missing) > 0){
                  warning(paste0("The observed data file includes ",
                                 str_comma(Missing), 
                                 ", which is/are not present in the simulated data. Observed data for ", 
                                 str_comma(Missing), 
                                 " will not be included in the output.\n"),
                          call. = FALSE)
                  obs_data <- obs_data %>% 
                     filter(!CompoundID %in% Missing)
               }
               
               # As necessary, convert simulated data units to match the
               # observed data
               if("individual" %in% returnAggregateOrIndiv){
                  sim_data_ind[[cmpd]] <-
                     match_units(DF_to_adjust = sim_data_ind[[cmpd]],
                                 goodunits = obs_data, 
                                 MW = Deets[paste0("MW", AllCompounds$Suffix[AllCompounds$CompoundID == cmpd])])
               }
               
               if("aggregate" %in% returnAggregateOrIndiv){
                  sim_data_mean[[cmpd]] <-
                     match_units(DF_to_adjust = sim_data_mean[[cmpd]],
                                 goodunits = obs_data, 
                                 MW = Deets[paste0("MW", AllCompounds$Suffix[AllCompounds$CompoundID == cmpd])])
               }
            }
         }
      }
      
      if(exists("obs_data", inherits = FALSE)){
         obs_data <- obs_data %>%
            mutate(Simulated = FALSE,
                   Trial = as.character(
                      ifelse(Inhibitor == "none",
                             "obs", "obs+inhibitor")))
         
         if(any(is.na(obs_data$Inhibitor)) & length(AllPerpsPresent) == 0){
            warning("There is a mismatch of some kind between the observed data and the simulated data in terms of a perpetrator or inhibitor being present. Please check that the output from this function looks the way you'd expect. Have you perhaps included observed data with an inhibitor present but the simulation does not include an inhibitor?\n",
                    call. = FALSE)
         }
      }
      
      # Dosing regimen info ---------------------------------------------------
      DosingScenario <- switch(cmpd,
                               "substrate" = Deets$Regimen_sub,
                               "primary metabolite 1" = Deets$Regimen_sub,
                               "primary metabolite 2" = Deets$Regimen_sub,
                               "secondary metabolite" = Deets$Regimen_sub,
                               "inhibitor 1" = Deets$Regimen_inhib,
                               "inhibitor 2" = Deets$Regimen_inhib2,
                               "inhibitor 1 metabolite" = Deets$Regimen_inhib)
      DosingScenario <- ifelse(CompoundType == "ADC", Deets$Regimen_sub, DosingScenario)
      
      if(adjust_obs_time & DosingScenario == "Multiple Dose" &
         exists("obs_data", inherits = FALSE)){
         # If this were a multiple-dose simulation, the observed data is,
         # presumably, at steady state. The simulated time we'd want those
         # data to match would be the *last* dose. Adjusting the time for the
         # obs data if the user requested that.
         
         DoseFreq <- Deets[["DoseInt_sub"]]
         NumDoses <- Deets[["NumDoses_sub"]]
         LastDoseTime <- DoseFreq * (NumDoses - 1)
         obs_data <- obs_data %>% mutate(Time = Time + LastDoseTime)
      }
      
   } # end of loop for each compound cmpd   
   
   # Putting everything together ----------------------------------------
   
   Data <- list()
   
   if("aggregate" %in% returnAggregateOrIndiv & Deets$PopRepSim == "No"){
      
      Data[["agg"]] <- bind_rows(sim_data_mean) %>%
         mutate(Simulated = TRUE, 
                Species = ifelse(is.na(Deets$Species), 
                                 "human", 
                                 tolower(sub("Sim-", "", Deets$Species)))) %>%
         arrange(Trial, Time)
   }
   
   if("individual" %in% returnAggregateOrIndiv){
      Data[["indiv"]] <- bind_rows(sim_data_ind) %>%
         mutate(Simulated = TRUE,
                Individual = as.character(Individual),
                Trial = as.character(Trial), 
                Species = ifelse(is.na(Deets$Species), 
                                 "human", 
                                 tolower(sub("Sim-", "", Deets$Species)))) %>%
         arrange(Individual, Time)
   }
   
   if(exists("obs_data", inherits = FALSE)){
      Data[["obs"]] <- obs_data %>% 
         mutate(Species = tolower(sub("Sim-", "", Species)))
   }
   
   Data <- bind_rows(Data) %>% 
      mutate(Species = ifelse(Species == "beagle", "dog", Species))
   
   if("individual" %in% returnAggregateOrIndiv){
      Data <- Data %>%
         mutate(Individual = ifelse(is.na(Individual), Trial, Individual),
                Individual =
                   factor(Individual,
                          levels = c(
                             c("obs", "obs+inhibitor", "mean", "median",
                               "geomean", "per5", "per95", "per10", "per90"),
                             setdiff(unique(Individual),
                                     c("obs", "obs+inhibitor", "mean", "median",
                                       "geomean", "per5", "per95", "per10", "per90")))))
   }
   
   Data <- calc_dosenumber(ct_dataframe = Data %>% mutate(File = sim_data_file), 
                           existing_exp_details = existing_exp_details)
   
   # Finalizing
   Data <- Data %>%
      mutate(Trial = factor(Trial, levels = c(
         c("obs", "obs+inhibitor", "mean", "median",
           "geomean", "per5", "per95", "per10", "per90"),
         setdiff(unique(Trial),
                 c("obs", "obs+inhibitor", "mean", "median",
                   "geomean", "per5", "per95", "per10", "per90")))),
         Tissue = tissue,
         Tissue = recode(Tissue, "jejunum i" = "jejunum I",
                         "jejunum ii" = "jejunum II",
                         "ileum i" = "ileum I", "ileum ii" = "ileum II",
                         "ileum iii" = "ileum III", 
                         "ileum iv" = "ileum IV"),
         File = sim_data_file) %>%
      arrange(across(any_of(c("Compound", "Inhibitor", "Simulated",
                              "Individual", "Trial", "Time")))) %>%
      select(any_of(c("Compound", "CompoundID", "Inhibitor", "Simulated",
                      "Species", "Tissue", "Individual", "Trial",
                      "Simulated", "Time", "Conc", "SD_SE",
                      "Time_units", "Conc_units", "subsection_ADAM", "DoseNum",
                      "DoseInt", "Dose_sub", "Dose_inhib", "Dose_inhib2", 
                      "File", "ObsFile")))
   
   # Filtering to return ONLY the compound the user requested. This is what
   # works for input to ct_plot at the moment, too, so things get buggered up
   # if there are multiple compounds and the user called on extractConcTime
   # itself rather than extractConcTime_mult.
   if(fromMultFunction == FALSE){
      Data <- Data %>%
         filter(CompoundID %in% c(compoundToExtract, "UNKNOWN"))
   }
   
   return(Data)
}


