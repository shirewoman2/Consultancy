#' Extract concentration-time data from a simulator output Excel file
#'
#' Extracts concentration-time data from simulator output Excel files and,
#' optionally, a separately specified observed data file, and puts all data into
#' a single, tidy data.frame. There are some nuances to how it deals with
#' observed data; please see the details at the bottom of this help file.
#'
#'
#' \strong{A note on observed data:} When observed data are included in a
#' simulator output file, because the simulator output does not explicitly say
#' whether those observed data were in the presence of an inhibitor or effector,
#' this function cannot tell the difference and will thus assume all observed
#' data included in the simulator output were for the substrate in the
#' \emph{absence} of any effector. It will further assume that the compound --
#' substrate or inhibitor 1 or primary metabolite 1 or whatever -- is the same
#' as \code{compoundToExtract}. If \code{compoundToExtract} was an inhibitor or
#' inhibitor metabolite, the observed data from the simulator output will NOT be
#' pulled since it is unlikely to be inhibitor concentrations.
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
#'   "Tissues" in the simulator. All possible options: "plasma", "blood",
#'   "unbound blood", "unbound plasma", "additional organ", "adipose", "bone",
#'   "brain", "feto-placenta", "GI tissue" (this is for a 1st-order absorption
#'   model; not ADAM model), "heart", "kidney", "liver", "lung", "muscle",
#'   "pancreas", "peripheral blood", "peripheral plasma", "peripheral unbound
#'   blood", "peripheral unbound plasma", "portal vein blood", "portal vein
#'   plasma", "portal vein unbound blood", "portal vein unbound plasma", "skin",
#'   or "spleen". If you have an ADAM model, additional options are: "stomach",
#'   "duodenum", "jejunum I", "jejunum II", "ileum I", "ileum II", "ileum III",
#'   "ileum IV", "colon", "faeces", or "gut tissue". Not case sensitive.
#' @param compoundToExtract For which compound do you want to extract
#'   concentration-time data? Options are "substrate" (default), "primary
#'   metabolite 1", "primary metabolite 2", "secondary metabolite", "inhibitor
#'   1" (this can be an inducer, inhibitor, activator, or suppresesor, but it's
#'   labeled as "Inhibitor 1" in the simulator), "inhibitor 2" for the 2nd
#'   inhibitor listed in the simulation, or "inhibitor 1 metabolite" for the
#'   primary metabolite of inhibitor 1. \emph{Note:} The simulator will report
#'   up to one metabolite for the 1st inhibitor but no other inhibitor
#'   metabolites. (Someone please correct me if that's wrong! -LS)
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated
#'   concentration-time data? Options are "aggregate", "individual", or "both"
#'   (default). Aggregated data are not calculated here but are pulled from the
#'   simulator output rows labeled as "Population Statistics".
#' @param expdetails If you have already run \code{extractExpDetails} to get all
#'   the details from the "Input Sheet", you can save some processing time by
#'   supplying it here, unquoted. If left as NA, this function will run
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
#'   \item{Inhibitor (as applicable)}{the inhibitor(s) or effector(s) of
#'   interest; this matches whatever you named "Inhibitor 1", "Inhibitor 2", or
#'   "Inhibitor 1 metabolite" in the simulator and will be a concatenation of
#'   all the effectors present}
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
#'   simulations), e.g., "solid compound", "free compound in lumen", "Heff",
#'   "absorption rate", "unreleased substrate in faeces", "unreleased inhibitor
#'   in faeces", "dissolved compound", or "luminal CLint of compound". }
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
                            adjust_obs_time = FALSE,
                            tissue = "plasma",
                            compoundToExtract = "substrate",
                            returnAggregateOrIndiv = "both",
                            expdetails = NA,
                            fromMultFunction = FALSE){
    
    if(returnAggregateOrIndiv[1] == "both"){
        returnAggregateOrIndiv <- c("aggregate", "individual")
    }
    
    # Error catching
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
    if(tissue == "feces"){
        tissue <- "faeces"
    }
    
    if(length(tissue) != 1 & fromMultFunction == FALSE){
        stop("You must enter one and only one option for 'tissue'. (Default is plasma.)",
             call. = FALSE)
    }
    
    if(length(compoundToExtract) != 1 & fromMultFunction == FALSE){
        stop("You must enter one and only one option for 'compoundToExtract'. (Default is the substrate.)",
             call. = FALSE)
    }
    
    tissue <- tolower(tissue)
    PossTiss <- c("gi tissue", "lung", "additional organ", "adipose",
                  "heart", "muscle", "feto-placenta", "bone", "kidney",
                  "skin", "pancreas", "brain", "liver", "spleen",
                  "plasma", "blood", "unbound plasma", "unbound blood",
                  "peripheral plasma", "peripheral blood",
                  "peripheral unbound plasma", "peripheral unbound blood",
                  "portal vein plasma", "portal vein blood",
                  "portal vein unbound plasma", "portal vein unbound blood",
                  "stomach", "duodenum", "jejunum i", "jejunum ii", "ileum i",
                  "ileum ii", "ileum iii", "ileum iv", "colon", "faeces",
                  "gut tissue")
    
    if(tissue %in% PossTiss == FALSE){
        stop("The requested tissue must be plasma, blood, or one of the options listed under 'Sheet Options', 'Tissues' in the Simulator or one of the ADAM model tissues. Please see the help file description for the 'tissue' argument.",
             call. = FALSE)
    }
    
    compoundToExtract <- tolower(compoundToExtract)
    if(any(compoundToExtract %in% c("substrate", "primary metabolite 1",
                                    "primary metabolite 2", "secondary metabolite",
                                    "inhibitor 1", "inhibitor 2", "inhibitor 1 metabolite",
                                    "inhibitor 2 metabolite") == FALSE)){
        stop("The compound for which you requested concentration-time data was not one of the possible options. For 'compoundToExtract', please enter 'substrate', 'primary metabolite 1', 'secondary metabolite', 'inhibitor 1', 'inhibitor 2', or 'inhibitor 1 metabolite'.",
             call. = FALSE)
    }
    
    # Noting whether the tissue was from an ADAM model
    ADAM <- tissue %in% c("stomach", "duodenum", "jejunum i", "jejunum ii", "ileum i",
                          "ileum ii", "ileum iii", "ileum iv", "colon", "faeces")
    
    # Getting summary data for the simulation(s)
    if(fromMultFunction){
        Deets <- expdetails
    } else {
        Deets <- extractExpDetails(sim_data_file, exp_details = "Input Sheet")
    } 
    
    # Effector present?
    EffectorPresent <- complete.cases(Deets$Inhibitor1)
    if(EffectorPresent == FALSE & any(str_detect(compoundToExtract, "inhibitor")) &
       fromMultFunction == FALSE){
        # Note: Asking for a non-existant effector from the mult function will
        # result in an empty data.frame for that, which is fine.
        stop("There are no inhibitor data in the simulator output file supplied. Please either submit a different output file or request concentration-time data for a substrate or metabolite.",
             call. = FALSE)
    }
    AllEffectors <- c(Deets$Inhibitor1, Deets$Inhibitor2,
                      Deets$Inhibitor1Metabolite)
    AllEffectors <- AllEffectors[complete.cases(AllEffectors)]
    
    # For matching the correct effector to the correct names in
    # the output file
    Eff_DF <- data.frame(CompoundID = c("inhibitor 1", "inhibitor 2",
                                        "inhibitor 1 metabolite"),
                         Compound = c(Deets$Inhibitor1,
                                      Deets$Inhibitor2,
                                      Deets$Inhibitor1Metabolite))
    
    # Extracting tissue or plasma/blood data? Sheet format differs.
    TissueType <- ifelse(str_detect(tissue, "plasma|blood|portal|peripheral"),
                         "systemic", "tissue")
    if(any(str_detect(compoundToExtract, "metabolite")) & TissueType == "tissue"){
        warning("You have requested metabolite concentrations in a solid tissue, which the simulator does not provide. Substrate or Inhibitor 1 concentrations will be provided instead, depending on whether you requested a substrate or inhibitor metabolite.",
                call. = FALSE)
        compoundToExtract <- compoundToExtract[!str_detect(compoundToExtract,
                                                           "metabolite")]
    }
    
    SheetNames <- readxl::excel_sheets(sim_data_file)
    
    if(fromMultFunction){
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
            filter(PossCompounds %in% compoundToExtract) %>% pull(Type) %>% 
            unique()
        
    } else {
        if(any(compoundToExtract %in% c("substrate", "inhibitor 1",
                                        "inhibitor 2", "inhibitor 1 metabolite"))){
            CompoundType <- "substrate"
        } else {
            CompoundType <- compoundToExtract
        }
    }
    
    if(TissueType == "systemic"){
        Piece1 <- ifelse(str_detect(tissue, "portal vein"), "PV ", "")
        
        Piece2 <- switch(CompoundType,
                         "substrate" = "Conc Profiles",
                         "primary metabolite 1" = "Sub Pri Met1",
                         "primary metabolite 2" = "Sub Pri Met2",
                         "secondary metabolite" = "Sub Sec Met")
        # If the tissue is the portal vein, all possible compounds are included, so make Piece2 be "Conc Profiles".
        Piece2 <- ifelse(str_detect(tissue, "portal vein"),
                         "Conc Profiles", Piece2)
        
        Piece3 <- ifelse(str_detect(tissue, "peripheral"), " CPeriph", " CSys")
        Piece3 <- ifelse(str_detect(tissue, "portal vein"),
                         "", Piece3)
        Piece3 <- ifelse(CompoundType != "substrate",
                         "", Piece3)
        
        Piece4 <- ifelse(str_detect(tissue, "unbound"), "Cu", "C")
        
        Piece5 <- str_to_title(str_extract(tissue, "blood|plasma"))
        
        SheetToDetect <- paste0(Piece1, Piece2, Piece3, "\\(", Piece4, Piece5, "\\)")
        SheetToDetect <- paste0(SheetToDetect, "|",
                                sub("Profiles", "Trials Profiles", SheetToDetect),
                                "|",
                                gsub("Profiles CSys", "Trials Profiles", SheetToDetect),
                                "|",
                                paste0(Piece1, Piece2, "\\(Trials\\)\\(",
                                       Piece4, Piece5, "\\)") )
    } else {
        SheetToDetect <-
            switch(tissue,
                   "gi tissue" = "Gut Tissue Conc",
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
                   "gut tissue" = "Gut Tissue Conc"
            )
    }
    
    if(tissue == "faeces"){ # NEED TO ADJUST THIS TO DEAL WITH MULT FUNCTION.
        SheetToDetect <- switch(compoundToExtract, 
                                "inhibitor 1" = "Faeces Prof. .Inh 1", 
                                # inhibitor 2 does not appear to be available
                                "substrate" = "Faeces Prof. .Sub")
    }
    
    Sheet <- SheetNames[str_detect(SheetNames, SheetToDetect)][1]
    
    if((length(Sheet) == 0 | is.na(Sheet))){
        if(fromMultFunction){
            warning(paste0("You requested data for ", str_comma(compoundToExtract),
                           " in ", tissue,
                           " from the file '",
                           sim_data_file, "', but that compound and/or tissue or that combination of compound and tissue is not available in that file and will be skipped."),
                    call. = FALSE)
            return(data.frame())
        } else {
            stop(paste0("You requested data for ", str_comma(compoundToExtract),
                        " in ", tissue,
                        " from the file '",
                        sim_data_file, "', but that compound and/or tissue or that combination of compound and tissue is not available in that file and will be skipped."),
                 call. = FALSE)
        }
    }
    
    # Reading in simulated concentration-time profile data
    sim_data_xl <- suppressMessages(
        readxl::read_excel(path = sim_data_file,
                           sheet = Sheet,
                           col_names = FALSE))
    
    # If "interaction" or "Csys" or other similar strings are part of the name
    # of any of the compounds, that messes up the regex. Substituting to
    # standardize the compound names.
    sim_data_xl$...1 <- sub(gsub("\\(|\\)", ".", Deets$Substrate),
                            "SUBSTRATE", sim_data_xl$...1)
    
    if(complete.cases(Deets$PrimaryMetabolite1)){
        sim_data_xl$...1 <- sub(gsub("\\(|\\)", ".", Deets$PrimaryMetabolite1),
                                "PRIMET1", sim_data_xl$...1)
    } else if("primary metabolite 1" %in% compoundToExtract){
        compoundToExtract <- setdiff(compoundToExtract, "primary metabolite 1")
    }
    
    if(complete.cases(Deets$PrimaryMetabolite2)){
        sim_data_xl$...1 <- sub(gsub("\\(|\\)", ".", Deets$PrimaryMetabolite2),
                                "PRIMET2", sim_data_xl$...1)
    } else if("primary metabolite 2" %in% compoundToExtract){
        compoundToExtract <- setdiff(compoundToExtract, "primary metabolite 2")
    }
    
    if(complete.cases(Deets$SecondaryMetabolite)){
        sim_data_xl$...1 <- sub(gsub("\\(|\\)", ".", Deets$SecondaryMetabolite),
                                "SECMET", sim_data_xl$...1)
    } else if("secondary metabolite" %in% compoundToExtract){
        compoundToExtract <- setdiff(compoundToExtract, "secondary metabolite")
    }
    
    if(complete.cases(Deets$Inhibitor1)){
        sim_data_xl$...1 <- sub(gsub("\\(|\\)", ".", Deets$Inhibitor1),
                                "EFFECTOR1", sim_data_xl$...1)
    } else if("inhibitor 1" %in% compoundToExtract){
        compoundToExtract <- setdiff(compoundToExtract, "inhibitor 1")
    }
    
    if(complete.cases(Deets$Inhibitor2)){
        sim_data_xl$...1 <- sub(gsub("\\(|\\)", ".", Deets$Inhibitor2),
                                "EFFECTOR2", sim_data_xl$...1)
    } else if("inhibitor 2" %in% compoundToExtract){
        compoundToExtract <- setdiff(compoundToExtract, "inhibitor 2")
    }
    
    if(complete.cases(Deets$Inhibitor1Metabolite)){
        sim_data_xl$...1 <- sub(gsub("\\(|\\)", ".", Deets$Inhibitor1Metabolite),
                                "EFFECTOR1MET", sim_data_xl$...1)
    } else if("inhibitor 1 metabolite" %in% compoundToExtract){
        compoundToExtract <- setdiff(compoundToExtract, "inhibitor 1 metabolite")
    }
    
    
    # Determining concentration and time units
    SimConcUnits <- as.character(
        sim_data_xl[2, which(str_detect(as.character(sim_data_xl[2, ]), "CMax"))])[1]
    SimConcUnits <- gsub("CMax \\(|\\)", "", SimConcUnits)
    
    # ADAM model -- except for gut tissue -- has different units depending on
    # what parameter you want.
    if(ADAM){
        PopStatRow <- which(sim_data_xl$...1 == "Population Statistics")
        Blank1 <- which(is.na(sim_data_xl$...1))
        Blank1 <- Blank1[Blank1 > PopStatRow][1]
        SimConcUnits <- sim_data_xl[PopStatRow:(Blank1 -1), 1] %>% 
            rename(OrigVal = ...1) %>% 
            mutate(TypeCode = str_extract(
                OrigVal, 
                "^Ms|^C Lumen Free|^Heff|^Absorption Rate|^Mur|^Md|^Inh Md|^Luminal CLint|CTissue"), 
                Type = recode(TypeCode, 
                              "Ms" = "solid compound", 
                              "C Lumen Free" = "free compound in lumen", 
                              "Heff" = "Heff", 
                              "Absorption Rate" = "absorption rate", 
                              "Mur" = "unreleased substrate in faeces", 
                              "Inh Mur" = "unreleased inhibitor in faeces", 
                              "Md" = "dissolved compound", 
                              "Luminal CLint" = "luminal CLint of compound"), 
                ConcUnit = str_extract(OrigVal, "mg/h|mg/L|mg/mL|µg/L|µg/mL|ng/L|ng/mL|µM|nM|mg|µg|ng|mmol|µmol|nmol|mM|L/h|mg/h")) %>% 
            filter(complete.cases(TypeCode)) %>% select(-OrigVal) %>% unique()
    }
    
    SimTimeUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "^Time"))][1]
    SimTimeUnits <- ifelse(str_detect(SimTimeUnits, "Time.* \\(h\\)"), "hours", "minutes")
    
    # Extracting each compound ----------------------------------------------
    
    # Note: This is a loop for use by extractConcTime_mult. For just running
    # extractConcTime, this will only have a single iteration.
    
    sim_data_mean <- list()
    sim_data_ind <- list()
    
    for(m in compoundToExtract){
        
        if(fromMultFunction){
            message(paste("Extracting data for compound =", m))
        }
        
        # "NotAvail" is a hack to skip this iteration of the loop if it's ADAM
        # concentrations that the user requested but they're not available for
        # this tissue/compound combination.
        NotAvail <- FALSE
        
        if(ADAM & m != "substrate"){
            if(tissue == "faeces"){
                if(m != "inhibitor 1"){
                    if(fromMultFunction){
                        warning(paste0(str_to_title(m), 
                                       " concentrations are not available for ",
                                       tissue, " and thus will not be extracted."),
                                call. = FALSE)
                        NotAvail <- TRUE
                    } else {
                        stop(paste0(str_to_title(m), 
                                    " concentrations are not available for ",
                                    tissue, " and thus cannot be extracted."),
                             call. = FALSE)
                    }
                }
            } else {
                if(fromMultFunction){
                    warning(paste0(str_to_title(m), 
                                   " concentrations are not available for ",
                                   tissue, " and thus will not be extracted."),
                            call. = FALSE) 
                    NotAvail <- TRUE
                } else {
                    stop(paste0(str_to_title(m), 
                                " concentrations are not available for ",
                                tissue, " and thus cannot be extracted."),
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
        
        MyCompound <- switch(paste(m, TissueType),
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
        
        if(EffectorPresent){
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
                
                # EffString <- gsub("_|\\-|\\+|\\(|\\)",
                #                   ".",
                #                   AllEffectors)
                temp <- data.frame(Name = NamesToCheck[
                    # which(str_detect(NamesToCheck,
                    #                  str_c(EffString, collapse = "|")))]) %>%
                    which(str_detect(NamesToCheck, "EFFECTOR1"))]) %>% 
                    mutate(Number = str_extract(Name, "ISys [0-9]|I[a-z]* [0-9]|InhM"),
                           Inhibitor =
                               str_extract(Name,
                                           # str_c(paste0(EffString, "( \\(.*\\))?$"),
                                           #       collapse = "|")),
                                           "EFFECTOR1")) %>%
                    select(Number, Inhibitor) %>% unique()
                NumCheck <- temp$Number
                names(NumCheck) <- temp$Inhibitor
                
                if(length(NumCheck) == 0){
                    NumCheck <- "Inh 1"
                    names(NumCheck) <- Deets$Inhibitor1
                }
                
                rm(temp, TimeRow, FirstBlank, NamesToCheck)
            } 
        }
        
        # ADAM data include multiple types of concentrations. Dealing
        # with that and extracting each one.
        if(ADAM){
            subsection_ADAMs <- SimConcUnits$Type
        } else {
            subsection_ADAMs <- "regular"
        }
        
        # aggregate data -------------------------------------------------------
        if("aggregate" %in% returnAggregateOrIndiv){
            
            sim_data_mean[[m]] <- list()
            
            ## m is substrate or substrate metabolite -----------
            if(str_detect(m, "substrate|metabolite") &
               !str_detect(m, "inhibitor")){
                
                TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
                TimeRow <- TimeRow[TimeRow > which(sim_data_xl$...1 == "Population Statistics")][1]
                
                # Figuring out which rows contain which data
                FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                        which(1:nrow(sim_data_xl) > TimeRow))[1]
                FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
                NamesToCheck <- tolower(sim_data_xl$...1[TimeRow:(FirstBlank-1)])
                
                for(n in subsection_ADAMs){
                    
                    # message(paste("subsection_ADAMs n =", n))
                    # Some sheets have all compounds included, so need to narrow
                    # down which rows to check. Others don't have metabolites
                    # listed on the same sheet, so that's why there are these
                    # options.
                    if(str_detect(tissue, "portal") | TissueType == "tissue"){
                        if(ADAM){
                            Include <- which(str_detect(NamesToCheck,
                                                        paste0("^", 
                                                               tolower(SimConcUnits$TypeCode[
                                                                   SimConcUnits$Type == n]))))
                        } else {
                            Include <-
                                which(str_detect(
                                    NamesToCheck,
                                    switch(m,
                                           "substrate" =
                                               paste0("^cpv|^ctissue|^c lumen free|^c", tolower(tissue)),
                                           "primary metabolite 1" =
                                               paste0("^mpv |^mpv\\+|^mtissue|^m", tolower(tissue)),
                                           "primary metabolite 2" =
                                               paste0("^pm2pv |^pm2pb\\+|^pm2tissue|^pm2", tolower(tissue)),
                                           "secondary metabolite" =
                                               paste0("^miipv|^miitissue|^mii", tolower(tissue)),
                                           "inhibitor 1" =
                                               paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                           "inhibitor 2" =
                                               paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                           "inhibitor 1 metabolite" =
                                               paste0("^cpv|^ctissue|^c", tolower(tissue)))))
                        }
                        
                    } else {
                        Include <- which(str_detect(NamesToCheck, "^csys"))
                    }
                    
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
                        sim_data_mean[[m]][[n]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                            t() %>%
                            as.data.frame() %>% slice(-(1:3)) %>%
                            mutate_all(as.numeric)
                    )
                    names(sim_data_mean[[m]][[n]]) <- c("Time", names(RowsToUse))
                    sim_data_mean[[m]][[n]] <- sim_data_mean[[m]][[n]] %>%
                        pivot_longer(names_to = "Trial", values_to = "Conc",
                                     cols = -c(Time)) %>%
                        mutate(Compound = MyCompound,
                               CompoundID = m,
                               Inhibitor = "none",
                               Time_units = SimTimeUnits,
                               Conc_units = ifelse(ADAM, 
                                                   SimConcUnits$ConcUnit[
                                                       SimConcUnits$Type == n],
                                                   SimConcUnits), 
                               subsection_ADAM = ifelse(ADAM, n, NA))
                    
                    rm(RowsToUse, Include)
                }
                
                if(EffectorPresent){
                    
                    for(n in subsection_ADAMs){
                        # message(paste("subsection_ADAMs n =", n))
                        # Some sheets have all compounds included, so need to narrow
                        # down which rows to check. Others don't have metabolites
                        # listed on the same sheet, so that's why there are these
                        # options.
                        if(str_detect(tissue, "portal") | TissueType == "tissue"){
                            if(ADAM){
                                Include <- which(str_detect(NamesToCheck,
                                                            paste0("^", 
                                                                   tolower(SimConcUnits$TypeCode[
                                                                       SimConcUnits$Type == n]))))
                            } else {
                                Include <-
                                    which(str_detect(
                                        NamesToCheck,
                                        switch(m,
                                               "substrate" =
                                                   paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                               "primary metabolite 1" =
                                                   paste0("^mpv |^mpv\\+|^mtissue|^m", tolower(tissue)),
                                               "primary metabolite 2" =
                                                   paste0("^pm2pv |^pm2pb\\+|^pm2tissue|^pm2", tolower(tissue)),
                                               "secondary metabolite" =
                                                   paste0("^miipv|^miitissue|^mii", tolower(tissue)),
                                               "inhibitor 1" =
                                                   paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                               "inhibitor 2" =
                                                   paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                               "inhibitor 1 metabolite" =
                                                   paste0("^cpv|^ctissue|^c", tolower(tissue)))))
                            }
                        } else {
                            Include <- which(str_detect(NamesToCheck, "^csys"))
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
                                sim_data_mean_SubPlusEffector <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                                    t() %>%
                                    as.data.frame() %>% slice(-(1:3)) %>%
                                    mutate_all(as.numeric)
                            )
                            names(sim_data_mean_SubPlusEffector) <- c("Time", names(RowsToUse))
                            sim_data_mean_SubPlusEffector <- sim_data_mean_SubPlusEffector %>%
                                pivot_longer(names_to = "Trial", values_to = "Conc",
                                             cols = -c(Time)) %>%
                                mutate(Compound = MyCompound,
                                       Inhibitor = str_comma(AllEffectors),
                                       CompoundID = m,
                                       Time_units = SimTimeUnits,
                                       Conc_units = ifelse(ADAM, 
                                                           SimConcUnits$ConcUnit[
                                                               SimConcUnits$Type == n],
                                                           SimConcUnits), 
                                       subsection_ADAM = ifelse(ADAM, n, NA))
                            
                            sim_data_mean[[m]][[n]] <- bind_rows(sim_data_mean[[m]][[n]],
                                                                 sim_data_mean_SubPlusEffector)
                            
                        }
                    }
                }
                
                sim_data_mean[[m]] <- bind_rows(sim_data_mean[[m]])
                
                rm(TimeRow, FirstBlank, NamesToCheck)
                
            }
            
            ## m is an inhibitor or inhibitor metabolite -----------
            
            # Inhibitor concentrations are only present on tabs
            # w/substrate info for systemic tissues.
            if(m %in% c("inhibitor 1", "inhibitor 2",
                        "inhibitor 1 metabolite") &
               length(AllEffectors) > 0){
                
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
                
                # Need to do this for each inhibitor present
                for(i in Eff_DF %>% filter(complete.cases(Compound)) %>% pull(CompoundID)){
                    
                    # message(paste("AllEffectors i =", i))
                    sim_data_mean[[m]][[i]] <- list()
                    
                    for(n in subsection_ADAMs){
                        
                        # message(paste("subsection_ADAMs n =", n))
                        # Some sheets have all compounds included, so need to narrow
                        # down which rows to check. Others don't have metabolites
                        # listed on the same sheet, so that's why there are these
                        # options.
                        if(str_detect(tissue, "portal") | TissueType == "tissue"){
                            if(ADAM){
                                Include <- which(str_detect(NamesToCheck,
                                                            paste0("^", 
                                                                   SimConcUnits$TypeCode[
                                                                       SimConcUnits$Type == n])))
                            } else {
                                Include <- which(str_detect(NamesToCheck, NumCheck[i]))
                            }
                        } else {
                            Include <- which(str_detect(NamesToCheck,
                                                        NumCheck[switch(i, 
                                                                        "inhibitor 1" = "EFFECTOR1", 
                                                                        "inhibitor 2" = "EFFECTOR2", 
                                                                        "inhibitor 1 metabolite" = "EFFECTOR1MET")]))
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
                            sim_data_mean[[m]][[i]][[n]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                                t() %>%
                                as.data.frame() %>% slice(-(1:3)) %>%
                                mutate_all(as.numeric)
                        )
                        names(sim_data_mean[[m]][[i]][[n]]) <-
                            c("Time", names(RowsToUse))
                        sim_data_mean[[m]][[i]][[n]] <- sim_data_mean[[m]][[i]][[n]] %>%
                            pivot_longer(names_to = "Trial", values_to = "Conc",
                                         cols = -c("Time")) %>%
                            mutate(Compound = Eff_DF$Compound[which(Eff_DF$CompoundID == i)],
                                   CompoundID = i,
                                   Inhibitor = str_comma(AllEffectors),
                                   Time_units = SimTimeUnits,
                                   Conc_units = ifelse(ADAM, 
                                                       SimConcUnits$ConcUnit[
                                                           SimConcUnits$Type == n],
                                                       SimConcUnits), 
                                   subsection_ADAM = ifelse(ADAM, n, NA))
                        
                        rm(RowsToUse, Include)
                    }
                    
                    sim_data_mean[[m]][[i]] <- bind_rows(sim_data_mean[[m]][[i]])
                    
                }
                
                sim_data_mean[[m]] <- bind_rows(sim_data_mean[[m]])
                rm(NamesToCheck, TimeRow, FirstBlank)
            }
        }
        
        # individual data ------------------------------------------------------
        if("individual" %in% returnAggregateOrIndiv){
            
            StartIndiv <- which(str_detect(sim_data_xl$...1, "Individual Statistics"))
            
            sim_data_ind[[m]] <- list()
            
            ## m is substrate or substrate metabolite -----------
            if(str_detect(m, "substrate|metabolite") &
               !str_detect(m, "inhibitor")){
                
                # substrate data
                TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
                TimeRow <- TimeRow[TimeRow > StartIndiv][1]
                
                # ADAM data include multiple types of concentrations. Dealing
                # with that and extracting each one.
                if(ADAM){
                    subsection_ADAMs <- SimConcUnits$Type
                } else {
                    subsection_ADAMs <- "regular"
                }
                
                for(n in subsection_ADAMs){
                    # message(paste("subsection_ADAMs n =", n))
                    if(ADAM){
                        RowsToUse <- which(
                            str_detect(tolower(sim_data_xl$...1), 
                                       paste0("^", 
                                              tolower(SimConcUnits$TypeCode[
                                                  SimConcUnits$Type == n]))) &
                                !str_detect(sim_data_xl$...1, 
                                            "with interaction|Inh C Lumen Free"))
                    } else {
                        
                        RowsToUse <- which(
                            str_detect(sim_data_xl$...1,
                                       switch(ifelse(TissueType == "systemic",
                                                     TissueType,
                                                     paste(TissueType, m)),
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
                        sim_data_ind[[m]][[n]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                            t() %>%
                            as.data.frame() %>% slice(-(1:3)) %>%
                            mutate_all(as.numeric) %>%
                            rename(Time = "V1")
                    )
                    
                    SubjTrial <- sim_data_xl[RowsToUse, 2:3] %>%
                        rename(Individual = ...2, Trial = ...3) %>%
                        mutate(SubjTrial = paste0("ID", Individual, "_", Trial))
                    
                    names(sim_data_ind[[m]][[n]])[2:ncol(sim_data_ind[[m]][[n]])] <- SubjTrial$SubjTrial
                    
                    sim_data_ind[[m]][[n]] <- sim_data_ind[[m]][[n]] %>%
                        pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                                     cols = -Time) %>%
                        mutate(Compound = MyCompound,
                               CompoundID = m,
                               Inhibitor = "none",
                               SubjTrial = sub("ID", "", SubjTrial),
                               Time_units = SimTimeUnits,
                               Conc_units = ifelse(ADAM, 
                                                   SimConcUnits$ConcUnit[
                                                       SimConcUnits$Type == n],
                                                   SimConcUnits), 
                               subsection_ADAM = ifelse(ADAM, n, NA)) %>%
                        separate(SubjTrial, into = c("Individual", "Trial"),
                                 sep = "_")
                    
                    rm(RowsToUse)
                    
                }
                
                if(EffectorPresent){
                    
                    for(n in subsection_ADAMs){
                        # message(paste("subsection_ADAMs n =", n))
                        if(ADAM){
                            RowsToUse <- intersect(
                                which(
                                    str_detect(tolower(sim_data_xl$...1), 
                                               paste0("^", 
                                                      tolower(SimConcUnits$TypeCode[
                                                          SimConcUnits$Type == n])))), 
                                which(
                                    str_detect(sim_data_xl$...1, 
                                               "with interaction|Inh C Lumen")) )
                        } else {
                            
                            RowsToUse <- which(
                                str_detect(sim_data_xl$...1,
                                           switch(ifelse(TissueType == "systemic",
                                                         TissueType,
                                                         paste(TissueType, m)),
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
                                sim_data_ind_SubPlusEffector <-
                                    sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                                    t() %>%
                                    as.data.frame() %>% slice(-(1:3)) %>%
                                    mutate_all(as.numeric) %>%
                                    rename(Time = "V1")
                            )
                            names(sim_data_ind_SubPlusEffector)[
                                2:ncol(sim_data_ind_SubPlusEffector)] <- SubjTrial$SubjTrial
                            sim_data_ind_SubPlusEffector <-
                                sim_data_ind_SubPlusEffector %>%
                                pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                                             cols = -Time) %>%
                                mutate(Compound = MyCompound,
                                       CompoundID = m,
                                       Inhibitor = str_comma(AllEffectors),
                                       SubjTrial = sub("ID", "", SubjTrial),
                                       Time_units = SimTimeUnits,
                                       Conc_units = ifelse(ADAM, 
                                                           SimConcUnits$ConcUnit[
                                                               SimConcUnits$Type == n],
                                                           SimConcUnits), 
                                       subsection_ADAM = ifelse(ADAM, n, NA)) %>%
                                separate(SubjTrial, into = c("Individual", "Trial"),
                                         sep = "_")
                            
                            sim_data_ind[[m]][[n]] <- bind_rows(sim_data_ind[[m]][[n]],
                                                                sim_data_ind_SubPlusEffector)
                        }
                        
                        rm(RowsToUse)
                    }
                }
                
                sim_data_ind[[m]] <- bind_rows(sim_data_ind[[m]])
                
                rm(TimeRow)
            }
            
            ## m is an inhibitor or inhibitor metabolite ----------------------
            
            # Inhibitor concentrations are only present on tabs
            # w/substrate info for systemic tissues.
            if(m %in% c("inhibitor 1", "inhibitor 2",
                        "inhibitor 1 metabolite") &
               length(AllEffectors) > 0){
                
                sim_data_ind[[m]] <- list()
                
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
                
                # Need to do this for each inhibitor present
                for(i in Eff_DF %>% filter(complete.cases(Compound)) %>% pull(CompoundID)){
                    
                    # message(paste("AllEffectors i =", i))
                    sim_data_ind[[m]][[i]] <- list()
                    
                    for(n in subsection_ADAMs){
                        # message(paste("subsection_ADAMs n =", n))
                        # Some sheets have all compounds included, so need to
                        # narrow down which rows to check. Others don't have
                        # metabolites listed on the same sheet, so that's why
                        # there are these options.
                        if(str_detect(tissue, "portal") | TissueType == "tissue"){
                            if(ADAM){
                                Include <- 
                                    which(str_detect(NamesToCheck,
                                                     paste0("^", 
                                                            SimConcUnits$TypeCode[
                                                                SimConcUnits$Type == n])))
                            } else {
                                Include <- which(str_detect(NamesToCheck, NumCheck[i]))
                            }
                        } else {
                            Include <- which(str_detect(NamesToCheck,
                                                        NumCheck[switch(i, 
                                                                        "inhibitor 1" = "EFFECTOR1", 
                                                                        "inhibitor 2" = "EFFECTOR2", 
                                                                        "inhibitor 1 metabolite" = "EFFECTOR1MET")]))
                        }
                        
                        if(length(Include) == 0){
                            Include <- 1:length(NamesToCheck)
                        }
                        
                        if(ADAM){
                            RowsToUse <- 
                                which(
                                    str_detect(sim_data_xl$...1, 
                                               paste0("^", 
                                                      SimConcUnits$TypeCode[
                                                          SimConcUnits$Type == n])))
                        } else {
                            RowsToUse <- which(str_detect(
                                sim_data_xl$...1,
                                switch(TissueType,
                                       "systemic" = paste0(
                                           NumCheck[switch(i, 
                                                           "inhibitor 1" = "EFFECTOR1", 
                                                           "inhibitor 2" = "EFFECTOR2", 
                                                           "inhibitor 1 metabolite" = "EFFECTOR1MET")],
                                           " \\(|", i),
                                       "tissue" = paste0("ITissue\\(Inh 1|",
                                                         "I", tissue, " 1 \\("))
                            ))
                        }
                        
                        RowsToUse <- RowsToUse[which(RowsToUse > TimeRow)]
                        
                        suppressWarnings(
                            sim_data_ind[[m]][[i]][[n]] <-
                                sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                                t() %>%
                                as.data.frame() %>% slice(-(1:3)) %>%
                                mutate_all(as.numeric) %>%
                                rename(Time = "V1")
                        )
                        
                        SubjTrial <- sim_data_xl[RowsToUse, 2:3] %>%
                            rename(Individual = ...2, Trial = ...3) %>%
                            mutate(SubjTrial = paste0("ID", Individual, "_", Trial))
                        
                        names(sim_data_ind[[m]][[i]][[n]])[2:ncol(sim_data_ind[[m]][[i]][[n]])] <-
                            SubjTrial$SubjTrial
                        
                        sim_data_ind[[m]][[i]][[n]] <-
                            sim_data_ind[[m]][[i]][[n]] %>%
                            pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                                         cols = -Time) %>%
                            mutate(Compound = Eff_DF$Compound[which(Eff_DF$CompoundID == i)],
                                   CompoundID = i,
                                   Inhibitor = str_comma(AllEffectors),
                                   SubjTrial = sub("ID", "", SubjTrial),
                                   Time_units = SimTimeUnits,
                                   Conc_units = ifelse(ADAM, 
                                                       SimConcUnits$ConcUnit[
                                                           SimConcUnits$Type == n],
                                                       SimConcUnits), 
                                   subsection_ADAM = ifelse(ADAM, n, NA)) %>%
                            separate(SubjTrial, into = c("Individual", "Trial"),
                                     sep = "_")
                        
                        rm(RowsToUse)
                    }
                    
                    sim_data_ind[[m]][[i]] <- bind_rows(sim_data_ind[[m]][[i]])
                }
                
                sim_data_ind[[m]] <- bind_rows(sim_data_ind[[m]])
                
            }
        }
        
        # observed data -------------------------------------------------------
        # only applies to systemic concs
        
        # Setting up some names of observed data for use later
        ObsCompounds <-
            c("substrate" = Deets$Substrate,
              "inhibitor 1" = Deets$Inhibitor1, 
              "inhibitor 2" = Deets$Inhibitor2, 
              "primary metabolite 1" = Deets$PrimaryMetabolite1,
              "primary metabolite 2" = Deets$PrimaryMetabolite2,
              "secondary metabolite" = Deets$SecondaryMetabolite,
              "inhibitor 1 metabolite" = Deets$Inhibitor1Metabolite)
        
        AllEffectors_comma <- ifelse(length(AllEffectors) == 0,
                                     NA, str_comma(AllEffectors))
        
        if(TissueType == "systemic" & fromMultFunction == FALSE){
            
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
                            
                            warning("WARNING: This function is extracting observed data from simulator output, which does not contain information about the observed compound ID or whether the observed compound was in the presence of an effector. The safer way to include observed data is to supply a separate file for 'obs_data_file'.",
                                    call. = FALSE)
                            
                            NewNamesObs <- obs_data[1, ]
                            NewNamesObs[str_detect(NewNamesObs, "Time")] <- "Time"
                            # NewNamesObs <- gsub(" |\\: DV [0-9]", "", NewNamesObs)
                            TimeCols <- which(NewNamesObs == "Time")
                            ConcCols <- which(NewNamesObs != "Time")
                            NewNamesObs[TimeCols] <- paste0("Time_", NewNamesObs[ConcCols])
                            NewNamesObs[ConcCols] <- paste0("Conc_", NewNamesObs[ConcCols])
                            names(obs_data) <- NewNamesObs
                            
                            suppressWarnings(
                                obs_data <- obs_data %>%
                                    mutate_all(as.numeric) %>%
                                    mutate(ID = 1:nrow(.)) %>%
                                    pivot_longer(cols = -ID,
                                                 names_to = "Param",
                                                 values_to = "Value") %>%
                                    separate(col = Param,
                                             into = c("Parameter", "Individual"),
                                             sep = "_") %>%
                                    pivot_wider(names_from = Parameter,
                                                values_from = Value) %>%
                                    filter(complete.cases(Time)) %>%
                                    mutate(Trial = "obs",
                                           Inhibitor = "none",
                                           CompoundID = m,
                                           Compound = MyCompound, # NOTE THAT THIS IS ASSUMED!
                                           # The simulator doesn't provide much
                                           # info on the identity of the
                                           # compound for the observed data
                                           # included in a simjlator file.
                                           ObsFile = NA,
                                           Individual = sub("^Subject", "", Individual),
                                           Time_units = SimTimeUnits,
                                           Conc_units = SimConcUnits) %>%
                                    select(-ID)
                            )
                        }
                    }
                }
                
            } else {
                # If the user did specify an observed data file, read in
                # observed data.
                obs_data <- extractObsConcTime(obs_data_file) %>%
                    mutate(Compound = ObsCompounds[CompoundID], 
                           Inhibitor = ifelse(Inhibitor == "inhibitor" &
                                                  complete.cases(AllEffectors_comma),
                                              AllEffectors_comma, Inhibitor))
                
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
                                   " will not be included in the output."),
                            call. = FALSE)
                    obs_data <- obs_data %>% 
                        filter(!CompoundID %in% Missing)
                }
                
                # As necessary, convert simulated data units to match the
                # observed data
                if("individual" %in% returnAggregateOrIndiv){
                    sim_data_ind[[m]] <-
                        match_units(DF_to_adjust = sim_data_ind[[m]],
                                    goodunits = obs_data)
                }
                
                if("aggregate" %in% returnAggregateOrIndiv){
                    sim_data_mean[[m]] <-
                        match_units(DF_to_adjust = sim_data_mean[[m]],
                                    goodunits = obs_data)
                }
            }
        }
        
        if(exists("obs_data", inherits = FALSE)){
            obs_data <- obs_data %>%
                mutate(Simulated = FALSE,
                       Trial = as.character(
                           ifelse(Inhibitor == "none",
                                  "obs", "obs+inhibitor")))
            
            if(any(is.na(obs_data$Inhibitor)) & length(AllEffectors) == 0){
                warning("There is a mismatch of some kind between the observed data and the simulated data in terms of an effector or inhibitor being present. Please check that the output from this function looks the way you'd expect. Have you perhaps included observed data with an inhibitor present but the simulation does not include an inhibitor?",
                        call. = FALSE)
            }
            
            if(exists("obs_eff_data", inherits = FALSE)){
                obs_data <- bind_rows(obs_data, obs_eff_data)
            }
        }
        
        DosingScenario <- switch(m,
                                 "substrate" = Deets$Regimen_sub,
                                 "primary metabolite 1" = Deets$Regimen_sub,
                                 "primary metabolite 2" = Deets$Regimen_sub,
                                 "secondary metabolite" = Deets$Regimen_sub,
                                 "inhibitor 1" = Deets$Regimen_inhib,
                                 "inhibitor 2" = Deets$Regimen_inhib2,
                                 "inhibitor 1 metabolite" = Deets$Regimen_inhib)
        
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
        
    } # end of loop for each compound m   
    
    # Putting everything together ----------------------------------------
    Data <- list()
    
    if("aggregate" %in% returnAggregateOrIndiv){
        Data[["agg"]] <- bind_rows(sim_data_mean) %>%
            mutate(Simulated = TRUE) %>%
            arrange(Trial, Time)
    }
    
    if("individual" %in% returnAggregateOrIndiv){
        Data[["indiv"]] <- bind_rows(sim_data_ind) %>%
            mutate(Simulated = TRUE,
                   Individual = as.character(Individual),
                   Trial = as.character(Trial)) %>%
            arrange(Individual, Time)
    }
    
    if(exists("obs_data", inherits = FALSE)){
        Data[["obs"]] <- obs_data
    }
    
    Data <- bind_rows(Data)
    
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
    
    # Adding DoseNumber so that we can skip extractExpDetails in ct_plot when
    # the user requests a specific dose.
    MyIntervals <- 
        c("substrate" = Deets$DoseInt_sub,
          "primary metabolite 1" = Deets$DoseInt_sub,
          "primary metabolite 2" = Deets$DoseInt_sub,
          "secondary metabolite" = Deets$DoseInt_sub,
          "inhibitor 1" = ifelse(is.null(Deets$DoseInt_inhib),
                                 NA, Deets$DoseInt_inhib),
          "inhibitor 1 metabolite" = ifelse(is.null(Deets$DoseInt_inhib),
                                            NA, Deets$DoseInt_inhib),
          "inhibitor 2" = ifelse(is.null(Deets$DoseInt_inhib2),
                                 NA, Deets$DoseInt_inhib2))
    
    MyStartTimes <- 
        c("substrate" = Deets$StartHr_sub,
          "primary metabolite 1" = Deets$StartHr_sub,
          "primarymetabolite 2" = Deets$StartHr_sub,
          "secondary metabolite" = Deets$StartHr_sub,
          "inhibitor 1" = ifelse(is.null(Deets$StartHr_inhib), NA,
                                 Deets$StartHr_inhib),
          "inhibitor 2" = ifelse(is.null(Deets$StartHr_inhib2), NA,
                                 Deets$StartHr_inhib2),
          "inhibitor 1 metabolite" = ifelse(is.null(Deets$StartHr_inhib), NA,
                                            Deets$StartHr_inhib))
    
    MyMaxDoseNum <- 
        c("substrate" = ifelse(Deets$Regimen_sub == "Single Dose", 
                               1, Deets$NumDoses_sub),
          "primary metabolite 1" = ifelse(Deets$Regimen_sub == "Single Dose", 
                                          1, Deets$NumDoses_sub),
          "primarymetabolite 2" = ifelse(Deets$Regimen_sub == "Single Dose", 
                                         1, Deets$NumDoses_sub),
          "secondary metabolite" = ifelse(Deets$Regimen_sub == "Single Dose", 
                                          1, Deets$NumDoses_sub),
          "inhibitor 1" = ifelse(is.null(Deets$NumDoses_inhib), NA,
                                 ifelse(Deets$Regimen_inhib == "Single Dose", 
                                        1, Deets$NumDoses_inhib)),
          "inhibitor 2" = ifelse(is.null(Deets$NumDoses_inhib2), NA,
                                 ifelse(Deets$Regimen_inhib2 == "Single Dose", 
                                        1, Deets$NumDoses_inhib2)),
          "inhibitor 1 metabolite" = ifelse(is.null(Deets$NumDoses_inhib), NA,
                                            ifelse(Deets$Regimen_inhib == "Single Dose", 
                                                   1, Deets$NumDoses_inhib)))
    
    # Converting data to numeric while also retaining names
    suppressWarnings(
        MyIntervals <- sapply(MyIntervals, FUN = as.numeric))
    suppressWarnings(
        MyStartTimes <- sapply(MyStartTimes, FUN = as.numeric))
    suppressWarnings(
        MyMaxDoseNum <- sapply(MyMaxDoseNum, FUN = as.numeric))
    
    Data <- Data %>%
        mutate(StartHr = MyStartTimes[CompoundID],
               TimeSinceDose1 = Time - StartHr,
               DoseInt = MyIntervals[CompoundID],
               MaxDoseNum = MyMaxDoseNum[CompoundID],
               DoseNum = Time %/% DoseInt + 1,
               # Taking care of possible artifacts
               DoseNum = ifelse(DoseNum < 0, 0, DoseNum),
               DoseNum = ifelse(DoseNum > MaxDoseNum, MaxDoseNum, DoseNum),
               # If it was a single dose, make everything after StartHr dose
               # 1 and everything before StartHr dose 0. if it was a single
               # dose, then DoseInt is NA.
               DoseNum = ifelse(is.na(DoseInt),
                                ifelse(TimeSinceDose1 < 0, 0, 1), DoseNum))
    
    # Checking for any custom dosing
    if(any(str_detect(names(Deets), "CustomDosing"))){
        CDCompounds <-
            data.frame(CompoundSuffix = 
                           str_extract(names(Deets)[str_detect(names(Deets),
                                                               "CustomDosing")],
                                       "_sub|_inhib(2)?")) %>% 
            mutate(CompoundID = recode(CompoundSuffix, "_sub" = "substrate", 
                                       "_inhib" = "inhibitor 1", 
                                       "_inhib2" = "inhibitor 2"))
        
        if(any(unique(Data$CompoundID) %in% CDCompounds$CompoundID)){
            
            Dosing <- list()
            # This is kind of a disaster... Looking for a better way to code this.
            
            for(j in CDCompounds$CompoundID){
                
                # message(paste("CDCompounds$CompoundID j =", j))
                Dosing[[j]] <-
                    Deets[[paste0("CustomDosing", 
                                  CDCompounds$CompoundSuffix[CDCompounds$CompoundID == j])]] %>% 
                    mutate(CompoundID = CDCompounds$CompoundID[CDCompounds$CompoundID == j])
                
                if(max(Data$Time) > max(Dosing[[j]]$Time)){
                    Dosing[[j]] <- Dosing[[j]] %>% 
                        bind_rows(data.frame(Time = max(Data$Time) + 1, 
                                             DoseNum = max(Dosing[[j]]$DoseNum)))
                }
                
                # If there was a loading dose or something (not really sure what
                # this would be), then there are two dose numbers listed for t0.
                # Removing the earlier one so that this will work.
                if(any(duplicated(Dosing[[j]]$Time))){
                    warning(paste0("There were multiple dose numbers listed at the same time for the ",
                                   j," in the file ", sim_data_file, 
                                   "; did you mean for that to be the case? For now, the dose number at that duplicated time will be set to the 2nd dose number listed."),
                            call. = FALSE)
                    TimeToRemove <- which(duplicated(
                        Dosing[[j]]$Time, fromLast = TRUE))
                    Dosing[[j]] <- Dosing[[j]] %>% slice(-TimeToRemove)
                }
                
                Dosing[[j]]$Breaks <-
                    as.character(cut(Dosing[[j]]$Time, breaks = Dosing[[j]]$Time,
                                     right = FALSE))
            }
            
            OrigCompounds <- unique(Data$CompoundID)
            Data <- Data %>% 
                mutate(CD = ifelse(CompoundID %in% CDCompounds$CompoundID, 
                                   CompoundID, "not CD"))
            
            MyData <- list()
            MyData[["not CD"]] <- Data %>% filter(CD == "not CD")
            
            for(j in unique(Data$CD)[!unique(Data$CD) == "not CD"]){
                
                # message(paste("CDCompounds$CompoundID (not CD) j =", j))
                MyData[[j]] <- Data %>% filter(CD == j) %>% select(-DoseNum)
                # This should make the right breaks for each possible compound
                # with custom dosing. They should match the breaks in the
                # appropriate list item in Dosing.
                MyData[[j]]$Breaks <-
                    as.character(cut(MyData[[j]]$Time, breaks = Dosing[[j]]$Time,
                                     right = FALSE))
                
                MyData[[j]] <- MyData[[j]] %>% 
                    left_join(Dosing[[j]] %>% select(CompoundID, Breaks, DoseNum))
                
            }
            
            Data <- bind_rows(MyData)
            if(length(setdiff(unique(OrigCompounds),
                              unique(Data$CompoundID))) > 0){
                warning("PROBLEM WITH CUSTOM DOSING! Please tell Laura Shireman if you see this message.",
                        call. = FALSE)
            }
        }
    }
    
    # Checking for when the simulation ends right at the last dose b/c
    # then, setting that number to 1 dose lower
    if(length(Data %>% filter(DoseNum == max(Data$DoseNum)) %>%
              pull(Time) %>% unique()) == 1){
        MyMaxDoseNum <- max(Data$DoseNum)
        Data <- Data %>%
            mutate(DoseNum = ifelse(DoseNum == MyMaxDoseNum,
                                    MyMaxDoseNum - 1, DoseNum))
    }
    
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
                        "Tissue", "Individual", "Trial",
                        "Simulated", "Time", "Conc",
                        "Time_units", "Conc_units", "subsection_ADAM", "DoseNum",
                        "DoseInt", "File", "ObsFile")))
    
    # Filtering to return ONLY the compound the user requested. This is what
    # works for input to ct_plot at the moment, too, so things get buggered up
    # if there are multiple compounds and the user called on extractConcTime
    # itself rather than extractConcTime_mult.
    if(fromMultFunction == FALSE){
        Data <- Data %>%
            filter(CompoundID %in% compoundToExtract)
    }
    
    return(Data)
}


