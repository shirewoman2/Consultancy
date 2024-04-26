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
   
   # Checking for file name issues
   CheckFileNames <- check_file_name(sim_data_file)
   BadFileNames <- CheckFileNames[!CheckFileNames == "File name meets naming standards."]
   if(length(BadFileNames)> 0){
      BadFileNames <- paste0(names(BadFileNames), ": ", BadFileNames)
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"), "\n"),
              call. = FALSE)
   }
   
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
   
   ADCCompoundIDs <- c("total protein",
                       "conjugated protein",
                       "released payload")
   
   if(any(compoundToExtract %in% c(MainCompoundIDs, ADCCompoundIDs) == FALSE)){
      stop("The compound for which you requested concentration-time data was not one of the possible options. For 'compoundToExtract', please enter 'substrate', 'primary metabolite 1', 'secondary metabolite', 'inhibitor 1', 'inhibitor 2', or 'inhibitor 1 metabolite'.",
           call. = FALSE)
   }
   
   # Noting whether the tissue was from an ADAM model
   ADAM <- tissue %in% c("stomach", "duodenum", "jejunum i", "jejunum ii", "ileum i",
                         "ileum ii", "ileum iii", "ileum iv", "colon", "faeces", 
                         "cumulative absorption", "cumulative dissolution", 
                         "cumulative fraction released", "gut tissue")
   
   
   # Main body of function -----------------------------------------------------
   
   ## Getting exp details ------------------------------------------------------
   if(fromMultFunction || ("logical" %in% class(existing_exp_details) == FALSE)){
      
      Deets <- filter_sims(existing_exp_details, sim_data_file, "include")
      Deets <- harmonize_details(Deets)[["MainDetails"]] %>% 
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
   
   
   ## Additional error catching now that we have Deets -------------------------
   
   # Checking that the file is, indeed, a simulator output file.
   SheetNames <- gsub("`", "", str_split_1(Deets$SheetNames, "` `"))
   
   if(all(c("Input Sheet", "Summary") %in% SheetNames) == FALSE){
      # Using "warning" instead of "stop" here b/c I want this to be able to
      # pass through to other functions and just skip any files that
      # aren't simulator output.
      warning(paste("The file", sim_data_file,
                    "does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file.\n"), 
              call. = FALSE)
      return(data.frame())
   }
   
   if(Deets$SimulatorUsed != "Simcyp Simulator" & 
      tissue %in% AllTissues$Tissue_input[
         AllTissues$SimulatorAvailability == "Simcyp Simulator only"]){
      
      BadTissue <- setdiff(tissue, c("plasma", "blood"))
      
      warning(paste0("The tissue(s) ", 
                     str_comma(paste0("`", BadTissue, "`")), 
                     " are not available for Simcyp Discovery files and will be ignored.\n"), 
              call. = FALSE)
      
      tissue <- intersect(tissue, c("plasma", "blood"))
      
      if(length(tissue) == 0){
         warning(paste0("None of the tissues requested could be found in the file ", 
                        sim_data_file, ".\n"), 
                 call. = FALSE)
         
         return(data.frame())
      }
   }
   
   
   ## Checking a few things based on Deets -------------------------------------
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
   
   # Extracting tissue or plasma/blood data? Sheet format differs.
   TissueType <- case_when(
      str_detect(tissue, "plasma|blood|peripheral") ~ "systemic",
      str_detect(tissue, "portal|liver") ~ "liver",
      str_detect(tissue, "faeces") ~ "faeces", 
      TRUE ~ "tissue")
   
   if(any(str_detect(compoundToExtract, "metabolite|inhibitor 2")) &
      TissueType %in% c("tissue", "faeces")){
      warning("You have requested metabolite or inhibitor 2 concentrations in a solid tissue, which the simulator does not provide.\n",
              call. = FALSE)
      compoundToExtract <- 
         compoundToExtract[!str_detect(compoundToExtract, "metabolite|inhibitor 2")]
   }
   
   ## Determining correct Excel tab and reading it in --------------------------
   
   # If extractConcTime is called alone, there will be only 1 compound ID. If
   # it's called from extractConcTime_mult, then we've already filtered to make
   # sure that that combination of compound ID and tissue are available on the
   # same tab. 
   
   sim_data_xl <- eCT_readxl(sim_data_file = sim_data_file, 
                             Deets = Deets, 
                             compoundToExtract = compoundToExtract, 
                             tissue = tissue, 
                             TissueType = TissueType,
                             SheetNames = SheetNames)
   
   if(nrow(sim_data_xl) == 0){
      return(data.frame())
   }
   
   AdvBrainModel <- any(str_detect(sim_data_xl$...1, "Intracranial"), na.rm = TRUE)
   
   ## Harmonizing compound names ---------------------------------------------
   
   sim_data_xl <- eCT_harmonize(sim_data_xl = sim_data_xl, 
                                compoundToExtract = compoundToExtract, 
                                AllCompoundsPresent = AllCompoundsPresent,
                                tissue = tissue, 
                                Deets = Deets, 
                                PerpPresent = PerpPresent, 
                                ADAM = ADAM,
                                AdvBrainModel = AdvBrainModel)
   
   ## Checking units ---------------------------------------------------------
   
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
                              "CBrainICF" ~ "brain ICF", # ICF = intracellular fluid
                              "CBrainISF" ~ "brain ISF", # ISF = interstitial fluid
                              "CCSFSpinal" ~ "spinal CSF", # CSF = cerebrospinal fluid
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
   
   # ADAM data include multiple types of concentrations. We'll extract each.
   # Also making "Cumulative" for cumulative absorption/dissolution lower case.
   # Using subsection_ADAM also for AdvBrainModel tissues. May need to changes
   # subsection_ADAM to "TissueSubtype" or something.
   if(ADAM|AdvBrainModel){
      subsection_ADAMs <- sub("Cumulative", "cumulative", SimConcUnits$Type)
   } else {
      subsection_ADAMs <- "regular"
   }
   
   SimTimeUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "^Time"))][1]
   SimTimeUnits <- ifelse(str_detect(SimTimeUnits, "Time.* \\(h\\)"), "hours", "days")
   
   
   # Extracting each compound ----------------------------------------------
   
   # Note: This is a loop for use by extractConcTime_mult. For just running
   # extractConcTime, this will only have a single iteration.
   
   sim_data <- list()
   
   for(cmpd in compoundToExtract){
      
      if(fromMultFunction){
         message(paste("          for compound ID =", cmpd))
      }
      
      sim_data[[cmpd]] <- list() 
      
      for(ss in subsection_ADAMs){
         
         # Pull the data needed 
         sim_data[[cmpd]][[ss]] <- 
            eCT_pulldata(sim_data_xl = sim_data_xl, 
                         cmpd = cmpd, 
                         AllPerpsPresent = AllPerpsPresent, 
                         pull_interaction_data = FALSE, 
                         fromMultFunction = fromMultFunction, 
                         Deets = Deets, 
                         ADAM = ADAM, 
                         ss = ss, 
                         AdvBrainModel = AdvBrainModel, 
                         tissue = tissue, 
                         SimConcUnits = SimConcUnits,
                         SimTimeUnits = SimTimeUnits,
                         returnAggregateOrIndiv = returnAggregateOrIndiv)
         
         if(length(AllPerpsPresent) > 0 & cmpd %in% c("substrate", 
                                                      "primary metabolite 1", 
                                                      "primary metabolite 2", 
                                                      "secondary metabolite")){
            sim_data[[cmpd]][[ss]] <- 
               bind_rows(sim_data[[cmpd]][[ss]], 
                         eCT_pulldata(sim_data_xl = sim_data_xl, 
                                      cmpd = cmpd, 
                                      AllPerpsPresent = AllPerpsPresent, 
                                      pull_interaction_data = TRUE, 
                                      fromMultFunction = fromMultFunction, 
                                      Deets = Deets, 
                                      ADAM = ADAM, 
                                      ss = ss, 
                                      AdvBrainModel = AdvBrainModel, 
                                      tissue = tissue, 
                                      SimConcUnits = SimConcUnits, 
                                      SimTimeUnits = SimTimeUnits,
                                      returnAggregateOrIndiv = returnAggregateOrIndiv))
         }
      }
      sim_data[[cmpd]] <- bind_rows(sim_data[[cmpd]])
   }
   
   sim_data <- bind_rows(sim_data)
   
   
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
   
   # if(CompoundType == "ADC"){
   #    ObsCompounds <- c(ObsCompounds,
   #                      "conjugated protein" = paste("conjugated", Deets$Substrate),
   #                      "total protein" = paste("total", Deets$Substrate),
   #                      "released payload" = Deets$PrimaryMetabolite1)
   # }
   
   AllPerps_comma <- ifelse(length(AllPerpsPresent) == 0,
                            NA, str_comma(AllPerpsPresent))
   
   # Use the supplied obs file here if a) tissue is systemic and b) the
   # function was NOT called from the mult function OR the function WAS
   # called from the mult function but the user supplied an obs data file.
   
   # !!!! CHANGING THIS. Allowing this to proceed even if it was from mult
   # function. NEED TO CHECK THAT THIS DOESN'T HAVE UNEXPECTED DOWNSTREAM
   # CONSEQUENCES.
   if(TissueType == "systemic"){
      
      # If the user did not specify a file to use for observed data, use the
      # observed data that they included for the simulation. Note that this will
      # NOT pull the observed data if the user asked for an inhibitor-related
      # compound b/c it's unlikely that that's what observed data they supplied
      # when they set up their simulation.
      if(is.na(obs_data_file)){
         
         if(any(str_detect(compoundToExtract, "inhibitor") == FALSE)){ # FIXME - check on this
            
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
                                                         all(compoundToExtract == "substrate") &
                                                         all(AllCompoundsID == "substrate"),
                                                      cmpd, "UNKNOWN"),
                                  Compound = ifelse(tissue == "plasma" &
                                                       all(compoundToExtract == "substrate") &
                                                       all(AllCompoundsID == "substrate"),
                                                    AllCompoundsPresent["substrate"], "UNKNOWN"),
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
         
         # FIXME - This is failing sometimes. not sure why. see acoziborole conmeds. 
         obs_data <- extractObsConcTime(obs_data_file) %>%
            mutate(CompoundID = as.character(CompoundID), # Need to include this b/c sometimes it could be a named character vector, which messes up the next step. 
                   Compound = ObsCompounds[CompoundID],
                   Inhibitor = ifelse(Inhibitor == "inhibitor" &
                                         complete.cases(AllPerps_comma),
                                      AllPerps_comma, Inhibitor))
         
         # if(CompoundType == "ADC"){
         #    obs_data <- obs_data %>%
         #       mutate(CompoundID = ifelse(CompoundID == "primary metabolite 1",
         #                                  "released payload", CompoundID))
         # }
         
         obs_data <- obs_data %>% filter(CompoundID %in% compoundToExtract)
         
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
            sim_data <- adjust_units(DF_to_adjust = sim_data,
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
   # DosingScenario <- ifelse(CompoundType == "ADC", Deets$Regimen_sub, DosingScenario)
   
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
   
   
   # Putting everything together ----------------------------------------
   
   Data <- list()
   
   Data[["sim"]] <- bind_rows(sim_data) %>%
      mutate(Simulated = TRUE,
             Species = ifelse(is.na(Deets$Species),
                              "human",
                              tolower(sub("Sim-", "", Deets$Species)))) %>%
      arrange(across(.cols = any_of(c("Trial", "Individual", "Time"))))
   
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


