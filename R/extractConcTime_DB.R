#' Extract concentration-time data from a Simcyp Simulator database file --
#' UNDER CONSTRUCTION!!!
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   concentration-time data, in quotes; must be an output file from the Simcyp
#'   simulator
#' @param tissue From which tissue should the desired concentrations be
#'   extracted? Default is plasma for typical plasma concentration-time data.
#'   All options: "plasma", "blood", "peripheral plasma", "portal vein plasma",
#'   "Kp,uu,brain", "Kp,uu,ICF", or "Kp,uu,ISF". We're working on adding more.
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
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#' @param obs_to_sim_assignment the assignment of which observed files go with
#'   which simulated files. (NA, which is the default, means no observed data
#'   will be extracted.) There are four ways to supply this:
#'
#'   \describe{\item{"use existing_exp_details"}{If you have already
#'   extracted the simulation experimental details with the function
#'   \code{\link{extractExpDetails_mult}} and you included observed data overlay
#'   files in your simulations, as long as those XML files have their
#'   corresponding Excel files in the \emph{same} location, we can use that
#'   information to figure out which observed Excel file should go with which
#'   simulation. Note that this \strong{does} require you to supply something
#'   for the argument \code{existing_exp_details} to work.}
#'
#'   \item{a character vector of the observed data files, each in
#'   quotes and encapsulated with \code{c(...)}}{If all the observed data can be
#'   compared to all the simulated data, then an example of acceptable input
#'   would be: \code{obs_to_sim_assignment = "obsdata1.xlsx"}. However, if you
#'   would like to specify which observed file goes with which simulated file,
#'   you can do this with a named character vector, e.g.,
#'   \code{c("obsdata1.xlsx" = "simfileA.xlsx", "obsdata2.xlsx" =
#'   "simfileB.xlsx")}. If one observed file needs to match more than one
#'   simulated file but not \emph{all} the simulated files, you can do that by
#'   separating the simulated files with commas, e.g.,
#'   \code{obs_to_sim_assignment = c("obs data 1.xlsx" = "mdz-5mg-qd.xlsx,
#'   mdz-5mg-qd-fa08.xlsx", "obs data 2.xlsx" = "mdz-5mg-qd-cancer.xlsx,
#'   mdz-5mg-qd-cancer-fa08.xlsx")}. Pay close attention to the position of
#'   commas and quotes there! This can get a bit confusing, in our opinions, so
#'   you may want to try the other options if you need to link specific observed
#'   and simulated files; they can be easier to follow but require more typing.}
#'
#'   \item{a data.frame with one column for the observed files and one column
#'   for the simulated files they each match}{The data.frame must have column
#'   names of "ObsFile" and "File" for the observed and simulated files,
#'   respectively. Here's an example of acceptable input:
#'   \code{obs_to_sim_assignment = data.frame(ObsFile = c("obsdata1.xlsx",
#'   "obsdata2.xlsx"), File = c("simfileA.xlsx", "simfileB.xlsx"))} Each row
#'   should contain one observed file and one simulated file, so if you want to
#'   compare a single observed file to multiple simulated files, you'll need to
#'   repeat the observed file, e.g., \code{obs_to_sim_assignment =
#'   data.frame(ObsFile = c("obsdata1.xlsx", "obsdata2.xlsx", "obsdata2.xlsx",
#'   "obsdata2.xlsx"), File = c("simfileA.xlsx", "simfileB.xlsx",
#'   "simfileC.xlsx", "simfileD.xlsx"))}}
#'
#'   \item{a csv file with one column for the observed files and one column for
#'   the simulated files they each match}{The setup of this csv file should be
#'   just like that described for supplying a data.frame, so one row for each
#'   pair of simulated and observed files you want to compare to each other.
#'   Supply this as a character string, like this: \code{obs_to_sim_assignment =
#'   "My obs to sim assignments.csv"}}}
#'
#'   For whichever option you choose, the observed files' paths should be
#'   included if they are located somewhere other than your working directory.
#'   The observed data files should be for the Excel file that it is
#'   \emph{ready} to be converted to an XML file, not the file that contains
#'   only the digitized time and concentration data. This function assumes that
#'   the dosing intervals for the observed data match those in the simulated
#'   data. See "Details" for more info.
#' @param conc_units desired concentration units; options are the same as the
#'   ones in the Excel form for PE data entry: "ng/mL" (default), "mg/L",
#'   "mg/mL", "µg/L" (or "ug/L"), "µg/mL" (or "ug/mL"), "ng/L", "µM" (or "uM"),
#'   or "nM".
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated
#'   concentration-time data? Options are "aggregate", "individual", or "both"
#'   (default). Aggregated data are not calculated here but are pulled from the
#'   simulator output rows labeled as "Population Statistics".
#'
#' @return a data.frame of concentration-time data
#' @export
#'
#' @examples
#' # none yet
extractConcTime_DB <- function(sim_data_file, 
                               existing_exp_details = NA,
                               compoundToExtract = "substrate", 
                               tissue = "plasma", 
                               obs_to_sim_assignment = "use existing_exp_details", 
                               conc_units = "ng/mL", 
                               returnAggregateOrIndiv = "aggregate"){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Check whether Simulator has been initialized. 
   SimInit <- check_simulator_initialized()
   if(SimInit == FALSE){
      warning(paste0(str_wrap(paste0(
         "The Simcyp Simulator has not been initialized, which must happen to pull data from the database file '", 
         sim_data_file, 
         "'. To initialize it, please run something like")), 
         
         "\n", 
         
         "   Simcyp::Initialise(species = Simcyp::SpeciesID$Human, requestedVersion = 23) \n"), 
         call. = FALSE)
      
      return(list())
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
   
   # If they didn't include ".db" at the end, add that.
   sim_data_file <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$|\\.db$", "", sim_data_file), ".db")
   
   # Main body of function ----------------------------------------------------
   
   # Note that this should be the workspace file and not the database file. 
   suppressMessages(Simcyp::SetWorkspace(
      sub("\\.db", ".wksz", sim_data_file), verbose = FALSE))
   
   conn <- RSQLite::dbConnect(RSQLite::SQLite(), sim_data_file) 
   
   ## Getting exp details as needed --------------------------------------------
   
   if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA and we have not yet extracted any details
      existing_exp_details <- 
         extractExpDetails_mult(sim_data_file = sim_data_file, 
                                exp_details = "Summary and Input", 
                                existing_exp_details = existing_exp_details)
      
   } else {
      
      existing_exp_details <- harmonize_details(existing_exp_details)
      
      if(sim_data_file %in% c(existing_exp_details$MainDetails$File, 
                              existing_exp_details$MainDetails$DBFile) == FALSE){
         existing_exp_details <- 
            extractExpDetails_mult(sim_data_files = sim_data_file, 
                                   exp_details = "Summary and Input", 
                                   existing_exp_details = existing_exp_details)
         
      }
   }
   
   # If the file is a Simulator output file, we should have it now. Checking and
   # removing any that are not.
   if(sim_data_file %in% c(existing_exp_details$MainDetails$File, 
                           existing_exp_details$MainDetails$DBFile) == FALSE){
      
      warning(wrapn(paste0("The file '",
                              sim_data_file, 
                              "' was requested but does not appear to be a Simcyp Simulator file.")), 
              call. = FALSE)
      
      return()
      
   }
   
   Deets <- harmonize_details(existing_exp_details) %>% 
      filter_sims(which_sims = sim_data_file, 
                  include_or_omit = "include")
   Deets <- Deets$MainDetails
   
   ## Dealing with possible observed data assignments -------------------------
   if("character" %in% class(obs_to_sim_assignment) && 
      any(str_detect(tolower(obs_to_sim_assignment), "use existing|use.*details"))){
      
      if("ObsOverlayFile" %in% names(Deets) == FALSE){
         warning("The observed data overlay file was not included in `existing_exp_details`, so we don't know which observed data files to use for the simulated files. We cannot extract any observed data.\n", 
                 call. = FALSE)
         ObsAssign <- data.frame()
         obs_to_sim_assignment <- NA
         
      } else {
         
         # Make this work for whoever the current user is, even if the XML
         # obs file path was for someone else.
         Deets$ObsOverlayFile <- 
            sub("Users\\\\.*\\\\Certara", 
                paste0("Users\\\\", Sys.info()["user"], "\\\\Certara"), 
                Deets$ObsOverlayFile)
         
         ObsAssign <- Deets %>% 
            select(File, ObsOverlayFile) %>% 
            rename(ObsFile = ObsOverlayFile) %>% 
            mutate(ObsFile = sub("\\.xml$", ".xlsx", ObsFile), 
                   ObsFile = gsub("\\\\", "/", ObsFile), 
                   ObsFile = sub("Users/.*/Certara", 
                                 paste0("Users/", Sys.info()["user"], 
                                        "/Certara"), ObsFile))
         
         if(nrow(ObsAssign %>% filter(complete.cases(ObsFile) & 
                                      File %in% sim_data_file)) == 0){
            ObsAssign <- data.frame()
            obs_to_sim_assignment <- NA
         } else {
            if(any(file.exists(ObsAssign$ObsFile[
               complete.cases(ObsAssign$ObsFile) &
               ObsAssign$File %in% sim_data_file]) == FALSE)){
               warning(paste0("We couldn't find the following observed data Excel files and thus cannot extract their data:\n", 
                              str_c(ObsAssign$ObsFile[
                                 which(
                                    file.exists(ObsAssign$ObsFile[
                                       complete.cases(ObsAssign$ObsFile) &
                                          ObsAssign$File %in% sim_data_file]) == FALSE)], 
                                 collapse = "\n"), "\n"), 
                       call. = FALSE)
            }
            
            ObsAssign <- ObsAssign %>% filter(file.exists(ObsFile) & 
                                                 File %in% sim_data_file)
            
            if(nrow(ObsAssign) == 0){
               warning("We can't find the Excel files that match the observed overlay files in your simulations. We cannot extract any observed data.\n", 
                       call. = FALSE)
               ObsAssign <- data.frame()
               obs_to_sim_assignment <- NA
            } 
         }
      }
   } else if("logical" %in% class(obs_to_sim_assignment)){
      
      ### SCENARIO B: No obs match -------------------------------------------
      
      # This is when the user has not specified anything for
      # obs_to_sim_assignment.
      ObsAssign <- data.frame()
      
   } else {
      
      ### SCENARIO C: Match w/user-specified files ------------------------------
      
      if("character" %in% class(obs_to_sim_assignment)){
         if(any(str_detect(obs_to_sim_assignment, ".csv$"))){
            # user has supplied a csv file for designating obs and sim
            # assignments.
            ObsAssign <- read.csv(obs_to_sim_assignment) %>% unique()
         } else {
            # Separating obs_to_sim_assignment so that it will work well
            # with each simulator file. I wanted this to be obs 1st and then
            # sim 2nd b/c it will often be the case that you would want to
            # compare multiple sim files to the same obs data, so I wanted
            # the value (sim file) to be able to be something convoluted
            # with commas I could separate. For *here*, though, inside the
            # actual function, it works better if things are named by the
            # sim file. Splitting up the character vector to get things
            # separated by sim file.
            
            # Making sure that the split pattern will work in case the user
            # omitted spaces.
            obs_to_sim_assignment <- gsub(",[^ ]", ", ", obs_to_sim_assignment)
            if(length(names(obs_to_sim_assignment)) > 0){
               ObsAssign <- as.data.frame(str_split(obs_to_sim_assignment, pattern = ", ", 
                                                    simplify = TRUE)) %>% 
                  mutate(ObsFile = names(obs_to_sim_assignment)) %>% 
                  pivot_longer(cols = -ObsFile, names_to = "V", values_to = "File") %>% 
                  select(File, ObsFile) %>% unique() %>% 
                  filter(File != "")
               
            } else {
               
               # NB: This is when the user wants a single obs file to match all
               # sim files they're extracting. 
               ObsAssign <- data.frame(ObsFile = obs_to_sim_assignment, 
                                       File = sim_data_file)
            }
         }
         
      } else if("data.frame" %in% class(obs_to_sim_assignment)){
         # This is when the user has supplied a data.frame for
         # obs_to_sim_assignment.
         ObsAssign <- obs_to_sim_assignment %>% unique()
      }
      
      # Tidying up a few things. Checking column names and dealing with any
      # misspecification of caps by user
      names(ObsAssign) <- toupper(names(ObsAssign))
      
      if("FILE" %in% names(ObsAssign) == FALSE && 
         "SIMFILE" %in% names(ObsAssign)){
         ObsAssign <- ObsAssign %>% rename(FILE = SIMFILE)
      } else if(all(c("FILE", "SIMFILE") %in% names(ObsAssign))){
         ObsAssign <- ObsAssign %>% select(-File) %>% rename(FILE = SIMFILE)
      }
      
      if(all(c("FILE", "OBSFILE") %in% names(ObsAssign)) == FALSE){
         warning("You have specified values for `obs_to_sim_assignment`, but it's not clear which should be for the observed files and which for the simulated files. Please check the help file for acceptable input. For now, we will not extract data from any observed data files.\n", 
                 call. = FALSE)
         ObsAssign <- NA
      } else {
         
         # Now that column names should be correct, converting to the case I like
         # for ease of coding
         ObsAssign <- ObsAssign %>% rename(File = FILE, ObsFile = OBSFILE) %>% 
            select(File, ObsFile)
         
         if(any(duplicated(ObsAssign$File))){ 
            Dups <- ObsAssign$File[duplicated(ObsAssign$File)]
            warning(paste0("You have more than one observed data file assigned to the simulator files ",
                           str_comma(paste0("`", Dups, "`")),
                           ". This function can only handle one observed file per simulator file, so only the first observed file listed will be used.\n"),
                    call. = FALSE)
            ObsAssign <- ObsAssign[!duplicated(ObsAssign$File), ]
         }
         
         if(any(complete.cases(ObsAssign$File))){
            MissingFiles <- setdiff(ObsAssign$File, sim_data_file)
            if(length(MissingFiles) > 0){
               warning(paste0("When you assigned observed data files to simulator files with the argument `obs_to_sim_assignment`, you included simulator files that are *not* included in `sim_data_file`. We cannot include these observed data files in the output data because we don't know which simulator files they belong with. The problem simulator files is/are: ", 
                              str_comma(MissingFiles), ", which is/are set to match the following observed files ",
                              str_comma(names(obs_to_sim_assignment[
                                 which(obs_to_sim_assignment %in% sim_data_file == FALSE)])), 
                              ".\n"), 
                       call. = FALSE)
               
               ObsAssign <- ObsAssign %>% filter(File %in% sim_data_file)
            }
            
            # Making sure obs files exist before trying to pull data from them
            if(any(file.exists(ObsAssign$ObsFile) == FALSE)){
               
               MissingObsFiles <- ObsAssign$ObsFile[
                  which(file.exists(ObsAssign$ObsFile) == FALSE)]
               warning(paste0("The file(s) ", 
                              str_comma(paste0("`", unique(MissingObsFiles), "`")), 
                              " is/are not present and thus will not be extracted.\n"), 
                       call. = FALSE)
               ObsAssign <- ObsAssign %>% filter(!ObsFile %in% MissingObsFiles)
            }
            
         }
      } 
   }
   
   # End of error catching for obs_to_sim_assignment. ObsAssign should now be a
   # data.frame with columns File and ObsFile or else a completely empty
   # data.frame.
   if(length(ObsAssign) > 0){
      ObsAssign <- ObsAssign %>% filter(complete.cases(File) &
                                           complete.cases(ObsFile) &
                                           File %in% sim_data_file) 
   }
   
   
   # conctime subfun --------------------------------------------------------
   
   get_ConcTime_subfun <- function(individual, 
                                   compoundToExtract = compoundToExtract, 
                                   tissue = tissue, 
                                   DDI = FALSE){
      
      compound_num <- as.numeric(Simcyp::CompoundID[
         AllCompounds$CompoundID_Simcyp[
            AllCompounds$CompoundID == compoundToExtract]])
      
      # Need to account for the fact that some tissues are actually tissue sub
      # types.
      if(tissue %in% AllTissues$Tissue){
         TissueID <- AllTissues %>% 
            mutate(ValToUse = Tissue)
      } else {
         TissueID <- AllTissues %>% 
            mutate(ValToUse = TissueSubType)
      }
      
      TissueID <- TissueID$Simcyp_ProfileName[which(TissueID$ValToUse == tissue)]
      TissueID <- as.numeric(Simcyp::ProfileID[TissueID])
      
      Time <- Simcyp::GetProfile_DB(
         profileID = Simcyp::ProfileID$nTimeSub, 
         compound = -1, # 
         inhibition = DDI, 
         conn = conn, 
         individual = individual)
      
      suppressWarnings(
         Conc <- Simcyp::GetProfile_DB(
            profileID = TissueID,
            compound = compound_num, 
            inhibition = DDI, 
            conn = conn, 
            individual = individual)
      )
      
      # Need to adjust if tissue was actually unbound. 
      CalcRequired <- AllTissues$Simcyp_calculation_required[
         AllTissues$Tissue == tissue]
      
      if(complete.cases(CalcRequired) && CalcRequired  == "Multiply by fu"){
         
         # Get fu for the individual
         fu_indiv <- Simcyp::GetCompoundResult_DB(individual = individual,
                                                  id = "idfuAdj",
                                                  compound = compound_num, 
                                                  conn = conn)
         
         Conc_unbound <- Conc * fu_indiv
      }
      
      if(length(Conc) == 0){
         return(list())
      }
      
      data.frame(
         Time = Time, 
         Conc = switch(as.character(str_detect(tissue, "unbound")), 
                       "TRUE" = Conc_unbound, 
                       "FALSE" = Conc)) %>% 
         mutate(CompoundID = compoundToExtract, 
                Compound = as.character(Deets[
                   AllCompounds$DetailNames[AllCompounds$CompoundID == compoundToExtract]]), 
                Tissue = tissue, 
                Individual = individual, 
                Trial = Simcyp::GetIndividualValue_DB(individual,
                                                      "idGroupNo", 
                                                      conn = conn), 
                Inhibitor = ifelse(DDI, 
                                   Deets$Inhibitor1, "none"))
      
   }
   
   # Try just one individual to see whether we can get any data for this tissue.
   CT1 <- get_ConcTime_subfun(individual = 1, 
                              compoundToExtract = compoundToExtract, 
                              tissue = tissue)
   
   if(length(CT1) == 0){
      warning(str_wrap(paste0("Concentrations for the ", 
                              compoundToExtract, 
                              " in ", 
                              tissue, 
                              "' are not available for the simulation '", 
                              sim_data_file, 
                              "'. No concentration-time data can be returned.")), 
              call. = FALSE)
      
      return(list())
   }
   
   # Get all the remaining conc-time profiles for all individuals
   CT_indiv <- 
      bind_rows(CT1) %>% 
      bind_rows(
         map(.x = 2:(as.numeric(Deets$NumTrials) * as.numeric(Deets$NumSubjTrial)), 
             .f = function(x){get_ConcTime_subfun(
                individual = x, 
                compoundToExtract = compoundToExtract, 
                tissue = tissue)}))
   
   if(is.null(Deets$Inhibitor1) == FALSE){
      
      CT_indiv_DDI <- map(.x = 1:(as.numeric(Deets$NumTrials) * as.numeric(Deets$NumSubjTrial)), 
                          .f = function(x){get_ConcTime_subfun(
                             individual = x, 
                             compoundToExtract = compoundToExtract, 
                             tissue = tissue, 
                             DDI = TRUE)}) %>% 
         bind_rows()  
      
      CT_indiv <- bind_rows(CT_indiv, CT_indiv_DDI)
   }
   
   CT_indiv <- CT_indiv %>% 
      mutate(Simulated = TRUE,
             Conc_units = "mg/L", # default. Can't change this.
             Time_units = "hours")
   
   
   # Calculating aggregate data -----------------------------------------------
   suppressMessages(
      CT_agg <- CT_indiv %>% 
         group_by(Time, CompoundID, Compound, Tissue, Inhibitor, Simulated,
                  Conc_units, Time_units) %>% 
         summarize(mean = mean(Conc, na.rm = T), 
                   geomean = gm_mean(Conc), 
                   median = median(Conc, na.rm = T), 
                   per5 = quantile(Conc, 0.95), 
                   per95 = quantile(Conc, 0.05)) %>% 
         ungroup() %>% 
         pivot_longer(cols = c(mean, geomean, median, per5, per95), 
                      names_to = "Trial", 
                      values_to = "Conc")
   )
   
   suppressMessages(
      CT_trial <- CT_indiv %>% 
         group_by(Time, CompoundID, Compound, Tissue, Inhibitor, Simulated,
                  Conc_units, Time_units, Trial) %>% 
         summarize(`trial mean` = mean(Conc, na.rm = T), 
                   `trial geomean` = gm_mean(Conc), 
                   `trial median` = median(Conc, na.rm = T), 
                   `trial per5` = quantile(Conc, 0.95), 
                   `trial per95` = quantile(Conc, 0.05)) %>% 
         ungroup() %>% 
         rename(TrialOrig = Trial) %>% 
         pivot_longer(cols = c(`trial mean`, `trial geomean`, `trial median`,
                               `trial per5`, `trial per95`), 
                      names_to = "Trial", 
                      values_to = "Conc") %>% 
         mutate(Individual = paste(Trial, TrialOrig)) %>% 
         select(-TrialOrig)
   )
   
   CT_agg <- bind_rows(CT_agg, CT_trial)
   
   RSQLite::dbDisconnect(conn)
   
   
   # Adding any obs data -----------------------------------------------------
   
   if(nrow(ObsAssign) > 0){
      ObsCT <- extractObsConcTime(obs_data_file = ObsAssign$ObsFile[ObsAssign$File == sim_data_file]) %>% 
         filter(CompoundID == compoundToExtract)
   }
   
   # Putting everything together ----------------------------------------
   
   Data <- list()
   
   if("individual" %in% returnAggregateOrIndiv){
      Data[["indiv"]] <- CT_indiv %>% 
         mutate(Trial = as.character(Trial), 
                Individual = as.character(Individual), 
                IndivOrAgg = "individual")
   }
   
   if("aggregate" %in% returnAggregateOrIndiv){
      Data[["agg"]] <- CT_agg %>% 
         mutate(Trial = as.character(Trial), 
                IndivOrAgg = "aggregate")
   }
   
   Data <- bind_rows(Data) %>%
      mutate(Species = "human", # FIXME - placeholder for now
             Individual = ifelse(is.na(Individual), Trial, Individual)) %>% 
      mutate(File = sim_data_file, 
             Tissue_subtype = NA)
   
   Data <- calc_dosenumber(ct_dataframe = Data, 
                           existing_exp_details = existing_exp_details)
   
   # Now that sim data are basically set up, can add obs data
   if(exists("ObsCT", inherits = FALSE) & 
      nrow(ObsCT %>% filter(CompoundID == compoundToExtract & 
                            Tissue == tissue)) > 0){
      
      Data <- match_obs_to_sim(ct_dataframe = Data, 
                               obs_dataframe = ObsCT, 
                               existing_exp_details = existing_exp_details)
   }
   
   # Adjusting concentration units as needed
   Data <- split(Data, f = list(Data$Compound, 
                                Data$CompoundID, 
                                Data$Conc_units))
   
   for(cmpd in names(Data)){
      if(nrow(Data[[cmpd]]) == 0){next}
      
      Data[[cmpd]] <- Data[[cmpd]] %>% 
         convert_conc_units(conc_units = conc_units, 
                            MW = Deets %>% select(matches("MW")))
   }
   
   Data <- bind_rows(Data)
   
   
   # Finalizing
   Data <- Data %>%
      mutate(Trial = factor(Trial, levels = c(
         c("obs", "obs+inhibitor", "mean", "median",
           "geomean", "per5", "per95", "per10", "per90"),
         setdiff(unique(Trial),
                 c("obs", "obs+inhibitor", "mean", "median",
                   "geomean", "per5", "per95", "per10", "per90")))),
         Tissue = recode(Tissue, 
                         "jejunum i" = "jejunum I",
                         "jejunum ii" = "jejunum II",
                         "ileum i" = "ileum I", 
                         "ileum ii" = "ileum II",
                         "ileum iii" = "ileum III",
                         "ileum iv" = "ileum IV")) %>%
      arrange(across(any_of(c("Compound", "Inhibitor", "Simulated",
                              "Individual", "Trial", "Time")))) %>%
      select(any_of(c("Compound", "CompoundID", "Inhibitor", 
                      "Species", "Tissue", "Individual", "Trial",
                      "Simulated", "IndivOrAgg", 
                      "Time", "Conc", "SD_SE",
                      "Time_units", "Conc_units", "Tissue_subtype", "DoseNum",
                      "DoseInt", "Dose_sub", "Dose_inhib", "Dose_inhib2",
                      "File", "DBFile", "ObsFile")))
   
   return(Data)
   
}

