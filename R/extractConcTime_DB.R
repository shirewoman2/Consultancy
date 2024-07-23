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
                               compoundToExtract = "substrate", 
                               tissue = "plasma", 
                               existing_exp_details = NA,
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
   suppressMessages(Simcyp::SetWorkspace(sim_data_file, verbose = FALSE))
   
   conn <- RSQLite::dbConnect(RSQLite::SQLite(), sim_data_file) 
   
   ## Getting exp details as needed --------------------------------------------
   
   if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA and we have not yet extracted any details
      existing_exp_details <- 
         extractExpDetails_mult(sim_data_files = sim_data_file, 
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
      
      warning(str_wrap(paste0("The file '",
                              sim_data_file, 
                              "' was requested but does not appear to be a Simcyp Simulator file.\n")), 
              call. = FALSE)
      
      return()
      
   }
   
   Deets <- harmonize_details(existing_exp_details) %>% 
      filter_sims(which_sims = sim_data_file, 
                  include_or_omit = "include")
   Deets <- Deets$MainDetails
   
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
      if(AllTissues$Simcyp_calculation_required[AllTissues$Tissue == tissue] == 
         "Multiply by fu"){
         
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
         map(.x = 2:(Deets$NumTrials * Deets$NumSubjTrial), 
             .f = function(x){get_ConcTime_subfun(
                individual = x, 
                compoundToExtract = compoundToExtract, 
                tissue = tissue)}))
   
   if(is.null(Deets$Inhibitor1) == FALSE){
      
      CT_indiv_DDI <- map(.x = 1:(Deets$NumTrials * Deets$NumSubjTrial), 
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
   
   # Putting everything together ----------------------------------------
   
   Data <- list()
   
   if("individual" %in% returnAggregateOrIndiv){
      Data[["indiv"]] <- CT_indiv %>% 
         mutate(Trial = as.character(Trial), 
                Individual = as.character(Individual))
   }
   
   if("aggregate" %in% returnAggregateOrIndiv){
      Data[["agg"]] <- CT_agg %>% 
         mutate(Trial = as.character(Trial))
   }
   
   # FIXME - Have not yet dealt with observed data 
   
   # if(exists("obs_data", inherits = FALSE)){
   #    Data[["obs"]] <- obs_data %>%
   #       mutate(Species = tolower(sub("Sim-", "", Species)))
   # }
   
   Data <- bind_rows(Data) %>%
      mutate(Species = "human", # FIXME - placeholder for now
             Individual = ifelse(is.na(Individual), Trial, Individual)) %>% 
      mutate(File = sim_data_file, 
             subsection_ADAM = NA)
   
   Data <- calc_dosenumber(ct_dataframe = Data, 
                           existing_exp_details = existing_exp_details)
   
   # Adjusting concentration units as needed
   Data <- Data %>% convert_conc_units(conc_units = conc_units, 
                                       MW = existing_exp_details$MainDetails %>% 
                                          select(matches("MW")))
   
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
                      "Simulated", "Time", "Conc", "SD_SE",
                      "Time_units", "Conc_units", "subsection_ADAM", "DoseNum",
                      "DoseInt", "Dose_sub", "Dose_inhib", "Dose_inhib2",
                      "File", "DBFile", "ObsFile")))
   
   return(Data)
   
}

