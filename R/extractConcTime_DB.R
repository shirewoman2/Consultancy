#' Extract concentration-time data from a Simcyp Simulator database file --
#' UNDER CONSTRUCTION!!!
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   concentration-time data, in quotes; must be an output file from the Simcyp
#'   simulator
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
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
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
                               returnAggregateOrIndiv = "aggregate"){
   
   # Error catching ----------------------------------------------------------
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
   
   # If they didn't include ".db" at the end, add that.
   sim_data_file <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$|\\.db$", "", sim_data_file), ".db")
   
   # Main body of function ----------------------------------------------------
   suppressMessages(Simcyp::SetWorkspace(sub("\\.db", ".wksz", sim_data_file), 
                                         verbose = FALSE))
   
   conn <- RSQLite::dbConnect(RSQLite::SQLite(), sim_data_file) 
   
   Deets <- harmonize_details(existing_exp_details) %>% 
      filter_sims(which_sims = sim_data_file, 
                  include_or_omit = "include")
   Deets <- Deets$MainDetails
   
   get_ConcTime_subfun <- function(individual, 
                                   compoundToExtract = compoundToExtract, 
                                   tissue = tissue, 
                                   DDI = FALSE){
      
      data.frame(
         Time = Simcyp::GetProfile_DB(
            profileID = Simcyp::ProfileID$nTimeSub, 
            compound = -1, # 
            inhibition = DDI, 
            conn = conn, 
            individual = individual), 
         
         Conc = Simcyp::GetProfile_DB(
            profileID = as.numeric(Simcyp::ProfileID[
               AllTissues$Simcyp_ProfileName[
                  AllTissues$Tissue == tissue]]),
            compound = as.numeric(Simcyp::CompoundID[
               AllCompounds$CompoundID_Simcyp[
                  AllCompounds$CompoundID == compoundToExtract]]), 
            inhibition = DDI, 
            conn = conn, 
            individual = individual)) %>% 
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
   
   # Get all the conc-time profiles for all individuals
   CT_indiv <- map(.x = 1:(Deets$NumTrials * Deets$NumSubjTrial), 
                   .f = function(x){get_ConcTime_subfun(
                      individual = x, 
                      compoundToExtract = compoundToExtract, 
                      tissue = tissue)}) %>% 
      bind_rows()
   
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
             Conc_units = "mg/L", # default. Not sure how to change this.
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
      mutate(Simulated = TRUE,
             Species = "human", # FIXME - placeholder for now
             Individual = ifelse(is.na(Individual), Trial, Individual)) %>% 
      mutate(File = sim_data_file, 
             subsection_ADAM = NA)
   
   Data <- calc_dosenumber(ct_dataframe = Data, 
                           existing_exp_details = existing_exp_details)
   
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

