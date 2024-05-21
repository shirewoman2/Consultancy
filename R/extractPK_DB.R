#' Extract PK from a Simcyp Simulator database file -- UNDER CONSTRUCTION!!!!
#'
#' \code{extractPK_DB} pulls PK data from a database file and calculates summary
#' statistics for the data
#'
#' @param sim_data_file name of the Excel file containing the simulator output,
#'   in quotes
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
#' @param returnAggregateOrIndiv return aggregate (default) and/or individual PK
#'   parameters? Options are "aggregate", "individual", or "both". For aggregate
#'   data, values are pulled from simulator output -- not calculated -- and the
#'   output will be a data.frame with the PK parameters in columns and the
#'   statistics reported exactly as in the simulator output file.
#'
#' @return a list: "individual" = individual PK data, "aggregate" = aggregate PK
#'   data, "TimeInterval" = the time intervals used
#' @export
#'
#' @examples
#' # none yet
extractPK_DB <- function(sim_data_file, 
                         compoundToExtract = "substrate", 
                         PKparameters = NA, 
                         tissue = "plasma", 
                         existing_exp_details, 
                         returnAggregateOrIndiv = "both"){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Keeping tissue as an argument as a placeholder only for now. I wonder why
   # we can't get all the PK we can get in the Excel outputs.
   if(tissue != "plasma"){
      warning("The Simcyp package will only pull plasma PK data; no other tissues are available.\n", 
              call. = FALSE)
      tissue <- "plasma"
   }
   
   # tissue <- tolower(tissue)
   # if(tissue %in% c("plasma", "unbound plasma", "blood", "unbound blood", 
   #                  "peripheral plasma", "peripheral blood") == FALSE){
   #    warning("You have not supplied a permissible value for tissue. Options are `plasma`, `unbound plasma`, `blood`, `unbound blood`, `peripheral plasma`, or `peripheral blood`. The PK parameters will be for plasma.", 
   #            call. = FALSE)
   #    tissue <- "plasma"
   # }
   
   # Harmonizing PK parameter names
   PKparameters <- harmonize_PK_names(PKparameters)
   
   # If they didn't include ".db" at the end, add that.
   sim_data_file <- ifelse(str_detect(sim_data_file, "db$"), 
                           sim_data_file, paste0(sim_data_file, ".db"))
   
   if(length(returnAggregateOrIndiv) > 2 | length(returnAggregateOrIndiv) < 1 |
      all(returnAggregateOrIndiv %in% c("aggregate", "both", "individual")) == FALSE){
      stop("Options for 'returnAggregateOrIndiv' are 'aggregate', 'individual', or 'both'.",
           call. = FALSE)
   }
   
   if(returnAggregateOrIndiv[1] == "both"){
      returnAggregateOrIndiv <- c("aggregate", "individual")
   }
   
   # Checking experimental details to only pull details that apply
   if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA
      Deets <- extractExpDetails_DB(sim_data_file = sim_data_file)[["MainDetails"]]
   } else {
      Deets <- harmonize_details(existing_exp_details)[["MainDetails"]] %>% 
         filter(File == sim_data_file)
      
      if(nrow(Deets) == 0){
         Deets <- extractExpDetails_DB(sim_data_file = sim_data_file)[["MainDetails"]]
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
   
   # Checking whether the user had supplied a vector of specific parameters
   # rather than a parameter set name and using those if so.
   if(is.null(PKparameters) & any(complete.cases(PKparameters_orig))){
      PKparameters <- PKparameters_orig
   }
   
   # Only keeping PK parameters that are among the posibilities for DB files. 
   PKparameters <- intersect(PKparameters, 
                             c("tmax_dose1", "tmax_last", "tmax", 
                               "Cmax_dose1", "Cmax_last", "Cmax", 
                               # "AccumulationIndex", 
                               "AUCt_dose1", "AUCtau_last", "AUCt", 
                               "AUCinf_dose1", "HalfLife_dose1"))
   
   # Main body of function ----------------------------------------------------
   conn <- RSQLite::dbConnect(SQLite(), sim_data_file) 
   
   Deets <- harmonize_details(existing_exp_details) %>% 
      filter_sims(which_sims = sub("\\.db", ".xlsx", sim_data_file), 
                  include_or_omit = "include")
   Deets <- Deets$MainDetails
   
   # # Comment this out for actual function, but here is how to see all possible
   # # profiles. 
   # AllProfiles <-
   #    data.frame(Name = names(Simcyp::ProfileID),
   #               Tag = as.character(unlist(Simcyp::ProfileID)))
   
   getPK_subfun <- function(individual, 
                            compoundToExtract = compoundToExtract, 
                            tissue = tissue){
      
      Simcyp::GetAUCFrom_DB(
         profile_id = as.numeric(Simcyp::ProfileID[
            AllTissues$Simcyp_ProfileName[
               AllTissues$Tissue == tissue]]),
         compound = as.numeric(Simcyp::CompoundID[
            AllCompounds$CompoundID_Simcyp[
               AllCompounds$CompoundID == compoundToExtract]]),
         individual = individual,
         allDoses = TRUE, 
         conn) %>% 
         mutate(Individual = individual, 
                Trial = Simcyp::GetIndividualValue_DB(individual,
                                                      "idGroupNo", 
                                                      conn = conn))
      
   }
   
   # Get all the PK_indiv values for all doses for all individuals
   PK_indiv <- map(.x = 1:(Deets$NumTrials * Deets$NumSubjTrial), 
                   .f = function(x){getPK_subfun(individual = x, 
                                                 compoundToExtract = compoundToExtract, 
                                                 tissue = tissue)}) %>% 
      bind_rows() %>% 
      rename(DoseNum = Dose)
   
   Intervals <- PK_indiv %>% select(DoseNum, StartTime, EndTime) %>% unique()
   
   PK_indiv <- PK_indiv %>% 
      mutate(Inhibitor = ifelse(Inhibition == 0, 
                                "none", Deets$Inhibitor1)) %>% 
      select(Trial, Individual, Inhibitor, DoseNum, Tmax, Cmin, Cmax, AUC, AUCinf, 
             HalfLife, AccumulationIndex) %>% 
      filter(DoseNum %in% c(1, max(Intervals$DoseNum))) %>% 
      pivot_longer(cols = -c(Trial, Individual, DoseNum, Inhibitor), 
                   names_to = "PKparameter", 
                   values_to = "Value") %>% 
      mutate(PKparameter = case_match(PKparameter, 
                                      "Tmax" ~ "tmax", 
                                      "AUC" ~ "AUCt", 
                                      .default = PKparameter), 
             PKparameter = paste0(PKparameter, ifelse(DoseNum == 1, 
                                                      "_dose1", "_last")), 
             PKparameter = ifelse(Inhibitor == "none", 
                                  PKparameter, paste0(PKparameter, "_withInhib")), 
             CompoundID = compoundToExtract, 
             Compound = as.character(Deets[
                AllCompounds$DetailNames[AllCompounds$CompoundID == compoundToExtract]]), 
             Tissue = tissue, 
             Simulated = TRUE, 
             File = sub("\\.db", ".xlsx", sim_data_file), 
             DBFile = sim_data_file, 
             Dose = as.numeric(
                Deets %>% 
                   pull(any_of(paste0("Dose", 
                                      AllCompounds$DosedCompoundSuffix[
                                         AllCompounds$CompoundID == compoundToExtract])))))
   
   ## Calculating aggregate stats --------------------------------------------
   
   if(returnAggregateOrIndiv %in% c("aggregate", "both")){
      
      suppressWarnings(suppressMessages(
         PK_agg <- PK_indiv %>% 
            group_by(Compound, CompoundID, Inhibitor, Tissue, 
                     Simulated, File, Dose, DoseNum, PKparameter) %>% 
            summarize(
               N = n(), 
               Mean = mean(Value, na.rm = T),
               SD = sd(Value, na.rm = T), 
               Geomean = gm_mean(Value, na.rm = F), # need this to fail when there were problems, e.g., problems extrapolating to inf
               GeoCV = gm_CV(Value, na.rm = F), 
               CI90_lower = gm_conf(Value, CI = 0.90)[1], 
               CI90_upper = gm_conf(Value, CI = 0.90)[2],
               Median = median(Value, na.rm = T), 
               Minimum = min(Value, na.rm = T), 
               Maximum = max(Value, na.rm = T)) 
      ))
      
      PK_agg <- PK_agg %>% ungroup() %>% 
         select(-Inhibitor, -DoseNum, -Dose) %>% 
         pivot_longer(cols = -c(Compound, CompoundID, Tissue, Simulated, 
                                File, Simulated, PKparameter), 
                      names_to = "Statistic", 
                      values_to = "Value") %>% 
         pivot_wider(names_from = PKparameter, 
                     values_from = Value) 
      
   }
   
   Intervals <- Intervals %>% filter(DoseNum %in% c(1, max(DoseNum))) %>% 
      mutate(DoseNum_text = case_when(
         DoseNum == 1 ~ "first dose",
         DoseNum == Deets[, paste0("NumDoses", 
                                   AllCompounds$DosedCompoundSuffix[
                                      AllCompounds$CompoundID == compoundToExtract])] ~ 
            "last dose"), 
         Interval = paste("from", StartTime, "h to", EndTime, "h")) %>% 
      select(DoseNum, DoseNum_text, Interval)
   
   RSQLite::dbDisconnect(conn)
   
   return(list("individual" = PK_indiv, 
               "aggregate" = PK_agg, 
               "TimeInterval" = Intervals))
   
   
}


