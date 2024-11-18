#' Extract PK from a Simcyp Simulator database file
#'
#' @description \code{extractPK_DB} pulls PK data from a database file and
#'   calculates summary statistics for the data
#'
#' @param sim_data_file name of the database file containing the simulator
#'   output, in quotes
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file. Options are: \describe{
#'
#'   \item{"all"}{all possible parameters}
#'
#'   \item{"AUC tab"}{only those parameters that would be found on an "AUC"
#'   tab of the Excel output (default)}
#'
#'   \item{"Absorption tab"}{only those parameters that would be found on the
#'   "Absorption" or "Overall Fa Fg" tab}
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
#' @param which_doses optionally specify which doses you would like. If left as
#'   NA (default), only the first- and last-dose PK will be included. An example
#'   of good input: \code{which_doses = 1} or \code{which_doses = c(1:3, 7)} or
#'   \code{which_doses = "first"} or \code{which_doses = "last"}
#' @param returnAggregateOrIndiv return aggregate (default) and/or individual PK
#'   parameters? Options are "aggregate", "individual", or "both". For aggregate
#'   data, values are pulled from simulator output -- not calculated -- and the
#'   output will be a data.frame with the PK parameters in columns and the
#'   statistics reported exactly as in the simulator output file.
#'   
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
                         which_doses = NA, 
                         tissue = "plasma", 
                         existing_exp_details, 
                         returnAggregateOrIndiv = "aggregate"){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if(length(sim_data_file) > 1){
      warning(wrapn("The extractPK_DB function is for getting information from one database simlation file at a time, and you have provided more than that. Please check your input and try again."), 
              call. = FALSE)
      return(list())
   }
   
   PossTissues <- AllTissues %>% filter(complete.cases(Simcyp_ProfileName)) %>% 
      pull(Tissue) %>% sort() %>% unique()
   if(tissue %in% PossTissues == FALSE){
      warning(wrapn(paste0("The Simcyp package will only pull PK data for ", 
                           str_comma(PossTissues, conjunction = "or"), 
                           "; no other tissues are available.")), 
              call. = FALSE)
      tissue <- "plasma"
   }
   
   # Harmonizing PK parameter names
   PKparameters <- harmonize_PK_names(PKparameters)
   
   # If they didn't include ".db" at the end, add that.
   sim_data_file <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$|\\.db", "", sim_data_file), ".db")
   
   returnAggregateOrIndiv <- tolower(returnAggregateOrIndiv)
   if(any(c("individual", "aggregate", "both") %in% returnAggregateOrIndiv) == FALSE){
      warning(wrapn("The only possibly values for the argument 'returnAggregateOrIndiv' are 'aggregate', 'individual', or 'both', and you have supplied something else. We'll return both."), 
              call. = FALSE)
      returnAggregateOrIndiv <- "both"
   }
   
   
   # Figuring out which data to pull --------------------------------------------------
   
   # Checking experimental details to only pull details that apply
   if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA
      existing_exp_details <- extractExpDetails_DB(sim_data_file = sim_data_file)
      Deets <- existing_exp_details$MainDetails
      Dosing <- existing_exp_details$Dosing
   } else {
      existing_exp_details <- harmonize_details(existing_exp_details)
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          which_sims = sim_data_file, 
                                          include_or_omit = "include")
      
      Deets <- existing_exp_details$MainDetails
      Dosing <- existing_exp_details$Dosing
      
      if(nrow(Deets) == 0){
         existing_exp_details <- extractExpDetails_DB(sim_data_file = sim_data_file)
         Deets <- existing_exp_details$MainDetails
         Dosing <- existing_exp_details$Dosing
      }
   }
   
   # Keeping track of originally requested parameters
   PKparameters_orig <- PKparameters
   if(all(is.na(PKparameters))){
      PKparameters <- "auc tab"
   }
   
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
   PKparameters <- intersect(
      PKparameters, 
      c("tmax_dose1", "tmax_last", "tmax", 
        "Cmax_dose1", "Cmax_last", "Cmax", 
        "Cmin_last", "Cmin", 
        "AUCt_dose1", "AUCtau_last", "AUCt", 
        "AUCinf_dose1", "HalfLife_dose1", 
        "tmax_dose1_withInhib", "tmax_last_withInhib", "tmax_withInhib", 
        "Cmax_dose1_withInhib", "Cmax_last_withInhib", "Cmax_withInhib", 
        "Cmin_last_withInhib", "Cmin_withInhib", "AUCt_dose1_withInhib", 
        "AUCtau_last_withInhib", "AUCt_withInhib", "AUCinf_dose1_withInhib", 
        "HalfLife_dose1_withInhib",
        "tmax_ratio_dose1", "tmax_ratio_last", "tmax", "Cmax_ratio_dose1", 
        "Cmax_ratio_last", "Cmax", "Cmin_ratio_last", "Cmin", "AUCt_ratio_dose1", 
        "AUCtau_ratio_last", "AUCt", "AUCinf_ratio_dose1", "HalfLife_ratio_dose1"))
   
   # Main body of function ----------------------------------------------------
   suppressMessages(Simcyp::SetWorkspace(sub("\\.db", ".wksz", sim_data_file), 
                                         verbose = FALSE))
   
   conn <- RSQLite::dbConnect(RSQLite::SQLite(), sim_data_file) 
   
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
         conn = conn) %>% 
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
   
   MaxDoseNum <- max(PK_indiv$DoseNum)
   
   PK_indiv <- PK_indiv %>% 
      mutate(Inhibition = case_match(Inhibition, 
                                     0 ~ "noinhib", 
                                     1 ~ "yesinhib")) %>% 
      select(Trial, Individual, Inhibition, DoseNum, 
             Tmax, Cmin, Cmax, AUC, AUCinf, HalfLife, AccumulationIndex) %>% 
      filter(DoseNum != -1) %>%
      pivot_longer(cols = -c(Trial, Individual, DoseNum, Inhibition), 
                   names_to = "PKparameter", 
                   values_to = "Value") %>% 
      # Adding fup 
      mutate(fu = existing_exp_details$MainDetails %>% 
                pull(paste0("fu", AllCompounds$Suffix[
                   AllCompounds$CompoundID == compoundToExtract])), 
             CalcRequired = AllTissues %>% filter(Tissue == tissue) %>% 
                pull(Simcyp_calculation_required), 
             Value = case_match(CalcRequired, 
                                "Multiply by fu" ~ Value * as.numeric(fu), 
                                .default = Value),  
             PKparameter = case_match(PKparameter, 
                                      "Tmax" ~ "tmax", 
                                      "AUC" ~ "AUCt", 
                                      .default = PKparameter), 
             PKparameter = paste0(PKparameter, 
                                  case_match(DoseNum, 
                                             1 ~ "_dose1",
                                             MaxDoseNum ~ "_last", 
                                             .default = "")), 
             CompoundID = compoundToExtract,
             Compound = as.character(Deets[
                AllCompounds$DetailNames[AllCompounds$CompoundID == compoundToExtract]]),
             Tissue = tissue,
             Simulated = TRUE,
             File = sim_data_file,
             Dose = as.numeric(
                Deets %>%
                   pull(any_of(paste0("Dose",
                                      AllCompounds$DosedCompoundSuffix[
                                         AllCompounds$CompoundID == compoundToExtract])))))
   
   # Calculating ratios
   if(any(PK_indiv$Inhibition == "yesinhib") & 
      AllCompounds$DDIrole[AllCompounds$CompoundID == compoundToExtract] == "victim"){
      
      Ratios <- PK_indiv %>% 
         pivot_wider(names_from = Inhibition, 
                     values_from = Value) %>% 
         mutate(Value = yesinhib / noinhib, 
                PKparameter = case_when(
                   str_detect(PKparameter, "_") ~ sub("_", "_ratio_", PKparameter), 
                   .default = paste0(PKparameter, "_ratio")), 
                Inhibition = "yesinhib") %>% 
         select(-yesinhib, -noinhib)
      
      # Putting baseline, DDI, and ratio PK together
      PK_indiv <- PK_indiv %>% 
         mutate(PKparameter = case_when(Inhibition == "noinhib" ~ PKparameter,
                                        .default = paste0(PKparameter, "_withInhib"))) %>% 
         bind_rows(Ratios)
   } 
   
   MyPerp <- determine_myperpetrator(Deets, prettify_compound_names = F)
   
   PK_indiv <- PK_indiv %>% 
      mutate(Inhibitor = 
                case_when(Inhibition == "noinhib" ~ "none",
                          .default = MyPerp),
             # Units are always these for db files
             Units = case_when(str_detect(PKparameter, "AUC") ~ Deets$Units_AUC, 
                               str_detect(PKparameter, "CL") ~ Deets$Units_CL, 
                               str_detect(PKparameter, "Cmax") ~ Deets$Units_Cmax, 
                               str_detect(PKparameter, "tmax") ~ Deets$Units_tmax))
   
   
   ## Calculating aggregate stats --------------------------------------------
   
   suppressWarnings(suppressMessages(
      PK_agg <- PK_indiv %>% 
         group_by(Compound, CompoundID, Inhibitor, Tissue, Simulated, File, 
                  Dose, DoseNum, PKparameter, Units) %>% 
         summarize(
            # NB: These statistic names match those in renameStats function.
            N = n(), 
            Mean = mean(Value, na.rm = T),
            SD = sd(Value, na.rm = T), 
            Geomean = gm_mean(Value, na.rm = F), # need this to be NA when there were problems, e.g., with extrapolating to inf
            GCV = gm_CV(Value, na.rm = F), 
            CI90_lower = gm_conf(Value, CI = 0.90)[1], 
            CI90_upper = gm_conf(Value, CI = 0.90)[2],
            Median = median(Value, na.rm = T), 
            Minimum = min(Value, na.rm = T), 
            Maximum = max(Value, na.rm = T)) %>% ungroup()
   )) 
   
   # Only return the PK they asked for and that make sense ---------------------
   
   if(all(is.na(which_doses))){
      which_doses <- c(1, MaxDoseNum)
   } else if(class(which_doses) == "character"){
      which_doses <- switch(tolower(which_doses), 
                            "first" = 1, 
                            "1st" = 1, 
                            "last" = MaxDoseNum)
   }
   
   PK_agg <- PK_agg %>% 
      mutate(RemoveThisRow = 
                case_when(
                   str_detect(PKparameter, 
                              "AccumulationIndex_(ratio_)?dose1|Cmin_(ratio_)?dose1") ~ TRUE, 
                   str_detect(PKparameter, "AUCinf") & DoseNum != 1 ~ TRUE, 
                   DoseNum %in% which_doses == FALSE ~ TRUE, 
                   .default = FALSE)) %>% 
      filter(RemoveThisRow == FALSE) %>% 
      select(File, CompoundID, Compound, Inhibitor, Tissue, Simulated,
             Dose, DoseNum, PKparameter, 
             N, Geomean, GCV, CI90_lower, CI90_upper, 
             Mean, SD, Median, Minimum, Maximum) 
   
   PK_indiv <- PK_indiv %>% 
      mutate(RemoveThisRow = 
                case_when(
                   str_detect(PKparameter, 
                              "AccumulationIndex_(ratio_)?dose1|Cmin_(ratio_)?dose1") ~ TRUE, 
                   str_detect(PKparameter, "AUCinf") & DoseNum != 1 ~ TRUE, 
                   DoseNum %in% which_doses == FALSE ~ TRUE, 
                   .default = FALSE)) %>% 
      filter(RemoveThisRow == FALSE) %>% 
      select(File, CompoundID, Compound, Inhibitor, Tissue, Simulated,
             Dose, DoseNum, Trial, Individual, PKparameter, Value)
   
   Intervals <- Intervals %>% 
      filter(DoseNum %in% which_doses) %>% 
      mutate(DoseNum_text = case_when(DoseNum == 1 ~ "first dose",
                                      DoseNum == MaxDoseNum ~ "last dose"), 
             Interval = paste("from", StartTime, "h to", EndTime, "h")) %>% 
      select(DoseNum, DoseNum_text, Interval)
   
   
   # Returning -------------------------------------------------------------
   
   RSQLite::dbDisconnect(conn)
   
   Out <- list()
   
   if(any(c("aggregate", "both") %in% returnAggregateOrIndiv)){
      Out[["aggregate"]] <- PK_agg
   }
   
   if(any(c("individual", "both") %in% returnAggregateOrIndiv)){
      Out[["individual"]] <- PK_indiv
   }
   
   Out[["TimeInterval"]] <- Intervals
   
   return(Out)
   
}


