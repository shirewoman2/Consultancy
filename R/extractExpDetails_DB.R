#' Extract simulation experimental details from a database file - UNDER
#' CONSTRUCTION!!!!!
#'
#' \code{extractExpDetails_DB} reads a Simcyp Simulator database file for
#' information about how the simulation was set up. This uses the Simcyp package
#' under the hood.
#'
#' @param sim_data_file the database file to get information from. Note that a
#'   matching workspace MUST also be present, as in, the exact same file name
#'   but ending in ".wksz" instead of ".db".
#'
#' @return a list of information, same as other extractExpDetails_x functions
#' @export
#'
#' @examples
#' # none yet
#' 
extractExpDetails_DB <- function(sim_data_file){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # FIXME - Will need to figure out how to check whether engine has been
   # initialized. If it has not, then initialize or give a stop message.
   
   # # Intialise the system files path. Not sure whether the version number will
   # # matter for this.
   # suppressMessages(Simcyp::Initialise(
   #    species = as.numeric(Simcyp::SpeciesID[str_to_title(species)]),
   #    verbose = FALSE))
   
   # If they didn't include ".db" at the end, add that.
   sim_data_file <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$|\\.db", "", sim_data_file), ".db")
   
   
   # Main body of function ---------------------------------------------------
   
   suppressMessages(
      Simcyp::SetWorkspace(sub("\\.db", ".wksz", sim_data_file), 
                           verbose = FALSE))
   
   conn <- RSQLite::dbConnect(RSQLite::SQLite(), sim_data_file) 
   
   # Figure out which compound positions were active. 
   ActiveCompounds <- 
      sapply(paste0("idInhEnabled", AllCompounds$CompoundID_num_Simcyp), 
             \(x) Simcyp::GetParameter(Tag = x, 
                                       Category = Simcyp::CategoryID$SimulationData, 
                                       SubCategory = 0))
   names(ActiveCompounds) <- AllCompounds$DetailNames
   ActiveCompounds <- names(ActiveCompounds)[ActiveCompounds]
   
   # # Comment this out for actual function, but here is how to see all possible
   # # parameters for compound, population, and simulation data.
   # AllSimulationParameters <- 
   #    data.frame(Name = names(Simcyp::SimulationParameterID), 
   #               Tag = as.character(unlist(Simcyp::SimulationParameterID)))
   # 
   # AllCompoundParameters <- 
   #    data.frame(Name = names(Simcyp::CompoundParameterID), 
   #               Tag = as.character(unlist(Simcyp::CompoundParameterID)))
   #
   # Below is not correct. Not sure how to figure out subcategory.
   # AllSubCategorySimulationParameters <- 
   #    data.frame(Name = names(Simcyp::SubCategoryID), 
   #               Tag = as.character(unlist(Simcyp::SubCategoryID)))
   # 
   # AllPopulationIDs <- 
   #    data.frame(Name = names(Simcyp::PopulationID), 
   #               Tag = as.character(unlist(Simcyp::PopulationID)))
   
   ParameterConversion <- AllExpDetails %>% 
      filter(Sheet == "workspace XML file" &
                complete.cases(SimcypParameterType)) %>% 
      select(Detail, CompoundID, matches("Level"), XMLswitch, SimcypParameterType,
             ColsChangeWithCmpd) %>% 
      mutate(Detail_nosuffix = case_when(
         ColsChangeWithCmpd == TRUE ~ sub("_sub|_inhib2|_inhib$|_met1|_met2|_secmet|_inhib1met", 
                                          "", Detail), 
         TRUE ~ Detail)) %>% 
      left_join(AllCompounds %>% select(CompoundID, Suffix, CompoundID_Simcyp), 
                by = "CompoundID") %>% 
      mutate(CompoundID_Simcyp = factor(CompoundID_Simcyp, 
                                        levels = c("Substrate", "SubPriMet1", "SubPriMet2", 
                                                   "SubSecMet", "Inhibitor1", "Inhibitor1Met", 
                                                   "Inhibitor2"))) %>% 
      arrange(CompoundID_Simcyp)
   
   Details <- list()
   
   # Compound parameters --------------------------------------------------------
   
   CmpdParam <- unique(
      ParameterConversion$Detail_nosuffix[
         ParameterConversion$SimcypParameterType == "compound parameter"])
   CmpdParam <- setdiff(CmpdParam, AllCompounds$DetailNames)
   
   for(k in CmpdParam){
      
      ParamConv_subset <- ParameterConversion %>% 
         filter(Detail_nosuffix == k)
      
      Details_toadd <- 
         map(.x = Simcyp::CompoundID[ParamConv_subset$CompoundID_Simcyp], 
             .f = function(x){Simcyp::GetCompoundParameter(
                Tag = unique(ParamConv_subset$Level3), 
                Compound = x)})
      
      # Decoding as necessary. Add to the options for k as needed.
      Details_toadd <- lapply(Details_toadd, as.character)
      Details_toadd <- lapply(Details_toadd, 
                              function(DeetValue) case_when(
                                 
                                 str_detect(k, "Abs_model") ~  
                                    case_match(DeetValue, 
                                               "0" ~ "1st order", 
                                               "2" ~ "ADAM"), 
                                 
                                 str_detect(k, "DistributionModel") ~ 
                                    case_match(DeetValue, 
                                               "1" ~ "Full PBPK Model", 
                                               "0" ~ "Minimal PBPK Model"), 
                                 
                                 str_detect(k, "DoseRoute") ~ 
                                    case_match(DeetValue, 
                                               "1" ~ "Oral", 
                                               "2" ~ "IV"), 
                                 
                                 str_detect(k, "Peff_MechPeff_totalvsfree") ~
                                    case_match(DeetValue, 
                                               "0" ~ "total", 
                                               "1" ~ "free"), 
                                 
                                 str_detect(k, "Peff_pred_or_user") ~
                                    case_match(DeetValue, 
                                               "0" ~ "user", 
                                               "1" ~ "predicted"), 
                                 
                                 str_detect(k, "Peff_prediction_method") ~
                                    case_match(DeetValue, 
                                               "3" ~ "Papp using PSA",
                                               "5" ~ "Papp", # need to add info here 
                                               "6" ~ "MechPeff"), 
                                 
                                 str_detect(k, "Permeability_reference_lock") ~
                                    case_match(DeetValue, 
                                               "true" ~ "unlocked", 
                                               "false" ~ "locked"), 
                                 
                                 TRUE ~ DeetValue))
      
      # FIXME - Need to deal with switches still. 
      
      names(Details_toadd) <- ParamConv_subset$Detail
      Details <- c(Details, Details_toadd)
      rm(Details_toadd, ParamConv_subset)
      
   }
   
   # Removing info for compounds that are not active
   Details <- Details[
      which(str_detect(names(Details), 
                       str_c(AllCompounds$Suffix[AllCompounds$DetailNames %in% ActiveCompounds], 
                             collapse = "|")))]
   
   
   # General parameters --------------------------------------------------------
   
   GenParam <- unique(
      ParameterConversion$Detail_nosuffix[
         ParameterConversion$SimcypParameterType == "general parameter"])
   
   # Have to deal w/NumDoses, StartHr specially b/c they're not set up not as a
   # compound parameter but as a general parameter.
   Remove <- setdiff(
      # All possible NumDoses_x
      paste0(rep(c("NumDoses", "StartHr"), each = 3), 
             c("_sub", "_inhib", "_inhib2")), 
      # Only the NumDoses_x that are active               
      paste0(rep(c("NumDoses", "StartHr"), each = length(ActiveCompounds)), 
             AllCompounds$DosedCompoundSuffix[
                AllCompounds$DetailNames %in% ActiveCompounds]))
   
   GenParam <- setdiff(GenParam, Remove)
   rm(Remove)
   
   for(k in GenParam){
      
      ParamConv_subset <- ParameterConversion %>% 
         filter(Detail_nosuffix == k)
      
      Details_toadd <- Simcyp::GetParameter(
         Tag = ParamConv_subset$Level2, 
         Category = Simcyp::CategoryID$SimulationData, 
         SubCategory = ifelse(is.na(ParamConv_subset$Level3),
                              0, ParamConv_subset$Level3))
      
      names(Details_toadd) <- k
      Details <- c(Details, Details_toadd)
      
      if(str_detect(k, "NumDoses")){
         MoreDetails_toadd <- 
            ifelse(Details_toadd > 1, "Multiple Dose", "Single Dose")
         names(MoreDetails_toadd) <- 
            sub("NumDoses", "Regimen", k)
         Details <- c(Details, MoreDetails_toadd)
         rm(MoreDetails_toadd)
      }
      
      rm(Details_toadd, ParamConv_subset)
      
   }
   
   # Getting compound names separately.
   CmpdNames <- 
      map(.x = AllCompounds$CompoundID_num_Simcyp, 
          .f = function(x){Simcyp::GetCompoundParameter(
             Tag = "idName", 
             Compound = x)})
   names(CmpdNames) <- AllCompounds$DetailNames
   
   Details <- c(Details, 
                CmpdNames[intersect(AllCompounds$DetailNames, ActiveCompounds)])
   
   # t(as.data.frame(Details))
   
   RSQLite::dbDisconnect(conn)
   
   Details <- as.data.frame(Details) %>% 
      mutate(File = sim_data_file)
   
   Details <- harmonize_details(list(MainDetails = Details))
   
   return(Details)
   
}

