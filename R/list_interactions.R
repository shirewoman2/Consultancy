#' Find all the possible drug-drug interactions between compounds included in a
#' single simulation
#'
#' @description \code{list_interactions} looks through the parameters for a
#'   Simcyp simulation for any pathways involved in the transport or elimination
#'   of the substrate, primary metabolite 1, primary metabolite 2, or secondary
#'   metabolite that are affected by any interactions for the inhibitor 1,
#'   inhibitor 1 metabolite, or inhibitor 2. This optionally includes
#'   interactions of the drug with itself. This returns a list of a)
#'   "affected_pathways" -- the affected metabolic pathways -- and b)
#'   "interactions" -- the specific set of elimination and interaction
#'   parameters and their values in the specified simulation.
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   concentration-time data, in quotes; must be an output file from the Simcyp
#'   simulator
#' @param existing_exp_details the output from running
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}}
#' @param include_auto_DDI TRUE or FALSE (default) for whether to list
#'   auto-induction or auto-inhibition.
#' @param include_victim_fms TRUE (default) or FALSE for whether to additionally
#'   read in any information on the substrate fm values for each pathway
#'   included in the interactions. This requires that a sheet titled "Time
#'   variance \%fm and fe", be included in the output.
#'
#' @return a list: "affected_pathways" (a character vector of the metabolic
#'   pathways affected) and "interactions" (a data.frame of all the affected
#'   pathways and their exact values for elimination and interaction paraemters)
#' @export
#'
#' @examples
#' # none yet
#' 
list_interactions <- function(sim_data_file, 
                              existing_exp_details, 
                              include_victim_fms = FALSE, 
                              include_auto_DDI = TRUE){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if(include_victim_fms & file.exists(sim_data_file) == FALSE){
      warning("You requested fm info for the victim drug, which we have to read from sim_data_file. That simulation file is not present, so we cannot return these data to you.", 
              call. = FALSE)
      include_victim_fms <- FALSE
   }
   
   # Main body of function ---------------------------------------------------
   
   details_subset <- annotateDetails(existing_exp_details, 
                                     sims_to_include = sim_data_file, 
                                     simulator_section = c("elimination", 
                                                           "transport", 
                                                           "interaction")) %>% 
      mutate(Pathway = str_extract(Detail, "(CYP|UGT)[0-9]{1,2}[A-Z][0-9]{1,3}|BCRP|Pgp|OAT[237]|OCT(N)?[123]|MATE[12](K)?|OATP[124][BC][13]|MRP[2346]|ASBT|PEPT1|MCT1|ENT[12]|NTCP|MDR1|GLUT1")) %>% 
      rename(Value = {{sim_data_file}}) %>% 
      filter(complete.cases(Pathway) & Value != "0")
   
   if(include_auto_DDI){
      
      vic_elim <- details_subset %>% 
         filter(SimulatorSection %in% c("Elimination", 
                                        "Transport"))
      
      perp_DDI <- details_subset %>% 
         filter(SimulatorSection == "Interaction")
      
   } else {
      
      vic_elim <- details_subset %>% 
         filter(CompoundID %in% AllCompounds$CompoundID[
            AllCompounds$DosedCompoundID == "substrate"] &
               SimulatorSection %in% c("Elimination", 
                                       "Transport"))
      
      perp_DDI <- details_subset %>% 
         filter(CompoundID %in% AllCompounds$CompoundID[
            AllCompounds$DosedCompoundID != "substrate"] &
               SimulatorSection == "Interaction")
   }
   
   vic_elim <- vic_elim %>% 
      select(CompoundID, Pathway, Detail, Value)
   
   perp_DDI <- perp_DDI %>% 
      select(CompoundID, Pathway, Detail, Value)
   
   affected_pathways <- intersect(unique(vic_elim$Pathway), 
                                  unique(perp_DDI$Pathway))
   
   interactions <- details_subset %>% 
      filter(Detail %in% c(vic_elim$Detail[vic_elim$Pathway %in% affected_pathways], 
                           perp_DDI$Detail[perp_DDI$Pathway %in% affected_pathways])) %>% 
      select(Pathway, CompoundID, Compound, SimulatorSection, Detail, Value, Notes) %>% 
      arrange(Pathway)
   
   if(include_victim_fms){
      Fms <- extractFmFe(sim_data_file = sim_data_file, 
                         existing_exp_details = existing_exp_details, 
                         returnOnlyMaxMin = TRUE, 
                         returnAggregateOrIndiv = "aggregate") %>% 
         filter(Parameter == "fm") %>% 
         select(Enzyme, Tissue, PerpPresent, Max) %>% 
         mutate(PerpPresent = ifelse(PerpPresent, "fm with DDI", "fm at baseline")) %>% 
         pivot_wider(names_from = PerpPresent, 
                     values_from = Max)
      
      return(list("affected_pathways" = affected_pathways, 
                  "interactions" = interactions, 
                  "fms" = Fms))
      
   } else {
      
      return(list("affected_pathways" = affected_pathways, 
                  "interactions" = interactions))
      
   }
   
}




