#' Create some draft text to insert into the results section of a report
#' describing the trial design of a simulation
#'
#' @param sim_data_file name of the Excel file containing the simulator output,
#'   in quotes
#' @param existing_exp_details the output from running either
#'   \code{\link{extractExpDetails}} or \code{\link{extractExpDetails_mult}} --
#'   either is fine as long as it contains details for \code{sim_data_file}.
#' @param prettify_compound_names TRUE (default) or FALSE for whether to make
#'   compound names prettier in legend entries and in any Word output files.
#'   This was designed for simulations where the substrate and any metabolites,
#'   perpetrators, or perpetrator metabolites are among the standard options for
#'   the simulator, and leaving \code{prettify_compound_names = TRUE} will make
#'   the name of those compounds something more human readable. For example,
#'   "SV-Rifampicin-MD" will become "rifampicin", and "Sim-Midazolam" will
#'   become "midazolam". 
#' @param mean_type "arithmetic" or "geometric" (default) means in PK tables
#'
#' @return list of generic results text
#' @export
#'
#' @examples
#' # none yet
#' 
draft_results_text <- function(sim_data_file, 
                               existing_exp_details, 
                               prettify_compound_names = TRUE, 
                               mean_type = "geometric"){
   
   Deets <- filter_sims(existing_exp_details, 
                        sim_data_file, 
                        "include")$MainDetails
   
   MySubstrate <- ifelse(prettify_compound_names, 
                         prettify_compound_name(Deets$Substrate), Deets$Substrate)
   
   SDorMD <- tolower(Deets$Regimen_sub)
   
   Heading <- paste0("Simulation of ", 
                     switch(SDorMD, "single dose" = "a Single ", 
                            "multiple dose" = "Multiple Doses ", 
                            "custom dosing" = "Multiple Doses "), 
                     Deets$DoseRoute_sub, " ",
                     "Dose of ", 
                     Deets$Dose_sub, " ", 
                     Deets$Units_dose_sub, " ", 
                     MySubstrate, " in ", 
                     sub("Healthy Volunteers", "Healthy Subjects", 
                         tidyPop(Deets$Population)$PopulationSimpleCap))
   
   Body <- paste0("The simulated profile of ", 
                  MySubstrate, " was comparable to the clinical data (**Figure XXX**). The simulated ", 
                  mean_type, switch(SDorMD, 
                                    "single dose" = " AUC~inf~, ", 
                                    "multiple dose" = " AUC~tau~, ", 
                                    "custom dosing" = " AUC~tau~, "),
                  "CL/F, C~max~, and half life values and the median t~max~ value for ", 
                  MySubstrate, 
                  " administered to ", 
                  sub("healthy volunteers", "healthy subjects", 
                      tidyPop(Deets$Population)$Population), 
                  " were within **XXX fold** of the observed values (**Table XXX**).")
   
   return(list(Heading = Heading, 
               Body = Body))
}
