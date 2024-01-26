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
   
   MyPerpetrator <- determine_myperpetrator(Deets = Deets, 
                                            prettify_compound_names = prettify_compound_names, 
                                            parent_only = TRUE)
   
   SDorMD <- tolower(Deets$Regimen_sub)
   
   SingMult_sub <- ifelse(Deets$Regimen_sub %in% c("custom dosing",
                                                   "Multiple Dose"),
                          "multiple", "single")
   
   SDMD_sub_txt <- paste(ifelse(SingMult_sub == "single", "a single", "multiple"),
                         
                         switch(Deets$DoseRoute_sub,
                                "custom dosing" = "**CUSTOM DOSING - FILL IN MANUALLY**",
                                "Oral" = "oral",
                                "IV" = "IV"),
                         
                         ifelse(SingMult_sub == "single", "dose", "doses")
   )
   
   DoseFreq_sub <- switch(as.character(Deets$DoseInt_sub),
                          "12" = "BID",
                          "24" = "QD",
                          "8" = "three times per day",
                          "6" = "four times per day",
                          "48" = "every other day",
                          "NA" = "single dose")
   DoseFreq_sub <- ifelse(is.null(DoseFreq_sub),
                          # paste("Q", DoseFreq_sub, "H"),
                          "**CUSTOM DOSING - FILL IN MANUALLY**",
                          DoseFreq_sub)
   
   Heading_DDI <- ifelse(MyPerpetrator == "none", 
                         "", 
                         paste0("Administered with ",
                                str_to_title(MyPerpetrator), " "))
   
   Heading <- paste0("Simulation of ", 
                     sub("^A", "a", str_to_title(SDMD_sub_txt)), 
                     " of ", Deets$Dose_sub, " ", 
                     Deets$Units_dose_sub, " ", 
                     ifelse(class(prettify_compound_names) == "logical" &&
                               prettify_compound_names == TRUE, 
                            str_to_title(MySubstrate), MySubstrate),
                     " ",
                     ifelse(SingMult_sub == "multiple", 
                            paste0(DoseFreq_sub, " "), ""), 
                     Heading_DDI, "in ",
                     sub("Healthy Volunteers", "Healthy Subjects",
                         tidyPop(Deets$Population)$PopulationCap))
   
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
