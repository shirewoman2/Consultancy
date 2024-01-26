#' Create some draft text to insert into the methods section of a report
#' describing the trial design of a simulation
#'
#'
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
#' @param default_cmpd_file Was one of the default compound files used for the
#'   substrate (if this was a perpetrator simulation) or the perpetrator (if
#'   this was a victim simulation)? TRUE (default) or FALSE. The only thing this
#'   affects is the sentence in the template report text, "The default compound
#'   library file for XXX was used."
#' @param victim_sim TRUE (default) or FALSE for whether this was a victim DDI
#'   simulation, so "TRUE" means that the client's drug was the victim. The only
#'   thing this affects is the sentence in the template report text "Performance
#'   verification of the XXX model is provided in Appendix B - Performance
#'   Verification of CYP3A Substrates, Inhibitors and Inducers." If this was a
#'   victim DDI simulation, then this sentence will replace "XXX" with the name
#'   of the perpetrator. If not, it will replace "XXX" with the name of the
#'   substrate.
#'
#' @return list of generic methods text
#' @export
#'
#' @examples
#' # none yet
draft_methods_text <- function(sim_data_file, 
                               existing_exp_details, 
                               prettify_compound_names = TRUE, 
                               default_cmpd_file = TRUE, 
                               victim_sim = FALSE){
   
   Deets <- filter_sims(existing_exp_details, sim_data_file, "include")$MainDetails
   
   MyPerpetrator <- determine_myperpetrator(Deets = Deets, 
                                            prettify_compound_names = prettify_compound_names, 
                                            parent_only = TRUE)
   
   MySubstrate <- ifelse(class(prettify_compound_names) == "logical", 
                         ifelse(prettify_compound_names, 
                                prettify_compound_name(Deets$Substrate), Deets$Substrate), 
                         prettify_compound_names["substrate"])
   
   SingMult_sub <- ifelse(Deets$Regimen_sub %in% c("custom dosing",
                                                   "Multiple Dose"),
                          "multiple", "single")
   SingMult_inhib <- ifelse(Deets$Regimen_inhib %in% c("custom dosing",
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
   
   SDMD_inhib_txt <- paste(ifelse(SingMult_inhib == "single", "a single", "multiple"),
                           
                           switch(Deets$DoseRoute_inhib,
                                  "custom dosing" = "**CUSTOM DOSING - FILL IN MANUALLY**",
                                  "Oral" = "oral",
                                  "IV" = "IV"),
                           
                           ifelse(SingMult_inhib == "single", "dose", "doses")
   )
   
   DoseFreq_inhib <- switch(as.character(Deets$DoseInt_inhib),
                            "12" = "BID",
                            "24" = "QD",
                            "8" = "three times per day",
                            "6" = "four times per day",
                            "48" = "every other day",
                            "NA" = "single dose")
   DoseFreq_inhib <- ifelse(is.null(DoseFreq_inhib),
                            # paste("Q", DoseFreq_inhib, "H"),
                            "**CUSTOM DOSING - FILL IN MANUALLY**",
                            DoseFreq_inhib)
   
   NumDaysInhib <- suppressWarnings(
      Deets$NumDoses_inhib*as.numeric(Deets$DoseInt_inhib)/24)
   NumDaysInhib <- ifelse(is.na(NumDaysInhib), "**CUSTOM DOSING - FILL IN MANUALLY**",
                          NumDaysInhib)
   
   numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight",
                "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
   
   Numbers <- str_to_title(numbers)
   
   DoseDay <- str_split_fixed(Deets$StartDayTime_sub, "Day |, ", 3)[2]
   LastDig <- as.numeric(str_sub(DoseDay, start = -1, end = -1))
   DoseDay <- paste0(DoseDay, 
                     case_when(LastDig %in% c(0, 4:9) ~ "^th^",
                               LastDig == 1 ~ "^st^",
                               LastDig == 2~ "^nd^",
                               LastDig == 3 ~ "^rd^"))
   
   DefaultCompound_txt <- ifelse(default_cmpd_file, 
                                 paste(" The default compound library file for",
                                       ifelse(victim_sim, MyPerpetrator, MySubstrate),
                                       "was used. "), "")
   
   Heading_DDI <- ifelse(MyPerpetrator == "none", 
                         "", 
                         paste0(" Administered with ",
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
                     Heading_DDI, " in ",
                     sub("Healthy Volunteers", "Healthy Subjects",
                         tidyPop(Deets$Population)$PopulationCap))
   
   Body_DDI1 <- ifelse(MyPerpetrator == "none", 
                      "", 
                      paste0("in the absence of ", 
                             MyPerpetrator, " and ", 
                             ifelse(SingMult_sub == "multiple", "with",
                                    paste("on the", DoseDay, "day of")), 
                             " ", NumDaysInhib, " days of dosing of ", 
                             MyPerpetrator, " (", Deets$Dose_inhib, " ", 
                             Deets$Units_dose_inhib, " ", 
                             DoseFreq_inhib, ") "))
   
   Body_DDI2 <- ifelse(MyPerpetrator == "none", 
                       "", 
                       paste0(DefaultCompound_txt,
                              " Performance verification of the ", 
                              ifelse(victim_sim, MyPerpetrator, MySubstrate), 
                              " model is provided in **Appendix A** – Performance Verification of CYP3A Substrates, Inhibitors and Inducers."))
   
   Body <- paste0(
      ifelse(Deets$NumTrials %in% 1:20, Numbers[Deets$NumTrials], Deets$NumTrials), 
      " virtual trials of ", 
      Deets$NumSubjTrial, " ",
      sub("healthy volunteers", "healthy subjects", tidyPop(Deets$Population)$Population), 
      " (", round(Deets$PercFemale*100, 2), "% female) aged ", 
      Deets$Age_min, " to ", Deets$Age_max, 
      " years receiving ", SDMD_sub_txt, " of ", 
      Deets$Dose_sub, " ", 
      Deets$Units_dose_sub, " ", 
      MySubstrate, " ",
      Body_DDI1, 
      "were generated.", 
      Body_DDI2)
   
   return(list(Heading = Heading, 
               Body = Body))
   
}



