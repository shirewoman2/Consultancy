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
   
   TextPieces <- make_text_legos(sim_data_file = sim_data_file, 
                                 existing_exp_details = existing_exp_details, 
                                 prettify_compound_names = prettify_compound_names)
   
   Deets <- TextPieces$Deets
   MySubstrate <- TextPieces$MySubstrate
   MyPerpetrator <- TextPieces$MyPerpetrator
   SingMult_sub <- TextPieces$SingMult_sub
   DosingText_sub_lower <- TextPieces$DosingText_sub_lower
   NumDaysInhib <- TextPieces$NumDaysInhib
   DosingText_inhib_lower <- TextPieces$DosingText_inhib_lower
   DoseFreq_inhib <- TextPieces$DoseFreq_inhib
   DoseDay_ordinal <- TextPieces$DoseDay_ordinal
   
   Pop <- TextPieces$Pop
   Heading <- TextPieces$Heading
   
   
   # DDI pieces ---------------------------------------------------------------
   
   Body_DDI1 <-
      ifelse(MyPerpetrator == "none",
             "",
             paste0("in the absence of ",
                    MyPerpetrator, " and ",
                    ifelse(SingMult_sub == "multiple", "with",
                           paste("on the", DoseDay_ordinal, "day of")),
                    " ", NumDaysInhib, " days of dosing of ",
                    MyPerpetrator, " (", Deets$Dose_inhib, " ",
                    Deets$Units_dose_inhib, " ",
                    DoseFreq_inhib, ")"))

   Body_DDI2 <- ifelse(MyPerpetrator == "none",
                       "",
                       paste0(" The default compound library file for ", 
                              ifelse(victim_sim, MyPerpetrator, MySubstrate), 
                              " was used. Performance verification of the ",
                              ifelse(victim_sim, MyPerpetrator, MySubstrate),
                              " model is provided in **Appendix A** – Performance Verification of CYP3A Substrates, Inhibitors and Inducers."))
   
   numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight",
                "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
   
   Numbers <- str_to_title(numbers)
   
   
   # Development or verification ----------------------------------------------
   
   Body_dev_ver <- paste0(
      "The trial design used for simulating plasma concentration-time profiles of ", 
      MySubstrate, 
      " in ", Pop, " was based on Clinical Study **XXX**. ",
      
      ifelse(MyPerpetrator == "none", 
             paste0("In the study, ", 
                    Pop, " aged ", 
                    Deets$Age_min, " to ", Deets$Age_max, 
                    " years (", round(Deets$PercFemale*100, 2), "% female) received ",
                    DosingText_sub_lower), 
              
             paste0("In treatment period 1, subjects received ", 
                    DosingText_sub_lower, 
                    ". In treatment period 2, subjects received ", 
                    DosingText_inhib_lower, " alone. On Day ", 
                    Deets$StartHr_sub %/% 24 + 1, 
                    ", subjects received ", 
                    Deets$Dose_inhib, " ", 
                    ifelse(is.na(Deets$Units_dose_inhib), 
                           "", paste0(Deets$Units_dose_inhib, " ")),
                    MyPerpetrator, 
                    " co-administered with ", 
                    DosingText_sub_lower, ". ", 
                    ifelse(Deets$NumSubjTrial %in% 1:20, 
                           Numbers[Deets$NumSubjTrial], Deets$NumSubjTrial), 
                    " subjects (", round(Deets$PercFemale*100, 2), "% female; aged ", 
                    Deets$Age_min, " to ", Deets$Age_max, 
                    " years) were recruited into the study")),
             
      ". For the simulations, ", Deets$NumTrials, " virtual trials of ", 
      Deets$NumSubjTrial, " subjects **matched by age, sex, height, and weight** were generated to assess variability across groups. The simulated concentration-time profiles of ", 
      MySubstrate, " were compared to observed data.")
   
   
   # Application --------------------------------------------------------------
   
   Body_app <- paste0(
      ifelse(Deets$NumTrials %in% 1:20, 
             Numbers[Deets$NumTrials], Deets$NumTrials), 
      " virtual trials of ", 
      Deets$NumSubjTrial, " ",
      Pop, 
      " (", round(Deets$PercFemale*100, 2), "% female) aged ", 
      Deets$Age_min, " to ", Deets$Age_max, 
      " years receiving ", DosingText_sub_lower, " ",
      
      ifelse(MyPerpetrator == "none", 
             "", paste0(Body_DDI1, " ")),
      "were generated.", 
      
      ifelse(MyPerpetrator == "none", 
             "", Body_DDI2))
   
   
   # DosingTextDays_inhib <-
   #    paste0(ifelse(SingMult_inhib == "single", 
   #                  "a single dose of ", ""), 
   #           Deets$Dose_inhib, " ", 
   #           ifelse(is.na(Deets$Units_dose_inhib), 
   #                  "", paste0(Deets$Units_dose_inhib, " ")),
   #           MyPerpetrator, " alone ", DoseFreq_inhib, ". ",
   #           "On Day ", Deets$StartHr_sub %/% 24 + 1, ", ",
   #           "subjects received ", 
   #           Deets$Dose_inhib, " ", 
   #           ifelse(is.na(Deets$Units_dose_inhib), 
   #                  "", paste0(Deets$Units_dose_inhib, " ")),
   #           MyPerpetrator, " co-adminstered with ", 
   #           DosingText_sub_lower, ".")
   
   
   # Output --------------------------------------------------------------------
   
   return(list(Heading = Heading, 
               Body_dev_ver = Body_dev_ver, 
               Body_app = Body_app))
   
}



