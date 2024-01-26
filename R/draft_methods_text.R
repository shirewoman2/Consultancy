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
   SDorMD <- TextPieces$SDorMD
   SingMult_sub <- TextPieces$SingMult_sub
   Pop <- TextPieces$Pop
   Body_DDI1 <- TextPieces$Body_DDI1
   Heading <- TextPieces$Heading
   SDMD_sub_txt <- TextPieces$SDMD_sub_txt
   
   numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight",
                "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
   
   Numbers <- str_to_title(numbers)
   
   DefaultCompound_txt <- 
      ifelse(default_cmpd_file, 
             paste(" The default compound library file for",
                   ifelse(victim_sim, MyPerpetrator, MySubstrate),
                   "was used. "), "")
   
   Body_DDI2 <- ifelse(MyPerpetrator == "none", 
                       "", 
                       paste0(DefaultCompound_txt,
                              " Performance verification of the ", 
                              ifelse(victim_sim, MyPerpetrator, MySubstrate), 
                              " model is provided in **Appendix A** – Performance Verification of CYP3A Substrates, Inhibitors and Inducers."))
   
   Body <- paste0(
      ifelse(Deets$NumTrials %in% 1:20, 
             Numbers[Deets$NumTrials], Deets$NumTrials), 
      " virtual trials of ", 
      Deets$NumSubjTrial, " ",
      Pop, 
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



