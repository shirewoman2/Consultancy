#' INTERNAL - Assemble pieces of text for headings, captions and trial design
#' descriptions
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
#'
#' @return list of text strings
#'
#' @examples
#' # none
#' 
make_text_legos <- function(sim_data_file, 
                            existing_exp_details, 
                            prettify_compound_names){
   
   Deets <- filter_sims(existing_exp_details, 
                        sim_data_file, 
                        "include")$MainDetails
   
   if(nrow(Deets) == 0){
      warning("We can't find the sim_data_file requested in what you supplied for existing_exp_details. We can't return any info here.\n", 
              call. = FALSE)
      
      return(list())
   }
   
   # Compounds involved -------------------------------------------------------
   MySubstrate <- ifelse(class(prettify_compound_names) == "logical", 
                         ifelse(prettify_compound_names, 
                                prettify_compound_name(Deets$Substrate), Deets$Substrate), 
                         prettify_compound_names["substrate"])
   
   MyPerpetrator <- determine_myperpetrator(Deets = Deets, 
                                            prettify_compound_names = prettify_compound_names, 
                                            parent_only = TRUE)
   
   
   # Dosing regimens ----------------------------------------------------------
   
   SingMult_sub <- ifelse(Deets$Regimen_sub %in% c("custom dosing",
                                                   "Multiple Dose"),
                          "multiple", "single")
   
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
   
   DosingText_sub_lower <- 
      paste0(ifelse(SingMult_sub == "single", "a single ", "multiple "),
             switch(Deets$DoseRoute_sub,
                    "custom dosing" = "**CUSTOM DOSING - FILL IN MANUALLY** ",
                    "Oral" = "oral ",
                    "IV" = "IV "),
             ifelse(SingMult_sub == "single", "dose", "doses"), 
             " of ", Deets$Dose_sub, " ", 
             ifelse(is.na(Deets$Units_dose_sub), 
                    "", paste0(Deets$Units_dose_sub, " ")),
             MySubstrate,
             ifelse(SingMult_sub == "multiple", 
                    paste0(" ", DoseFreq_sub), ""))
   
   DosingText_sub_upper <- 
      paste0(ifelse(SingMult_sub == "single", "a Single ", "Multiple "),
             switch(Deets$DoseRoute_sub,
                    "custom dosing" = "**CUSTOM DOSING - FILL IN MANUALLY** ",
                    "Oral" = "Oral ",
                    "IV" = "IV "),
             ifelse(SingMult_sub == "single", "Dose", "Doses"), 
             " of ", Deets$Dose_sub, " ", 
             ifelse(is.na(Deets$Units_dose_sub), 
                    "", paste0(Deets$Units_dose_sub, " ")),
             ifelse(class(prettify_compound_names) == "logical" &&
                       prettify_compound_names == TRUE, 
                    str_to_title(MySubstrate), MySubstrate),
             ifelse(SingMult_sub == "multiple", 
                    paste0(" ", DoseFreq_sub), ""))
   
   
   if(MyPerpetrator != "none"){
      
      SingMult_inhib <- ifelse(Deets$Regimen_inhib %in% c("custom dosing",
                                                          "Multiple Dose"),
                               "multiple", "single")
      
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
      
      DosingText_inhib_lower <- 
         paste0(ifelse(SingMult_inhib == "single", "a single ", "multiple "),
                switch(Deets$DoseRoute_inhib,
                       "custom dosing" = "**CUSTOM DOSING - FILL IN MANUALLY** ",
                       "Oral" = "oral ",
                       "IV" = "IV "),
                ifelse(SingMult_inhib == "single", "dose", "doses"), 
                " of ", Deets$Dose_inhib, " ", 
                ifelse(is.na(Deets$Units_dose_inhib), 
                       "", paste0(Deets$Units_dose_inhib, " ")),
                MyPerpetrator,
                ifelse(SingMult_inhib == "multiple", 
                       paste0(" ", DoseFreq_inhib), ""))
      
      DosingText_inhib_upper <- 
         paste0(ifelse(SingMult_inhib == "single", "a Single ", "Multiple "),
                switch(Deets$DoseRoute_inhib,
                       "custom dosing" = "**CUSTOM DOSING - FILL IN MANUALLY** ",
                       "Oral" = "Oral ",
                       "IV" = "IV "),
                ifelse(SingMult_inhib == "single", "Dose", "Doses"), 
                " of ", Deets$Dose_inhib, " ", 
                ifelse(is.na(Deets$Units_dose_inhib), 
                       "", paste0(Deets$Units_dose_inhib, " ")),
                ifelse(class(prettify_compound_names) == "logical" &&
                          prettify_compound_names == TRUE, 
                       str_to_title(MyPerpetrator), MyPerpetrator),
                ifelse(SingMult_inhib == "multiple", 
                       paste0(" ", DoseFreq_inhib), ""))
      
      NumDaysInhib <- suppressWarnings(
         Deets$NumDoses_inhib*as.numeric(Deets$DoseInt_inhib)/24)
      NumDaysInhib <- ifelse(is.na(NumDaysInhib), "**CUSTOM DOSING - FILL IN MANUALLY**",
                             NumDaysInhib)
      
      DoseDay_ordinal <- str_split_fixed(Deets$StartDayTime_sub, "Day |, ", 3)[2]
      LastDig <- as.numeric(str_sub(DoseDay_ordinal, start = -1, end = -1))
      DoseDay_ordinal <- paste0(DoseDay_ordinal,
                                case_when(LastDig %in% c(0, 4:9) ~ "^th^",
                                          LastDig == 1 ~ "^st^",
                                          LastDig == 2~ "^nd^",
                                          LastDig == 3 ~ "^rd^"))
      
   } else {
      DosingText_inhib_lower <- NA
      DosingText_inhib_upper <- NA
      NumDaysInhib <- NA
      DoseFreq_inhib <- NA
      DoseDay_ordinal <- NA
   }
   
   # LastDoseDay_sub <- str_split_fixed(Deets$StartDayTime_sub, "Day |, ", 3)[2]
   if(complete.cases(Deets$DoseInt_sub) && 
      Deets$DoseInt_sub == "custom dosing"){
      LastDoseDay_sub <- "CUSTOM DOSING -- FILL IN MANUALLY"
   } else {
      LastDoseDay_sub <- (as.numeric(Deets$StartHr_sub) + 
                             as.numeric(Deets$NumDoses_sub) * as.numeric(Deets$DoseInt_sub)) %/% 24
   }
   
   # Population ---------------------------------------------------------------
   
   Pop <- sub("healthy volunteers", "healthy subjects",
              tidyPop(Deets$Population)$Population)
   
   PopCap <- sub("Healthy Volunteers", "Healthy Subjects",
                 tidyPop(Deets$Population)$PopulationCap)
   
   # Heading text -------------------------------------------------------------
   
   Heading_DDI <- ifelse(MyPerpetrator == "none", 
                         "", 
                         paste0(" with ", DosingText_inhib_upper))
   
   Heading <- paste0("Simulation of ", 
                     DosingText_sub_upper, 
                     Heading_DDI, " in ", PopCap)
   
   
   # Output -------------------------------------------------------------------
   
   return(list(
      Deets = Deets, 
      MySubstrate = MySubstrate, 
      MyPerpetrator = MyPerpetrator, 
      SingMult_sub = SingMult_sub, 
      DoseFreq_sub = DoseFreq_sub, 
      DoseFreq_inhib = DoseFreq_inhib, 
      DosingText_sub_lower = DosingText_sub_lower, 
      DosingText_sub_upper = DosingText_sub_upper,
      DosingText_inhib_lower = DosingText_inhib_lower, 
      DosingText_inhib_upper = DosingText_inhib_upper,
      Pop = Pop,
      LastDoseDay_sub = LastDoseDay_sub,
      DoseDay_ordinal = DoseDay_ordinal,
      NumDaysInhib = NumDaysInhib, 
      Heading = Heading
   ))
   
   
}