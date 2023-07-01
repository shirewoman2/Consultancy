#' Make table headings and captions for use internally when saving PK tables to
#' Word
#'
#' \code{make_table_annotations} is meant to make a table heading and caption
#' for a table with a single set of results. This is meant for use with
#' pksummary_table or for pksummary_mult when the results are split into
#' multiple tables rather than all together in one. Note that this is designed
#' for scenarios where, if there's an effector, there's only one. The text won't
#' be quite right if there are 2 effectors. UNDER CONSTRUCTION.
#'
#' @param MyPKResults only the PK table
#' @param MyFile the specific sim_data_file in question
#' @param PKpulled PK that was pulled -- the coded version, i.e., "AUCinf_dose1"
#'   or "Cmax_last"
#' @param MyCompoundID substrate, inhibitor 1, etc. for this specific table
#' @param prettify_compound_names pass through from parent function
#' @param Deets experimental details for only the single sim_data_file in
#'   question rather than for all possible files that may have been contained in
#'   existing_exp_details
#' @param MeanType mean type used
#' @param tissue tissue in which the PK parameters were measured
#'
#' @return a list with a) TableHeading text and b) TableCaption text
#'
#' @examples
#' # None yet
#'
#' 
make_table_annotations <- function(MyPKResults, # only PK table
                                   MyFile, # specific sim_data_file in question
                                   PKpulled,
                                   MyCompoundID,
                                   prettify_compound_names,
                                   Deets,
                                   MeanType,
                                   tissue){
   
   
   # Main info ------------------------------------------------------------
   
   Deets <- switch(as.character("File" %in% names(as.data.frame(Deets))), 
                   "TRUE" = as.data.frame(Deets), 
                   "FALSE" = deannotateDetails(Deets)) %>% 
      filter(File == MyFile)
   
   ## General info on MyCompoundID ----------------------------------------
   Dose1included <- any(str_detect(PKpulled, "_dose1"))
   LastDoseincluded <- any(str_detect(PKpulled, "_last"))
   Observedincluded <- any(str_detect(MyPKResults$Statistic, "S/O"))
   
   MyDosedCompound <- switch(MyCompoundID, 
                             "substrate" = Deets$Substrate,
                             "primary metabolite 1" = Deets$Substrate,
                             "primary metabolite 2" = Deets$Substrate,
                             "secondary metabolite" = Deets$Substrate,
                             "inhibitor 1" = Deets$Inhibitor1,
                             "inhibitor 2" = Deets$Inhibitor2,
                             "inhibitor 1 metabolite" = Deets$Inhibitor1)
   
   MyDoseRoute <- switch(MyCompoundID, 
                         "substrate" = "DoseRoute_sub",
                         "primary metabolite 1" = "DoseRoute_sub",
                         "primary metabolite 2" = "DoseRoute_sub",
                         "secondary metabolite" = "DoseRoute_sub",
                         "inhibitor 1" = "DoseRoute_inhib",
                         "inhibitor 2" = "DoseRoute_inhib2",
                         "inhibitor 1 metabolite" = "DoseRoute_inhib1")
   
   MyDose <- switch(MyCompoundID, 
                    "substrate" = "Dose_sub",
                    "primary metabolite 1" = "Dose_sub",
                    "primary metabolite 2" = "Dose_sub",
                    "secondary metabolite" = "Dose_sub",
                    "inhibitor 1" = "Dose_inhib",
                    "inhibitor 2" = "Dose_inhib2",
                    "inhibitor 1 metabolite" = "Dose_inhib")
   
   if(class(prettify_compound_names) == "logical" &&
      # NB: prettify_compound_names is the argument; prettify_compound_name is the function.
      prettify_compound_names){
      MyCompound <- prettify_compound_name(switch(MyCompoundID, 
                                                  "substrate" = Deets$Substrate,
                                                  "primary metabolite 1" = Deets$PrimaryMetabolite1,
                                                  "primary metabolite 2" = Deets$PrimaryMetabolite2,
                                                  "secondary metabolite" = Deets$SecondaryMetabolite,
                                                  "inhibitor 1" = Deets$Inhibitor1,
                                                  "inhibitor 2" = Deets$Inhibitor2,
                                                  "inhibitor 1 metabolite" = Deets$Inhibitor1Metabolite))
      MyDosedCompound <- prettify_compound_name(MyDosedCompound)
   } else if(class(prettify_compound_names) == "character"){
      MyCompound <- prettify_compound_names[MyCompoundID]
      MyDosedCompound <- prettify_compound_name(MyDosedCompound)
   } else {
      MyCompound <- switch(MyCompoundID, 
                           "substrate" = Deets$Substrate,
                           "primary metabolite 1" = Deets$PrimaryMetabolite1,
                           "primary metabolite 2" = Deets$PrimaryMetabolite2,
                           "secondary metabolite" = Deets$SecondaryMetabolite,
                           "inhibitor 1" = Deets$Inhibitor1,
                           "inhibitor 2" = Deets$Inhibitor2,
                           "inhibitor 1 metabolite" = Deets$Inhibitor1Metabolite)
   }
   
   DoseFreq_sub <- switch(as.character(Deets$DoseInt_sub),
                          "12" = "BID", 
                          "24" = "QD", 
                          "8" = "three times per day", 
                          "6" = "four times per day", 
                          "48" = "every other day", 
                          "NA" = "single dose")
   DoseFreq_sub <- ifelse(is.null(DoseFreq_sub), 
                          # paste("Q", DoseFreq_sub, "H"), 
                          "CUSTOM DOSING - FILL IN MANUALLY",
                          DoseFreq_sub)
   
   
   ## Info on any effectors included ---------------------------------
   
   # Note that make_table_annotations was designed for there being only 1 effector. 
   MyEffector <- determine_myeffector(Deets, prettify_compound_names)
   
   if(MyEffector != "none"){
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
                               "CUSTOM DOSING - FILL IN MANUALLY",
                               DoseFreq_inhib)
      
      NumDaysInhib <- suppressWarnings(
         Deets$NumDoses_inhib*as.numeric(Deets$DoseInt_inhib)/24)
      NumDaysInhib <- ifelse(is.na(NumDaysInhib), "CUSTOM DOSING - FILL IN MANUALLY",
                             NumDaysInhib)
      
      DoseDay <- str_split_fixed(Deets$StartDayTime_sub, "Day |, ", 3)[2]
      LastDig <- as.numeric(str_sub(DoseDay, start = -1, end = -1))
      DoseDay <- paste0(DoseDay, ifelse(LastDig %in% c(0, 4:9), 
                                        "th", 
                                        ifelse(LastDig == 1, "st", 
                                               ifelse(LastDig == 2, "nd", "rd"))))
      
   }
   
   # Heading --------------------------------------------------------------
   DosesIncluded <- c("Dose1" = Dose1included, "Last" = LastDoseincluded)
   DosesIncluded <- str_c(names(DosesIncluded)[DosesIncluded == TRUE], 
                          collapse = " ")
   
   FigText2 <- switch(DosesIncluded, 
                      "Dose1 Last" = paste("the first and multiple", 
                                           ifelse(Deets[[MyDoseRoute]] == "Oral", 
                                                  "oral", Deets[[MyDoseRoute]]),
                                           "doses"),
                      
                      "Dose1" = paste("the first",
                                      ifelse(Deets[[MyDoseRoute]] == "Oral", 
                                             "oral", Deets[[MyDoseRoute]]), 
                                      "dose"),
                      
                      "Last" = paste("multiple",
                                     ifelse(Deets[[MyDoseRoute]] == "Oral", 
                                            "oral", Deets[[MyDoseRoute]]), 
                                     "doses"))
   
   FigText3 <- ifelse(MyEffector != "none" & 
                         MyCompoundID %in% c("inhibitor 1", "inhibitor 2",
                                             "inhibitor 1 metabolite") == FALSE,
                      paste(" with or without", Deets$Dose_inhib, "mg",
                             MyEffector, DoseFreq_inhib),
                      "")
   
   Heading <- paste0("*Table XXX. Simulated ",
                     ifelse(Observedincluded, "and observed", ""),
                     MeanType, " mean ", tissue, " PK parameters for ",
                     MyCompound, " after ", FigText2, " of ",
                     paste(Deets[[MyDose]], "mg", MyDosedCompound), 
                     FigText3, " in ", 
                     tidyPop(Deets$Population)$PopulationSimpleLower, ".*")
   
   
   # Caption ---------------------------------------------------------------
   
   CapText1 <- ifelse(any(str_detect(names(MyPKResults), "tmax")), 
                      paste("the", MeanType, "means, except for t~max~, which is median, minimum, and maximum"),
                      paste("the", MeanType, "means"))
   
   CapText2a <- paste0(ifelse(any(str_detect(MyPKResults$Statistic, "CV")),
                              "CV: coefficient of variation; ", ""), 
                       ifelse(any(str_detect(MyPKResults$Statistic, " CI")),
                              "CI: confidence interval; ", ""))
   
   CapText2 <- paste0(sub("; $", ".", CapText2a), 
                      ifelse(Observedincluded, 
                             " S/O: simulated/observed; source observed data: Clinical Study XXX; ",
                             ""))
   
   Caption <- paste0("*Simulated values listed are ", 
                     CapText1, ". ", CapText2, " Source simulated data: ",
                     basename(MyFile), "*")
   
   
   # Return -----------------------------------------------------------
   
   return(list("TableHeading" = Heading, 
               "TableCaption" = Caption))
   
}


