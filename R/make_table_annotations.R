#' Make table headings and captions for use internally when saving PK tables to
#' Word
#'
#' \code{make_table_annotations} is meant to make a table heading and caption
#' for a table with a single set of results but will return more generic info
#' for tables with multiple sets of results. UNDER CONSTRUCTION.
#'
#' @param MyPKResults only the PK table
#' @param MyFile the specific sim_data_file in question
#' @param MyCompoundID substrate, inhibitor 1, etc. for this specific table. If
#'   length > 1, then generic info will be used.
#' @param prettify_compound_names pass through from parent function
#' @param Deets experimental details for only the single sim_data_file in
#'   question rather than for all possible files that may have been contained in
#'   existing_exp_details
#' @param MeanType mean type used
#' @param tissue tissue in which the PK parameters were measured
#' @param DosesIncluded which doses were included? options: "Dose1 Last" when
#'   both 1st and last-dose or user-defined interval info included, "Dose1" when
#'   it's only dose 1, "Last" when it's either last or user-defined info
#'   included. Leave as NA to make a guess, which works well when
#'   include_dose_num = TRUE in original PK table call but less so when
#'   include_dose_num = FALSE. Set to "no dose num included" to use the
#'   most-generic text for the annotations.
#' @param return_all_objects T or F (default) for whether to return a ton of
#'   objects for use downstream
#'
#' @return a list with a) TableHeading text and b) TableCaption text
#' @export
#'
#' @examples
#' # None yet
#'
#' 
make_table_annotations <- function(MyPKResults, # only PK table
                                   MyFile = NA, # specific sim_data_file in question
                                   MyCompoundID = NA,
                                   prettify_compound_names,
                                   Deets = NA,
                                   MeanType,
                                   tissue = NA, 
                                   DosesIncluded = NA, 
                                   return_all_objects = FALSE){
   
   
   # Main info ------------------------------------------------------------
   
   PKpulled <- prettify_column_names(MyPKResults, return_which_are_PK = TRUE)
   PKpulled <- PKpulled$PKparameter[which(PKpulled$IsPKParam)]
   
   Dose1included <- any(str_detect(names(PKpulled), "_dose1|Dose 1")) |
      str_detect(tolower(DosesIncluded), "dose( )?1")
   LastDoseincluded <- any(str_detect(names(PKpulled), "_last|Last dose")) |
      str_detect(tolower(DosesIncluded), "last")
   Observedincluded <- any(str_detect(MyPKResults$Statistic, "S/O|Observed"))
   
   MeanType <- ifelse(MeanType == "arithmetic for most, geometric for ratios", 
                      "arithmetic (except for DDI ratios, which are geometric)", 
                      MeanType)
   
   if(all(is.na(MyFile))){
      if("File" %in% names(MyPKResults)){
         MyFileLen <- length(sort(unique(MyPKResults$File)))
         MyFile <- str_comma(sort(unique(MyPKResults$File)))
      } else {
         MyFileLen <- 1
         MyFile <- "***SIMULATION FILE NAME***"
      }
   } else {
      MyFileLen <- length(MyFile)
      MyFile <- str_comma(unique(MyFile))
   }
   
   if(all(is.na(MyCompoundID))){
      if("CompoundID" %in% names(MyPKResults)){
         MyCompoundIDLen <- length(sort(unique(MyPKResults$CompoundID)))
         MyCompoundID <- str_comma(sort(unique(MyPKResults$CompoundID)))
      } else {
         # Assume it's the substrate
         MyCompoundIDLen <- 1
         MyCompoundID <- "substrate"
      }
   } else {
      MyCompoundIDLen <- length(MyCompoundID)
      MyCompoundID <- str_comma(MyCompoundID)
   }
   
   if(all(is.na(tissue))){
      if("Tissue" %in% names(MyPKResults)){
         MyTissueLen <- length(sort(unique(MyPKResults$Tissue)))
         tissue <- str_comma(sort(unique(MyPKResults$Tissue)))
      } else {
         # Assume it's plasma
         MyTissueLen <- 1
         tissue <- "plasma"
      }
   } else {
      MyTissueLen <- length(tissue)
      tissue <- str_comma(tissue)
   }
   
   # Caption ---------------------------------------------------------------
   
   CapText1 <- ifelse(any(str_detect(names(MyPKResults), "tmax")), 
                      paste("the", MeanType, "means, except for t~max~, which is median, minimum, and maximum"),
                      paste("the", MeanType, "means"))
   
   CapText2a <- paste0(ifelse(any(str_detect(MyPKResults$Statistic, "CV")),
                              "CV: coefficient of variation; ", ""), 
                       ifelse(any(str_detect(MyPKResults$Statistic, " CI")),
                              "CI: confidence interval; ", ""))
   
   CapText2 <- paste0(sub("; $", ". ", CapText2a), 
                      ifelse(Observedincluded, 
                             "S/O: simulated/observed. Source observed data: **Clinical Study XXX**; ",
                             ""))
   
   Caption <- paste0("Simulated values listed are ", 
                     CapText1, ". ", CapText2, "Source simulated data: ",
                     basename(MyFile))
   
   
   # Pass through for more generic info ---------------------------------------
   
   # There are some situations where we want to just pass through generic info,
   # so that's why I'm returning things here rather than stopping.
   if("logical" %in% class(Deets) | 
      ("data.frame" %in% class(Deets) && nrow(Deets) == 0) |
      any(c(MyTissueLen, MyCompoundIDLen, MyFileLen) > 1)){
      return(list(TableHeading = paste0("Simulated ",
                                        ifelse(Observedincluded, "and observed ", ""),
                                        tissue, " PK data"),
                  TableCaption = Caption))
      
   }
   
   # From here down, there is only 1 value for MyCompoundID, tissue, and MyFile.
   
   Deets <- harmonize_details(Deets)$MainDetails %>% 
      filter(File == MyFile)
   
   ## General info on MyCompoundID ----------------------------------------
   
   MyDosedCompound <- switch(MyCompoundID, 
                             "substrate" = Deets$Substrate,
                             "primary metabolite 1" = Deets$Substrate,
                             "primary metabolite 2" = Deets$Substrate,
                             "secondary metabolite" = Deets$Substrate,
                             "inhibitor 1" = Deets$Inhibitor1,
                             "inhibitor 2" = Deets$Inhibitor2,
                             "inhibitor 1 metabolite" = Deets$Inhibitor1)
   
   MyDoseRoute <- Deets[[switch(MyCompoundID, 
                                "substrate" = "DoseRoute_sub",
                                "primary metabolite 1" = "DoseRoute_sub",
                                "primary metabolite 2" = "DoseRoute_sub",
                                "secondary metabolite" = "DoseRoute_sub",
                                "inhibitor 1" = "DoseRoute_inhib",
                                "inhibitor 2" = "DoseRoute_inhib2",
                                "inhibitor 1 metabolite" = "DoseRoute_inhib")]]
   MyDoseRoute <- switch(MyDoseRoute, 
                         "Oral" = "oral", 
                         "IV" = "IV", 
                         "i.v. infusion" = "IV", 
                         "Inhaled" = "inhaled", 
                         "custom dosing" = "**CUSTOM DOSING - CHECK ADMINISTRATION ROUTE**")
   
   MyDose <- switch(MyCompoundID, 
                    "substrate" = "Dose_sub",
                    "primary metabolite 1" = "Dose_sub",
                    "primary metabolite 2" = "Dose_sub",
                    "secondary metabolite" = "Dose_sub",
                    "inhibitor 1" = "Dose_inhib",
                    "inhibitor 2" = "Dose_inhib2",
                    "inhibitor 1 metabolite" = "Dose_inhib")
   MyDose <- sub("custom dosing", "**CUSTOM DOSING**", MyDose)
   MyDose <- ifelse(is.na(MyDose), "**MISSING VALUE FOR DOSE; CHECK THIS**", MyDose)
   
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
   
   MyDoseInt <- paste0("DoseInt", AllCompounds$Suffix[
      AllCompounds$CompoundID == MyCompoundID])
   
   if(MyDoseInt %in% names(Deets)){
      MyDoseFreq <- switch(as.character(Deets[MyDoseInt]),
                           "12" = "BID", 
                           "24" = "QD", 
                           "8" = "three times per day", 
                           "6" = "four times per day", 
                           "48" = "every other day", 
                           "NA" = "single dose")
   } else {
      MyDoseFreq <- "single dose"
   }
   
   MyDoseFreq <- ifelse(is.null(MyDoseFreq), 
                        # paste("Q", MyDoseFreq, "H"), 
                        "**CUSTOM DOSING OR ATYPICAL DOSING INTERVAL - FILL IN MANUALLY**",
                        MyDoseFreq)
   
   ## Info on any perpetrators included ---------------------------------
   
   AllPerpetrators <- c("inhibitor 1" = ifelse("Inhibitor1" %in% names(Deets), 
                                               Deets$Inhibitor1, NA), 
                        "inhibitor 2" = ifelse("Inhibitor2" %in% names(Deets), 
                                               Deets$Inhibitor2, NA), 
                        "inhibitor 1 metabolite" = ifelse("Inhibitor1Metabolite" %in% names(Deets), 
                                                          Deets$Inhibitor1, NA))
   AllPerpetrators <- AllPerpetrators[complete.cases(AllPerpetrators)]
   
   MyPerpetrator <- determine_myperpetrator(Deets, prettify_compound_names)
   
   if(MyPerpetrator != "none"){
      SingMult_inhib <- ifelse(Deets$Regimen_inhib %in% c("custom dosing",
                                                          "Multiple Dose"), 
                               "multiple", "single")
      
      if("DoseInt_inhib" %in% names(Deets)){
         DoseFreq_inhib <- switch(as.character(Deets$DoseInt_inhib),
                                  "12" = "BID", 
                                  "24" = "QD", 
                                  "8" = "three times per day", 
                                  "6" = "four times per day", 
                                  "48" = "every other day", 
                                  "NA" = "single dose")
      } else {
         DoseFreq_inhib <- "single dose"
      }
      
      DoseFreq_inhib <- ifelse(is.null(DoseFreq_inhib), 
                               # paste("Q", DoseFreq_inhib, "H"), 
                               "**CUSTOM DOSING OR ATYPICAL DOSING INTERVAL - FILL IN MANUALLY**",
                               DoseFreq_inhib)
      
      NumDaysInhib <- suppressWarnings(
         Deets$NumDoses_inhib*as.numeric(Deets$DoseInt_inhib)/24)
      NumDaysInhib <- ifelse(is.na(NumDaysInhib), "**CUSTOM DOSING - FILL IN MANUALLY**",
                             NumDaysInhib)
      
      # FIXME - Started to set this up for when there are 2 inhibitors but
      # haven't finished b/c not applicable to my current situation
      if(complete.cases(Deets$Inhibitor2)){
         if("DoseInt_inhib2" %in% names(Deets)){
            DoseFreq_inhib2 <- switch(as.character(Deets$DoseInt_inhib2),
                                      "12" = "BID", 
                                      "24" = "QD", 
                                      "8" = "three times per day", 
                                      "6" = "four times per day", 
                                      "48" = "every other day", 
                                      "NA" = "single dose")
         } else {
            DoseFreq_inhib2 <- "single dose"
         }
         
         DoseFreq_inhib2 <- ifelse(is.null(DoseFreq_inhib2), 
                                   # paste("Q", DoseFreq_inhib2, "H"), 
                                   "**CUSTOM DOSING OR ATYPICAL DOSING INTERVAL - FILL IN MANUALLY**",
                                   DoseFreq_inhib2)
         
         NumDaysInhib2 <- suppressWarnings(
            Deets$NumDoses_inhib2*as.numeric(Deets$DoseInt_inhib2)/24)
         NumDaysInhib2 <- ifelse(is.na(NumDaysInhib2), "**CUSTOM DOSING - FILL IN MANUALLY**",
                                 NumDaysInhib2)
         
      }
      
      DoseDay_sub <- str_split_fixed(Deets$StartDayTime_sub, "Day |, ", 3)[2]
      LastDig <- as.numeric(str_sub(DoseDay_sub, start = -1, end = -1))
      DoseDay_sub <- paste0(DoseDay_sub, ifelse(LastDig %in% c(0, 4:9), 
                                                "th", 
                                                ifelse(LastDig == 1, "st", 
                                                       ifelse(LastDig == 2, "nd", "rd"))))
      
   } else {
      SingMult_inhib <- NA
      DoseFreq_inhib <- NA
      NumDaysInhib <- NA
      
   }
   
   # Heading --------------------------------------------------------------
   
   if(all(is.na(DosesIncluded))){
      DosesIncluded <- c("Dose1" = Dose1included, "Last" = LastDoseincluded)
      DosesIncluded <- str_c(names(DosesIncluded)[DosesIncluded == TRUE], 
                             collapse = " ")
      DosesIncluded <- ifelse(DosesIncluded == "", "no dose num included", DosesIncluded)
   }
   
   FigText2 <- switch(
      DosesIncluded, 
      "Dose1 Last" = paste("the first and multiple", 
                           MyDoseRoute, "doses"),
      
      "Dose1" = paste(ifelse(is.na(Deets$DoseInt_sub), 
                             "a single", "the first"),
                      MyDoseRoute, "dose"),
      
      "Last" = paste("multiple",
                     MyDoseRoute, "doses"), 
      
      "no dose num included" = ifelse(is.na(Deets$DoseInt_sub), 
                                      paste("a single", MyDoseRoute, "dose"), 
                                      paste("the first and/or multiple", MyDoseRoute, "doses"))
   )
   
   FigText3 <- ifelse(
      MyPerpetrator != "none" & 
         (MyCompoundID %in% c("inhibitor 1", "inhibitor 2",
                              "inhibitor 1 metabolite") == FALSE |
             length(setdiff(names(AllPerpetrators), MyCompoundID)) > 0),
      paste(" with or without", 
            sub("custom dosing", "**CUSTOM DOSING**", Deets$Dose_inhib), 
            "mg",
            MyPerpetrator, DoseFreq_inhib),
      "")
   
   Heading <- paste0("Simulated ",
                     ifelse(Observedincluded, "and observed ", ""),
                     MeanType, " mean ", 
                     str_comma(tissue), " PK parameters for ",
                     MyCompound, " after ", FigText2, " of ",
                     paste(sub("custom dosing", "**CUSTOM DOSING**", Deets[[MyDose]]), 
                           "mg", MyDosedCompound), 
                     FigText3, " in ", 
                     tidyPop(Deets$Population)$Population, ".")
   
   
   # Return -----------------------------------------------------------
   
   if(return_all_objects){
      return(list("TableHeading" = Heading, 
                  "TableCaption" = Caption, 
                  "MyCompoundID" = MyCompoundID,
                  "MyCompound" = MyCompound,
                  "MyDosedCompound" = MyDosedCompound, 
                  "MyDoseRoute" = MyDoseRoute, 
                  "MyDose" = MyDose, 
                  "MyDoseFreq" = MyDoseFreq,
                  "Dose1included" = Dose1included, 
                  "LastDoseincluded" = LastDoseincluded, 
                  "Observedincluded" = Observedincluded, 
                  "MyPerpetrator" = MyPerpetrator, # note that this is only 1 perpetrator
                  "DoseDay_sub" = DoseDay_sub))
   } else {
      return(list("TableHeading" = Heading, 
                  "TableCaption" = Caption))
   }
   
}


