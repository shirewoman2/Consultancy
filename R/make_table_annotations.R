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
#' @param existing_exp_details experimental details for only the single
#'   sim_data_file in question rather than for all possible files that may have
#'   been contained in existing_exp_details
#' @param mean_type mean type used
#' @param tissue tissue in which the PK parameters were measured
#' @param DosesIncluded which doses were included? options: "Dose1 Last" when
#'   both 1st and last-dose or user-defined interval info included, "Dose1" when
#'   it's only dose 1, "Last" when it's either last or user-defined info
#'   included. Leave as NA to make a guess, which works well when
#'   include_dose_num = TRUE in original PK table call but less so when
#'   include_dose_num = FALSE. Set to "no dose num included" to use the
#'   most-generic text for the annotations.
#' @param name_clinical_study optionally specify the name(s) of the clinical
#'   study or studies for any observed data. This only affects the caption of
#'   the graph. For example, specifying \code{name_clinical_study = "101, fed
#'   cohort"} will result in a figure caption that reads in part "Clinical Study
#'   101, fed cohort". If you have more than one study, that's fine; we'll take
#'   care of stringing them together appropriately. Just list them as a
#'   character vector, e.g., \code{name_clinical_study = c("101",
#'   "102", "103")} will become "clinical studies 101, 102, and 103."
#' @param return_all_objects T or F (default) for whether to return a ton of
#'   objects for use downstream
#'
#' @return a list with a) table_heading text and b) table_caption text
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
                                   existing_exp_details = NA,
                                   mean_type,
                                   tissue = NA, 
                                   DosesIncluded = NA, 
                                   name_clinical_study = NA, 
                                   return_all_objects = FALSE){
   
   # Error catching -------------------------------------------------------
   
   if(class(prettify_compound_names) != "logical"){
      
      if(is.null(names(prettify_compound_names))){
         warning(wrapn("It looks like you're trying to supply a character vector for the argument 'prettify_compound_names', but we need that character vector to be named, which it is not. We'll set 'prettify_compound_names' to FALSE for now, but please check the help file on this."), 
                 call. = FALSE)
         prettify_compound_names <- FALSE
      } else {
         # Making sure that the names are compound IDs
         names(prettify_compound_names)[
            which(tolower(names(prettify_compound_names)) == "perpetrator")] <- "inhibitor 1"
         
         if(any(names(prettify_compound_names) %in% 
                AllRegCompounds$CompoundID == FALSE)){
            warning(wrapn("It looks like you're trying to supply a named character vector for the argument 'prettify_compound_names', but we need the names to be compound IDs such as 'substrate' or 'inhibitor 1'. We'll set 'prettify_compound_names' to FALSE for now, but please check the help file on this."), 
                    call. = FALSE)
            prettify_compound_names <- FALSE
         }
      }
   }
   
   
   # Main info ------------------------------------------------------------
   
   PKpulled <- prettify_column_names(MyPKResults, return_which_are_PK = TRUE)
   PKpulled <- PKpulled$PKparameter[which(PKpulled$IsPKParam)]
   
   Dose1included <- any(str_detect(names(PKpulled), "_dose1|Dose 1")) |
      str_detect(tolower(DosesIncluded), "dose( )?1")
   LastDoseincluded <- any(str_detect(names(PKpulled), "_last|Last dose")) |
      str_detect(tolower(DosesIncluded), "last")
   Observedincluded <- any(str_detect(MyPKResults$Statistic, "S/O|Observed"))
   
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
      MyFile <- str_comma(unique(basename(MyFile)))
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
   
   if(all(PKpulled %in% 
          (AllPKParameters %>% 
           filter(AppliesToAllTissues == TRUE) %>% 
           pull(PKparameter)))){
      MyTissueLen <- 1
      tissue <- NA
      
   } else if(all(is.na(tissue))){
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
   
   # Setting up text for clinical study or studies ----------------------------
   
   if(length(name_clinical_study) > 1){
      MyClinStudies <- str_comma(name_clinical_study)
   } else if(complete.cases(name_clinical_study)){
      MyClinStudies <- paste("Clinical Study", name_clinical_study)
   } else {
      MyClinStudies <- "Clinical Study **XXX**"
   }
   
   
   # Caption ---------------------------------------------------------------
   
   CapText1 <- case_when(
      any(str_detect(names(MyPKResults), "tmax")) & 
         mean_type %in% c("arithmetic", "geometric") & 
         any(str_detect(MyPKResults$Statistic, "range|min|max|CI|centile")) ~ 
         paste("the", mean_type, "means, except for t~max~ values, which are medians and ranges"),
      
      any(str_detect(names(MyPKResults), "tmax")) & 
         mean_type %in% c("arithmetic", "geometric") & 
         any(str_detect(MyPKResults$Statistic, "range|min|max|CI|centile")) == FALSE ~ 
         paste("the", mean_type, "means, except for t~max~ values, which are medians"),
      
      any(str_detect(names(MyPKResults), "tmax")) & 
         mean_type %in% c("arithmetic for most, geometric for ratios") & 
         any(str_detect(MyPKResults$Statistic, "range|min|max|CI|centile")) ~ 
         "the arithmetic means, except for t~max~ values, which are medians and ranges, and the ratios, which are geometric", 
      
      any(str_detect(names(MyPKResults), "tmax")) & 
         mean_type %in% c("arithmetic for most, geometric for ratios") & 
         any(str_detect(MyPKResults$Statistic, "range|min|max|CI|centile")) == FALSE ~ 
         "the arithmetic means, except for t~max~ values, which are medians, and the ratios, which are geometric", 
      
      any(str_detect(names(MyPKResults), "tmax")) == FALSE & 
         mean_type %in% c("arithmetic", "geometric") ~ 
         paste("the", mean_type, "means"), 
      
      any(str_detect(names(MyPKResults), "tmax")) == FALSE & 
         mean_type %in% c("arithmetic for most, geometric for ratios") ~ 
         "the arithmetic means, except for the ratios, which are geometric")
   
   CapText2a <- paste0(ifelse(any(str_detect(MyPKResults$Statistic, "CV")),
                              "CV: coefficient of variation; ", ""), 
                       ifelse(any(str_detect(MyPKResults$Statistic, " CI")),
                              "CI: confidence interval; ", ""))
   
   CapText2 <- paste0(sub("; $", ". ", CapText2a), 
                      ifelse(Observedincluded, 
                             
                             # obs data present
                             paste0("S/O: simulated/observed. Source observed data: ", 
                                    MyClinStudies, 
                                    "; "),
                             
                             # no obs data present
                             ""))
   
   Caption <- paste0("Simulated values listed are ", 
                     CapText1, ". ", CapText2, "Source simulated data: ",
                     MyFile)
   
   
   # Pass through for more generic info ---------------------------------------
   
   # There are some situations where we want to just pass through generic info,
   # so that's why I'm returning things here rather than stopping.
   if("logical" %in% class(existing_exp_details) | 
      ("data.frame" %in% class(existing_exp_details) && 
       nrow(existing_exp_details) == 0) |
      nrow(filter_sims(existing_exp_details, MyFile, "include")$MainDetails) == 0 |
      any(c(MyTissueLen, MyCompoundIDLen, MyFileLen) > 1)){
      return(list(table_heading = paste0("Simulated ",
                                         ifelse(Observedincluded, "and observed ", ""),
                                         tissue, " PK data"),
                  table_caption = Caption))
      
   }
   
   # From here down, there is only 1 value for MyCompoundID, tissue, and MyFile.
   
   Deets <- harmonize_details(existing_exp_details)$MainDetails %>% 
      filter(File == MyFile)
   
   CustomDosing <- 
      Deets[[switch(MyCompoundID, 
                    "substrate" = "DoseRoute_sub",
                    "primary metabolite 1" = "DoseRoute_sub",
                    "primary metabolite 2" = "DoseRoute_sub",
                    "secondary metabolite" = "DoseRoute_sub",
                    "inhibitor 1" = "DoseRoute_inhib",
                    "inhibitor 2" = "DoseRoute_inhib2",
                    "inhibitor 1 metabolite" = "DoseRoute_inhib")]] == "custom dosing"
   
   
   ## General info on MyCompoundID ----------------------------------------
   
   MyDosedCompound <- switch(MyCompoundID, 
                             "substrate" = Deets$Substrate,
                             "primary metabolite 1" = Deets$Substrate,
                             "primary metabolite 2" = Deets$Substrate,
                             "secondary metabolite" = Deets$Substrate,
                             "inhibitor 1" = Deets$Inhibitor1,
                             "inhibitor 2" = Deets$Inhibitor2,
                             "inhibitor 1 metabolite" = Deets$Inhibitor1)
   
   MyDoseRoute <- existing_exp_details$Dosing %>% 
      filter(File %in% MyFile & CompoundID %in% MyCompoundID) %>% 
      select(DoseRoute) %>% 
      mutate(DoseRoute = case_match(DoseRoute, 
                                    "Oral" ~ "oral", 
                                    "IV" ~ "IV", 
                                    "i.v. infusion" ~ "IV", 
                                    "i.v. bolus" ~ "IV", 
                                    "Inhaled" ~ "inhaled")) %>% 
      pull(DoseRoute) %>% unique() %>% 
      str_comma()
   
   MyDose <- existing_exp_details$Dosing %>% 
      filter(File %in% MyFile & CompoundID %in% MyCompoundID) %>%
      pull(Dose) %>% sort() %>% unique()
   
   if(length(MyDose) > 1){
      MyDose <- paste0(str_comma(MyDose), " **multiple dose levels - check wording here**")
   } 
   
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
      MyDosedCompound <- prettify_compound_names[
         AllRegCompounds$DosedCompoundID[AllRegCompounds$CompoundID == MyCompoundID]]
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
   
   MyDoseUnits <- existing_exp_details$Dosing %>% 
      filter(File %in% MyFile & CompoundID %in% MyCompoundID) %>% 
      pull(Dose_units) %>% unique() %>% 
      str_comma()
   
   MyDoseInt <- paste0("DoseInt", AllRegCompounds$Suffix[
      AllRegCompounds$CompoundID == MyCompoundID])
   
   if(MyDoseInt %in% names(Deets)){
      
      if(CustomDosing){
         MyDoseFreq <- ""
         
      } else {
         
         MyDoseInt <- Deets[[MyDoseInt]]
         
         MyDoseFreq <- case_when(
            MyDoseInt == 12 ~ "BID", 
            MyDoseInt == 24 ~ "QD", 
            MyDoseInt == 8 ~ "three times per day", 
            MyDoseInt == 6 ~ "four times per day", 
            MyDoseInt == 48 ~ "every other day", 
            is.na(MyDoseInt) ~ "single dose", 
            .default = paste("every", MyDoseInt, "hours"))
      }
      
   } else {
      MyDoseFreq <- "single dose"
   }
   
   
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
         DoseFreq_inhib <- case_when(
            Deets$DoseInt_inhib == 12 ~ "BID", 
            Deets$DoseInt_inhib == 24 ~ "QD", 
            Deets$DoseInt_inhib == 8 ~ "three times per day", 
            Deets$DoseInt_inhib == 6 ~ "four times per day", 
            Deets$DoseInt_inhib == 48 ~ "every other day", 
            is.na(Deets$DoseInt_inhib) ~ "single dose", 
            .default = paste("every", Deets$DoseInt_inhib, "hours"))
         
      } else {
         DoseFreq_inhib <- "single dose"
      }
      
      DoseFreq_inhib <- ifelse(is.null(DoseFreq_inhib), 
                               # paste("Q", DoseFreq_inhib, "H"), 
                               "**CUSTOM DOSING OR ATYPICAL DOSING INTERVAL - FILL IN MANUALLY**",
                               DoseFreq_inhib)
      
      NumDaysInhib <- suppressWarnings(
         as.numeric(Deets$NumDoses_inhib) * as.numeric(Deets$DoseInt_inhib)/24)
      NumDaysInhib <- ifelse(is.na(NumDaysInhib), "**CUSTOM DOSING - FILL IN MANUALLY**",
                             NumDaysInhib)
      
      # FIXME - Started to set this up for when there are 2 inhibitors but
      # haven't finished b/c not applicable to my current situation
      if(complete.cases(Deets$Inhibitor2)){
         if("DoseInt_inhib2" %in% names(Deets)){
            DoseFreq_inhib2 <- case_when(
               DoseInt_inhib2 == 12 ~ "BID", 
               DoseInt_inhib2 == 24 ~ "QD", 
               DoseInt_inhib2 == 8 ~ "three times per day", 
               DoseInt_inhib2 == 6 ~ "four times per day", 
               DoseInt_inhib2 == 48 ~ "every other day", 
               is.na(DoseInt_inhib2) ~ "single dose", 
               .default = paste("every", DoseInt_inhib2, "hours"))
            
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
      DoseDay_sub <- paste0(DoseDay_sub, 
                            case_when(LastDig %in% c(0, 4:9) ~ "th", 
                                      LastDig == 1 ~ "st", 
                                      LastDig == 2 ~ "nd",
                                      LastDig == 3 ~ "rd"))
      
   } else {
      SingMult_inhib <- NA
      DoseFreq_inhib <- NA
      NumDaysInhib <- NA
      
   }
   
   # Heading --------------------------------------------------------------
   
   if(all(is.na(DosesIncluded)) |
      all(DosesIncluded == "")){
      DosesIncluded <- c("Dose1" = Dose1included, "Last" = LastDoseincluded)
      DosesIncluded <- str_c(names(DosesIncluded)[DosesIncluded == TRUE], 
                             collapse = " ")
      DosesIncluded <- ifelse(DosesIncluded == "", "no dose num included", DosesIncluded)
   }
   
   HeadText2 <- case_when(
      # 1st dose and also either last or user interval
      str_detect(DosesIncluded, "Dose1 Last|Dose1 User") ~
         paste("the first and multiple", MyDoseRoute, "doses"),
      
      # only 1st dose
      DosesIncluded == "Dose1" ~
         paste(ifelse(is.na(Deets$DoseInt_sub), "a single", "the first"),
               MyDoseRoute, "dose"),
      
      DosesIncluded %in% c("Last", "User") ~
         paste("multiple", MyDoseRoute, "doses"), 
      
      DosesIncluded == "no dose num included" ~
         ifelse(is.na(Deets$DoseInt_sub), 
                paste("a single", MyDoseRoute, "dose"), 
                paste("the first and/or multiple", MyDoseRoute, "doses"))
   )
   
   HeadText3 <- ifelse(
      MyPerpetrator != "none" & 
         (MyCompoundID %in% c("inhibitor 1", "inhibitor 2",
                              "inhibitor 1 metabolite") == FALSE |
             length(setdiff(names(AllPerpetrators), MyCompoundID)) > 0),
      paste(" with or without", 
            ifelse(DoseFreq_inhib == "single dose", 
                   paste0("a single dose of ", 
                          Deets$Dose_inhib, " ", 
                          MyDoseUnits, " ", 
                          MyPerpetrator), 
                   paste0("multiple doses of ", 
                          Deets$Dose_inhib, " ", 
                          MyDoseUnits, " ", 
                          MyPerpetrator, 
                          ifelse(DoseFreq_inhib == "", 
                                 "", 
                                 paste0(" ", DoseFreq_inhib))))),
      "")
   
   Heading <- paste0("Simulated ",
                     ifelse(Observedincluded, "and observed ", ""),
                     case_match(mean_type, 
                                "geometric" ~ "geometric mean ", 
                                "arithmetic" ~ "arithmetic mean ", 
                                "arithmetic for most, geometric for ratios" ~ 
                                   "arithmetic or geometric mean "), 
                     ifelse(is.na(tissue), "", paste0(str_comma(tissue), " ")), 
                     "PK parameters for ",
                     MyCompound, " after ", HeadText2, " of ",
                     MyDose, " ", MyDoseUnits, " ", MyDosedCompound, 
                     HeadText3, " in ", 
                     tidyPop(Deets$Population)$Population, ".")
   
   
   # Return -----------------------------------------------------------
   
   if(return_all_objects){
      return(list("table_heading" = Heading, 
                  "table_caption" = Caption, 
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
      return(list("table_heading" = Heading, 
                  "table_caption" = Caption))
   }
   
}


