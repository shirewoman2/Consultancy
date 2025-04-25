#' INTERNAL - Harmonize inputs for demog_x functions
#'
#' @param demog_dataframe data.frame of demographic data
#' @param demog_parameters which demographic parameters user wants
#'
#' @returns list of harmonized demog_dataframe and demog_parameters
harmonize_demog <- function(demog_dataframe, 
                            demog_parameters, 
                            table_or_graph = "graph"){
   
   PossDemogParams <- tibble(
      Parameter = c("Age", 
                    "AGP_gL",
                    "AllometricScalar", 
                    "BMI_kgm2", 
                    "BSA_m2", 
                    "BrainWt_g", 
                    "CardiacOut", 
                    "Creatinine_umolL", 
                    "GFR_mLminm2",
                    "GFR_mLmin", 
                    "Haematocrit", 
                    "Height_cm", 
                    "Height vs Age", 
                    "HSA_gL",
                    "KidneyWt_g", 
                    "LiverWt_g", 
                    "Weight_kg",
                    "Weight vs Age", 
                    "Weight vs Height",
                    "Sex", 
                    "Sex vs Age", 
                    "RenalFunction"), 
      LowerCase = tolower(Parameter), 
      Label = c("Age (years)", 
                "AGP (g/L)", 
                "Allometric scalar", 
                "BMI (kg/m2)", 
                "Body surface area (m2)", 
                "Brain weight (g)", 
                "Cardiac output (L/h)", 
                "Creatinine (uM)", 
                "Glomerular filtration rate\n(mL/min/m2 body surface area)", 
                "Glomerular filtration rate\n(mL/min)", 
                "Haematocrit (%)", 
                "Height (cm)", 
                NA, 
                "Human serum albumin (g/L)", 
                "Kidney weight (g)", 
                "Liver weight (g)", 
                "Weight (kg)", 
                NA,
                NA, 
                "Percent female", 
                NA, 
                "renalfunction" = "Renal function"))
   
   if(table_or_graph == "table"){
      PossDemogParams <- PossDemogParams %>% 
         filter(!str_detect(Parameter, " vs "))
   }
   
   # Checking for which kind of GFR was included
   Omit <- setdiff(c("gfr_mlminm2", "gfr_mlmin"), 
                   names(demog_dataframe))
   PossDemogParams <- PossDemogParams %>% 
      filter(!LowerCase %in% Omit)
   rm(Omit)
   
   # Returning to error catching ---------------------------------------------
   
   # Noting original requests for parameters
   DemogParams <- tibble(Orig = demog_parameters) %>% 
   # Addressing any issues w/case and periods for "vs"
      mutate(Parameter = tolower(gsub("\\.", "", as.character(Orig))))
   
   DemogParams <- DemogParams %>% 
      mutate(Parameter = case_match(Parameter, 
                                    "height vs weight" ~ "weight vs height", 
                                    "age vs height" ~ "height vs age", 
                                    "age vs weight" ~ "weight vs age", 
                                    "age vs sex" ~ "sex vs age", 
                                    "weight" ~ "weight_kg",
                                    "height" ~ "height_cm",
                                    "hsa" ~ "hsa_gl",
                                    "agp" ~ "agp_gl",
                                    "bmi" ~ "bmi_kgm2",
                                    "bsa" ~ "bsa_m2", 
                                    "brain" ~ "brainwt_g", 
                                    "brainwt" ~ "brainwt_g", 
                                    "cardiac" ~ "cardiacout", 
                                    "creatinine" ~ "creatinine_umoll", 
                                    "creatinine_um"  ~ "creatinine_umoll", 
                                    "gfr" ~ "gfr_mlminm2", 
                                    "hematocrit" ~ "haematocrit", 
                                    "kidney" ~ "kidneywt_g", 
                                    "kidneywt" ~ "kidneywt_g", 
                                    "liverwt" ~ "liverwt_g", 
                                    "liver" ~ "liverwt_g", 
                                    .default = Parameter), 
             Parameter = case_when("gfr_mlmin" %in% names(demog_dataframe) & 
                                      Parameter == "gfr_mlminm2" ~ "gfr_mlmin", 
                                   .default = Parameter))
   
   BadVar <- setdiff(DemogParams$Parameter, 
                     tolower(PossDemogParams$Parameter))
   
   if(length(BadVar) > 0 && any(complete.cases(BadVar))){
      warning(wrapn(paste0(
         "The demographic parameter(s) ", 
         str_comma(paste0("'", 
                          DemogParams$Orig[DemogParams$Parameter %in% BadVar], 
                          "'")), 
         " is/are not among the possible options for demog_parameters, so they won't be included. Please check the help file for options.")), 
         call. = FALSE)
      
      DemogParams <- DemogParams %>% filter(!Parameter %in% BadVar)
   }
   
   if("sex" %in% names(demog_dataframe) == FALSE){
      demog_dataframe$sex <- "unknown"
   }
   
   if(all(is.na(DemogParams$Parameter)) && 
      nrow(DemogParams) > 0){
      DemogParams <- 
         tibble(Orig = as.character(NA), 
                Parameter = tolower(PossDemogParams$Parameter))
   } 
   
   return(list(DemogParams = DemogParams, 
               PossDemogParams = PossDemogParams))
   
}


