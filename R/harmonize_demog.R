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
                    "RenalFunction", 
                    
                    # all non-Demographics tab parameters
                    "HPGL_organ_scalar", "MPPGL_organ_scalar", 
                    "CPPGL_organ_scalar", "S9PPGL_organ_scalar", "MPPI_organ_scalar", 
                    "CPPGI_organ_scalar", "S9PPI_organ_scalar", "MPPC_organ_scalar", 
                    "PTCPGK_organ_scalar", "MPPGK_organ_scalar", "CPPGK_organ_scalar", 
                    "S9PPGK_organ_scalar", "MPPLu_organ_scalar", "CYP1A1_abundance_pmol", 
                    "CYP1A1_phenotype", "CYP1A2_abundance_pmol", "CYP1A2_phenotype", 
                    "CYP2A6_abundance_pmol", "CYP2A6_phenotype", "CYP2B6_abundance_pmol", 
                    "CYP2B6_phenotype", "CYP2C8_abundance_pmol", "CYP2C8_phenotype", 
                    "CYP2C9_abundance_pmol", "CYP2C9_phenotype", "CYP2C18_abundance_pmol", 
                    "CYP2C18_phenotype", "CYP2C19_abundance_pmol", "CYP2C19_phenotype", 
                    "CYP2D6_abundance_pmol", "CYP2D6_phenotype", "CYP2E1_abundance_pmol", 
                    "CYP2E1_phenotype", "CYP2J2_abundance_pmol", "CYP2J2_phenotype", 
                    "CYP3A4_abundance_pmol", "CYP3A4_phenotype", "CYP3A5_abundance_pmol", 
                    "CYP3A5_phenotype", "CYP3A7_abundance_pmol", "CYP3A7_phenotype", 
                    "CYP1A1_abundance_pmol_mg", "CYP1A2_abundance_pmol_mg", "CYP2A6_abundance_pmol_mg", 
                    "CYP2B6_abundance_pmol_mg", "CYP2C8_abundance_pmol_mg", "CYP2C9_abundance_pmol_mg", 
                    "CYP2C18_abundance_pmol_mg", "CYP2C19_abundance_pmol_mg", "CYP2D6_abundance_pmol_mg", 
                    "CYP2E1_abundance_pmol_mg", "CYP2J2_abundance_pmol_mg", "CYP3A4_abundance_pmol_mg", 
                    "CYP3A5_abundance_pmol_mg", "CYP3A7_abundance_pmol_mg", "Small_Intestinal_CYP2C9_abundance_pmol", 
                    "Small_Intestinal_CYP2C19_abundance_pmol", "Small_Intestinal_CYP2D6_abundance_pmol", 
                    "Small_Intestinal_CYP2J2_abundance_pmol", "Small_Intestinal_CYP3A4_abundance_pmol", 
                    "Small_Intestinal_CYP3A5_abundance_pmol", "Colon_CYP2C9_abundance_pmol", 
                    "Colon_CYP2C19_abundance_pmol", "Colon_CYP2D6_abundance_pmol", 
                    "Colon_CYP2J2_abundance_pmol", "Colon_CYP3A4_abundance_pmol", 
                    "Colon_CYP3A5_abundance_pmol", "Lung_CYP1A1_abundance_pmol", 
                    "Liver_UGT1A1_abundance_pmol", "Liver_UGT1A3_abundance_pmol", 
                    "Liver_UGT1A4_abundance_pmol", "Liver_UGT1A5_abundance_pmol", 
                    "Liver_UGT1A6_abundance_pmol", "Liver_UGT1A7_abundance_pmol", 
                    "Liver_UGT1A8_abundance_pmol", "Liver_UGT1A9_abundance_pmol", 
                    "Liver_UGT1A10_abundance_pmol", "Liver_UGT2B4_abundance_pmol", 
                    "Liver_UGT2B7_abundance_pmol", "Liver_UGT2B10_abundance_pmol", 
                    "Liver_UGT2B11_abundance_pmol", "Liver_UGT2B15_abundance_pmol", 
                    "Liver_UGT2B17_abundance_pmol", "Liver_UGT2B28_abundance_pmol", 
                    "Liver_User_UGT1_abundance_pmol", "Liver_UGT1A1_rel_abundance", 
                    "Liver_UGT1A1_phenotype", "Liver_UGT1A3_rel_abundance", "Liver_UGT1A3_phenotype", 
                    "Liver_UGT1A4_rel_abundance", "Liver_UGT1A4_phenotype", "Liver_UGT1A5_rel_abundance", 
                    "Liver_UGT1A5_phenotype", "Liver_UGT1A6_rel_abundance", "Liver_UGT1A6_phenotype", 
                    "Liver_UGT1A7_rel_abundance", "Liver_UGT1A7_phenotype", "Liver_UGT1A8_rel_abundance", 
                    "Liver_UGT1A8_phenotype", "Liver_UGT1A9_rel_abundance", "Liver_UGT1A9_phenotype", 
                    "Liver_UGT1A10_rel_abundance", "Liver_UGT1A10_phenotype", "Liver_UGT2B4_rel_abundance", 
                    "Liver_UGT2B4_phenotype", "Liver_UGT2B7_rel_abundance", "Liver_UGT2B7_phenotype", 
                    "Liver_UGT2B10_rel_abundance", "Liver_UGT2B10_phenotype", "Liver_UGT2B11_rel_abundance", 
                    "Liver_UGT2B11_phenotype", "Liver_UGT2B15_rel_abundance", "Liver_UGT2B15_phenotype", 
                    "Liver_UGT2B17_rel_abundance", "Liver_UGT2B17_phenotype", "Liver_UGT2B28_rel_abundance", 
                    "Liver_UGT2B28_phenotype", "Liver_User_UGT1_rel_abundance", "Liver_User_UGT1_phenotype", 
                    "Gut_UGT1A1_abundance_pmol", "Gut_UGT1A3_abundance_pmol", "Gut_UGT1A4_abundance_pmol", 
                    "Gut_UGT1A5_abundance_pmol", "Gut_UGT1A6_abundance_pmol", "Gut_UGT1A7_abundance_pmol", 
                    "Gut_UGT1A8_abundance_pmol", "Gut_UGT1A9_abundance_pmol", "Gut_UGT1A10_abundance_pmol", 
                    "Gut_UGT2B4_abundance_pmol", "Gut_UGT2B7_abundance_pmol", "Gut_UGT2B10_abundance_pmol", 
                    "Gut_UGT2B11_abundance_pmol", "Gut_UGT2B15_abundance_pmol", "Gut_UGT2B17_abundance_pmol", 
                    "Gut_UGT2B28_abundance_pmol", "Gut_User_UGT1_abundance_pmol", 
                    "Gut_UGT1A1_rel_abundance", "Gut_UGT1A3_rel_abundance", "Gut_UGT1A4_rel_abundance", 
                    "Gut_UGT1A5_rel_abundance", "Gut_UGT1A6_rel_abundance", "Gut_UGT1A7_rel_abundance", 
                    "Gut_UGT1A8_rel_abundance", "Gut_UGT1A9_rel_abundance", "Gut_UGT1A10_rel_abundance", 
                    "Gut_UGT2B4_rel_abundance", "Gut_UGT2B7_rel_abundance", "Gut_UGT2B10_rel_abundance", 
                    "Gut_UGT2B11_rel_abundance", "Gut_UGT2B15_rel_abundance", "Gut_UGT2B17_rel_abundance", 
                    "Gut_UGT2B28_rel_abundance", "Gut_User_UGT1_rel_abundance", "Kidney_UGT1A1_abundance_pmol", 
                    "Kidney_UGT1A3_abundance_pmol", "Kidney_UGT1A4_abundance_pmol", 
                    "Kidney_UGT1A5_abundance_pmol", "Kidney_UGT1A6_abundance_pmol", 
                    "Kidney_UGT1A7_abundance_pmol", "Kidney_UGT1A8_abundance_pmol", 
                    "Kidney_UGT1A9_abundance_pmol", "Kidney_UGT1A10_abundance_pmol", 
                    "Kidney_UGT2B4_abundance_pmol", "Kidney_UGT2B7_abundance_pmol", 
                    "Kidney_UGT2B10_abundance_pmol", "Kidney_UGT2B11_abundance_pmol", 
                    "Kidney_UGT2B15_abundance_pmol", "Kidney_UGT2B17_abundance_pmol", 
                    "Kidney_UGT2B28_abundance_pmol", "Kidney_User_UGT1_abundance_pmol", 
                    "Kidney_UGT1A1_rel_abundance", "Kidney_UGT1A3_rel_abundance", 
                    "Kidney_UGT1A4_rel_abundance", "Kidney_UGT1A5_rel_abundance", 
                    "Kidney_UGT1A6_rel_abundance", "Kidney_UGT1A7_rel_abundance", 
                    "Kidney_UGT1A8_rel_abundance", "Kidney_UGT1A9_rel_abundance", 
                    "Kidney_UGT1A10_rel_abundance", "Kidney_UGT2B4_rel_abundance", 
                    "Kidney_UGT2B7_rel_abundance", "Kidney_UGT2B10_rel_abundance", 
                    "Kidney_UGT2B11_rel_abundance", "Kidney_UGT2B15_rel_abundance", 
                    "Kidney_UGT2B17_rel_abundance", "Kidney_UGT2B28_rel_abundance", 
                    "Kidney_User_UGT1_rel_abundance", "BP_sub", "fu_sub", "PercBoundLPP_sub", 
                    "Qgut_sub", "Dose_mg_sub"), 
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
                "Renal function", 
                
                "HPGL (organ scalar)", "MPPGL (organ scalar)", 
                "CPPGL (organ scalar)", "S9PPGL (organ scalar)", "MPPI (organ scalar)", 
                "CPPGI (organ scalar)", "S9PPI (organ scalar)", "MPPC (organ scalar)", 
                "PTCPGK (organ scalar)", "MPPGK (organ scalar)", "CPPGK (organ scalar)", 
                "S9PPGK (organ scalar)", "MPPLu (organ scalar)", "CYP1A1 abundance (pmol)", 
                "CYP1A1 phenotype", "CYP1A2 abundance (pmol)", "CYP1A2 phenotype", 
                "CYP2A6 abundance (pmol)", "CYP2A6 phenotype", "CYP2B6 abundance (pmol)", 
                "CYP2B6 phenotype", "CYP2C8 abundance (pmol)", "CYP2C8 phenotype", 
                "CYP2C9 abundance (pmol)", "CYP2C9 phenotype", "CYP2C18 abundance (pmol)", 
                "CYP2C18 phenotype", "CYP2C19 abundance (pmol)", "CYP2C19 phenotype", 
                "CYP2D6 abundance (pmol)", "CYP2D6 phenotype", "CYP2E1 abundance (pmol)", 
                "CYP2E1 phenotype", "CYP2J2 abundance (pmol)", "CYP2J2 phenotype", 
                "CYP3A4 abundance (pmol)", "CYP3A4 phenotype", "CYP3A5 abundance (pmol)", 
                "CYP3A5 phenotype", "CYP3A7 abundance (pmol)", "CYP3A7 phenotype", 
                "CYP1A1 abundance (pmol/mg)", "CYP1A2 abundance (pmol/mg)", "CYP2A6 abundance (pmol/mg)", 
                "CYP2B6 abundance (pmol/mg)", "CYP2C8 abundance (pmol/mg)", "CYP2C9 abundance (pmol/mg)", 
                "CYP2C18 abundance (pmol/mg)", "CYP2C19 abundance (pmol/mg)", "CYP2D6 abundance (pmol/mg)", 
                "CYP2E1 abundance (pmol/mg)", "CYP2J2 abundance (pmol/mg)", "CYP3A4 abundance (pmol/mg)", 
                "CYP3A5 abundance (pmol/mg)", "CYP3A7 abundance (pmol/mg)", "Small Intestinal CYP2C9 abundance (pmol)", 
                "Small Intestinal CYP2C19 abundance (pmol)", "Small Intestinal CYP2D6 abundance (pmol)", 
                "Small Intestinal CYP2J2 abundance (pmol)", "Small Intestinal CYP3A4 abundance (pmol)", 
                "Small Intestinal CYP3A5 abundance (pmol)", "Colon CYP2C9 abundance (pmol)", 
                "Colon CYP2C19 abundance (pmol)", "Colon CYP2D6 abundance (pmol)", 
                "Colon CYP2J2 abundance (pmol)", "Colon CYP3A4 abundance (pmol)", 
                "Colon CYP3A5 abundance (pmol)", "Lung CYP1A1 abundance (pmol)", 
                "Liver UGT1A1 abundance (pmol)", "Liver UGT1A3 abundance (pmol)", 
                "Liver UGT1A4 abundance (pmol)", "Liver UGT1A5 abundance (pmol)", 
                "Liver UGT1A6 abundance (pmol)", "Liver UGT1A7 abundance (pmol)", 
                "Liver UGT1A8 abundance (pmol)", "Liver UGT1A9 abundance (pmol)", 
                "Liver UGT1A10 abundance (pmol)", "Liver UGT2B4 abundance (pmol)", 
                "Liver UGT2B7 abundance (pmol)", "Liver UGT2B10 abundance (pmol)", 
                "Liver UGT2B11 abundance (pmol)", "Liver UGT2B15 abundance (pmol)", 
                "Liver UGT2B17 abundance (pmol)", "Liver UGT2B28 abundance (pmol)", 
                "Liver User UGT1 abundance (pmol)", "Liver UGT1A1 relative abundance", 
                "Liver UGT1A1 phenotype", "Liver UGT1A3 relative abundance", "Liver UGT1A3 phenotype", 
                "Liver UGT1A4 relative abundance", "Liver UGT1A4 phenotype", "Liver UGT1A5 relative abundance", 
                "Liver UGT1A5 phenotype", "Liver UGT1A6 relative abundance", "Liver UGT1A6 phenotype", 
                "Liver UGT1A7 relative abundance", "Liver UGT1A7 phenotype", "Liver UGT1A8 relative abundance", 
                "Liver UGT1A8 phenotype", "Liver UGT1A9 relative abundance", "Liver UGT1A9 phenotype", 
                "Liver UGT1A10 relative abundance", "Liver UGT1A10 phenotype", "Liver UGT2B4 relative abundance", 
                "Liver UGT2B4 phenotype", "Liver UGT2B7 relative abundance", "Liver UGT2B7 phenotype", 
                "Liver UGT2B10 relative abundance", "Liver UGT2B10 phenotype", "Liver UGT2B11 relative abundance", 
                "Liver UGT2B11 phenotype", "Liver UGT2B15 relative abundance", "Liver UGT2B15 phenotype", 
                "Liver UGT2B17 relative abundance", "Liver UGT2B17 phenotype", "Liver UGT2B28 relative abundance", 
                "Liver UGT2B28 phenotype", "Liver User UGT1 relative abundance", "Liver User UGT1 phenotype", 
                "Gut UGT1A1 abundance (pmol)", "Gut UGT1A3 abundance (pmol)", "Gut UGT1A4 abundance (pmol)", 
                "Gut UGT1A5 abundance (pmol)", "Gut UGT1A6 abundance (pmol)", "Gut UGT1A7 abundance (pmol)", 
                "Gut UGT1A8 abundance (pmol)", "Gut UGT1A9 abundance (pmol)", "Gut UGT1A10 abundance (pmol)", 
                "Gut UGT2B4 abundance (pmol)", "Gut UGT2B7 abundance (pmol)", "Gut UGT2B10 abundance (pmol)", 
                "Gut UGT2B11 abundance (pmol)", "Gut UGT2B15 abundance (pmol)", "Gut UGT2B17 abundance (pmol)", 
                "Gut UGT2B28 abundance (pmol)", "Gut User UGT1 abundance (pmol)", 
                "Gut UGT1A1 relative abundance", "Gut UGT1A3 relative abundance", "Gut UGT1A4 relative abundance", 
                "Gut UGT1A5 relative abundance", "Gut UGT1A6 relative abundance", "Gut UGT1A7 relative abundance", 
                "Gut UGT1A8 relative abundance", "Gut UGT1A9 relative abundance", "Gut UGT1A10 relative abundance", 
                "Gut UGT2B4 relative abundance", "Gut UGT2B7 relative abundance", "Gut UGT2B10 relative abundance", 
                "Gut UGT2B11 relative abundance", "Gut UGT2B15 relative abundance", "Gut UGT2B17 relative abundance", 
                "Gut UGT2B28 relative abundance", "Gut User UGT1 relative abundance", "Kidney UGT1A1 abundance (pmol)", 
                "Kidney UGT1A3 abundance (pmol)", "Kidney UGT1A4 abundance (pmol)", 
                "Kidney UGT1A5 abundance (pmol)", "Kidney UGT1A6 abundance (pmol)", 
                "Kidney UGT1A7 abundance (pmol)", "Kidney UGT1A8 abundance (pmol)", 
                "Kidney UGT1A9 abundance (pmol)", "Kidney UGT1A10 abundance (pmol)", 
                "Kidney UGT2B4 abundance (pmol)", "Kidney UGT2B7 abundance (pmol)", 
                "Kidney UGT2B10 abundance (pmol)", "Kidney UGT2B11 abundance (pmol)", 
                "Kidney UGT2B15 abundance (pmol)", "Kidney UGT2B17 abundance (pmol)", 
                "Kidney UGT2B28 abundance (pmol)", "Kidney User UGT1 abundance (pmol)", 
                "Kidney UGT1A1 relative abundance", "Kidney UGT1A3 relative abundance", 
                "Kidney UGT1A4 relative abundance", "Kidney UGT1A5 relative abundance", 
                "Kidney UGT1A6 relative abundance", "Kidney UGT1A7 relative abundance", 
                "Kidney UGT1A8 relative abundance", "Kidney UGT1A9 relative abundance", 
                "Kidney UGT1A10 relative abundance", "Kidney UGT2B4 relative abundance", 
                "Kidney UGT2B7 relative abundance", "Kidney UGT2B10 relative abundance", 
                "Kidney UGT2B11 relative abundance", "Kidney UGT2B15 relative abundance", 
                "Kidney UGT2B17 relative abundance", "Kidney UGT2B28 relative abundance", 
                "Kidney User UGT1 relative abundance", "B/P", "fu,p", "Percent bound LPP", 
                "Qgut (L/h)", "Dose (mg)"))
   
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
                                    "gfr" ~ "gfr_mlmin", 
                                    "gfr_mlmin1.73m2" ~ "gfr_mlminm2", 
                                    "hematocrit" ~ "haematocrit", 
                                    "kidney" ~ "kidneywt_g", 
                                    "kidneywt" ~ "kidneywt_g", 
                                    "liverwt" ~ "liverwt_g", 
                                    "liver" ~ "liverwt_g", 
                                    "renal function" ~ "renalfunction", 
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


