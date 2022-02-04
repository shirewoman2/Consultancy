#' Extract observed concentration-time data from an Excel file
#'
#' Extract observed data from an Excel file that follows the Simcyp Simulator
#' template for converting concentration-time data into an XML file. Note: This
#' does not pull dosing information at this time, but we could change that if
#' there's interest.
#'
#' @param obs_data_file name of the Excel file containing the observed
#'   concentration-time data, in quotes. This is the file that it is ready to be
#'   converted to an XML file, not the file that contains only the digitized
#'   time and concentration data.
#'
#' @return a data.frame with the following columns:
#'   \describe{\item{Individual}{the individual ID}
#'
#'   \item{CompoundID}{the compound ID listed in the observed file, e.g., "Sub
#'   Plasma", "Sub PM1 Plasma", "Sub (Inb) Plasma"}
#'   
#'   \item{Tissue}{the tissue}
#'
#'   \item{Time}{time since dosing}
#'
#'   \item{Conc}{concentration}
#'
#'   \item{Time_units}{the units of measurement for the time column}
#'
#'   \item{Conc_units}{the units of measurement for the concentration column}
#'
#'   \item{Period, Age, Weight_kg, Height_cm, Sex, SerumCreatinine_umolL,
#'   HSA_gL, Haematocrit, PhenotypeCYP2D6, SmokingStatus}{the columns in the
#'   template for "Period" and "Covariates" but with R-friendly names.
#'   (Currently, no dosing information is pulled because the data format is
#'   different from what we need for other functions related to
#'   concentration-time data.)}}
#'
#' @import tidyverse
#' @import readxl
#' @export
#' @examples
#' obs_data_file = "../fig1-242-06-001-MD - for XML conversion.xlsx"
#' extractObsConcTime(obs_data_file)
#'
extractObsConcTime <- function(obs_data_file){
    
    obs_data_xl <- suppressMessages(
        readxl::read_excel(path = obs_data_file, col_names = FALSE))
    
    TimeUnits <- tolower(as.character(obs_data_xl[5, 1]))
    
    CompoundCode <- c("1" = as.character(obs_data_xl[5, 3]),
                      "2" = as.character(obs_data_xl[6, 3]),
                      "3" = as.character(obs_data_xl[7, 3]))
    
    Smoke <- c("0" = as.character(obs_data_xl[5, 7]),
               "1" = as.character(obs_data_xl[6, 7]),
               "2" = as.character(obs_data_xl[7, 7]),
               "3" = as.character(obs_data_xl[8, 7]))
    
    # Converting to appropriate ObsConcUnits as necessary
    ObsConcUnits <- c("1" = as.character(obs_data_xl[5, 4]),
                      "2" = as.character(obs_data_xl[6, 4]),
                      "3" = as.character(obs_data_xl[7, 4]))
    
    # Noting tissue
    Tissue <-
        c("Sub Plasma" = "plasma",
          "Sub Unbound Plasma" = "plasma",
          "Sub Blood" = "blood",
          "Sub PD Response" = "PD response",
          "Sub (Inb) Plasma" = "plasma",
          "Sub (Inb) Blood" = "blood",
          "Inh 1 Plasma" = "plasma",
          "Inh 1 Blood" = "blood",
          "Sub PM1 Plasma" = "plasma",
          "Sub PM1 Blood" = "blood",
          "Adipose (Sub)" = "adipose",
          "Spinal CSF (Sub)" = "CSF",
          "Organ Conc" = "solid organ",
          "Organ Conc (Inb)" = "solid organ",
          "Sub SM plasma" = "plasma",
          "Sub SM blood" = "blood",
          "Sub Urine" = "urine",
          "Inh 1 Urine" = "urine",
          "Met (Sub) Urine" = "urine",
          "Sub PM2 Plasma" = "plasma",
          "Sub PM2 Blood" = "blood",
          "Inh1 Met Plasma" = "plasma",
          "Inh1 Met Blood" = "blood",
          "Inh 2 Plasma" = "plasma",
          "Inh 2 Blood" = "blood",
          # "Inh 3 Plasma" = "plasma", # we haven't set up extractConcTime or extractExpDetails to pull inhibitor 3 yet.
          # "Inh 3 Blood" = "blood",
          "Sub (Inb) Urine" = "urine",
          "Met(Inh 1) Urine" = "urine",
          "Inh 1 PD Response" = "PD response",
          "Sub (Inb) PD Response" = "PD response",
          "ADC Plasma Free" = "plasma",
          "Conjugated Antibody Plasma Free" = "plasma",
          "Conjugated Drug Plasma Free" = "plasma",
          "PM1(Sub) PD Response" = "PD response",
          "ADC Plasma Total" = "plasma",
          "Conjugated Antibody Plasma Total" = "plasma",
          "Sub Plasma Total Drug" = "plasma",
          "Tumour Volume" = "tumour volume",
          "Tumour Volume (Inb)" = "tumour volume")
    
    ObsCompoundIDs <-
        c("Sub Plasma" = "substrate",
          "Sub Unbound Plasma" = "substrate",
          "Sub Blood" = "substrate",
          "Sub PD Response" = "Sub PD Response",
          "Sub (Inb) Plasma" = "substrate",
          "Sub (Inb) Blood" = "substrate",
          "Inh 1 Plasma" = "inhibitor 1",
          "Inh 1 Blood" = "inhibitor 1",
          "Sub PM1 Plasma" = "primary metabolite 1",
          "Sub PM1 Blood" = "primary metabolite 1",
          "Adipose (Sub)" = "substrate",
          "Spinal CSF (Sub)" = "substrate",
          "Organ Conc" = "substrate",
          "Organ Conc (Inb)" = "substrate",
          "Sub SM plasma" = "secondary metabolite",
          "Sub SM blood" = "secondary metabolite",
          "Sub Urine" = "substrate",
          "Inh 1 Urine" = "inhibitor 1",
          "Met (Sub) Urine" = "substrate",
          "Sub PM2 Plasma" = "primary metabolite 2",
          "Sub PM2 Blood" = "primary metabolite 2",
          "Inh1 Met Plasma" = "inhibitor 1 metabolite",
          "Inh1 Met Blood" = "inhibitor 1 metabolite",
          "Inh 2 Plasma" = "inhibitor 2",
          "Inh 2 Blood" = "inhibitor 2",
          # "Inh 3 Plasma" = "inhibitor 3", # we haven't set up extractConcTime or extractExpDetails to pull inhibitor 3 yet.
          # "Inh 3 Blood" = "inhibitor 3",
          "Sub (Inb) Urine" = "substrate",
          "Met(Inh 1) Urine" = "inhibitor 1 metabolite",
          "Inh 1 PD Response" = "Inh 1 PD Response",
          "Sub (Inb) PD Response" = "Sub (Inb) PD Response",
          "ADC Plasma Free" = "ADC Plasma Free",
          "Conjugated Antibody Plasma Free" = "Conjugated Antibody Plasma Free",
          "Conjugated Drug Plasma Free" = "Conjugated Drug Plasma Free",
          "PM1(Sub) PD Response" = "PM1(Sub) PD Response",
          "ADC Plasma Total" = "ADC Plasma Total",
          "Conjugated Antibody Plasma Total" = "Conjugated Antibody Plasma Total",
          "Sub Plasma Total Drug" = "Sub Plasma Total Drug",
          "Tumour Volume" = "Tumour Volume",
          "Tumour Volume (Inb)" = "Tumour Volume (Inb)")
    
    ObsEffectors <- c("Sub Plasma" = "none",
                      "Sub Unbound Plasma" = "none",
                      "Sub Blood" = "none",
                      "Sub PD Response" = "none",
                      "Sub (Inb) Plasma" = "inhibitor",
                      "Sub (Inb) Blood" = "inhibitor",
                      "Inh 1 Plasma" = "inhibitor",
                      "Inh 1 Blood" = "inhibitor",
                      "Sub PM1 Plasma" = "none",
                      "Sub PM1 Blood" = "none",
                      "Adipose (Sub)" = "none",
                      "Spinal CSF (Sub)" = "none",
                      "Organ Conc" = "none",
                      "Organ Conc (Inb)" = "inhibitor",
                      "Sub SM plasma" = "none",
                      "Sub SM blood" = "none",
                      "Sub Urine" = "none",
                      "Inh 1 Urine" = "inhibitor",
                      "Met (Sub) Urine" = "none",
                      "Sub PM2 Plasma" = "none",
                      "Sub PM2 Blood" = "none",
                      "Inh1 Met Plasma" = "inhibitor",
                      "Inh1 Met Blood" = "inhibitor",
                      "Inh 2 Plasma" = "inhibitor",
                      "Inh 2 Blood" = "inhibitor",
                      # "Inh 3 Plasma" = "inhibitor", # we haven't set up extractConcTime or extractExpDetails to pull inhibitor 3 yet.
                      # "Inh 3 Blood" = "inhibitor",
                      "Sub (Inb) Urine" = "none",
                      "Met(Inh 1) Urine" = "inhibitor",
                      "Inh 1 PD Response" = "inhibitor",
                      "Sub (Inb) PD Response" = "inhibitor",
                      "ADC Plasma Free" = "none",
                      "Conjugated Antibody Plasma Free" = "none",
                      "Conjugated Drug Plasma Free" = "none",
                      "PM1(Sub) PD Response" = "none",
                      "ADC Plasma Total" = "none",
                      "Conjugated Antibody Plasma Total" = "none",
                      "Sub Plasma Total Drug" = "none",
                      "Tumour Volume" = "none",
                      "Tumour Volume (Inb)" = "inhibitor")
    
    
    obs_data <- obs_data_xl[12:nrow(obs_data_xl), 1:ncol(obs_data_xl)]
    if(any(str_detect(t(obs_data_xl[11, ]), "Period"), na.rm = TRUE)){
        names(obs_data) <- c("Individual", "Time", "Conc", "DVID", "Weighting",
                             "Compound", "DoseRoute", "DoseUnit", "DoseAmount",
                             "InfDuration", "Period", "Age", "Weight_kg",
                             "Height_cm", "Sex", "SerumCreatinine_umolL",
                             "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                             "SmokingStatus")
    } else {
        names(obs_data) <- c("Individual", "Time", "Conc", "DVID", "Weighting",
                             "Compound", "DoseRoute", "DoseUnit", "DoseAmount",
                             "InfDuration", "Age", "Weight_kg",
                             "Height_cm", "Sex", "SerumCreatinine_umolL",
                             "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                             "SmokingStatus")
        
    }
    
    obs_data <- obs_data %>%
        filter(complete.cases(DVID)) %>%
        mutate(across(.cols = c(Time, Conc), .fns = as.numeric)) %>%
        mutate(CompoundID_obsfile = CompoundCode[as.character(DVID)],
               CompoundID = ObsCompoundIDs[CompoundID_obsfile],
               Inhibitor = ObsEffectors[CompoundID_obsfile],
               Tissue = Tissue[CompoundID_obsfile],
               File = obs_data_file,
               SmokingStatus = Smoke[SmokingStatus],
               Time_units = TimeUnits,
               Conc_units = ObsConcUnits[as.character(DVID)]) %>%
        select(any_of(c("CompoundID", 
                        # "CompoundID_obsfile",
                        "Individual",
                        "Tissue", "Inhibitor",
                        "Time", "Time_units", "Conc", "Conc_units", "DVID",
                        "File", "Weighting",
                        "Period", "Age", "Weight_kg",
                        "Height_cm", "Sex", "SerumCreatinine_umolL",
                        "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                        "SmokingStatus")))
    
    return(obs_data)
}

