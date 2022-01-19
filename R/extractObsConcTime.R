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
#' @return a data.frame with the following columns: \describe{
#'
#'   \item{Individual}{the individual ID for the data point}
#'
#'   \item{CompoundID}{the compound ID listed in the observed file, e.g., "Sub
#'   Plasma", "Sub PM1 Plasma", "Sub (Inb) Plasma"}
#'
#'   \item{Time}{time since dosing}
#'
#'   \item{Conc}{concentration}
#'
#'   \item{TimeUnits}{the units of measurement for the time column}
#'
#'   \item{ConcUnits}{the units of measurement for the concentration column}
#'
#'   \item{the columns in the template for "Period" and "Covariates".
#'   (Currently, no dosing information is pulled, but we could change that.)}}
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

      ObsCompoundID <- c("1" = as.character(obs_data_xl[5, 3]),
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

      obs_data <- obs_data_xl[12:nrow(obs_data_xl), 1:20]
      names(obs_data) <- c("Individual", "Time", "Conc", "DVID", "Weighting",
                           "Compound", "DoseRoute", "DoseUnit", "DoseAmount",
                           "InfDuration", "Period", "Age", "Weight_kg",
                           "Height_cm", "Sex", "SerumCreatinine_umolL",
                           "HSA_gL", "Haematocrit", "PhenotypeCYP2D6",
                           "SmokingStatus")

      obs_data <- obs_data %>%
            filter(complete.cases(DVID)) %>%
            mutate(across(.cols = c(Time, Conc), .fns = as.numeric)) %>%
            mutate(CompoundID = ObsCompoundID[as.character(DVID)],
                   SmokingStatus = Smoke[SmokingStatus],
                  Time_units = TimeUnits,
                   Conc_units = ObsConcUnits[as.character(DVID)]) %>%
            select(Individual, CompoundID, Time, Conc,
                   Time_units, Conc_units, Period:SmokingStatus)

}

