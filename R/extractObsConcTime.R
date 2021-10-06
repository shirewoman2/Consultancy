#' Extract observed concentration-time data from an Excel file
#'
#' Extract observed data from an Excel file that follows the Simcyp Simulator
#' template for converting concentration-time data into an XML file.
#' \emph{Important caveat:} This function has only been set up for really simple
#' files with only a single substrate concentration in plasma.
#'
#' @param obs_data_file name of the Excel file containing the observed
#'   concentration-time data, in quotes. This is the file that it is ready to be
#'   converted to an XML file, not the file that contains only the digitized
#'   time and concentration data.
#'
#' @return a data.frame with the following columns:
#'   \describe{\item{Individual}{the individual ID for the data point}
#'   \item{Time}{time since dosing} \item{Conc}{concentration}
#'   \item{TimeUnits}{the units of measurement for the time column}
#'   \item{ConcUnits}{the units of measurement for the concentration column}}
#'
#'
#' @export
#'
#' @examples
#' obs_data_file = "../fig1-242-06-001-MD - for XML conversion.xlsx"
#' extractObsConcTime(obs_data_file)
#'
extractObsConcTime <- function(obs_data_file){
      obs_data_xl <- suppressMessages(
            readxl::read_excel(path = obs_data_file, col_names = FALSE))

      TimeUnits <- tolower(as.character(obs_data_xl[5, 1]))

      # Converting to appropriate ObsConcUnits as necessary
      ObsConcUnits <- as.character(obs_data_xl[5, 4])

      obs_data <- obs_data_xl[12:nrow(obs_data_xl), 1:3] %>%
            filter(complete.cases(...3)) %>%
            rename(Individual = ...1, Time = ...2, Conc = ...3) %>%
            mutate(across(.cols = c(Time, Conc), .fns = as.numeric)) %>%
            mutate(Time_units = TimeUnits,
                   Conc_units = ObsConcUnits)
}

