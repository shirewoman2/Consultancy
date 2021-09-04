#' extractConcTime
#'
#' Extracts concentration-time data from simulater output Excel files and,
#' optionally, a separately specified clinical data file, and puts all data into
#' a single, tidy data.frame.
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   concentration-time data
#' @param obs_data_file name of the Excel file containing the observed
#'   concentration-time data. If the observed data you want to plot were already
#'   included in the Excel output from the simulator, leave this as NA.
#'   Otherwise, this is the file that it is ready to be converted to an XML
#'   file, not the file that contains only the digitized time and concentration
#'   data.
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated
#'   concentration-time data? Options are one or both of "aggregate" and
#'   "individual".
#'
#' @return A data.frame of concentration-time data
#'
#' @export
#'
#' @examples
#' # sim_data_file <- "../Example simulator output MD.xlsx"
#' extractConcTime("../Example simulator output MD.xlsx")
#'
extractConcTime <- function(sim_data_file,
                            obs_data_file = NA,
                            returnAggregateOrIndiv = c("aggregate",
                                                       "individual")){

      # Error catching
      if(any(length(returnAggregateOrIndiv) < 1 |
             length(returnAggregateOrIndiv) > 2 |
             unique(returnAggregateOrIndiv) %in% c("aggregate", "individual") == FALSE)) {
            stop("You must return one or both of 'aggregate' or 'individual' data for the parameter 'returnAggregateOrIndiv'.")
      }

      # Getting summary data for the simulation
      SimSummary <- suppressMessages(
            readxl::read_excel(path = sim_data_file, sheet = "Summary",
                               col_names = FALSE))

      # Reading in simulated concentration-time profile data
      sim_data_xl <- suppressMessages(
            readxl::read_excel(path = sim_data_file,
                               sheet = "Conc Profiles CSys(CPlasma)",
                               col_names = FALSE))

      if("aggregate" %in% returnAggregateOrIndiv){
            # mean data
            StartRow_mean <- which(sim_data_xl$...1 == "Population Statistics") + 1
            sim_data_mean <- sim_data_xl[StartRow_mean:(StartRow_mean+3), ] %>% t() %>%
                  as.data.frame() %>% slice(-(1:4)) %>%
                  mutate_all(as.numeric) %>%
                  rename(Time = "V1", Mean = "V2", per5 = "V3", per95 = "V4") %>%
                  pivot_longer(names_to = "ID", values_to = "Conc", cols = -Time)

      }

      if("individual" %in% returnAggregateOrIndiv){

            # individual data
            StartRow_ind <- which(sim_data_xl$...1 == "Individual Statistics") + 3
            EndRow_ind <- which(sim_data_xl$...1 == "Observed Data") - 2
            sim_data_ind <- sim_data_xl[StartRow_ind:EndRow_ind, ] %>% t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric) %>%
                  rename(Time = "V1") %>%
                  pivot_longer(names_to = "ID", values_to = "Conc", cols = -Time)

      }

      # Determining concentration units
      SimConcUnits <- str_extract(sim_data_xl[3, "...1"], "[µumnp]?g/m?L")

      # If the user did not specify a file to use for observed data, use the
      # observed data that they included for the simulation.
      if(is.na(obs_data_file)){

            StartRow_obs <- which(sim_data_xl$...1 == "Observed Data") + 1
            obs_data <- sim_data_xl[StartRow_obs:(StartRow_obs+1), ] %>% t() %>%
                  as.data.frame() %>% slice(-1) %>%
                  mutate_all(as.numeric) %>%
                  rename(Time = "V1", Conc = "V2") %>% filter(complete.cases(Time)) %>%
                  mutate(ID = "obs")

            TimeUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "^Time"))][1]
            TimeUnits <- ifelse(TimeUnits == "Time (h)", "Hours", "Minutes")

      } else {
            # If the user did specify an observed data file, read in observed data.
            obs_data_xl <- suppressMessages(
                  readxl::read_excel(path = obs_data_file, col_names = FALSE))

            obs_data <- obs_data_xl[12:nrow(obs_data_xl), 2:3] %>%
                  filter(complete.cases(...3)) %>%
                  rename(Time = ...2, Conc = ...3) %>%
                  mutate_all(as.numeric) %>%
                  mutate(ID = "obs")

            TimeUnits <- suppressMessages(
                  readxl::read_excel(path = obs_data_file,
                                     range = "A5", col_names = FALSE)) %>%
                  pull()

            # Converting to appropriate ObsConcUnits as necessary
            ObsConcUnits <- suppressMessages(
                  readxl::read_excel(path = obs_data_file,
                                     range = "D5", col_names = FALSE)) %>%
                  pull()

            if(ObsConcUnits != SimConcUnits){

                  # Starting with this table of conversion factors, which is assuredly not
                  # exhaustive. Add to this as needed.
                  ConvTable <- data.frame(ObsUnits = c("ng/mL", "ng/mL",
                                                       "ng/mL", "pg/mL",
                                                       "µg/mL"),
                                          SimUnits = c("mg/L", "µg/mL",
                                                       "ng/L", "mg/L",
                                                       "ng/mL"),
                                          Factor = c(10^3, 10^3,
                                                     10^-3, 10^6,
                                                     10^-3))

                  if(SimConcUnits %in% ConvTable$SimUnits == FALSE |
                     ObsConcUnits %in% ConvTable$ObsUnits == FALSE |
                     all(c(SimConcUnits, ObsConcUnits) %in% c("µg/mL", "ng/mL", "ng/L",
                                                              "µM", "nM", "mg", "mL",
                                                              "PD response") == FALSE)){
                        stop("Our apologies, but we have not yet set up this function to deal with your concentration units. Please tell the Consultancy Team R working group what units you're working with and we can fix this.")
                  }

                  ConversionFactor <-
                        ConvTable$Factor[which(ConvTable$SimUnits == SimConcUnits &
                                                     ConvTable$ObsUnits == ObsConcUnits)]

                  sim_data_ind <- sim_data_ind %>%
                        mutate(Conc = Conc*ConversionFactor)
                  sim_data_mean <- sim_data_mean %>%
                        mutate(Conc = Conc*ConversionFactor)
            }

      }

      # If this were a multiple-dose simulation, the observed data is, presumably,
      # at steady state. The simulated time we'd want those data to match would be
      # the *last* dose. Adjusting the time for the obs data.
      DosingScenario <- SimSummary %>%
            slice(which(...5 == "Dosing Regimen")) %>% pull(...6)

      if(DosingScenario == "Multiple Dose"){

            DoseFreq <- SimSummary %>%
                  slice(which(str_detect(...5, "Dose Interval"))) %>%
                  pull(...6) %>% as.numeric()
            NumDoses <- SimSummary %>%
                  slice(which(str_detect(...5, "Number of Doses"))) %>%
                  pull(...6) %>% as.numeric()

            LastDoseTime <- DoseFreq * (NumDoses - 1)

            obs_data <- obs_data %>% mutate(Time = Time + LastDoseTime)

      }

      Data <- list()

      if("aggregate" %in% returnAggregateOrIndiv){
            Data[["agg"]] <- sim_data_mean %>%
                  mutate(Simulated = TRUE) %>%
                  arrange(ID, Time)
      }

      if("individual" %in% returnAggregateOrIndiv){
            Data[["indiv"]] <- sim_data_ind %>%
                  mutate(Simulated = TRUE,
                         ID = as.numeric(sub("V", "", ID))) %>%
                  arrange(ID, Time) %>%
                  mutate(ID = as.character(ID))
      }

      Data[["obs"]] <- obs_data %>% mutate(Simulated = FALSE)

      Data <- bind_rows(Data) %>%
            mutate(Time_units = tolower(TimeUnits),
                   Conc_units = ifelse(exists("ObsConcUnits"),
                                       ObsConcUnits, SimConcUnits),
                   ID = factor(ID, levels = c(
                         c("obs", "Mean", "per5", "per95"),
                         setdiff(unique(ID),
                                 c("obs", "Mean", "per5", "per95"))))) %>%
            arrange(ID, Time)

      return(Data)

}
