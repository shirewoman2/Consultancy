#' extractConcTime
#'
#' Extracts concentration-time data from simulater output Excel files and,
#' optionally, a separately specified clinical data file, and puts all data into
#' a single, tidy data.frame. \emph{Note: Currently, only set up to extract
#' concentration-time data for one substrate and up to one inhibitor as
#' applicable.}
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
#' @return A data.frame of concentration-time data with the following columns:
#'   \describe{\item{Compound}{the compound of interest; this matches whatever
#'   you named your compound in the simulator}
#'
#'   \item{Effector (as applies)}{the effector of interest; this also matches
#'   whatever you named "Inhibitor 1" in the simulator}
#'
#'   \item{ID}{the ID of the profile, which will be a number for a simulated
#'   individual or will be "obs" for observed data, "mean" for the mean data, or
#'   "per5" or "per95" for the 5th and 95th percentile data.}
#'
#'   \item{Simulated}{TRUE or FALSE for whether the data were simulated}
#'
#'   \item{Time}{the time since the first dose}
#'
#'   \item{Conc}{concentration of the compound listed}
#'
#'   \item{Time_units}{units used for time}
#'
#'   \item{Conc_units}{units used for concentrations}}
#'
#' @export
#'
#' @examples
#' # sim_data_file <- "../Example simulator output MD.xlsx"
#' extractConcTime("../Example simulator output MD.xlsx")
#'
#' extractConcTime("../Example simulator output MD.xlsx",
#'                 returnAggregateOrIndiv = "individual")
#'
#' extractConcTime("../Example simulator output MD.xlsx",
#'                 obs_data_file = "../fig1-242-06-001-MD - for XML conversion.xlsx")
#'
#'
#'
extractConcTime <- function(sim_data_file,
                            obs_data_file = NA,
                            returnAggregateOrIndiv = c("aggregate",
                                                       "individual")){

      # Error catching
      if(any(c(length(returnAggregateOrIndiv) < 1,
               length(returnAggregateOrIndiv) > 2,
               any(unique(returnAggregateOrIndiv) %in% c("aggregate", "individual") == FALSE)))) {
            stop("You must return one or both of 'aggregate' or 'individual' data for the parameter 'returnAggregateOrIndiv'.")
      }

      # Getting summary data for the simulation
      SimSummary <- suppressMessages(
            readxl::read_excel(path = sim_data_file, sheet = "Summary",
                               col_names = FALSE))

      Compound <- SimSummary$...2[which(str_detect(SimSummary$...1, "^Compound Name"))]

      # Effector present?
      EffectorRow <- which(str_detect(SimSummary$...1, "Inhibitor"))
      Inhibitor <- SimSummary$...2[EffectorRow]
      EffectorPresent <- length(Inhibitor) > 0

      # Reading in simulated concentration-time profile data
      sim_data_xl <- suppressMessages(
            readxl::read_excel(path = sim_data_file,
                               sheet = "Conc Profiles CSys(CPlasma)",
                               col_names = FALSE))

      if("aggregate" %in% returnAggregateOrIndiv){
            # mean data
            StartRow_mean <- which(sim_data_xl$...1 == "Population Statistics") + 1
            sim_data_mean <- sim_data_xl[StartRow_mean:(StartRow_mean+3), ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric) %>%
                  rename(Time = "V1", mean = "V2", per5 = "V3", per95 = "V4") %>%
                  pivot_longer(names_to = "ID", values_to = "Conc", cols = -Time) %>%
                  mutate(Compound = Compound)

            if(EffectorPresent){
                  StartRow_mean_SubPlusEffector <-
                        which(str_detect(sim_data_xl$...1, "interaction"))
                  sim_data_mean_SubPlusEffector <-
                        sim_data_xl[c(StartRow_mean,
                                      StartRow_mean_SubPlusEffector:(StartRow_mean_SubPlusEffector+2)), ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = "V1", mean = "V2", per5 = "V3", per95 = "V4") %>%
                        pivot_longer(names_to = "ID", values_to = "Conc", cols = -Time) %>%
                        mutate(Compound = Compound,
                               Effector = Inhibitor)

                  StartRow_mean_Effector <- which(str_detect(sim_data_xl$...1,
                                                             "Time - Inhibitor"))[1]
                  sim_data_mean_Effector <-
                        sim_data_xl[StartRow_mean_Effector:(StartRow_mean_Effector+3), ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = "V1", mean = "V2", per5 = "V3", per95 = "V4") %>%
                        pivot_longer(names_to = "ID", values_to = "Conc", cols = -Time) %>%
                        mutate(Compound = Inhibitor, Effector = Inhibitor)


                  sim_data_mean <- bind_rows(sim_data_mean,
                                             sim_data_mean_SubPlusEffector,
                                             sim_data_mean_Effector) %>%
                        mutate(Effector = ifelse(is.na(Effector),
                                                 "none", Effector))


            }
      }

      if("individual" %in% returnAggregateOrIndiv){

            # individual data
            sim_data_ind <- sim_data_xl[which(str_detect(sim_data_xl$...1, "CSys \\(")), ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric) %>%
                  rename(Time = "V1") %>%
                  pivot_longer(names_to = "ID", values_to = "Conc", cols = -Time) %>%
                  mutate(Compound = Compound)

            if(EffectorPresent){
                  sim_data_ind_SubPlusEffector <-
                        sim_data_xl[which(str_detect(sim_data_xl$...1, "CSys After Inh")), ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = "V1") %>%
                        pivot_longer(names_to = "ID", values_to = "Conc",
                                     cols = -Time) %>%
                        mutate(Compound = Compound,
                               Effector = Inhibitor)

                  sim_data_ind_Effector <-
                        sim_data_xl[which(str_detect(sim_data_xl$...1, "ISys 1 \\(")), ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = "V1") %>%
                        pivot_longer(names_to = "ID", values_to = "Conc",
                                     cols = -Time) %>%
                        mutate(Compound = Inhibitor,
                               Effector = Inhibitor)

                  sim_data_ind <- bind_rows(sim_data_ind,
                                            sim_data_ind_SubPlusEffector,
                                            sim_data_ind_Effector)  %>%
                        mutate(Effector = ifelse(is.na(Effector),
                                                 "none", Effector))

            }
      }

      # Determining concentration units
      SimConcUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "CSys Mean \\("))]
      SimConcUnits <- str_extract(SimConcUnits, "[µumnp]?g/m?L")

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

      Data[["obs"]] <- obs_data %>% mutate(Simulated = FALSE,
                                           Compound = Compound)

      Data <- bind_rows(Data) %>%
            mutate(Time_units = tolower(TimeUnits),
                   Conc_units = ifelse(exists("ObsConcUnits"),
                                       ObsConcUnits, SimConcUnits),
                   ID = factor(ID, levels = c(
                         c("obs", "mean", "per5", "per95"),
                         setdiff(unique(ID),
                                 c("obs", "mean", "per5", "per95"))))) %>%
            arrange(Compound, ID, Time) %>%
            select(any_of(c("Compound", "Effector", "ID", "Simulated",
                            "Time", "Conc", "Time_units", "Conc_units")))

      return(Data)

}
