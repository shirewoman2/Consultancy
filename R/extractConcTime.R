#' Extract concentration-time data from a simulator output Excel file
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
#' @param adjust_obs_time TRUE or FALSE: Adjust the time listed in the observed
#'   data file to match the last dose administered? This only applies to
#'   multiple-dosing regimens. If TRUE, the graph will show the observed data
#'   overlaid with the simulated data such that the dose in the observed data
#'   was administered at the same time as the last dose in the simulated data.
#'   If FALSE, the observed data will start at whatever times are listed in the
#'   Excel file.
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated
#'   concentration-time data? Options are one or both of "aggregate" and
#'   "individual". Aggregated data are not calculated here but are pulled from
#'   the simulator output rows labeled as "mean".
#'
#' @return A data.frame of concentration-time data with the following columns:
#'   \describe{
#'
#'   \item{Compound}{the compound whose concentration is listed; this matches
#'   whatever you named your substrate or inhibitor in the simulator}
#'
#'   \item{Effector (as applicable)}{the effector of interest; this matches
#'   whatever you named "Inhibitor 1" in the simulator}
#'
#'   \item{SubjectID}{the SubjectID of the profile, which will be a number for a
#'   simulated individual or will be "obs" for observed data, "mean" for the
#'   mean data, or "per5" or "per95" for the 5th and 95th percentile data.}
#'
#'   \item{Trial}{the trial number for that set of simulations}
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
#' extractConcTime(sim_data_file = "../Example simulator output MD.xlsx")
#'
#' extractConcTime(sim_data_file = "../Example simulator output MD.xlsx",
#'                 returnAggregateOrIndiv = "individual")
#'
#' extractConcTime(sim_data_file = "../Example simulator output MD.xlsx",
#'                 obs_data_file = "../fig1-242-06-001-MD - for XML conversion.xlsx")
#'
#' extractConcTime(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'                 returnAggregateOrIndiv = c("aggregate", "individual"))
#'
#' extractConcTime(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'                 obs_data_file = "../fig1-242-06-001-MD - for XML conversion.xlsx",
#'                 obs_effector_data_file = "../Mallikaarjun_2016_RTV-fig1-100mg-BID-DLM+Kaletra - for XML conversion.xlsx",
#'                 returnAggregateOrIndiv = c("aggregate", "individual"))
#'
#'
extractConcTime <- function(sim_data_file,
                            obs_data_file = NA,
                            obs_effector_data_file = NA,
                            adjust_obs_time = TRUE,
                            returnAggregateOrIndiv = c("aggregate",
                                                       "individual")){

      # Error catching
      if(any(c(length(returnAggregateOrIndiv) < 1,
               length(returnAggregateOrIndiv) > 2,
               any(unique(returnAggregateOrIndiv) %in% c("aggregate", "individual") == FALSE)))) {
            stop("You must return one or both of 'aggregate' or 'individual' data for the parameter 'returnAggregateOrIndiv'.")
      }

      # Getting summary data for the simulation
      SimSummary <- extractExpDetails(sim_data_file)

      Compound <- SimSummary[["Substrate"]]

      # Effector present?
      EffectorPresent <- complete.cases(SimSummary[["Inhibitor"]])

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
                  pivot_longer(names_to = "SubjectID", values_to = "Conc", cols = -Time) %>%
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
                        pivot_longer(names_to = "SubjectID", values_to = "Conc", cols = -Time) %>%
                        mutate(Compound = Compound,
                               Effector = SimSummary[["Inhibitor"]])

                  StartRow_mean_Effector <- which(str_detect(sim_data_xl$...1,
                                                             "Time - Inhibitor"))[1]
                  sim_data_mean_Effector <-
                        sim_data_xl[StartRow_mean_Effector:(StartRow_mean_Effector+3), ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = "V1", mean = "V2", per5 = "V3", per95 = "V4") %>%
                        pivot_longer(names_to = "SubjectID", values_to = "Conc",
                                     cols = -Time) %>%
                        mutate(Compound = SimSummary[["Inhibitor"]],
                               Effector = SimSummary[["Inhibitor"]])


                  sim_data_mean <- bind_rows(sim_data_mean,
                                             sim_data_mean_SubPlusEffector,
                                             sim_data_mean_Effector) %>%
                        mutate(Effector = ifelse(is.na(Effector),
                                                 "none", Effector))


            }
      }

      if("individual" %in% returnAggregateOrIndiv){

            # individual data
            RowsToUse <- which(str_detect(sim_data_xl$...1, "CSys \\("))
            RowsToUse <- c(RowsToUse[1] - 1, RowsToUse)
            TimeRow <- RowsToUse[1]

            sim_data_ind <- sim_data_xl[RowsToUse, ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric) %>%
                  rename(Time = "V1")

            SubjTrial <- sim_data_xl[RowsToUse[2:length(RowsToUse)], 2:3] %>%
                  rename(SubjectID = ...2, Trial = ...3) %>%
                  mutate(SubjTrial = paste0("ID", SubjectID, "_", Trial))

            names(sim_data_ind)[2:ncol(sim_data_ind)] <- SubjTrial$SubjTrial

            sim_data_ind <- sim_data_ind %>%
                  pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                               cols = -Time) %>%
                  mutate(Compound = Compound,
                         SubjTrial = sub("ID", "", SubjTrial)) %>%
                  separate(SubjTrial, into = c("SubjectID", "Trial"),
                           sep = "_") %>%
                  mutate(across(.cols = c("SubjectID", "Trial"),
                                .fns = as.numeric))
            rm(RowsToUse)

            if(EffectorPresent){

                  # Compound conc time data in presence of effector
                  RowsToUse <- which(str_detect(sim_data_xl$...1, "CSys After Inh"))
                  RowsToUse <- c(TimeRow, RowsToUse)

                  sim_data_ind_SubPlusEffector <-
                        sim_data_xl[RowsToUse, ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = "V1")

                  names(sim_data_ind_SubPlusEffector)[
                        2:ncol(sim_data_ind_SubPlusEffector)] <- SubjTrial$SubjTrial

                  sim_data_ind_SubPlusEffector <-
                        sim_data_ind_SubPlusEffector %>%
                        pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                                     cols = -Time) %>%
                        mutate(Compound = Compound,
                               Effector = SimSummary[["Inhibitor"]],
                               SubjTrial = sub("ID", "", SubjTrial)) %>%
                        separate(SubjTrial, into = c("SubjectID", "Trial"),
                                 sep = "_") %>%
                        mutate(across(.cols = c("SubjectID", "Trial"),
                                      .fns = as.numeric))

                  rm(RowsToUse)

                  # Effector conc time data
                  RowsToUse <- which(str_detect(sim_data_xl$...1, "ISys 1 \\("))
                  RowsToUse <- c(RowsToUse[1] - 1, RowsToUse)

                  sim_data_ind_Effector <-
                        sim_data_xl[RowsToUse, ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = "V1")

                  names(sim_data_ind_Effector)[
                        2:ncol(sim_data_ind_Effector)] <- SubjTrial$SubjTrial

                  sim_data_ind_Effector <-
                        sim_data_ind_Effector %>%
                        pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                                     cols = -Time) %>%
                        mutate(Compound = SimSummary[["Inhibitor"]],
                               Effector = SimSummary[["Inhibitor"]],
                               SubjTrial = sub("ID", "", SubjTrial)) %>%
                        separate(SubjTrial, into = c("SubjectID", "Trial"),
                                 sep = "_") %>%
                        mutate(across(.cols = c("SubjectID", "Trial"),
                                      .fns = as.numeric))

                  rm(RowsToUse)

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

            if(length(StartRow_obs) != 0){

                  obs_data <- sim_data_xl[StartRow_obs:(StartRow_obs+1), ] %>% t() %>%
                        as.data.frame() %>% slice(-1) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = "V1", Conc = "V2") %>% filter(complete.cases(Time)) %>%
                        mutate(SubjectID = "obs")
            }

            TimeUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "^Time"))][1]
            TimeUnits <- ifelse(TimeUnits == "Time (h)", "Hours", "Minutes")

      } else {
            # If the user did specify an observed data file, read in observed data.
            obs_data <- extractObsConcTime(obs_data_file) %>%
                  mutate(SubjectID = "obs", Simulated = FALSE,
                         Compound = Compound)

            TimeUnits <- unique(obs_data$Time_units)

            # Converting to appropriate ObsConcUnits as necessary
            ObsConcUnits <- unique(obs_data$Conc_units)

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

      obs_data$Compound <- Compound
      obs_data$Simulated <- FALSE

      if(complete.cases(obs_effector_data_file)){

            obs_eff_data <- extractObsConcTime(obs_data_file =
                                                     obs_effector_data_file) %>%
                  mutate(Simulated = FALSE, SubjectID = "obs+effector",
                         Compound = Compound)

      }

      DosingScenario <- SimSummary[["Regimen_sub"]]

      if(adjust_obs_time & DosingScenario == "Multiple Dose"){
            # If this were a multiple-dose simulation, the observed data is,
            # presumably, at steady state. The simulated time we'd want those
            # data to match would be the *last* dose. Adjusting the time for the
            # obs data.

            DoseFreq <- SimSummary[["DoseInt_sub"]]
            NumDoses <- SimSummary[["NumDoses_sub"]]
            LastDoseTime <- DoseFreq * (NumDoses - 1)

            obs_data <- obs_data %>% mutate(Time = Time + LastDoseTime)

            if(complete.cases(obs_effector_data_file)){
                  obs_eff_data <- obs_eff_data %>% mutate(Time = Time + LastDoseTime)
            }
      }

      Data <- list()

      if("aggregate" %in% returnAggregateOrIndiv){
            Data[["agg"]] <- sim_data_mean %>%
                  mutate(Simulated = TRUE) %>%
                  arrange(SubjectID, Time)

            if(exists("sim_data_mean_Effector")){
                  Data[["sim_data_mean_Effector"]] <- sim_data_mean_Effector %>%
                        mutate(Simulated = TRUE,
                               SubjectID = as.character(SubjectID))
            }

            if(exists("sim_data_mean_SubPlusEffector")){
                  Data[["sim_data_mean_SubPlusEffector"]] <- sim_data_mean_SubPlusEffector %>%
                        mutate(Simulated = TRUE,
                               SubjectID = as.character(SubjectID))
            }

      }

      if("individual" %in% returnAggregateOrIndiv){
            Data[["indiv"]] <- sim_data_ind %>%
                  mutate(Simulated = TRUE,
                         SubjectID = as.character(SubjectID)) %>%
                  arrange(SubjectID, Time)

            if(exists("sim_data_ind_Effector")){
                  Data[["sim_data_ind_Effector"]] <- sim_data_ind_Effector %>%
                        mutate(Simulated = TRUE,
                               SubjectID = as.character(SubjectID))
            }

            if(exists("sim_data_ind_SubPlusEffector")){
                  Data[["sim_data_ind_SubPlusEffector"]] <- sim_data_ind_SubPlusEffector %>%
                        mutate(Simulated = TRUE,
                               SubjectID = as.character(SubjectID))
            }

      }

      if(exists("obs_data")){
            Data[["obs"]] <- obs_data

            if(any(c(exists("sim_data_mean_Effector"),
                     exists("sim_data_ind_Effector")))){
                  Data[["obs"]]$Effector <- "none"
            }
      }

      if(exists("obs_eff_data")){
            Data[["obs_eff"]] <- obs_eff_data
      }

      Data <- bind_rows(Data)

      if("Effector" %in% names(Data)){
            Data <- Data %>% mutate(Effector = ifelse(is.na(Effector),
                                                      "none", Effector))
            MyEffector <- unique(Data$Effector[Data$Effector != "none"])

            Data$Effector[Data$SubjectID == "obs+effector"] <- MyEffector
      }

      Data <- Data %>%
            mutate(Time_units = tolower({{TimeUnits}}),
                   Conc_units = ifelse(exists("ObsConcUnits"),
                                       ObsConcUnits, SimConcUnits),
                   SubjectID = factor(SubjectID, levels = c(
                         c("obs", "obs+effector", "mean", "per5", "per95"),
                         setdiff(unique(SubjectID),
                                 c("obs", "obs+effector", "mean", "per5", "per95"))))) %>%
            arrange(Compound, SubjectID, Time) %>%
            select(any_of(c("Compound", "Effector", "SubjectID", "Trial",
                            "Simulated", "Time", "Conc",
                            "Time_units", "Conc_units")))

      return(Data)

}
