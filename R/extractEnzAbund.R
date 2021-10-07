#' Extract enzyme abundance data from a simulator output Excel file
#'
#' Extracts enzyme abundance data from a simulator output Excel file. The
#' appropriate tab must be present in the output file.
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   enzyme-abundance-time data
#' @param enzyme the enzyme, e.g., "CYP3A4", "UGT1A1", etc. Spaces or hyphens in
#'   enzyme names will be ignored. Not case sensitive.
#' @param tissue From which tissue should the desired enzyme abundance be
#'   extracted? Options are "liver" (default), "gut", or "kidney".
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated
#'   enzyme abundance data? Options are one or both of "aggregate" and
#'   "individual". Aggregated data are not calculated here but are pulled from
#'   the simulator output rows labeled as "mean".
#'
#' @return A data.frame of enzyme abundance with time with the following
#'   columns: \describe{
#'
#'   \item{Enzyme}{the enzyme whose abundance is listed}
#'
#'   \item{Tissue}{the tissue}
#'
#'   \item{EffectorPresent}{TRUE or FALSE for whether the inhibitor was present
#'   at that time point}
#'
#'   \item{Time}{the time since the first dose}
#'
#'   \item{Abundance}{abundance of the enzyme listed}
#'
#'   \item{Time_units}{units used for time}
#' }
#'
#' @export
#'
#' @examples
#' extractEnzAbund(sim_data_file = "../Example simulator output MD.xlsx",
#'                 enzyme = "CYP3A4", tissue = "liver")



extractEnzAbund <- function(sim_data_file,
                            enzyme = "CYP3A4",
                            tissue = "liver",
                            returnAggregateOrIndiv = c("aggregate",
                                                       "individual")){

      # Error catching
      if(any(c(length(returnAggregateOrIndiv) < 1,
               length(returnAggregateOrIndiv) > 2,
               any(unique(returnAggregateOrIndiv) %in% c("aggregate", "individual") == FALSE)))) {
            stop("You must return one or both of 'aggregate' or 'individual' data for the parameter 'returnAggregateOrIndiv'.")
      }

      if(length(tissue) != 1){
            stop("You must enter one and only one tissue option. (Default is liver.)")
      }

      tissue <- tolower(tissue)
      enzyme <- gsub(" |_|-", "", toupper(enzyme))

      if(tissue %in% c("liver", "gut", "kidney") == FALSE){
            stop("The requested tissue must be one of the options listed under 'Sheet Options', 'Tissues' in the Simulator.")
      }

      AllSheets <- readxl::excel_sheets(sim_data_file)

      SheetToExtract <- paste(enzyme, switch(tissue,
                                             "liver" = "(liver)",
                                             "gut" = "(gut)",
                                             "kidney" = "(kidney)"))

      # Reading in simulated abundance-time profile data
      sim_data_xl <- suppressMessages(
            readxl::read_excel(path = sim_data_file,
                               sheet = SheetToExtract,
                               col_names = FALSE))

      if("aggregate" %in% returnAggregateOrIndiv){
            # mean data
            StartRow_mean <- which(sim_data_xl$...1 == "Population Statistics") + 1
            sim_data_mean <- sim_data_xl[StartRow_mean:(StartRow_mean+3), ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric) %>%
                  rename(Time = "V1", mean = "V2", per5 = "V3", per95 = "V4") %>%
                  pivot_longer(names_to = "Trial", values_to = "Abundance", cols = -Time) %>%
                  mutate(Enzyme = Enzyme)

            # Checking for inhibitor
            EffectorPresent <- str_detect(sim_data_xl$...1[StartRow_mean + 4],
                                           "Inh")
            if(EffectorPresent){
                  sim_data_mean_inhib <- sim_data_xl[c(StartRow_mean, (StartRow_mean+4):(StartRow_mean+6)), ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = "V1", mean = "V2", per5 = "V3", per95 = "V4") %>%
                        pivot_longer(names_to = "Trial", values_to = "Abundance", cols = -Time) %>%
                        mutate(Enzyme = Enzyme,
                               EffectorPresent = TRUE)

                  sim_data_mean <- bind_rows(sim_data_mean, sim_data_mean_inhib)

            }

      }

      if("individual" %in% returnAggregateOrIndiv){

            # individual data
            RowsToUse <- which(str_detect(sim_data_xl$...1,
                                          paste("Enzyme value",
                                                switch(tissue,
                                                       "liver" = ".Liver.",
                                                       "gut" = ".Gut.",
                                                       "kidney" = ".Kidney."))) &
                                     !str_detect(sim_data_xl$...1, "mean"))
            RowsToUse <- c(RowsToUse[1]-1, RowsToUse)

            sim_data_ind <- sim_data_xl[RowsToUse, ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric) %>%
                  rename(Time = "V1")

            SubjTrial <- sim_data_xl[RowsToUse[2:length(RowsToUse)], 2:3] %>%
                  rename(Individual = ...2, Trial = ...3) %>%
                  mutate(SubjTrial = paste0("ID", Individual, "_", Trial))

            names(sim_data_ind)[2:ncol(sim_data_ind)] <- SubjTrial$SubjTrial

            sim_data_ind <- sim_data_ind %>%
                  pivot_longer(names_to = "SubjTrial", values_to = "Abundance",
                               cols = -Time) %>%
                  mutate(Enzyme = Enzyme,
                         SubjTrial = sub("ID", "", SubjTrial)) %>%
                  separate(SubjTrial, into = c("Individual", "Trial"),
                           sep = "_") %>%
                  mutate(across(.cols = c("Individual", "Trial"),
                                .fns = as.numeric))

            # Checking for inhibitor
            EffectorPresent <- str_detect(sim_data_xl$...1[max(RowsToUse)+1], "Inh")

            RowsToUse_inhib <- c(RowsToUse[1],
                                 which(str_detect(sim_data_xl$...1, "Enzyme value with Inh") &
                                       !str_detect(sim_data_xl$...1, "mean|percentile")))

            if(EffectorPresent){
                  sim_data_ind_inhib <-
                        sim_data_xl[RowsToUse_inhib, ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = "V1")

                  names(sim_data_ind_inhib)[
                        2:ncol(sim_data_ind_inhib)] <- SubjTrial$SubjTrial

                  sim_data_ind_inhib <- sim_data_ind_inhib %>%
                        pivot_longer(names_to = "SubjTrial", values_to = "Abundance",
                                     cols = -Time) %>%
                        mutate(Enzyme = Enzyme, EffectorPresent = TRUE,
                               SubjTrial = sub("ID", "", SubjTrial)) %>%
                        separate(SubjTrial, into = c("Individual", "Trial"),
                                 sep = "_") %>%
                        mutate(across(.cols = c("Individual", "Trial"),
                                      .fns = as.numeric))

                  sim_data_ind <- bind_rows(sim_data_ind, sim_data_ind_inhib)

                  rm(RowsToUse_inhib)

            }

            rm(RowsToUse)

      }

      TimeUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "^Time"))][1]
      TimeUnits <- ifelse(TimeUnits == "Time (h)", "Hours", "Minutes")

      Data <- list()

      if("aggregate" %in% returnAggregateOrIndiv){
            Data[["agg"]] <- sim_data_mean %>%
                  arrange(Trial, Time)
      }

      if("individual" %in% returnAggregateOrIndiv){
            Data[["indiv"]] <- sim_data_ind %>%
                  mutate(Individual = as.character(Individual),
                         Trial = as.character(Trial)) %>%
                  arrange(Individual, Time)
      }

      Data <- bind_rows(Data)

      if("individual" %in% returnAggregateOrIndiv){
            Data <- Data %>%
                  mutate(Individual = ifelse(is.na(Individual), Trial, Individual))
      }

      if("EffectorPresent" %in% names(Data)){
            Data <- Data %>%
                  mutate(EffectorPresent = ifelse(is.na(EffectorPresent),
                                                      FALSE, EffectorPresent))
      }

      Data <- Data %>%
            mutate(Time_units = tolower({{TimeUnits}}),
                   Tissue = tissue) %>%
            arrange(across(any_of(c("Enzyme", "Tissue", "EffectorPresent",
                                    "Individual", "Trial", "Time")))) %>%
            select(any_of(c("Enzyme", "Tissue", "EffectorPresent",
                            "Individual", "Trial",
                            "Time", "Abundance",
                            "Time_units")))

      return(Data)

}
