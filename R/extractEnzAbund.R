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
#'   extracted? Options are "liver" (default), "gut", or "kidney". Note: If
#'   "gut" is selected, the output will return both colon and small intestine
#'   concentrations.
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
#'   \item{EffectorPresent}{TRUE or FALSE for whether any effectors were present
#'   at that time point (NB: The simulator shows two scenarios only: enzyme
#'   abundances 1. in the presence or 2. the absence of the effector(s). If
#'   there were multiple effectors, it does not show enzyme abundances with each
#'   separately.)}
#'
#'   \item{Time}{the time since the first dose}
#'
#'   \item{Abundance}{abundance of the enzyme listed}
#'
#'   \item{Time_units}{units used for time} }
#'
#' @import tidyverse
#' @import readxl
#' @export
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

      # Extracting aggregate data ---------------------------------------------
      if("aggregate" %in% returnAggregateOrIndiv){

            # If the tissue was gut, there are separate data sets for small
            # intestine and colon. Checking for that.
            GutParts <- c("colon", "small intestine")[
                  c(any(str_detect(tolower(sim_data_xl$...1), "\\(colon\\)")),
                    any(str_detect(tolower(sim_data_xl$...1), "\\(si\\)")))]

            if(all(complete.cases(GutParts)) & tissue == "gut"){

                  sim_data_mean <- list()

                  # mean data
                  StartRow_agg <- which(sim_data_xl$...1 == "Population Statistics")
                  TimeRows <- which(str_detect(sim_data_xl$...1, "^Time "))
                  TimeRows <- TimeRows[TimeRows > StartRow_agg][1:2]

                  # Figuring out which rows contain which data
                  FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                          which(1:nrow(sim_data_xl) > TimeRows[2]))[1]
                  FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
                  NamesToCheck <- tolower(sim_data_xl$...1[TimeRows[1]:(FirstBlank-1)])

                  # Need to note which rows are for which gut part.
                  SIrows <- which(str_detect(NamesToCheck, "\\(si\\)")) +
                        TimeRows[1]-1
                  SITimeRow <- TimeRows[TimeRows + 1 == SIrows]
                  # Looking for the next blank row after SITimeRow
                  SIEndRow <- which(is.na(sim_data_xl$...1))
                  SIEndRow <- SIEndRow[SIEndRow > SITimeRow][1] - 1

                  Colonrows <- which(str_detect(NamesToCheck, "\\(colon\\)")) +
                        TimeRows[1]-1
                  ColonTimeRow <- TimeRows[TimeRows + 1 == Colonrows]
                  # Looking for the next blank row after ColonTimeRow
                  ColonEndRow <- which(is.na(sim_data_xl$...1))
                  ColonEndRow <- ColonEndRow[ColonEndRow > ColonTimeRow][1] - 1

                  GutRows <- list("colon" = ColonTimeRow:ColonEndRow,
                                  "small intestine" = SITimeRow:SIEndRow)

                  # Checking for inhibitor
                  EffectorPresent <- any(str_detect(NamesToCheck, "with inh"))

                  rm(NamesToCheck)

                  for(i in GutParts){

                        # Checking which cells contain mean, 5th, and 95th
                        # percentile data.
                        NamesToCheck <- tolower(sim_data_xl$...1[GutRows[[i]]])

                        RowsToUse <-
                              c("mean" = which(str_detect(NamesToCheck,
                                                          "enzyme value mean")) +
                                      GutRows[[i]][1]-1,
                                "per5" = which(str_detect(NamesToCheck,
                                                          "enzyme value 5th percentile")) +
                                      GutRows[[i]][1]-1,
                                "per95" = which(str_detect(NamesToCheck,
                                                           "enzyme value 95th percentile")) +
                                      GutRows[[i]][1]-1)

                        sim_data_mean[[i]] <- sim_data_xl[c(GutRows[[i]][1], RowsToUse), ] %>%
                              t() %>%
                              as.data.frame() %>% slice(-(1:3)) %>%
                              mutate_all(as.numeric)
                        names(sim_data_mean[[i]]) <- c("Time", names(RowsToUse))
                        sim_data_mean[[i]] <- sim_data_mean[[i]] %>%
                              pivot_longer(names_to = "Trial", values_to = "Abundance", cols = -Time) %>%
                              mutate(Enzyme = enzyme,
                                     Tissue = i)

                        rm(RowsToUse, NamesToCheck)

                        if(EffectorPresent){

                              # Checking which cells contain mean, 5th, and 95th
                              # percentile data.
                              NamesToCheck <- tolower(sim_data_xl$...1[GutRows[[i]]])

                              RowsToUse <-
                                    c("mean" = which(str_detect(NamesToCheck,
                                                                "enzyme value with inh mean")) +
                                            GutRows[[i]][1]-1,
                                      "per5" = which(str_detect(NamesToCheck,
                                                                "enzyme value with inh 5th percentile")) +
                                            GutRows[[i]][1]-1,
                                      "per95" = which(str_detect(NamesToCheck,
                                                                 "enzyme value with inh 95th percentile")) +
                                            GutRows[[i]][1]-1)

                              sim_data_mean_inhib <- sim_data_xl[c(GutRows[[i]][1], RowsToUse), ] %>%
                                    t() %>%
                                    as.data.frame() %>% slice(-(1:3)) %>%
                                    mutate_all(as.numeric)
                              names(sim_data_mean_inhib) <- c("Time", names(RowsToUse))
                              sim_data_mean_inhib <- sim_data_mean_inhib %>%
                                    pivot_longer(names_to = "Trial",
                                                 values_to = "Abundance",
                                                 cols = -Time) %>%
                                    mutate(Enzyme = enzyme,
                                           Tissue = i,
                                           EffectorPresent = TRUE)

                              sim_data_mean[[i]] <- bind_rows(sim_data_mean[[i]],
                                                              sim_data_mean_inhib) %>%
                                    mutate(EffectorPresent = ifelse(is.na(EffectorPresent),
                                                                    FALSE, EffectorPresent))
                              rm(NamesToCheck, RowsToUse, sim_data_mean_inhib)
                        }
                  }

                  sim_data_mean <- bind_rows(sim_data_mean)

            } else {

                  # Substrate or substrate metabolites
                  TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
                  TimeRow <- TimeRow[TimeRow > which(sim_data_xl$...1 == "Population Statistics")][1]

                  # Figuring out which rows contain which data
                  FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                          which(1:nrow(sim_data_xl) > TimeRow))[1]
                  FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
                  NamesToCheck <- tolower(sim_data_xl$...1[TimeRow:(FirstBlank-1)])

                  RowsToUse <- c(
                        "mean" =
                              which(str_detect(NamesToCheck, "mean") &
                                          !str_detect(NamesToCheck,
                                                      "geometric|with inh")) +
                              TimeRow-1,
                        "per5" =
                              which(str_detect(NamesToCheck," 5(th)? percentile") &
                                          !str_detect(NamesToCheck, "with inh|95")) +
                              TimeRow-1,
                        "per95" =
                              which(str_detect(NamesToCheck, " 95(th)? percentile") &
                                          !str_detect(NamesToCheck, "with inh")) +
                              TimeRow-1,
                        "per10" =
                              which(str_detect(NamesToCheck," 10(th)? percentile") &
                                          !str_detect(NamesToCheck, "with inh")) +
                              TimeRow-1,
                        "per90" =
                              which(str_detect(NamesToCheck, " 90(th)? percentile") &
                                          !str_detect(NamesToCheck, "with inh")) +
                              TimeRow-1,
                        "geomean" =
                              which(str_detect(NamesToCheck, "geometric mean") &
                                          !str_detect(NamesToCheck, "with inh")) +
                              TimeRow-1,
                        "median" =
                              which(str_detect(NamesToCheck, "median") &
                                          !str_detect(NamesToCheck, "with inh")) +
                              TimeRow-1)

                  sim_data_mean <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric)
                  names(sim_data_mean) <- c("Time", names(RowsToUse))
                  sim_data_mean <- sim_data_mean %>%
                        pivot_longer(names_to = "Trial",
                                     values_to = "Abundance",
                                     cols = -Time) %>%
                        mutate(Enzyme = enzyme,
                               Tissue = tissue)
                  rm(RowsToUse)

                  # Checking for inhibitor
                  EffectorPresent <- any(str_detect(NamesToCheck, "with inh"))
                  if(EffectorPresent){

                        RowsToUse <- c(
                              "mean" =
                                    which(str_detect(NamesToCheck, "mean") &
                                                !str_detect(NamesToCheck, "geometric") &
                                                str_detect(NamesToCheck, "with inh")) +
                                    TimeRow-1,
                              "per5" =
                                    which(str_detect(NamesToCheck," 5(th)? percentile") &
                                                !str_detect(NamesToCheck, "95") &
                                                str_detect(NamesToCheck, "with inh")) +
                                    TimeRow-1,
                              "per95" =
                                    which(str_detect(NamesToCheck, " 95(th)? percentile") &
                                                str_detect(NamesToCheck, "with inh")) +
                                    TimeRow-1,
                              "per10" =
                                    which(str_detect(NamesToCheck," 10(th)? percentile") &
                                                str_detect(NamesToCheck, "with inh")) +
                                    TimeRow-1,
                              "per90" =
                                    which(str_detect(NamesToCheck, " 90(th)? percentile") &
                                                str_detect(NamesToCheck, "with inh")) +
                                    TimeRow-1,
                              "geomean" =
                                    which(str_detect(NamesToCheck, "geometric mean") &
                                                str_detect(NamesToCheck, "with inh")) +
                                    TimeRow-1,
                              "median" =
                                    which(str_detect(NamesToCheck, "median") &
                                                str_detect(NamesToCheck, "with inh")) +
                                    TimeRow-1)

                        sim_data_mean_inhib <-
                              sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                              t() %>%
                              as.data.frame() %>% slice(-(1:3))
                        names(sim_data_mean_inhib) <- c("Time", names(RowsToUse))
                        sim_data_mean_inhib <- sim_data_mean_inhib %>%
                              mutate_all(as.numeric) %>%
                              pivot_longer(names_to = "Trial",
                                           values_to = "Abundance",
                                           cols = -Time) %>%
                              mutate(Enzyme = enzyme,
                                     Tissue = tissue,
                                     EffectorPresent = TRUE)

                        sim_data_mean <- bind_rows(sim_data_mean,
                                                   sim_data_mean_inhib) %>%
                              mutate(EffectorPresent = ifelse(is.na(EffectorPresent),
                                                              FALSE, EffectorPresent))
                        rm(RowsToUse)
                  }
                  rm(TimeRow)
            }
      }

      # Extracting individual data --------------------------------------------
      if("individual" %in% returnAggregateOrIndiv){

            # If the tissue was gut, there are separate data sets for small
            # intestine and colon. Checking for that.
            GutParts <- c("colon", "small intestine")[
                  c(any(str_detect(tolower(sim_data_xl$...1), "\\(colon\\)")),
                    any(str_detect(tolower(sim_data_xl$...1), "\\(si\\)")))]

            if(length(which(complete.cases(GutParts))) > 0 & tissue == "gut"){

                  sim_data_ind <- list()

                  StartRow_ind <- which(sim_data_xl$...1 == "Individual Statistics")
                  TimeRows <- which(str_detect(sim_data_xl$...1, "^Time "))
                  TimeRows <- TimeRows[TimeRows > StartRow_ind][1:2]

                  # Figuring out which rows contain which data
                  FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                          which(1:nrow(sim_data_xl) > TimeRows[2]))[1]
                  FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
                  NamesToCheck <- tolower(sim_data_xl$...1[TimeRows[1]:nrow(sim_data_xl)])

                  # Need to note which rows are for which gut part.
                  SIrows <- which(str_detect(NamesToCheck, "\\(si\\)")) +
                        TimeRows[1]-1
                  SITimeRow <- intersect(TimeRows+1, SIrows) - 1
                  # Looking for the next blank row after SITimeRow
                  SIEndRow <- which(is.na(sim_data_xl$...1))
                  SIEndRow <- SIEndRow[SIEndRow > SITimeRow][1] - 1
                  # The above doesn't work if the last row is the last row of in the file, so
                  # catching that exception.
                  SIEndRow <- ifelse(is.na(SIEndRow), nrow(sim_data_xl), SIEndRow)

                  Colonrows <- which(str_detect(NamesToCheck, "\\(colon\\)")) +
                        TimeRows[1]-1
                  ColonTimeRow <- intersect(TimeRows+1, Colonrows) - 1
                  # Looking for the next blank row after ColonTimeRow
                  ColonEndRow <- which(is.na(sim_data_xl$...1))
                  ColonEndRow <- ColonEndRow[which(ColonEndRow > ColonTimeRow)][1] - 1
                  # The above doesn't work if the last row is the last row of in the file, so
                  # catching that exception.
                  ColonEndRow <- ifelse(is.na(ColonEndRow), nrow(sim_data_xl), ColonEndRow)

                  GutRows <- list("colon" = ColonTimeRow:ColonEndRow,
                                  "small intestine" = SITimeRow:SIEndRow)

                  # Checking for inhibitor
                  EffectorPresent <- any(str_detect(NamesToCheck, "with inh"))

                  rm(NamesToCheck)

                  for(i in GutParts){

                        RowsToUse <- intersect(
                              GutRows[[i]],
                              GutRows[[i]][which(!str_detect(tolower(sim_data_xl$...1[GutRows[[i]]]),
                                                             "with inh"))])

                        sim_data_ind[[i]] <- sim_data_xl[RowsToUse, ] %>%
                              t() %>%
                              as.data.frame() %>% slice(-(1:3)) %>%
                              mutate_all(as.numeric)%>%
                              rename(Time = "V1")

                        SubjTrial <- sim_data_xl[RowsToUse[-1], 2:3] %>%
                              rename(Individual = ...2, Trial = ...3) %>%
                              mutate(SubjTrial = paste0("ID", Individual, "_", Trial))

                        names(sim_data_ind[[i]])[2:ncol(sim_data_ind[[i]])] <- SubjTrial$SubjTrial

                        sim_data_ind[[i]] <- sim_data_ind[[i]] %>%
                              pivot_longer(names_to = "SubjTrial", values_to = "Abundance",
                                           cols = -Time) %>%
                              mutate(Enzyme = enzyme,
                                     Tissue = i,
                                     SubjTrial = sub("ID", "", SubjTrial)) %>%
                              separate(SubjTrial, into = c("Individual", "Trial"),
                                       sep = "_") %>%
                              mutate(across(.cols = c("Individual", "Trial"),
                                            .fns = as.numeric))

                        rm(RowsToUse)

                        if(EffectorPresent){

                              RowsToUse <- GutRows[[i]][which(
                                    str_detect(tolower(sim_data_xl$...1[GutRows[[i]]]),
                                               "with inh"))]

                              sim_data_ind_inhib <- sim_data_xl[c(GutRows[[i]][1], RowsToUse), ] %>%
                                    t() %>%
                                    as.data.frame() %>% slice(-(1:3)) %>%
                                    mutate_all(as.numeric)%>%
                                    rename(Time = "V1")

                              SubjTrial <- sim_data_xl[RowsToUse, 2:3] %>%
                                    rename(Individual = ...2, Trial = ...3) %>%
                                    mutate(SubjTrial = paste0("ID", Individual, "_", Trial))

                              names(sim_data_ind_inhib)[2:ncol(sim_data_ind_inhib)] <- SubjTrial$SubjTrial

                              sim_data_ind_inhib <- sim_data_ind_inhib %>%
                                    pivot_longer(names_to = "SubjTrial", values_to = "Abundance",
                                                 cols = -Time) %>%
                                    mutate(Enzyme = enzyme,
                                           Tissue = i,
                                           EffectorPresent = TRUE,
                                           SubjTrial = sub("ID", "", SubjTrial)) %>%
                                    separate(SubjTrial, into = c("Individual", "Trial"),
                                             sep = "_") %>%
                                    mutate(across(.cols = c("Individual", "Trial"),
                                                  .fns = as.numeric))

                              sim_data_ind[[i]] <- bind_rows(sim_data_ind[[i]],
                                                             sim_data_ind_inhib) %>%
                                    mutate(EffectorPresent = ifelse(is.na(EffectorPresent),
                                                                    FALSE, EffectorPresent))
                              rm(RowsToUse, sim_data_ind_inhib)
                        }
                  }

                  sim_data_ind <- bind_rows(sim_data_ind)

            } else {

                  # individual data
                  StartRow_ind <- which(sim_data_xl$...1 == "Individual Statistics")
                  TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
                  TimeRow <- TimeRow[TimeRow > StartRow_ind][1]

                  # Figuring out which rows contain which data
                  FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                          which(1:nrow(sim_data_xl) > TimeRow))[1]
                  FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
                  NamesToCheck <- tolower(sim_data_xl$...1[TimeRow:nrow(sim_data_xl)])

                  RowsToUse <- which(!str_detect(NamesToCheck, "with inh")) + TimeRow-1

                  sim_data_ind <- sim_data_xl[RowsToUse, ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = "V1")

                  SubjTrial <- sim_data_xl[RowsToUse[-1], 2:3] %>%
                        rename(Individual = ...2, Trial = ...3) %>%
                        mutate(SubjTrial = paste0("ID", Individual, "_", Trial))

                  names(sim_data_ind)[2:ncol(sim_data_ind)] <- SubjTrial$SubjTrial

                  sim_data_ind <- sim_data_ind %>%
                        pivot_longer(names_to = "SubjTrial",
                                     values_to = "Abundance",
                                     cols = -Time) %>%
                        mutate(Enzyme = enzyme,
                               Tissue = tissue,
                               SubjTrial = sub("ID", "", SubjTrial)) %>%
                        separate(SubjTrial, into = c("Individual", "Trial"),
                                 sep = "_") %>%
                        mutate(across(.cols = c("Individual", "Trial"),
                                      .fns = as.numeric))

                  rm(RowsToUse)

                  # Checking for inhibitor
                  EffectorPresent <- any(str_detect(NamesToCheck, "with inh"))

                  if(EffectorPresent){

                        RowsToUse <- which(str_detect(NamesToCheck, "with inh")) + TimeRow-1

                        sim_data_ind_inhib <-
                              sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                              t() %>%
                              as.data.frame() %>% slice(-(1:3)) %>%
                              mutate_all(as.numeric) %>%
                              rename(Time = "V1")

                        names(sim_data_ind_inhib)[
                              2:ncol(sim_data_ind_inhib)] <- SubjTrial$SubjTrial

                        sim_data_ind_inhib <- sim_data_ind_inhib %>%
                              pivot_longer(names_to = "SubjTrial",
                                           values_to = "Abundance",
                                           cols = -Time) %>%
                              mutate(Enzyme = enzyme,
                                     Tissue = tissue,
                                     EffectorPresent = TRUE,
                                     SubjTrial = sub("ID", "", SubjTrial)) %>%
                              separate(SubjTrial, into = c("Individual", "Trial"),
                                       sep = "_") %>%
                              mutate(across(.cols = c("Individual", "Trial"),
                                            .fns = as.numeric))

                        sim_data_ind <- bind_rows(sim_data_ind,
                                                  sim_data_ind_inhib) %>%
                              mutate(EffectorPresent = ifelse(is.na(EffectorPresent),
                                                              FALSE, EffectorPresent))

                        rm(RowsToUse)
                  }
                  rm(TimeRow, NamesToCheck)
            }
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

      Data <- Data %>%
            mutate(Time_units = tolower({{TimeUnits}})) %>%
            arrange(across(any_of(c("Enzyme", "Tissue", "EffectorPresent",
                                    "Individual", "Trial", "Time")))) %>%
            select(any_of(c("Enzyme", "Tissue", "EffectorPresent",
                            "Individual", "Trial",
                            "Time", "Abundance",
                            "Time_units")))

      return(Data)

}
