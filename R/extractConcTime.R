#' Extract concentration-time data from a simulator output Excel file
#'
#' Extracts concentration-time data from simulator output Excel files and,
#' optionally, a separately specified clinical data file, and puts all data into
#' a single, tidy data.frame. \emph{Note: Currently, only set up to extract
#' concentration-time data for the substrate, metabolite 1, metabolite 2, or
#' inhibitor 1.}
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   concentration-time data; must be an output file from the Simcyp simulator
#' @param obs_data_file name of the Excel file containing the observed
#'   concentration-time data for the substrate or metabolite you're extracting.
#'   If the observed data you want to plot were already included in the Excel
#'   output from the simulator, leave this as NA. Otherwise, this is the file
#'   that it is ready to be converted to an XML file, not the file that contains
#'   only the digitized time and concentration data.
#' @param obs_effector_data_file name of the Excel file containing observed
#'   concentration-time data for the effector, when appropriate
#' @param adjust_obs_time TRUE or FALSE: Adjust the time listed in the observed
#'   data file to match the last dose administered? This only applies to
#'   multiple-dosing regimens. If TRUE, the graph will show the observed data
#'   overlaid with the simulated data such that the dose in the observed data
#'   was administered at the same time as the last dose in the simulated data.
#'   If FALSE, the observed data will start at whatever times are listed in the
#'   Excel file.
#' @param tissue From which tissue should the desired concentrations be
#'   extracted? Default is plasma for typical plasma concentration-time data.
#'   Other options are "blood" or any tissues included in "Sheet Options",
#'   "Tissues" in the simulator, e.g., "lung", "brain", etc.
#' @param compoundToExtract For which compound do you want to extract
#'   concentration-time data? Options are "substrate" (default), "metabolite 1",
#'   "metabolite 2", or "effector" (this can be either an inducer or inhibitor;
#'   this is labeled as "inhibitor 1" in the simulator).
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
#'   \item{Individual}{the individual for the given profile, which will be a
#'   number for a simulated individual or will be "obs" or "obs+effector" for
#'   observed data, "mean" for the mean data, or "per5" or "per95" for the 5th
#'   and 95th percentile data.}
#'
#'   \item{Trial}{the trial number for that set of simulations or "obs", "mean",
#'   etc. for the observed or aggregate data}
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
#' @import tidyverse
#' @import readxl
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
#' extractConcTime(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'                 tissue = "lung")
#'
#'
extractConcTime <- function(sim_data_file,
                            obs_data_file = NA,
                            obs_effector_data_file = NA,
                            adjust_obs_time = FALSE,
                            tissue = "plasma",
                            compoundToExtract = "substrate",
                            returnAggregateOrIndiv = c("aggregate",
                                                       "individual")){

      # Error catching
      if(any(c(length(returnAggregateOrIndiv) < 1,
               length(returnAggregateOrIndiv) > 2,
               any(unique(returnAggregateOrIndiv) %in% c("aggregate", "individual") == FALSE)))) {
            stop("You must return one or both of 'aggregate' or 'individual' data for the parameter 'returnAggregateOrIndiv'.")
      }

      if(length(tissue) != 1){
            stop("You must enter one and only one tissue option. (Default is plasma.)")
      }

      tissue <- tolower(tissue)

      if(tissue %in% c("gi tissue", "lung", "additional organ", "adipose",
                       "heart", "muscle", "feto-placenta", "bone", "kidney",
                       "skin", "pancreas", "brain", "liver", "spleen",
                       "plasma", "blood") == FALSE){
            stop("The requested tissue must be plasma, blood, or one of the options listed under 'Sheet Options', 'Tissues' in the Simulator.")
      }

      compoundToExtract <- tolower(compoundToExtract)
      if(compoundToExtract %in% c("substrate", "metabolite 1", "metabolite 2",
                                  "effector") == FALSE){
            stop("The compound for which you requested concentration-time data was not one of the possible options. For 'compoundToExtract', please enter 'substrate', 'metabolite 1', 'metabolite 2', or 'effector'.")
      }

      # Getting summary data for the simulation
      SimSummary <- extractExpDetails(sim_data_file)

      # Effector present?
      EffectorPresent <- complete.cases(SimSummary[["Inhibitor"]])
      if(EffectorPresent == FALSE & compoundToExtract == "effector"){
            stop("There are no effector data in the simulator output file supplied. Please either submit a different output file or request concentration-time data for a substrate or metabolite.")
      }

      # Extracting tissue or plasma/blood data? Sheet format differs.
      TissueType <- ifelse(tissue %in% c("plasma", "blood"), "systemic", "tissue")

      SheetNames <- readxl::excel_sheets(sim_data_file)

      SheetToDetect <-
            switch(ifelse(TissueType == "systemic",
                          paste(tissue, sub(" 1| 2", "", compoundToExtract)),
                          tissue),
                   "plasma substrate" =
                         "Conc Profiles CSys\\(CPlasma\\)|Conc Trials Profiles\\(CPlasma\\)",
                   "plasma effector" =
                         "Conc Profiles CSys\\(CPlasma\\)|Conc Trials Profiles\\(CPlasma\\)",
                   "plasma metabolite" = paste0("Sub Pri Met",
                                                str_extract(compoundToExtract, "1|2"),
                                                ".*CPlasma"),
                   "blood substrate" = "Conc Profiles CSys\\(CBlood\\)|Conc Trials Profiles\\(CBlood\\)",
                   "blood effector" = "Conc Profiles CSys\\(CBlood\\)|Conc Trials Profiles\\(CBlood\\)",
                   "blood metabolite" = paste0("Sub Pri Met",
                                               str_extract(compoundToExtract, "1|2"),
                                               ".*CBlood"),
                   "gi tissue" = "Gut Tissue Conc",
                   "lung" = "Lung Conc",
                   "additional organ" = "Additional Organ Conc",
                   "adipose" = "Adipose Conc",
                   "heart" = "Heart Conc",
                   "muscle" = "Muscle Conc",
                   "bone" = "Bone Conc",
                   "kidney" = "Kidney Conc",
                   "skin" = "Skin Conc",
                   "pancreas" = "Pancreas Conc",
                   "brain" = "Brain Conc",
                   "liver" = "Liver Conc",
                   "spleen" = "Spleen Conc")
      Sheet <- SheetNames[str_detect(SheetNames, SheetToDetect)][1]
      if(length(Sheet) == 0){
            stop("We cannot find the necessary sheet in the simulator ouput file submitted. Please check that you have submitted the correct file for the tissue and compound requested.")
      }

      # Reading in simulated concentration-time profile data
      sim_data_xl <- suppressMessages(
            readxl::read_excel(path = sim_data_file,
                               sheet = Sheet,
                               col_names = FALSE))

      SubstrateName <- switch(paste(compoundToExtract, TissueType),
                              "substrate systemic" = SimSummary$Substrate,
                              "substrate tissue" = SimSummary$Substrate,
                              "effector systemic" = SimSummary$Substrate,
                              "effector tissue" = SimSummary$Substrate,
                              "metabolite 1 systemic" =
                                    gsub("CSys.*?-| \\(Trial [0-9]*\\)|.interaction",
                                         "", sim_data_xl[4, 1]),
                              "metabolite 2 systemic" =
                                    gsub("CSys.*?-| \\(Trial [0-9]*\\)|.interaction",
                                         "", sim_data_xl[4, 1]),
                              "metabolite 1 tissue" = sim_data_xl[5, 1],
                              "metabolite 2 tissue" = sim_data_xl[7, 1]) %>%
            as.character()

      if("aggregate" %in% returnAggregateOrIndiv){
            # mean data
            StartRow_mean <-
                  switch(paste(compoundToExtract, TissueType),
                         "substrate systemic" = which(sim_data_xl$...1 == "Population Statistics") + 2,
                         "substrate tissue" = which(sim_data_xl$...1 == "Population Statistics") + 2,
                         "effector systemic" = which(sim_data_xl$...1 == "Population Statistics") + 2,
                         "effector tissue" = which(sim_data_xl$...1 == "Population Statistics") + 2,
                         "metabolite 1 systemic" = which(sim_data_xl$...1 == "Population Statistics") + 2,
                         "metabolite 2 systemic" = which(sim_data_xl$...1 == "Population Statistics") + 2,
                         "metabolite 1 tissue" = which(str_detect(sim_data_xl$...1,
                                                                  paste0("M", tissue, " Mean"))),
                         "metabolite 2 tissue" = which(str_detect(sim_data_xl$...1,
                                                                  paste0("PM2", tissue, " Mean"))))[1]

            TimeRow <- which(sim_data_xl$...1 == "Population Statistics") + 1

            # Checking which cells contain mean, 5th, and 95th percentile data.
            NamesToCheck <- sim_data_xl$...1[c(StartRow_mean:(StartRow_mean + 4))]
            RowsToKeep <- c("mean" = which(str_detect(tolower(NamesToCheck), "mean") &
                                                 !str_detect(tolower(NamesToCheck),
                                                             "geometric")) + StartRow_mean-1,
                            "per5" = which(str_detect(tolower(NamesToCheck),
                                                      " 5th percentile")) + StartRow_mean-1,
                            "per95" = which(str_detect(tolower(NamesToCheck),
                                                       " 95th percentile")) + StartRow_mean-1)

            sim_data_mean <- sim_data_xl[c(TimeRow, RowsToKeep), ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric) %>%
                  rename(Time = 1,
                         mean = 2,
                         per5 = 3,
                         per95 = 4) %>%
                  pivot_longer(names_to = "Trial", values_to = "Conc",
                               cols = -c(Time)) %>%
                  mutate(Compound = SubstrateName,
                         Effector = "none")

            rm(NamesToCheck, RowsToKeep)

            if(EffectorPresent){

                  # Substrate concentrations in presence of effector
                  StartRow_mean_SubPlusEffector <-
                        which(str_detect(
                              tolower(sim_data_xl$...1),
                              switch(paste(TissueType, compoundToExtract),
                                     "systemic substrate" =
                                           tolower(
                                                 paste0("csys mean.*?",
                                                        SimSummary$Substrate,
                                                        " . interaction|csys mean.interaction.*?-",
                                                        SimSummary$Substrate)),
                                     "systemic metabolite 1" =
                                           tolower(
                                                 paste0("csys mean.*?",
                                                        SubstrateName,
                                                        " . interaction|csys mean.interaction.*?-",
                                                        SubstrateName)),
                                     "systemic metabolite 2" =
                                           tolower(
                                                 paste0("csys mean.*?",
                                                        SubstrateName,
                                                        " . interaction|csys mean.interaction.*?-",
                                                        SubstrateName)),
                                     "systemic effector" =
                                           tolower(
                                                 paste0("csys mean.*?",
                                                        SimSummary$Substrate,
                                                        " . interaction|csys mean.interaction.*?-",
                                                        SimSummary$Substrate)),
                                     "tissue substrate" =
                                           paste0("ctissue . interaction mean|",
                                                  "c", tissue, " mean.*?interaction"),
                                     "tissue effector" =
                                           paste0("ctissue . interaction mean|",
                                                  "c", tissue, " mean.*?interaction"),
                                     "tissue metabolite 1" =
                                           paste0("m", tissue, " mean.*?interaction"),
                                     "tissue metabolite 2" =
                                           paste0("pm2", tissue, " mean.*?interaction"))
                        ))[1]


                  # Checking which cells contain mean, 5th, and 95th percentile data.
                  NamesToCheck <- sim_data_xl$...1[
                        c(StartRow_mean_SubPlusEffector:(
                              StartRow_mean_SubPlusEffector + 4))]
                  RowsToKeep <- c(
                        "mean" = which(
                              str_detect(tolower(NamesToCheck), "mean") &
                                    !str_detect(tolower(NamesToCheck),
                                                "geometric")) +
                              StartRow_mean_SubPlusEffector-1,
                        "per5" = which(str_detect(tolower(NamesToCheck),
                                                  " 5th percentile")) +
                              StartRow_mean_SubPlusEffector-1,
                        "per95" = which(str_detect(tolower(NamesToCheck),
                                                   " 95th percentile")) +
                              StartRow_mean_SubPlusEffector-1)

                  sim_data_mean_SubPlusEffector <-
                        sim_data_xl[c(TimeRow, RowsToKeep), ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric) %>%
                        rename(Time = 1,
                               mean = 2,
                               per5 = 3,
                               per95 = 4) %>%
                        pivot_longer(names_to = "Trial", values_to = "Conc", cols = -Time) %>%
                        mutate(Compound = SubstrateName,
                               Effector = SimSummary[["Inhibitor"]])

                  rm(NamesToCheck, RowsToKeep)

                  # Effector concentrations -- only present on tabs w/substrate
                  # info for systemic tissues so extracting effector
                  # concentrations any time substrate concentrations requested
                  if(compoundToExtract %in% c("substrate", "effector")){

                        StartRow_mean_Effector <-
                              which(str_detect(
                                    sim_data_xl$...1,
                                    switch(TissueType,
                                           "systemic" = paste0("ISys 1 Mean.*?",
                                                               SimSummary[["Inhibitor"]]),
                                           "tissue" =
                                                 paste0("ITissue\\(Inh 1\\) Mean|",
                                                        "I", tissue, " 1 Mean"))
                              ))[1]

                        # Checking which cells contain mean, 5th, and 95th percentile data.
                        NamesToCheck <- sim_data_xl$...1[
                              c(StartRow_mean_Effector:(
                                    StartRow_mean_Effector + 4))]
                        RowsToKeep <- c(
                              "mean" = which(str_detect(tolower(NamesToCheck), "mean") &
                                                   !str_detect(tolower(NamesToCheck),
                                                               "geometric")) +
                                    StartRow_mean_Effector-1,
                              "per5" = which(str_detect(tolower(NamesToCheck),
                                                        " 5th percentile")) +
                                    StartRow_mean_Effector-1,
                              "per95" = which(str_detect(tolower(NamesToCheck),
                                                         " 95th percentile")) +
                                    StartRow_mean_Effector-1)

                        sim_data_mean_Effector <-
                              sim_data_xl[c(TimeRow, RowsToKeep), ] %>%
                              t() %>%
                              as.data.frame() %>% slice(-(1:3)) %>%
                              mutate_all(as.numeric) %>%
                              rename(Time = 1,
                                     mean = 2,
                                     per5 = 3,
                                     per95 = 4) %>%
                              pivot_longer(names_to = "Trial", values_to = "Conc",
                                           cols = -Time) %>%
                              mutate(Compound = SimSummary[["Inhibitor"]],
                                     Effector = SimSummary[["Inhibitor"]])

                        rm(NamesToCheck, Cells)

                        # All together
                        sim_data_mean <- bind_rows(sim_data_mean,
                                                   sim_data_mean_SubPlusEffector,
                                                   sim_data_mean_Effector)

                        rm(StartRow_mean_Effector, sim_data_mean_Effector)

                  } else {
                        # All together
                        sim_data_mean <- bind_rows(sim_data_mean,
                                                   sim_data_mean_SubPlusEffector)
                  }
                  rm(sim_data_mean_SubPlusEffector,
                     StartRow_mean_SubPlusEffector)
            }

            rm(StartRow_mean)
      }

      if("individual" %in% returnAggregateOrIndiv){

            # individual data
            RowsToUse <- which(
                  str_detect(sim_data_xl$...1,
                             switch(ifelse(TissueType == "systemic",
                                           TissueType,
                                           paste(TissueType, compoundToExtract)),
                                    "systemic" = "CSys \\(",
                                    "tissue substrate" =
                                          paste0("CTissue$|",
                                                 "C", tissue, " \\("),
                                    "tissue effector" =
                                          paste0("CTissue$|",
                                                 "C", tissue, " \\("),
                                    "tissue metabolite 1" =
                                          paste0("M", tissue, " \\("),
                                    "tissue metabolite 2" =
                                          paste0("PM2", tissue, " \\("))))
            RowsToUse <- RowsToUse[RowsToUse >
                                         which(str_detect(sim_data_xl$...1,
                                                          "Individual Statistics"))]
            if(TissueType == "tissue"){
                  RowsToUse <- c(RowsToUse[2] - 1, RowsToUse[-1])
            }

            TimeRow <- which(str_detect(sim_data_xl$...1,
                                        "Individual Statistics"))
            TimeRow <- which(
                  str_detect(as.character(sim_data_xl$...1[TimeRow:nrow(sim_data_xl)]),
                             "^Time"))[1] + TimeRow -1

            sim_data_ind <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric) %>%
                  rename(Time = "V1")

            SubjTrial <- sim_data_xl[RowsToUse, 2:3] %>%
                  rename(Individual = ...2, Trial = ...3) %>%
                  mutate(SubjTrial = paste0("ID", Individual, "_", Trial))

            names(sim_data_ind)[2:ncol(sim_data_ind)] <- SubjTrial$SubjTrial

            sim_data_ind <- sim_data_ind %>%
                  pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                               cols = -Time) %>%
                  mutate(Compound = SubstrateName,
                         SubjTrial = sub("ID", "", SubjTrial)) %>%
                  separate(SubjTrial, into = c("Individual", "Trial"),
                           sep = "_")
            rm(RowsToUse)

            if(EffectorPresent){

                  # Substrate conc time data in presence of effector
                  RowsToUse <- which(
                        str_detect(sim_data_xl$...1,
                                   switch(ifelse(TissueType == "systemic",
                                                 TissueType,
                                                 paste(TissueType, compoundToExtract)),
                                          "systemic" = "CSys After Inh|CSys.interaction",
                                          "tissue substrate" =
                                                paste0("CTissue . Interaction|",
                                                       "C", tissue, " After Inh"),
                                          "tissue effector" =
                                                paste0("CTissue . Interaction|",
                                                       "C", tissue, " After Inh"),
                                          "tissue metabolite 1" =
                                                paste0("M", tissue, " After Inh"),
                                          "tissue metabolite 2" =
                                                paste0("PM2", tissue, " After Inh"))
                        ))
                  RowsToUse <- RowsToUse[which(
                        RowsToUse > which(sim_data_xl$...1 == "Individual Statistics"))]
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
                        mutate(Compound = SubstrateName,
                               Effector = SimSummary[["Inhibitor"]],
                               SubjTrial = sub("ID", "", SubjTrial)) %>%
                        separate(SubjTrial, into = c("Individual", "Trial"),
                                 sep = "_")

                  rm(RowsToUse)

                  # Effector conc time data -- only present on substrate tabs
                  if(compoundToExtract %in% c("substrate", "effector")){

                        RowsToUse <- which(str_detect(
                              sim_data_xl$...1,
                              switch(TissueType,
                                     "systemic" = "ISys 1 \\(",
                                     "tissue" = paste0("ITissue\\(Inh 1|",
                                                       "I", tissue, " 1 \\("))
                        ))

                        RowsToUse <- RowsToUse[which(
                              RowsToUse > which(sim_data_xl$...1 == "Individual Statistics"))]
                        RowsToUse <- c(TimeRow, RowsToUse)

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
                              separate(SubjTrial, into = c("Individual", "Trial"),
                                       sep = "_")

                        rm(RowsToUse)

                        sim_data_ind <- bind_rows(sim_data_ind,
                                                  sim_data_ind_SubPlusEffector,
                                                  sim_data_ind_Effector)  %>%
                              mutate(Effector = ifelse(is.na(Effector),
                                                       "none", Effector))
                        rm(sim_data_ind_SubPlusEffector)

                  } else {
                        sim_data_ind <- bind_rows(sim_data_ind,
                                                  sim_data_ind_SubPlusEffector)  %>%
                              mutate(Effector = ifelse(is.na(Effector),
                                                       "none", Effector))
                  }
            }

            sim_data_ind <- sim_data_ind %>%
                  mutate(Trial = as.character(Trial))
            rm(TimeRow, SubjTrial)
      }

      # Determining concentration units
      SimConcUnits <- as.character(
            sim_data_xl[2, which(str_detect(as.character(sim_data_xl[2, ]), "CMax"))])
      SimConcUnits <- gsub("CMax \\(|\\)", "", SimConcUnits)

      TimeUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "^Time"))][1]
      TimeUnits <- ifelse(str_detect(TimeUnits, "Time.* \\(h\\)"), "hours", "minutes")

      # Observed data -- only applies to systemic concs
      if(TissueType == "systemic"){

            # If the user did not specify a file to use for observed data, use the
            # observed data that they included for the simulation.
            if(is.na(obs_data_file)){

                  StartRow_obs <- which(sim_data_xl$...1 == "Observed Data") + 1

                  if(length(StartRow_obs) != 0){

                        obs_data <- sim_data_xl[StartRow_obs:(StartRow_obs+1), ] %>% t() %>%
                              as.data.frame() %>% slice(-1) %>%
                              mutate_all(as.numeric) %>%
                              rename(Time = "V1", Conc = "V2") %>% filter(complete.cases(Time)) %>%
                              mutate(Trial = "obs")
                  }

            } else {
                  # If the user did specify an observed data file, read in observed data.
                  obs_data <- extractObsConcTime(obs_data_file) %>%
                        mutate(Trial = "obs", Simulated = FALSE,
                               Compound = SubstrateName)

                  TimeUnits <- unique(obs_data$Time_units)

                  # Converting to appropriate ObsConcUnits as necessary
                  ObsConcUnits <- unique(obs_data$Conc_units)

                  if(ObsConcUnits != SimConcUnits){

                        # Starting with this table of conversion factors, which is assuredly not
                        # exhaustive. Add to this as needed.
                        ConvTable <- data.frame(ObsUnits = c("ng/mL",
                                                             "ng/mL",
                                                             "ng/mL",
                                                             "pg/mL",
                                                             "µg/mL",
                                                             "mg/L",
                                                             "µg/mL",
                                                             "ng/mL"),
                                                SimUnits = c("mg/L",
                                                             "µg/mL",
                                                             "ng/L",
                                                             "mg/L",
                                                             "ng/mL",
                                                             "µg/mL",
                                                             "mg/L",
                                                             "mg/L"),
                                                Factor = c(10^3,
                                                           10^3,
                                                           10^-3,
                                                           10^6,
                                                           10^-3,
                                                           1,
                                                           1,
                                                           10^3))

                        if(SimConcUnits %in% ConvTable$SimUnits == FALSE |
                           ObsConcUnits %in% ConvTable$ObsUnits == FALSE |
                           all(c(SimConcUnits, ObsConcUnits) %in% c("µg/mL", "ng/mL", "ng/L",
                                                                    "µM", "nM", "mg", "mL", "mg/L",
                                                                    "PD response") == FALSE)){
                              stop("Our apologies, but we have not yet set up this function to deal with your concentration units. Please tell the Consultancy Team R working group what units you're working with and we can fix this.")
                        }

                        ConversionFactor <-
                              ConvTable$Factor[which(ConvTable$SimUnits == SimConcUnits &
                                                           ConvTable$ObsUnits == ObsConcUnits)]

                        if("individual" %in% returnAggregateOrIndiv){
                              sim_data_ind <- sim_data_ind %>%
                                    mutate(Conc = Conc*ConversionFactor)
                        }

                        if("aggregate" %in% returnAggregateOrIndiv){
                              sim_data_mean <- sim_data_mean %>%
                                    mutate(Conc = Conc*ConversionFactor)
                        }
                  }
            }

            if(exists("obs_data", inherits = FALSE)){
                  obs_data$Compound <- SimSummary[["Substrate"]]
                  obs_data$Simulated <- FALSE
            }

            if(complete.cases(obs_effector_data_file)){

                  obs_eff_data <- extractObsConcTime(obs_data_file =
                                                           obs_effector_data_file) %>%
                        mutate(Simulated = FALSE, Trial = "obs+effector",
                               Compound = SubstrateName,
                               Effector = SimSummary[["Inhibitor"]])

            }

            DosingScenario <- SimSummary[["Regimen_sub"]]

            if(adjust_obs_time & DosingScenario == "Multiple Dose" &
               exists("obs_data", inherits = FALSE)){
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
      }

      Data <- list()

      if("aggregate" %in% returnAggregateOrIndiv){
            Data[["agg"]] <- sim_data_mean %>%
                  mutate(Simulated = TRUE) %>%
                  arrange(Trial, Time)
      }

      if("individual" %in% returnAggregateOrIndiv){
            Data[["indiv"]] <- sim_data_ind %>%
                  mutate(Simulated = TRUE,
                         Individual = as.character(Individual),
                         Trial = as.character(Trial)) %>%
                  arrange(Individual, Time)
      }

      if(exists("obs_data", inherits = FALSE)){
            Data[["obs"]] <- obs_data

            if(any(c(exists("sim_data_mean_Effector", inherits = FALSE),
                     exists("sim_data_ind_Effector", inherits = FALSE)))){
                  Data[["obs"]]$Effector <- "none"
            }
      }

      if(exists("obs_eff_data", inherits = FALSE)){
            Data[["obs_eff"]] <- obs_eff_data
      }

      Data <- bind_rows(Data)

      if("individual" %in% returnAggregateOrIndiv){
            Data <- Data %>%
                  mutate(Individual = ifelse(is.na(Individual), Trial, Individual),
                         Individual = factor(Individual, levels = c(
                               c("obs", "obs+effector", "mean", "per5", "per95"),
                               setdiff(unique(Individual),
                                       c("obs", "obs+effector", "mean", "per5", "per95")))) )
      }

      if(EffectorPresent){
            Data$Effector[Data$Trial == "obs+effector"] <- SimSummary$Inhibitor
      }

      Data <- Data %>%
            mutate(Time_units = TimeUnits,
                   Conc_units = ifelse(exists("ObsConcUnits", inherits = FALSE),
                                       ObsConcUnits, SimConcUnits),
                   Trial = factor(Trial, levels = c(
                         c("obs", "obs+effector", "mean", "per5", "per95"),
                         setdiff(unique(Trial),
                                 c("obs", "obs+effector", "mean", "per5", "per95")))),
                   Tissue = tissue) %>%
            arrange(across(any_of(c("Compound", "Effector",
                                    "Individual", "Trial", "Time")))) %>%
            select(any_of(c("Compound", "Effector", "Tissue",
                            "Individual", "Trial",
                            "Simulated", "Time", "Conc",
                            "Time_units", "Conc_units")))

      # NOTE: I've been thinking about the output from this, and, for version
      # 0.1.2 and earlier, when there was an effector molecule present in the
      # simulation, the output included the concentrations of the effector
      # molecule as well as the substrate when the user asked for the substrate.
      # I'm concerned that this will lead to confusion because people will only
      # be expecting the concentrations of their substrate or metabolite.
      # Starting with version 0.1.3, I'm going to REMOVE effector concentrations
      # if the user requested "substrate" for compoundTOExtract, and, similarly,
      # I'm going to REMOVE substrate concentrations if the user specified
      # "effector" (something which wasn't an option previously). I think this
      # will also make it easier for this function to interface with ct_plot and
      # will result in more predictable output. If users want the concentrations
      # from multiple molecules, they can always run this function more than
      # once. If we decide this is NOT a route we want to go, remove the
      # "filter" statements I've added below.  -LS

      if(compoundToExtract == "substrate" & EffectorPresent){
            Data <- Data %>% filter(Compound != SimSummary[["Inhibitor"]])
      }

      if(compoundToExtract == "effector"){
            Data <- Data %>% filter(Compound == SimSummary[["Inhibitor"]])
      }

      return(Data)

}
