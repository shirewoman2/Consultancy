#' Extract concentration-time data from a simulator output Excel file
#'
#' Extracts concentration-time data from simulator output Excel files and,
#' optionally, a separately specified clinical data file, and puts all data into
#' a single, tidy data.frame.
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
#'   "Tissues" in the simulator. All possible options: "plasma", "blood",
#'   "unbound blood", "unbound plasma", "additional organ", "adipose", "bone",
#'   "brain", "feto-placenta", "GI tissue", "heart", "kidney", "liver", "lung",
#'   "muscle", "pancreas", "peripheral blood", "peripheral plasma", "peripheral
#'   unbound blood", "peripheral unbound plasma", "portal vein blood", "portal
#'   vein plasma", "portal vein unbound blood", "portal vein unbound plasma",
#'   "skin", or "spleen". Not case sensitive.
#' @param compoundToExtract For which compound do you want to extract
#'   concentration-time data? Options are "substrate" (default), "primary
#'   metabolite 1", "secondary metabolite", "effector" (this can be either an
#'   inducer or inhibitor; this is labeled as "inhibitor 1" in the simulator),
#'   "effector 2" for the 2nd inhibitor or inducer listed in the simulation, or
#'   "effector 1 metabolite" for the primary metabolite of effector 1.
#'   \emph{Note:} The simulator will report up to one metabolite for the 1st
#'   effector but no other effector metabolites. (Someone please correct me if
#'   that's wrong! -LS)
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated
#'   concentration-time data? Options are one or both of "aggregate" and
#'   "individual". Aggregated data are not calculated here but are pulled from
#'   the simulator output rows labeled as "Population Statistics".
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
      PossTiss <- c("gi tissue", "lung", "additional organ", "adipose",
                    "heart", "muscle", "feto-placenta", "bone", "kidney",
                    "skin", "pancreas", "brain", "liver", "spleen",
                    "plasma", "blood", "unbound plasma", "unbound blood",
                    "peripheral plasma", "peripheral blood",
                    "peripheral unbound plasma", "peripheral unbound blood",
                    "portal vein plasma", "portal vein blood",
                    "portal vein unbound plasma", "portal vein unbound blood")

      if(tissue %in% PossTiss == FALSE){
            stop("The requested tissue must be plasma, blood, or one of the options listed under 'Sheet Options', 'Tissues' in the Simulator. Please see the help file description for the 'tissue' argument.")
      }

      compoundToExtract <- tolower(compoundToExtract)
      if(compoundToExtract %in% c("substrate", "primary metabolite 1", "secondary metabolite",
                                  "effector", "effector 2", "effector 1 metabolite",
                                  "inhibitor 2 metabolite") == FALSE){
            stop("The compound for which you requested concentration-time data was not one of the possible options. For 'compoundToExtract', please enter 'substrate', 'primary metabolite 1', 'secondary metabolite', 'effector', 'effector 2', or 'effector 1 metabolite'.")
      }

      # Getting summary data for the simulation
      Deets <- extractExpDetails(sim_data_file)

      # Effector present?
      EffectorPresent <- complete.cases(Deets[["Inhibitor"]])
      if(EffectorPresent == FALSE & compoundToExtract == "effector"){
            stop("There are no effector data in the simulator output file supplied. Please either submit a different output file or request concentration-time data for a substrate or metabolite.")
      }
      AllEffectors <- c(Deets[["Inhibitor"]], Deets[["Inhibitor2"]],
                        Deets[["Inhibitor1Metabolite"]])
      AllEffectors <- AllEffectors[complete.cases(AllEffectors)]

      # Extracting tissue or plasma/blood data? Sheet format differs.
      TissueType <- ifelse(str_detect(tissue, "plasma|blood|portal|peripheral"),
                           "systemic", "tissue")
      if(str_detect(compoundToExtract, "metabolite") & TissueType == "tissue"){
            warning("You have requested metabolite concentrations in a solid tissue, which the simulator does not provide. Substrate or effector concentrations will be provided instead, depending on whether you requested a substrate or inhibitor metabolite.")
      }

      SheetNames <- readxl::excel_sheets(sim_data_file)
      if(TissueType == "systemic"){
            Piece1 <- ifelse(str_detect(tissue, "portal vein"), "PV ", "")

            Piece2 <- switch(compoundToExtract,
                             "substrate" = "Conc Profiles",
                             "primary metabolite 1" = "Sub Pri Met1",
                             "secondary metabolite" = "Sub Sec Met",
                             "effector" = "Conc Profiles",
                             "effector2" = "Conc Profiles",
                             "effector 1 metabolite" = "Conc Profiles")
            # If the tissue is the portal vein, all possible compounds are included, so make Piece2 be "Conc Profiles".
            Piece2 <- ifelse(str_detect(tissue, "portal vein"),
                             "Conc Profiles", Piece2)

            Piece3 <- ifelse(str_detect(tissue, "peripheral"), " CPeriph", " CSys")
            Piece3 <- ifelse(str_detect(tissue, "portal vein"),
                             "", Piece3)
            Piece3 <- ifelse(str_detect(compoundToExtract, "metabolite") &
                                   !str_detect(compoundToExtract, "effector"),
                             "", Piece3)

            Piece4 <- ifelse(str_detect(tissue, "unbound"), "Cu", "C")

            Piece5 <- str_to_title(str_extract(tissue, "blood|plasma"))

            SheetToDetect <- paste0(Piece1, Piece2, Piece3, "\\(", Piece4, Piece5, "\\)")
            SheetToDetect <- paste0(SheetToDetect, "|",
                                    sub("Profiles", "Trials Profiles", SheetToDetect))
      } else {
            SheetToDetect <-
                  switch(tissue,
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
                         "spleen" = "Spleen Conc",
                         "feto-placenta" = "Feto-Placenta" # Need to check this one. I don't ahve an example output file for this yet!
                  )
      }

      Sheet <- SheetNames[str_detect(SheetNames, SheetToDetect)][1]
      if(length(Sheet) == 0 | is.na(Sheet)){
            stop("We cannot find the necessary sheet in the simulator ouput file submitted. Please check that you have submitted the correct file for the tissue and compound requested.")
      }

      # Reading in simulated concentration-time profile data
      sim_data_xl <- suppressMessages(
            readxl::read_excel(path = sim_data_file,
                               sheet = Sheet,
                               col_names = FALSE))

      MyCompound <- switch(paste(compoundToExtract, TissueType),
                           "substrate systemic" = Deets$Substrate,
                           "substrate tissue" = Deets$Substrate,
                           "effector systemic" = Deets$Inhibitor,
                           "effector tissue" = Deets$Inhibitor,
                           "effector 1 metabolite systemic" = Deets$Inhibitor1Metabolite,
                           "primary metabolite 1 systemic" = Deets$Metabolite1,
                           "secondary metabolite systemic" = Deets$Metabolite2,
                           # The ones below here are the ones we can't provide,
                           # so we're giving them the concentration that *is*
                           # available instead.
                           "primary metabolite 1 tissue" = Deets$Substrate,
                           "secondary metabolite tissue" = Deets$Substrate,
                           "effector 1 metabolite tissue" = Deets$Inhibitor) %>%
            as.character()

      if(EffectorPresent){
            # When the simulator output is for the effector, for reasons I
            # *cannot fathom*, they include a number after "ISys" to designate
            # which effector the data pertain to and, *sometimes*, they will
            # list the name of the effector and *sometimes* they will only list
            # that number. I cannot determine exactly how they decide what that
            # number will be, so we need to figure out what that number is,
            # assign it to the correct effector, and extract appropriately.
            TimeRow <- which(str_detect(sim_data_xl$...1,
                                        "^Time.*Inhibitor "))[1]
            if(is.na(TimeRow)){ # This occurs when the tissue is not systemic
                  TimeRow <- which(str_detect(sim_data_xl$...1,
                                              "Time "))
                  TimeRow <- TimeRow[which(str_detect(sim_data_xl$...1[TimeRow + 1],
                                                      "^I|^CTissue"))][1]
            }

            # If TimeRow is still NA, it doesn't apply so ok to skip the rest of
            # this section.
            if(complete.cases(TimeRow)){

                  # Figuring out which rows contain which data
                  FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                          which(1:nrow(sim_data_xl) > TimeRow))[1]
                  FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
                  NamesToCheck <- sim_data_xl$...1[(TimeRow+1):(FirstBlank-1)]

                  temp <- data.frame(Name = NamesToCheck[
                        which(str_detect(NamesToCheck,
                                         gsub("_|\\-|\\+|\\(|\\)",
                                              ".", str_c(AllEffectors,
                                                         collapse = "|"))))]) %>%
                        mutate(Number = str_extract(Name, "ISys [0-9]|I[a-z]* [0-9]|InhM"),
                               Effector = str_extract(
                                     Name, gsub("_|\\-|\\+|\\(|\\)",
                                                ".",
                                                str_c(AllEffectors,
                                                      collapse = "|")))) %>%
                        select(Number, Effector) %>% unique()
                  NumCheck <- temp$Number
                  names(NumCheck) <- temp$Effector

                  if(length(NumCheck) == 0){
                        NumCheck <- "Inh 1"
                        names(NumCheck) <- Deets$Inhibitor
                  }

                  rm(temp, TimeRow, FirstBlank, NamesToCheck)
            } else {
                  rm(TimeRow)
            }
      }

      if("aggregate" %in% returnAggregateOrIndiv){
            # mean data
            TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
            TimeRow <- TimeRow[TimeRow > which(sim_data_xl$...1 == "Population Statistics")][1]

            # Figuring out which rows contain which data
            FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                    which(1:nrow(sim_data_xl) > TimeRow))[1]
            FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
            NamesToCheck <- tolower(sim_data_xl$...1[TimeRow:(FirstBlank-1)])


            # LEFT OFF HERE. Issues: Currently, this is extracting the effector
            # twice, I think, when compoundToExtract is set to effector. Need to
            # decide whether to change what we extract in this subsection from
            # compoundToExtract to whatever the substrate was and then filter at
            # the bottom of the script. That's how I had set it up prevoiusly.


            # Portal vein and tissue sheets have all compounds included, so need
            # to narrow down which rows to check further than the others.
            if(str_detect(tissue, "portal") | TissueType == "tissue"){
                  Include <-
                        which(str_detect(
                              NamesToCheck,
                              switch(compoundToExtract,
                                     "substrate" =
                                           paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                     "primary metabolite 1" =
                                           paste0("^mpv |^mpv\\+|^mtissue|^m", tolower(tissue)),
                                     "secondary metabolite" =
                                           paste0("^miipv|^miitissue|^mii", tolower(tissue)),
                                     "effector" =
                                           paste0(
                                                 "^ipv.*",
                                                 gsub("_|\\-|\\+|\\(|\\)",
                                                      ".",
                                                      tolower(Deets$Inhibitor)),
                                                 "|^", tolower(NumCheck[Deets$Inhibitor])),
                                     "effector 2" =
                                           paste0(
                                                 "^ipv.*",
                                                 gsub("_|\\-|\\+|\\(|\\)",
                                                      ".",
                                                      tolower(Deets$Inhibitor2)),
                                                 "|^", tolower(NumCheck[Deets$Inhibitor2])),
                                     "effector 1 metabolite" = "^inhm")))
            } else {
                  Include <- 1:length(NamesToCheck)
            }

            RowsToUse <- c(
                  "mean" = intersect(
                        which(str_detect(NamesToCheck, "mean") &
                                    !str_detect(NamesToCheck,
                                                "geometric|interaction")),
                        Include) + TimeRow-1,
                  "per5" = intersect(
                        which(str_detect(NamesToCheck," 5(th)? percentile") &
                                    !str_detect(NamesToCheck, "interaction|95")),
                        Include) + TimeRow-1,
                  "per95" = intersect(
                        which(str_detect(NamesToCheck, " 95(th)? percentile") &
                                    !str_detect(NamesToCheck, "interaction")),
                        Include) + TimeRow-1,
                  "per10" = intersect(
                        which(str_detect(NamesToCheck," 10(th)? percentile") &
                                    !str_detect(NamesToCheck,
                                                "interaction")),
                        Include) + TimeRow-1,
                  "per90" = intersect(
                        which(str_detect(NamesToCheck, " 90(th)? percentile") &
                                    !str_detect(NamesToCheck, "interaction")),
                        Include) + TimeRow-1,
                  "geomean" = intersect(
                        which(str_detect(NamesToCheck, "geometric mean") &
                                    !str_detect(NamesToCheck, "interaction")),
                        Include) + TimeRow-1,
                  "median" = intersect(
                        which(str_detect(NamesToCheck, "median") &
                                    !str_detect(NamesToCheck, "interaction")),
                        Include) + TimeRow-1)

            sim_data_mean <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric)
            names(sim_data_mean) <- c("Time", names(RowsToUse))
            sim_data_mean <- sim_data_mean %>%
                  pivot_longer(names_to = "Trial", values_to = "Conc",
                               cols = -c(Time)) %>%
                  mutate(Compound = MyCompound,
                         Effector = "none")

            rm(RowsToUse, Include)

            if(EffectorPresent){

                  # Portal vein and tissue sheets have all compounds included, so
                  # need to narrow down which rows to check further than the
                  # others.
                  if(str_detect(tissue, "portal") | TissueType == "tissue"){
                        Include <-
                              which(str_detect(
                                    NamesToCheck,
                                    switch(compoundToExtract,
                                           "substrate" =
                                                 paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                           "primary metabolite 1" =
                                                 paste0("^mpv |^mpv\\+|^mtissue|^m", tolower(tissue)),
                                           "secondary metabolite" =
                                                 paste0("^miipv|^miitissue|^mii", tolower(tissue)),
                                           "effector" =
                                                 paste0(
                                                       "^ipv.*",
                                                       gsub("_|\\-|\\+|\\(|\\)",
                                                            ".",
                                                            tolower(Deets$Inhibitor)),
                                                       "|^", tolower(NumCheck[Deets$Inhibitor])),
                                           "effector 2" =
                                                 paste0(
                                                       "^ipv.*",
                                                       gsub("_|\\-|\\+|\\(|\\)",
                                                            ".",
                                                            tolower(Deets$Inhibitor2)),
                                                       "|^", tolower(NumCheck[Deets$Inhibitor2])),
                                           "effector 1 metabolite" = "^inhm")))
                  } else {
                        Include <- 1:length(NamesToCheck)
                  }

                  RowsToUse <- c(
                        "mean" = intersect(
                              which(str_detect(NamesToCheck, "mean") &
                                          str_detect(NamesToCheck, "interaction") &
                                          !str_detect(NamesToCheck, "geometric")),
                              Include) + TimeRow-1,
                        "per5" = intersect(
                              which(str_detect(NamesToCheck," 5(th)? percentile") &
                                          str_detect(NamesToCheck, "interaction") &
                                          !str_detect(NamesToCheck, "95")),
                              Include) + TimeRow-1,
                        "per95" = intersect(
                              which(str_detect(NamesToCheck, " 95(th)? percentile") &
                                          str_detect(NamesToCheck, "interaction")),
                              Include) + TimeRow-1,
                        "per10" = intersect(
                              which(str_detect(NamesToCheck," 10(th)? percentile") &
                                          str_detect(NamesToCheck, "interaction")),
                              Include) + TimeRow-1,
                        "per90" = intersect(
                              which(str_detect(NamesToCheck, " 90(th)? percentile") &
                                          str_detect(NamesToCheck, "interaction")),
                              Include) + TimeRow-1,
                        "geomean" = intersect(
                              which(str_detect(NamesToCheck, "geometric mean") &
                                          str_detect(NamesToCheck, "interaction")),
                              Include) + TimeRow-1,
                        "median" = intersect(
                              which(str_detect(NamesToCheck, "median") &
                                          str_detect(NamesToCheck, "interaction")),
                              Include) + TimeRow-1)

                  sim_data_mean_SubPlusEffector <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                        t() %>%
                        as.data.frame() %>% slice(-(1:3)) %>%
                        mutate_all(as.numeric)
                  names(sim_data_mean_SubPlusEffector) <- c("Time", names(RowsToUse))
                  sim_data_mean_SubPlusEffector <- sim_data_mean_SubPlusEffector %>%
                        pivot_longer(names_to = "Trial", values_to = "Conc",
                                     cols = -c(Time)) %>%
                        mutate(Compound = MyCompound,
                               Effector = str_c(AllEffectors, collapse = ", "))

                  rm(RowsToUse, NamesToCheck, TimeRow, Include)

                  # Effector concentrations -- only present on tabs w/substrate
                  # info for systemic tissues so extracting effector
                  # concentrations any time substrate concentrations requested
                  if(compoundToExtract %in% c("substrate", "effector")){

                        # Need to do this for each effector present
                        sim_data_mean_Effector <- list()
                        TimeRow <- which(str_detect(sim_data_xl$...1,
                                                    "^Time.*Inhibitor "))[1]
                        if(is.na(TimeRow)){ # This occurs when the tissue is not systemic
                              TimeRow <- which(str_detect(sim_data_xl$...1,
                                                          "Time "))
                              TimeRow <- TimeRow[which(str_detect(sim_data_xl$...1[TimeRow + 1],
                                                                  "^I|CTissue"))][1]
                        }

                        # Figuring out which rows contain which data
                        FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                                which(1:nrow(sim_data_xl) > TimeRow))[1]
                        FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
                        NamesToCheck <- sim_data_xl$...1[(TimeRow+1):(FirstBlank-1)]

                        for(i in AllEffectors){

                              Include <- which(str_detect(NamesToCheck, NumCheck[i]))

                              RowsToUse <- c(
                                    "mean" = intersect(
                                          which(str_detect(NamesToCheck, "Mean") &
                                                      !str_detect(NamesToCheck, "Geometric")),
                                          Include) + TimeRow,
                                    "per5" = intersect(
                                          which(str_detect(NamesToCheck," 5(th)? percentile") &
                                                      !str_detect(NamesToCheck, "95")),
                                          Include) + TimeRow,
                                    "per95" = intersect(
                                          which(str_detect(NamesToCheck, " 95(th)? percentile")),
                                          Include) + TimeRow,
                                    "per10" = intersect(
                                          which(str_detect(NamesToCheck," 10(th)? percentile")),
                                          Include) + TimeRow,
                                    "per90" = intersect(
                                          which(str_detect(NamesToCheck, " 90(th)? percentile")),
                                          Include) + TimeRow,
                                    "geomean" = intersect(
                                          which(str_detect(NamesToCheck, "Geometric Mean")),
                                          Include) + TimeRow,
                                    "median" = intersect(
                                          which(str_detect(NamesToCheck, "Median")),
                                          Include) + TimeRow)

                              sim_data_mean_Effector[[i]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                                    t() %>%
                                    as.data.frame() %>% slice(-(1:3)) %>%
                                    mutate_all(as.numeric)
                              names(sim_data_mean_Effector[[i]]) <- c("Time", names(RowsToUse))
                              sim_data_mean_Effector[[i]] <- sim_data_mean_Effector[[i]] %>%
                                    pivot_longer(names_to = "Trial", values_to = "Conc",
                                                 cols = -c(Time)) %>%
                                    mutate(Compound = i,
                                           Effector = str_c(AllEffectors, collapse = ", "))

                              rm(RowsToUse, Include)
                        }

                        sim_data_mean_Effector <- bind_rows(sim_data_mean_Effector)
                        rm(NamesToCheck, TimeRow, FirstBlank)

                        # All data together
                        sim_data_mean <- bind_rows(sim_data_mean,
                                                   sim_data_mean_SubPlusEffector,
                                                   sim_data_mean_Effector)

                  } else {
                        # All together
                        sim_data_mean <- bind_rows(sim_data_mean,
                                                   sim_data_mean_SubPlusEffector)
                  }

            } else {
                  # If no effector present, need to remove stuff that pertained
                  # to aggregate data before doing individual data just to be on
                  # the safe side
                  rm(NamesToCheck)
            }
      }

      if("individual" %in% returnAggregateOrIndiv){

            # individual data
            StartIndiv <- which(str_detect(sim_data_xl$...1, "Individual Statistics"))

            TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
            TimeRow <- TimeRow[TimeRow > StartIndiv][1]

            RowsToUse <- which(
                  str_detect(sim_data_xl$...1,
                             switch(ifelse(TissueType == "systemic",
                                           TissueType,
                                           paste(TissueType, compoundToExtract)),
                                    "systemic" = "C(Sys|pv) \\(|CPeripheral",
                                    "tissue substrate" =
                                          paste0("CTissue$|",
                                                 "C", tissue, " \\("),
                                    "tissue effector" =
                                          paste0("CTissue$|",
                                                 "C", tissue, " \\("),
                                    "tissue primary metabolite 1" =
                                          paste0("M", tissue, " \\("),
                                    "tissue secondary metabolite" =
                                          paste0("PM2", tissue, " \\("))) &
                        !str_detect(sim_data_xl$...1, "interaction|After Inh"))
            RowsToUse <- RowsToUse[RowsToUse > TimeRow]

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
                  mutate(Compound = MyCompound,
                         Effector = "none",
                         SubjTrial = sub("ID", "", SubjTrial)) %>%
                  separate(SubjTrial, into = c("Individual", "Trial"),
                           sep = "_")
            rm(RowsToUse)

            if(EffectorPresent){

                  RowsToUse <- which(
                        str_detect(sim_data_xl$...1,
                                   switch(ifelse(TissueType == "systemic",
                                                 TissueType,
                                                 paste(TissueType, compoundToExtract)),
                                          "systemic" = "C(Sys|pv) After Inh|C(Sys|pv).interaction",
                                          "tissue substrate" =
                                                paste0("CTissue . Interaction|",
                                                       "C", tissue, " After Inh"),
                                          "tissue effector" =
                                                paste0("CTissue . Interaction|",
                                                       "C", tissue, " After Inh"),
                                          "tissue primary metabolite 1" =
                                                paste0("M", tissue, " After Inh"),
                                          "tissue secondary metabolite" =
                                                paste0("PM2", tissue, " After Inh"))
                        ))
                  RowsToUse <- RowsToUse[RowsToUse > TimeRow]

                  sim_data_ind_SubPlusEffector <-
                        sim_data_xl[c(TimeRow, RowsToUse), ] %>%
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
                        mutate(Compound = MyCompound,
                               Effector = str_c(AllEffectors, collapse = ", "),
                               SubjTrial = sub("ID", "", SubjTrial)) %>%
                        separate(SubjTrial, into = c("Individual", "Trial"),
                                 sep = "_")

                  rm(RowsToUse, TimeRow)

                  # Effector conc time data -- only present on substrate tabs
                  if(compoundToExtract %in% c("substrate", "effector")){

                        # Need to do this for each effector present
                        sim_data_ind_Effector <- list()
                        TimeRow <- which(str_detect(sim_data_xl$...1, "^Time.*Inhibitor "))
                        if(length(TimeRow) == 0 || is.na(TimeRow)){ # This occurs when the tissue is not systemic
                              TimeRow <- which(str_detect(sim_data_xl$...1,
                                                          "Time "))
                              TimeRow <- TimeRow[TimeRow > StartIndiv]
                              TimeRow <- TimeRow[which(str_detect(sim_data_xl$...1[TimeRow + 1],
                                                                  "^I|^CTissue"))][1]
                        }

                        # Figuring out which rows contain which data
                        FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                                which(1:nrow(sim_data_xl) > TimeRow))[1]
                        FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
                        NamesToCheck <- sim_data_xl$...1[(TimeRow+1):(FirstBlank-1)]

                        sim_data_ind_Effector <- list()

                        for(i in AllEffectors){
                              Include <- which(str_detect(NamesToCheck, NumCheck[i]))

                              RowsToUse <- which(str_detect(
                                    sim_data_xl$...1,
                                    switch(TissueType,
                                           "systemic" = paste0(NumCheck[i], " \\("),
                                           "tissue" = paste0("ITissue\\(Inh 1|",
                                                             "I", tissue, " 1 \\("))
                              ))
                              RowsToUse <- RowsToUse[which(RowsToUse > TimeRow)]

                              sim_data_ind_Effector[[i]] <-
                                    sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                                    t() %>%
                                    as.data.frame() %>% slice(-(1:3)) %>%
                                    mutate_all(as.numeric) %>%
                                    rename(Time = "V1")
                              names(sim_data_ind_Effector[[i]])[
                                    2:ncol(sim_data_ind_Effector[[i]])] <- SubjTrial$SubjTrial
                              sim_data_ind_Effector[[i]] <-
                                    sim_data_ind_Effector[[i]] %>%
                                    pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                                                 cols = -Time) %>%
                                    mutate(Compound = i,
                                           Effector = str_c(AllEffectors, collapse = ", "),
                                           SubjTrial = sub("ID", "", SubjTrial)) %>%
                                    separate(SubjTrial, into = c("Individual", "Trial"),
                                             sep = "_")

                              rm(RowsToUse)
                        }

                        sim_data_ind_Effector <- bind_rows(sim_data_ind_Effector)

                        sim_data_ind <- bind_rows(sim_data_ind,
                                                  sim_data_ind_SubPlusEffector,
                                                  sim_data_ind_Effector)  %>%
                              mutate(Effector = ifelse(is.na(Effector),
                                                       "none", Effector))

                        rm(TimeRow)

                  } else {
                        sim_data_ind <- bind_rows(sim_data_ind,
                                                  sim_data_ind_SubPlusEffector)  %>%
                              mutate(Effector = ifelse(is.na(Effector),
                                                       "none", Effector))
                  }
            }

            sim_data_ind <- sim_data_ind %>%
                  mutate(Trial = as.character(Trial))
            rm(SubjTrial)
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

                        obs_data <- sim_data_xl[StartRow_obs:nrow(sim_data_xl), ] %>% t() %>%
                              as.data.frame()
                        NewNamesObs <- obs_data[1, ]
                        NewNamesObs[str_detect(NewNamesObs, "Time")] <- "Time"
                        NewNamesObs <- gsub(" |\\: DV [0-9]", "", NewNamesObs)
                        TimeCols <- which(NewNamesObs == "Time")
                        ConcCols <- which(NewNamesObs != "Time")
                        NewNamesObs[TimeCols] <- paste0("Time_", NewNamesObs[ConcCols])
                        NewNamesObs[ConcCols] <- paste0("Conc_", NewNamesObs[ConcCols])
                        names(obs_data) <- NewNamesObs

                        suppressWarnings(
                              obs_data <- obs_data %>%
                                    mutate_all(as.numeric) %>%
                                    mutate(ID = 1:nrow(.)) %>%
                                    pivot_longer(cols = -ID,
                                                 names_to = "Param",
                                                 values_to = "Value") %>%
                                    separate(col = Param,
                                             into = c("Parameter", "Individual"),
                                             sep = "_") %>%
                                    pivot_wider(names_from = Parameter,
                                                values_from = Value) %>%
                                    filter(complete.cases(Time)) %>%
                                    mutate(Trial = "obs",
                                           Individual = sub("^Subject", "", Individual)) %>%
                                    select(-ID)
                        )
                  }

            } else {
                  # If the user did specify an observed data file, read in observed data.
                  obs_data <- extractObsConcTime(obs_data_file) %>%
                        mutate(Trial = "obs", Simulated = FALSE,
                               Compound = MyCompound)

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
                  obs_data$Compound <- Deets[["Substrate"]]
                  obs_data$Simulated <- FALSE
            }

            if(complete.cases(obs_effector_data_file)){

                  obs_eff_data <- extractObsConcTime(obs_data_file =
                                                           obs_effector_data_file) %>%
                        mutate(Simulated = FALSE, Trial = "obs+effector",
                               Compound = MyCompound,
                               Effector = Deets[["Inhibitor"]])

            }

            DosingScenario <- Deets[["Regimen_sub"]]

            if(adjust_obs_time & DosingScenario == "Multiple Dose" &
               exists("obs_data", inherits = FALSE)){
                  # If this were a multiple-dose simulation, the observed data is,
                  # presumably, at steady state. The simulated time we'd want those
                  # data to match would be the *last* dose. Adjusting the time for the
                  # obs data.

                  DoseFreq <- Deets[["DoseInt_sub"]]
                  NumDoses <- Deets[["NumDoses_sub"]]
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
            Data$Effector[Data$Trial == "obs+effector"] <- str_c(AllEffectors, collapse = ", ")
      }

      Data <- Data %>%
            mutate(Time_units = TimeUnits,
                   Conc_units = ifelse(exists("ObsConcUnits", inherits = FALSE),
                                       ObsConcUnits, SimConcUnits),
                   Trial = factor(Trial, levels = c(
                         c("obs", "obs+effector", "mean", "median",
                           "geomean", "per5", "per95", "per10", "per90"),
                         setdiff(unique(Trial),
                                 c("obs", "obs+effector", "mean", "median",
                                   "geomean", "per5", "per95", "per10", "per90")))),
                   Tissue = tissue) %>%
            arrange(across(any_of(c("Compound", "Effector",
                                    "Individual", "Trial", "Time")))) %>%
            select(any_of(c("Compound", "Effector", "Tissue",
                            "Individual", "Trial",
                            "Simulated", "Time", "Conc",
                            "Time_units", "Conc_units")))

      if(compoundToExtract %in% c("substrate", "primary metabolite 1",
                                  "secondary metabolite") &
         EffectorPresent){
            Data <- Data %>% filter(Compound !=
                                          str_c(AllEffectors, collapse = ", "))
      }

      if(compoundToExtract == "effector"){
            Data <- Data %>% filter(Compound == Deets$Inhibitor)
      }

      if(compoundToExtract == "effector 2"){
            Data <- Data %>% filter(Compound == Deets$Inhibitor2)
      }

      if(compoundToExtract == "effector 1 metabolite"){
            Data <- Data %>% filter(Compound == Deets$Inhibitor1Metabolite)
      }

      return(Data)

}
