#' Extract concentration-time data from a simulator output Excel file
#'
#' Extracts concentration-time data from simulator output Excel files and,
#' optionally, a separately specified clinical data file, and puts all data into
#' a single, tidy data.frame. \strong{A note on observed data:} When observed
#' data are included in a simulator output file, because the simulator output
#' does not explicitly say whether those observed data were in the presence of
#' an inhibitor or effector, this function cannot tell the difference and will
#' thus assume all observed data included in the simulator output were for the
#' substrate in the \emph{absence} of any effector. It will further assume that
#' the compound the observed data is for is the same as
#' \code{compoundToExtract}. If \code{compoundToExtract} was an inhibitor or
#' inhibitor metabolite, the observed data from the simulator output will NOT be
#' pulled since it is unlikely to be inhibitor concentrations. It's generally
#' much safer to supply an observed data file here to make sure the data are
#' what you're expecting.
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   concentration-time data; must be an output file from the Simcyp simulator
#' @param obs_data_file name of the Excel file containing the observed
#'   concentration-time data for the substrate or metabolite you're extracting.
#'   If the observed data you want to plot were already included in the Excel
#'   output from the simulator, leave this as NA. Otherwise, this is the file
#'   that it is ready to be converted to an XML file, not the file that contains
#'   only the digitized time and concentration data.
#' @param obs_inhibitor_data_file name of the Excel file containing observed
#'   concentration-time data for when an effector is present, when appropriate.
#'   Only bother with this if the observed data in the presence of an inhibitor
#'   were not already included in the \code{obs_data_file}.
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
#'   metabolite 1", "primary metabolite 2", "secondary metabolite", "inhibitor
#'   1" (this can be an inducer, inhibitor, activator, or suppresesor, but it's
#'   labeled as "Inhibitor 1" in the simulator), "inhibitor 2" for the 2nd
#'   inhibitor listed in the simulation, or "inhibitor 1 metabolite" for the
#'   primary metabolite of inhibitor 1. \emph{Note:} The simulator will report
#'   up to one metabolite for the 1st inhibitor but no other inhibitor
#'   metabolites. (Someone please correct me if that's wrong! -LS)
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
#'   \item{Inhibitor (as applicable)}{the inhibitor or effector of interest;
#'   this matches whatever you named "Inhibitor 1" in the simulator}
#'
#'   \item{Individual}{the individual for the given profile, which will be a
#'   number for a simulated individual or will be "obs" or "obs+inhibitor1" for
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
#'                 obs_inhibitor_data_file = "../Mallikaarjun_2016_RTV-fig1-100mg-BID-DLM+Kaletra - for XML conversion.xlsx",
#'                 returnAggregateOrIndiv = c("aggregate", "individual"))
#'
#' extractConcTime(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'                 tissue = "lung")
#'
#'
extractConcTime <- function(sim_data_file,
                            obs_data_file = NA,
                            obs_inhibitor_data_file = NA,
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

      # The "exists" call in the next line is how we're checking whether this
      # function was called on its own (the result will be FALSE) or called from
      # extractConcTime_mult (result will be TRUE), where you can have multiple
      # tissues, files, and compounds. We need extractConcTime to ONLY give ONE
      # set of concentration-time data when called on alone so that it will work
      # as expected with, e.g., ct_plot.
      FromMultFun <- all(c(exists("compoundsToExtract", where = -1),
                           exists("tissues", where = -1),
                           exists("sim_data_files", where = -1)))

      if(length(tissue) != 1 & FromMultFun == FALSE){
            stop("You must enter one and only one option for 'tissue'. (Default is plasma.)")
      }

      if(length(compoundToExtract) != 1 & FromMultFun == FALSE){
            stop("You must enter one and only one option for 'compoundToExtract'. (Default is the substrate.)")
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
      if(any(compoundToExtract %in% c("substrate", "primary metabolite 1",
                                      "primary metabolite 2", "secondary metabolite",
                                      "inhibitor 1", "inhibitor 2", "inhibitor 1 metabolite",
                                      "inhibitor 2 metabolite") == FALSE)){
            stop("The compound for which you requested concentration-time data was not one of the possible options. For 'compoundToExtract', please enter 'substrate', 'primary metabolite 1', 'secondary metabolite', 'inhibitor 1', 'inhibitor 2', or 'inhibitor 1 metabolite'.")
      }

      # Getting summary data for the simulation(s)
      if(exists("Deets", where = -1) == FALSE){
            Deets <- extractExpDetails(sim_data_file)
      }

      # Effector present?
      EffectorPresent <- complete.cases(Deets[["Inhibitor1"]])
      if(EffectorPresent == FALSE & any(str_detect(compoundToExtract, "inhibitor")) &
         FromMultFun == FALSE){
            stop("There are no inhibitor data in the simulator output file supplied. Please either submit a different output file or request concentration-time data for a substrate or metabolite.")
      }
      AllEffectors <- c(Deets[["Inhibitor1"]], Deets[["Inhibitor2"]],
                        Deets[["Inhibitor1Metabolite"]])
      AllEffectors <- AllEffectors[complete.cases(AllEffectors)]

      # Extracting tissue or plasma/blood data? Sheet format differs.
      TissueType <- ifelse(str_detect(tissue, "plasma|blood|portal|peripheral"),
                           "systemic", "tissue")
      if(any(str_detect(compoundToExtract, "metabolite")) & TissueType == "tissue"){
            warning("You have requested metabolite concentrations in a solid tissue, which the simulator does not provide. Substrate or Inhibitor 1 concentrations will be provided instead, depending on whether you requested a substrate or inhibitor metabolite.")
            compoundToExtract <- compoundToExtract[!str_detect(compoundToExtract,
                                                               "metabolite")]
      }

      SheetNames <- readxl::excel_sheets(sim_data_file)

      if(FromMultFun & exists("k", where = -1)){
            CompoundType <- k
      } else {
            if(any(compoundToExtract %in% c("substrate", "inhibitor 1",
                                            "inhibitor 2", "inhibitor 1 metabolite"))){
                  CompoundType <- "substrate"
            } else {
                  CompoundType <- compoundToExtract
            }
      }

      if(TissueType == "systemic"){
            Piece1 <- ifelse(str_detect(tissue, "portal vein"), "PV ", "")

            Piece2 <- switch(CompoundType,
                             "substrate" = "Conc Profiles",
                             "primary metabolite 1" = "Sub Pri Met1",
                             "primary metabolite 2" = "Sub Pri Met2",
                             "secondary metabolite" = "Sub Sec Met")
            # If the tissue is the portal vein, all possible compounds are included, so make Piece2 be "Conc Profiles".
            Piece2 <- ifelse(str_detect(tissue, "portal vein"),
                             "Conc Profiles", Piece2)

            Piece3 <- ifelse(str_detect(tissue, "peripheral"), " CPeriph", " CSys")
            Piece3 <- ifelse(str_detect(tissue, "portal vein"),
                             "", Piece3)
            Piece3 <- ifelse(CompoundType != "substrate",
                             "", Piece3)

            Piece4 <- ifelse(str_detect(tissue, "unbound"), "Cu", "C")

            Piece5 <- str_to_title(str_extract(tissue, "blood|plasma"))

            SheetToDetect <- paste0(Piece1, Piece2, Piece3, "\\(", Piece4, Piece5, "\\)")
            SheetToDetect <- paste0(SheetToDetect, "|",
                                    sub("Profiles", "Trials Profiles", SheetToDetect),
                                    "|",
                                    gsub("Profiles CSys", "Trials Profiles", SheetToDetect),
                                    "|",
                                    paste0(Piece1, Piece2, "\\(Trials\\)\\(",
                                           Piece4, Piece5, "\\)") )
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
      if((length(Sheet) == 0 | is.na(Sheet))){
            if(FromMultFun == FALSE){
                  stop("We cannot find the necessary sheet in the simulator ouput file submitted. Please check that you have submitted the correct file for the tissue and compound requested.")
            } else {
                  warning(paste0("You requested data for ", str_comma(compoundToExtract),
                                 " in ", tissue,
                                 " from the file '",
                                 sim_data_file, "', but that compound and/or tissue or that combination of compound and tissue is not available in that file and will be skipped."))
                  return(data.frame())
            }
      }
      # Reading in simulated concentration-time profile data
      sim_data_xl <- suppressMessages(
            readxl::read_excel(path = sim_data_file,
                               sheet = Sheet,
                               col_names = FALSE))

      # Extracting each compound ----------------------------------------------

      # Note: This is a loop for use by extractConcTime_mult. For just running
      # extractConcTime, this will only have a single iteration.

      sim_data_mean <- list()
      sim_data_ind <- list()

      for(m in compoundToExtract){

            MyCompound <- switch(paste(m, TissueType),
                                 "substrate systemic" = Deets$Substrate,
                                 "substrate tissue" = Deets$Substrate,
                                 "inhibitor 1 systemic" = Deets$Inhibitor1,
                                 "inhibitor 1 tissue" = Deets$Inhibitor1,
                                 "inhibitor 1 metabolite systemic" = Deets$Inhibitor1Metabolite,
                                 "inhibitor 2 systemic" = Deets$Inhibitor2,
                                 "inhibitor 2 tissue" = Deets$Inhibitor2,
                                 "primary metabolite 1 systemic" = Deets$PrimaryMetabolite1,
                                 "primary metabolite 2 systemic" = Deets$PrimaryMetabolite2,
                                 "secondary metabolite systemic" = Deets$SecondaryMetabolite,
                                 "primary metabolite 1 tissue" = Deets$PrimaryMetabolite1,
                                 "primary metabolite 2 tissue" = Deets$PrimaryMetabolite2,
                                 "secondary metabolite tissue" = Deets$SecondaryMetabolite,
                                 # inhibitor 1 metabolite concs aren't available in
                                 # tissues, are they? Giving the user the Inhibitor
                                 # instead b/c I don't think they are.
                                 "inhibitor 1 metabolite tissue" = Deets$Inhibitor1) %>%
                  as.character()

            if(EffectorPresent){
                  # When the simulator output is for an inhibitor, for reasons I *cannot
                  # fathom*, they include a number after "ISys" to designate which
                  # inhibitor the data pertain to and, *sometimes*, they will list
                  # the name of the inhibitor and *sometimes* they will only list
                  # that number. I cannot determine exactly how they decide what that
                  # number will be, so we need to figure out what that number is,
                  # assign it to the correct inhibitor, and extract appropriately.
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

                        EffString <- gsub("_|\\-|\\+|\\(|\\)",
                                          ".",
                                          AllEffectors)
                        temp <- data.frame(Name = NamesToCheck[
                              which(str_detect(NamesToCheck,
                                               str_c(EffString, collapse = "|")))]) %>%
                              mutate(Number = str_extract(Name, "ISys [0-9]|I[a-z]* [0-9]|InhM"),
                                     Inhibitor =
                                           str_extract(Name,
                                                       str_c(paste0(EffString, "( \\(.*\\))?$"),
                                                             collapse = "|")),
                                     Inhibitor = sub("( \\(.*\\))?$", "", Inhibitor)) %>%
                              select(Number, Inhibitor) %>% unique()
                        NumCheck <- temp$Number
                        names(NumCheck) <- temp$Inhibitor

                        if(length(NumCheck) == 0){
                              NumCheck <- "Inh 1"
                              names(NumCheck) <- Deets$Inhibitor1
                        }

                        rm(temp, TimeRow, FirstBlank, NamesToCheck)
                  } else {
                        rm(TimeRow)
                  }
            }

            # Determining concentration and time units
            SimConcUnits <- as.character(
                  sim_data_xl[2, which(str_detect(as.character(sim_data_xl[2, ]), "CMax"))])[1]
            SimConcUnits <- gsub("CMax \\(|\\)", "", SimConcUnits)

            SimTimeUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "^Time"))][1]
            SimTimeUnits <- ifelse(str_detect(SimTimeUnits, "Time.* \\(h\\)"), "hours", "minutes")

            # aggregate data -------------------------------------------------------
            if("aggregate" %in% returnAggregateOrIndiv){

                  ## m is substrate or substrate metabolite -----------
                  if(str_detect(m, "substrate|metabolite") &
                     !str_detect(m, "inhibitor")){

                        TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
                        TimeRow <- TimeRow[TimeRow > which(sim_data_xl$...1 == "Population Statistics")][1]

                        # Figuring out which rows contain which data
                        FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                                which(1:nrow(sim_data_xl) > TimeRow))[1]
                        FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
                        NamesToCheck <- tolower(sim_data_xl$...1[TimeRow:(FirstBlank-1)])

                        # Some sheets have all compounds included, so need to narrow
                        # down which rows to check. Others don't have metabolites
                        # listed on the same sheet, so that's why there are these
                        # options.
                        if(str_detect(tissue, "portal") | TissueType == "tissue"){
                              Include <-
                                    which(str_detect(
                                          NamesToCheck,
                                          switch(m,
                                                 "substrate" =
                                                       paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                                 "primary metabolite 1" =
                                                       paste0("^mpv |^mpv\\+|^mtissue|^m", tolower(tissue)),
                                                 "primary metabolite 2" =
                                                       paste0("^pm2pv |^pm2pb\\+|^pm2tissue|^pm2", tolower(tissue)),
                                                 "secondary metabolite" =
                                                       paste0("^miipv|^miitissue|^mii", tolower(tissue)),
                                                 "inhibitor 1" =
                                                       paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                                 "inhibitor 2" =
                                                       paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                                 "inhibitor 1 metabolite" =
                                                       paste0("^cpv|^ctissue|^c", tolower(tissue)))))
                        } else {
                              Include <- which(str_detect(NamesToCheck, "^csys"))
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

                        sim_data_mean[[m]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                              t() %>%
                              as.data.frame() %>% slice(-(1:3)) %>%
                              mutate_all(as.numeric)
                        names(sim_data_mean[[m]]) <- c("Time", names(RowsToUse))
                        sim_data_mean[[m]] <- sim_data_mean[[m]] %>%
                              pivot_longer(names_to = "Trial", values_to = "Conc",
                                           cols = -c(Time)) %>%
                              mutate(Compound = MyCompound,
                                     CompoundID = m,
                                     Inhibitor = "none",
                                     Time_units = SimTimeUnits,
                                     Conc_units = SimConcUnits)

                        rm(RowsToUse, Include)

                        if(EffectorPresent){

                              # Some sheets have all compounds included, so need to
                              # narrow down which rows to check. Others don't have
                              # metabolites listed on the same sheet, so that's why
                              # there are these options.
                              if(str_detect(tissue, "portal") | TissueType == "tissue"){
                                    Include <-
                                          which(str_detect(
                                                NamesToCheck,
                                                switch(m,
                                                       "substrate" =
                                                             paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                                       "primary metabolite 1" =
                                                             paste0("^mpv |^mpv\\+|^mtissue|^m", tolower(tissue)),
                                                       "primary metabolite 2" =
                                                             paste0("^pm2pv |^pm2pb\\+|^pm2tissue|^pm2", tolower(tissue)),
                                                       "secondary metabolite" =
                                                             paste0("^miipv|^miitissue|^mii", tolower(tissue)),
                                                       "inhibitor 1" =
                                                             paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                                       "inhibitor 2" =
                                                             paste0("^cpv|^ctissue|^c", tolower(tissue)),
                                                       "inhibitor 1 metabolite" =
                                                             paste0("^cpv|^ctissue|^c", tolower(tissue)))))
                              } else {
                                    Include <- which(str_detect(NamesToCheck, "^csys"))
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
                                           Inhibitor = str_c(AllEffectors, collapse = ", "),
                                           CompoundID = m,
                                           Time_units = SimTimeUnits,
                                           Conc_units = SimConcUnits)

                              sim_data_mean[[m]] <- bind_rows(sim_data_mean[[m]],
                                                              sim_data_mean_SubPlusEffector)

                              rm(RowsToUse, NamesToCheck, TimeRow, Include,
                                 FirstBlank, sim_data_mean_SubPlusEffector)
                        }
                  }

                  ## m is an inhibitor or inhibitor metabolite -----------

                  # Inhibitor concentrations are only present on tabs
                  # w/substrate info for systemic tissues.
                  if(m %in% c("inhibitor 1", "inhibitor 2",
                              "inhibitor 1 metabolite") &
                     length(AllEffectors) > 0){

                        # Need to do this for each inhibitor present
                        sim_data_mean[[m]] <- list()
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
                                                      !str_detect(NamesToCheck, "Geome(t)?ric")), # There's a spelling error in some simulator output, and geometric is listed as "geomeric".
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
                                          which(str_detect(NamesToCheck, "Geome(t)?ric Mean")),
                                          Include) + TimeRow,
                                    "median" = intersect(
                                          which(str_detect(NamesToCheck, "Median")),
                                          Include) + TimeRow)

                              sim_data_mean[[m]][[i]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                                    t() %>%
                                    as.data.frame() %>% slice(-(1:3)) %>%
                                    mutate_all(as.numeric)
                              names(sim_data_mean[[m]][[i]]) <- c("Time", names(RowsToUse))
                              sim_data_mean[[m]][[i]] <- sim_data_mean[[m]][[i]] %>%
                                    pivot_longer(names_to = "Trial", values_to = "Conc",
                                                 cols = -c(Time)) %>%
                                    mutate(Compound = i,
                                           CompoundID = m,
                                           Inhibitor = str_c(AllEffectors, collapse = ", "),
                                           Time_units = SimTimeUnits,
                                           Conc_units = SimConcUnits)

                              rm(RowsToUse, Include)
                        }

                        sim_data_mean[[m]] <- bind_rows(sim_data_mean[[m]])

                        rm(NamesToCheck, TimeRow, FirstBlank)
                  }
            }

            # individual data ------------------------------------------------------
            if("individual" %in% returnAggregateOrIndiv){

                  ## m is substrate or substrate metabolite -----------
                  if(str_detect(m, "substrate|metabolite") &
                     !str_detect(m, "inhibitor")){

                        # substrate data
                        StartIndiv <- which(str_detect(sim_data_xl$...1, "Individual Statistics"))

                        TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
                        TimeRow <- TimeRow[TimeRow > StartIndiv][1]

                        RowsToUse <- which(
                              str_detect(sim_data_xl$...1,
                                         switch(ifelse(TissueType == "systemic",
                                                       TissueType,
                                                       paste(TissueType, m)),
                                                "systemic" = "C(Sys|pv)|CPeripheral",
                                                "tissue substrate" =
                                                      paste0("CTissue$|",
                                                             "C", tissue, " \\("),
                                                "tissue inhibitor 1" =
                                                      paste0("CTissue$|",
                                                             "C", tissue, " \\("),
                                                "tissue primary metabolite 1" =
                                                      paste0("M", tissue, " \\("),
                                                "tissue secondary metabolite" =
                                                      paste0("PM2", tissue, " \\("))) &
                                    !str_detect(sim_data_xl$...1, "interaction|After Inh"))
                        RowsToUse <- RowsToUse[RowsToUse > TimeRow]

                        sim_data_ind[[m]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                              t() %>%
                              as.data.frame() %>% slice(-(1:3)) %>%
                              mutate_all(as.numeric) %>%
                              rename(Time = "V1")

                        SubjTrial <- sim_data_xl[RowsToUse, 2:3] %>%
                              rename(Individual = ...2, Trial = ...3) %>%
                              mutate(SubjTrial = paste0("ID", Individual, "_", Trial))

                        names(sim_data_ind[[m]])[2:ncol(sim_data_ind[[m]])] <- SubjTrial$SubjTrial

                        sim_data_ind[[m]] <- sim_data_ind[[m]] %>%
                              pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                                           cols = -Time) %>%
                              mutate(Compound = MyCompound,
                                     CompoundID = m,
                                     Inhibitor = "none",
                                     SubjTrial = sub("ID", "", SubjTrial),
                                     Time_units = SimTimeUnits,
                                     Conc_units = SimConcUnits) %>%
                              separate(SubjTrial, into = c("Individual", "Trial"),
                                       sep = "_")
                        rm(RowsToUse)

                        if(EffectorPresent){

                              RowsToUse <- which(
                                    str_detect(sim_data_xl$...1,
                                               switch(ifelse(TissueType == "systemic",
                                                             TissueType,
                                                             paste(TissueType, m)),
                                                      "systemic" = "C(Sys|pv) After Inh|C(Sys|pv).interaction",
                                                      "tissue substrate" =
                                                            paste0("CTissue . Interaction|",
                                                                   "C", tissue, " After Inh"),
                                                      "tissue inhibitor 1" =
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
                                           CompoundID = m,
                                           Inhibitor = str_c(AllEffectors, collapse = ", "),
                                           SubjTrial = sub("ID", "", SubjTrial),
                                           Time_units = SimTimeUnits,
                                           Conc_units = SimConcUnits) %>%
                                    separate(SubjTrial, into = c("Individual", "Trial"),
                                             sep = "_")

                              sim_data_ind[[m]] <- bind_rows(sim_data_ind[[m]],
                                                             sim_data_ind_SubPlusEffector)

                              rm(RowsToUse, TimeRow, sim_data_ind_SubPlusEffector)
                        }
                  }

                  ## m is an inhibitor or inhibitor metabolite -----------

                  # Inhibitor concentrations are only present on tabs
                  # w/substrate info for systemic tissues.
                  if(m %in% c("inhibitor 1", "inhibitor 2",
                              "inhibitor 1 metabolite") &
                     length(AllEffectors) > 0){

                        sim_data_ind[[m]] <- list()

                        TimeRow <- which(str_detect(sim_data_xl$...1, "^Time.*Inhibitor "))
                        TimeRow <- TimeRow[TimeRow > StartIndiv]
                        if(length(TimeRow) == 0 || is.na(TimeRow)){ # This occurs when the tissue is not systemic or w/portal vein
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

                        # Need to do this for each inhibitor present
                        for(i in AllEffectors){
                              Include <- which(str_detect(NamesToCheck, NumCheck[i]))
                              if(length(Include) == 0){
                                    Include <- 1:length(NamesToCheck)
                              }

                              RowsToUse <- which(str_detect(
                                    sim_data_xl$...1,
                                    switch(TissueType,
                                           "systemic" = paste0(NumCheck[i], " \\(|",
                                                               i),
                                           "tissue" = paste0("ITissue\\(Inh 1|",
                                                             "I", tissue, " 1 \\("))
                              ))
                              RowsToUse <- RowsToUse[which(RowsToUse > TimeRow)]

                              sim_data_ind[[m]][[i]] <-
                                    sim_data_xl[c(TimeRow, RowsToUse), ] %>%
                                    t() %>%
                                    as.data.frame() %>% slice(-(1:3)) %>%
                                    mutate_all(as.numeric) %>%
                                    rename(Time = "V1")

                              SubjTrial <- sim_data_xl[RowsToUse, 2:3] %>%
                                    rename(Individual = ...2, Trial = ...3) %>%
                                    mutate(SubjTrial = paste0("ID", Individual, "_", Trial))

                              names(sim_data_ind[[m]][[i]])[2:ncol(sim_data_ind[[m]][[i]])] <-
                                    SubjTrial$SubjTrial

                              sim_data_ind[[m]][[i]] <-
                                    sim_data_ind[[m]][[i]] %>%
                                    pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                                                 cols = -Time) %>%
                                    mutate(Compound = i,
                                           CompoundID = m,
                                           Inhibitor = str_c(AllEffectors, collapse = ", "),
                                           SubjTrial = sub("ID", "", SubjTrial),
                                           Time_units = SimTimeUnits,
                                           Conc_units = SimConcUnits) %>%
                                    separate(SubjTrial, into = c("Individual", "Trial"),
                                             sep = "_")

                              rm(RowsToUse)
                        }

                        sim_data_ind[[m]] <- bind_rows(sim_data_ind[[m]])

                        rm(TimeRow, FirstBlank, NamesToCheck)
                  }
            }

            rm(MyCompound)
      }

      sim_data_mean <- bind_rows(sim_data_mean)
      sim_data_ind <- bind_rows(sim_data_ind)

      # observed data -------------------------------------------------------
      # only applies to systemic concs

      # Setting up some names of observed data for use later
      ObsCompounds <-
            c("Sub Plasma" = Deets$Substrate,
              "Sub Unbound Plasma" = Deets$Substrate,
              "Sub Blood" = Deets$Substrate,
              "Sub PD Response" = "Sub PD Response",
              "Sub (Inb) Plasma" = Deets$Substrate,
              "Sub (Inb) Blood" = Deets$Substrate,
              "Inh 1 Plasma" = Deets$Inhibitor1,
              "Inh 1 Blood" = Deets$Inhibitor1,
              "Sub PM1 Plasma" = Deets$PrimaryMetabolite1,
              "Sub PM1 Blood" = Deets$PrimaryMetabolite1,
              "Adipose (Sub)" = Deets$Substrate,
              "Spinal CSF (Sub)" = Deets$Substrate,
              "Organ Conc" = Deets$Substrate,
              "Organ Conc (Inb)" = Deets$Substrate,
              "Sub SM plasma" = Deets$SecondaryMetabolite,
              "Sub SM blood" = Deets$SecondaryMetabolite,
              "Sub Urine" = Deets$Substrate,
              "Inh 1 Urine" = Deets$Inhibitor1,
              "Met (Sub) Urine" = Deets$Substrate,
              "Sub PM2 Plasma" = Deets$PrimaryMetabolite2,
              "Sub PM2 Blood" = Deets$PrimaryMetabolite2,
              "Inh1 Met Plasma" = Deets$Inhibitor1Metabolite,
              "Inh1 Met Blood" = Deets$Inhibitor1Metabolite,
              "Inh 2 Plasma" = Deets$Inhibitor2,
              "Inh 2 Blood" = Deets$Inhibitor2,
              # "Inh 3 Plasma" = Deets$Inhibitor3, # we haven't set up extractConcTime or extractExpDetails to pull inhibitor 3 yet.
              # "Inh 3 Blood" = Deets$Inhibitor3,
              "Sub (Inb) Urine" = Deets$Substrate,
              "Met(Inh 1) Urine" = Deets$Inhibitor1Metabolite,
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

      AllEffectors_comma <- ifelse(length(AllEffectors) == 0,
                                   NA, str_comma(AllEffectors))

      ObsEffectors <- c("Sub Plasma" = "none",
                        "Sub Unbound Plasma" = "none",
                        "Sub Blood" = "none",
                        "Sub PD Response" = "none",
                        "Sub (Inb) Plasma" = AllEffectors_comma,
                        "Sub (Inb) Blood" = AllEffectors_comma,
                        "Inh 1 Plasma" = AllEffectors_comma,
                        "Inh 1 Blood" = AllEffectors_comma,
                        "Sub PM1 Plasma" = "none",
                        "Sub PM1 Blood" = "none",
                        "Adipose (Sub)" = "none",
                        "Spinal CSF (Sub)" = "none",
                        "Organ Conc" = "none",
                        "Organ Conc (Inb)" = AllEffectors_comma,
                        "Sub SM plasma" = "none",
                        "Sub SM blood" = "none",
                        "Sub Urine" = "none",
                        "Inh 1 Urine" = AllEffectors_comma,
                        "Met (Sub) Urine" = "none",
                        "Sub PM2 Plasma" = "none",
                        "Sub PM2 Blood" = "none",
                        "Inh1 Met Plasma" = AllEffectors_comma,
                        "Inh1 Met Blood" = AllEffectors_comma,
                        "Inh 2 Plasma" = AllEffectors_comma,
                        "Inh 2 Blood" = AllEffectors_comma,
                        # "Inh 3 Plasma" = AllEffectors_comma, # we haven't set up extractConcTime or extractExpDetails to pull inhibitor 3 yet.
                        # "Inh 3 Blood" = AllEffectors_comma,
                        "Sub (Inb) Urine" = "none",
                        "Met(Inh 1) Urine" = AllEffectors_comma,
                        "Inh 1 PD Response" = AllEffectors_comma,
                        "Sub (Inb) PD Response" = AllEffectors_comma,
                        "ADC Plasma Free" = "none",
                        "Conjugated Antibody Plasma Free" = "none",
                        "Conjugated Drug Plasma Free" = "none",
                        "PM1(Sub) PD Response" = "none",
                        "ADC Plasma Total" = "none",
                        "Conjugated Antibody Plasma Total" = "none",
                        "Sub Plasma Total Drug" = "none",
                        "Tumour Volume" = "none",
                        "Tumour Volume (Inb)" = AllEffectors_comma)

      if(TissueType == "systemic" & FromMultFun == FALSE){

            # If the user did not specify a file to use for observed data, use
            # the observed data that they included for the simulation. Note that
            # this will NOT pull the observed data if the user asked for an
            # inhibitor-related compound b/c it's unlikely that that's what
            # observed data they supplied when they set up their simulation.
            if(str_detect(compoundToExtract, "inhibitor") == FALSE){

                  if(is.na(obs_data_file)){

                        StartRow_obs <- which(sim_data_xl$...1 == "Observed Data") + 1

                        if(length(StartRow_obs) != 0){

                              obs_data <-
                                    sim_data_xl[StartRow_obs:nrow(sim_data_xl), ] %>%
                                    t() %>%
                                    as.data.frame()
                              NewNamesObs <- obs_data[1, ]
                              NewNamesObs[str_detect(NewNamesObs, "Time")] <- "Time"
                              # NewNamesObs <- gsub(" |\\: DV [0-9]", "", NewNamesObs)
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
                                                 Inhibitor = "none",
                                                 CompoundID = compoundToExtract,
                                                 Compound = MyCompound, # NOTE THAT THIS IS ASSUMED!
                                                 # The simulator doesn't provide much
                                                 # info on the identity of the
                                                 # compound for the observed data
                                                 # included in a simjlator file.
                                                 Individual = sub("^Subject", "", Individual),
                                                 Time_units = SimTimeUnits,
                                                 Conc_units = SimConcUnits) %>%
                                          select(-ID)
                              )
                        }

                  } else {
                        # If the user did specify an observed data file, read in
                        # observed data.
                        obs_data <- extractObsConcTime(obs_data_file) %>%
                              mutate(Compound = ObsCompounds[CompoundID],
                                     Inhibitor = ObsEffectors[CompoundID] ,
                                     CompoundID = ObsCompoundIDs[CompoundID])

                        # As necessary, convert simulated data units to match the
                        # observed data
                        if("individual" %in% returnAggregateOrIndiv){
                              sim_data_ind <-
                                    match_units(DF_to_adjust = sim_data_ind,
                                                goodunits = obs_data)
                        }

                        if("aggregate" %in% returnAggregateOrIndiv){
                              sim_data_mean <-
                                    match_units(DF_to_adjust = sim_data_mean,
                                                goodunits = obs_data)
                        }
                  }
            }
      }

      if(complete.cases(obs_inhibitor_data_file)){

            obs_eff_data <- extractObsConcTime(
                  obs_data_file = obs_inhibitor_data_file) %>%
                  mutate(Simulated = FALSE, Trial = "obs+inhibitor",
                         Compound = MyCompound,
                         Inhibitor = AllEffectors_comma)

            # As necessary, convert simulated data units to match the observed
            # data.
            if(exists("obs_data", inherits = FALSE)){
                  obs_eff_data <- match_units(DF_to_adjust = obs_eff_data,
                                              goodunits = obs_data)
            } else {
                  # If there was no obs_data_file, then match units in simulated
                  # data to the units in obs_inhibitor_data_file.
                  if("individual" %in% returnAggregateOrIndiv){
                        sim_data_ind <-
                              match_units(DF_to_adjust = sim_data_ind,
                                          goodunits = obs_eff_data)
                  }

                  if("aggregate" %in% returnAggregateOrIndiv){
                        sim_data_mean <-
                              match_units(DF_to_adjust = sim_data_mean,
                                          goodunits = obs_eff_data)
                  }
            }
      }

      if(exists("obs_data", inherits = FALSE)){
            obs_data <- obs_data %>%
                  mutate(Simulated = FALSE,
                         Trial = ifelse(Inhibitor == "none",
                                        "obs", "obs+inhibitor"))

            if(any(is.na(obs_data$Inhibitor)) & length(AllEffectors) == 0){
                  warning("There is a mismatch of some kind between the observed data and the simulated data in terms of an effector or inhibitor being present. Please check that the output from this function looks the way you'd expect. Have you perhaps included observed data with an inhibitor present but the simulation does not include an inhibitor?")
            }

            if(exists("obs_eff_data", inherits = FALSE)){
                  obs_data <- bind_rows(obs_data, obs_eff_data)
            }
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
      }

      Data <- bind_rows(Data)

      if("individual" %in% returnAggregateOrIndiv){
            Data <- Data %>%
                  mutate(Individual = ifelse(is.na(Individual), Trial, Individual),
                         Individual =
                               factor(Individual,
                                      levels = c(
                                            c("obs", "obs+inhibitor", "mean", "median",
                                              "geomean", "per5", "per95", "per10", "per90"),
                                            setdiff(unique(Individual),
                                                    c("obs", "obs+inhibitor", "mean", "median",
                                                      "geomean", "per5", "per95", "per10", "per90")))))

      }

      if(EffectorPresent){
            Data$Inhibitor[Data$Trial == "obs+inhibitor"] <- AllEffectors_comma
      }

      Data <- Data %>%
            mutate(Trial = factor(Trial, levels = c(
                  c("obs", "obs+inhibitor", "mean", "median",
                    "geomean", "per5", "per95", "per10", "per90"),
                  setdiff(unique(Trial),
                          c("obs", "obs+inhibitor", "mean", "median",
                            "geomean", "per5", "per95", "per10", "per90")))),
                  Tissue = tissue,
                  File = sim_data_file) %>%
            arrange(across(any_of(c("Compound", "Inhibitor",
                                    "Individual", "Trial", "Time")))) %>%
            select(any_of(c("Compound", "CompoundID", "Inhibitor", "Tissue",
                            "Individual", "Trial",
                            "Simulated", "Time", "Conc",
                            "Time_units", "Conc_units", "File")))

      # Filtering to return ONLY the compound the user requested. This is what
      # works for input to ct_plot at the moment, too, so things get buggered up
      # if there are multiple compounds and the user called on extractConcTime
      # itself rather than extractConcTime_mult.
      if(FromMultFun == FALSE){
            Data <- Data %>%
                  filter(CompoundID %in% compoundToExtract)
      }

      return(Data)
}

