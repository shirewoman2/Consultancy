#' Using observed and simulated concentration-time data, make
#' publication-quality graphs.
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   concentration-time data
#' @param obs_data_file name of the Excel file containing the observed
#'   concentration-time data. If the observed data you want to plot were already
#'   included in the Excel output from the simulator, leave this as NA.
#'   Otherwise, this is the file that it is ready to be converted to an XML
#'   file, not the file that contains only the digitized time and concentration
#'   data.
#' @param obs_effector_data_file name of the Excel file containing the observed
#'   concentration-time data in the presence of an effector. This is the file
#'   that is ready to be converted to an XML file.
#' @param sim_obs_dataframe If you have already extracted the concentration-time
#'   data using the function \code{extractConcTime}, you can enter the name of
#'   the output data.frame from that function instead of re-reading the Excel
#'   file.
#' @param tissue the tissue to plot. Default is plasma for typical plasma
#'   concentration-time data. Other tissues are acceptable, e.g., "lung",
#'   "brain", etc., as long as the tissue is one of the options included in
#'   "Sheet Options", "Tissues" in the simulator.
#' @param figure_type type of figure to plot. Options are:
#'
#'   \describe{
#'
#'   \item{trial means}{plots a black line for the mean data, gray lines for the
#'   mean of each trial of simulated data, and open circles for the observed
#'   data. If an effector was present, gray dashed lines indicate the mean of
#'   each trial of simulated data in the presence of the effector. (At present,
#'   if an effector were present, this does NOT graph observed data because I
#'   haven't yet figured out the best way to indicate whether those data are
#'   with or without the effector. -LS)}
#'
#'   \item{trial percentiles}{plots a black line for the mean data, gray lines
#'   for the 5th and 95th percentiles of the simulated data, and open circles
#'   for the observed data}
#'
#'   \item{means only}{plots a black line for the mean data and, if an effector
#'   was modeled, a dashed line for the concentration-time data with the the
#'   effector. (At present, if an effector were present, this does NOT graph
#'   observed data because I haven't yet figured out the best way to indicate
#'   whether those data are with or without the effector. -LS)}
#'
#'   }
#' @param substrate_or_effector "substrate" or "effector" for whether to plot
#'   the substrate (default) or the effector concentration-time data
#' @param adjust_obs_time TRUE or FALSE: Adjust the time listed in the observed
#'   data file to match the last dose administered? This only applies to
#'   multiple-dosing regimens. If TRUE, the graph will show the observed data
#'   overlaid with the simulated data such that the dose in the observed data
#'   was administered at the same time as the last dose in the simulated data.
#'   If FALSE, the observed data will start at whatever times are listed in the
#'   Excel file.
#' @param time_range time range to graph. Options: \describe{
#'
#'   \item{NA}{entire time range of data}
#'
#'   \item{a start time and end time in hours}{only data in that time range,
#'   e.g. \code{c(24, 48)}}
#'
#'   \item{"first dose"}{only the time range of the first dose}
#'
#'   \item{"last dose"}{only the time range of the last dose}
#'
#'   \item{"penultimate dose"}{only the time range of the 2nd-to-last dose, which
#'   can be useful for BID data where the end of the simulation extended past
#'   the dosing interval or data when the substrate was dosed BID and the
#'   effector was dosed QD} }
#'
#' @param mean_type "geometric" or "arithmetic" for which type of mean to
#'   calculate
#' @param return_data TRUE or FALSE: Return the data used in the graphs? If
#'   TRUE, this will return a named list of: \describe{ \item{Graphs}{the set of
#'   graphs} \item{Data}{a data.frame of the concentration-time data used in the
#'   set of graphs} }
#' @param return_indiv_graphs TRUE or FALSE: Return each of the two individual
#'   graphs? This can be useful if you want to modify the graphs further or only
#'   use one, etc.
#'
#' @return Depending on the options selected, returns either a set of graphs or
#'   a list of the set of graphs (named "Graphs" in the  output), the individual
#'   graphs ("Linear graph" and "Semi-log graph"), and/or the data used for
#'   creating the graphs ("Data").
#'
#' @export
#'
#' @examples
#' sim_data_file <- "../Example simulator output.xlsx"
#' obs_data_file <- "../fig1-242-06-001-MD - for XML conversion.xlsx"
#'
#' ct_plot(sim_data_file)
#' ct_plot(sim_data_file, figure_type = "trial percentiles")
#' ct_plot(sim_data_file, return_data = TRUE)
#' ct_plot(sim_data_file, return_indiv_graphs = TRUE)
#'
#' # Here's where adjusting or not adjusting the observed data time comes
#' # into play:
#' ct_plot(sim_data_file = "../Example simulator output MD.xlsx",
#'         obs_data_file = "../fig1-242-06-001-MD - for XML conversion.xlsx")
#' ct_plot(sim_data_file = "../Example simulator output MD.xlsx",
#'         obs_data_file = "../fig1-242-06-001-SD - for XML conversion.xlsx",
#'         adjust_obs_time = FALSE)
#'
#' # Perhaps you don't want to show *all* the data but instead want to
#' # limit the time interval that is graphed. Use \code{time_range} here:
#' ct_plot(sim_data_file = "../Example simulator output MD.xlsx",
#'         obs_data_file = "../fig1-242-06-001-SD - for XML conversion.xlsx",
#'         adjust_obs_time = FALSE, time_range = c(0, 24))
#'
#' # Or you can let it automatically calculate the time frame for the first or
#' # last dose simulated
#' ct_plot(sim_data_file = "../Example simulator output MD.xlsx",
#'         obs_data_file = "../fig1-242-06-001-SD - for XML conversion.xlsx",
#'         time_range = "first dose")
#' ct_plot(sim_data_file = "../Example simulator output MD.xlsx",
#'         obs_data_file = "../fig1-242-06-001-MD - for XML conversion.xlsx",
#'         time_range = "last dose")
#'
#' # These may be too busy when you've got an effector present:
#' ct_plot(sim_data_file = "../Example simulator output MD + inhibitor.xlsx")
#' ct_plot(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'         figure_type = "trial percentiles")
#' # so you may want to consider only plotting means as an alternative:
#' ct_plot(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'         figure_type = "means only")
#'
#' # You can also add a separate observed concentration-time file for the
#' # compound with the effector present. The graph will show those points
#' # as open triangles.
#' ct_plot(sim_data_file = "../Example simulator output MD.xlsx",
#'         obs_data_file = "../fig1-242-06-001-SD - for XML conversion.xlsx",
#'         obs_effector_data_file = "../Mallikaarjun_2016_RTV-fig1-100mg-BID-DLM+Kaletra - for XML conversion.xlsx",
#'         time_range = "last dose")
#'
#' # If you've already got a data.frame formatted like the output
#' # from extractConcTime...
#' data(ConcTime)
#' ct_plot(sim_obs_dataframe = ConcTime)
#'
ct_plot <- function(sim_data_file,
                    obs_data_file = NA,
                    obs_effector_data_file = NA,
                    sim_obs_dataframe = NA,
                    tissue = "plasma",
                    figure_type = "trial means",
                    substrate_or_effector = "substrate",
                    adjust_obs_time = TRUE,
                    time_range = NA,
                    mean_type = "arithmetic",
                    return_data = FALSE,
                    return_indiv_graphs = FALSE){

      # Error catching
      if(length(figure_type) != 1 |
         figure_type %in% c("trial means", "trial percentiles",
                            "means only") == FALSE){
            stop("The only acceptable options for figure_type are 'trial means', 'trial percentiles', or 'means only'.")
      }

      if(all(complete.cases(time_range)) & class(time_range) == "numeric" &
         length(time_range) != 2){
            stop("You must enter a start and stop time for 'time_range', e.g., 'c(0, 24)' or enter 'last dose' to plot only the time range for the last dose.")
      }

      if(all(complete.cases(time_range)) & class(time_range) == "numeric" &
         time_range[1] >= time_range[2]){
            stop("The 1st value for 'time_range' must be less than the 2nd value.")
      }

      if(all(complete.cases(time_range)) & class(time_range) == "character" &
         length(time_range != 1)){
            time_range <- time_range[1]
      }

      if(all(complete.cases(time_range)) & class(time_range) == "character" &
         !any(time_range %in% c("last dose", "first dose", "penultimate dose"))){
            stop("time_range must be 'first dose', 'last dose', 'penultimate dose', or a numeric time range, e.g., c(12, 24).")
      }


      # Extract the data to plot
      if(is.data.frame(sim_obs_dataframe)){
            Data <- sim_obs_dataframe
      } else {
            Data <- extractConcTime(sim_data_file = sim_data_file,
                                    obs_data_file = obs_data_file,
                                    tissue = tissue,
                                    obs_effector_data_file = obs_effector_data_file,
                                    adjust_obs_time = adjust_obs_time)
      }

      if("Effector" %in% names(Data)){
            Data$Effector[is.na(Data$Effector)] <- "none"
      }

      TimeUnits <- sort(unique(Data$Time_units))
      ObsConcUnits <- sort(unique(Data$Conc_units))

      # Adjusting graph labels as appropriate for the observed data
      xlab <- switch(TimeUnits,
                     "hours" = "Time (hr)",
                     "minutes" = "Time (min)")

      ylab <- switch(ObsConcUnits,
                     "µg/mL" = expression(Concentration~"("*mu*g/mL*")"),
                     "ng/mL" = "Concentration (ng/mL)",
                     "ng/L" = "Concentration (ng/L)",
                     "µM" = expression(Concentration~"("*mu*M*")"),
                     "nM" = "Concentration (nM)",
                     "mg" = "Concentration (mg)",
                     "mg/L" = expression(Concentration~"("*mu*g/mL*")"),
                     "mL" = "mL",
                     "PD response" = "PD response")

      # Setting the breaks for the x axis
      tlast <- ifelse(all(complete.cases(time_range)) &
                            length(time_range) == 2,
                      time_range[2], max(Data$Time))

      time_range_input <- time_range

      if(time_range_input[1] %in% c("first dose", "last dose", "penultimate dose")){
            Deets <- extractExpDetails(sim_data_file)

            Sub_t0 <- str_split(Deets[["StartDayTime_sub"]], ", ")[[1]]
            Inhib_t0 <- str_split(Deets[["StartDayTime_inhib"]], ", ")[[1]]

            Day_t0 <- as.numeric(sub("Day ", "", c(Sub_t0[1], Inhib_t0[1])))
            names(Day_t0) <- c("Sub", "Inhib")

            # t0 for first dose of substrate and inhibitor in hours
            DayTime_t0 <-
                  as.numeric((hms::parse_hm(c(Sub_t0[2], Inhib_t0[2])) + 60*60*24*Day_t0)/(60*60))
            DayTime_t0 <- DayTime_t0 - DayTime_t0[which.min(DayTime_t0)]
            names(DayTime_t0) <- c("Sub", "Inhib")

            if(TimeUnits == "minutes"){
                  DayTime_t0 <- DayTime_t0 * 60
            }

            # start of 2nd dose of substrate and inhibitor
            DoseInt <- c("Sub" = Deets$DoseInt_sub,
                         "Inhib" = Deets$DoseInt_inhib)
            StartDose2 <- DayTime_t0 + DoseInt

            # start of last dose simulated of substrate and inhibitor
            NumDoses <- c("Sub" = Deets$NumDoses_sub,
                          "Inhib" = Deets$NumDoses_inhib)
            StartLastDose <- DoseInt * NumDoses
            if(any(StartLastDose == max(Data$Time), na.rm = T)){
                  StartLastDose <- StartLastDose - DoseInt
            }

            if(time_range_input[1] == "first dose"){
                  Start <- ifelse(substrate_or_effector == "substrate",
                                  DayTime_t0[["Sub"]],
                                  DayTime_t0[["Inhib"]])
                  End <- ifelse(substrate_or_effector == "substrate",
                                StartDose2[["Sub"]],
                                StartDose2[["Inhib"]])
                  time_range <- c(Start, End)
                  rm(Start, End)
            }

            if(time_range_input[1] == "last dose"){
                  Start <- ifelse(substrate_or_effector == "substrate",
                                  StartLastDose[["Sub"]],
                                  StartLastDose[["Inhib"]])
                  End <- max(Data$Time)
                  time_range <- c(Start, End)
                  rm(Start, End)
            }

            if(time_range_input[1] == "penultimate dose"){

                  DoseIntToUse <- ifelse(substrate_or_effector == "substrate",
                                         DoseInt["Sub"], DoseInt["Inhib"])

                  Start <- ifelse(substrate_or_effector == "substrate",
                                  StartLastDose[["Sub"]] - DoseIntToUse,
                                  StartLastDose[["Inhib"]] - DoseIntToUse)
                  End <- ifelse(substrate_or_effector == "substrate",
                                StartLastDose[["Sub"]],
                                StartLastDose[["Inhib"]])
                  time_range <- c(Start, End)

                  rm(Start, End)
            }
      }

      # This doesn't work well if the time range starts at something other than
      # 0 or ends somewhere other than the max time, so adjusting for that situation.
      if(all(complete.cases(time_range)) &
         (time_range[1] != 0 | time_range[2] != max(Data$Time))){

            tlast <- time_range[2] - time_range[1]
            LastDoseTime <- time_range[1]

      }

      # If tlast is just a smidge over one of the possible breaks I've set, it
      # goes to the next one and doesn't look as nice on the graph. Rounding
      # tlast down to the nearest 4 for hours and nearest 15 for minutes.
      tlast <- ifelse(TimeUnits == "hours",
                      round_down_unit(tlast, 4),
                      round_down_unit(tlast, 15))

      if(TimeUnits == "hours"){

            PossBreaks <- data.frame(
                  Tlast = c(12, 24, 48, 96, 168, 336, 360, 504, 672),
                  BreaksToUse = c("12hr", "24hr", "48hr", "96hr", "1wk", "2wk",
                                  "15d", "3wk", "4wk"))

            BreaksToUse <- PossBreaks %>% filter(Tlast >= tlast) %>%
                  slice(which.min(Tlast)) %>% pull(BreaksToUse)

            XBreaks <- switch(BreaksToUse,
                              "12hr" = seq(0, 12, 2),
                              "24hr" = seq(0, 24, 4),
                              "48hr" = seq(0, 48, 8),
                              "96hr" = seq(0, 96, 12),
                              "1wk" = seq(0, 168, 24),
                              "2wk" = seq(0, 336, 48),
                              "15d" = seq(0, 360, 48),
                              "3wk" = seq(0, 504, 72),
                              "4wk" = seq(0, 672, 96))
      }

      if(TimeUnits == "minutes"){
            PossBreaks <- data.frame(Tlast = c(60, 240, 480, 720, 1440),
                                     BreaksToUse = c("1hr", "4hr",
                                                     "8hr", "12hr",
                                                     "24hr"))
            BreaksToUse <- PossBreaks %>% filter(Tlast >= tlast) %>%
                  slice(which.min(Tlast)) %>% pull(BreaksToUse)

            XBreaks <- switch(BreaksToUse,
                              "1hr" = seq(0, 60, 15),
                              "4hr" = seq(0, 240, 30),
                              "8hr" = seq(0, 480, 60),
                              "12hr" = seq(0, 720, 120),
                              "24hr" = seq(0, 1440, 240))
      }

      # Adjusting the breaks when time_range[1] isn't 0
      if(all(complete.cases(time_range)) & time_range[1] != 0){
            XBreaks <- XBreaks + LastDoseTime
      }


      # Adding a grouping variable to data and also making the effector name
      # prettier for the graphs.
      if("Effector" %in% names(Data)){
            Data <- Data %>%
                  mutate(Effector = tolower(gsub(
                        "SV-|Sim-|_EC|_SR|-MD|-SD|-[1-9]00 mg [QMSTBI]{1,2}D|_Fasted Soln|_Fed Capsule",
                        "",
                        Effector)),
                        Group = paste(Compound, Effector, Trial))
      } else {
            Data <- Data %>% mutate(Group = paste(Compound, Trial))
      }

      # Separating the data by type and calculating trial means
      suppressMessages(
            sim_data_ind <- Data %>%
                  filter(Simulated == TRUE &
                               Trial %in% c("mean", "per5", "per95") == FALSE) %>%
                  group_by(across(any_of(c("Compound", "Tissue", "Effector", "Simulated", "Trial",
                                           "Time", "Time_units", "Conc_units", "Group")))) %>%
                  summarize(Conc = switch(mean_type,
                                          "arithmetic" = mean(Conc),
                                          "geometric" = gm_mean(Conc))) %>%
                  ungroup()
      )

      sim_data_mean <- Data %>%
            filter(Simulated == TRUE  &
                         Trial %in% c("mean", "per5", "per95"))

      obs_data <- Data %>% filter(Simulated == FALSE)

      # Setting the time range since I use it later.
      if(is.na(time_range_input[1])){
            time_range <- range(Data$Time, na.rm = T)
      }

      if(figure_type == "trial means"){

            NumTrials <- length(unique(sim_data_ind$Trial))
            AlphaToUse <- ifelse(NumTrials > 10, 0.05, 0.12)
            # Adjust this as needed. May want to use "switch" as we did for XBreaks.

            if("Effector" %in% names(sim_data_ind)){

                  MyEffector <- sim_data_ind %>% filter(Compound == Effector) %>%
                        pull(Compound) %>% unique()

                  if(substrate_or_effector == "substrate"){

                        Ylim <- bind_rows(sim_data_ind, obs_data) %>%
                              filter(Compound != MyEffector &
                                           Time >= time_range[1] &
                                           Time <= time_range[2] &
                                           complete.cases(Conc)) %>%
                              pull(Conc) %>% range()

                  } else {

                        Ylim <- bind_rows(sim_data_ind, obs_data) %>%
                              filter(Compound == MyEffector &
                                           Time >= time_range[1] &
                                           Time <= time_range[2] &
                                           complete.cases(Conc)) %>%
                              pull(Conc) %>% range()
                  }

                  ## linear plot
                  if(substrate_or_effector == "substrate"){
                        A <- ggplot(sim_data_ind %>% filter(Compound != MyEffector),
                                    aes(x = Time, y = Conc, group = Group,
                                        linetype = Effector, shape = Effector)) +
                              guides(linetype = guide_legend(
                                    override.aes=list(fill=c(NA, NA)))) +
                              geom_line(alpha = AlphaToUse, lwd = 1) +
                              geom_line(data = sim_data_mean %>%
                                              filter(Trial == "mean" &
                                                           Compound != MyEffector),
                                        lwd = 1) +
                              geom_point(data = obs_data, size = 2) +
                              scale_shape_manual(values = c(21, 24))

                  } else {
                        A <- ggplot(sim_data_ind %>% filter(Compound == MyEffector),
                                    aes(x = Time, y = Conc, group = Group)) +
                              geom_line(alpha = AlphaToUse, lwd = 1) +
                              geom_line(data = sim_data_mean %>%
                                              filter(Trial == "mean" &
                                                           Compound == MyEffector),
                                        lwd = 1) +
                              geom_point(data = obs_data %>%
                                               filter(Compound == MyEffector),
                                         size = 2, shape = 21)
                  }


            } else {

                  Ylim <- bind_rows(sim_data_ind, obs_data) %>%
                        filter(Time >= time_range[1] &
                                     Time <= time_range[2] &
                                     complete.cases(Conc)) %>%
                        pull(Conc) %>% range()

                  ## linear plot
                  A <- ggplot(sim_data_ind,
                              aes(x = Time, y = Conc, group = Trial)) +
                        geom_line(alpha = AlphaToUse, lwd = 1) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"),
                                  lwd = 1) +
                        geom_point(data = obs_data, size = 2, shape = 21)

            }

      }

      if(figure_type == "trial percentiles"){

            # graphs with 95% confidence interval

            if("Effector" %in% names(sim_data_mean)){

                  MyEffector <- sim_data_mean %>%
                        filter(Compound == Effector) %>%
                        pull(Compound) %>% unique()

                  if(substrate_or_effector == "substrate"){

                        Ylim <- bind_rows(sim_data_mean, obs_data) %>%
                              filter(Compound != MyEffector &
                                           Time >= time_range[1] &
                                           Time <= time_range[2] &
                                           complete.cases(Conc)) %>%
                              pull(Conc) %>% range()

                  } else {

                        Ylim <- bind_rows(sim_data_mean, obs_data) %>%
                              filter(Compound == MyEffector &
                                           Time >= time_range[1] &
                                           Time <= time_range[2] &
                                           complete.cases(Conc)) %>%
                              pull(Conc) %>% range()

                  }

                  if(substrate_or_effector == "substrate"){

                        A <- ggplot(sim_data_mean %>%
                                          filter(Trial %in% c("per5", "per95") &
                                                       Compound != MyEffector) %>%
                                          mutate(Group = paste(Group, Trial)),
                                    aes(x = Time, y = Conc,
                                        linetype = Effector, shape = Effector,
                                        group = Group)) +
                              geom_line(color = "gray80", lwd = 0.8) +
                              geom_line(data = sim_data_mean %>%
                                              filter(Trial == "mean" &
                                                           Compound != MyEffector),
                                        lwd = 1) +
                              geom_point(data = obs_data, size = 2) +
                              scale_shape_manual(values = c(21, 24))

                  } else {
                        A <- ggplot(sim_data_mean %>%
                                          filter(Trial %in% c("per5", "per95") &
                                                       Compound == MyEffector) %>%
                                          mutate(Group = paste(Group, Trial)),
                                    aes(x = Time, y = Conc,
                                        group = Group)) +
                              geom_line(color = "gray80", lwd = 0.8) +
                              geom_line(data = sim_data_mean %>%
                                              filter(Trial == "mean" &
                                                           Compound == MyEffector),
                                        lwd = 1) +
                              geom_point(data = obs_data, size = 2, shape = 21)
                  }

            } else {

                  ## linear plot

                  Ylim <- bind_rows(sim_data_mean, obs_data) %>%
                        filter(Time >= time_range[1] &
                                     Time <= time_range[2] &
                                     complete.cases(Conc)) %>%
                        pull(Conc) %>% range()

                  A <- ggplot(sim_data_mean %>% filter(Trial != "mean"),
                              aes(x = Time, y = Conc, group = Trial)) +
                        geom_line(color = "gray80", lwd = 0.8) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"), lwd = 1) +
                        geom_point(data = obs_data, size = 2, shape = 21)
            }

      }

      if(figure_type == "means only"){

            if("Effector" %in% names(sim_data_mean)){

                  MyEffector <- sim_data_mean %>%
                        filter(Compound == Effector) %>%
                        pull(Compound) %>% unique()

                  if(substrate_or_effector == "substrate"){

                        Ylim <- sim_data_mean %>%
                              filter(Compound != MyEffector &
                                           Time >= time_range[1] &
                                           Time <= time_range[2] &
                                           complete.cases(Conc)) %>%
                              pull(Conc) %>% range()

                  } else {

                        Ylim <- sim_data_mean %>%
                              filter(Compound == MyEffector &
                                           Time >= time_range[1] &
                                           Time <= time_range[2] &
                                           complete.cases(Conc)) %>%
                              pull(Conc) %>% range()

                  }


                  if(substrate_or_effector == "substrate"){
                        A <- ggplot(sim_data_mean %>%
                                          filter(Trial == "mean" &
                                                       Compound != MyEffector) %>%
                                          mutate(Group = paste(Group, Trial)),
                                    aes(x = Time, y = Conc, linetype = Effector)) +
                              geom_line(lwd = 1)

                  } else {
                        A <- ggplot(sim_data_mean %>%
                                          filter(Trial == "mean" &
                                                       Compound == MyEffector) %>%
                                          mutate(Group = paste(Group, Trial)),
                                    aes(x = Time, y = Conc)) +
                              geom_line(lwd = 1)
                  }

            } else {

                  Ylim <- sim_data_mean %>%
                        filter(Time >= time_range[1] &
                                     Time <= time_range[2] &
                                     complete.cases(Conc)) %>%
                        pull(Conc) %>% range()

                  A <- ggplot(sim_data_mean %>%
                                    filter(Trial == "mean"),
                              aes(x = Time, y = Conc)) +
                        geom_line(lwd = 1)

            }
      }

      A <- A +
            scale_x_continuous(breaks = XBreaks,
                               expand = expansion(mult = c(0, 0.04))) +
            # scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
            #                    limits = Ylim) +
            labs(x = xlab, y = ylab) +
            coord_cartesian(xlim = time_range) +
            theme(panel.background = element_rect(fill="white", color=NA),
                  legend.key = element_rect(fill = "white"),
                  axis.line.x.bottom = element_line(color = "grey60"),
                  axis.line.y.left = element_line(color = "grey60"),
                  axis.ticks = element_line(color = "grey60"),
                  text = element_text(family = "Calibri")
            )

      # # Freddy's original graphing code:
      # A <- ## normal scale plot
      #       ggplot(sim_data, aes(x = Time)) +
      #       geom_ribbon(aes(ymin = per5, ymax = per95), alpha = 0.2) +
      #       geom_line(aes(y = mean), lwd = 1.2) +
      #       geom_point(data = obs_data, aes(x = Time, y = Conc),
      #                  size = 2, shape = 21, fill = "white", alpha = 0.8) +
      #       labs(x = xlab, y = ylab) +
      #       theme_bw()
      # B <- ## semi-log scale plot
      #       A +
      #       scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
      #                     labels = scales::trans_format("log10", scales::math_format(10^.x)))


      ## semi-log plot
      B <- suppressMessages(
            A + scale_y_log10(labels = scales::comma) +
                  coord_cartesian(xlim = time_range,
                                  ylim = c(ifelse(Ylim[1] == 0, 1, Ylim),
                                           Ylim[2]))
      )

      # both plots together, aligned vertically
      AB <- suppressWarnings(
            ggpubr::ggarrange(A, B, ncol = 1, labels = c("A", "B"),
                              common.legend = TRUE, legend = "right",
                              align = "v")
      )

      if(return_data){
            Out <- list(AB, Data)
            names(Out) <- c("Graphs", "Data")

            if(return_indiv_graphs){
                  Out[["Linear graph"]] <- A
                  Out[["Semi-log graph"]] <- B
            }

      } else {

            if(return_indiv_graphs){
                  Out <- list(AB)
                  names(Out) <- "Graphs"
                  Out[["Linear graph"]] <- A
                  Out[["Semi-log graph"]] <- B
            } else {
                  Out <- AB
            }
      }

      return(Out)

}




