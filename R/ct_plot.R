#' Concentration-time plots to match Consultancy template
#'
#' Using observed and simulated concentration-time data, make
#' publication-quality graphs that match the consultancy template formatting
#' instructions. A note: The breaks on the x axis are set up to work nicely for
#' time intervals up to 4 weeks; if the time you're monitoring is longer than
#' that, you may want to set \code{return_indiv_graphs = TRUE} and set the x
#' axis breaks yourself.
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
#'   data using the function \code{\link{extractConcTime}}, you can enter the
#'   name of the output data.frame from that function instead of re-reading the
#'   Excel file.
#' @param tissue the tissue to plot. Default is plasma for typical plasma
#'   concentration-time data. Other tissues are acceptable, e.g., "lung",
#'   "brain", etc., as long as the tissue is one of the options included in
#'   "Sheet Options", "Tissues" in the simulator.
#' @param compoundToExtract For which compound do you want to extract
#'   concentration-time data? Options are "substrate" (default), "metabolite 1",
#'   "metabolite 2", or "effector" (this can be either an inducer or inhibitor;
#'   this is labeled as "inhibitor 1" in the simulator).
#' @param figure_type type of figure to plot. Options are:
#'
#'   \describe{
#'
#'   \item{trial means}{plots a black line for the mean data, gray lines for the
#'   mean of each trial of simulated data, and open circles for the observed
#'   data. If an effector was present, gray dashed lines indicate the mean of
#'   each trial of simulated data in the presence of the effector.}
#'
#'   \item{trial percentiles}{plots a black line for the mean data, gray lines
#'   for the 5th and 95th percentiles of the simulated data, and open circles
#'   for the observed data}
#'
#'   \item{means only}{plots a black line for the mean data and, if an effector
#'   was modeled, a dashed line for the concentration-time data with the the
#'   effector.} }
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
#'   \item{"penultimate dose"}{only the time range of the 2nd-to-last dose,
#'   which can be useful for BID data where the end of the simulation extended
#'   past the dosing interval or data when the substrate was dosed BID and the
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
#' @import tidyverse
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
ct_plot_hb <- function (sim_data_file = NA,
                        obs_data_file = NA,
                        obs_effector_data_file = NA,
                        sim_obs_dataframe = NA,
                        tissue = "plasma",
                        compoundToExtract = "substrate",
                        figure_type = "trial means",
                        substrate_or_effector = "substrate",
                        adjust_obs_time = TRUE,
                        time_range = NA,
                        mean_type = "arithmetic",
                        return_data = FALSE,
                        return_indiv_graphs = FALSE)
{
      if (length(figure_type) != 1 | figure_type %in% c("trial means",
                                                        "trial percentiles",
                                                        "means only") == FALSE) {
            stop("The only acceptable options for figure_type are 'trial means', 'trial percentiles', or 'means only'.")
      }
      if (all(complete.cases(time_range)) & class(time_range) ==
          "numeric" & length(time_range) != 2) {
            stop("You must enter a start and stop time for 'time_range', e.g., 'c(0, 24)' or enter 'last dose' to plot only the time range for the last dose.")
      }
      if (all(complete.cases(time_range)) & class(time_range) ==
          "numeric" & time_range[1] >= time_range[2]) {
            stop("The 1st value for 'time_range' must be less than the 2nd value.")
      }
      if (all(complete.cases(time_range)) & class(time_range) ==
          "character" & length(time_range != 1)) {
            time_range <- time_range[1]
      }
      if (all(complete.cases(time_range)) & class(time_range) ==
          "character" & !any(time_range %in% c("last dose",
                                               "first dose", "penultimate dose"))) {
            stop("time_range must be 'first dose', 'last dose', 'penultimate dose', or a numeric time range, e.g., c(12, 24).")
      }
      if (is.data.frame(sim_obs_dataframe)) {
            Data <- sim_obs_dataframe
      } else {
            Data <- extractConcTime(sim_data_file = sim_data_file,
                                    obs_data_file = obs_data_file, tissue = tissue,
                                    compoundToExtract = compoundToExtract,
                                    obs_effector_data_file = obs_effector_data_file,
                                    adjust_obs_time = adjust_obs_time)
      }

      TimeUnits <- sort(unique(Data$Time_units))
      ObsConcUnits <- sort(unique(Data$Conc_units))
      xlab <- switch(TimeUnits, hours = "\nTime (h)", minutes = "\nTime (min)")
      ylab <- switch(ObsConcUnits,
                     `µg/mL` = "Concentration (µg/mL)\n",
                     `ng/mL` = "Concentration (ng/mL)\n",
                     `ng/L` = "Concentration (ng/L)\n",
                     `µM` = expression(Concentration ~ "(" * mu * M * ")"),
                     nM = "Concentration (nM)\n",
                     mg = "Concentration (mg)\n",
                     `mg/L` = expression(Concentration ~ "(" * mu * g/mL * ")"),
                     mL = "mL",
                     `PD response` = "PD response\n")
      tlast <- ifelse(all(complete.cases(time_range)) & length(time_range) ==
                            2, time_range[2], max(Data$Time))
      time_range_input <- time_range
      if (time_range_input[1] %in% c("first dose", "last dose",
                                     "penultimate dose")) {
            Deets <- extractExpDetails(sim_data_file)
            if (Deets$Regimen_sub == "Single Dose") {
                  time_range <- c(0, tlast)
                  warning(paste0("You requested the ", time_range_input[1],
                                 ", but the substrate was administered as a single dose. The full time range will be plotted."))
            }
            else {
                  DayTime_t0 <- difftime_sim(time1 = c(Deets[["SimStartDayTime"]],
                                                       Deets[["SimStartDayTime"]]),
                                             time2 = c(Deets[["StartDayTime_sub"]],
                                                       Deets[["StartDayTime_inhib"]]),
                                             units = TimeUnits)
                  names(DayTime_t0) <- c("Sub", "Inhib")
                  DoseInt <- c(Sub = Deets$DoseInt_sub, Inhib = Deets$DoseInt_inhib)
                  StartDose2 <- DayTime_t0 + DoseInt
                  NumDoses <- c(Sub = Deets$NumDoses_sub, Inhib = Deets$NumDoses_inhib)
                  StartLastDose <- DoseInt * (NumDoses - 1)
                  if (StartLastDose[["Sub"]] == max(Data$Time,
                                                    na.rm = T)) {
                        StartLastDose[["Sub"]] <- StartLastDose[["Sub"]] -
                              DoseInt[["Sub"]]
                  }
                  if (StartLastDose[["Inhib"]] == max(Data$Time,
                                                      na.rm = T)) {
                        StartLastDose[["Inhib"]] <- StartLastDose[["Inhib"]] -
                              DoseInt[["Inhib"]]
                  }
                  if (time_range_input[1] == "first dose") {
                        Start <- ifelse(compoundToExtract == "effector",
                                        DayTime_t0[["Inhib"]], DayTime_t0[["Sub"]])
                        End <- ifelse(compoundToExtract == "effector",
                                      StartDose2[["Inhib"]], StartDose2[["Sub"]])
                        time_range <- c(Start, End)
                        rm(Start, End)
                  }
                  if (time_range_input[1] == "last dose") {
                        Start <- ifelse(compoundToExtract == "effector",
                                        StartLastDose[["Inhib"]], StartLastDose[["Sub"]])
                        End <- max(Data$Time)
                        time_range <- c(Start, End)
                        rm(Start, End)
                  }
                  if (time_range_input[1] == "penultimate dose") {
                        DoseIntToUse <- ifelse(compoundToExtract == "effector",
                                               DoseInt["Inhib"], DoseInt["Sub"])
                        Start <- ifelse(compoundToExtract == "effector",
                                        StartLastDose[["Inhib"]] - DoseIntToUse,
                                        StartLastDose[["Sub"]] - DoseIntToUse)
                        End <- ifelse(compoundToExtract == "effector",
                                      StartLastDose[["Inhib"]], StartLastDose[["Sub"]])
                        time_range <- c(Start, End)
                        rm(Start, End)
                  }
            }
      }
      if (all(complete.cases(time_range)) & (time_range[1] != 0 |
                                             time_range[2] != max(Data$Time))) {
            tlast <- time_range[2] - time_range[1]
            LastDoseTime <- time_range[1]
      }
      tlast <- ifelse(TimeUnits == "hours",
                      round_down_unit(tlast, 4),
                      round_down_unit(tlast, 15))
      if (TimeUnits == "hours") {
            PossBreaks <- data.frame(Tlast = c(12, 24, 48, 96, 168,
                                               336, 360, 504, 672, Inf),
                                     BreaksToUse = c("12hr",
                                                     "24hr", "48hr", "96hr", "1wk",
                                                     "2wk", "15d", "3wk", "4wk",
                                                     "4wkplus"))
            BreaksToUse <- PossBreaks %>% filter(Tlast >= tlast) %>%
                  slice(which.min(Tlast)) %>% pull(BreaksToUse)
            XBreaks <- switch(BreaksToUse,
                              `12hr` = seq(0, 12, 1),
                              `24hr` = seq(0, 24, 2),
                              `48hr` = seq(0, 48, 4),
                              `96hr` = seq(0, 96, 6),
                              `1wk` = seq(0, 168, 12),
                              `2wk` = seq(0, 336, 24),
                              `15d` = seq(0, 360, 24),
                              `3wk` = seq(0, 504, 36),
                              `4wk` = seq(0, 672, 48),
                              `4wkplus` = round_up_nice(seq(0, tlast,
                                                            length.out = 6)))
            XLabels <- XBreaks                                            # HB 25/11/2021
            XLabels[seq(2,length(XLabels),2)] <- ""                       # HB 25/11/2021
      }
      if (TimeUnits == "minutes") {
            PossBreaks <- data.frame(Tlast = c(60, 240, 480, 720,
                                               1440, Inf),
                                     BreaksToUse = c("1hr", "4hr",
                                                     "8hr", "12hr", "24hr", "24hrplus"))
            BreaksToUse <- PossBreaks %>% filter(Tlast >= tlast) %>%
                  slice(which.min(Tlast)) %>% pull(BreaksToUse)
            XBreaks <- switch(BreaksToUse, `1hr` = seq(0, 60, 15),
                              `4hr` = seq(0, 240, 30),
                              `8hr` = seq(0, 480, 60),
                              `12hr` = seq(0, 720, 120),
                              `24hr` = seq(0, 1440, 240),
                              `24hrplus` = round_up_nice(seq(0, tlast,
                                                             length.out = 6)))
            XLabels <- XBreaks                                             # HB 25/11/2021
            XLabels[seq(2,length(XLabels),2)] <- ""                        # HB 25/11/2021
      }
      if (all(complete.cases(time_range)) & time_range[1] != 0) {
            XBreaks <- XBreaks + LastDoseTime
            XLabels <- XBreaks                                            # HB 25/11/2021
            XLabels[seq(2,length(XLabels),2)] <- ""                       # HB 25/11/2021
      }
      MyEffector <- unique(Data$Effector) %>% as.character()
      MyEffector <- MyEffector[!MyEffector == "none"]
      if (complete.cases(MyEffector)) {
            Data <- Data %>%
                  mutate(CompoundIsEffector = Compound == MyEffector,
                         Effector = as.character(ifelse(is.na(Effector),
                                                        "none", Effector)),
                         Effector = tolower(gsub("SV-|Sim-|_EC|_SR|-MD|-SD|-[1-9]00 mg [QMSTBI]{1,2}D|_Fasted Soln|_Fed Capsule",
                                                 "", Effector)),
                         Compound = ifelse(CompoundIsEffector, Effector, Compound),
                         Group = paste(Compound, Effector, Trial)) %>%
                  select(-CompoundIsEffector)
      }
      MyEffector <- tolower(gsub("SV-|Sim-|_EC|_SR|-MD|-SD|-[1-9]00 mg [QMSTBI]{1,2}D|_Fasted Soln|_Fed Capsule",
                                 "", MyEffector))
      Data <- Data %>% mutate(Effector = factor(Effector, levels = c("none",
                                                                     MyEffector)))
      suppressMessages(
            sim_data_trial <- Data %>% filter(Simulated == TRUE &
                                                    Trial %in%
                                                    c("mean", "per5", "per95") == FALSE) %>%
                  group_by(across(any_of(c("Compound", "Tissue", "Effector",
                                           "Simulated", "Trial", "Time",
                                           "Time_units", "Conc_units", "Group")))) %>%
                  summarize(Conc = switch(mean_type,
                                          arithmetic = mean(Conc),
                                          geometric = gm_mean(Conc))) %>%
                  ungroup())
      sim_data_mean <- Data %>%
            filter(Simulated == TRUE & !Trial %in%
                         c("mean", "per5", "per95")) %>%
            group_by(across(any_of(c("Compound",
                                     "Tissue", "Effector", "Simulated",
                                     "Time", "Time_units", "Conc_units")))) %>%
            summarize(mean = switch(mean_type,
                                    arithmetic = mean(Conc, na.rm = TRUE),
                                    geometric = gm_mean(Conc)),
                      per5 = quantile(Conc, 0.05),
                      per95 = quantile(Conc, 0.95)) %>%
            pivot_longer(cols = c("mean", "per5", "per95"),
                         names_to = "Trial", values_to = "Conc") %>%
            mutate(Group = paste(Compound,
                                 Effector, Trial))
      obs_data <- Data %>% filter(Simulated == FALSE) %>% droplevels()
      if (is.na(time_range_input[1])) {
            time_range <- range(Data$Time, na.rm = T)
      }

      # HB 25/11/2021:
      if (figure_type == "trial means") {
            Ylim_data <- bind_rows(sim_data_trial, obs_data)
      } else if (figure_type == "trial percentiles") {
            Ylim_data <- bind_rows(sim_data_trial, sim_data_mean, obs_data)
      } else if (figure_type == "means only") {
            Ylim_data <- sim_data_mean %>% filter(Trial == "mean") }

      Ylim <- Ylim_data %>% filter(Time >= time_range[1] &
                                         Time <= time_range[2] &
                                         complete.cases(Conc)) %>%
            pull(Conc) %>% range()

      PossYBreaks <- data.frame(Ymax = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50,
                                         100, 200, 500, 1000, 2000, 5000,
                                         10000, 20000, 50000, 100000,
                                         200000, 500000, Inf),
                                YBreaksToUse = c(0.02, 0.05, 0.1, 0.2, 0.5,
                                                 1, 2, 5, 10, 20, 50, 100, 200,
                                                 500, 1000, 2000, 5000, 10000,
                                                 20000, 50000, 100000, 200000))

      YBreaksToUse <- PossYBreaks %>% filter(Ymax >= Ylim[2]) %>%
            slice(which.min(Ymax)) %>% pull(YBreaksToUse)

      YInterval    <- YBreaksToUse
      YmaxRnd      <- round_up_unit(Ylim[2], YInterval)
      YBreaks      <- seq(0, YmaxRnd, YInterval/2)                    # create labels at major and minor points
      YLabels      <- YBreaks
      YLabels[seq(2,length(YLabels),2)] <- ""                         # add blank labels at every other point i.e. for just minor tick marks at every other point

      if (figure_type == "trial means") {
            NumTrials <- length(unique(sim_data_trial$Trial))
            AlphaToUse <- ifelse(NumTrials > 10, 0.05, 0.4)
            if (complete.cases(MyEffector) & MyEffector[1] != "none") {
                  A <- ggplot(sim_data_trial, aes(x = Time, y = Conc,
                                                  group = Group,
                                                  linetype = Effector,
                                                  shape = Effector)) +
                        geom_line(alpha = AlphaToUse, lwd = 1) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"), lwd = 1) +
                        geom_point(data = obs_data, size = 2) +
                        scale_shape_manual(values = c(21, 24))
            } else {
                  A <- ggplot(sim_data_trial, aes(x = Time, y = Conc,
                                                  group = Trial)) +
                        geom_line(alpha = AlphaToUse, lwd = 1) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"), lwd = 1) +
                        geom_point(data = obs_data, size = 2, shape = 21)
            }
      }
      if (figure_type == "trial percentiles") {
            if (complete.cases(MyEffector) & MyEffector[1] != "none") {
                  A <- ggplot(sim_data_mean %>%
                                    filter(Trial %in% c("per5", "per95")) %>%
                                    mutate(Group = paste(Group, Trial)),
                              aes(x = Time, y = Conc, linetype = Effector,
                                  shape = Effector, group = Group)) +
                        geom_line(color = "gray80", lwd = 0.8) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"), lwd = 1) +
                        geom_point(data = obs_data,size = 2) +
                        scale_shape_manual(values = c(21,24))
            } else {
                  A <- ggplot(sim_data_mean %>% filter(Trial != "mean"),
                              aes(x = Time, y = Conc, group = Trial)) +
                        geom_line(color = "gray80", lwd = 0.8) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"), lwd = 1) +
                        geom_point(data = obs_data, size = 2, shape = 21)
            }
      }
      if (figure_type == "means only") {
            if (complete.cases(MyEffector) & MyEffector[1] != "none") {
                  A <- ggplot(sim_data_mean %>% filter(Trial == "mean") %>%
                                    mutate(Group = paste(Group, Trial)),
                              aes(x = Time, y = Conc, linetype = Effector)) +
                        geom_line(lwd = 1)
            } else {
                  A <- ggplot(sim_data_mean %>% filter(Trial == "mean"),
                              aes(x = Time, y = Conc)) + geom_line(lwd = 1)
            }
      }
      A <- A +
            scale_x_continuous(breaks = XBreaks, labels = XLabels,
                               expand = expansion(mult = c(0, 0.04))) +
            # scale_y_continuous(limits = c(0, YmaxRnd), breaks = YBreaks, labels = YLabels, expand = expansion(mult = c(0, 0.1))) +
            scale_y_continuous(limits = c(0, YmaxRnd), breaks = YBreaks,
                               labels = YLabels) +
            labs(x = xlab, y = ylab) + coord_cartesian(xlim = time_range) +
            theme(panel.background = element_rect(fill = "white", color = NA),
                  axis.ticks = element_line(color = "black"),
                  axis.text = element_text(color = "black", size = 12),
                  axis.title = element_text(color = "black", face = "bold"),
                  axis.line.x.bottom = element_line(color = "black"),
                  axis.line.y.left = element_line(color = "black"),
                  text = element_text(family = "Calibri"),
                  legend.position = "none")
      # if (compoundToExtract == "effector") {
      # A <- A + theme(legend.position = "none")
      # }
      Ylim_log <- Ylim
      # if (Ylim[1] == 0 | Ylim[1] < mean(Data$Conc, na.rm = TRUE) *
      #     0.01) {
      #     Ylim_log[1] <- bind_rows(sim_data_trial, sim_data_mean,
      #         obs_data) %>% filter(Time > switch(TimeUnits, hours = 4,
      #         minutes = 4 * 60) & complete.cases(Conc) & Conc >
      #         0) %>% pull(Conc) %>% min() * 0.8
      # }
      #

      near_match <- function(x, t) {x[which.min(abs(t - x))]}

      Ylim_log[1] <- Ylim_data %>%
            filter(Time == near_match(Ylim_data$Time, time_range[2])) %>%
            pull(Conc) %>% min()

      Ylim_log[2] <- round_up(Ylim[2])
      Ylim_log[1] <- round_down(Ylim_log[1])

      YLogBreaks <- as.vector(outer(1:9, 10^(log10(Ylim_log[1]):log10(Ylim_log[2]))))
      YLogBreaks <- YLogBreaks[YLogBreaks >= Ylim_log[1] & YLogBreaks <= Ylim_log[2]]
      YLogLabels   <- rep("",length(YLogBreaks))
      YLogLabels[seq(1,length(YLogLabels),9)] <- YLogBreaks[seq(1,length(YLogLabels),9)]    # add labels at order of magnitude

      B <- suppressMessages(A + scale_y_log10(limits = Ylim_log,
                                              breaks = YLogBreaks, labels = YLogLabels)+
                                  # labels = function(.) format(., scientific = FALSE, drop0trailing = TRUE)) +
                                  coord_cartesian(xlim = time_range))
      if (compoundToExtract == "effector") {
            AB <- suppressWarnings(ggpubr::ggarrange(A, B, ncol = 1,
                                                     labels = c("A", "B"),
                                                     align = "v"))
      }
      else {
            AB <- suppressWarnings(ggpubr::ggarrange(A, B, ncol = 1,
                                                     labels = c("A", "B"),
                                                     common.legend = FALSE,
                                                     legend = "none", align = "v"))
      }
      Out <- list(Graphs = AB)
      if (return_data) {
            Out[["Data"]] <- Data
      }
      if (return_indiv_graphs) {
            Out[["Linear graph"]] <- A
            Out[["Semi-log graph"]] <- B
      }
      if (length(Out) == 1) {
            Out <- Out[[1]]
      }
      return(Out)
}



