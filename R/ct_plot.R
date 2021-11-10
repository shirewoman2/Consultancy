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
#'   or "metabolite 2". (Note: If you want inhibitor concentration-time data,
#'   those show up with the substrate, so enter "substrate" here.)
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
ct_plot <- function(sim_data_file,
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
                                    compoundToExtract = compoundToExtract,
                                    obs_effector_data_file = obs_effector_data_file,
                                    adjust_obs_time = adjust_obs_time)
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

            if(Deets$Regimen_sub == "Single Dose"){
                  time_range <- c(0, tlast)
                  warning(paste0("You requested the ", time_range_input[1],
                                 ", but the substrate was administered as a single dose. The full time range will be plotted."))
            } else {

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
                        Start <- ifelse(compoundToExtract == "effector",
                                        DayTime_t0[["Inhib"]],
                                        DayTime_t0[["Sub"]])
                        End <- ifelse(compoundToExtract == "effector",
                                      StartDose2[["Inhib"]],
                                      StartDose2[["Sub"]])
                        time_range <- c(Start, End)
                        rm(Start, End)
                  }

                  if(time_range_input[1] == "last dose"){
                        Start <- ifelse(compoundToExtract == "effector",
                                        StartLastDose[["Inhib"]],
                                        StartLastDose[["Sub"]])
                        End <- max(Data$Time)
                        time_range <- c(Start, End)
                        rm(Start, End)
                  }

                  if(time_range_input[1] == "penultimate dose"){
                        DoseIntToUse <- ifelse(compoundToExtract == "effector",
                                               DoseInt["Inhib"], DoseInt["Sub"])

                        Start <- ifelse(compoundToExtract == "effector",
                                        StartLastDose[["Inhib"]] - DoseIntToUse,
                                        StartLastDose[["Sub"]] - DoseIntToUse)
                        End <- ifelse(compoundToExtract == "effector",
                                      StartLastDose[["Inhib"]],
                                      StartLastDose[["Sub"]])
                        time_range <- c(Start, End)

                        rm(Start, End)
                  }
            }
      }

      # This doesn't work well if the time range starts at something other than
      # 0 or ends somewhere other than the max time, so adjusting for that
      # situation.
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
                  Tlast = c(12, 24, 48, 96, 168, 336, 360, 504, 672, Inf),
                  BreaksToUse = c("12hr", "24hr", "48hr", "96hr", "1wk", "2wk",
                                  "15d", "3wk", "4wk", "4wkplus"))

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
                              "4wk" = seq(0, 672, 96),
                              "4wkplus" = round_up_nice(seq(0, tlast,
                                                            length.out = 6)))
      }

      if(TimeUnits == "minutes"){
            PossBreaks <- data.frame(Tlast = c(60, 240, 480, 720, 1440, Inf),
                                     BreaksToUse = c("1hr", "4hr",
                                                     "8hr", "12hr",
                                                     "24hr", "24hrplus"))
            BreaksToUse <- PossBreaks %>% filter(Tlast >= tlast) %>%
                  slice(which.min(Tlast)) %>% pull(BreaksToUse)

            XBreaks <- switch(BreaksToUse,
                              "1hr" = seq(0, 60, 15),
                              "4hr" = seq(0, 240, 30),
                              "8hr" = seq(0, 480, 60),
                              "12hr" = seq(0, 720, 120),
                              "24hr" = seq(0, 1440, 240),
                              "24hrplus" = round_up_nice(seq(0, tlast,
                                                             length.out = 6)))
      }

      # Adjusting the breaks when time_range[1] isn't 0
      if(all(complete.cases(time_range)) & time_range[1] != 0){
            XBreaks <- XBreaks + LastDoseTime
      }

      # Adding a grouping variable to data and also making the effector name
      # prettier for the graphs.
      MyEffector <- unique(Data$Effector) %>% as.character()
      MyEffector <- MyEffector[!MyEffector == "none"]

      if(complete.cases(MyEffector)){

            Data <- Data %>%
                  mutate(CompoundIsEffector = Compound == MyEffector,
                         Effector = as.character(ifelse(is.na(Effector), "none", Effector)),
                         Effector = tolower(gsub(
                               "SV-|Sim-|_EC|_SR|-MD|-SD|-[1-9]00 mg [QMSTBI]{1,2}D|_Fasted Soln|_Fed Capsule",
                               "",
                               Effector)),
                         Compound = ifelse(CompoundIsEffector, Effector, Compound),
                         Group = paste(Compound, Effector, Trial)) %>%
                  select(-CompoundIsEffector)
      }

      MyEffector <- tolower(gsub(
            "SV-|Sim-|_EC|_SR|-MD|-SD|-[1-9]00 mg [QMSTBI]{1,2}D|_Fasted Soln|_Fed Capsule",
            "",
            MyEffector))

      # Always want "none" to be the 1st item on the legend.
      Data <- Data %>%
            mutate(Effector = factor(Effector, levels = c("none", MyEffector)))

      # Separating the data by type and calculating trial means
      suppressMessages(
            sim_data_trial <- Data %>%
                  filter(Simulated == TRUE &
                               Trial %in% c("mean", "per5", "per95") == FALSE) %>%
                  group_by(across(any_of(c("Compound", "Tissue", "Effector",
                                           "Simulated", "Trial",
                                           "Time", "Time_units", "Conc_units",
                                           "Group")))) %>%
                  summarize(Conc = switch(mean_type,
                                          "arithmetic" = mean(Conc),
                                          "geometric" = gm_mean(Conc))) %>%
                  ungroup()
      )

      # The mean and percentile data listed in the simulator output files I've
      # seen are arithmetic, although I'm not sure if that's always true. If the
      # user specified "geometric", we need to calculate that or the wrong type
      # of mean will be plotted in the graph b/c the overall mean will be
      # arithmetic while the trial means will be geometric. To be on the safe
      # side, calculating for both instances since I just don't know how
      # consistent the simulator output is. -LS
      sim_data_mean <- Data %>%
            filter(Simulated == TRUE  &
                         !Trial %in% c("mean", "per5", "per95")) %>%
            group_by(across(
                  any_of(c("Compound", "Tissue", "Effector", "Simulated",
                           "Time", "Time_units", "Conc_units")))) %>%
            summarize(mean = switch(mean_type,
                                    "arithmetic" = mean(Conc, na.rm = TRUE),
                                    "geometric" = gm_mean(Conc)),
                      per5 = quantile(Conc, 0.05),
                      per95 = quantile(Conc, 0.95)) %>%
            pivot_longer(cols = c("mean", "per5", "per95"),
                         names_to = "Trial", values_to = "Conc") %>%
            mutate(Group = paste(Compound, Effector, Trial))

      obs_data <- Data %>% filter(Simulated == FALSE) %>% droplevels()

      # Setting the time range since I use it later.
      if(is.na(time_range_input[1])){
            time_range <- range(Data$Time, na.rm = T)
      }

      # Setting Y axis limits for both linear and semi-log plots
      Ylim <- bind_rows(sim_data_trial, obs_data) %>%
            filter(Time >= time_range[1] &
                         Time <= time_range[2] &
                         complete.cases(Conc)) %>%
            pull(Conc) %>% range()

      if(figure_type == "trial means"){

            NumTrials <- length(unique(sim_data_trial$Trial))
            AlphaToUse <- ifelse(NumTrials > 10, 0.05, 0.12)
            # Adjust this as needed. May want to use "switch" as we did for XBreaks.

            if(complete.cases(MyEffector) & MyEffector[1] != "none"){

                  ## linear plot
                  A <- ggplot(sim_data_trial,
                              aes(x = Time, y = Conc, group = Group,
                                  linetype = Effector, shape = Effector)) +
                        # guides(linetype = guide_legend(
                        #       override.aes = list(fill = c(NA, NA)))) + # this is causing problems, probably b/c I've only got 1 effector, so length linetype = 1
                        geom_line(alpha = AlphaToUse, lwd = 1) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"),
                                  lwd = 1) +
                        geom_point(data = obs_data, size = 2) +
                        scale_shape_manual(values = c(21, 24))

            } else {

                  ## linear plot
                  A <- ggplot(sim_data_trial,
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

            if(complete.cases(MyEffector) & MyEffector[1] != "none"){

                  A <- ggplot(sim_data_mean %>%
                                    filter(Trial %in% c("per5", "per95")) %>%
                                    mutate(Group = paste(Group, Trial)),
                              aes(x = Time, y = Conc,
                                  linetype = Effector, shape = Effector,
                                  group = Group)) +
                        geom_line(color = "gray80", lwd = 0.8) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"),
                                  lwd = 1) +
                        geom_point(data = obs_data, size = 2) +
                        scale_shape_manual(values = c(21, 24))

            } else {

                  ## linear plot
                  A <- ggplot(sim_data_mean %>% filter(Trial != "mean"),
                              aes(x = Time, y = Conc, group = Trial)) +
                        geom_line(color = "gray80", lwd = 0.8) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"), lwd = 1) +
                        geom_point(data = obs_data, size = 2, shape = 21)
            }

      }

      if(figure_type == "means only"){

            if(complete.cases(MyEffector) & MyEffector[1] != "none"){

                  A <- ggplot(sim_data_mean %>%
                                    filter(Trial == "mean") %>%
                                    mutate(Group = paste(Group, Trial)),
                              aes(x = Time, y = Conc, linetype = Effector)) +
                        geom_line(lwd = 1)

            } else {

                  A <- ggplot(sim_data_mean %>%
                                    filter(Trial == "mean"),
                              aes(x = Time, y = Conc)) +
                        geom_line(lwd = 1)

            }
      }

      A <- A +
            scale_x_continuous(breaks = XBreaks,
                               expand = expansion(mult = c(0, 0.04))) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
            labs(x = xlab, y = ylab) +
            coord_cartesian(xlim = time_range) +
            theme(panel.background = element_rect(fill="white", color=NA),
                  legend.key = element_rect(fill = "white"),
                  axis.ticks = element_line(color = "grey60"),
                  axis.text = element_text(color = "black"),
                  axis.title = element_text(color = "black", face = "bold"),
                  axis.line.x.bottom = element_line(color = "grey60"),
                  axis.line.y.left = element_line(color = "grey60"),
                  text = element_text(family = "Calibri")
            )

      # If the graph is of the effector, remove legend.
      if(compoundToExtract == "effector"){
            A <- A + theme(legend.position = "none")
      }

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
      Ylim_log <- Ylim
      if(Ylim[1] == 0 |
         # Also need to trim a bit from the low values when the data don't start
         # at 0 but are still really small
         Ylim[1] < mean(Data$Conc, na.rm = TRUE) * 0.01){

            Ylim_log[1] <- bind_rows(sim_data_trial, obs_data) %>%
                  filter(
                        # probably at tmax by 4 hrs for pretty much
                        # anything... This is pretty hacky, though,
                        # and I bet it will break at some point.
                        # Add option to let people set the y axis
                        # limits? Meh. If they want to do that,
                        # they can save the graphs individually and
                        # adjust as needed.
                        Time > switch(TimeUnits, "hours" = 4,
                                      "minutes" = 4*60) &
                              complete.cases(Conc) &
                              Conc > 0) %>%
                  pull(Conc) %>% min() * 0.8
      }

      # Just not quite getting the top part of the graph every time for the
      # semi-log plots. Adding a little more cushion to the upper Y limit
      Ylim_log[2] <- Ylim[2] * 1.5

      # Now, adjusting to nearest power of 10 on the bottom. Often, that's
      # too much for the top, so not rounding up to nearest power of 10 for
      # Ylim_log[2].
      Ylim_log[1] <- round_down(Ylim_log[1])

      B <- suppressMessages(
            A + scale_y_log10(limits = Ylim_log,
                              labels = function(.) format(., scientific = FALSE,
                                                          drop0trailing = TRUE)) +
                  coord_cartesian(xlim = time_range)
      )

      # both plots together, aligned vertically
      if(compoundToExtract == "effector"){
            AB <- suppressWarnings(
                  ggpubr::ggarrange(A, B, ncol = 1, labels = c("A", "B"),
                                    align = "v")
            )

      } else {
            AB <- suppressWarnings(
                  ggpubr::ggarrange(A, B, ncol = 1, labels = c("A", "B"),
                                    common.legend = TRUE, legend = "right",
                                    align = "v")
            )
      }

      Out <- list("Graphs" = AB)

      if(return_data){
            Out[["Data"]] <- Data
      }

      if(return_indiv_graphs){
            Out[["Linear graph"]] <- A
            Out[["Semi-log graph"]] <- B
      }

      if(length(Out) == 1){
            Out <- Out[[1]]
      }

      return(Out)
}




