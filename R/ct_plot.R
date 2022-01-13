#' Concentration-time plots to match Consultancy template
#'
#' Using observed and simulated concentration-time data, make
#' publication-quality graphs that match the consultancy template formatting
#' instructions. We've tried to include a fair number of options here for
#' flexibility, but many of the function arguments are optional; most of the
#' time, you'll get decent-looking graphs while only setting a minimal number of
#' arguments. If you want to plot enzyme abundance data, please see
#' \code{\link{enz_plot}}. \strong{Note:} Currently, this only works for
#' concentration-time data for the substrate, primary metabolite 1, secondary
#' metabolite, or inhibitor 1. \strong{If your simulation included
#' \emph{anything} other than those compounds, this is \emph{not} reliable.}
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   concentration-time data
#' @param obs_data_file name of the Excel file containing the observed
#'   concentration-time data. If the observed data you want to plot were already
#'   included in the Excel output from the simulator, leave this as NA.
#'   Otherwise, this is the file that it is ready to be converted to an XML
#'   file, not the file that contains only the digitized time and concentration
#'   data.
#' @param obs_inhibitor_data_file name of the Excel file containing the observed
#'   concentration-time data in the presence of an effector (labeled "Inhibitor
#'   1" in the simulator). This is the file that is ready to be converted to an
#'   XML file. If your Inhibitor 1 data were already included in
#'   \code{obs_data_file}, leave this as NA.
#' @param sim_obs_dataframe If you have already extracted the concentration-time
#'   data using the function \code{\link{extractConcTime}}, you can enter the
#'   name of the output data.frame from that function instead of re-reading the
#'   Excel file, which is the bottleneck step in running this function.
#' @param tissue the tissue to plot. Default is plasma for typical plasma
#'   concentration-time data. Other tissues are acceptable, e.g., "lung",
#'   "brain", etc., as long as the tissue is one of the options included in
#'   "Sheet Options", "Tissues" in the simulator.
#' @param compoundToExtract For which compound do you want to extract
#'   concentration-time data? Options are "substrate" (default), "primary
#'   metabolite 1", "secondary metabolite", or "inhibitor 1" (this actually can
#'   be an inducer, activator, suppressor, or inhibitor, but it's labeled as
#'   "Inhibitor 1" in the simulator).
#' @param figure_type type of figure to plot. Options are:
#'
#'   \describe{
#'
#'   \item{trial means}{plots a black line for the mean data, gray lines for the
#'   mean of each trial of simulated data, and open circles for the observed
#'   data. If an effector was present, gray dashed lines indicate the mean of
#'   each trial of simulated data in the presence of the effector.}
#'
#'   \item{percentiles}{plots a black line for the mean data, gray lines for the
#'   5th and 95th percentiles of the simulated data, and open circles for the
#'   observed data}
#'
#'   \item{means only}{plots a black line for the mean data and, if an effector
#'   was modeled, a dashed line for the concentration-time data with Inhibitor
#'   1.}
#'
#'   \item{Freddy}{Freddy's favorite style of plot with trial means in light
#'   gray, the overall mean in thicker black, the 5th and 95th percentiles in
#'   dashed lines, and the observed data in semi-transparent purple-blue. Graphs
#'   with an effector present lose the trial means, and the percentiles switch
#'   to solid, gray lines. \strong{An editorial comment:} While this does not
#'   align with the officially sanctioned template at this time, this looks
#'   \emph{sharp}, makes it easy to see the defining characteristics of the
#'   data, and I recommend checking it out. If the color is too much for you but
#'   you like the rest, try setting \code{obs_color = "none"}. -LS}}
#'
#' @param time_range time range to show relative to the start of the simulation.
#'   Options: \describe{
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
#' @param t0 What event should be used for time zero? Options are: "simulation
#'   start" (default), "substrate dose 1", "inhibitor 1 dose 1", "substrate last
#'   dose", "inhibitor 1 last dose", "substrate penultimate dose", or "inhibitor
#'   1 penultimate dose". \emph{This does not change which data are included in
#'   the graph;} instead, this determines whether the x axis numbers are offset
#'   so that, e.g., the last dose is administered at time 0.
#'
#' @param adjust_obs_time TRUE or FALSE: Adjust the time listed in the observed
#'   data file to match the last dose administered? This only applies to
#'   multiple-dosing regimens. If TRUE, the graph will show the observed data
#'   overlaid with the simulated data such that the dose in the observed data
#'   was administered at the same time as the last dose in the simulated data.
#'   If FALSE, the observed data will start at whatever times are listed in the
#'   Excel file.
#' @param pad_x_axis Optionally add a smidge of padding to the left side of the
#'   x axis. If left as FALSE, the y axis will be placed right at the beginning
#'   of your time range. If set to TRUE, there will be a little bit of space
#'   between the y axis and the start of your time range. NOTE: We could allow
#'   users to specify exactly how much padding and on which sides of the x axis
#'   if there's interest from users. -LS
#' @param x_axis_interval Set the x-axis major tick-mark interval. Acceptable
#'   input: any number or leave as NA to accept default values.
#'
#' @param y_axis_limits_lin Optionally set the Y axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as NA, the Y axis limits for the
#'   linear plot will be automatically selected.
#' @param y_axis_limits_log Optionally set the Y axis limits for the semi-log
#'   plot, e.g., \code{c(10, 1000)}. Values will be rounded down and up,
#'   respectively, to the nearest order of magnitude. If left as NA, the Y axis
#'   limits for the semi-log plot will be automatically selected.
#'
#' @param obs_data_option Set options for how to view observed data. Options
#'   are:
#'
#'   \describe{
#'
#'   \item{"means only"}{show only a single point at the arithmetic mean value
#'   for each time point}
#'
#'   \item{"geometric means only"}{show a single point at the geometric mean}
#'
#'   \item{"all"}{show all the individual data (equivalently, leave
#'   \code{obs_data_option} as NA)}
#'
#'   \item{"mean bars"}{show a point at the arithmetic mean for each time point
#'   and error bars for the arithmetic standard deviation}}
#'
#' @param obs_color If you would like the observed data points to be in color,
#'   either list a specific color or set this to "default". Points will be
#'   displayed in semi-transparent blue-purple for "default" and the
#'   semi-transparent version of whatever other color you list otherwise.
#'   Setting this to "none" will make sure that the symbols are black outlines
#'   only with no fill. Hex color codes are also ok to use.
#' @param obs_shape Optionally specify what shapes are used to depict observed
#'   data for 1. the substrate drug alone and 2. the substrate drug in the
#'   presence of an effector. Input should look like this, for example:
#'   \code{c(1, 2)} to get an open circle and an open triangle. To see all the
#'   possible shapes and what number corresponds to which shape, see
#'   \url{https://r-graphics.org/recipe-scatter-shapes} (there's a graph around
#'   the middle of that page). If left as NA, substrate alone will be an open
#'   circle and substrate + inhibitor 1 will be an open triangle.
#'
#' @param line_transparency Optionally specify the transparency for the trial
#'   mean or percentile lines. Acceptable values are from 0 (fully transparent,
#'   so no line at all) to 1 (completely opaque or black). If left as NA, this
#'   value will be automatically determined.
#' @param line_type Optionally specify what types of lines are used to depict 1.
#'   the substrate drug alone and 2. the substrate drug in the presence of an
#'   effector (when applicable). Input should look like this, for example:
#'   \code{c("solid", "dashed")} to get a solid line for the substrate drug and
#'   a dashed line for inhibitor 1. To see all possible \code{line_type}
#'   options: \code{ggpubr::show_line_types()}. If left as NA, substrate alone
#'   will be a solid line and substrate + inhibitor 1 will be a dashed line. If
#'   \code{figure_type} is "Freddy" and there's no effector present, which is
#'   a slightly different scenario than the other graph types, the 1st line type
#'   specified will be for the mean simulated concentration and the trial means,
#'   and the 2nd line type specified will be for the 5th and 95th percentiles.
#' @param line_color Optionally specify what colors to use for the lines.
#'   Acceptable input for, e.g., the substrate alone to be blue and the
#'   substrate + Inhibitor 1 to be red: \code{c("blue", "red")}. If left as NA,
#'   lines will be black or gray. Hex color codes are also ok to use.
#'
#' @param legend_label Optionally indicate on the legend whether the effector is
#'   an inhibitor,inducer, activator, or suppressor. Input will be used as the
#'   label in the legend for the line style and the shape. If left as NA when a
#'   legend is included and an effector is present, the label in the legend will
#'   be "Inhibitor 1".
#' @param prettify_effector_name Optionally make the Inhibitor 1 name prettier
#'   in the legend. This was designed for simulations where Inhibitor 1 is one
#'   of the standard options for the simulator, and leaving
#'   \code{prettify_effector_name = TRUE} will make the name of Inhibitor 1 be
#'   something more human readable. For example, "SV-Rifampicin-MD" will become
#'   "rifampicin", and "Sim-Ketoconazole-200 mg BID" will become "ketoconazole".
#'   Set it to the name you'd prefer to see in your legend if you would like
#'   something different.
#'
#' @param return_data TRUE or FALSE: Return the data used in the graphs? If
#'   TRUE, this will return a named list of: \describe{ \item{Graphs}{the set of
#'   graphs} \item{Data}{a data.frame of the concentration-time data used in the
#'   set of graphs} }
#' @param return_indiv_graphs TRUE or FALSE: Return each of the two individual
#'   graphs? This can be useful if you want to modify the graphs further or only
#'   use one, etc.
#'
#' @return Depending on the options selected, returns either a set of graphs or
#'   a named list of the set of the two graphs together ("Graphs" in the
#'   output), the individual graphs ("Linear graph" and "Semi-log graph"),
#'   and/or the data used for creating the graphs ("Data").
#' @import tidyverse
#' @export
#'
#' @examples
#' sim_data_file <- "../Example simulator output.xlsx"
#' obs_data_file <- "../fig1-242-06-001-MD - for XML conversion.xlsx"
#'
#' ct_plot(sim_data_file)
#' ct_plot(sim_data_file, figure_type = "percentiles")
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
#' # These may be too busy when Inhibitor 1 is present:
#' ct_plot(sim_data_file = "../Example simulator output MD + inhibitor.xlsx")
#' ct_plot(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'         figure_type = "trial percentiles")
#' # so you may want to consider only plotting means as an alternative:
#' ct_plot(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'         figure_type = "means only")
#'
#' # You can also add a separate observed concentration-time file for the
#' # compound with Inhibitor 1 present. The graph will show those points
#' # as open triangles.
#' ct_plot(sim_data_file = "../Example simulator output MD.xlsx",
#'         obs_data_file = "../fig1-242-06-001-SD - for XML conversion.xlsx",
#'         obs_inhibitor_data_file = "../Mallikaarjun_2016_RTV-fig1-100mg-BID-DLM+Kaletra - for XML conversion.xlsx",
#'         time_range = "last dose")
#'
#' # If you've already got a data.frame formatted like the output
#' # from extractConcTime...
#' data(ConcTime)
#' ct_plot(sim_obs_dataframe = ConcTime)
#'
#' # Add some further options for the look of your graph -- especially useful
#' # if the default settings are clipping your data.
#' ct_plot(sim_data_file = "../Example simulator output - MDZ + metabolites + inhibitor.xlsx",
#'         obs_data_option = "mean bars",
#'         obs_color = "red",
#'         y_axis_limits_log = c(1e-05, 0.1),
#'         pad_x_axis = TRUE,
#'         include_legend = TRUE,
#'         legend_label = "Inhibitor")

ct_plot <- function(sim_data_file = NA,
                    obs_data_file = NA,
                    obs_inhibitor_data_file = NA,
                    sim_obs_dataframe = NA,
                    tissue = "plasma",
                    compoundToExtract = "substrate",
                    figure_type = "trial means",
                    time_range = NA,
                    x_axis_interval = NA,
                    pad_x_axis = FALSE,
                    adjust_obs_time = FALSE,
                    t0 = "simulation start",
                    y_axis_limits_lin = NA,
                    y_axis_limits_log = NA,
                    obs_data_option = NA,
                    obs_color = NA,
                    obs_shape = NA,
                    line_type = NA,
                    line_transparency = NA,
                    line_color = NA,
                    include_legend = FALSE,
                    legend_label = NA,
                    prettify_effector_name = TRUE,
                    return_data = FALSE,
                    return_indiv_graphs = FALSE){

      # Error catching
      if(length(figure_type) != 1 |
         figure_type %in% c("trial means", "percentiles", "trial percentiles",
                            "Freddy", "means only") == FALSE){
            stop("The only acceptable options for figure_type are 'trial means', 'trial percentiles', 'means only', or 'Freddy'.")
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

      if(complete.cases(obs_data_option) &&
         obs_data_option %in% c("means only", "geometric means only", "all",
                                "mean bars") == FALSE){
            stop("The value for obs_data_option must be one of 'means only', 'geometric means only', 'all', or 'mean bars'.")
      }

      t0 <- tolower(t0)
      t0_opts <- c("simulation start", "substrate dose 1", "inhibitor 1 dose 1",
                   "substrate last dose", "inhibitor 1 last dose",
                   "substrate penultimate dose", "inhibitor 1 penultimate dose")
      if(t0 %in% t0_opts == FALSE){
            stop(paste0("t0 must be set to ",
                        sub("and", "or", str_comma(t0_opts)), "."))
      }

      # Extract the data to plot
      if(is.data.frame(sim_obs_dataframe)){
            Data <- sim_obs_dataframe
      } else {
            Data <- extractConcTime(sim_data_file = sim_data_file,
                                    obs_data_file = obs_data_file,
                                    tissue = tissue,
                                    compoundToExtract = compoundToExtract,
                                    obs_inhibitor_data_file = obs_inhibitor_data_file,
                                    adjust_obs_time = adjust_obs_time)
      }

      TimeUnits <- sort(unique(Data$Time_units))

      # Check whether the user is plotting enzyme abundance
      EnzPlot <- names(Data)[1] == "Enzyme"
      if(EnzPlot){
            ObsConcUnits <- "Relative abundance"
            Data <- Data %>% rename(Conc = Abundance) %>%
                  mutate(Simulated = TRUE,
                         Compound = Enzyme,
                         Inhibitor = ifelse(EffectorPresent,
                                           "inhibitor 1", "none"))
      } else {
            ObsConcUnits <- sort(unique(Data$Conc_units))
      }


      # A little more error catching
      if(all(complete.cases(time_range) & class(time_range) == "numeric") &
         (any(time_range < min(Data$Time[Data$Simulated == TRUE])) |
          any(time_range > max(Data$Time[Data$Simulated == TRUE])))){
            stop(paste0(
                  "Both the values entered for the time range must be within the range of time simulated. The range of time in your simulation was ",
                  min(Data$Time[Data$Simulated == TRUE]), " to ",
                  max(Data$Time[Data$Simulated == TRUE]), " ", TimeUnits, "."))
      }


      # Setting x axis (time) ------------------------------------------------
      # Adjusting graph labels as appropriate for the observed data
      xlab <- switch(TimeUnits,
                     "hours" = "Time (h)",
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
                     "PD response" = "PD response",
                     "Relative abundance" = "Relative abundance")

      # Setting the breaks for the x axis
      tlast <- ifelse(all(complete.cases(time_range)) &
                            length(time_range) == 2,
                      time_range[2], max(Data$Time))

      time_range_input <- time_range

      if(time_range_input[1] %in% c("first dose", "last dose",
                                    "penultimate dose") |
         t0 != "simulation start"){
            Deets <- extractExpDetails(sim_data_file)

            # start of last dose simulated of substrate and inhibitor
            NumDoses <- c("Sub" = Deets$NumDoses_sub,
                          "Inhib" = Deets$NumDoses_inhib)
            DoseInt <- c("Sub" = Deets$DoseInt_sub,
                         "Inhib" = Deets$DoseInt_inhib)
            StartLastDose <- DoseInt * (NumDoses - 1) # Time starts at 0, not 1, so that's why it's "NumDoses - 1" rather than "NumDoses" alone.

      }

      if(time_range_input[1] %in% c("first dose", "last dose",
                                    "penultimate dose")){

            if(Deets$Regimen_sub == "Single Dose"){

                  time_range <- c(difftime_sim(Deets$SimStartDayTime,
                                               Deets$StartDayTime_sub),
                                  tlast)
                  warning(paste0("You requested the ", time_range_input[1],
                                 ", but the substrate was administered as a single dose. The graph x axis will cover the substrate administration time until the end of the simulation."))
            } else {

                  DayTime_t0 <- difftime_sim(
                        time1 = c(Deets[["SimStartDayTime"]], Deets[["SimStartDayTime"]]),
                        time2 = c(Deets[["StartDayTime_sub"]], Deets[["StartDayTime_inhib"]]),
                        units = TimeUnits)
                  names(DayTime_t0) <- c("Sub", "Inhib")

                  # start of 2nd dose of substrate and inhibitor
                  StartDose2 <- DayTime_t0 + DoseInt

                  # There's a possibility that the user set up the simulation to
                  # start the last dose right at the end of the simulation;
                  # adjusting for that.
                  if(StartLastDose[["Sub"]] == max(Data$Time, na.rm = T)){
                        StartLastDose[["Sub"]] <-
                              StartLastDose[["Sub"]] - DoseInt[["Sub"]]
                  }

                  if(complete.cases(StartLastDose[["Inhib"]]) &
                     StartLastDose[["Inhib"]] == max(Data$Time, na.rm = T)){
                        StartLastDose[["Inhib"]] <-
                              StartLastDose[["Inhib"]] - DoseInt[["Inhib"]]
                  }

                  if(time_range_input[1] == "first dose"){
                        Start <- ifelse(compoundToExtract == "inhibitor 1",
                                        DayTime_t0[["Inhib"]],
                                        DayTime_t0[["Sub"]])
                        End <- ifelse(compoundToExtract == "inhibitor 1",
                                      StartDose2[["Inhib"]],
                                      StartDose2[["Sub"]])
                        time_range <- c(Start, End)
                        rm(Start, End)
                  }

                  if(time_range_input[1] == "last dose"){
                        Start <- ifelse(compoundToExtract == "inhibitor 1",
                                        StartLastDose[["Inhib"]],
                                        StartLastDose[["Sub"]])
                        End <- max(Data$Time)
                        time_range <- c(Start, End)
                        rm(Start, End)
                  }

                  if(time_range_input[1] == "penultimate dose"){
                        DoseIntToUse <- ifelse(compoundToExtract == "inhibitor 1",
                                               DoseInt["Inhib"], DoseInt["Sub"])

                        Start <- ifelse(compoundToExtract == "inhibitor 1",
                                        StartLastDose[["Inhib"]] - DoseIntToUse,
                                        StartLastDose[["Sub"]] - DoseIntToUse)
                        End <- ifelse(compoundToExtract == "inhibitor 1",
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

      # Setting the time range if it's not already set since we use it later.
      if(is.na(time_range_input[1])){
            time_range <- range(Data$Time, na.rm = T)
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

            BreaksToUse <- ifelse(complete.cases(x_axis_interval),
                                  "UserDefined", BreaksToUse)

            XBreaks <- switch(BreaksToUse,
                              "12hr" = seq(0, 12, 1),
                              "24hr" = seq(0, 24, 2),
                              "48hr" = seq(0, 48, 4),
                              "96hr" = seq(0, 96, 6),
                              "1wk" = seq(0, 168, 12),
                              "2wk" = seq(0, 336, 24),
                              "15d" = seq(0, 360, 24),
                              "3wk" = seq(0, 504, 36),
                              "4wk" = seq(0, 672, 48),
                              "4wkplus" = round_up_nice(seq(0, tlast,
                                                            length.out = 12)),
                              "UserDefined" = seq(0, max(Data$Time, na.rm = T),
                                                  x_axis_interval/2))

      }

      if(TimeUnits == "minutes"){
            PossBreaks <- data.frame(Tlast = c(60, 240, 480, 720, 1440, Inf),
                                     BreaksToUse = c("1hr", "4hr",
                                                     "8hr", "12hr",
                                                     "24hr", "24hrplus"))

            BreaksToUse <- PossBreaks %>% filter(Tlast >= tlast) %>%
                  slice(which.min(Tlast)) %>% pull(BreaksToUse)

            BreaksToUse <- ifelse(complete.cases(x_axis_interval),
                                  "UserDefined", BreaksToUse)

            XBreaks <- switch(BreaksToUse,
                              "1hr" = seq(0, 60, 7.5),
                              "4hr" = seq(0, 240, 15),
                              "8hr" = seq(0, 480, 30),
                              "12hr" = seq(0, 720, 60),
                              "24hr" = seq(0, 1440, 120),
                              "24hrplus" = round_up_nice(seq(0, tlast,
                                                             length.out = 12)),
                              "UserDefined" = seq(0, max(Data$Time, na.rm = T),
                                                  x_axis_interval/2))
      }

      # Adjusting the breaks when time_range[1] isn't 0
      if(all(complete.cases(time_range)) & time_range[1] != 0){
            XBreaks <- XBreaks + LastDoseTime
      }

      # If t0 isn't "simulation start", need to adjust x axis.
      if(t0 != "simulation start"){
            t0_num <- switch(
                  t0,
                  "substrate dose 1" = difftime_sim(Deets$SimStartDayTime,
                                                    Deets$StartDayTime_sub),
                  "inhibitor 1 dose 1" = difftime_sim(Deets$SimStartDayTime,
                                                   Deets$StartDayTime_inhib),
                  "substrate last dose" =
                        ifelse(StartLastDose["Sub"] == max(Data$Time),
                               StartLastDose["Sub"] - DoseInt["Sub"],
                               StartLastDose["Sub"]),
                  "inhibitor 1 last dose" =
                        ifelse(StartLastDose["Inhib"] == max(Data$Time),
                               StartLastDose["Inhib"] - DoseInt["Inhib"],
                               StartLastDose["Inhib"]),
                  "substrate penultimate dose" =
                        ifelse(StartLastDose["Sub"] == max(Data$Time),
                               StartLastDose["Sub"] - 2*DoseInt["Sub"],
                               StartLastDose["Sub"] - DoseInt["Sub"]),
                  "inhibitor 1 penultimate dose"  =
                        ifelse(StartLastDose["Inhib"] == max(Data$Time),
                               StartLastDose["Inhib"] - 2*DoseInt["Inhib"],
                               StartLastDose["Inhib"] - DoseInt["Inhib"]))
            Data$Time_orig <- Data$Time
            Data$Time <- Data$Time - t0_num
            XBreaks <- XBreaks - t0_num
      } else {
            Data$Time_orig <- Data$Time
      }

      XLabels <- XBreaks
      XLabels[seq(2,length(XLabels),2)] <- ""
      XLabels[which(XBreaks == 0)] <- "0"

      # Dealing with possible inhibitor 1 data ---------------------------------
      # Adding a grouping variable to data and also making the inhibitor 1 name
      # prettier for the graphs.
      MyEffector <- unique(Data$Inhibitor) %>% as.character()
      MyEffector <- MyEffector[!MyEffector == "none"]

      if(length(MyEffector) > 0 && complete.cases(MyEffector)){

            Data <- Data %>%
                  mutate(CompoundIsEffector = Compound == MyEffector,
                         Inhibitor = as.character(ifelse(is.na(Inhibitor),
                                                        "none", Inhibitor)))

            if(class(prettify_effector_name) == "logical" &&
               prettify_effector_name){
                  MyEffector <-
                        tolower(gsub(
                              "SV-|Sim-|_EC|_SR|-MD|-SD|-[1-9]00 mg [QMSTBI]{1,2}D|_Fasted Soln|_Fed Capsule",
                              "", MyEffector))
            }

            if(class(prettify_effector_name) == "character"){
                  MyEffector <- prettify_effector_name
            }

            Data <- Data %>%
                  mutate(Compound = ifelse(CompoundIsEffector, MyEffector, Compound),
                         Inhibitor = ifelse(Inhibitor != "none", MyEffector, Inhibitor),
                         Group = paste(Compound, Inhibitor, Trial)) %>%
                  select(-CompoundIsEffector)
      }

      # Error catching for when user specifies linetype, color or shape and
      # doesn't include enough values when effector present
      if(complete.cases(obs_shape[1]) && length(MyEffector) > 0 &&
         complete.cases(MyEffector) &&
         compoundToExtract != "inhibitor 1" &&
         length(complete.cases(obs_shape)) < 2){
            warning("There is an inhibitor or effector present and you have specified what the symbol shapes should be, but you have not listed enough values (you need 2). The default shapes will be used.")
            obs_shape <- NA
      }

      if(complete.cases(obs_color[1]) && length(MyEffector) > 0 &&
         complete.cases(MyEffector) &&
         compoundToExtract != "inhibitor 1" &&
         length(complete.cases(obs_color)) < 2){
            warning("There is an inhibitor or effector present and you have specified what the symbol colors should be, but you have not listed enough values (you need 2). The default colors will be used.")
            obs_color <- NA
      }

      if(complete.cases(line_color[1]) && length(MyEffector) > 0 &&
         complete.cases(MyEffector) &&
         compoundToExtract != "inhibitor 1" &&
         length(complete.cases(line_color)) < 2){
            warning("There is an inhibitor or effector present and you have specified what the line colors should be, but you have not listed enough values (you need 2). The default colors will be used.")
            line_color <- NA
      }

      if(complete.cases(line_type[1]) && length(MyEffector) > 0 &&
         complete.cases(MyEffector) &&
         compoundToExtract != "inhibitor 1" &&
         length(complete.cases(line_type)) < 2){
            warning("There is an inhibitor or effector present and you have specified what the line types should be, but you have not listed enough values (you need 2). The default line types will be used.")
            line_type <- NA
      }

      # Always want "none" to be the 1st item on the legend, and we need there
      # to be some value present for "Inhibitor" for function to work correctly.
      Data <- Data %>%
            mutate(Inhibitor = ifelse(is.na(Inhibitor), "none", Inhibitor))
      if(length(MyEffector) > 0){
            Data <- Data %>%
                  mutate(Inhibitor = factor(Inhibitor, levels = c("none", MyEffector)))
      }

      # Setting up data.frames to graph ---------------------------------------
      # Separating the data by type and calculating trial means
      suppressMessages(
            sim_data_trial <- Data %>%
                  filter(Simulated == TRUE &
                               Trial %in% c("mean", "per5", "per95") == FALSE) %>%
                  group_by(across(any_of(c("Compound", "Tissue", "Inhibitor",
                                           "Simulated", "Trial", "Group",
                                           "Time", "Time_orig",
                                           "Time_units", "Conc_units")))) %>%
                  summarize(Conc = mean(Conc, na.rm = T)) %>%
                  ungroup()
      )

      sim_data_mean <- Data %>%
            filter(Simulated == TRUE  &
                         Trial %in% c("mean", "per5", "per95")) %>%
            mutate(Group = paste(Compound, Inhibitor, Trial))

      # Setting up observed data per user input -------------------------------

      obs_data <- Data %>% filter(Simulated == FALSE) %>% droplevels()

      if(complete.cases(obs_data_option) &
         str_detect(obs_data_option, "mean")){

            suppressMessages(
                  obs_data <- obs_data %>%
                        group_by(across(any_of(c("Compound", "Tissue", "Inhibitor",
                                                 "Simulated", "Trial", "Group",
                                                 "Time", "Time_orig",
                                                 "Time_units", "Conc_units")))) %>%
                        summarize(SDConc = sd(Conc, na.rm = T),
                                  Conc = switch(obs_data_option,
                                                "means only" = mean(Conc, na.rm = T),
                                                "mean bars" = mean(Conc, na.rm = T),
                                                "geometric means only" = gm_mean(Conc))) %>%
                        ungroup()
            )
      }

      # Setting y axis (concentration) ---------------------------------------
      # Setting Y axis limits for both linear and semi-log plots
      if (figure_type == "trial means") {
            Ylim_data <- bind_rows(sim_data_trial, obs_data)
      } else if (figure_type %in% c("trial percentiles", "Freddy", "percentiles")) {
            Ylim_data <- bind_rows(sim_data_trial, sim_data_mean, obs_data)
      } else if (figure_type == "means only") {
            Ylim_data <- sim_data_mean %>% filter(Trial == "mean") }

      Ylim <- Ylim_data %>% filter(Time_orig >= time_range[1] &
                                         Time_orig <= time_range[2] &
                                         complete.cases(Conc)) %>% pull(Conc) %>%
            range()

      if(any(complete.cases(y_axis_limits_lin))){
            Ylim <- y_axis_limits_lin[1:2]
      }

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


      # Figure types ---------------------------------------------------------

      # Setting user specifications for shape, linetype, and color where
      # applicable.
      if(is.na(line_type[1])){
            line_type <- c("solid", "dashed")
      }

      if(is.na(obs_shape[1])){
            obs_shape <- c(21, 24)
      }

      if(complete.cases(line_color[1]) & is.na(obs_color[1])){
            obs_color <- line_color
      }

      if(is.na(line_color[1])){
            line_color <- c("black", "black")
      }

      if(complete.cases(line_color[1]) && figure_type == "Freddy" &&
         length(line_color) == 1){
            line_color <- rep(line_color, 2)
      }

      obs_color <- ifelse((complete.cases(obs_color) & obs_color == "default") |
                                (is.na(obs_color[1]) & figure_type == "Freddy"),
                          "#3030FE", obs_color)

      ## figure_type: trial means -----------------------------------------------------------
      if(figure_type == "trial means"){

            NumTrials <- length(unique(sim_data_trial$Trial))
            AlphaToUse <- ifelse(complete.cases(line_transparency),
                                 line_transparency,
                                 ifelse(NumTrials > 10, 0.05, 0.4))

            if(length(MyEffector) > 0 && complete.cases(MyEffector[1]) &&
               MyEffector[1] != "none" & compoundToExtract != "inhibitor 1"){

                  ## linear plot
                  A <- ggplot(sim_data_trial,
                              aes(x = Time, y = Conc, group = Group,
                                  linetype = Inhibitor, shape = Inhibitor,
                                  color = Inhibitor)) +
                        geom_line(alpha = AlphaToUse, lwd = 1) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"),
                                  lwd = 1) +
                        scale_shape_manual(values = obs_shape[1:2]) +
                        scale_linetype_manual(values = line_type[1:2]) +
                        scale_color_manual(values = line_color[1:2])

                  if(nrow(obs_data) > 0){
                        if(all(is.na(obs_color)) | obs_color[1] == "none"){
                              A <-  A + geom_point(data = obs_data, size = 2,
                                                   stroke = 1)
                        } else {
                              A <- A +
                                    geom_point(data = obs_data, size = 2,
                                               fill = obs_color[1:2], alpha = 0.5,
                                               stroke = 1) +
                                    geom_point(data = obs_data, size = 2,
                                               fill = NA, stroke = 1)
                        }
                  }

            } else {

                  ## linear plot
                  A <- ggplot(sim_data_trial,
                              aes(x = Time, y = Conc, group = Trial)) +
                        geom_line(alpha = AlphaToUse, lwd = 1,
                                  linetype = line_type[1],
                                  color = line_color[1]) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"),
                                  lwd = 1, linetype = line_type[1],
                                  color = line_color[1])

                  if(nrow(obs_data) > 0){
                        if(all(is.na(obs_color)) | obs_color[1] == "none"){
                              A <- A + geom_point(data = obs_data, size = 2,
                                                  shape = obs_shape[1], stroke = 1)
                        } else {
                              A <- A + geom_point(data = obs_data, size = 2,
                                                  fill = obs_color[1], alpha = 0.5,
                                                  shape = obs_shape[1], stroke = 1) +
                                    geom_point(data = obs_data, size = 2,
                                               fill = NA, shape = obs_shape[1], stroke = 1)
                        }
                  }
            }
      }

      ## figure_type: percentiles ----------------------------------------------------------
      if(str_detect(figure_type, "percentile")){
            # graphs with 95th percentiles

            AlphaToUse <- ifelse(complete.cases(line_transparency),
                                 line_transparency, 0.25)

            if(length(MyEffector) > 0 && complete.cases(MyEffector[1]) &&
               MyEffector[1] != "none" & compoundToExtract != "inhibitor 1"){

                  A <- ggplot(sim_data_mean %>%
                                    filter(Trial %in% c("per5", "per95")) %>%
                                    mutate(Group = paste(Group, Trial)),
                              aes(x = Time, y = Conc,
                                  linetype = Inhibitor, shape = Inhibitor,
                                  color = Inhibitor,
                                  group = Group)) +
                        geom_line(alpha = AlphaToUse, lwd = 0.8) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"),
                                  lwd = 1)  +
                        scale_shape_manual(values = obs_shape[1:2]) +
                        scale_linetype_manual(values = line_type[1:2]) +
                        scale_color_manual(values = line_color[1:2])

                  if(nrow(obs_data) > 0){
                        if(all(is.na(obs_color)) | obs_color[1] == "none"){
                              A <- A + geom_point(data = obs_data, size = 2,
                                                  stroke = 1)
                        } else {
                              A <- A +
                                    geom_point(data = obs_data, size = 2,
                                               fill = obs_color, alpha = 0.5,
                                               stroke = 1) +
                                    geom_point(data = obs_data, size = 2,
                                               fill = NA, stroke = 1)
                        }
                  }

            } else {

                  ## linear plot
                  A <- ggplot(sim_data_mean %>% filter(Trial != "mean"),
                              aes(x = Time, y = Conc, group = Trial)) +
                        geom_line(alpha = AlphaToUse, lwd = 0.8,
                                  linetype = line_type[1],
                                  color = line_color[1]) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"), lwd = 1,
                                  linetype = line_type[1],
                                  color = line_color[1])

                  if(nrow(obs_data) > 0){
                        if(all(is.na(obs_color)) | obs_color[1] == "none"){
                              A <- A + geom_point(data = obs_data, size = 2,
                                                  stroke = 1, shape = obs_shape[1])
                        } else {
                              A <- A +
                                    geom_point(data = obs_data, size = 2,
                                               fill = obs_color[1], alpha = 0.5,
                                               stroke = 1, shape = obs_shape[1]) +
                                    geom_point(data = obs_data, size = 2,
                                               fill = NA, shape = obs_shape[1], stroke = 1)
                        }
                  }
            }
      }

      ## figure_type: Freddy --------------------------------------------------------------
      if(figure_type == "Freddy"){

            NumTrials <- length(unique(sim_data_trial$Trial))
            AlphaToUse <- ifelse(complete.cases(line_transparency),
                                 line_transparency,
                                 ifelse(NumTrials > 10, 0.05, 0.25))

            if(length(MyEffector) > 0 && complete.cases(MyEffector[1]) &&
               MyEffector[1] != "none" & compoundToExtract != "inhibitor 1"){

                  ## linear plot
                  A <- ggplot(data = sim_data_mean %>%
                                    filter(Trial == "mean"),
                              aes(x = Time, y = Conc, group = Group,
                                  linetype = Inhibitor, shape = Inhibitor,
                                  color = Inhibitor)) +
                        geom_line(lwd = 1) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial %in% c("per5", "per95")),
                                  alpha = AlphaToUse, lwd = 1) +
                        scale_shape_manual(values = obs_shape[1:2]) +
                        scale_linetype_manual(values = line_type[1:2]) +
                        scale_color_manual(values = line_color[1:2])

                  if(nrow(obs_data) > 0){
                        # When figure_type == "Freddy", I want the default to be
                        # a blue-purple semi-transparent fill. However, I want
                        # people to have the option to override that, so
                        # setting obs_color to "none" will override the
                        # "Freddy" default. -LS
                        if(all(is.na(obs_color)) | obs_color[1] == "none"){
                              A <- A + geom_point(data = obs_data, size = 2,
                                                  fill = NA, stroke = 1)
                        } else {
                              # This is the situation when the user has
                              # requested a specific color for the Freddy figure
                              # type.
                              A <- A +
                                    geom_point(data = obs_data, size = 2,
                                               fill = obs_color, alpha = 0.5,
                                               stroke = 1) +
                                    geom_point(data = obs_data, size = 2,
                                               fill = NA, stroke = 1)
                        }
                  }

            } else {

                  ## linear plot
                  A <- ggplot(sim_data_trial,
                              aes(x = Time, y = Conc, group = Trial)) +
                        geom_line(alpha = AlphaToUse, lwd = 1,
                                  linetype = line_type[1],
                                  color = line_color[1]) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial == "mean"),
                                  lwd = 1, linetype = line_type[1],
                                  color = line_color[1]) +
                        geom_line(data = sim_data_mean %>%
                                        filter(Trial %in% c("per5", "per95")),
                                  linetype = line_type[2],
                                  color = line_color[2])

                  if(nrow(obs_data) > 0){
                        # When figure_type == "Freddy", I want the default to be
                        # a blue-purple semi-transparent fill. However, I want
                        # people to have the option to override that, so
                        # setting obs_color to "none" will override the
                        # "Freddy" default. -LS
                        if(all(is.na(obs_color)) | obs_color[1] == "none"){
                              A <- A + geom_point(data = obs_data, size = 2,
                                                  fill = NA, stroke = 1,
                                                  shape = obs_shape[1])
                        } else {
                              # This is the situation when the user has
                              # requested a specific color for the Freddy figure
                              # type.
                              A <- A +
                                    geom_point(data = obs_data, size = 2,
                                               fill = obs_color[1], alpha = 0.5,
                                               stroke = 1, shape = obs_shape[1]) +
                                    geom_point(data = obs_data, size = 2,
                                               fill = NA, stroke = 1,
                                               shape = obs_shape[1])
                        }
                  }
            }
      }

      ## figure_type: means only -----------------------------------------------------------
      if(figure_type == "means only"){

            if(length(MyEffector) > 0 && complete.cases(MyEffector[1]) &&
               MyEffector[1] != "none" & compoundToExtract != "inhibitor 1"){

                  A <- ggplot(sim_data_mean %>%
                                    filter(Trial == "mean") %>%
                                    mutate(Group = paste(Group, Trial)),
                              aes(x = Time, y = Conc, linetype = Inhibitor,
                                  color = Inhibitor)) +
                        geom_line(lwd = 1) +
                        scale_linetype_manual(values = line_type[1:2]) +
                        scale_color_manual(values = line_color[1:2])

            } else {

                  A <- ggplot(sim_data_mean %>%
                                    filter(Trial == "mean"),
                              aes(x = Time, y = Conc)) +
                        geom_line(lwd = 1, linetype = line_type[1],
                                  color = line_color[1])
            }
      }

      if(nrow(obs_data) == 0){
            A <- A + guides(shape = "none")
      }

      if(complete.cases(obs_data_option) & obs_data_option == "mean bars" &
         figure_type != "means only"){
            A <- A +
                  geom_errorbar(data = obs_data,
                                aes(x = Time, ymin = Conc - SDConc,
                                    ymax = Conc + SDConc),
                                width = (time_range[2] - time_range[1])/80)
      }

      # Applying aesthetics ------------------------------------------------
      if(t0 != "simulation start"){
            time_range_relative <- time_range - t0_num
      } else {
            time_range_relative <- time_range
      }

      A <- A +
            scale_x_continuous(breaks = XBreaks, labels = XLabels,
                               expand = expansion(
                                     mult = c(ifelse(pad_x_axis, 0.02, 0), 0.04))) +
            scale_y_continuous(limits = c(0, YmaxRnd), breaks = YBreaks,
                               labels = YLabels,
                               expand = expansion(mult = c(0, 0.1))) +
            labs(x = xlab, y = ylab,
                 linetype = ifelse(complete.cases(legend_label),
                                   legend_label, "Inhibitor"),
                 shape = ifelse(complete.cases(legend_label),
                                legend_label, "Inhibitor")) +
            coord_cartesian(xlim = time_range_relative) +
            theme(panel.background = element_rect(fill="white", color=NA),
                  legend.key = element_rect(fill = "white"),
                  axis.ticks = element_line(color = "black"),
                  axis.text = element_text(color = "black"),
                  axis.title = element_text(color = "black", face = "bold"),
                  axis.line.x.bottom = element_line(color = "black"),
                  axis.line.y.left = element_line(color = "black"),
                  text = element_text(family = "Calibri"))

      # If the user didn't want the legend or if the graph is of Inhibitor1,
      # remove legend.
      if(include_legend == FALSE | compoundToExtract == "inhibitor 1"){
            A <- A + theme(legend.position = "none")
      }

      ## semi-log plot
      if(is.na(y_axis_limits_log[1])){ # Option to consider for the future: Allow user to specify only the upper limit, which would leave y_axis_limits_log[1] as NA?

            Ylim_log <- Ylim

            near_match <- function(x, t) {x[which.min(abs(t - x))]} # LS to HB: Clever solution to this problem! :-)

            Ylim_log[1] <- Ylim_data %>%
                  filter(Time == near_match(Ylim_data$Time, time_range_relative[2])) %>%
                  pull(Conc) %>% min()
            Ylim_log[1] <- round_down(Ylim_log[1])
            Ylim_log[2] <- round_up(Ylim[2])

      } else {
            # Having trouble w/our current setup sometimes clipping early data,
            # especially when figure type is trial means. Allowing user to
            # specify y axis limits here.
            Ylim_log <- y_axis_limits_log
            Ylim_log[1] <- round_down(Ylim_log[1])
            Ylim_log[2] <- round_up(Ylim[2])
      }

      YLogBreaks <- as.vector(outer(1:9, 10^(log10(Ylim_log[1]):log10(Ylim_log[2]))))
      YLogBreaks <- YLogBreaks[YLogBreaks >= Ylim_log[1] & YLogBreaks <= Ylim_log[2]]
      YLogLabels   <- rep("",length(YLogBreaks))
      YLogLabels[seq(1,length(YLogLabels),9)] <- YLogBreaks[seq(1,length(YLogLabels),9)]    # add labels at order of magnitude

      B <- suppressMessages(
            A + scale_y_log10(limits = Ylim_log, breaks = YLogBreaks,
                              labels = YLogLabels) +
                  # labels = function(.) format(., scientific = FALSE, drop0trailing = TRUE)) +
                  coord_cartesian(xlim = time_range_relative)
      )

      # both plots together, aligned vertically
      if(compoundToExtract == "inhibitor 1"){
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




