#' ct_plot
#'
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
#' @param sim_obs_dataframe If you have already extracted the concentration-time
#'   data using the function \code{extractConcTime}, you can enter the name of
#'   the output data.frame from that function instead of re-reading the Excel
#'   file.
#' @param figure_type type of figure to plot. Options are:
#'
#'   \describe{
#'
#'   \item{trial means}{plots a black line for the mean data, gray lines for the
#'   individual simulated data, and open circles for the observed data}
#'
#'   \item{trial percentiles}{plots a black line for the mean data, gray lines
#'   for the 5th and 95th percentiles of the simulated data, and open circles
#'   for the observed data}
#'
#'   \item{means only}{plots a black line for the mean data and, if an effector
#'   was modeled, a dashed line for the concentration-time data with the the
#'   effector. (At present, this does NOT graph observed data because I haven't
#'   yet figured out the best way to indicate whether those data are with or
#'   without the effector present. -LS)}
#'
#'   }
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
#' # These are probably too busy when you've got an effector present:
#' ct_plot(sim_data_file = "../Example simulator output MD + inhibitor.xlsx")
#' ct_plot(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'         figure_type = "trial percentiles")
#' # so you may want to consider this alternative instead:
#' ct_plot(sim_data_file = "../Example simulator output MD + inhibitor.xlsx",
#'         figure_type = "means only")
#'
#' # If you've already got a data.frame formatted
#' like the output from extractConcTime...
#' data(ConcTime)
#' ct_plot(sim_obs_dataframe = ConcTime)
#'
ct_plot <- function(sim_data_file,
                    obs_data_file = NA,
                    sim_obs_dataframe = NA,
                    figure_type = "trial means",
                    return_data = FALSE,
                    return_indiv_graphs = FALSE){

      # Error catching
      if(length(figure_type) != 1 |
         figure_type %in% c("trial means", "trial percentiles",
                            "means only") == FALSE){
            stop("The only acceptable options for figure_type are 'method development', 'method verification', or 'means only'.")
      }

      if(is.data.frame(sim_obs_dataframe)){
            Data <- sim_obs_dataframe
      } else {
            Data <- extractConcTime(sim_data_file, obs_data_file)
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
      tlast <- max(Data$Time)

      if(TimeUnits == "hours"){

            PossBreaks <- data.frame(
                  Tlast = c(24, 48, 96, 168, 336),
                  BreaksToUse = c("24hr", "48hr", "96hr", "1wk", "2wk"))

            BreaksToUse <- PossBreaks %>% filter(tlast <= Tlast) %>%
                  slice(which.min(Tlast)) %>% pull(BreaksToUse)

            XBreaks <- switch(BreaksToUse,
                              "24hr" = seq(0, 24, 4),
                              "48hr" = seq(0, 48, 8),
                              "96hr" = seq(0, 96, 12),
                              "1wk" = seq(0, 168, 24),
                              "2wk" = seq(0, 336, 48))
      }

      if(TimeUnits == "minutes"){
            PossBreaks <- data.frame(Tlast = c(60, 240, 480, 720, 1440),
                                     BreaksToUse = c("1hr", "4hr",
                                                     "8hr", "12hr",
                                                     "24hr"))
            BreaksToUse <- PossBreaks %>% filter(tlast <= Tlast) %>%
                  slice(which.min(Tlast)) %>% pull(BreaksToUse)

            XBreaks <- switch(BreaksToUse,
                              "1hr" = seq(0, 60, 15),
                              "4hr" = seq(0, 240, 30),
                              "8hr" = seq(0, 480, 60),
                              "12hr" = seq(0, 720, 120),
                              "24hr" = seq(0, 1440, 240))
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

      # Adding a grouping variable to data.
      if("Effector" %in% names(Data)){
            Data <- Data %>% mutate(Group = paste(Compound, Effector, Trial))
      } else {
            Data <- Data %>% mutate(Group = paste(Compound, Trial))
      }

      # Separating the data by type and calculating trial means
      suppressMessages(
            sim_data_ind <- Data %>%
                  filter(Simulated == TRUE &
                               SubjectID %in% c("mean", "per5", "per95") == FALSE) %>%
                  group_by(across(any_of(c("Compound", "Effector", "Simulated", "Trial",
                                           "Time", "Time_units", "Conc_units", "Group")))) %>%
                  summarize(Conc = mean(Conc)) %>%
                  ungroup()
      )

      sim_data_mean <- Data %>%
            filter(Simulated == TRUE  &
                         SubjectID %in% c("mean", "per5", "per95"))

      obs_data <- Data %>% filter(Simulated == FALSE)


      if(figure_type == "trial means"){

            NumTrials <- length(unique(sim_data_ind$Trial))
            AlphaToUse <- ifelse(NumTrials > 50, 0.05, 0.15)
            # Adjust this as needed. May want to use "switch" as we did for XBreaks.

            if("Effector" %in% names(sim_data_ind)){

                  MyEffector <- sim_data_ind %>% filter(Compound == Effector) %>%
                        pull(Compound) %>% unique()

                  Ylim <- c(0,
                            1.1 * max(
                                  c(sim_data_ind$Conc[sim_data_ind$Compound !=
                                                            MyEffector],
                                    obs_data$Conc), na.rm = T))
                  Xlim <- c(min(XBreaks),
                            1.01 * max(c(sim_data_ind$Time,
                                         obs_data$Time)))


                  ## linear plot
                  A <- ggplot(sim_data_ind %>% filter(Compound != MyEffector),
                              aes(x = Time, y = Conc, group = Group,
                                  linetype = Effector)) +
                        guides(linetype = guide_legend(
                              override.aes=list(fill=c(NA, NA)))) +
                        geom_line(alpha = AlphaToUse, lwd = 1) +
                        geom_line(data = sim_data_mean %>%
                                        filter(SubjectID == "mean" &
                                                     Compound != MyEffector),
                                  lwd = 1) +
                        geom_point(data = obs_data, size = 2, shape = 21)


            } else {

                  Ylim <- c(0,
                            1.1 * max(c(sim_data_ind$Conc,
                                        obs_data$Conc), na.rm = T))
                  Xlim <- c(min(XBreaks),
                            1.01 * max(c(sim_data_ind$Time,
                                         obs_data$Time)))

                  ## linear plot
                  A <- ggplot(sim_data_ind,
                              aes(x = Time, y = Conc, group = Trial)) +
                        geom_line(alpha = AlphaToUse, lwd = 1) +
                        geom_line(data = sim_data_mean %>%
                                        filter(SubjectID == "mean"),
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

                  Ylim <- c(0,
                            1.1 * max(
                                  c(sim_data_mean$Conc[sim_data_mean$Compound !=
                                                             MyEffector],
                                    obs_data$Conc), na.rm = T))
                  Xlim <- c(min(XBreaks),
                            1.01 * max(c(sim_data_mean$Time,
                                         obs_data$Time)))

                  A <- ggplot(sim_data_mean %>%
                                    filter(SubjectID %in% c("per5", "per95") &
                                                 Compound != MyEffector) %>%
                                    mutate(Group = paste(Group, SubjectID)),
                              aes(x = Time, y = Conc, linetype = Effector,
                                  group = Group)) +
                        geom_line(color = "gray80", lwd = 0.8) +
                        geom_line(data = sim_data_mean %>%
                                        filter(SubjectID == "mean" &
                                                     Compound != MyEffector),
                                  lwd = 1) +
                        geom_point(data = obs_data, size = 2, shape = 21)

            } else {

                  ## linear plot

                  Ylim <- c(0,
                            1.1 * max(c(sim_data_mean$Conc,
                                        obs_data$Conc), na.rm = T))
                  Xlim <- c(min(XBreaks),
                            1.01 * max(c(sim_data_mean$Time,
                                         obs_data$Time)))

                  A <- ggplot(sim_data_mean %>% filter(SubjectID != "mean"),
                              aes(x = Time, y = Conc, group = SubjectID)) +
                        geom_line(color = "gray80", lwd = 0.8) +
                        geom_line(data = sim_data_mean %>%
                                        filter(SubjectID == "mean"), lwd = 1) +
                        geom_point(data = obs_data, size = 2, shape = 21)
            }

      }

      if(figure_type == "means only"){

            if("Effector" %in% names(sim_data_mean)){

                  MyEffector <- sim_data_mean %>%
                        filter(Compound == Effector) %>%
                        pull(Compound) %>% unique()

                  Ylim <- c(0,
                            1.1 * max(
                                  c(sim_data_mean$Conc[sim_data_mean$Compound !=
                                                             MyEffector],
                                    obs_data$Conc), na.rm = T))
                  Xlim <- c(min(XBreaks),
                            1.01 * max(c(sim_data_mean$Time,
                                         obs_data$Time)))

                  A <- ggplot(sim_data_mean %>%
                                    filter(SubjectID == "mean" &
                                                 Compound != MyEffector) %>%
                                    mutate(Group = paste(Group, SubjectID)),
                              aes(x = Time, y = Conc, linetype = Effector)) +
                        geom_line(lwd = 1)

            } else {

                  Ylim <- c(0,
                            1.1 * max(
                                  c(sim_data_mean$Conc,
                                    obs_data$Conc), na.rm = T))
                  Xlim <- c(min(XBreaks),
                            1.01 * max(c(sim_data_mean$Time,
                                         obs_data$Time)))

                  A <- ggplot(sim_data_mean %>%
                                    filter(SubjectID == "mean"),
                              aes(x = Time, y = Conc)) +
                        geom_line(lwd = 1)

            }
      }

      A <- A +
            scale_x_continuous(breaks = XBreaks, expand = c(0, 0),
                               limits = Xlim) +
            scale_y_continuous(expand = c(0, 0),
                               limits = Ylim) +
            labs(x = xlab, y = ylab) +
            theme(panel.background = element_rect(fill="white", color=NA),
                  legend.key = element_rect(fill = "white"),
                  axis.line.x.bottom = element_line(color = "grey60"),
                  axis.line.y.left = element_line(color = "grey60"),
                  axis.ticks = element_line(color = "grey60"),
                  text = element_text(family = "Calibri")
            )

      ## semi-log plot
      B <- suppressMessages(
            A + scale_y_log10(labels = scales::comma)
      )

      # suppressWarnings(gridExtra::grid.arrange(A, B, ncol = 2)) ## this allows you to look at the plot in R
      AB <- suppressWarnings(
            ggpubr::ggarrange(A, B, ncol = 1, labels = c("A", "B"),
                              common.legend = TRUE, legend = "right")
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




