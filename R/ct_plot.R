#' ct_plot
#'
#' This function takes observed and simulated concentration-time data and makes
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
#' @param figure_type type of figure to plot. Options are: \describe{
#'   \item{method development}{plots a black line for the mean data, gray lines
#'   for the individual simulated data, and open circles for the observed data}
#'   \item{method verification}{plots a black line for the mean data, gray lines
#'   for the 5th and 95th percentiles of the simulated data, and open circles
#'   for the observed data} }
#' @param save TRUE or FALSE: Save the graph?
#' @param figname If \code{save} is TRUE, the file name for the output figure,
#'   e.g., "Study 001 SD.png"
#' @param return_data TRUE or FALSE: Return the data used in the graphs? If
#'   TRUE, this will return a named list of: \describe{ \item{Graphs}{the set of
#'   graphs} \item{Data}{a data.frame of the concentration-time data used in the
#'   set of graphs} }
#'
#' @return
#' @export
#'
#' @examples
#' sim_data_file <- "../Example simulator output.xlsx"
#'
#' obs_data_file <- "../fig1-242-06-001-MD - for XML conversion.xlsx"
#'
#' ct_plot(sim_data_file)
#' ct_plot(sim_data_file, obs_data_file, figure_type = "method verification")
#' ct_plot(sim_data_file, return_data = TRUE)
#'
ct_plot <- function(sim_data_file,
                    obs_data_file = NA,
                    figure_type = "method development",
                    save = FALSE,
                    figname = NA,
                    return_data = FALSE){

   # Error catching
   if(length(figure_type) != 1 |
      figure_type %in% c("method development", "method verification")){
      stop("The only acceptable options for figure_type are 'method development' or 'method verification'.")
   }

   # Getting summary data for the simulation
   SimSummary <- suppressMessages(
      readxl::read_excel(path = sim_data_file, sheet = "Summary",
                         col_names = FALSE))

   # Reading in simulated concentration-time profile data
   sim_data_xl <- suppressMessages(
      readxl::read_excel(path = sim_data_file,
                         sheet = "Conc Profiles CSys(CPlasma)",
                         col_names = FALSE))

   # mean data
   StartRow_mean <- which(sim_data_xl$...1 == "Population Statistics") + 1
   sim_data_mean <- sim_data_xl[StartRow_mean:(StartRow_mean+3), ] %>% t() %>%
      as.data.frame() %>% slice(-(1:4)) %>%
      mutate_all(as.numeric) %>%
      rename(Time = "V1", Mean = "V2", per5 = "V3", per95 = "V4") %>%
      pivot_longer(names_to = "ID", values_to = "Conc", cols = -Time)

   # individual data
   StartRow_ind <- which(sim_data_xl$...1 == "Individual Statistics") + 3
   EndRow_ind <- which(sim_data_xl$...1 == "Observed Data") - 2
   sim_data_ind <- sim_data_xl[StartRow_ind:EndRow_ind, ] %>% t() %>%
      as.data.frame() %>% slice(-(1:3)) %>%
      mutate_all(as.numeric) %>%
      rename(Time = "V1") %>%
      pivot_longer(names_to = "ID", values_to = "Conc", cols = -Time)

   # Determining concentration units
   SimConcUnits <- str_extract(sim_data_xl[3, "...1"], "[µumnp]?g/m?L")

   # If the user did not specify a file to use for observed data, use the
   # observed data that they included for the simulation.
   if(is.na(obs_data_file)){

      StartRow_obs <- which(sim_data_xl$...1 == "Observed Data") + 1
      obs_data <- sim_data_xl[StartRow_obs:(StartRow_obs+1), ] %>% t() %>%
         as.data.frame() %>% slice(-1) %>%
         mutate_all(as.numeric) %>%
         rename(Time = "V1", Conc = "V2") %>% filter(complete.cases(Time)) %>%
         mutate(ID = "obs")

   } else {
      # If the user did specify an observed data file, read in observed data.
      obs_data_xl <- suppressMessages(
         readxl::read_excel(path = obs_data_file, col_names = FALSE))

      obs_data <- obs_data_xl[12:nrow(obs_data_xl), 2:3] %>%
         filter(complete.cases(...3)) %>%
         rename(Time = ...2, Conc = ...3) %>%
         mutate_all(as.numeric) %>%
         mutate(ID = "obs")

      TimeUnits <- suppressMessages(
         readxl::read_excel(path = obs_data_file,
                            range = "A5", col_names = FALSE)) %>%
         pull()

      # Converting to appropriate ObsConcUnits as necessary
      ObsConcUnits <- suppressMessages(
         readxl::read_excel(path = obs_data_file,
                            range = "D5", col_names = FALSE)) %>%
         pull()

      if(ObsConcUnits != SimConcUnits){

         # Starting with this table of conversion factors, which is assuredly not
         # exhaustive. Add to this as needed.
         ConvTable <- data.frame(ObsUnits = c("ng/mL", "ng/mL",
                                              "ng/mL", "pg/mL",
                                              "µg/mL"),
                                 SimUnits = c("mg/L", "µg/mL",
                                              "ng/L", "mg/L",
                                              "ng/mL"),
                                 Factor = c(10^3, 10^3,
                                            10^-3, 10^6,
                                            10^-3))

         if(SimConcUnits %in% ConvTable$SimUnits == FALSE |
            ObsConcUnits %in% ConvTable$ObsUnits == FALSE |
            all(c(SimConcUnits, ObsConcUnits) %in% c("µg/mL", "ng/mL", "ng/L",
                                                     "µM", "nM", "mg", "mL",
                                                     "PD response") == FALSE)){
            stop("Our apologies, but we have not yet set up this function to deal with your concentration units. Please tell the Consultancy Team R working group what units you're working with and we can fix this.")
         }

         ConversionFactor <-
            ConvTable$Factor[which(ConvTable$SimUnits == SimConcUnits &
                                      ConvTable$ObsUnits == ObsConcUnits)]

         sim_data_ind <- sim_data_ind %>%
            mutate(Conc = Conc*ConversionFactor)
         sim_data_mean <- sim_data_mean %>%
            mutate(Conc = Conc*ConversionFactor)
      }

   }

   # If this were a multiple-dose simulation, the observed data is, presumably,
   # at steady state. The simulated time we'd want those data to match would be
   # the *last* dose. Adjusting the time for the obs data.
   DosingScenario <- SimSummary %>%
      slice(which(...5 == "Dosing Regimen")) %>% pull(...6)

   if(DosingScenario == "Multiple Dose"){

      DoseFreq <- SimSummary %>%
         slice(which(str_detect(...5, "Dose Interval"))) %>%
         pull(...6) %>% as.numeric()
      NumDoses <- SimSummary %>%
         slice(which(str_detect(...5, "Number of Doses"))) %>%
         pull(...6) %>% as.numeric()

      LastDoseTime <- DoseFreq * (NumDoses - 1)

      obs_data <- obs_data %>% mutate(Time = Time + LastDoseTime)

   }

   # Adjusting graph labels as appropriate for the observed data
   xlab <- switch(TimeUnits,
                  "Hours" = "Time (hr)",
                  "Minutes" = "Time (min)")

   ylab <- switch(ObsConcUnits,
                  "µg/mL" = expression(Concentration~"("*mu*g/mL*")"),
                  "ng/mL" = "Concentration (ng/mL)",
                  "ng/L" = "Concentration (ng/L)",
                  "µM" = expression(Concentration~"("*mu*M*")"),
                  "nM" = "Concentration (nM)",
                  "mg" = "Concentration (mg)",
                  "mL" = "mL",
                  "PD response" = "PD response")

   # Setting the breaks for the x axis
   tlast <- max(c(obs_data$Time, sim_data_mean$Time))

   if(TimeUnits == "Hours"){

      PossBreaks <- data.frame(
         Tlast = c(24, 48, 96, 168, 336),
         BreaksToUse = c("24hr", "48hr", "96hr", "1wk", "2wk"))

      BreaksToUse <- PossBreaks %>% filter(tlast <= Tlast) %>%
         slice(which.max(Tlast)) %>% pull(BreaksToUse)

      XBreaks <- switch(BreaksToUse,
                        "24hr" = seq(0, 24, 4),
                        "48hr" = seq(0, 48, 8),
                        "96hr" = seq(0, 96, 12),
                        "1wk" = seq(0, 168, 24),
                        "2wk" = seq(0, 336, 48))
   }

   if(TimeUnits == "Minutes"){
      PossBreaks <- data.frame(Tlast = c(60, 240, 480),
                               BreaksToUse = c("1hr", "4hr",
                                               "8hr", "12hr",
                                               "24hr"))
      BreaksToUse <- PossBreaks %>% filter(Tlast <= tlast) %>%
         slice(which.max(Tlast)) %>% pull(BreaksToUse)

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


   if(figure_type == "method development"){

      NumSubj <- length(unique(sim_data_ind$ID))
      AlphaToUse <- ifelse(NumSubj > 50, 0.01, 0.2)
      # Adjust this as needed. May want to use "switch" as we did for XBreaks.

      ## normal scale plot
      A <- ggplot(sim_data_ind, aes(x = Time, y = Conc, group = ID)) +
         geom_line(alpha = AlphaToUse, lwd = 1) +
         geom_line(data = sim_data_mean %>% filter(ID == "Mean"), lwd = 1) +
         geom_point(data = obs_data, size = 2, shape = 21) +
         scale_x_continuous(breaks = XBreaks, expand = c(0, 0)) +
         scale_y_continuous(expand = c(0, 0)) +
         labs(x = xlab, y = ylab) +
         theme(panel.background = element_rect(fill="white", color=NA),
               axis.line.x.bottom = element_line(color = "grey60"),
               axis.line.y.left = element_line(color = "grey60"),
               axis.ticks = element_line(color = "grey60")
         )

      ## semi-log scale plot
      B <- suppressMessages(
         A + scale_y_log10(labels = scales::comma)
      )

   } else {

      # method verification style plot

      ## normal scale plot
      A <- ggplot(sim_data_mean %>% filter(ID != "Mean"),
                  aes(x = Time, y = Conc, group = ID)) +
         geom_line(color = "gray80", lwd = 0.8) +
         geom_line(data = sim_data_mean %>% filter(ID == "Mean"), lwd = 1) +
         geom_point(data = obs_data, size = 2, shape = 21) +
         scale_x_continuous(breaks = XBreaks, expand = c(0, 0)) +
         scale_y_continuous(expand = c(0, 0)) +
         labs(x = xlab, y = ylab) +
         theme(panel.background = element_rect(fill="white", color=NA),
               axis.line.x.bottom = element_line(color = "grey60"),
               axis.line.y.left = element_line(color = "grey60"),
               axis.ticks = element_line(color = "grey60")
         )

      ## semi-log scale plot
      B <- suppressMessages(
         A + scale_y_log10(labels = scales::comma)
      )

   }

   # suppressWarnings(gridExtra::grid.arrange(A, B, ncol = 2)) ## this allows you to look at the plot in R
   AB <- suppressWarnings(
      ggpubr::ggarrange(A, B, ncol = 1, labels = c("A", "B")))

   if(save){
      ggsave(filename = figname, plot = AB,
             width = 8, height = 4, dpi = 600) ## you can change dimension and resolution
   }

   if(return_data){
      Data <- bind_rows(
         obs_data %>% mutate(Simulated = FALSE),

         sim_data_ind %>% mutate(Simulated = TRUE,
                                 ID = sub("V", "Subject ", ID)) %>%
            arrange(ID, Time),
         sim_data_mean %>% mutate(Simulated = TRUE) %>%
            arrange(ID, Time))

      Out <- list(AB, Data)
      names(Out) <- c("Graphs", "Data")

      return(Out)

   } else {
      return(AB)
   }

}




