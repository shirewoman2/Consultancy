

#' ct_plot
#'
#' This function takes observed and simulated concentration-time data and makes
#' publication-quality graphs.
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   concentration-time data
#' @param sim_data_cells which cells in the Excel sheet "Conc Profiles
#'   CSys(CPlasma)" contain your data, e.g., "D29:GU32". You don't need to
#'   change this every time if the number of sampling stays the same.
#' @param obs_data_file names of the Excel file containing the observed
#'   concentration-time data. This is the file that it is ready to be converted
#'   to an XML file, not the file that contains only the digitized time and
#'   concentration data.
#' @param obs_data_cells which cells contain the observed data, e.g., "A11:C259"
#' @param xlab text you want for the x-axis title
#' @param ylab text you want for the y-axis title
#' @param save TRUE or FALSE: Save the graph?
#' @param figname If \code{save} is TRUE, the file name for the output figure, e.g., "Study 001 SD.png"
#'
#' @return
#' @export
#'
#' @examples
#' # (These won't actually work yet. -LS)
#' sim_data_file <- "Delamanid output v8.xlsx"
#' sim_data_cells <- "D29:GU32"
#'
#' obs_data_file <- "fig1-242-06-001-SD - for XML conversion.xlsx"
#' obs_data_cells <- "A11:C259"
#' xlab <- "Time (hr)"
#' ylab <- "Concentration (uM)"
#' ct_plot(sim_data_file, sim_data_cells, obs_data_file, obs_data_cells,
#'    xlab = "Time (hr)", ylab = "Concentration (uM)")
#'
ct_plot <- function(sim_data_file,
                    sim_data_cells,
                    obs_data_file,
                    obs_data_cells,
                    save = FALSE,
                    figname = NA){

      sim_data <- readxl::read_excel(path = sim_data_file,
                             sheet = "Conc Profiles CSys(CPlasma)",
                             range = sim_data_cells,
                             col_names = FALSE) %>% t() %>% as.data.frame() %>%
            rename(time = V1, mean = V2, per95 = V3, per5 = V4)

      ## read in observed data that Louise prepared, remember to specify range
      obs_data <- readxl::read_excel(path = obs_data_file,
                                range = obs_data_cells) %>% drop_na()

      # Converting to appropriate ConcUnits as necessary
      ConcUnits <- readxl::read_excel(path = obs_data_file,
                                  range = "D5", col_names = FALSE) %>%
         pull()

      TimeUnits <- readxl::read_excel(path = obs_data_file,
                                      range = "A5", col_names = FALSE) %>%
         pull()

      ConversionFactor <- switch(ConcUnits,
                                 "ng/mL" = 1/1000,
                                 "µg/mL" = 1,
                                 "ng/L" = 1/1e6)

      if(is.null(ConversionFactor)){
         ConversionFactor <- 1
      }

      obs_data <- obs_data %>% mutate(DV = DV*ConversionFactor)


      # Adjusting graph labels as appropriate for the observed data
      xlab <- switch(TimeUnits,
                     "Hours" = "Time (hr)",
                     "Minutes" = "Time (min)")

      ylab <- switch(ConcUnits,
                     "µg/mL" = expression(Concentration~"("*mu*g/mL*")"),
                     "ng/mL" = "Concentration (ng/mL)",
                     "ng/L" = "Concentration (ng/L)",
                     "uM" = expression(Concentration~"("*mu*M*")"),
                     "nM" = "Concentration (nM)",
                     "mg" = "Concentration (mg)",
                     "mL" = "mL",
                     "PD response" = "PD response")

      # # Freddy's original graphing code:
      # A <- ## normal scale plot
      #       ggplot(sim_data, aes(x = time)) +
      #       geom_ribbon(aes(ymin = per5, ymax = per95), alpha = 0.2) +
      #       geom_line(aes(y = mean), lwd = 1.2) +
      #       geom_point(data = obs_data, aes(x = Time, y = DV),
      #                  size = 2, shape = 21, fill = "white", alpha = 0.8) +
      #       labs(x = xlab, y = ylab) +
      #       theme_bw()
      # B <- ## semi-log scale plot
      #       A +
      #       scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
      #                     labels = scales::trans_format("log10", scales::math_format(10^.x)))

      # Freddy's revised graphing code:
      A <- ## normal scale plot
         ggplot(sim_data, aes(x = time)) +
         geom_ribbon(aes(ymin = per5, ymax = per95), alpha = 0.2) +
         geom_line(aes(y = mean), lwd = 1.2) +
         geom_point(data = obs_data, aes(x = Time, y = DV),
                    size = 2, shape = 21, fill = "white", alpha = 0.8) +
         scale_x_continuous(breaks = function(x) seq(0, max(x), 4)) +
         labs(x = xlab, y = ylab) +
         theme_bw()
      B <- ## semi-log scale plot
         A +
         #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
         #              labels = trans_format("log10", math_format(10^.x))) +
         scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000),
                       labels = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000)) +
         coord_cartesian(ylim = c(0.01, NA)) ## adjust y-axis range


      gridExtra::grid.arrange(A, B, ncol = 2) ## this allows you to look at the plot in R
      AB <- gridExtra::arrangeGrob(grobs = list(A, B), ncol = 2)

      if(save){
            ggsave(filename = figname, plot = AB,
                   width = 8, height = 4, dpi = 500) ## you can change dimension and resolution
      }

}




