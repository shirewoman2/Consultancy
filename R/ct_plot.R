

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
                    xlab,
                    ylab,
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

      # Converting to appropriate units. Need to figure out how to do this
      # conditionally...
      obs_data <- obs_data %>%
            mutate(DV = DV/1000)

      A <- ## normal scale plot
            ggplot(sim_data, aes(x = time)) +
            geom_ribbon(aes(ymin = per5, ymax = per95), alpha = 0.2) +
            geom_line(aes(y = mean), lwd = 1.2) +
            geom_point(data = obs_data, aes(x = Time, y = DV),
                       size = 2, shape = 21, fill = "white", alpha = 0.8) +
            labs(x = xlab, y = ylab) +
            theme_bw()
      B <- ## semi-log scale plot
            A +
            scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                          labels = scales::trans_format("log10", scales::math_format(10^.x)))
      gridExtra::grid.arrange(A, B, ncol = 2) ## this allows you to look at the plot in R
      AB <- gridExtra::arrangeGrob(grobs = list(A, B), ncol = 2)

      if(save){
            ggsave(filename = figname, plot = AB,
                   width = 8, height = 4, dpi = 500) ## you can change dimension and resolution
      }

}




