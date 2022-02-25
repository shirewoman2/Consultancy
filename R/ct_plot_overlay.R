#' Overlay multiple data sets onto a single concentration-time graph
#'
#' \code{ct_plot_overlay} is meant to be used in conjunction with
#' \code{\link{extractConcTime_mult}} to create single graphs with overlaid
#' concentration-time data from multiple Simcyp Simulator output files for easy
#' comparisons. UNDER CONSTRUCTION.
#'
#' @param sim_obs_dataframe the data.frame with multiple sets of concentration-time
#'   data, including observed data where appropriate
#' @param aggregate_option What type of aggregate measure should be shown?
#'   Options are any combination of "mean" (arithmetic mean), "geomean"
#'   (geometric mean), "median", "per5" (5th percentile), "per95" (95th
#'   percentile), "per10" (10th percentile), or "per90" (90th percentile).
#'   Default is "geomean". Example of useage: \code{aggregate_option = c("mean",
#'   "per5", "per95")}
#' @param colorBy What column in \code{sim_obs_dataframe} should be used for coloring
#'   the lines and/or points on the graph? This should be unquoted, e.g.,
#'   \code{colorBy = Tissue}.
#' @param facet_column1 If you would like to break up your graph into small
#'   multiples, you can break the graphs up by up to two columns in
#'   \code{sim_obs_dataframe}. What should be the 1st column to break up the data by?
#'   This should be unquoted.
#' @param facet_column2 What should be the 2nd column to break up the data into
#'   small multiples by? This should be unquoted.
#'
#' @return
#' @export
#'
#' @examples
#' # No examples yet
#' # ct_plot_overlay(sim_obs_dataframe = CT, facet_column1 = Compound,
#' #                 facet_column2 = Tissue)
#'
#'
ct_plot_overlay <- function(sim_obs_dataframe,
                            aggregate_option = "geomean",
                            colorBy = File,
                            linetypeBy = Inhibitor,
                            facet_column1,
                            facet_column2){
    
    colorBy <- rlang::enquo(colorBy)
    linetypeBy <- rlang::enquo(linetypeBy)
    facet_column1 <- rlang::enquo(facet_column1)
    facet_column2 <- rlang::enquo(facet_column2)
    
    sim_obs_dataframe <- sim_obs_dataframe %>%
        mutate(Group = paste(File, Trial, Tissue, CompoundID, Compound, Inhibitor))
    
    obs_data <- sim_obs_dataframe %>% filter(Simulated == FALSE)
    sim_obs_dataframe <- sim_obs_dataframe %>%
        filter(Trial %in% aggregate_option & Simulated == TRUE)
    
    ggplot(sim_obs_dataframe,
           aes(x = Time, y = Conc, color = !!colorBy, linetype = !!linetypeBy,
               group = Group)) +
        geom_line() +
        geom_point(data = obs_data) +
        facet_grid(rows = vars(!!facet_column1),
                   cols = vars(!!facet_column2),
                   scales = "free") +
        theme(panel.background = element_rect(fill = "white", color = NA),
              panel.border = element_rect(color = "black", fill = NA),
              strip.background = element_rect(fill = "white"),
              legend.key = element_rect(fill = "white"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(color = "black"),
              axis.title = element_text(color = "black", face = "bold"),
              axis.line.y = element_line(color = "black"),
              axis.line.x.bottom = element_line(color = "black"),
              text = element_text(family = "Calibri"))
    
}

# ct_plot_overlay(sim_obs_dataframe = CT, facet_column1 = Compound,
#                 facet_column2 = Tissue)
#
# ct_plot_overlay(sim_obs_dataframe = CT, aggregate_option = c("mean", "per5", "per95"),
#                 facet_column1 = Compound,
#                 facet_column2 = Tissue, linetype = Trial)



