#' Overlay multiple data sets onto a single concentration-time graph
#'
#' \code{ct_plot_overlay} is meant to be used in conjunction with
#' \code{\link{extractConcTime_mult}} to create single graphs with overlaid
#' concentration-time data from multiple Simcyp Simulator output files for easy
#' comparisons. UNDER CONSTRUCTION.
#'
#' @param sim_obs_dataframe the data.frame with multiple sets of
#'   concentration-time data, including observed data where appropriate
#' @param mean_type show "geometric" (default) or "arithmetic" means for the
#'   main (thickest) line for each data set. If this summary stat is not
#'   available in the simulator output, it will not be plotted.
#' @param colorBy What column in \code{sim_obs_dataframe} should be used for
#'   coloring the lines and/or points on the graph? This should be unquoted,
#'   e.g., \code{colorBy = Tissue}.
#' @param facet_column1 If you would like to break up your graph into small
#'   multiples, you can break the graphs up by up to two columns in
#'   \code{sim_obs_dataframe}. What should be the 1st column to break up the
#'   data by? This should be unquoted.
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
        mutate(Group = paste(File, Trial, Tissue, CompoundID, Compound,
                             Inhibitor, subsection_ADAM))
    
    obs_data <- sim_obs_dataframe %>% filter(Simulated == FALSE)
    sim_dataframe <- sim_obs_dataframe %>%
        filter(Simulated == TRUE)
    
    message("This graph contains the following unique combinations of data (make sure they are what you were expecting):")
    print(sim_obs_dataframe %>% 
              filter(Trial == {{aggregate_option}}) %>% 
              select(File, Tissue, CompoundID, Compound, Inhibitor, 
                     subsection_ADAM) %>% unique())
    
    UniqueGroups <- sim_obs_dataframe %>% 
        summarize(across(.cols = c(File, Tissue, CompoundID, Compound,
                                   Inhibitor, subsection_ADAM), 
                         .fns = function(x) length(unique(x)))) %>% 
        t() %>% as.data.frame() %>% 
        mutate(MyCols = rownames(.)) %>% 
        filter(V1 > 1) %>% pull(MyCols)
    
    MyAES <- c("color" = as_label(quo(colorBy)), 
               "linetype" = as_label(quo(linetypeBy)), 
               "facet1" = as_label(quo(facet_column1)), 
               "facet2" = as_label(quo(facet_column2)))
    UniqueAES <- MyAES[which(complete.cases(MyAES))]
    
    if(length(UniqueGroups[UniqueGroups != "CompoundID"]) > length(UniqueAES)){
    warning(paste("You have requested", length(UniqueGroups),
                  "unique data sets but only", 
                  length(which(complete.cases(MyAES))), 
                  "unique aesthetics for denoting those datasets. This is likely to result in an unclear graph."))
    message(paste("Unique datasets:", str_comma(UniqueGroups)))
    message(paste("Unique aesthetics:", str_comma(UniqueAES)))
    }
    
    ggplot(sim_dataframe %>% filter(Trial == {{aggregate_option}}),
           # aes(x = Time, y = Conc, color = !!colorBy, linetype = !!linetypeBy, # Comment this for easier code development
           aes(x = Time, y = Conc, color = colorBy, linetype = linetypeBy, # Uncomment this for easier code development
               group = Group)) +
        geom_line() +
        geom_point(data = obs_data) +
        facet_grid(rows = vars(!!facet_column1), # Comment this for easier code development
                   cols = vars(!!facet_column2)) + # Comment this for easier code development
        # facet_grid(rows = vars(facet_column1), # Uncomment this for easier code development
        #            cols = vars(facet_column2), # Uncomment this for easier code development
        #            scales = "free") +
        scale_linetype_manual(values = rep("solid", 4)) +
        # scale_x_continuous(limits = c(336-24, 336)) +
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




