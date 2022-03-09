#' Overlay multiple data sets onto a single concentration-time graph
#'
#' \code{ct_plot_overlay} is meant to be used in conjunction with
#' \code{\link{extractConcTime_mult}} to create single graphs with overlaid
#' concentration-time data from multiple Simcyp Simulator output files for easy
#' comparisons. \strong{Note:} This has not been set up yet to plot enzyme
#' abundance data, ADAM-model simulations, or observed data. Really, this is
#' generally workable but is admittedly not one of our most maturely developed
#' functions. :-)
#'
#' @param sim_obs_dataframe the data.frame with multiple sets of
#'   concentration-time data. At the moment, this function does not plot
#'   observed data, though; that's still under construction.
#' @param mean_type show "geometric" means (default) or "arithmetic" means for
#'   the main (thickest or only) line for each data set. If this summary stat is
#'   not available in the simulator output, the other one will be plotted.
#' @param linear_or_log the type of graph to be returned. Options: "semi-log",
#'   "linear", or "both".
#' @param colorBy What column in \code{sim_obs_dataframe} should be used for
#'   coloring the lines and/or points on the graph? This should be unquoted,
#'   e.g., \code{colorBy = Tissue}.
#' @param facet_column1 If you would like to break up your graph into small
#'   multiples, you can break the graphs up by up to two columns in
#'   \code{sim_obs_dataframe}. What should be the 1st column to break up the
#'   data by? This should be unquoted.
#' @param facet_column2 What should be the 2nd column to break up the data into
#'   small multiples by? This should be unquoted.
#' @param floating_facet_scale TRUE or FALSE for whether to allow the axes for
#'   each facet of a multi-facetted graph to scale freely according to what data
#'   are present. Default is FALSE, which means that all data will be on the
#'   same scale for easy comparison. However, this could mean that some graphs
#'   have lines that are hard to see, so you can set this to TRUE to allow the
#'   axes to shrink or expand according to what data are present for that facet.
#'   Floating axes comes with a trade-off for the looks of the graphs, though:
#'   Setting this to TRUE does mean that your x axis won't have pretty breaks
#'   that are sensible for times in hours and that your y axis won't have minor
#'   ticks.
#' @param time_range time range to show relative to the start of the simulation.
#'   A note on how this differs from \code{\link{ct_plot}}: Since this function
#'   does allow for multiple files to be plotted, please note that you cannot
#'   specify a dose number for the time range as you can with \code{ct_plot};
#'   that's because we wouldn't know which simulation you wanted that dose
#'   number for. Options that will work here: \describe{
#'
#'   \item{NA}{entire time range of data}
#'
#'   \item{a start time and end time in hours}{only data in that time range,
#'   e.g. \code{c(24, 48)}}}
#'
#'
#' @param pad_x_axis Optionally add a smidge of padding to the left side of the
#'   x axis. If left as FALSE, the y axis will be placed right at the beginning
#'   of your time range. If set to TRUE, there will be a little bit of space
#'   between the y axis and the start of your time range. If you want a
#'   \emph{specific} amount of x axis padding, set this to a number; the default
#'   is 0.02, which adds 2 percent more space to the left side of the axis.
#' @param pad_y_axis Similar to the \code{pad_x_axis} argument, optionally add a
#'   smidge of padding to the bottom of the y axis. As with \code{pad_x_axis},
#'   the default (FALSE) is no padding, but you can set this to either TRUE to
#'   get 2 percent more space on the bottom of the y axis or set it to a number
#'   to get a specific amount of padding there.
#' @param x_axis_interval Set the x-axis major tick-mark interval. Acceptable
#'   input: any number or leave as NA to accept default values.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png". If you leave off ".png", it
#'   will be saved as a png file, but if you specify a different file extension,
#'   it will be saved as that file format. Acceptable extensions are "eps",
#'   "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg".
#'   Leaving this as NA means the file will not be automatically saved to disk.
#' @param fig_height figure height in inches; default is 6
#' @param fig_width figure width in inches; default is 5
#'
#' @param y_axis_limits_lin Optionally set the Y axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as NA, the Y axis limits for the
#'   linear plot will be automatically selected.
#' @param y_axis_limits_log Optionally set the Y axis limits for the semi-log
#'   plot, e.g., \code{c(10, 1000)}. Values will be rounded down and up,
#'   respectively, to the nearest order of magnitude. If left as NA, the Y axis
#'   limits for the semi-log plot will be automatically selected.
#'
#'
#'
#' @return
#' @export
#'
#' @examples
#' # Use syntax like this:
#' # ct_plot_overlay(sim_obs_dataframe = CT, facet_column1 = Compound,
#' #                 facet_column2 = Tissue)
#'
#' 
ct_plot_overlay <- function(sim_obs_dataframe,
                            mean_type = "geometric",
                            linear_or_log = "semi-log",
                            colorBy = File,
                            facet_column1,
                            facet_column2, 
                            floating_facet_scale = FALSE,
                            time_range = NA, 
                            x_axis_interval = NA,
                            pad_x_axis = FALSE,
                            pad_y_axis = FALSE,
                            y_axis_limits_lin = NA,
                            y_axis_limits_log = NA, 
                            save_graph = NA,
                            fig_height = 6,
                            fig_width = 5){
    
    colorBy <- rlang::enquo(colorBy)
    facet_column1 <- rlang::enquo(facet_column1)
    facet_column2 <- rlang::enquo(facet_column2)
    
    if(length(time_range) == 1){
        if(complete.cases(time_range)){
            warning("You have specified only 1 value for the time range, and we're not sure whether you want that to be the start or the end time. The full time range of all simulations will be used.")
            time_range <- NA
        }
    } else {
        if(length(time_range) > 2){
            warning("You have specified more than 2 values for the time range, which only calls for a start time and an end time. Only the 1st two values you listed will be used for the time range.")
            time_range <- time_range[1:2]
        } 
        
        if(class(time_range) != "numeric"){
            warning("You have not specified numeric data for the start and end of your time range, which is the input required for this function. The full time range will be used.")
            time_range <- NA
        }
    }
    
    sim_obs_dataframe <- sim_obs_dataframe %>% 
        # At least at this point, I can't see this function working well with
        # ADAM model data b/c the y axis units differ. Removing all ADAM model
        # data. May need to tweak this b/c you can get data from colon w/out
        # ADAM model; return to this later to make more adaptable.
        filter(!Tissue %in% c("stomach", "duodenum", "jejunum I", "jejunum II", "ileum I",
                              "ileum II", "ileum III", "ileum IV", "colon", "faeces")) %>%
        # If it's dose number 0, remove those rows so that we'll show only the
        # parts we want when facetting and user wants scales to float freely.
        filter(DoseNum != 0) %>% 
        mutate(Group = paste(File, Trial, Tissue, CompoundID, Compound,
                             Inhibitor),
               CompoundID = factor(CompoundID,
                                   levels = c("substrate", "primary metabolite 1",
                                              "primary metabolite 2", "secondary metabolite",
                                              "inhibitor 1", "inhibitor 1 metabolite", 
                                              "inhibitor 2"))) 
    
    obs_data <- sim_obs_dataframe %>% filter(Simulated == FALSE)
    sim_dataframe <- sim_obs_dataframe %>%
        filter(Simulated == TRUE)
    
    if(switch(mean_type, "geometric" = "geomean", 
              "arithmetic" = "mean") %in% sim_obs_dataframe$Trial == FALSE){
        
        MyMeanType <- sim_obs_dataframe %>%
            filter(Trial %in% c("geomean", "mean")) %>% 
            pull(Trial) %>% unique() %>% as.character()
        
        warning(paste0("You requested the ", mean_type, 
                       " mean, but that is not included in your data. Instead, the ",
                       switch(MyMeanType, "geometric" = "geomean", 
                              "mean" = "arithmetic"), 
                       " mean will be used."))
    } else {
        MyMeanType <- switch(mean_type, "geometric" = "geomean", 
                             "arithmetic" = "mean")
    }
    
    message("This graph contains the following unique combinations of data (make sure they are what you were expecting):")
    print(sim_obs_dataframe %>% 
              filter(Trial == MyMeanType) %>% 
              select(File, Tissue, CompoundID, Compound, Inhibitor) %>% unique())
    
    UniqueGroups <- sim_obs_dataframe %>% 
        summarize(across(.cols = c(File, Tissue, CompoundID, Compound,
                                   Inhibitor), 
                         .fns = function(x) length(unique(x)))) %>% 
        t() %>% as.data.frame() %>% 
        mutate(MyCols = rownames(.)) %>% 
        filter(V1 > 1) %>% pull(MyCols)
    
    MyAES <- c("color" = as_label(quo(colorBy)), 
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
    
    # Some of the options inherited from ct_plot depend on there being just one
    # compound that the user is plotting. Using whatever is the compoundID that
    # has the base level for the factor. <--- This may not be necessary, now
    # that I think about it further...
    AnchorCompound <- sim_obs_dataframe %>% select(CompoundID) %>% unique() %>% 
        mutate(CompoundLevel = as.numeric(CompoundID)) %>% 
        filter(CompoundLevel == min(CompoundLevel)) %>% 
        pull(CompoundID) %>% as.character()
    
    # Setting up the x axis using the subfunction ct_x_axis
    ct_x_axis(Data = sim_obs_dataframe,
              time_range = time_range, 
              t0 = "simulation start",
              x_axis_interval = x_axis_interval,
              pad_x_axis = pad_x_axis,
              compoundToExtract = AnchorCompound, EnzPlot = FALSE)
    
    # Setting up the y axis using the subfunction ct_y_axis
    ct_y_axis(Data = sim_obs_dataframe, 
              ADAM = FALSE, 
              subsection_ADAM = "free compound in lumen", # This is just a placeholder since no ADAM data are currently allowed.
              EnzPlot = FALSE, 
              time_range_relative = time_range_relative,
              Ylim_data = sim_obs_dataframe %>% mutate(Time_orig = Time), 
              pad_y_axis = pad_y_axis,
              y_axis_limits_lin = y_axis_limits_lin, 
              time_range = time_range,
              y_axis_limits_log = y_axis_limits_log)
    
    
    # Linear graph --------------------------------------------------------
    A <- ggplot(sim_dataframe %>% filter(Trial == MyMeanType),
                aes(x = Time, y = Conc, color = !!colorBy, # Comment this for easier code development
                    # aes(x = Time, y = Conc, color = colorBy, # Uncomment this for easier code development
                    group = Group)) +
        geom_line() +
        # geom_point(data = obs_data) + # not ready to add obs data to this function yet!
        labs(color = as_label(colorBy)) +
        xlab(paste0("Time (", unique(sim_dataframe$Time_units), ")")) +
        ylab(paste0("Concentration (", unique(sim_dataframe$Conc_units), ")")) +
        scale_color_brewer(palette = "Set1") +
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
    
    if(floating_facet_scale){
        A <- A + 
            scale_x_continuous(expand = expansion(
                                   mult = c(pad_x_num, 0.04))) +
            scale_y_continuous(expand = expansion(mult = c(pad_y_num, 0.1))) +
            facet_wrap(vars(!!facet_column1, !!facet_column2), 
                       scales = "free")
        
    } else {
        A <- A +
            scale_x_continuous(breaks = XBreaks, labels = XLabels,
                               expand = expansion(
                                   mult = c(pad_x_num, 0.04))) +
            coord_cartesian(xlim = time_range_relative) +
            scale_y_continuous(limits = c(ifelse(is.na(y_axis_limits_lin[1]), 
                                                 0, y_axis_limits_lin[1]),
                                          YmaxRnd), 
                               breaks = YBreaks,
                               labels = YLabels,
                               expand = expansion(mult = c(pad_y_num, 0.1))) +
            facet_grid(rows = vars(!!facet_column1), # Comment this for easier code development
                       cols = vars(!!facet_column2)) # Comment this for easier code development
        # facet_grid(rows = vars(facet_column1), # Uncomment this for easier code development
        #            cols = vars(facet_column2), # Uncomment this for easier code development
    }
    
    ## Making semi-log graph ------------------------------------------------
    B <- suppressMessages(
        A + scale_y_log10(limits = Ylim_log, breaks = YLogBreaks,
                          labels = YLogLabels,
                          expand = expansion(mult = c(pad_y_num, 0.1))) +
            # labels = function(.) format(., scientific = FALSE, drop0trailing = TRUE)) +
            coord_cartesian(xlim = time_range_relative)
    )
    
    # both plots together, aligned vertically
    AB <- suppressWarnings(
        ggpubr::ggarrange(A, B, ncol = 1, labels = c("A", "B"),
                          align = "v")
    )
    
    Out <- switch(linear_or_log, 
                  "linear" = A,
                  "semi-log" = B,
                  "log" = B,
                  "both" = AB)
    
    if(complete.cases(save_graph)){
        FileName <- save_graph
        if(str_detect(FileName, "\\.")){
            # Making sure they've got a good extension
            Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
            FileName <- sub(paste0(".", Ext), "", FileName)
            Ext <- ifelse(Ext %in% c("eps", "ps", "jpeg", "tiff",
                                     "png", "bmp", "svg", "jpg"), 
                          Ext, "png")
            FileName <- paste0(FileName, ".", Ext)
        } else {
            FileName <- paste0(FileName, ".png")
        }
        
        if(linear_or_log == "both"){
            print(AB)
            ggsave(FileName, height = fig_height, width = fig_width, dpi = 600)
        }
        
        if(linear_or_log == "linear"){
            print(A)
            ggsave(FileName, height = fig_height, width = fig_width, dpi = 600)
        }
        
        if(str_detect(linear_or_log, "log")){
            print(B)
            ggsave(FileName, height = fig_height, width = fig_width, dpi = 600)
        }
    }
    
    return(Out)
    
}




