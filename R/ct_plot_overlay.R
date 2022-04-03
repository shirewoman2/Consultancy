#' Overlay multiple data sets onto a single concentration-time graph
#'
#' \code{ct_plot_overlay} is meant to be used in conjunction with
#' \code{\link{extractConcTime_mult}} to create single graphs with overlaid
#' concentration-time data of multiple tissues, compounds, or Simcyp Simulator
#' output files for easy comparisons. \emph{Note:} There are some nuances to
#' overlaying observed data. Please see the "Details" section at the bottom of
#' this help file. Also, this hasn't really been developed or tested with
#' enzyme-abundance data or ADAM data yet.
#'
#' \strong{Notes on including observed data:} We recently added the option of
#' including observed data and are in the process of testing this. To include
#' observed data, you have two options: \enumerate{
#'
#' \item{Use the Simulator Excel PE data entry template to save information
#' about your observed data. Then, when you run
#' \code{\link{extractConcTime_mult}}, supply the names of those Excel files to
#' the observed data  function argument. This is the BEST option because it
#' contains the most information about the observed data.}
#'
#' \item{Include observed data in your simulation files. Those data will be
#' automatically extracted when you run \code{\link{extractConcTime_mult}} if
#' "obs_data_files" is left as the default NA. } }
#'
#' The \code{ct_plot_overlay} function will automatically figure out which
#' observed data should be compared with which simulated compound IDs, tissues,
#' etc. However, because the function doesn't know which simulator \emph{file}
#' goes with which observed data, it will assume that \emph{all} the observed
#' data are appropriate to compare to \emph{all} the files included in
#' \code{sim_obs_dataframe} by default. If that's not the case, after you use
#' \code{\link{extractConcTime_mult}} to extract your data, you can indicate
#' which simulator output file goes with which observed file by setting the
#' simulator output file in the column "File". (Please ask a member of the R
#' Working Group for help if you're not clear on how to do this.) Be warned,
#' though, that if you assign "File" for some observed data but not all, only
#' the observed data with an assignment for "File" will show up on the graph.
#'
#' One other note: The observed data files don't include the \emph{name} of the
#' compound you're simulating (column: "Compound"). They do include whether it
#' was a substrate, metabolite, or inhibitor (column: CompoundID), but not the
#' compound's actual name. For that reason, try coloring or facetting your data
#' by CompoundID rather than by Compound. Similarly, if you have an inhibitor
#' and you have observed data, it will be listed as the generic "inhibitor" here
#' rather than, e.g., "ketoconazole" because the observed data can't tell which
#' is which.
#'
#'
#'
#' @param sim_obs_dataframe the data.frame with multiple sets of
#'   concentration-time data. At the moment, this function does not plot
#'   observed data; that's still under construction.
#' @param mean_type plot "arithmetic" (default) or "geometric" mean
#'   concentrations or "median" concentrations as the main (thickest or only)
#'   line for each data set. If this aggregate measure is not available in the
#'   simulator output, we'll warn you that we're plotting a different one.
#' @param figure_type the type of figure to plot. Default is "means only" to
#'   show only the mean, geometric mean, or median (whatever you chose for
#'   "mean_type"). Other option: "percentile ribbon" to show an opaque line for
#'   the mean data and transparent shading for the 5th to 95th percentiles.
#'   Note: You may sometimes see some artifacts -- especially for semi-log plots
#'   -- where the ribbon gets partly cut off. For arcane reasons we don't want
#'   to bore you with here, we can't easily prevent this. However, a possible
#'   fix is to set your y axis limits for the semi-log plot to be wider.
#' @param linear_or_log the type of graph to be returned. Options: "semi-log",
#'   "linear", "both vertical" (default, graphs are stacked vertically), or
#'   "both horizontal" (graphs are side by side).
#' @param colorBy What column in \code{sim_obs_dataframe} should be used for
#'   coloring the lines and/or points on the graph? This should be unquoted,
#'   e.g., \code{colorBy = Tissue}.
#' @param legend_labels_color Optionally specify a character vector for how
#'   you'd like the labels for whatever you choose for \code{colorBy} to show up
#'   in the legend. For example, if you want to color by the column "File" and
#'   you know that, e.g. "file 1.xlsx" is for when you were simulating an fa of
#'   0.5 and "file 2.xlsx" was for an fa of 0.2, you could specify that like
#'   this: c("file 1.xlsx" = "fa = 0.5", "file 2.xlsx" = "fa = 0.2") The order
#'   in the legend will match the order here. 
#' @param facet_column1 If you would like to break up your graph into small
#'   multiples, you can break the graphs up by up to two columns in
#'   \code{sim_obs_dataframe}. What should be the 1st column to break up the
#'   data by? This should be unquoted. If \code{floating_facet_scale} is FALSE,
#'   then \code{facet_column1} will make the rows of the output graphs.
#' @param facet_column2 What should be the 2nd column to break up the data into
#'   small multiples by? This should be unquoted. If \code{floating_facet_scale}
#'   is FALSE, then \code{facet_column2} will make the columns of the output
#'   graphs.
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
#' @param pad_x_axis Optionally add a smidge of padding to the the x axis
#'   (default is TRUE, which includes some generally reasonable padding). If
#'   changed to FALSE, the y axis will be placed right at the beginning of your
#'   time range and all data will end \emph{exactly} at the end of the time
#'   range specified. If you want a \emph{specific} amount of x-axis padding,
#'   set this to a number; the default is \code{c(0.02, 0.04)}, which adds 2\%
#'   more space to the left side and 4\% more to the right side of the x axis.
#'   If you only specify one number, we'll assume that's the percent you want
#'   added to the left side.
#' @param pad_y_axis Optionally add a smidge of padding to the y axis (default
#'   is TRUE, which includes some generally reasonable padding). As with
#'   \code{pad_x_axis}, if changed to FALSE, the x axis will be placed right at
#'   the bottom of your data, possible cutting a point in half. If you want a
#'   \emph{specific} amount of y-axis padding, set this to a number; the default
#'   is \code{c(0.02, 0)}, which adds 2\% more space to the bottom and nothing
#'   to the top of the y axis. If you only specify one number, we'll assume
#'   that's the percent you want added to the bottom.
#' @param x_axis_interval Set the x-axis major tick-mark interval. Acceptable
#'   input: any number or leave as NA to accept default values.
#' @param color_set the set of colors to use. Options: \describe{
#'
#'   \item{"default"}{colors selected from the color brewer palette "set 1"}
#'
#'   \item{"ggplot2 default"}{the default set of colors used in ggplot2 graphs
#'   (ggplot2 is an R package for graphing.)}
#'
#'   \item{"rainbow"}{colors selected from a rainbow palette. The default
#'   palette is limited to something like 6 colors, so if you have more than
#'   that, that's when this palette is most useful. It's \emph{not} very useful
#'   when you only need a couple of colors.}
#'
#'   \item{"blue-green"}{a set of blues and greens}
#'
#'   \item{"Brewer set 2"}{a set of colors from Cynthia Brewer et al. from Penn
#'   State that are friendly to those with red-green colorblindness}
#'
#'   \item{"Tableau"}{uses the standard Tableau palette; requires the "ggthemes"
#'   package}}
#' @param obs_transparency Optionally make the observed data points
#'   semi-transparent, which can be helpful when there are numerous
#'   observations. Acceptable values are 0 (completely transparent) to 1
#'   (completely opaque).
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png". If you leave off ".png", it
#'   will be saved as a png file, but if you specify a different file extension,
#'   it will be saved as that file format. Acceptable extensions are "eps",
#'   "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg". Leaving this as NA
#'   means the file will not be automatically saved to disk.
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
#' @param legend_position Specify where you want the legend to be. Options are
#'   "left", "right" (default in most scenarios), "bottom", "top", or "none" if
#'   you don't want one at all.
#' @param facet_spacing Optionally set the spacing between facets. If left as
#'   NA, a reasonable amount of space will be used. Units are "lines", so try,
#'   e.g. 2.
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
                            mean_type = "arithmetic",
                            figure_type = "means only", 
                            linear_or_log = "semi-log",
                            colorBy,
                            legend_labels_color = NA, 
                            color_set = "default",
                            obs_transparency = NA, 
                            facet_column1,
                            facet_column2, 
                            floating_facet_scale = FALSE,
                            facet_spacing = NA,
                            time_range = NA, 
                            x_axis_interval = NA,
                            pad_x_axis = TRUE,
                            pad_y_axis = TRUE,
                            y_axis_limits_lin = NA,
                            y_axis_limits_log = NA, 
                            legend_position = NA,
                            save_graph = NA,
                            fig_height = 6,
                            fig_width = 5, 
                            include_messages = TRUE){
    
    # Setting things up for some nonstandard evaluation -------------------------
    
    facet_column1 <- rlang::enquo(facet_column1)
    facet_column2 <- rlang::enquo(facet_column2)
    colorBy <- rlang::enquo(colorBy)
    
    # I *would* be doing this whole function with nonstandard evaluation except
    # that I CANNOT figure out how to use NSE to redefine a user-supplied
    # column, so I'm going to have to rename all of them. This makes the rest of
    # checking and developing this function easier, too, though. 
    
    # sim_obs_dataframe <- sim_obs_dataframe %>%
    #     mutate(COLORBY = ifelse(as_label(colorBy) == "<empty>", NA, {{colorBy}}),
    #            FC1 = ifelse(as_label(facet_column1) == "<empty>", NA, {{facet_column1}}),
    #            FC2 = ifelse(as_label(facet_column2) == "<empty>", NA, {{facet_column2}}))
    
    ### NOT THE ABOVE. This causes everything to be the same value. Below code works. 
    
    if(as_label(colorBy) != "<empty>"){
        sim_obs_dataframe <- sim_obs_dataframe %>%
            mutate(COLORBY = {{colorBy}})
    }
    
    if(as_label(facet_column1) != "<empty>"){
        sim_obs_dataframe <- sim_obs_dataframe %>%
            mutate(FC1 = {{facet_column1}})
    }
    
    if(as_label(facet_column2) != "<empty>"){
        sim_obs_dataframe <- sim_obs_dataframe %>%
            mutate(FC2 = {{facet_column2}})
    }
    
    # error catching -------------------------------------------------------
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
    
    MyMeanType <- sim_obs_dataframe %>%
        filter(Trial %in% c("geomean", "mean", "median")) %>% 
        pull(Trial) %>% unique() %>% 
        factor(levels = c("mean", "geomean", "median")) %>% 
        sort()
    
    if(switch(mean_type, "arithmetic" = "mean", "geometric" = "geomean",
              "median" = "median") %in% sim_obs_dataframe$Trial == FALSE){
        
        warning(paste0("You requested the ", 
                       switch(mean_type, "arithmetic" = "arithmetic means",
                              "geometric" = "geometric means", 
                              "median" = "medians"), 
                       ", but those are not included in your data. Instead, the ",
                       ifelse(MyMeanType[1] == "mean", 
                              "arithmetic mean", MyMeanType[1]),
                       "s will be used."))
        MyMeanType <- MyMeanType[1] %>% as.character()
        
    } else {
        
        MyMeanType <- switch(mean_type, "arithmetic" = "mean",
                             "geometric" = "geomean",
                             "median" = "median")
        
    }
    
    sim_obs_dataframe <- sim_obs_dataframe %>% 
        # At least at this point, I can't see this function working well with
        # ADAM model data b/c the y axis units differ. Removing all ADAM model
        # data.
        filter(is.na(subsection_ADAM)) %>%
        # If it's dose number 0, remove those rows so that we'll show only the
        # parts we want when facetting and user wants scales to float freely.
        filter(DoseNum != 0 | Simulated == FALSE) %>% 
        mutate(Group = paste(File, Trial, Tissue, CompoundID, Compound, Inhibitor),
               CompoundID = factor(CompoundID,
                                   levels = c("substrate", "primary metabolite 1",
                                              "primary metabolite 2", "secondary metabolite",
                                              "inhibitor 1", "inhibitor 1 metabolite", 
                                              "inhibitor 2"))) 
    
    sim_dataframe <- sim_obs_dataframe %>%
        filter(Simulated == TRUE &
                   Trial %in% 
                   switch(figure_type, 
                          "means only" = MyMeanType, 
                          "percentile ribbon" = c(MyMeanType, "per5", "per95"),
                          "ribbon" = c(MyMeanType, "per5", "per95")))
    
    obs_data <- sim_obs_dataframe %>% filter(Simulated == FALSE) %>% 
        mutate(Trial = {MyMeanType})
    
    # Making obs data black if File was originally NA for all
    if(nrow(obs_data) > 0 && "File" == as_label(colorBy) & all(is.na(obs_data$File))){
        InternalAssignFile <- TRUE
    } else {
        InternalAssignFile <- FALSE
    }
    
    # Setting this up so that observed data will be shown for all Files
    if(nrow(obs_data) > 0 && "File" %in% c(as_label(colorBy), as_label(facet_column1), 
                     as_label(facet_column2)) &&
       all(is.na(obs_data$File))){
        
        ToAdd <- expand_grid(ObsFile = unique(obs_data$ObsFile), 
                             File = unique(sim_dataframe$File))
        obs_data <- obs_data %>% select(-File) %>% 
            left_join(ToAdd)
    } 
    
    # Dealing with the fact that the observed data will list the inhibitor as
    # "inhibitor" unless the user changes it, but the sim data will list its name
    if(nrow(obs_data) > 0 & any(obs_data$Inhibitor == "inhibitor")){
        sim_obs_dataframe <- sim_obs_dataframe %>% 
            mutate(Inhibitor = ifelse(Inhibitor == "none", Inhibitor, "inhibitor"))
    }
    
    # Now that all columns in both sim and obs data are filled in whenever they
    # need to be, setting factors for legend_labels_color
    if(complete.cases(legend_labels_color[1])){
        simcheck <- sim_dataframe %>% select(COLORBY) %>% unique() %>% pull()
        obscheck <- obs_data %>% select(COLORBY) %>% unique() %>% pull()
        if(length(unique(c(simcheck, obscheck))) > length(legend_labels_color)){
            warning(paste0("You have not included enough labels for the colors in the legend. The values in the column '",
                           as_label(colorBy), 
                           "' will be used as labels instead."))
            legend_labels_color <- NA
        } else {
            
            sim_dataframe <- sim_dataframe %>% 
                mutate(COLORBY = legend_labels_color[COLORBY], 
                       COLORBY = factor(COLORBY, levels = legend_labels_color))
            
            obs_data <- obs_data %>% 
                mutate(COLORBY = legend_labels_color[COLORBY], 
                       COLORBY = factor(COLORBY, levels = legend_labels_color))
        }
    }
    
    MyUniqueData <- sim_obs_dataframe %>% 
        filter(Trial == MyMeanType) %>% 
        select(File, Tissue, CompoundID, Compound, Inhibitor) %>% unique()
    
    if(include_messages){
        print("This graph contains the following unique combinations of data (make sure they are what you were expecting):")
        print(MyUniqueData)
    }
    
    UniqueGroups <- sim_obs_dataframe %>% 
        summarize(across(.cols = c(File, Tissue, CompoundID, Compound,
                                   Inhibitor), 
                         .fns = function(x) length(unique(x)))) %>% 
        t() %>% as.data.frame() %>% 
        mutate(MyCols = rownames(.)) %>% 
        filter(V1 > 1) %>% pull(MyCols)
    
    MyAES <- c("color" = as_label(colorBy), 
               "facet1" = as_label(facet_column1), 
               "facet2" = as_label(facet_column2))
    UniqueAES <- MyAES[which(MyAES != "<empty>")]
    
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
    
    
    # Setting figure types ---------------------------------------------------
    ## Figure type: means only ---------------------------------------------
    if(figure_type == "means only"){
        A <- ggplot(sim_dataframe,
                    aes(x = Time, y = Conc, color = COLORBY, 
                        group = Group)) +
            geom_line()
        
        if(nrow(obs_data) > 0){
            if(InternalAssignFile){
                A <- A +
                    geom_point(data = obs_data, inherit.aes = FALSE,
                               aes(x = Time, y = Conc, group = Group), 
                               alpha = ifelse(complete.cases(obs_transparency), 
                                              obs_transparency, 1), 
                               show.legend = FALSE)
            } else {
                A <- A +
                    geom_point(data = obs_data, inherit.aes = FALSE,
                               aes(x = Time, y = Conc, group = Group,
                                   color = COLORBY), 
                               alpha = ifelse(complete.cases(obs_transparency), 
                                              obs_transparency, 1), 
                               show.legend = FALSE)
            }
        }
    }
    
    
    ## Figure type: ribbon --------------------------------------------------
    if(str_detect(figure_type, "ribbon")){
        
        RibbonDF <-  sim_dataframe %>% 
            filter(Trial %in% c({MyMeanType}, "per5", "per95")) %>% 
            select(-any_of(c("Group", "Individual"))) %>% 
            pivot_wider(names_from = Trial, values_from = Conc)
        names(RibbonDF)[names(RibbonDF) == MyMeanType] <- "MyMean"
        
        A <- ggplot(RibbonDF, aes(x = Time, y = MyMean, ymin = per5, ymax = per95, 
                                  color = COLORBY, fill = COLORBY)) + 
            geom_ribbon(alpha = 0.25, color = NA) +
            geom_line()
        
        if(nrow(obs_data) > 0){
            if(InternalAssignFile){
                A <- A + 
                    geom_point(data = obs_data, 
                               aes(x = Time, y = Conc, group = Group),
                               inherit.aes = FALSE, 
                               alpha = ifelse(complete.cases(obs_transparency), 
                                              obs_transparency, 1), 
                               show.legend = FALSE) 
                
            } else {
                A <- A + 
                    geom_point(data = obs_data, 
                               aes(x = Time, y = Conc, group = Group,
                                   color = COLORBY), 
                               inherit.aes = FALSE, 
                               alpha = ifelse(complete.cases(obs_transparency), 
                                              obs_transparency, 1), 
                               show.legend = FALSE) 
            }
        }
    }
    
    # Making linear graph --------------------------------------------------------
    A <-  A +
        xlab(paste0("Time (", unique(sim_dataframe$Time_units), ")")) +
        ylab(paste0("Concentration (", unique(sim_dataframe$Conc_units), ")")) +
        theme(panel.background = element_rect(fill = "white", color = NA),
              panel.border = element_rect(color = "black", fill = NA),
              strip.background = element_rect(fill = "white"),
              legend.key = element_rect(fill = "white"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(color = "black"),
              axis.title = element_text(color = "black", face = "bold"),
              axis.line.y = element_line(color = "black"),
              axis.line.x.bottom = element_line(color = "black"))
    
    if(complete.cases(legend_labels_color[1])){
        A <- A + labs(color = NULL, fill = NULL)
    } else {
        A <- A + labs(color = as_label(colorBy), fill = as_label(colorBy))
    }
    
    if(floating_facet_scale){
        A <- A + 
            scale_x_continuous(expand = expansion(
                mult = pad_x_num)) +
            scale_y_continuous(expand = expansion(mult = pad_y_num)) +
            facet_wrap(vars(!!facet_column1, !!facet_column2), 
                       scales = "free")
        
    } else {
        A <- A +
            scale_x_continuous(breaks = XBreaks, labels = XLabels,
                               expand = expansion(
                                   mult = pad_x_num)) +
            coord_cartesian(xlim = time_range_relative) +
            scale_y_continuous(limits = c(ifelse(is.na(y_axis_limits_lin[1]), 
                                                 0, y_axis_limits_lin[1]),
                                          YmaxRnd), 
                               breaks = YBreaks,
                               labels = YLabels,
                               expand = expansion(mult = pad_y_num)) +
            facet_grid(rows = vars(!!facet_column1), cols = vars(!!facet_column2)) 
    }
    
    # Colors ----------------------------------------------------------------
    
    # Adding options for colors
    colRainbow <- colorRampPalette(c("gray20", "antiquewhite4", "firebrick3",
                                     "darkorange", "green3", "seagreen3",
                                     "cadetblue", "dodgerblue3", "royalblue4",
                                     "darkorchid4"))
    
    blueGreen <- colorRampPalette(c("green3", "seagreen3", "cadetblue", 
                                    "dodgerblue3", "royalblue4"))
    
    NumColorsNeeded <- sim_dataframe %>% pull(MyAES["color"]) %>% 
        unique() %>% length()
    
    # print(NumColorsNeeded)
    
    if(color_set == "default"){
        A <- A + scale_color_brewer(palette = "Set1") +
            scale_fill_brewer(palette="Set1")
    }
    
    if(color_set == "blue-green"){
        A <- A + scale_color_manual(values = blueGreen(NumColorsNeeded)) +
            scale_fill_manual(values = blueGreen(NumColorsNeeded))
    }
    
    if(color_set == "rainbow"){
        A <- A + scale_color_manual(values = colRainbow(NumColorsNeeded)) +
            scale_fill_manual(values = colRainbow(NumColorsNeeded))
    }
    
    if(color_set == "Brewer set 2"){
        A <- A + scale_fill_brewer(palette = "Set2") +
            scale_color_brewer(palette = "Set2")
    }
    
    if(color_set == "Tableau"){
        A <- A + ggthemes::scale_color_tableau() +
            ggthemes::scale_fill_tableau()
    }
    
    ## Adding spacing between facets if requested
    if(complete.cases(facet_spacing)){
        A <- A + theme(panel.spacing = unit(facet_spacing, "lines"))
    }
    
    
    ## Making semi-log graph ------------------------------------------------
    
    B <- suppressMessages(
        A + scale_y_log10(limits = Ylim_log, breaks = YLogBreaks,
                          labels = YLogLabels,
                          expand = expansion(mult = pad_y_num)) 
    )
    
    if(floating_facet_scale == FALSE){
        B <- suppressMessages(B + coord_cartesian(xlim = time_range_relative))
    }
    
    if(complete.cases(legend_position)){
        A <- A + theme(legend.position = legend_position)  
        B <- B + theme(legend.position = legend_position)
    }    
    
    # both plots together, aligned vertically
    AB <- suppressWarnings(
        ggpubr::ggarrange(A, B, ncol = 1, labels = c("A", "B"),
                          common.legend = TRUE, align = "hv", 
                          legend = ifelse(is.na(legend_position), 
                                          "right", legend_position))
    )
    
    # both plots together, aligned horizontally
    ABhoriz <- suppressWarnings(
        ggpubr::ggarrange(A, B, ncol = 2, labels = c("A", "B"),
                          common.legend = TRUE, align = "hv", 
                          legend = ifelse(is.na(legend_position), 
                                          "bottom", legend_position))
    )
    
    Out <- switch(linear_or_log, 
                  "linear" = A,
                  "semi-log" = B,
                  "log" = B,
                  "both" = AB, 
                  "both vertical" = AB,
                  "both horizontal" = ABhoriz)
    
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
        
        if(linear_or_log %in% c("both", "both vertical")){
            ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
                   plot = AB)
        }
        
        if(linear_or_log %in% c("both horizontal")){
            ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
                   plot = ABhoriz)
        }
        
        if(linear_or_log == "linear"){
            ggsave(FileName, height = fig_height, width = fig_width, dpi = 600, 
                   plot = A)
        }
        
        if(str_detect(linear_or_log, "log")){
            ggsave(FileName, height = fig_height, width = fig_width, dpi = 600, 
                   plot = B)
        }
    }
    
    return(Out)
    
}




