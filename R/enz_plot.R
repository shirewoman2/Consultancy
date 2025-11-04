#' Plots of enzyme abundance changes over time to match Consultancy template
#'
#' Using simulated enzyme-abundance data generated from running the function
#' \code{\link{extractEnzAbund}}, make publication-quality graphs that match the
#' consultancy template formatting instructions. For detailed instructions and
#' examples, please see the SharePoint file "Simcyp PBPKConsult R Files - Simcyp
#' PBPKConsult R Files/SimcypConsultancy function examples and
#' instructions/Enzyme abundance plots/Enzyme-abundance-plot-examples.docx".
#' (Sorry, we are unable to include a link to it here.)
#'
#' @param sim_enz_dataframe the data.frame of enzyme abundance data obtained
#'   from running the function \code{\link{extractEnzAbund}}. Not quoted.
#' @param figure_type type of figure to plot. Options are:
#'
#'   \describe{
#'
#'   \item{"means only"}{(default) plots a black line for the mean data and, if
#'   a perpetrator was modeled, a dashed line for the concentration-time data with
#'   Inhibitor 1.}
#'
#'   \item{"trial means"}{plots an opaque line for the mean data, lighter lines
#'   for the mean of each trial of simulated data, and open circles for the
#'   observed data. If a perpetrator were present, lighter dashed lines indicate
#'   the mean of each trial of simulated data in the presence of the perpetrator.}
#'
#'   \item{"percentiles"}{plots an opaque line for the mean data, lighter lines
#'   for the 5th and 95th percentiles of the simulated data, and open circles
#'   for the observed data. If an effecter were present, the default is dashed
#'   lines for the data in the presence of a perpetrator.}
#'
#'   \item{"percentile ribbon"}{plots an opaque line for the mean data,
#'   transparent shading for the 5th to 95th percentiles of the simulated data,
#'   and open circles for the observed data. If a perpetrator were present, the
#'   default is to show the data without the perpetrator in blue and the data in
#'   the presence of the perpetrator in red. Note: You may sometimes see some
#'   artifacts -- especially for semi-log plots -- where the ribbon gets partly
#'   cut off. For arcane reasons we don't want to bore you with here, we can't
#'   easily prevent this. However, a possible fix is to set your y axis limits
#'   for the semi-log plot to be wider using \code{y_axis_limits_log}.}
#'
#'   \item{"Freddy"}{Freddy's favorite style of plot with trial means in light
#'   gray, the overall mean in thicker black, the 5th and 95th percentiles in
#'   dashed lines, and the observed data in semi-transparent purple-blue. Graphs
#'   with a perpetrator present lose the trial means, and the percentiles switch
#'   to solid, gray lines. \strong{An editorial comment:} While this does not
#'   align with the officially sanctioned template at this time, this looks
#'   \emph{sharp}, makes it easy to see the defining characteristics of the
#'   data, and I recommend checking it out, even just for your own purposes of
#'   examining your data. If the color is too much for you but you like the
#'   rest, try setting \code{obs_color = "none"}. -LSh}}
#'
#' @param gut_tissue Which of the two types of gut tissue would you like to
#'   plot? Acceptable options are "colon" (default) or "small intestine".
#'   Applicable only when the tissue extracted with
#'   \code{\link{extractEnzAbund}} included gut tissue and ignored in all other
#'   cases.
#'
#' @param mean_type graph "arithmetic" (default) or "geometric" means or
#'   "median" for median concentrations. If that option was not included in the
#'   output, you'll get a warning and the graph will include one that was.
#'
#' @param linear_or_log the type of graph to be returned. Options: \describe{
#'   \item{"semi-log"}{y axis is log transformed}
#'
#'   \item{"linear"}{no axis transformation}
#'
#'   \item{"both vertical"}{(default) both the linear and the semi-log graphs
#'   will be returned, and graphs are stacked vertically}
#'
#'   \item{"both horizontal"}{both the linear and the semi-log graphs will be
#'   returned, and graphs are side by side horizontally}
#'
#'   \item{"horizontal and vertical"}{both the linear and the semi-log graphs
#'   will be returned, and graphs are side by side horizontally (one graph; file
#'   name will end in "- horizontal") and stacked vertically (second graph; file
#'   name will end in "- vertical"). This option, which was designed to create
#'   the vertically stacked version of a graph for a report and the horizontal,
#'   side-by-side version for a presentation, is a bit different from the others
#'   since it will return two separate files. In the RStudio "Plots" window,
#'   you'll only see the vertically stacked version. Setting \code{fig_height}
#'   and \code{fig_width} will adjust only the dimensions of the horizontal
#'   figure; the default values will be used for the vertical one. If you
#'   request Word output, only the vertical plot will be saved in Word format;
#'   the horizontal plot will be saved as a png file.}}
#' @param time_range time range to show relative to the start of the simulation.
#'   Options: \describe{
#'
#'   \item{NA}{(default) entire time range of data}
#'
#'   \item{a start time and end time in hours}{only data in that time range,
#'   e.g. \code{c(24, 48)}. Note that there are no quotes around numeric data.}
#'
#'   \item{specific doses}{If you would like to plot a specific dose number for
#'   "time_range" similar to how you would for \code{\link{ct_plot}}, you must
#'   also specify which compound's dose number you want, e.g., \code{time_range
#'   = "dose 1 substrate"} or \code{time_range = "last dose inhibitor 1"} or
#'   even \code{time_range = "doses 4 to 6 substrate"}.}}
#'
#' @param x_axis_interval optionally set the x-axis major tick-mark interval.
#'   Acceptable input: any number or leave as NA to accept default values, which
#'   are generally reasonable guesses as to aesthetically pleasing and
#'   PK-relevant intervals.
#' @param x_axis_label optionally supply a character vector or an expression to
#'   use for the x axis label
#' @param pad_x_axis optionally add a smidge of padding to the the x axis
#'   (default is TRUE, which includes some generally reasonable padding). If
#'   changed to FALSE, the y axis will be placed right at the beginning of your
#'   time range and all data will end \emph{exactly} at the end of the time
#'   range specified. If you want a \emph{specific} amount of x-axis padding,
#'   set this to a number; the default is \code{c(0.02, 0.04)}, which adds 2\%
#'   more space to the left side and 4\% more to the right side of the x axis.
#'   If you only specify one number, we'll assume that's the percent you want
#'   added to the left side.
#'
#' @param pad_y_axis optionally add a smidge of padding to the y axis (default
#'   is TRUE, which includes some generally reasonable padding). As with
#'   \code{pad_x_axis}, if changed to FALSE, the x axis will be placed right at
#'   the bottom of your data, possibly cutting a point in half. If you want a
#'   \emph{specific} amount of y-axis padding, set this to a number; the default
#'   is \code{c(0.02, 0)}, which adds 2\% more space to the bottom and nothing
#'   to the top of the y axis. If you only specify one number, we'll assume
#'   that's the percent you want added to the bottom.
#' @param y_axis_limits_lin optionally set the Y axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as the default NA, the Y axis
#'   limits for the linear plot will be automatically selected.
#' @param y_axis_limits_log optionally set the Y axis limits for the semi-log
#'   plot, e.g., \code{c(10, 1000)}. Values will be rounded down and up,
#'   respectively, to a round number. If left as the default NA, the Y axis
#'   limits for the semi-log plot will be automatically selected.
#' @param y_axis_label optionally supply a character vector or an expression to
#'   use for the y axis label
#' @param line_transparency optionally specify the transparency for the trial
#'   mean or percentile lines. Acceptable values are from 0 (fully transparent,
#'   so no line at all) to 1 (completely opaque or black). If left as the
#'   default NA, this value will be automatically determined.
#' @param line_type Optionally specify what types of lines are used to depict
#'   \enumerate{\item{the substrate drug alone and} \item{the substrate drug in
#'   the presence of a perpetrator (when applicable).}} Input should look like
#'   this, for example: \code{c("solid", "dashed")} to get a solid line for the
#'   substrate drug and a dashed line for inhibitor 1. \itemize{ \item{To see
#'   all possible \code{line_type} options: \code{ggpubr::show_line_types()}}
#'   \item{If left as NA, substrate alone will be a solid line and substrate +
#'   inhibitor 1 will be a dashed line.} \item{If \code{figure_type} is "Freddy"
#'   and there's no perpetrator present, which is a slightly different scenario
#'   than the other graph types, the 1st line type specified will be for the
#'   mean simulated concentration and the trial means, and the 2nd line type
#'   specified will be for the 5th and 95th percentiles.}}
#' @param line_color optionally specify what colors to use for the lines.
#'   Acceptable input for, e.g., the substrate alone to be blue and the
#'   substrate + Inhibitor 1 to be red: \code{c("blue", "red")}. If left as the
#'   default NA, lines will be black or gray. Hex color codes are also
#'   acceptable to use.
#' @param line_width optionally specify how thick to make the lines. Acceptable
#'   input is a number; the default is 1 for most lines and 0.8 for some, to
#'   give you an idea of where to start.
#' @param hline_position numerical position(s) of any horizontal lines to add to
#'   the graph. The default is NA to have no lines, and good syntax if you
#'   \emph{do} want lines would be, for example, \code{hline_position = 100} to
#'   have a horizontal line at 100 percent of the baseline enzyme abundance or
#'   \code{hline_position = c(50, 100, 200)} to have horizontal lines at each of
#'   those y values.
#' @param hline_style the line color and type to use for any horizontal lines
#'   that you add to the graph with \code{hline_position}. Default is "red
#'   dotted", but any combination of 1) a color in R and 2) a named linetype is
#'   acceptable. Examples: "red dotted", "blue dashed", or "#FFBE33 longdash".
#'   To see all the possible linetypes, type \code{ggpubr::show_line_types()}
#'   into the console.
#' @param vline_position numerical position(s) of any vertical lines to add to
#'   the graph. The default is NA to have no lines, and good syntax if you
#'   \emph{do} want lines would be, for example, \code{vline_position = 12} to
#'   have a vertical line at 12 h or \code{vline_position = seq(from = 0, to =
#'   168, by = 24)} to have horizontal lines every 24 hours for one week.
#'   Examples of where this might be useful would be indicating dosing times or
#'   the time at which some other drug was started or stopped.
#' @param vline_style the line color and type to use for any vertical lines that
#'   you add to the graph with \code{vline_position}. Default is "red dotted",
#'   but any combination of 1) a color in R and 2) a named linetype is
#'   acceptable. Examples: "red dotted", "blue dashed", or "#FFBE33 longdash".
#'   To see all the possible linetypes, type \code{ggpubr::show_line_types()}
#'   into the console.
#' @param graph_labels TRUE or FALSE for whether to include labels (A, B, C,
#'   etc.) for each of the small graphs. (Not applicable if only outputting
#'   linear or only semi-log graphs.)
#' @param graph_title optionally specify a title that will be centered across
#'   your graph or set of graphs
#' @param graph_title_size the font size for the graph title if it's included;
#'   default is 14
#' @param legend_label optionally indicate on the legend whether the perpetrator
#'   is an inhibitor, inducer, activator, or suppressor. Input will be used as
#'   the label in the legend for the line style and the shape. If left as the
#'   default NA when a legend is included and a perpetrator is present, the
#'   label in the legend will be "Inhibitor".
#' @param prettify_compound_names TRUE (default), FALSE or a character vector:
#'   This is asking whether to make compound names prettier in legend entries
#'   and in any Word output files. This was designed for simulations where the
#'   substrate and any metabolites, perpetrators, or perpetrator metabolites are
#'   among the standard options for the simulator, and leaving
#'   \code{prettify_compound_names = TRUE} will make the name of those compounds
#'   something more human readable. For example, "SV-Rifampicin-MD" will become
#'   "rifampicin", and "Sim-Midazolam" will become "midazolam". Setting this to
#'   FALSE will leave the compound names as is. For an approach with more
#'   control over what the compound names will look like in legends and Word
#'   output, set each compound to the exact name you  want with a named
#'   character vector where the name is "perpetrator" and the value is the name
#'   you want, e.g., \code{prettify_compound_names = c("perpetrator" =
#'   "teeswiftavir")}. Please note that "perpetrator" includes \emph{all} the
#'   perpetrators and perpetrator metabolites present, so, if you're setting the
#'   perpetrator name, you really should use something like
#'   this if you're including perpetrator metabolites: \code{prettify_compound_names =
#'   c("perpetrator" = "teeswiftavir and 1-OH-teeswiftavir")}.
#' @param legend_position specify where you want the legend to be. Options are
#'   "left", "right", "bottom", "top", or "none" (default) if you don't want one
#'   at all.
#' @param qc_graph TRUE or FALSE (default) on whether to create a second copy of
#'   the graph where the left panel shows the original graph and the right panel
#'   shows information about the simulation trial design. This works MUCH faster
#'   when you have already used \code{\link{extractExpDetails_mult}} to get
#'   information about how your simulation or simulations were set up and supply
#'   that object to the argument \code{existing_exp_details}.
#' @param existing_exp_details output from \code{\link{extractExpDetails}} or
#'   \code{\link{extractExpDetails_mult}} to be used for creating figure
#'   headings and captions tailored to the specific simulation when saving to a
#'   Word file or for use with \code{qc_graph}
#' @param return_caption TRUE or FALSE (default) for whether to return any
#'   caption text to use with the graph. This works best if you supply something
#'   for the argument \code{existing_exp_details}. If set to TRUE, you'll get as
#'   output a list of the graph, the figure heading, and the figure caption.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png" or "My conc time
#'   graph.docx". If you leave off ".png" or ".docx" from the file name, it will
#'   be saved as a png file, but if you specify a different graphical file
#'   extension, it will be saved as that file format. Acceptable graphical file
#'   extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg".
#'   Do not include any slashes, dollar signs, or periods in the file name.
#'   Leaving this as NA means the file will not be saved to disk.
#' @param fig_height figure height in inches; default is 6
#' @param fig_width figure width in inches; default is 5
#'
#' @return Output is a ggplot2 graph or two ggplot2 graphs arranged with
#'   ggpubr::ggarrange()
#' @export
#' @examples
#' enz_plot(sim_enz_dataframe = CYP3A4_liver)
#' enz_plot(sim_enz_dataframe = CYP3A4_gut, legend_position = "right")
#' enz_plot(sim_enz_dataframe = CYP3A4_gut,
#'          gut_tissue = "small intestine",
#'          figure_type = "percentile ribbon",
#'          legend_position = "right")


enz_plot <- function(sim_enz_dataframe,
                     gut_tissue = "colon",
                     figure_type = "means only",
                     mean_type = "arithmetic",
                     linear_or_log = "linear",
                     time_range = NA,
                     x_axis_interval = NA,
                     x_axis_label = NA,
                     pad_x_axis = TRUE,
                     pad_y_axis = TRUE,
                     y_axis_limits_lin = NA,
                     y_axis_limits_log = NA,
                     y_axis_label = NA,
                     line_type = NA,
                     line_transparency = NA,
                     line_color = NA,
                     line_width = NA,
                     hline_position = NA, 
                     hline_style = "red dotted", 
                     vline_position = NA, 
                     vline_style = "red dotted",
                     legend_position = "none", 
                     legend_label = NA,
                     prettify_compound_names = TRUE,
                     graph_labels = TRUE,
                     graph_title = NA,
                     graph_title_size = 14, 
                     qc_graph = FALSE,
                     existing_exp_details = NA,
                     return_caption = FALSE, 
                     save_graph = NA,
                     fig_height = 4, 
                     fig_width = 6){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
      
   # Observed data are not included for enzyme-abundance plots, so remove that
   # regular option for time_range.
   if(any(complete.cases(time_range)) && any(str_detect(time_range, "all obs"))){
      warning("Since there are no observed data for enzyme abundance, the option for time_range of `all observed` does not work for the function enz_plot. The full time range will be used.", 
              call. = FALSE)
      time_range = NA
   }
   
   # If the supplied sim_enz_dataframe includes both colon and small intestine
   # data but the user did not explicitly call on the argument "gut_tissue",
   # give a warning that the colon tissue will be plotted.
   if(hasArg("gut_tissue") == FALSE & length(unique(sim_enz_dataframe$Tissue)) > 1){
      warning("The supplied data include enzyme abundance levels for both colon and small intestine. By default, the abundance levels in colon will be shown. Set the argument `gut_tissue` to `small intestine` if that's not what you would like.", 
              call. = FALSE)
   }
   
   if("character" %in% class(prettify_compound_names) &&
      is.null(names(prettify_compound_names)) == FALSE){
      # Hacking this to work w/ct_plot and make_ct_caption functions.
      names(prettify_compound_names)[which(names(prettify_compound_names) == "enzyme")] <- 
         "substrate"
   }
   
   # main body of function -----------------------------------------------
   
   Data <- sim_enz_dataframe %>% 
      mutate(Simulated = TRUE)
   
   if(all(c("colon", "small intestine") %in% unique(Data$Tissue))){
      if(hasArg("gut_tissue")){
         Data <- Data %>% filter(Tissue == gut_tissue)
      } else {
         warning("The supplied data include both colon and small intestine enzyme levels, but you have not specified which tissue you would like. Since the enz_plot function can only plot one tissue at a time, we'll use the default value of `colon`.\n", 
                 call. = FALSE)
         Data <- Data %>% filter(Tissue == "colon")
      }
   }
   
   # Tell the user what they're plotting.
   message(paste0("Graphing enzyme abundance levels for ", 
                  unique(Data$Enzyme), " in ",
                  unique(Data$Tissue), "."))
   
   if("logical" %in% class(prettify_compound_names) &&
      prettify_compound_names){
      
      # This defaults to "none" if nothing was supplied for
      # existing_exp_details, so, if that's the case, hacking this afterwards to
      # check for whatever inhibitor was present in the data.
      MyPerpetrator <- determine_myperpetrator(existing_exp_details, 
                                               prettify_compound_names = TRUE)
      
      if("Inhibitor" %in% names(Data) &&
         MyPerpetrator == "none" & any(Data$Inhibitor != "none")){
         
         AllPerpsInCT <- unique(as.character(Data$Inhibitor[
            Data$Inhibitor != "none"]))
         MyPerpetrator <- ifelse(length(AllPerpsInCT) > 1, 
                                 str_comma(sort(AllPerpsInCT), conjunction = "or"), 
                                 AllPerpsInCT)
      }
      
      prettify_compound_names <- c("substrate" = unique(Data$Enzyme), 
                                   "perpetrator" = prettify_compound_name(MyPerpetrator))
      
   }
   
   ct_plot(ct_dataframe = Data, 
           figure_type = figure_type,
           mean_type = mean_type, 
           time_range = time_range,
           x_axis_interval = x_axis_interval,
           pad_x_axis = pad_x_axis,
           pad_y_axis = pad_y_axis,
           adjust_obs_time = adjust_obs_time,
           y_axis_limits_lin = y_axis_limits_lin,
           y_axis_limits_log = y_axis_limits_log,
           y_axis_label = y_axis_label,
           line_type = line_type,
           line_transparency = line_transparency,
           line_color = line_color,
           line_width = line_width,
           hline_position = hline_position, 
           hline_style = hline_style, 
           vline_position = vline_position, 
           vline_style = vline_style,
           legend_position = legend_position, 
           legend_label = legend_label,
           prettify_compound_names = prettify_compound_names,
           linear_or_log = linear_or_log,
           graph_labels = graph_labels,
           graph_title = graph_title, 
           graph_title_size = graph_title_size, 
           qc_graph = qc_graph,
           existing_exp_details = existing_exp_details,
           return_caption = return_caption, 
           save_graph = save_graph, 
           fig_height = fig_height, 
           fig_width = fig_width,
           # arguments that don't apply to enz_plots:
           obs_color = NA, obs_shape = NA, showBLQ = FALSE, 
           t0 = "simulation start")
}

