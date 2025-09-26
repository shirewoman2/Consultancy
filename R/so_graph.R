#' Graph of simulated vs. observed PK
#'
#' \code{so_graph} makes a graph of simulated vs. observed PK, including
#' indicating where the predicted parameters fell within X fold of the observed,
#' where "X" is whatever cutoffs you specify.
#'
#' @param PKtable a table in the same format as output from the function
#'   \code{\link{pk_table}}. This should include a column titled "Statistic" and
#'   columns for each of the PK parameters you want to graph. The column
#'   statistic should have values of "Simulated" and "Observed" for the
#'   simulated and observed PK, respectively; anything else will be ignored. The
#'   columns for each PK parameter should be named like the values in the
#'   data.frame PKParameterDefinitions, in the column "PKparameter". To see
#'   that, please enter \code{view(PKParameterDefinitions)} into the console.
#' @param PKparameters any of the AUC, Cmax, tmax, CL, or half-life PK
#'   parameters included in the output from \code{\link{pk_table}}; if left as
#'   NA, this will make graphs for each parameter included in \code{PKtable}. To
#'   see the full set of possible parameters, enter
#'   \code{view(PKParameterDefinitions)} into the console.
#' @param PKorder optionally specify the order of the graphs. Leaving this as
#'   "default" puts the graphs in the same order as the columns in the Simcyp
#'   Consultancy Team template for PK tables (plus some guesses at a good order
#'   for PK parameters that are not listed in said template table). Setting this
#'   to "user specified" will make the order of the graphs match the order you
#'   specified with the argument \code{PKparameters}. Graphs are plotted left to
#'   right and then top to bottom. If you would like a blank space inserted
#'   between some parameters -- for example, to keep all your Cmax values in the
#'   same column or something like that -- include "BLANK" in the values you
#'   list for \code{PKparameters} wherever you want that to happen, e.g.,
#'   \code{PKparameters = c("Cmax_dose1", "BLANK", "AUCinf_dose1", "BLANK",
#'   "tmax_dose1")}
#' @param all_intervals_together TRUE or FALSE (default) for whether to combine
#'   all of a single type of PK parameter into a single graph. For example,
#'   setting this to TRUE would put all the Cmax PK -- regardless of whether it
#'   was for the 1st dose, the last dose simulated, or a custom interval -- into
#'   a single graph. The default, FALSE, means that anything that was its own
#'   column in the PK table would also be its own graph here. \emph{NOTE:} If
#'   you do set this to TRUE, the shape of the points will be mapped to which
#'   interval it is, which means that you can't \emph{also} specify something
#'   for the argument \code{point_shape_column}. If you do, it will be ignored.
#'   Try this out if you're uncertain what we mean.
#' @param all_AUCs_together TRUE or FALSE (default) for whether to combine,
#'   e.g., AUCinf and AUCt for dose 1 into a single graph. \strong{Be careful}
#'   with this because if you have points for both AUCinf and AUCt for a
#'   simulation, then BOTH of those points will show up on the graph.
#' @param boundaries Numerical boundaries to show on the graph. Defaults to the
#'   1.5- and 2-fold boundaries. Indicate boundaries you want like this:
#'   \code{boundaries = c(1.25, 1.5, 2)}
#' @param boundaries_Guest Numerical boundaries to show on the graph when the PK
#'   parameter is a mean ratio of the parameter in the presence of a perpetrator
#'   / the parameter in the absence of the perpetrator. Please see
#'   https://pubmed.ncbi.nlm.nih.gov/21036951/ for a reference for this type of
#'   graph. If you'd rather show straight lines for these parameters instead of
#'   Guest curves, set this to NA. The default boundaries for Guest curves are 1
#'   (straight line at unity; even if you don't include 1, we'll add it back in)
#'   and 2. For all numbers > 1, you'll get a Guest curve that approaches that
#'   value at higher DDI ratios, and, for the highest number you list, you'll
#'   additionally get straight line boundaries. This matches what is described
#'   in the Guest Galetin 2011 Drug Metab Dispos paper. We recommend using only
#'   1 and 2 as Guest boundaries for clarity of the graph.
#' @param boundary_color_set set of colors to use for indicating the X-fold
#'   boundaries of the simulated / observed ratio. The default is "red black",
#'   which, for the default boundaries, results in a black line at the 1.5-fold
#'   boundary and a red one at the 2-fold boundary. Other options are "red
#'   green", "muted red green" (a lighter, more muted red and green that work
#'   well for indicating boundaries when you're using shading instead of lines),
#'   and "black", which will result in only black lines or shading. You also can
#'   set this to any set of colors you'd like, e.g., \code{boundary_color_set =
#'   c("yellow", "blue")}. The number of colors should equal the number of
#'   boundaries that you've indicated or the graph won't be easily
#'   interpretable.
#' @param boundary_color_set_Guest set of colors to use for indicating the
#'   X-fold boundaries of the simulated / observed ratio for DDI ratio graphs.
#'   The default is "red black", which, for the default Guest boundaries,
#'   results in a black curved line and a red straight line at the 2-fold
#'   boundary. Other options are "red green", "muted red green" (a lighter, more
#'   muted red and green that work well for indicating boundaries when you're
#'   using shading instead of lines), and "black", which will result in only
#'   black lines or shading. You also can set this to
#'   any set of colors you'd like, e.g., \code{boundary_color_set_Guest = c("yellow",
#'   "blue")}. The number of colors should equal the number of Guest boundaries
#'   that you've indicated or the graph won't be easily interpretable.
#' @param boundary_line_types optionally specify the line types to use for the
#'   boundaries. Leaving this as "default" results in a dashed line at unity and
#'   solid lines for all others, but you can specify this with any R-acceptable
#'   line types, e.g., \code{boundary_line_types = c("dotted", "dashed",
#'   "solid")}. To see the possibilities, type \code{ggpubr::show_line_types()}
#'   into the console.
#' @param boundary_line_types_Guest optionally specify the line types to use for
#'   the DDI ratio graph boundaries. Leaving this as "default" results in a
#'   dashed line at unity and solid lines for all others, but you can specify
#'   this with any R-acceptable line types, e.g., \code{boundary_line_types_Guest =
#'   c("dotted", "dashed", "solid")}. To see the possibilities, type
#'   \code{ggpubr::show_line_types()} into the console.
#' @param boundary_line_width line width; default is 0.7.
#' @param graph_labels TRUE or FALSE (default) for whether to include labels (A,
#'   B, C, etc.) for each of the small graphs.
#' @param axis_title_x title for the x axis; default is "Observed"
#' @param axis_title_y title for the y axis; default is "Simulated"
#' @param error_bars Which error bars should be shown on the graph? Options are
#'   "none" (default), "simulated" to show vertical error bars ("vertical" also
#'   works in case that's easier to remember), "observed" ("horizontal" also
#'   works), or "both".
#' @param variability_type If you're including error bars, what kind of
#'   variability would you like to have those error bars display? Options are
#'   "90\% CI" (default), "95\% CI", "CV\%", "percentiles", "standard
#'   deviation", ("SD" will also work fine), or "range". If \code{error_bars} is
#'   set to "none", this will be ignored.
#' @param point_color_column (optional) the column in \code{PKtable} that should
#'   be used for determining which color the points will be. This should be
#'   unquoted. For example, if you have a column named "Study" in the data.frame
#'   you're using for PKtable and you want to color the points by which study
#'   they came from, you would use: \code{point_color_column = Study}.
#' @param point_color_set the set of colors to use for the points. Options:
#'   \describe{
#'
#'   \item{"default"}{black if nothing is specified for
#'   \code{point_color_column} and, otherwise, a set of colors from Cynthia
#'   Brewer et al. from Penn State that are friendly to those with red-green
#'   colorblindness. The first three colors are green, orange, and purple. This
#'   can also be referred to as "Brewer set 2". If there are only two unique
#'   values in the point_color_column, then Brewer set 1 will be used since red
#'   and blue are still easily distinguishable but also more aesthetically
#'   pleasing than green and orange.}
#'
#'   \item{"Brewer set 1"}{colors selected from the Brewer palette "set 1". The
#'   first three colors are red, blue, and green.}
#'
#'   \item{"ggplot2 default"}{the default set of colors used in ggplot2 graphs
#'   (ggplot2 is an R package for graphing.)}
#'
#'   \item{"rainbow"}{colors selected from a rainbow palette. The default
#'   palette is limited to something like 6 colors, so if you have more than
#'   that, that's when this palette is most useful. It's \emph{not} very useful
#'   when you only need a couple of colors.}
#'
#'   \item{"blue-green"}{a set of blues fading into greens. This palette can be
#'   especially useful if you are comparing a systematic change in some
#'   continuous variable -- for example, increasing dose or predicting how a
#'   change in intrinsic solubility will affect concentration-time profiles --
#'   because the direction of the trend will be clear.}
#'
#'   \item{"blues"}{a set of blues fading light blue to dark blue. Like
#'   "blue-green", this palette can be especially useful if you are comparing a
#'   systematic change in some continuous variable. Other similar continuous
#'   color sets to try out: "purples", "greens", or "reds".}
#'
#'   \item{"Tableau"}{uses the standard Tableau palette; requires the "ggthemes"
#'   package}
#'
#'   \item{"viridis"}{from the eponymous package by Simon Garnier and ranges
#'   colors from purple to blue to green to yellow in a manner that is
#'   "printer-friendly, perceptually uniform and easy to read by those with
#'   colorblindness", according to the package author}
#'
#'   \item{a character vector of colors}{If you'd prefer to set all the colors
#'   yourself to \emph{exactly} the colors you want, you can specify those
#'   colors here. An example of how the syntax should look: \code{color_set =
#'   c("dodgerblue3", "purple", "#D8212D")} or, if you want to specify exactly
#'   which item in \code{point_color_column} gets which color, you can supply a
#'   named vector. For example, if you're coloring the lines by the compound ID,
#'   you could do this: \code{color_set = c("substrate" = "dodgerblue3",
#'   "inhibitor 1" = "purple", "primary metabolite 1" = "#D8212D")}. If you'd
#'   like help creating a specific gradation of colors, please talk to a member
#'   of the R Working Group about how to do that using
#'   \link{colorRampPalette}.}}
#' @param legend_label_point_color optionally indicate on the legend something
#'   explanatory about what the colors represent. For example, if
#'   \code{point_color_column = Study} and \code{legend_label_point_color =
#'   "Studies included"}, that will make the label above the items in the legend
#'   "Studies included" rather than the default, which is to use whatever the
#'   column name is for \code{point_color_column}. If you don't want a label for
#'   this legend item, set this to "none".
#' @param point_shape_column (optional) the column in \code{PKtable} that should
#'   be used for determining which shape the points will be. This should be
#'   unquoted. For example, if you have a column named "DosingFrequency" in the
#'   data.frame you're using for PKtable and you want to change the shape of the
#'   points by which dosing frequency was used, you would use:
#'   \code{point_color_column = DosingFrequency}.
#' @param point_shape optionally specify what shapes are used for the points.
#'   Input should look like this, for example: \code{c(1, 2)} to get an open
#'   circle and an open triangle. If you only specify one value, it will be used
#'   for all points. If you don't specify anything for
#'   \code{point_shape_column}, then only the first value listed will be used.
#'   To see all the possible shapes and what number corresponds to which shape,
#'   type \code{ggpubr::show_point_shapes()} into the console. If left as NA,
#'   all points will be filled circles.
#' @param point_size optionally specify the size of the points to use for the
#'   observed data. If left as NA, the size will be 2.
#' @param point_transparency optionally specify how transparent to make the
#'   points. The default of 1 will make completely opaque points, and 0 would be
#'   completely transparent (invisible).
#' @param legend_label_point_shape optionally indicate on the legend something
#'   explanatory about what the colors represent. For example, if
#'   \code{point_color_column = Study} and \code{legend_label_point_color =
#'   "Studies included"}, that will make the label above the items in the legend
#'   "Studies included" rather than the default, which is to use whatever the
#'   column name is for \code{point_color_column}. If you don't want a label for
#'   this legend item, set this to "none".
#' @param legend_position Specify where you want the legend to be. Options are
#'   "left", "right" (default in most scenarios), "bottom", "top", or "none" if
#'   you don't want one at all.
#' @param grid_color optionally specify the color of the major and minor grid
#'   lines on your graph. Default is "grey92", which is the standard grey for
#'   theme_bw() in ggplot2. Set this to "none" if you want no grid lines under
#'   your graph.
#' @param ncol number of columns of graphs to show. If left as NULL (default), R
#'   will make a reasonable guess for the number.
#' @param nrow number of rows of graphs to show. If left as NULL (default), R
#'   will make a reasonable guess for the number.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png"
#' @param fig_height figure height in inches; default is 8
#' @param fig_width figure width in inches; default is 6
#' @param return_list_of_graphs TRUE or FALSE (default) for whether to return a
#'   list of each individual graph as its own separate ggplot2 object.
#' @param include_dose_num Should the dose number be included? If set to TRUE,
#'   then the dose number part of the graph title, e.g., the "Dose 1" or "Last
#'   dose" part of "Dose 1 AUCinf" or "Last dose Cmax", will be included. If set
#'   to FALSE, those would become "AUCinf" and "Cmax" only with no reference to
#'   which dose it was. If left as the default NA, then the dose number will be
#'   omitted if all the data are all for dose 1 or all for the last dose, and it
#'   will be included if you have a mix of dosing intervals.
#' @param facet_title_size optionally specify what font size to use for the
#'   facet titles. If left as NA, a reasonable guess will be used.
#' @param title_adjustments a character vector or list of text adjustments for
#'   the graph titles. Possible options:
#'
#'   \describe{\item{"sub steady-state for last"}{Instead of the
#'   default PK parameters for the last dose being labeled as, e.g.,
#'   "Last dose Cmax", this will use "steady-state" instead, e.g.,
#'   "Steady-state Cmax"}
#'
#'   \item{"use my expressions"}{If you'd like to use your own specific R
#'   expressions rather than the defaults included in the package, you can do
#'   that. You will need to supply a list here, and all of your PK parameters
#'   must be included or things will not work well. Here is an example of how
#'   to use this: title_adjustments = list("AUCtau_last" = expression(AUC[t]),
#'   "Cmax_last" = expression(C[max]), "Cmin_last" = expression(C[trough]))}
#'
#'   \item{sub 0 to inf for inf}{NOT SET UP YET. This is a placeholder for other
#'   substitutions people might want. Instead of the using AUCinf, graph titles will
#'   use AUC0 to inf}}
#'
#' @return a set of arranged ggplot2 graphs
#' @export
#'
#' @examples
#'
#' # The object SOdata has some example PK data to work with
#' # and is included in the package.
#' so_graph(PKtable = SOdata)
#'
#' so_graph(PKtable = SOdata,
#'          axis_title_y = "Predicted",
#'          axis_title_x = "Observed",
#'          grid_color = "none")
#'
#' so_graph(PKtable = SOdata,
#'          ncol = 1)
#'
#' so_graph(PKtable = SOdata,
#'          point_shape_column = Study,
#'          legend_position = "bottom")
#'
#' so_graph(PKtable = SOdata,
#'          point_shape_column = Study,
#'          legend_label_point_shape = "Studies involving Drug X",
#'          point_shape = c(21:25),
#'          point_color_column = File,
#'          legend_label_point_color = "Simulation file",
#'          point_color_set = "blues",
#'          legend_position = "right",
#'          error_bars = "horizontal")
#' # NB: This will generate 3 warnings about 
#' # `override.aes` being ignored that we *CANNOT* 
#' # get to disappear. Please ignore the warnings 
#' # about that being ignored. 
#' 
#' 

so_graph <- function(PKtable, 
                     PKparameters = NA, 
                     PKorder = "default", 
                     boundaries = c(1, 1.5, 2),
                     boundaries_Guest = c(1, 2),
                     boundary_color_set = "red black", 
                     boundary_color_set_Guest = "red black", 
                     boundary_line_types = "default",
                     boundary_line_types_Guest = "default",
                     boundary_line_width = 0.3, 
                     error_bars = "none", 
                     variability_type = "90% CI", 
                     point_color_column, 
                     point_color_set = "default",
                     legend_label_point_color = NA, 
                     point_shape_column, 
                     point_shape = NA,
                     point_size = NA,
                     point_transparency = 1, 
                     legend_label_point_shape = NA, 
                     legend_position = "none",
                     axis_title_x = "Observed",
                     axis_title_y = "Simulated", 
                     facet_title_size = NA, 
                     title_adjustments = c(), 
                     graph_labels = FALSE, 
                     include_dose_num = NA,
                     all_intervals_together = FALSE, 
                     all_AUCs_together = FALSE, 
                     grid_color = NA, 
                     ncol = NULL, 
                     nrow = NULL,
                     save_graph = NA, 
                     fig_width = 8, 
                     fig_height = 6, 
                     return_list_of_graphs = FALSE){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if("list" %in% class(PKtable)){
      PKtable <- PKtable$Table
   }
   
   if("CompoundID" %in% names(PKtable) && 
      length(unique(PKtable$CompoundID)) > 1){
      stop(wrapn("You have more than one compound ID in your data, and the so_graph function is meant to be used with just one compound ID at a time since the idea is that we'd be evaluating just one model at a time. Please remove the rows in your table that apply to whatever compounds you do not want and try again."), 
           call. = FALSE)
   }
   
   if(is.na(axis_title_x)){
      warning(wrapn("You must specify a value for `axis_title_x`; it can't be NA. We'll use the default of `Observed`."), 
              call. = FALSE)
      axis_title_x <- "Observed"
   }
   
   if(is.na(axis_title_y)){
      warning(wrapn("You must specify a value for `axis_title_y`; it can't be NA. We'll use the default of `Simulated`"), 
              call. = FALSE)
      axis_title_y <- "Simulated"
   }
   
   
   if("Observed" %in% PKtable$Statistic == FALSE){
      stop("We can't find the observed data in the table provided for `PKtable`, so we can't make a simulated-versus-observed graph.", 
           call. = FALSE)
   }
   
   # Checking for appropriate numeric input for boundaries
   if(any(boundaries < 1)){
      warning("At least one of the numbers you specified for boundaries was < 1. We will automatically use both the original number you specified and its inverse for boundaries.", 
              call. = FALSE)
   }
   boundaries <- sort(unique(c(1, as.numeric(boundaries[boundaries > 0]),
                               1/as.numeric(boundaries[boundaries > 0]))))
   boundaries <- boundaries[boundaries >= 1]
   
   if(any(boundaries_Guest < 1)){
      warning("At least one of the numbers you specified for boundaries_Guest was < 1. We will automatically use both the original number you specified and its inverse for boundaries_Guest.", 
              call. = FALSE)
   }
   boundaries_Guest <- sort(unique(c(1, as.numeric(boundaries_Guest[boundaries_Guest > 0]),
                                     1/as.numeric(boundaries_Guest[boundaries_Guest > 0]))))
   boundaries_Guest <- boundaries_Guest[boundaries_Guest >= 1]
   
   # Checking color input. Should be fine to set this to lower case b/c named
   # colors should be lower case and even hex colors are not case sensitive.
   boundary_color_set <- tolower(boundary_color_set)
   boundary_color_set_Guest <- tolower(boundary_color_set_Guest)
   
   # Making sure they've specified correct number of colors for boundaries
   if(length(boundaries) != length(boundary_color_set) &
      boundary_color_set[1] %in% c("red green", "red black", "black", "muted red green") == FALSE){
      warning("You have specified one number of boundaries and a different number of colors for those boundaries, so we don't know what colors you want. We'll use the default boundary line colors.\n", 
              call. = FALSE)
      boundary_color_set <- "red black"
   }
   
   if(boundary_color_set[1] %in% c("red green", "muted red green", 
                                   "red black") == FALSE && 
      tryCatch(is.matrix(col2rgb(boundary_color_set)),
               error = function(x) FALSE) == FALSE){
      warning("The values you used for boundary colors are not all valid colors in R. We'll used the default colors instead.\n", 
              call. = FALSE)
      boundary_color_set <- "red black"
   } 
   
   if(length(boundaries_Guest) != length(boundary_color_set_Guest) &
      boundary_color_set_Guest[1] %in% c("red green", "red black", "black", "muted red green") == FALSE){
      warning("You have specified one number of boundaries for DDI graphs and a different number of colors for those boundaries, so we don't know what colors you want. We'll use the default Guest boundary line colors.\n", 
              call. = FALSE)
      boundary_color_set_Guest <- "red black"
   }
   
   if(boundary_color_set_Guest[1] %in% c("red green", "muted red green", 
                                         "red black") == FALSE && 
      tryCatch(is.matrix(col2rgb(boundary_color_set_Guest)),
               error = function(x) FALSE) == FALSE){
      warning("The values you used for Guest boundary colors are not all valid colors in R. We'll used the default colors instead.\n", 
              call. = FALSE)
      boundary_color_set_Guest <- "red black"
   } 
   
   boundary_color_set_Guest <- boundary_color_set_Guest
   
   # This next bit is for dealing with blank graphs when user specifies PKorder
   # and wants to include some blank spaces so that, for example, all the Cmax
   # graphs are in the same column or row. See the help file for the arguments
   # PKorder and PKparameters to see how this would be set up.
   PKwithBlanks <- PKparameters
   PKwithBlanks[toupper(PKwithBlanks) == "BLANK"] <- 
      paste0(PKwithBlanks[toupper(PKwithBlanks) == "BLANK"], 
             1:length(PKwithBlanks[toupper(PKwithBlanks) == "BLANK"]))
   PKparameters <- PKparameters[!toupper(PKparameters) == "BLANK"]
   
   if(PKorder %in% c("default", "user specified") == FALSE){
      warning(paste0("PKorder must be either `default` or `user specified`, and you listed `", 
                     PKorder, "`. We'll set this to the default (1st dose before last dose; baseline, then + perpetrator, then GMRs).\n"), 
              call. = FALSE)
      PKorder <- "default"
   }
   
   if(PKorder == "user specified" & all(is.na(PKparameters))){
      warning("You said that you wanted to specify the order of the graphs by PK parameter but did not list any specific PK parameters with the argument `PKparameters`, so we don't know what order you would like. We'll set the order to the default, which is alphabetical by PK parameter name.\n", 
              call. = FALSE)
      PKorder <- "default"
   }
   
   grid_color_gg <- ifelse(is.na(grid_color[1]), "grey92", grid_color[1])
   if(grid_color_gg == "none"){
      grid_color_gg <- NA
   } else {
      ColorCheck <- try(expr = col2rgb(grid_color), silent = TRUE)
      if(is.matrix(ColorCheck) == FALSE){
         warning(wrapn("You have supplied something that is not an acceptable color in R for the argument 'grid_color', so we'll use the default value of 'grey92'."), 
                 call. = FALSE)
         grid_color_gg <- "grey92"
      }
   }
   
   suppressWarnings(point_transparency <- as.numeric(point_transparency[1]))
   
   error_bars <- tolower(error_bars)[1]
   error_bars <- case_match(error_bars, 
                            "none" ~ "none", 
                            "simulated" ~ "vertical", 
                            "vertical" ~ "vertical", 
                            "observed" ~ "horizontal", 
                            "horizontal" ~ "horizontal", 
                            "both" ~ "both")
   if(is.na(error_bars)){
      warning(wrapn("You have requested something for error_bars that is not among the possible options, so we will use the default value of 'none'."), 
              call. = FALSE)
      error_bars <- "none"
   }
   
   variability_type <- tolower(variability_type)[1]
   variability_type <- case_match(variability_type, 
                                  "90% ci" ~ "90% CI",
                                  "ci" ~ "90% CI", 
                                  "95% ci" ~ "95% CI", 
                                  "cv%" ~ "CV%",
                                  "cv" ~ "CV%", 
                                  "gcv" ~ "CV%", 
                                  "percentiles" ~ "percentiles",
                                  "sd" ~ "SD",
                                  "standard deviation" ~ "SD",
                                  "range" ~ "range")
   if(error_bars != "none" & is.na(variability_type)){
      warning(wrapn("You have requested something for variability_type that is not among the possible options, so we will use the default value of '90% CI'."), 
              call. = FALSE)
   }
   
   
   # Main body of function --------------------------------------------------
   
   ## Setting colors & linetypes -----------------------------------------------
   
   if(boundary_color_set[1] %in% c("red green", "red black", 
                                   "muted red green")){
      
      boundary_color_set <- 
         set_boundary_colors(color_set = boundary_color_set,
                             boundaries = boundaries, 
                             break_type = "SO line")
      
   }
   
   if(boundary_color_set_Guest[1] %in% c("red green", "red black", 
                                         "muted red green")){
      
      boundary_color_set_Guest <- 
         set_boundary_colors(color_set = boundary_color_set_Guest,
                             boundaries = c(boundaries_Guest, Inf), # Hack to get enough colors
                             break_type = "SO line")
      
   } else {
      # Making sure we have enough colors
      boundary_color_set <- rep(boundary_color_set, length(boundaries))
      boundary_color_set <- boundary_color_set[1:length(boundaries)]
      
      boundary_color_set_Guest <- rep(boundary_color_set_Guest, length(boundaries_Guest) + 1)
      # NB: Guest boundaries require an extra color b/c boundary for
      # highest-fold error has both curved and straight lines.
      boundary_color_set_Guest <- boundary_color_set_Guest[1:(length(boundaries_Guest) + 1)]
   }
   
   # Making sure we have enough linetypes and line widths
   if(boundary_line_types[1] == "default"){
      boundary_line_types_straight <-
         c("dashed", rep("solid", length(boundaries) - 1))
      
   } else {
      boundary_line_types_straight <-
         rep(boundary_line_types, length(boundaries))
   }
   
   if(boundary_line_types_Guest[1] == "default"){
      boundary_line_types_Guest <- c("dashed", 
                                     rep("solid", length(boundaries_Guest)))
   } else {
      boundary_line_types_Guest <- rep(boundary_line_types_Guest,
                                       length(boundaries_Guest) + 1)
   }
   
   boundary_line_types_straight <- 
      boundary_line_types_straight[1:length(boundaries)]
   
   boundary_line_width <- as.numeric(boundary_line_width[1])
   
   # NB: There is not actually a difference between the boundary line widths
   # when they're either straight or Guest curves, but this makes sure we have
   # enough of them.
   boundary_line_width_straight <- 
      rep(boundary_line_width, length(boundaries))[1:length(boundaries)]
   
   # NB: Guest boundaries require an extra linetype b/c boundary for
   # highest-fold error has both curved and straight lines.
   boundary_line_types_Guest <- 
      boundary_line_types_Guest[1:(length(boundaries_Guest) + 1)]
   
   boundary_line_width_Guest <- 
      rep(boundary_line_width, length(boundaries_Guest) + 1)[
         1:(length(boundaries_Guest) + 1)]
   
   
   ## Setting boundaries ------------------------------------------------------
   
   # Noting user's original preferences for a few things
   point_color_set_user <- point_color_set
   point_shape_user <- point_shape
   point_size_user <- point_size
   
   # Setting up data for boundaries on graphs
   Boundaries_num <- boundaries
   Boundaries_num_Guest <- boundaries_Guest
   Boundaries <- list()
   GuestCurves <- list()
   GuestStraight <- list()
   Poly <- list()
   PolyGuest <- list()
   
   # Regular boundaries
   for(j in Boundaries_num){
      
      Boundaries[[as.character(j)]] <-
         tibble(Observed = rep(10^seq(-4, 9, length.out = 100), 2), 
                LimitName = rep(c("upper", "lower"), each = 100), 
                Simulated = case_when(LimitName == "upper" ~ Observed * j, 
                                      LimitName == "lower" ~ Observed / j), 
                Group = paste(LimitName, j), 
                Boundary = paste0(j, "x"))
   }
   
   Boundaries <- bind_rows(Boundaries) %>% 
      mutate(Boundary = factor(Boundary, 
                               levels = paste0(unique(Boundaries_num), "x")))
   
   
   # BoundariesGuest 
   for(j in Boundaries_num_Guest){
      
      GuestCurves[[as.character(j)]] <- 
         list("Upper" = data.frame(Observed = 10^seq(-4, 9, length.out = 1000)) %>% 
                 mutate(Limit = ifelse(Observed >= 1, 
                                       (1 + j*(Observed - 1))/Observed,
                                       (1 + j*((1/Observed) - 1))/(1/Observed)),
                        LimitName = "upper",
                        Simulated = Observed * Limit, 
                        Limit = j, 
                        Boundary = paste0(j, "x (Guest curve)")),
              "Lower" = data.frame(Observed = 10^seq(-4, 9, length.out = 1000)) %>% 
                 mutate(Limit = ifelse(Observed >= 1, 
                                       (1 + j*(Observed - 1))/Observed,
                                       (1 + j*((1/Observed) - 1))/(1/Observed)),
                        LimitName = "lower", 
                        Simulated = Observed / Limit, 
                        Limit = j, 
                        Boundary = paste0(j, "x (Guest curve)"))) %>% 
         bind_rows()
      
      if(j %in% c(1, max(Boundaries_num_Guest))){
         GuestStraight[[as.character(j)]] <- 
            list("Upper" = data.frame(Observed = 10^seq(-4, 9, length.out = 100)) %>% 
                    mutate(LimitName = "upper", 
                           Simulated = Observed * j, 
                           Limit = j, 
                           Boundary = paste0(j, "x")), 
                 "Lower" = data.frame(Observed = 10^seq(-4, 9, length.out = 100)) %>% 
                    mutate(LimitName = "lower", 
                           Simulated = Observed / j, 
                           Limit = j, 
                           Boundary = paste0(j, "x"))) %>% 
            bind_rows()
      }
   }
   
   BoundariesGuest <- bind_rows(
      bind_rows(GuestCurves), 
      bind_rows(GuestStraight)) %>% 
      filter(Boundary != "1x (Guest curve)") %>% 
      arrange(Limit) %>% 
      mutate(Group = paste(LimitName, Boundary), 
             Boundary = factor(Boundary, levels = unique(Boundary)))
   
   ## Setting things up for nonstandard evaluation - Part 1 --------------------
   point_color_column <- rlang::enquo(point_color_column)
   point_shape_column <- rlang::enquo(point_shape_column)
   
   # Dealing with units if there are more than one set.
   PKUnits <- names(PKtable)[str_detect(names(PKtable), " \\(")]
   # CL will always be L/h, so removing those from consideration. Also removing
   # anything with a time unit s/a tmax b/c it will always be h.
   PKUnits <- PKUnits[!str_detect(PKUnits, "\\(L/h|\\(h\\)")]
   PKUnits <- sort(unique(gsub("\\(|\\)", "", str_extract(PKUnits, "\\(.*\\)"))))
   
   # Check whether there are any mixes of molar and mass per volume units b/c we
   # can't interconvert since we don't know MWs.
   if(any(str_detect(PKUnits, ".M")) & any(str_detect(PKUnits, "/.*L"))){
      stop("You appear to have some molar units and some mass per volume units, and we can't interconvert and compare those here. Please adjust your units, change your header row to reflect whichever units you're using, and try again.", 
           call. = FALSE)
   }
   # Everything should be all mass per volume or all molar concs now. 
   
   # Separating AUC and Cmax units b/c they have different regex requirements.
   PKAUCUnits <- PKUnits[str_detect(PKUnits, "\\.h")]
   PKCmaxUnits <- setdiff(PKUnits, PKAUCUnits)
   
   # Making all mass per volume units use ng/mL units and all molar units be uM
   # b/c that matches the units in PKexpressions. Otherwise, the mini graph
   # titles will be incorrect.
   PKAUCUnits <- setdiff(PKAUCUnits, "ng/mL.h")
   PKCmaxUnits <- setdiff(PKCmaxUnits, "ng/mL")
   
   if(length(c(PKAUCUnits, PKCmaxUnits)) > 0 &&
      any(c(length(PKAUCUnits), length(PKCmaxUnits)) > 1)){
      AUCcols <- names(PKtable)[str_detect(names(PKtable), 
                                           str_c(PKAUCUnits, collapse = "|"))]
      Cmaxcols <- names(PKtable)[str_detect(names(PKtable), 
                                            str_c(PKCmaxUnits, collapse = "|"))]
      Cmaxcols <- setdiff(Cmaxcols, AUCcols)
      
      for(col in c(AUCcols, Cmaxcols)){
         # convert_conc_units was specifically set up for converting conc-time
         # data.frames where there is a column Conc that is to be converted and
         # another column, Conc_units, that indicates what the units are.
         # Hacking PKtable to match that format.
         names(PKtable)[which(names(PKtable) == col)] <- "Conc"
         
         PKtable <- PKtable %>% 
            mutate(Conc_units = sub("\\.h", "", 
                                    str_extract(col, str_c(c(PKAUCUnits, 
                                                             PKCmaxUnits), collapse = "|"))), 
                   Conc = as.numeric(Conc)) %>% 
            convert_conc_units(conc_units = "ng/mL") %>% 
            select(-Conc_units)
         
         # Can't just change the name of this column b/c that column name may
         # already exist. Need to remove and then join w/original PK table,
         # keeping all necessary columns when joining.
         ToJoin <- PKtable %>% 
            select(File, Conc, Statistic, 
                   any_of(c("Compound", "CompoundID", "Tissue", 
                            as_label(point_color_column), 
                            as_label(point_shape_column)))) %>% 
            filter(complete.cases(Conc))
         
         newcol <- sub("\\(.*\\)", 
                       ifelse(col %in% Cmaxcols, 
                              "(ng/mL)", "(ng/mL.h)"),
                       col)
         names(ToJoin)[which(names(ToJoin) == "Conc")] <- newcol
         
         suppressMessages(
            PKtable <- PKtable %>% 
               mutate(across(.cols = any_of({{newcol}}), 
                             .fns = as.numeric)) %>% 
               full_join(ToJoin) %>% select(-Conc)
         )
         
         rm(newcol, ToJoin)
         
      }
   }
   
   # Will need to figure out what PK parameters are and will need deprettified
   # names when reshaping and organizing data here and lower in function. 
   PKCols <- prettify_column_names(PKtable, return_which_are_PK = TRUE)
   
   # Need to check when concs are molar b/c changes graph title
   ConcType <- ifelse(any(str_detect(PKCols$PrettifiedNames, "µM")), 
                      "molar", "mass per volume")
   
   if(any(is.na(PKparameters))){
      PKparameters <- unique(PKCols$PKparameter[PKCols$IsPKParam])
   }
   
   # Arranging and tidying input data. 
   SO <- PKtable %>% 
      pivot_longer(names_to = "ColName", 
                   values_to = "Value", 
                   cols = any_of(PKCols$ColName[PKCols$IsPKParam == TRUE])) %>% 
      filter(complete.cases(Value)) %>% 
      mutate(Statistic = as.character(Statistic), 
             Statistic = ifelse(str_detect(Statistic, "^Simulated"),
                                "Simulated", Statistic)) %>% 
      left_join(PKCols %>% 
                   select(ColName, PKparameter, Interval), 
                by = "ColName")
   
   # # Removing additional columns since they mess up pivoting.
   # select(Statistic, File,
   #        any_of(c(PKparameters, "CompoundID", "Tissue", "Sheet", 
   #                 as_label(point_color_column), 
   #                 as_label(point_shape_column))))%>% 
   # unique() %>% 
   
   
   ## Tidying input data further now that format is long by parameter -------------
   
   # Standardizing names and un-concatenating variability as necessary so that
   # we can graph it as error bars. Lower error bar will be for "Var_lower" and
   # upper error bar will be "Var_upper". Observed error bars will be
   # "ObsVar_lower" and "ObsVar_upper". First, need to fill in any NA values in
   # columns we're splitting by b/c they will make the whole list be empty. This
   # won't affect anything else, so fine to leave "default" in the data. This is
   # also where I'm making everything in the Value column numeric.
   
   # Adding placeholder columns as needed
   MissingCols <- setdiff(c("File", "CompoundID", "Tissue", "Sheet", 
                            "Interval"), 
                          names(SO))
   for(mm in MissingCols){
      SO[, mm] <- "default"
   }
   
   SO <- SO %>% 
      mutate(across(.cols = c(Statistic, File, CompoundID, Tissue, Sheet,
                              PKparameter, Interval), 
                    .fns = \(x) ifelse(is.na(x), "default", x)), 
             Value = gsub("\\(|\\)|\\[|\\]", "", Value), 
             Value = sub(", | - ", " to ", Value))
   
   SO <- split(SO, list(SO$Statistic, 
                        SO$File, 
                        SO$CompoundID, 
                        SO$Tissue, 
                        SO$Sheet, 
                        SO$Interval, 
                        SO$PKparameter))
   
   for(i in names(SO)){
      
      # This allows for row binding later.
      if(nrow(SO[[i]]) == 0){
         suppressWarnings(
            SO[[i]]$Value <- as.numeric(SO[[i]]$Value)
         )
         next
      }
      
      # NB: Values in the column Statistic should ONLY be among those listed
      # in AllStats$ReportNames.
      VarType <- unique(SO[[i]]$Statistic)
      
      # Need to skip central stats, need to split any concatenated
      # variability and need to calculate any variability that would only be
      # 1 number, e.g., CV or SD.
      
      if(VarType %in% c(AllStats$ReportNames[
         which(AllStats$StatType == "central statistic")], 
         
         AllStats$ReportNames[
            which(str_detect(AllStats$ReportNames, "trial means"))])){
         
         suppressWarnings(
            SO[[i]]$Value <- as.numeric(SO[[i]]$Value)
         )
         
      } else if(VarType %in% c("CV%", "Standard deviation")){
         # VarType must be calculated to get high and low - simulated
         
         CentralValue <- as.numeric(SO[[sub(VarType, "Simulated", i)]]$Value)
         
         SO[[i]] <- SO[[i]] %>% 
            mutate(
               Value = as.numeric(Value), 
               Var_lower = case_match(
                  {VarType}, 
                  "CV%" ~ CentralValue - CentralValue * Value/100, 
                  "Standard deviation" ~ CentralValue - Value), 
               
               Var_upper = case_match(
                  {VarType}, 
                  "CV%" ~ CentralValue + CentralValue * Value/100, 
                  "Standard deviation" ~ CentralValue + Value)) %>% 
            select(-Value, -Statistic) %>% 
            pivot_longer(cols = c(Var_lower, Var_upper), 
                         names_to = "Statistic", 
                         values_to = "Value")
         
      } else if(VarType %in% c("Observed CV%", 
                               "Observed standard deviation")){
         # VarType must be calculated to get high and low - observed
         
         CentralValue <- as.numeric(SO[[sub(VarType, "Observed", i)]]$Value)
         
         SO[[i]] <- SO[[i]] %>% 
            mutate(
               Value = as.numeric(Value), 
               Var_lower = case_match(
                  {VarType}, 
                  "Observed CV%" ~ CentralValue - CentralValue * Value/100, 
                  "Observed standard deviation" ~ CentralValue - Value), 
               
               Var_upper = case_match(
                  {VarType}, 
                  "Observed CV%" ~ CentralValue + CentralValue * Value/100, 
                  "Observed standard deviation" ~ CentralValue + Value)) %>% 
            select(-Value, -Statistic) %>% 
            pivot_longer(cols = c(Var_lower, Var_upper), 
                         names_to = "Statistic", 
                         values_to = "Value")
         
      } else if(VarType %in% c("5th to 95th Percentile", 
                               "Observed 5th to 95th Percentile", 
                               "90% CI", "95% CI", 
                               "Observed 90% CI", 
                               "Observed 95% CI", 
                               "Observed CI", 
                               "Observed range", "Range")){
         # VarType must be split to get high and low
         
         SO[[i]] <- SO[[i]] %>% 
            separate_wider_delim(cols = Value, 
                                 delim = " to ", 
                                 names = c("Var_lower", "Var_upper"), 
                                 too_few = "align_start") %>% 
            select(-Statistic) %>% 
            pivot_longer(cols = c(Var_lower, Var_upper), 
                         names_to = "Statistic", 
                         values_to = "Value") %>% 
            mutate(Value = as.numeric(Value))
         
      } else if(VarType %in% c("90% CI - Lower", "95% CI - Lower", 
                               "5th Percentile", "Observed 5th Percentile",
                               "Minimum", 
                               "Observed CI - Lower")){
         # VarType must be renamed to Var_lower
         SO[[i]] <- SO[[i]] %>% rename(Value = Var_lower)
         
      } else if(VarType %in% c("90% CI - Upper", "95% CI - Upper", 
                               "95th Percentile", "Observed 95th Percentile", 
                               "Maximum", 
                               "Observed CI - Upper")){
         # VarType must be renamed to Var_upper
         SO[[i]] <- SO[[i]] %>% rename(Value = Var_upper)
         
      }
      
      if(VarType %in% AllStats$ReportNames[AllStats$SorO == "Observed"]){
         SO[[i]] <- SO[[i]] %>% 
            mutate(Statistic = case_match(Statistic, 
                                          "Var_lower" ~ "ObsVar_lower", 
                                          "Var_upper" ~ "ObsVar_upper", 
                                          .default = Statistic))
         
      }
      
      SO[[i]]$OrigStat <- VarType
      
      rm(VarType)
   }
   
   SO <- bind_rows(SO)
   
   # Filtering to retain only the central stat and the variability type they
   # requested.
   GoodStats <- c("Simulated", "Observed", 
                  switch(variability_type, 
                         "90% CI" = c("90% CI - Lower", 
                                      "90% CI - Upper", 
                                      "90% CI", 
                                      "Observed CI - Lower", 
                                      "Observed CI - Upper", 
                                      "Observed CI", 
                                      "Observed 90% CI"),
                         "95% CI" = c("95% CI - Lower", 
                                      "95% CI - Upper", 
                                      "95% CI", 
                                      "Observed CI - Lower", 
                                      "Observed CI - Upper", 
                                      "Observed 95% CI",
                                      "Observed CI"), 
                         "CV%" = c("CV%", "Observed CV%"), 
                         "percentiles" = c("5th Percentile", 
                                           "95th Percentile", 
                                           "5th to 95th Percentile", 
                                           "Observed 5th to 95th Percentile"), 
                         "SD" = "Standard deviation", 
                         "Standard deviation" = "Standard deviation", 
                         "range" = c("Minimum", "Maximum", "Range", 
                                     "Observed range")))
   
   SO <- SO %>% 
      filter(OrigStat %in% GoodStats & 
                complete.cases(Value)) %>% 
      select(-OrigStat) %>% 
      pivot_wider(names_from = Statistic, 
                  values_from = Value) %>% 
      filter(complete.cases(Observed) & 
                complete.cases(Simulated) & 
                PKparameter %in% {{PKparameters}})
   
   # A bit more error catching now that everything is tidy
   if("CompoundID" %in% names(SO) && length(unique(SO$CompoundID)) > 1){
      warning(paste0(wrapn("You have more than one compound ID present in your PK data, so making a single set of simulated-vs-observed graphs for all of them might not be advisable. Specifically you have the following compounds present in your PK data:"), 
                     str_c(sort(unique(SO$CompoundID)), collapse = "\n"), "\n"), 
              call. = FALSE)
   }
   
   DupCheck <- SO %>% select(any_of(
      c("File", "CompoundID", "Tissue", "Sheet", "Interval", "Statistic", "PKparameter")))
   DupCheck <- DupCheck[which(duplicated(DupCheck)), ] %>% as.data.frame()
   
   if(nrow(DupCheck) > 0){
      message(wrapn("You have some duplicates present in your data, which makes it unclear which simulated value matches which observed. Specifically, here are the places where you have more than one value for the same thing:"))
      Problem <- capture.output(print(DupCheck, row.names = FALSE))
      message(str_c(Problem, collapse = "\n"))
      
      stop(wrapn("We're sorry, but we cannot make your graphs as long as these duplicates are present."), 
           call. = FALSE)
      
   }
   
   include_dose_num <- check_include_dose_num(PK = PKparameters, 
                                              include_dose_num = include_dose_num)
   
   if(include_dose_num == FALSE){
      PKparameters <- sub("Dose 1 |Last dose |_dose1$|_last$", "",
                          PKparameters)
      SO$PKparameter <- sub("_dose1|_last", "", SO$PKparameter)
      PKCols$PKparameter <- sub("_dose1|_last", "", PKCols$PKparameter)
      
      if("list" %in% class(title_adjustments)){
         names(title_adjustments) <- 
            sub("_last|_dose1", "", names(title_adjustments))
      }
   }
   
   if(all_intervals_together){
      
      # For this option, data must be in long format. If user has not included
      # dose numbers in the column names when they ran pk_table, we'll assume
      # that all data were for the same standard interval. If some of the names
      # include interval and some don't, then the ones w/out must be
      # user-defined intervals.
      AnyIntervalIncluded <- any(str_detect(
         PKCols$PKparameter[PKCols$IsPKParam], "_dose1|_last"), na.rm = T)
      
      UnlabeledIntText <- ifelse(AnyIntervalIncluded, 
                                 "user-defined interval", 
                                 "applies to all intervals")
      
      if(as_label(point_shape_column) != "<empty>"){
         warning(wrapn("You have specified something for the point_shape_column but also requested that we show all dosing intervals together, which overrides that. The point shape will be determined by the interval rather than by point_shape_column."), 
                 call. = FALSE)
      }
      
      SO <- SO %>% 
         mutate(
            PKparameter_orig = PKparameter, 
            PKparameter = gsub("_last|_dose1|inf|tau", "", PKparameter), 
            PKparameter = gsub("AUCt", "AUC", PKparameter), 
            point_shape_column = Interval)
      
      legend_label_point_shape <- "Interval"
      
   } else if(all(is.na(legend_label_point_shape))){
      legend_label_point_shape <- as_label(point_shape_column)
   }
   
   if(all(is.na(legend_label_point_color))){
      legend_label_point_color <- as_label(point_color_column)
   }
   
   
   if(all_AUCs_together){
      SO$PKparameter <- sub("AUCinf|AUCtau", "AUC", SO$PKparameter)
      SO$PKparameter <- sub("AUCt_", "AUC_", SO$PKparameter)
      
      PKparameters <- sub("AUCinf|AUCtau", "AUC", PKparameters)
      PKparameters <- sub("AUCt_", "AUC_", PKparameters)
      PKparameters <- unique(PKparameters)
   }
   
   # It's possible to have both CLt_dose1 and CLinf_dose1 and they're labeled
   # the same way in PKexpressions. Adjusting for that scenario.
   if(all(c("CLinf_dose1", "CLt_dose1") %in% PKparameters)){
      PKexpressions[["CLinf_dose1"]] <- 
         expression(atop(Dose ~ 1 ~ CL ~ "(L/h)", 
                         paste("CL as dose/AUCinf")))
      PKexpressions[["CLt_dose1"]] <- 
         expression(atop(Dose ~ 1 ~ CL ~ "(L/h)", 
                         paste("CL as dose/AUCt")))
   }
   
   # Checking for other graph title substitutions they want
   if(length(title_adjustments) > 0){
      if("character" %in% class(title_adjustments) &&
         "sub steady-state for last" %in% tolower(title_adjustments) |
         "sub steady state for last" %in% tolower(title_adjustments)){
         
         PKparameters <- sub("_last", "_ss", PKparameters)
         PKwithBlanks <- sub("_last", "_ss", PKwithBlanks)
         SO$PKparameter <- sub("_last", "_ss", SO$PKparameter)
      } else if("list" %in% class(title_adjustments)){
         # These should all be expressions. If they're not, force it.
         if(any(sapply(title_adjustments, class) != "expression", na.rm = T)){
            
            for(i in which(sapply(title_adjustments, class) != "expression")){
               title_adjustments[[i]] <- as.expression(title_adjustments[[i]])
            }
         }
         
         # Checking for missing titles
         MissingTitles <- setdiff(PKparameters, names(title_adjustments))
         
         for(i in MissingTitles){
            if(i %in% names(PKexpressions)){
               title_adjustments[[i]] <- PKexpressions[[i]]
            } else {
               title_adjustments[[i]] <- 
                  as.expression(PKCols$PrettifiedNames[PKCols$PKparameter == i])
            }
         }
      }
   }
   
   ## Setting things up for nonstandard evaluation - Part 2 --------------------
   if(as_label(point_color_column) != "<empty>"){
      SO <- SO %>% mutate(point_color_column = {{point_color_column}}) %>% 
         droplevels()
      
      if(class(SO$point_color_column) != "factor"){
         Levels <- sort(unique(SO$point_color_column))
         SO <- SO %>% 
            mutate(point_color_column = factor(point_color_column, levels = Levels))
      } 
      
      NumColorsNeeded <- length(sort(unique(SO$point_color_column)))
      point_color_set <- make_color_set(color_set = point_color_set, 
                                        num_colors = NumColorsNeeded)
      MyPointColors <- point_color_set
      
   } else {
      # Setting color to black if point_color_column unspecified
      point_color_set <- make_color_set(color_set = point_color_set, 
                                        num_colors = 1)
      MyPointColors <- point_color_set
      SO$point_color_column <- "A" # placeholder
   }
   
   if(all_intervals_together == FALSE){
      if(as_label(point_shape_column) != "<empty>"){
         SO <- SO %>% mutate(point_shape_column = {{point_shape_column}}) %>% 
            droplevels()
      } else {
         SO$point_shape_column <- "A" # placeholder
      }
   }
   
   # Checking for any missing values in point_shape_column or point_color_column
   # b/c that will mess things up.
   if(any(is.na(SO$point_shape_column))){
      warning(wrapn(paste0("You have missing values in the column '", 
                           as_label(point_shape_column), "', which messes up assigning point shapes, so we will use the same point shape for everything.")), 
              call. = FALSE)
      SO$point_shape_column <- "A" # placeholder
   }
   
   if(any(is.na(SO$point_color_column))){
      warning(wrapn(paste0("You have missing values in the column '", 
                           as_label(point_color_column), "', which messes up assigning point colors, so we will use the same point color for everything.")), 
              call. = FALSE)
      SO$point_color_column <- "A" # placeholder
   }
   
   if(as_label(point_shape_column) != "<empty>" | 
      all_intervals_together){
      
      if(class(SO$point_shape_column) != "factor"){
         Levels <- sort(unique(SO$point_shape_column))
         SO <- SO %>% 
            mutate(point_shape_column = factor(point_shape_column, levels = Levels))
      }
      
      NumShapesNeeded <- length(sort(unique(SO$point_shape_column)))
      
      if(any(complete.cases(point_shape))){
         if(NumShapesNeeded == 1){
            MyPointShapes <- point_shape[1]
         } else {
            if(length(point_shape) < NumShapesNeeded){
               # This odd syntax will work both when point_shape is a single value
               # and when it is multiple values.
               MyPointShapes <- rep(point_shape, NumShapesNeeded)[1:NumShapesNeeded] 
            } else if(length(point_shape) >= NumShapesNeeded){
               MyPointShapes <- point_shape[1:NumShapesNeeded] 
            }
         }
      } else {
         MyPointShapes <- c(16:18, 15, 8, 3:7, 9:14)[1:NumShapesNeeded]
      }
   } else {
      # If they haven't specified a point shape column, then make the shape be
      # the 1st number in point_shape
      MyPointShapes <- point_shape[1]
      MyPointShapes <- ifelse(is.na(MyPointShapes), 16, MyPointShapes)
   }
   
   if(length(MyPointShapes) != length(unique(SO$point_shape_column))){
      warning(wrapn(paste0("The number of point shapes you requested is not equal to the number of unique values in the column '", 
                           as_label(point_shape_column), 
                           "', so we will recycle or remove shapes to get as many as we need.")), 
              call. = FALSE)
      
      if(length(MyPointShapes) > length(unique(SO$point_shape_column))){
         MyPointShapes <- MyPointShapes[1:length(unique(SO$point_shape_column))]
      } else {
         MyPointShapes <- rep(MyPointShapes, 
                              length(unique(SO$point_shape_column)))[1:length(unique(SO$point_shape_column))]
         names(MyPointShapes) <- unique(SO$point_shape_column)
      }
   } else {
      names(MyPointShapes) <- unique(SO$point_shape_column)
   }
   
   if(length(MyPointColors) != length(unique(SO$point_color_column))){
      warning(wrapn(paste0("The number of point colors you requested is not equal to the number of unique values in the column '", 
                           as_label(point_color_column), 
                           "', so we will recycle or remove colors to get as many as we need.")), 
              call. = FALSE)
      
      if(length(MyPointColors) > length(unique(SO$point_color_column))){
         MyPointColors <- MyPointColors[1:length(unique(SO$point_color_column))]
      } else {
         MyPointColors <- rep(MyPointColors, 
                              length(unique(SO$point_color_column)))[1:length(unique(SO$point_color_column))]
         names(MyPointColors) <- unique(SO$point_color_column)
      }
   } else {
      names(MyPointColors) <- unique(SO$point_color_column)
   }
   
   SO <- SO %>% 
      mutate(Shape = MyPointShapes[point_shape_column], 
             Color = MyPointColors[point_color_column], 
             Fill = Color, 
             Color = case_when(Shape %in% c(21:25) ~ "black", 
                               .default = Color))
   
   # Establishing names for colors and shapes
   MyColors <- setNames(SO$Color, SO$point_color_column)
   MyFillColors <- setNames(SO$Fill, SO$point_color_column)
   MyShapes <- setNames(SO$Shape, SO$point_shape_column)
   
   # Adding legend label for color and shape as appropriate
   ShowLegColor <- as_label(point_color_column) != "<empty>" &&
      length(unique(SO$point_color_column)) > 1
   
   ShowLegShape <- 
      (all_intervals_together == FALSE &&
          as_label(point_shape_column) != "<empty>" &&
          length(unique(SO$point_shape_column)) > 1) |
      
      (all_intervals_together == TRUE &&
          length(unique(SO$point_shape_column)) > 1)
   
   
   ## Making graphs ---------------------------------------------------------
   G <- list()
   SO <- split(SO, f = SO$PKparameter)
   ErrorBarMsg <- list()
   
   if(length(find.package("ggnewscale", quiet = TRUE)) == 0){
      
      message(paste0("\n", wrapn("To include a legend for the boundaries, we need to install the R package 'ggnewscale'.")))
      Install <- readline(prompt = "Is it ok to install ggnewscale for you? (y or n)   ")
      
      if(tolower(str_sub(Install, 1, 1)) == "y"){
         install.packages("ggnewscale")
      } else {
         message(wrapn("Ok, we will not install ggnewscale for you; we will not include a legend for the boundaries."))
      }
   }
   
   for(i in names(SO)){
      
      # Removing columns when everything is NA b/c that will make for inaccurate
      # warnings.
      SO[[i]] <- SO[[i]] %>% purrr::discard(~all(is.na(.)))
      
      # Noting which error bars are possible
      ErrorBarCheck <- 
         switch(
            paste("Sim", all(c("Var_lower", "Var_upper") %in% names(SO[[i]])), 
                  "Obs", all(c("ObsVar_lower", "ObsVar_upper") %in% names(SO[[i]]))), 
            
            "Sim TRUE Obs TRUE" = c("none", "vertical", "horizontal", "both"), 
            "Sim TRUE Obs FALSE" = c("none", "vertical"), 
            "Sim FALSE Obs TRUE" = c("none", "horizontal"), 
            "Sim FALSE Obs FALSE" = c("none"))
      
      ErrorBarMsg[[i]] <- case_when(
         error_bars %in% ErrorBarCheck ~ "no message", 
         error_bars == "none" ~ "no message", 
         error_bars == "both" & "horizontal" %in% ErrorBarCheck ~ "you requested error bars in both directions, but your data only included information for horizontal ones.", 
         error_bars == "both" & "vertical" %in% ErrorBarCheck ~ "you requested error bars in both directions, but your data only included information for vertical ones.", 
         error_bars %in% ErrorBarCheck == FALSE ~ paste0("you requested ", 
                                                         case_match(error_bars, 
                                                                    "both" ~ "error bars in both directions", 
                                                                    "vertical" ~ "vertical error bars", 
                                                                    "horizontal" ~ "horizontal error bars"), 
                                                         ", but no variability data were included in what you supplied for PKtable. We won't be able to include error bars for this/these parameter(s).")
      )
      
      error_bars_i <- case_when(
         error_bars %in% ErrorBarCheck ~ error_bars, 
         error_bars == "none" ~ error_bars, 
         error_bars == "both" & "horizontal" %in% ErrorBarCheck ~ "horizontal", 
         error_bars == "both" & "vertical" %in% ErrorBarCheck ~ "vertical", 
         # below is when they've requested horizontal and only vertical
         # available or vice versa
         error_bars %in% ErrorBarCheck == FALSE ~ "none", 
         # Above scenarios should cover all possibilities, but including
         # .default just in case.
         .default = "none")
      
      Limits <- switch(
         error_bars_i, 
         "none" = c(
            round_down(min(c(SO[[i]]$Observed, 
                             SO[[i]]$Simulated), na.rm = T)),
            
            round_up(max(c(SO[[i]]$Observed, 
                           SO[[i]]$Simulated), na.rm = T))), 
         
         "vertical" = c(
            round_down(min(c(SO[[i]]$Observed, 
                             SO[[i]]$Var_lower, 
                             SO[[i]]$Simulated), na.rm = T)),
            
            round_up(max(c(SO[[i]]$Observed, 
                           SO[[i]]$Var_upper, 
                           SO[[i]]$Simulated), na.rm = T))), 
         
         "horizontal" = c(
            round_down(min(c(SO[[i]]$Observed, 
                             SO[[i]]$ObsVar_lower, 
                             SO[[i]]$Simulated), na.rm = T)),
            
            round_up(max(c(SO[[i]]$Observed, 
                           SO[[i]]$ObsVar_upper, 
                           SO[[i]]$Simulated), na.rm = T))), 
         
         "both" = c(
            round_down(min(c(SO[[i]]$Observed, 
                             SO[[i]]$ObsVar_lower, 
                             SO[[i]]$Var_lower, 
                             SO[[i]]$Simulated), na.rm = T)),
            
            round_up(max(c(SO[[i]]$Observed, 
                           SO[[i]]$ObsVar_upper, 
                           SO[[i]]$Var_upper, 
                           SO[[i]]$Simulated), na.rm = T)))
      )
      
      if(str_detect(i, "ratio")){
         
         # If it was a Guest plot, then set the limit to be < 1 and the upper to
         # be > 1. Otherwise, that lowest/highest point is way in the corner.
         if(Limits[1] == 1){Limits[1] <- 2/3}
         if(Limits[2] == 1){Limits[2] <- 3}
         
         G[[i]] <- ggplot() +
            geom_line(data = BoundariesGuest,
                      aes(x = Observed, y = Simulated,
                          color = Boundary, 
                          group = Group,
                          linetype = Boundary, 
                          linewidth = Boundary), 
                      show.legend = T) + 
            scale_linewidth_manual(
               values = c(boundary_line_width_Guest, 
                          boundary_line_width_straight), 
               name = "Boundaries for ratios") +
            # NB: For things to work correctly with ggnewscale, the legend name
            # MUST be specified with the scale_x_value call; it does NOT work
            # when specified with, e.g., labs(color = ...)
            scale_color_manual(
               values = boundary_color_set_Guest, 
               name = "Boundaries for ratios", 
               guide = guide_legend(
                  override.aes = list(shape = NA))) +
            
            scale_linetype_manual(
               values = boundary_line_types_straight, 
               name = "Boundaries for ratios")
         
      } else {
         G[[i]] <- ggplot() +
            geom_line(data = Boundaries, 
                      aes(x = Observed, y = Simulated,
                          color = Boundary, 
                          group = Group,
                          linetype = Boundary, 
                          linewidth = Boundary), 
                      show.legend = T) +
            scale_linewidth_manual(
               values = boundary_line_width_straight, 
               name = "Boundaries") +
            # NB: For things to work correctly with ggnewscale, the legend name
            # MUST be specified with the scale_x_value call; it does NOT work
            # when specified with, e.g., labs(color = ...)
            scale_color_manual(
               values = boundary_color_set, 
               name = "Boundaries", 
               guide = guide_legend(
                  override.aes = list(shape = NA))) +
            scale_linetype_manual(
               values = boundary_line_types_straight, 
               name = "Boundaries")
         
      }
      
      if(length(find.package("ggnewscale", quiet = TRUE)) != 0){
         
         # Adding a new scale for colors so that the legend for the lines will look
         # different from the legend for the points.
         G[[i]] <- G[[i]] +
            ggnewscale::new_scale("colour") +
            ggnewscale::new_scale("linetype") +
            ggnewscale::new_scale("linewidth")
      } 
      
      PossBreaks <- sort(c(10^(-3:9),
                           3*10^(-3:9),
                           5*10^(-3:9)))
      PossBreaks <- PossBreaks[PossBreaks >= Limits[1] &
                                  PossBreaks <= Limits[2]]
      
      if(length(PossBreaks) >= 5){
         MajBreaks <- 10^(-3:9)
         MinBreaks <- rep(1:9)*rep(10^(-3:9), each = 9)
      } else {
         MajBreaks <- c(10^(-3:9),
                        3*10^(-3:9),
                        5*10^(-3:9))
         MinBreaks <- rep(1:9)*rep(10^(-3:9), each = 9)
      }
      
      # Need to set the width and height of these error bars or they're
      # preposterously large for some and itty bitty for others. This required a
      # lot of fiddling before I figured out what the relationship was b/c it's
      # really not clear what a given value for the bar width means.
      BarWidth <- log10(round_up(Limits[2] / Limits[1])) * 0.05 - 0.025
      
      if(error_bars_i %in% c("vertical", "both")){
         suppressWarnings(
            G[[i]] <- G[[i]] +
               geom_errorbar(data = SO[[i]], 
                             aes(x = Observed, 
                                 color = point_color_column, 
                                 ymin = Var_lower, ymax = Var_upper),
                             width = BarWidth, 
                             show.legend = FALSE)
         )
      } 
      
      if(error_bars_i %in% c("horizontal", "both")){
         suppressWarnings(
            G[[i]] <- G[[i]] +
               geom_errorbarh(data = SO[[i]], 
                              aes(y = Simulated, 
                                  color = point_color_column, 
                                  xmin = ObsVar_lower, xmax = ObsVar_upper), 
                              height = BarWidth, 
                              show.legend = FALSE)
         )
      } 
      
      # Aesthetics for points are determined by:
      
      # 1) whether they've specified anything for point shape column. Note that,
      # if they specified that they wanted all intervals together, that means
      # that the point shape column will be the interval. 
      
      # 2) whether they've specified anything for point color column, 
      
      # 3) whether what they've specified for point shape and point color is the
      # same, and 
      
      # 4) whether the specific point shape they have requested is one of the
      # ones with both fill and outline.
      
      if(as_label(point_color_column) == as_label(point_shape_column) & 
         all_intervals_together == FALSE){
         
         G[[i]] <- G[[i]] +
            geom_point(data = SO[[i]], 
                       aes(x = Observed, y = Simulated, 
                           color = point_color_column, 
                           fill = point_color_column, 
                           shape = point_color_column),
                       size = ifelse(is.na(point_size), 2, point_size), 
                       alpha = ifelse(is.na(point_transparency), 1, point_transparency), 
                       show.legend = TRUE) +
            
            scale_color_manual(
               values = MyColors, drop = FALSE, 
               name = legend_label_point_color, 
               guide = guide_legend(
                  nrow = switch(legend_position,
                                "bottom" = 1,
                                "top" = 1, 
                                "left" = NULL,
                                "right" = NULL), 
                  order = case_when(
                     # no legend for color
                     as_label(point_color_column) == "<empty>" ~ 99,
                     
                     as_label(point_color_column) != "<empty>" ~ 1))) +
            
            scale_fill_manual(
               values = MyFillColors, drop = FALSE, 
               name = legend_label_point_color, 
               guide = guide_legend(
                  nrow = switch(legend_position,
                                "bottom" = 1,
                                "top" = 1, 
                                "left" = NULL,
                                "right" = NULL), 
                  order = case_when(
                     # no legend for color
                     as_label(point_color_column) == "<empty>" ~ 99,
                     
                     as_label(point_color_column) != "<empty>" ~ 1))) +
            
            scale_shape_manual(
               values = MyShapes, drop = FALSE, 
               name = legend_label_point_shape, 
               guide = guide_legend(
                  nrow = switch(legend_position,
                                "bottom" = 1,
                                "top" = 1, 
                                "left" = NULL,
                                "right" = NULL), 
                  order = case_when(
                     # no legend for color
                     as_label(point_shape_column) == "<empty>" & 
                        all_intervals_together == FALSE ~ 99,
                     
                     as_label(point_color_column) == "<empty>" & 
                        as_label(point_shape_column) != "<empty>" ~ 1, 
                     
                     as_label(point_color_column) != "<empty>" & 
                        as_label(point_color_column) != as_label(point_shape_column) ~ 2, 
                     
                     all_intervals_together == TRUE & 
                        as_label(point_color_column) != "<empty>" ~ 2, 
                     
                     all_intervals_together == TRUE & 
                        as_label(point_color_column) == "<empty>" ~ 1, 
                     
                     as_label(point_color_column) != "<empty>" & 
                        as_label(point_color_column) == as_label(point_shape_column) ~ 1)))
         
      } else {
         
         G[[i]] <- G[[i]] +
            geom_point(data = SO[[i]], 
                       aes(x = Observed, y = Simulated, 
                           color = point_color_column, 
                           fill = point_color_column, 
                           shape = point_shape_column),
                       size = ifelse(is.na(point_size), 2, point_size), 
                       alpha = ifelse(is.na(point_transparency), 1, point_transparency), 
                       show.legend = TRUE) +
            scale_color_manual(
               values = MyColors, drop = FALSE, 
               name = legend_label_point_color, 
               guide = guide_legend(
                  nrow = switch(legend_position,
                                "bottom" = 1,
                                "top" = 1, 
                                "left" = NULL,
                                "right" = NULL), 
                  order = case_when(
                     # no legend for color
                     as_label(point_color_column) == "<empty>" ~ 99,
                     
                     as_label(point_color_column) != "<empty>" ~ 1))) +
            
            scale_fill_manual(
               values = MyFillColors, drop = FALSE, 
               name = legend_label_point_color, 
               guide = guide_legend(
                  nrow = switch(legend_position,
                                "bottom" = 1,
                                "top" = 1, 
                                "left" = NULL,
                                "right" = NULL), 
                  order = case_when(
                     # no legend for color
                     as_label(point_color_column) == "<empty>" ~ 99,
                     
                     as_label(point_color_column) != "<empty>" ~ 1))) +
            
            scale_shape_manual(
               values = MyShapes, drop = FALSE, 
               name = legend_label_point_shape, 
               guide = guide_legend(
                  nrow = switch(legend_position,
                                "bottom" = 1,
                                "top" = 1, 
                                "left" = NULL,
                                "right" = NULL), 
                  order = case_when(
                     # no legend for color
                     as_label(point_shape_column) == "<empty>" & 
                        all_intervals_together == FALSE ~ 99,
                     
                     as_label(point_color_column) == "<empty>" & 
                        as_label(point_shape_column) != "<empty>" ~ 1, 
                     
                     as_label(point_color_column) != "<empty>" & 
                        as_label(point_color_column) != as_label(point_shape_column) ~ 2, 
                     
                     all_intervals_together == TRUE & 
                        as_label(point_color_column) != "<empty>" ~ 2, 
                     
                     all_intervals_together == TRUE & 
                        as_label(point_color_column) == "<empty>" ~ 1, 
                     
                     as_label(point_color_column) != "<empty>" & 
                        as_label(point_color_column) == as_label(point_shape_column) ~ 1)))
         
      }
      
      if(length(MyPointColors) > 3){
         if(any(MyPointShapes %in% c(21:25))){
            G[[i]] <- G[[i]] + 
               guides(
                  color = guide_legend(
                     override.aes = list(shape = 21), 
                     ncol = case_when(
                        legend_position %in% c("left", "right") ~ 2, 
                        legend_position %in% c("top", "bottom") ~ 3)), 
                  fill = guide_legend(
                     override.aes = list(shape = 21), 
                     ncol = case_when(
                        legend_position %in% c("left", "right") ~ 2, 
                        legend_position %in% c("top", "bottom") ~ 3)))
         } else {
            G[[i]] <- G[[i]] + 
               guides(
                  color = guide_legend(
                     ncol = case_when(
                        legend_position %in% c("left", "right") ~ 2, 
                        legend_position %in% c("top", "bottom") ~ 3)))
         }
      } else if(any(MyPointShapes %in% c(21:25))){
         G[[i]] <- G[[i]] + 
            guides(
               color = guide_legend(
                  override.aes = list(shape = 21)), 
               fill = guide_legend(
                  override.aes = list(shape = 21)))
      }
      
      if(length(MyPointShapes) > 3){
         G[[i]] <- G[[i]] + 
            guides(
               shape = guide_legend(
                  ncol = case_when(
                     legend_position %in% c("left", "right") ~ 2, 
                     legend_position %in% c("top", "bottom") ~ 3)))
      }
      
      if("list" %in% class(title_adjustments)){
         Gtitle <- title_adjustments[[i]]
      } else {
         if(str_detect(i, "_withInhib")){
            Gtitle <- PKexpressions[[paste0(i, switch(ConcType, 
                                                      "molar" = "_molar", 
                                                      "mass per volume" = ""), 
                                            "_2")]]
         } else {
            Gtitle <- PKexpressions[[paste0(i, switch(ConcType, 
                                                      "molar" = "_molar", 
                                                      "mass per volume" = ""))]]
         }
      }
      
      CheckRange <- ifelse(round_up(max(c(SO[[i]]$Simulated, 
                                          SO[[i]]$Observed),
                                        na.rm = T)) >= 1e5, 
                           "sci", "comma")
      
      G[[i]] <- G[[i]] + 
         xlab(axis_title_x) +
         ylab(axis_title_y) +
         scale_y_log10(breaks = MajBreaks, 
                       minor_breaks = MinBreaks, 
                       labels = switch(CheckRange, 
                                       "sci" = scales::label_scientific(MajBreaks), 
                                       "comma" = scales::label_comma(MajBreaks))) +
         scale_x_log10(breaks = MajBreaks, 
                       minor_breaks = MinBreaks, 
                       labels = switch(CheckRange, 
                                       "sci" = scales::label_scientific(MajBreaks), 
                                       "comma" = scales::label_comma(MajBreaks))) +
         coord_cartesian(xlim = Limits, ylim = Limits) + # this causes the shading to disappear for BoundariesGuest curves. no idea why, but I think it's a bug w/coord_cartesian.
         ggtitle(Gtitle) +
         theme_bw() +
         theme(aspect.ratio = 1, 
               panel.grid = element_line(color = grid_color_gg), 
               plot.title = element_text(hjust = 0.5, 
                                         size = ifelse(is.na(facet_title_size), 
                                                       12, facet_title_size)),
               axis.title = element_text(color = "black", face = "bold"),
               axis.title.x = element_text(margin = margin(2.75, 0, 0, 0)),
               axis.title.x.top = element_text(margin = margin(0, 0, 2.75, 0)),
               axis.title.y = element_text(margin = margin(0, 2.75, 0, 0)),
               axis.title.y.right = element_text(margin = margin(0, 0, 0, 2.75)))
      
      if(ShowLegColor){
         if(complete.cases(legend_label_point_color)){
            if(legend_label_point_color == "none"){    
               G[[i]] <- G[[i]] + labs(color = NULL, 
                                       fill = NULL)
            } else {
               G[[i]] <- G[[i]] + labs(color = legend_label_point_color, 
                                       fill = legend_label_point_color)
            }
         } else {
            # This is when no legend_label_point_color has been specified.
            G[[i]] <- G[[i]] + labs(color = as_label(point_color_column), 
                                    fill = as_label(point_color_column))
         }
      } else {
         G[[i]] <- G[[i]] + guides(color = "none", 
                                   fill = "none")
      }
      
      if(ShowLegShape){
         if(complete.cases(legend_label_point_shape)){
            if(legend_label_point_shape == "none"){    
               G[[i]] <- G[[i]] + labs(shape = NULL)
            } else {
               G[[i]] <- G[[i]] + labs(shape = legend_label_point_shape)
            }
         } else {
            # This is when no legend_label_point_color has been specified.
            G[[i]] <- G[[i]] + labs(shape = as_label(point_shape_column))
         }
      } else {
         G[[i]] <- G[[i]] + guides(shape = "none")
      }
   }
   
   if(length(G) == 1){
      G <- G[[1]] + theme(legend.position = legend_position)
      
      if(return_list_of_graphs){
         PlotList <- G
      }
      
   } else {
      
      # ncol and nrow must both be specified or neither specified. Dealing with
      # that.
      NumCR <- paste(is.null(ncol), is.null(nrow))
      
      # Setting the order if user requested that. 
      
      if(PKorder == "user specified"){
         if(include_dose_num == FALSE){
            PKwithBlanks <- sub("_dose1|_last", "", PKwithBlanks)
         }
         
         if(all_AUCs_together){
            PKwithBlanks <- sub("AUCinf|AUCtau", "AUC", PKwithBlanks)
            PKwithBlanks <- sub("AUCt_", "AUC_", PKwithBlanks)
         }
         
         for(blanks in PKwithBlanks[str_detect(PKwithBlanks, "BLANK")]){
            G[[blanks]] <- ggplot() + theme_void()
         }
         
         G <- G[PKwithBlanks[PKwithBlanks %in% names(G)]]
         
      } else {
         
         GoodOrder <- AllPKParameters %>% select(PKparameter, SortOrder) %>% 
            bind_rows(AllPKParameters %>% select(PKparameter_nodosenum, SortOrder) %>% 
                         rename(PKparameter = PKparameter_nodosenum)) %>% 
            arrange(SortOrder) %>% pull(PKparameter) %>% unique()
         
         if(all_AUCs_together){
            GoodOrder <- sub("AUCinf|AUCtau", "AUC", GoodOrder)
            GoodOrder <- sub("AUCt_", "AUC_", GoodOrder)
            GoodOrder <- unique(GoodOrder)
         }
         
         if(all_intervals_together){
            GoodOrder <- sub("AUCinf|AUCtau", "AUC", GoodOrder)
            GoodOrder <- sub("AUCt_", "AUC_", GoodOrder)
            GoodOrder  <- gsub("_last|_dose1|inf|tau", "", GoodOrder)
            GoodOrder <- unique(GoodOrder)
         }
         
         if("sub steady-state for last" %in% title_adjustments){
            GoodOrder <- sub("_last", "_ss", GoodOrder)
         }
         
         GoodOrder <- GoodOrder[GoodOrder %in% names(G)]
         
         G <- G[GoodOrder]
      }
      
      if(return_list_of_graphs){
         PlotList <- G
      }
      
      G <- patchwork::wrap_plots(
         G,
         nrow = switch(NumCR, 
                       
                       # specified both
                       "FALSE FALSE" = nrow,
                       
                       # specified only ncol
                       "FALSE TRUE" = round_up_unit(length(G)/ncol, 1),
                       
                       # specified only nrow
                       "TRUE FALSE" = nrow,
                       
                       # specified neither
                       "TRUE TRUE" = NULL),
         ncol = switch(NumCR, 
                       
                       # specified both
                       "FALSE FALSE" = ncol,
                       
                       # specified only ncol
                       "FALSE TRUE" = ncol,
                       
                       # specified only nrow
                       "TRUE FALSE" = round_up_unit(length(G)/nrow, 1),
                       
                       # specified neither
                       "TRUE TRUE" = NULL)) + 
         patchwork::plot_layout(guides = "collect")
      
      if(graph_labels){
         G <- G + patchwork::plot_annotation(tag_level = "A")
      }
      
      if(legend_position == "none"){
         
         G <- G & theme(legend.position = "none")
         
      } else {
         
         if(legend_position %in% c("bottom", "top")){
            if(packageVersion("ggplot2") > "3.5.0"){
               G <- G &
                  theme(legend.box = "vertical", 
                        legend.spacing.y = unit(0, units = "lines"), 
                        legend.key.spacing.y = unit(-0.15, units = "lines"), 
                        legend.position = legend_position)
            } else {
               G <- G & 
                  theme(legend.box = "vertical", 
                        legend.spacing.y = unit(0, units = "lines"), 
                        legend.position = legend_position)
            }
            
         } else {
            G <- G + theme(legend.spacing.y = unit(0.5, units = "lines"), 
                           legend.position = legend_position)
         }
      }
   }
   
   # Collecting error bar warnings
   ErrorBarMsg <- 
      tibble(PKparameter = names(ErrorBarMsg), 
             Msg = unlist(ErrorBarMsg)) %>% 
      filter(!Msg == "no message") %>% 
      group_by(Msg) %>% 
      summarize(Warning = paste0("For ", str_comma(PKparameter), ", ",
                                 unique(Msg))) %>% 
      ungroup() %>% 
      pull(Warning)
   
   if(length(ErrorBarMsg) > 0){
      warning(str_c(wrapn(ErrorBarMsg), sep = "\n"), 
              call. = FALSE)
   }
   
   # Saving --------------------------------------------------------------
   if(complete.cases(save_graph)){
      FileName <- save_graph
      if(str_detect(FileName, "\\.")){
         # Making sure they've got a good extension
         Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
         FileName <- sub(paste0(".", Ext), "", FileName)
         if(Ext %in% c("eps", "ps", "jpeg", "tiff",
                       "png", "bmp", "svg", "jpg", "docx") == FALSE){
            warning(paste0("You have requested the graph's file extension be `", 
                           Ext, "`, but we haven't set up that option. We'll save your graph as a `png` file instead.\n"),
                    call. = FALSE)
         }
         Ext <- ifelse(Ext %in% c("eps", "ps", "jpeg", "tiff",
                                  "png", "bmp", "svg", "jpg", "docx"), 
                       Ext, "png")
         FileName <- paste0(FileName, ".", Ext)
      } else {
         FileName <- paste0(FileName, ".png")
         Ext <- "png"
      }
      
      if(Ext == "docx"){
         
         # This is when they want a Word file as output
         OutPath <- dirname(FileName)
         if(OutPath == "."){
            OutPath <- getwd()
         }
         
         FileName <- basename(FileName)
         
         rmarkdown::render(system.file("rmarkdown/templates/sograph/skeleton/skeleton.Rmd",
                                       package="SimcypConsultancy"), 
                           output_dir = OutPath, 
                           output_file = FileName, 
                           quiet = TRUE)
         # Note: The "system.file" part of the call means "go to where the
         # package is installed, search for the file listed, and return its
         # full path.
         
      } else {
         # This is when they want any kind of graphical file format.
         ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
                plot = G)
         
      }
   }
   
   if(return_list_of_graphs){
      return(PlotList)
   } else {
      return(G)
   }
   
}


