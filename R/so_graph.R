#' Graph of simulated vs. observed PK
#'
#' \code{so_graph} makes a graph of simulated vs. observed PK, including
#' indicating where the predicted parameters fell within X fold of the observed.
#'
#' @param PKtable a table in the same format as output from the function
#'   \code{\link{pksummary_mult}}. This should include a column titled
#'   "Statistic" and columns for each of the PK parameters you want to graph.
#'   The column statistic should have values of "Simulated" and "Observed" for
#'   the simulated and observed PK, respectively; anything else will be ignored.
#'   The columns for each PK parameter should be named like the values in the
#'   data.frame PKParameterDefinitions, in the column "PKparameter". To see
#'   that, please enter \code{view(PKParameterDefinitions)} into the console.
#' @param PKparameters any of the AUC, Cmax, tmax, CL, or half-life PK
#'   parameters included in the output from \code{\link{pksummary_mult}}; if
#'   left as NA, this will make graphs for each parameter included in
#'   \code{PKtable}. To see the full set of possible parameters, enter
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
#' @param all_intervals_together NOT YET SET UP. TRUE or FALSE (default) for
#'   whether to combine all of a single type of PK parameter into a single
#'   graph. For example, setting this to TRUE would put all the Cmax PK --
#'   regardless of whether it was for the 1st dose, the last dose, or a custom
#'   interval -- into a single graph. The default, FALSE, means that anything
#'   that was its own column in the PK summary table would also be its own graph
#'   here. If you do set this to TRUE, the color and shape of the points will be
#'   mapped to which interval it is, which means that you can't \emph{also}
#'   specify something for the arguments \code{point_color_column} or
#'   \code{point_shape_column}. If you do, those will be ignored. Try this out
#'   if you're uncertain what we mean.
#' @param boundaries Numerical boundaries to show on the graph. Defaults to the
#'   1.5- and 2-fold boundaries. Indicate boundaries you want like this:
#'   \code{boundaries = c(1.25, 1.5, 2)}
#' @param boundaries_Guest Numerical boundaries to show on the graph when the PK
#'   parameter is a mean ratio of the parameter in the presence of a perpetrator
#'   / the parameter in the absence of the perpetrator. Please see
#'   [BoundariesGuest Galetin 2011 Drug Metab
#'   Dispos](https://pubmed.ncbi.nlm.nih.gov/21036951/) for a reference for this
#'   type of graph. If you'd rather show straight lines for these parameters
#'   instead of BoundariesGuest curves, set this to NA. The default boundaries
#'   for BoundariesGuest curves are 1 (straight line at unity; even if you don't
#'   include 1, we'll add it back in) and 2. For all numbers > 1, you'll get a
#'   BoundariesGuest curve that approaches that value at higher DDI ratios, and,
#'   for the highest number you list, you'll additionally get straight line
#'   boundaries. This matches what is described in the BoundariesGuest Galetin
#'   2011 paper. We recommend using only 1 and 2 as BoundariesGuest boundaries
#'   for clarity of the graph.
#' @param boundary_indicator how to indicate the boundaries for simulated /
#'   observed. Options are "lines" (default), "fill" to get a shaded area, or
#'   "none" to remove any indicators of those
#'   boundaries. \strong{NOTE: There is a known bug within RStudio that causes
#'   filled semi-transparent areas like you get with the "fill" option to NOT
#'   get graphed for certain versions of RStudio.} To get around this, within
#'   RStudio, go to Tools --> Global Options --> General --> Graphics --> And
#'   then set "Graphics device: backend" to "AGG". Honestly, this is a better
#'   option for higher-quality graphics anyway!
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
#'   The default is "red black", which, for the default BoundariesGuest
#'   boundaries, results in a black curved line and a red straight line at the
#'   2-fold boundary. Other options are "red green", "muted red green" (a
#'   lighter, more muted red and green that work well for indicating boundaries
#'   when you're using shading instead of lines), and "black", which will result
#'   in only black lines or shading. You also can set this to
#'   any set of colors you'd like, e.g., \code{boundary_color_set_Guest = c("yellow",
#'   "blue")}. The number of colors should equal the number of BoundariesGuest boundaries
#'   that you've indicated or the graph won't be easily interpretable.
#' @param boundary_line_types optionally specify the line types to use for the
#'   boundaries (only applicable when \code{boundary_indicator = "lines"}).
#'   Leaving this as "default" results in a dashed line at unity and solid lines
#'   for all others, but you can specify this with any R-acceptable line types,
#'   e.g., \code{boundary_line_types = c("dotted", "dashed", "solid")}. To see
#'   the possibilities, type \code{ggpubr::show_line_types()} into the console.
#' @param boundary_line_types_Guest optionally specify the line types to use for
#'   the DDI ratio graph boundaries (only applicable when
#'   \code{boundary_indicator = "lines"}). Leaving this as "default" results in
#'   a dashed line at unity and solid lines for all others, but you can specify
#'   this with any R-acceptable line types, e.g., \code{boundary_line_types_Guest =
#'   c("dotted", "dashed", "solid")}. To see the possibilities, type
#'   \code{ggpubr::show_line_types()} into the console.
#' @param boundary_line_width line width; default is 0.7. This only applies when
#'   \code{boundary_indicator} is set to "lines", the default.
#' @param axis_title_x title for the x axis; default is "Observed"
#' @param axis_title_y title for the y axis; default is "Simulated"
#' @param axis_titles SOON TO BE DEPRECATED in favor of \code{axis_title_x} and
#'   \code{axis_title_x}. Optionally specify what you'd like for the x and y
#'   axis titles with a named character vector. The default is \code{axis_titles =
#'   c("x" = "Observed", "y" = "Simulated")}
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
#'   systematic change in some continuous variable.}
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
#' @param ncol number of columns of graphs to show. If left as NULL (default), R
#'   will make a reasonable guess for the number.
#' @param nrow number of rows of graphs to show. If left as NULL (default), R
#'   will make a reasonable guess for the number.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png"
#' @param fig_height figure height in inches; default is 8
#' @param fig_width figure width in inches; default is 6
#' @param include_dose_num Should the dose number be included? If set to TRUE,
#'   then the dose number part of the graph title, e.g., the "Dose 1" or "Last
#'   dose" part of "Dose 1 AUCinf" or "Last dose Cmax", will be included. If set
#'   to FALSE, those would be come "AUCinf" and "Cmax" only with no reference to
#'   which dose it was. If left as the default NA, then the dose number will be
#'   omitted if all the data are all for dose 1 or all for the last dose, and it
#'   will be included if you have a mix of dosing intervals.
#' @param facet_title_size optionally specify what font size to use for the
#'   facet titles. If left as NA, a reasonable guess will be used.
#' @param title_adjustments a character vector of text adjustments for the
#'   title. Possible options:
#'
#'   \describe{\item{"sub steady-state for last"}{Instead of the
#'   default PK parameters for the last dose being labeled as, e.g.,
#'   "Last dose Cmax", this will use "steady-state" instead, e.g.,
#'   "Steady-state Cmax"}
#'
#'   \item{sub 0 to inf for inf}{NOT SET UP YET. This is a placeholder for other
#'   substitutions people might want. Instead of the using AUCinf, graph titles will
#'   use AUC0 to inf}}
#'
#'
#' @return a set of arranged ggplot2 graphs
#' @export
#'
#' @examples
#' # Assuming you have run pksummary_mult on a few files with observed data
#' # to generate an object called MyPKOutput, then:
#' so_graph(PKtable = SOdata)
#' so_graph(PKtable = SOdata, boundary_indicator = "fill")
#' so_graph(PKtable = SOdata,
#'          axis_titles = c("y" = "Predicted", "x" = "Observed"))
#' so_graph(PKtable = SOdata, ncol = 1)
#' so_graph(PKtable = SOdata, point_shape_column = Study,
#'          legend_position = "bottom")
#' so_graph(PKtable = SOdata,
#'          point_shape_column = Study,
#'          legend_label_point_shape = "Studies involving\nDrug X",
#'          point_color_column = File,
#'          legend_label_point_color = "Simulation file",
#'          legend_position = "right")
#'
#' 

so_graph <- function(PKtable, 
                     PKparameters = NA, 
                     PKorder = "default", 
                     boundary_indicator = "lines",
                     boundaries = c(1, 1.5, 2),
                     boundaries_Guest = c(1, 2),
                     boundary_color_set = "red black", 
                     boundary_color_set_Guest = "red black", 
                     boundary_line_types = "default",
                     boundary_line_types_Guest = "default",
                     boundary_line_width = 0.7, 
                     point_color_column, 
                     point_color_set = "default",
                     legend_label_point_color = NA, 
                     point_shape_column, 
                     point_shape = NA,
                     point_size = NA,
                     legend_label_point_shape = NA, 
                     legend_position = "none",
                     axis_title_x = "Observed",
                     axis_title_y = "Simulated", 
                     include_dose_num = NA,
                     facet_title_size = NA, 
                     title_adjustments = c(), 
                     all_intervals_together = FALSE, 
                     ncol = NULL, 
                     nrow = NULL,
                     save_graph = NA, 
                     fig_width = 8, 
                     fig_height = 6, 
                     axis_titles = NA){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if("list" %in% class(PKtable)){
      PKtable <- PKtable$Table
   }
   
   if(any(complete.cases(axis_titles))){
      warning("YOu have specified something for the argument `axis_titles`, which we're deprecating and splitting into `axis_title_x` and `axis_title_y`. Please note that this argument may not work in the future.\n", 
              call. = FALSE)
      names(axis_titles) <- tolower(names(axis_titles))
      
      if(all(c("x", "y") %in% names(axis_titles)) == FALSE){
         warning("It is not clear what you want for the x axis title and what you want for the y. Please check the help file for the arguments `axis_title_x` and `axis_title_y`. For now, we'll use the default values.", 
                 call. = FALSE)
         axis_title_x <- "Observed"
         axis_title_y <- "Simulated"
      } else {
         axis_title_x <- axis_titles["x"]
         axis_title_y <- axis_titles["y"]
      }
   }
   
   if(is.na(axis_title_x)){
      warning("You must specify a value for `axis_title_x`; it can't be NA. We'll use the default of `Observed`.\n", 
              call. = FALSE)
      axis_title_x <- "Observed"
   }
   
   if(is.na(axis_title_y)){
      warning("You must specify a value for `axis_title_y`; it can't be NA. We'll use the default of `Simulated`.\n", 
              call. = FALSE)
      axis_title_y <- "Simulated"
   }
   
   
   if("Observed" %in% PKtable$Statistic == FALSE){
      stop("We can't find the observed data in the table provided for `PKtable`, so we can't make a simulated-versus-observed graph.", 
           call. = FALSE)
   }
   
   BoundIndOptions <- c("fill"= str_detect(tolower(boundary_indicator), "fil"), 
                        "lines" = str_detect(tolower(boundary_indicator), "line"), 
                        "none" = str_detect(tolower(boundary_indicator), "none"))
   boundary_indicator <- names(BoundIndOptions)[BoundIndOptions]
   
   if(length(boundary_indicator) != 1){
      warning("There's something wrong with your input for `boundary_indicator`, so we'll set this to the default value of `lines`.", 
              call. = FALSE)
      boundary_indicator <- "lines"
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
      is.matrix(col2rgb(boundary_color_set)) == FALSE){
      warning("The values you used for boundary colors are not all valid colors in R. We'll used the default colors instead.\n", 
              call. = FALSE)
      boundary_color_set <- "red black"
   } 
   
   if(length(boundaries_Guest) != length(boundary_color_set_Guest) &
      boundary_color_set_Guest[1] %in% c("red green", "red black", "black", "muted red green") == FALSE){
      warning("You have specified one number of boundaries for DDI graphs and a different number of colors for those boundaries, so we don't know what colors you want. We'll use the default BoundariesGuest boundary line colors.\n", 
              call. = FALSE)
      boundary_color_set_Guest <- "red black"
   }
   
   if(boundary_color_set_Guest[1] %in% c("red green", "muted red green", 
                                         "red black") == FALSE && 
      is.matrix(col2rgb(boundary_color_set_Guest)) == FALSE){
      warning("The values you used for BoundariesGuest boundary colors are not all valid colors in R. We'll used the default colors instead.\n", 
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
   
   
   # Main body of function --------------------------------------------------
   
   ## Setting colors & linetypes -----------------------------------------------
   
   if(boundary_color_set[1] %in% c("red green", "red black", 
                                   "muted red green") &
      boundary_indicator != "none"){
      
      boundary_color_set <- 
         set_boundary_colors(color_set = boundary_color_set,
                             boundaries = boundaries, 
                             break_type = "SO")
      
   }
   
   if(boundary_color_set_Guest[1] %in% c("red green", "red black", 
                                         "muted red green") &
      boundary_indicator != "none"){
      
      boundary_color_set_Guest <- 
         set_boundary_colors(color_set = boundary_color_set_Guest,
                             boundaries = c(boundaries_Guest, 
                                            # repeat the last boundary b/c we
                                            # have curved and straight lines for
                                            # that one.
                                            boundaries_Guest[length(boundaries_Guest)]), 
                             break_type = "SO")
      
   } else {
      # Making sure we have enough colors
      boundary_color_set <- rep(boundary_color_set, length(boundaries))
      boundary_color_set <- boundary_color_set[1:length(boundaries)]
      
      boundary_color_set_Guest <- rep(boundary_color_set_Guest, length(boundaries_Guest) + 1)
      # NB: BoundariesGuest boundaries require an extra color b/c boundary for
      # highest-fold error has both curved and straight lines.
      boundary_color_set_Guest <- boundary_color_set_Guest[1:(length(boundaries_Guest) + 1)]
   }
   
   # Making sure we have enough linetypes
   if(boundary_line_types[1] == "default"){
      boundary_line_types_straight <- c("dashed", 
                                        rep("solid", length(boundaries) - 1))
      boundary_line_types_Guest <- c("dashed", 
                                     rep("solid", length(boundaries_Guest)))
      
   } else {
      boundary_line_types_straight <- rep(boundary_line_types,
                                          length(boundaries))
      boundary_line_types_Guest <- rep(boundary_line_types_Guest,
                                       length(boundaries_Guest) + 1)
   }
   
   boundary_line_types_straight <- boundary_line_types_straight[1:length(boundaries)]
   # NB: BoundariesGuest boundaries require an extra linetype b/c boundary for
   # highest-fold error has both curved and straight lines.
   boundary_line_types_Guest <- boundary_line_types_Guest[1:(length(boundaries_Guest) + 1)]
   
   
   ## Getting data arranged  ------------------------------------------------
   
   # Noting user's original preferences for a few things
   point_color_set_user <- point_color_set
   point_shape_user <- point_shape
   point_size_user <- point_size
   
   # Setting up data for boundaries on graphs
   Boundaries_num <- boundaries
   Boundaries_num_Guest <- boundaries_Guest
   Boundaries <- list()
   BoundariesGuest <- list()
   GuestStraight <- list()
   Poly <- list()
   PolyGuest <- list()
   
   # Regular boundaries
   for(j in Boundaries_num){
      
      Boundaries[[as.character(j)]] <-
         list("Upper" = data.frame(Observed = 10^seq(-4, 9, length.out = 100)) %>% 
                 mutate(LimitName = "upper", 
                        Simulated = Observed * j), 
              "Lower" = data.frame(Observed = 10^seq(-4, 9, length.out = 100)) %>% 
                 mutate(LimitName = "upper", 
                        Simulated = Observed / j))
   }
   
   # BoundariesGuest 
   for(j in Boundaries_num_Guest){
      BoundariesGuest[[as.character(j)]] <- 
         list("Upper" = data.frame(Observed = 10^seq(-4, 9, length.out = 1000)) %>% 
                 mutate(Limit = ifelse(Observed >= 1, 
                                       (1 + j*(Observed - 1))/Observed,
                                       (1 + j*((1/Observed) - 1))/(1/Observed)),
                        LimitName = "upper",
                        Simulated = Observed * Limit),
              "Lower" = data.frame(Observed = 10^seq(-4, 9, length.out = 1000)) %>% 
                 mutate(Limit = ifelse(Observed >= 1, 
                                       (1 + j*(Observed - 1))/Observed,
                                       (1 + j*((1/Observed) - 1))/(1/Observed)),
                        LimitName = "lower", 
                        Simulated = Observed / Limit))
      GuestStraight[[as.character(j)]] <- 
         list("Upper" = data.frame(Observed = 10^seq(-4, 9, length.out = 100)) %>% 
                 mutate(LimitName = "upper", 
                        Simulated = Observed * j), 
              "Lower" = data.frame(Observed = 10^seq(-4, 9, length.out = 100)) %>% 
                 mutate(LimitName = "upper", 
                        Simulated = Observed / j))
   }
   
   # Setting up polygons -- This needs to be done a bit differently b/c we'll
   # need to combine each boundary w/the one before it except for the smallest
   # boundary.
   
   Poly[["1"]] <- Boundaries[["1"]][["Upper"]] %>% 
      arrange(Observed) %>% 
      bind_rows(Boundaries[["1"]][["Lower"]] %>% 
                   arrange(desc(Observed)))
   
   PolyGuest[["1"]] <- 
      BoundariesGuest[["1"]][["Upper"]] %>% 
      arrange(Observed) %>% 
      bind_rows(BoundariesGuest[["1"]][["Lower"]] %>% 
                   arrange(desc(Observed)))
   
   for(j_index in 2:length(Boundaries_num)){ # <--- Note that this is by index and not name!!!!
      Poly[[as.character(Boundaries_num[j_index]) ]] <- 
         Boundaries[[as.character(Boundaries_num[j_index])]][["Upper"]] %>%
         arrange(Observed) %>%
         bind_rows(Boundaries[[as.character(Boundaries_num[j_index-1])]][["Upper"]] %>%
                      arrange(desc(Observed))) %>%
         bind_rows(Boundaries[[as.character(Boundaries_num[j_index-1])]][["Lower"]] %>%
                      arrange(Observed)) %>%
         bind_rows(Boundaries[[as.character(Boundaries_num[j_index])]][["Lower"]] %>% 
                      arrange(desc(Observed)))
      
   }
   
   for(j_index in 2:length(Boundaries_num_Guest)){ # <--- Note that this is by index and not name!!!!
      
      PolyGuest[[as.character(Boundaries_num_Guest[j_index])]] <- 
         BoundariesGuest[[as.character(Boundaries_num_Guest[j_index])]][["Upper"]] %>%
         arrange(Observed) %>%
         bind_rows(BoundariesGuest[[as.character(Boundaries_num_Guest[j_index-1])]][["Upper"]] %>%
                      arrange(desc(Observed))) %>%
         bind_rows(BoundariesGuest[[as.character(Boundaries_num_Guest[j_index-1])]][["Lower"]] %>%
                      arrange(Observed)) %>%
         bind_rows(BoundariesGuest[[as.character(Boundaries_num_Guest[j_index])]][["Lower"]] %>% 
                      arrange(desc(Observed)))
      
      if(j_index == length(Boundaries_num_Guest)){
         PolyGuest[[j_index + 1]] <- 
            GuestStraight[[as.character(Boundaries_num_Guest[j_index])]][["Upper"]] %>%
            arrange(Observed) %>%
            bind_rows(BoundariesGuest[[as.character(Boundaries_num_Guest[j_index])]][["Upper"]] %>%
                         arrange(desc(Observed))) %>%
            bind_rows(BoundariesGuest[[as.character(Boundaries_num_Guest[j_index])]][["Lower"]] %>%
                         arrange(Observed)) %>%
            bind_rows(GuestStraight[[as.character(Boundaries_num_Guest[j_index])]][["Lower"]] %>% 
                         arrange(desc(Observed)))
      }
   }
   
   ## Setting things up for nonstandard evaluation - Part 1 --------------------
   point_color_column <- rlang::enquo(point_color_column)
   point_shape_column <- rlang::enquo(point_shape_column)
   
   # Only including sim and obs data.
   PKtable <- PKtable %>% filter(Statistic %in% c("Simulated", "Observed"))
   
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
   
   # Making everything use ng/mL units b/c that matches the units in
   # PKexpressions. Otherwise, the mini graph titles will be incorrect.
   PKAUCUnits <- setdiff(PKAUCUnits, "ng/mL.h")
   PKCmaxUnits <- setdiff(PKCmaxUnits, "ng/mL")
   
   if(length(c(PKAUCUnits, PKCmaxUnits)) > 0){
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
   PKCols <- data.frame(Orig = names(PKtable)) %>% 
      mutate(Pretty = prettify_column_names(Orig), 
             Ugly = prettify_column_names(Orig, pretty_or_ugly_cols = "ugly"))
   
   # Find all the parameters that were for a user-defined AUC interval and
   # adjust those.
   WhichUserInt <- which(str_detect(PKCols$Orig, " for interval from"))
   UserInt <- PKCols$Orig[WhichUserInt]
   
   # Can't just change the name of each user int column b/c that column name may
   # already exist. Need to remove and then join w/original PK table, keeping
   # all necessary columns when joining.
   for(col in UserInt){
      
      ToJoin <- PKtable %>% 
         select(File, col, Statistic, 
                any_of(c("Compound", "CompoundID", "Tissue", 
                         as_label(point_color_column), 
                         as_label(point_shape_column)))) 
      ToJoin <- ToJoin[which(complete.cases(ToJoin[, col])), ]
      
      StartCh <- as.data.frame(str_locate(col, " for interval"))
      newcol <- str_sub(col, start = 1, end = StartCh$start - 1)
      
      names(ToJoin)[which(names(ToJoin) == col)] <- newcol
      
      suppressMessages(
         PKtable <- PKtable %>% 
            full_join(ToJoin) %>% select(-any_of(col))
      )
      
      rm(newcol, ToJoin)
      
   }
   
   PKCols <- PKCols %>% filter(!str_detect(Orig, " for interval"))
   PKCols$IsPK <- PKCols$Ugly %in% c(AllPKParameters$PKparameter, 
                                     AllPKParameters$PKparameter_nodosenum)
   
   if(any(is.na(PKparameters))){
      PKparameters <- PKCols$Ugly[PKCols$IsPK]
   }
   
   # Arranging and tidying input data. First, de-prettifying column names.
   SO <- PKtable %>% 
      mutate(Statistic = as.character(Statistic), 
             Statistic = ifelse(str_detect(Statistic, "^Simulated"),
                                "Simulated", Statistic))
   
   names(SO) <- PKCols$Ugly
   
   if(is.na(include_dose_num)){
      # Dropping dose number depending on input. First, checking whether they have
      # both dose 1 and last-dose data.
      DoseCheck <- c("first" = any(str_detect(PKparameters, "dose1")), 
                     "user-defined" = any(str_detect(PKparameters, "dose1|last")) == FALSE, 
                     "last" = any(str_detect(PKparameters, "last")))
      include_dose_num <- length(which(DoseCheck)) > 1
   }
   
   # include_dose_num now should be either T or F no matter what, so checking
   # that.
   if(is.logical(include_dose_num) == FALSE){
      warning("Something is amiss with your input for `include_dose_num`, which should be NA, TRUE, or FALSE. We'll assume you meant for it to be TRUE.", 
              call. = FALSE)
      include_dose_num <- TRUE
   }
   
   if(include_dose_num == FALSE){
      PKparameters <- sub("_dose1|_last", "", PKparameters)
      names(SO) <- sub("_dose1|_last", "", names(SO))
      PKCols$Ugly <- sub("_dose1|_last", "", PKCols$Ugly)
   }
   
   # Removing additional columns since they mess up pivoting. 
   SO <- SO[, PKCols %>% filter(IsPK == FALSE | Ugly %in% PKparameters) %>% 
               pull(Ugly)]
   
   suppressWarnings(
      SO <- SO %>% 
         unique() %>% 
         mutate(across(.cols = PKparameters, .fns = as.numeric)) %>% 
         pivot_longer(names_to = "PKparameter", 
                      values_to = "Value", 
                      cols = PKparameters) %>% 
         filter(complete.cases(Value)) %>% 
         pivot_wider(names_from = Statistic, values_from = Value) %>% 
         filter(complete.cases(Observed) & PKparameter %in% {{PKparameters}})
   )
   
   if(all_intervals_together){
      
      # FIXME - Just started working on this. This is NOT set up yet. 
      
      # For this option, data must be in long format w/ a column for interval.
      TEMP <- SO %>% 
         mutate(Interval = case_when(str_detect(PKparameter, "dose1") ~ "first dose", 
                                     str_detect(PKparameter, "last") ~ "last dose", 
                                     PKparameter %in% PKCols$Ugly[
                                        str_detect(PKCols$Orig, "for interval from")] ~ "user-defined interval", 
                                     TRUE ~ "applies to all intervals"), 
                PKparameter_rev = sub("_last|_dose1", "", PKparameter), 
                point_color_column = Interval, 
                point_shape_column = Interval)
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
      if("sub steady-state for last" %in% tolower(title_adjustments) |
         "sub steady state for last" %in% tolower(title_adjustments)){
         
         PKparameters <- sub("_last", "_ss", PKparameters)
         SO$PKparameter <- sub("_last", "_ss", SO$PKparameter)
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
      
      # If nothing was set for point_color_column, then set the color to
      # "black". If there are only 2 groups for the point_color_column and
      # point_color_set was set to "default", use Brewer set 1 instead of
      # Brewer set 2 b/c it's more aethetically pleasing.
      NumColorsNeeded <- length(sort(unique(SO$point_color_column)))
      
      if(length(point_color_set) == 1 &&
         point_color_set %in% c("default", "set2", "blue-green", "blues", 
                                "rainbow", "set1", "Tableau", "viridis")){
         
         if(NumColorsNeeded == 1){
            point_color_set <- switch(as.character(point_color_set == "default"),
                                      "TRUE" = "black",
                                      "FALSE" = point_color_set)
         } else if(NumColorsNeeded == 2){
            point_color_set <- switch(as.character(point_color_set == "default"),
                                      "TRUE" = "set1", 
                                      "FALSE" = point_color_set)
         } else {
            point_color_set <- switch(as.character(point_color_set == "default"),
                                      "TRUE" = "set2",
                                      "FALSE" = point_color_set)
            # There are limits to the number of colors available for Brewer
            # palettes. Checking that and setting to "rainbow" if needed.
            if((point_color_set == "set1" & NumColorsNeeded > 9) |
               (point_color_set == "set2" & NumColorsNeeded > 8) |
               (point_color_set == "Tableau" & NumColorsNeeded > 10)){
               
               warning(paste("There are", NumColorsNeeded,
                             "unique values in the column you have specified for the colors, but the color set you requested doesn't have as many possible colors as that. We'll change the color set to `rainbow` for now."), 
                       call. = FALSE)
               point_color_set <- "rainbow"
            }
         }
         
         suppressWarnings(
            MyPointColors <- 
               switch(
                  point_color_set,
                  # Using "Dark2" b/c "Set2" is just really,
                  # really light.
                  "set2" = RColorBrewer::brewer.pal(NumColorsNeeded, "Dark2")[
                     1:NumColorsNeeded], 
                  "blue-green" = blueGreens(NumColorsNeeded),
                  "blues" = blues(NumColorsNeeded),
                  "rainbow" = rainbow(NumColorsNeeded),
                  "set1" = RColorBrewer::brewer.pal(NumColorsNeeded, "Set1")[
                     1:NumColorsNeeded],
                  "Tableau" = ggthemes::tableau_color_pal(
                     palette = "Tableau 10")(NumColorsNeeded),
                  "viridis" = viridis::viridis_pal()(NumColorsNeeded))
         )   
         # NB: For the RColorBrewer palettes, the minimum number of
         # colors you can get is 3. Since sometimes we might only want 1
         # or 2 colors, though, we have to add the [1:NumColorsNeeded]
         # bit.
         
      } else {
         MyPointColors <- point_color_set
         
         if(length(MyPointColors) < NumColorsNeeded){
            warning(paste("There are", NumColorsNeeded,
                          "unique values in the column you have specified for the point colors, but you have only specified", 
                          length(MyPointColors), 
                          "colors to use. We will recycle the colors to get enough to display your data, but you probably will want to supply more colors and re-graph."), 
                    call. = FALSE)
            
            MyPointColors <- rep(point_color_set, 100)[1:NumColorsNeeded]
         }
      }
      
   } else {
      # Setting color to black if point_color_column unspecified
      point_color_set <- "black"
   }
   
   if(as_label(point_shape_column) != "<empty>"){
      SO <- SO %>% mutate(point_shape_column = {{point_shape_column}}) %>% 
         droplevels()
      
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
         MyPointShapes <- c(15:19, 8, 3:7, 9:14)[1:NumShapesNeeded]
      }
   }
   
   
   ## Making graphs ---------------------------------------------------------
   G <- list()
   SO <- split(SO, f = SO$PKparameter)
   
   for(i in names(SO)){
      
      Limits <- c(
         round_down(min(c(SO[[i]]$Observed, SO[[i]]$Simulated), na.rm = T)),
         
         round_up(max(c(SO[[i]]$Observed, SO[[i]]$Simulated), na.rm = T)))
      
      if(str_detect(i, "ratio")){
         G[[i]] <- ggplot()  +
            geom_line(data = BoundariesGuest[["1"]][["Upper"]],
                      aes(x = Observed, y = Simulated),
                      linetype = boundary_line_types_Guest[1], 
                      color = boundary_color_set_Guest[1],
                      linewidth = boundary_line_width)
         
      } else {
         G[[i]] <- ggplot()  +
            geom_line(data = Boundaries[["1"]][["Upper"]],
                      aes(x = Observed, y = Simulated),
                      linetype = boundary_line_types_straight[1],
                      color = boundary_color_set[1],
                      linewidth = boundary_line_width)
      }
      
      if(boundary_indicator == "lines"){
         if(str_detect(i, "ratio")){
            
            for(j_index in 2:length(Boundaries_num_Guest)){ # <--- Note that this is by index and not name!!!!
               G[[i]] <- G[[i]] + 
                  geom_line(data = BoundariesGuest[[j_index]][["Upper"]],
                            aes(x = Observed, y = Simulated),
                            color = boundary_color_set_Guest[j_index], 
                            linewidth = boundary_line_width, 
                            linetype = boundary_line_types_Guest[j_index]) +
                  geom_line(data = BoundariesGuest[[j_index]][["Lower"]], 
                            aes(x = Observed, y = Simulated),
                            color = boundary_color_set_Guest[j_index], 
                            linewidth = boundary_line_width, 
                            linetype = boundary_line_types_Guest[j_index])
               
               if(j_index == length(Boundaries_num_Guest)){
                  G[[i]] <- G[[i]] +
                     geom_line(data = GuestStraight[[j_index]][["Upper"]],
                               aes(x = Observed, y = Simulated),
                               color = boundary_color_set_Guest[j_index + 1], 
                               linewidth = boundary_line_width, 
                               linetype = boundary_line_types_Guest[j_index + 1]) +
                     geom_line(data = GuestStraight[[j_index]][["Lower"]],
                               aes(x = Observed, y = Simulated),
                               color = boundary_color_set_Guest[j_index + 1], 
                               linewidth = boundary_line_width, 
                               linetype = boundary_line_types_Guest[j_index + 1])
               }
            }
            
         } else {
            
            for(j_index in 2:length(Boundaries_num)){ # <--- Note that this is by index and not name!!!!
               G[[i]] <- G[[i]] + 
                  geom_line(data = Boundaries[[j_index]][["Upper"]],
                            aes(x = Observed, y = Simulated),
                            color = boundary_color_set[j_index], 
                            linewidth = boundary_line_width, 
                            linetype = boundary_line_types_straight[j_index]) +
                  geom_line(data = Boundaries[[j_index]][["Lower"]],
                            aes(x = Observed, y = Simulated),
                            color = boundary_color_set[j_index], 
                            linewidth = boundary_line_width, 
                            linetype = boundary_line_types_straight[j_index])
            }
         }
      }
      
      if(boundary_indicator == "fill"){
         if(str_detect(i, "ratio")){
            
            for(j_index in 2:length(Boundaries_num_Guest)){ # <--- Note that this is by index and not name!!!!
               G[[i]] <- G[[i]] +
                  geom_polygon(data = PolyGuest[[j_index]],
                               aes(x = Observed, y = Simulated), inherit.aes = F,
                               fill = boundary_color_set_Guest[j_index], 
                               alpha = 0.2)
               
               if(j_index == length(Boundaries_num_Guest)){
                  G[[i]] <- G[[i]] +
                     geom_polygon(data = PolyGuest[[j_index + 1]],
                                  aes(x = Observed, y = Simulated), inherit.aes = F,
                                  fill = boundary_color_set_Guest[j_index + 1], 
                                  alpha = 0.2)
               }
            }
            
         } else {
            
            for(j_index in 2:length(Boundaries_num)){ # <--- Note that this is by index and not name!!!!
               G[[i]] <- G[[i]] +
                  geom_polygon(data = Poly[[j_index]],
                               aes(x = Observed, y = Simulated), inherit.aes = F,
                               fill = boundary_color_set[j_index], alpha = 0.2) 
            }
         }
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
      
      G[[i]] <- 
         switch(paste(
            as_label(point_color_column) != "<empty>",
            as_label(point_shape_column) != "<empty>"),
            
            # User has NOT specified anything for point shape or color
            "FALSE FALSE" = G[[i]] + 
               geom_point(data = SO[[i]],
                          aes(x = Observed, y = Simulated), 
                          size = ifelse(is.na(point_size), 2, point_size)), 
            
            # User has specified a column for point color
            "TRUE FALSE" = G[[i]] +
               geom_point(data = SO[[i]],
                          aes(x = Observed, y = Simulated, 
                              color = point_color_column),
                          size = ifelse(is.na(point_size), 2, point_size)) + 
               scale_color_manual(values = MyPointColors, drop = FALSE), 
            
            # User has specified a column for point shape
            "FALSE TRUE" = 
               G[[i]] +
               geom_point(data = SO[[i]],
                          aes(x = Observed, y = Simulated, 
                              shape = point_shape_column),
                          size = ifelse(is.na(point_size), 2, point_size)) +
               scale_shape_manual(values = MyPointShapes, drop = FALSE),
            
            # User has specified both color and point shape columns
            "TRUE TRUE" = 
               G[[i]] + 
               geom_point(data = SO[[i]], aes(x = Observed, y = Simulated, 
                                              color = point_color_column, 
                                              shape = point_shape_column),
                          size = ifelse(is.na(point_size), 2, point_size)) +
               scale_color_manual(values = MyPointColors, drop = FALSE) +
               scale_shape_manual(values = MyPointShapes, drop = FALSE))
      
      if(str_detect(i, "_withInhib")){
         Gtitle <- PKexpressions[[paste0(i, "_2")]]
      } else {
         Gtitle <- PKexpressions[[i]]
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
               plot.title = element_text(hjust = 0.5, 
                                         size = ifelse(is.na(facet_title_size), 
                                                       12, facet_title_size)),
               axis.title = element_text(color = "black", face = "bold"),
               axis.title.x = element_text(margin = margin(2.75, 0, 0, 0)),
               axis.title.x.top = element_text(margin = margin(0, 0, 2.75, 0)),
               axis.title.y = element_text(margin = margin(0, 2.75, 0, 0)),
               axis.title.y.right = element_text(margin = margin(0, 0, 0, 2.75)))
      
      # Adding legend label for color and shape as appropriate
      if(as_label(point_color_column) != "<empty>"){
         if(complete.cases(legend_label_point_color)){
            if(legend_label_point_color == "none"){    
               G[[i]] <- G[[i]] + labs(color = NULL)
            } else {
               G[[i]] <- G[[i]] + labs(color = legend_label_point_color)
            }
         } else {
            # This is when no legend_label_point_color has been specified.
            G[[i]] <- G[[i]] + labs(color = as_label(point_color_column))
         }
      }
      
      if(as_label(point_shape_column) != "<empty>"){
         if(complete.cases(legend_label_point_shape)){
            if(legend_label_point_shape == "none"){    
               G[[i]] <- G[[i]] + labs(shape = NULL)
            } else {
               G[[i]] <- G[[i]] + labs(shape = legend_label_point_shape)
            }
         } else {
            # This is when no legend_label_point_shape has been specified.
            G[[i]] <- G[[i]] + labs(shape = as_label(point_shape_column))
         } 
      }
   }
   
   if(length(G) == 1){
      G <- G[[1]] + theme(legend.position = legend_position)
      
   } else {
      
      # ncol and nrow must both be specified or neither specified. Dealing with
      # that.
      NumCR <- paste(is.null(ncol), is.null(nrow))
      
      # Setting the order if user requested that. 
      
      if(PKorder == "user specified"){
         if(include_dose_num == FALSE){
            PKwithBlanks <- sub("_dose1|_last", "", PKwithBlanks)
         }
         
         for(blanks in PKwithBlanks[str_detect(PKwithBlanks, "BLANK")]){
            G[[blanks]] <- ggplot() + theme_void()
         }
         
         G <- G[PKwithBlanks]
         
      } else {
         
         GoodOrder <- AllPKParameters %>% select(PKparameter, SortOrder) %>% 
            bind_rows(AllPKParameters %>% select(PKparameter_nodosenum, SortOrder) %>% 
                         rename(PKparameter = PKparameter_nodosenum)) %>% 
            arrange(SortOrder) %>% pull(PKparameter) %>% unique()
         
         if("sub steady-state for last" %in% title_adjustments){
            GoodOrder <- sub("_last", "_ss", GoodOrder)
         }
         
         GoodOrder <- GoodOrder[GoodOrder %in% names(G)]
         
         G <- G[GoodOrder]
      }
      
      G <-  ggpubr::ggarrange(
         plotlist = G, align = "hv", 
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
                       "TRUE TRUE" = NULL),
         legend = legend_position, 
         common.legend = TRUE)
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
   
   return(G)
   
}


