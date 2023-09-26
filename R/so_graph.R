#' Graph of simulated vs. observed PK
#'
#' \code{so_graph} makes a graph of simulated vs. observed PK, including
#' indicating where the predicted parameters fell within X fold of the observed.
#'
#' @param PKtable a table in the same format as output from the function
#'   \code{\link{pksummary_mult}}
#' @param PKparameters any of the PK parameters included in the output from
#'   \code{\link{pksummary_mult}}; if left as NA, this will make graphs for each
#'   parameter included in \code{PKtable}. To see the full set of possible
#'   parameters, enter \code{view(PKParameterDefinitions)} into the console.
#' @param boundaries Numerical boundaries to show on the graph. Defaults to the
#'   1.5- and 2-fold boundaries. Indicate boundaries you want like this:
#'   \code{boundaries = c(1.25, 1.5, 2)}
#' @param boundaries_Guest Numerical boundaries to show on the graph when the PK
#'   parameter is a mean ratio of the parameter in the presence of an effector /
#'   the parameter in the absence of the effector. The default boundary for
#'   Guest curves is 2, which will show both the Guest curve and a straight
#'   line. Honestly, we recommend leaving this as 2 for clarity of the graph.
#'   For these PK parameters, if there are any values listed for
#'   \code{boundaries_Guest}, the lines will be replaced with a curve as
#'   described in [Guest Galetin 2011 Drug Metab
#'   Dispos](https://pubmed.ncbi.nlm.nih.gov/21036951/). If you'd rather show
#'   straight lines for these parameters instead of Guest curves, set this to
#'   NA.
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
#'   green", "muted red green" (a lighter, somewhat more muted red and green
#'   that work well for indicating the boundary when you're using shading
#'   instead of lines), and "black", which will result in only black lines or
#'   shading. You also can set this to any set of colors you'd like, e.g.,
#'   \code{boundary_color_set = c("yellow", "blue")}. The number of colors
#'   should equal the number of boundaries that you've indicated or the graph
#'   won't be easily interpretable.
#' @param boundary_line_types optionally specify the line types to use for the
#'   boundaries (only applicable when \code{boundary_indicator = "lines"});
#'   leaving this as "default" results in a dashed line at unity and solid lines
#'   for all others, but you can specify this with any R-acceptable line types,
#'   e.g., \code{boundary_line_types = c("dotted", "dashed", "solid")}. To see
#'   the possibilities, type \code{ggpubr::show_line_types()} into the console.
#' @param boundary_line_width line width; default is 0.7. This only applies when
#'   \code{boundary_indicator} is set to "lines", the default.
#' @param axis_titles optionally specify what you'd like for the x and y axis
#'   titles with a named character vector. The default is 
#'   \code{axis_titles = c("x" = "Observed", "y" = "Simulated")} 
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
                     boundaries = c(1.5, 2),
                     boundaries_Guest = 2,
                     boundary_indicator = "lines",
                     boundary_color_set = "red black", 
                     boundary_line_types = "default",
                     boundary_line_width = 0.7, 
                     axis_titles = c("y" = "Simulated", "x" = "Observed"),
                     point_color_column, 
                     point_color_set = "default",
                     legend_label_point_color = NA, 
                     point_shape_column, 
                     point_shape = NA,
                     point_size = NA,
                     legend_label_point_shape = NA, 
                     legend_position = "none",
                     ncol = NULL, 
                     nrow = NULL,
                     save_graph = NA, 
                     fig_width = 8, 
                     fig_height = 6){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if("list" %in% class(PKtable)){
      PKtable <- PKtable$Table
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
   
   # Checking color input
   boundary_color_set <- tolower(boundary_color_set)
   
   if(length(boundaries) != length(boundary_color_set) &
      boundary_color_set[1] %in% c("red green", "red black", "black", "muted red green") == FALSE){
      warning("You have specified one number of colors for highlighting S/O values and a different number of cutoff values, so we don't know what colors you want. We'll use the default colors for highlighting.", 
              call. = FALSE)
      boundary_color_set <- "red black"
   }
   
   if(boundary_color_set[1] %in% c("red green", "muted red green", 
                                   "red black") == FALSE && 
      is.matrix(col2rgb(boundary_color_set)) == FALSE){
      warning("The values you used for boundary colors are not all valid colors in R. We'll used the default colors instead.", 
              call. = FALSE)
      boundary_color_set <- "red black"
   } 
   
   boundary_color_set_orig <- boundary_color_set
   
   if(boundary_color_set[1] %in% c("red green", "red black", 
                                   "muted red green") &
      boundary_indicator != "none"){
      
      ColorChoices <- paste(
         boundary_color_set, boundary_indicator,
         cut(length(boundaries), breaks = c(0:3, Inf)))
      
      boundary_color_set <- 
         switch(ColorChoices, 
                ## red black lines
                
                # 1 boundary, e.g., it's only unity
                "red black lines (0,1]" = "black", 
                
                # 2 boundaries
                "red black lines (1,2]" = c("black", "black"), 
                
                # 3 boundaries
                "red black lines (2,3]" = c("black", "black", "red"), 
                
                # >3 boundaries
                "red black lines (3,Inf]" = colorRampPalette(c("black", "red"))(
                   length(boundaries)), 
                
                
                ## red black fill
                
                # 1 boundary, e.g., it's only unity
                "red black fill (0,1]" = "black", 
                
                # 2 boundaries
                "red black fill (1,2]" = c("black", "black"), 
                
                # 3 boundaries
                "red black fill (2,3]" = c("black", "black", "red"), 
                
                # >3 boundaries
                "red black fill (3,Inf]" = c("black", 
                                             colorRampPalette(c("black", "#FFC000", "red"))(
                                                length(boundaries) - 1)), 
                
                
                ## red green lines
                # 1 boundary, e.g., it's only unity
                "red green lines (0,1]" = "#17A142", 
                
                # 2 boundaries
                "red green lines (1,2]" = c("#17A142", "#17A142"), 
                
                # 3 boundaries
                "red green lines (2,3]" = c("#17A142", "#17A142", "red"), 
                
                # >3 boundaries
                "red green lines (3,Inf]" = c("#17A142", 
                                              colorRampPalette(c("#17A142", "red"))(
                                                 length(boundaries) - 1)), 
                
                
                ## red green fill
                # 1 boundary, e.g., it's only unity
                "red green fill (0,1]" = "#17A142", 
                
                # 2 boundaries
                "red green fill (1,2]" = c("#17A142", "#17A142"), 
                
                # 3 boundaries
                "red green fill (2,3]" = c("#17A142", "#17A142", "red"), 
                
                # >3 boundaries
                "red green fill (3,Inf]" = c("#17A142", 
                                             colorRampPalette(c("#17A142", "red"))(
                                                length(boundaries) - 1)), 
                
                
                ## muted red green lines
                # 1 boundary, e.g., it's only unity
                "muted red green lines (0,1]" = "#A4E4AF", 
                
                # 2 boundaries
                "muted red green lines (1,2]" = c("#A4E4AF", "#A4E4AF"), 
                
                # 3 boundaries
                "muted red green lines (2,3]" = c("#A4E4AF", "#A4E4AF", "#E6A2A2"), 
                
                # >3 boundaries
                "muted red green lines (3,Inf]" = c("#A4E4AF", 
                                                    colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                                                       length(boundaries)-1)), 
                
                
                ## muted red green fill
                # 1 boundary, e.g., it's only unity
                "muted red green fill (0,1]" = "#A4E4AF", 
                
                # 2 boundaries
                "muted red green fill (1,2]" = c("#A4E4AF", "#A4E4AF"), 
                
                # 3 boundaries
                "muted red green fill (2,3]" = c("#A4E4AF", "#A4E4AF", "#E6A2A2"), 
                
                # >3 boundaries
                "muted red green fill (3,Inf]" = c("#A4E4AF", 
                                                   colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                                                      length(boundaries)-1))
         )
      
      if(any(complete.cases(boundaries_Guest))){
         ColorChoicesGuest <- paste(
            boundary_color_set_orig, 
            cut(length(boundaries_Guest), breaks = c(0:3, Inf)))
         
         boundary_color_set_guest <- 
            switch(ColorChoicesGuest,
                   ## red black -- Need 1 extra color for b/c last
                   ## color will be for the straight line
                   
                   # 1 boundary, e.g., it's only unity
                   "red black (0,1]" = c("black", "black"),
                   
                   # 2 boundaries
                   "red black (1,2]" = c("black", "black", "red"), 
                   
                   # 3 boundaries
                   "red black (2,3]" = c("black", "black", "#FFC000", "red"), 
                   
                   # >3 boundaries
                   "red black (3,Inf]" = c("black", 
                                           colorRampPalette(c("black", "#FFC000", "red"))(
                                              length(boundaries))), 
                   
                   ## red green -- Need 1 extra color for b/c last
                   ## color will be for the straight line 
                   # 1 boundary, e.g., it's only unity
                   "red green (0,1]" = c("#17A142", "#17A142"), 
                   
                   # 2 boundaries
                   "red green (1,2]" = c("#17A142", "#17A142", "red"), 
                   
                   # 3 boundaries
                   "red green (2,3]" = c("#17A142", 
                                         colorRampPalette(c("#17A142", "red"))(
                                            length(boundaries))),
                   
                   # >3 boundaries
                   "red green (3,Inf]" = c("#17A142", 
                                           colorRampPalette(c("#17A142", "red"))(
                                              length(boundaries))), 
                   
                   ## muted red green -- Need 1 extra color for b/c last
                   ## color will be for the straight line 
                   # 1 boundary, e.g., it's only unity
                   "muted red green (0,1]" = c("#A4E4AF", "#A4E4AF"),
                   
                   # 2 boundaries
                   "muted red green (1,2]" = c("#A4E4AF", "#A4E4AF", "#E6A2A2"), 
                   
                   # 3 boundaries
                   "muted red green (2,3]" = c("#A4E4AF", "#A4E4AF", "#FFFF95", "#E6A2A2"), 
                   
                   # >3 boundaries
                   "muted red green (3,Inf]" = c("#A4E4AF", 
                                                 colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                                                    length(boundaries)))
            )
      }
      
   } else {
      boundary_color_set <- rep(boundary_color_set, length(boundaries))
      boundary_color_set_guest <- rep(boundary_color_set, length(boundaries_Guest) + 1)
   }
   
   # Will need to figure out what PK parameters are and will need deprettified
   # names when reshaping and organizing data here and lower in function
   AllPKParameters_pretty <- AllPKParameters %>%
      filter(!PKparameter == "CLt_dose1") %>% 
      select(PrettifiedNames, PKparameter) %>% unique()
   
   AllPKParameters_pretty <- bind_rows(
      AllPKParameters_pretty, 
      AllPKParameters_pretty %>% 
         mutate(PrettifiedNames = sub("(for )?[Dd]ose 1 |Last dose ", "", PrettifiedNames), 
                PKparameter = sub("_dose1|_last", "", PKparameter)))
   
   if(any(is.na(PKparameters))){
      PKparameters <- names(PKtable)
      
      # Need to get the un-prettified names here. First, check whether they're
      # pretty or R friendly.
      if(any(PKparameters %in% AllPKParameters$PKparameter)){
         PKparameters <- PKparameters[PKparameters %in% AllPKParameters$PKparameter]
      } else {
         PKparameters <- data.frame(PrettifiedNames = PKparameters) %>% 
            left_join(AllPKParameters_pretty, by = join_by(PrettifiedNames)) %>% 
            filter(complete.cases(PKparameter)) %>% 
            pull(PKparameter)
      }
   }
   
   if(boundary_line_types[1] == "default"){
      boundary_line_types_straight <- c("dashed", 
                                        rep("solid", length(boundaries) - 1))
      boundary_line_types_guest <- c("dashed", 
                                     rep("solid", length(boundaries_Guest) - 1))
   } else {
      boundary_line_types_straight <- rep(boundary_line_types,
                                          length(boundaries))
      boundary_line_types_guest <- rep(boundary_line_types,
                                       length(boundaries_Guest))
   }
   
   names(axis_titles) <- tolower(names(axis_titles))
   if(all(c("x", "y") %in% names(axis_titles)) == FALSE){
      warning("It is not clear what you want for the x axis title and what you want for the y. Please check the help file for the argument `axis_titles`. For now, we'll use the default values.", 
              call. = FALSE)
      axis_titles <- c("y" = "Simulated", "x" = "Observed")
   }
   
   # Main body of function --------------------------------------------------
   
   ## Getting data arranged  ------------------------------------------------
   
   # Noting user's original preferences for a few things
   point_color_set_user <- point_color_set
   point_shape_user <- point_shape
   point_size_user <- point_size
   
   # Setting up data for boundaries on graphs
   Boundaries_num <- boundaries
   Boundaries_num_guest <- boundaries_Guest
   Boundaries <- list()
   Guest <- list()
   GuestStraight <- list()
   Poly <- list()
   PolyGuest <- list()
   
   # Regular boundaries
   for(j in Boundaries_num){
      
      Boundaries[[as.character(j)]] <-
         list("Upper" = data.frame(Observed = 10^seq(-4, 6, length.out = 100)) %>% 
                 mutate(LimitName = "upper", 
                        Simulated = Observed * j), 
              "Lower" = data.frame(Observed = 10^seq(-4, 6, length.out = 100)) %>% 
                 mutate(LimitName = "upper", 
                        Simulated = Observed / j))
   }
   
   # Guest boundaries
   for(j in Boundaries_num_guest){
      Guest[[as.character(j)]] <- 
         list("Upper" = data.frame(Observed = 10^seq(-4, 6, length.out = 1000)) %>% 
                 mutate(Limit = ifelse(Observed >= 1, 
                                       (1 + j*(Observed - 1))/Observed,
                                       (1 + j*((1/Observed) - 1))/(1/Observed)),
                        LimitName = "upper",
                        Simulated = Observed * Limit),
              "Lower" = data.frame(Observed = 10^seq(-4, 6, length.out = 1000)) %>% 
                 mutate(Limit = ifelse(Observed >= 1, 
                                       (1 + j*(Observed - 1))/Observed,
                                       (1 + j*((1/Observed) - 1))/(1/Observed)),
                        LimitName = "lower", 
                        Simulated = Observed / Limit))
      GuestStraight[[as.character(j)]] <- 
         list("Upper" = data.frame(Observed = 10^seq(-4, 6, length.out = 100)) %>% 
                 mutate(LimitName = "upper", 
                        Simulated = Observed * j), 
              "Lower" = data.frame(Observed = 10^seq(-4, 6, length.out = 100)) %>% 
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
      Guest[["1"]][["Upper"]] %>% 
      arrange(Observed) %>% 
      bind_rows(Guest[["1"]][["Lower"]] %>% 
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
   
   for(j_index in 2:length(Boundaries_num_guest)){ # <--- Note that this is by index and not name!!!!
      
      PolyGuest[[as.character(Boundaries_num_guest[j_index])]] <- 
         Guest[[as.character(Boundaries_num_guest[j_index])]][["Upper"]] %>%
         arrange(Observed) %>%
         bind_rows(Guest[[as.character(Boundaries_num_guest[j_index-1])]][["Upper"]] %>%
                      arrange(desc(Observed))) %>%
         bind_rows(Guest[[as.character(Boundaries_num_guest[j_index-1])]][["Lower"]] %>%
                      arrange(Observed)) %>%
         bind_rows(Guest[[as.character(Boundaries_num_guest[j_index])]][["Lower"]] %>% 
                      arrange(desc(Observed)))
      
      if(j_index == length(Boundaries_num_guest)){
         PolyGuest[[j_index + 1]] <- 
            GuestStraight[[as.character(Boundaries_num_guest[j_index])]][["Upper"]] %>%
            arrange(Observed) %>%
            bind_rows(Guest[[as.character(Boundaries_num_guest[j_index])]][["Upper"]] %>%
                         arrange(desc(Observed))) %>%
            bind_rows(Guest[[as.character(Boundaries_num_guest[j_index])]][["Lower"]] %>%
                         arrange(Observed)) %>%
            bind_rows(GuestStraight[[as.character(Boundaries_num_guest[j_index])]][["Lower"]] %>% 
                         arrange(desc(Observed)))
      }
   }
   
   # Arranging and tidying input data. First, de-prettifying column names.
   SO <- PKtable %>% 
      mutate(Statistic = as.character(Statistic), 
             Statistic = ifelse(str_detect(Statistic, "^Simulated"),
                                "Simulated", Statistic))
   
   if(any(names(PKtable) %in% AllPKParameters$PKparameter)){
      PKnames <- data.frame(PKparameter = PKparameters) %>% 
         left_join(AllPKParameters_pretty, by = "PKparameter") %>% 
         mutate(NewName = PKparameter)
   } else {
      PKnames <- data.frame(OrigName = names(PKtable)) %>% 
         mutate(PrettifiedNames = sub("with .* ", "with effector ", OrigName)) %>% 
         left_join(AllPKParameters_pretty %>% unique(),  by = join_by(PrettifiedNames)) %>% 
         mutate(NewName = ifelse(is.na(PKparameter), OrigName, PKparameter))
      
      names(SO) <- PKnames$NewName
   }
   
   suppressWarnings(
      SO <- SO %>% 
         filter(Statistic %in% c("Simulated", "Observed")) %>%
         unique() %>% 
         pivot_longer(names_to = "PKparameter", 
                      values_to = "Value", 
                      cols = c(PKnames$PKparameter[complete.cases(PKnames$PKparameter)])) %>% 
         mutate(Value = as.numeric(Value)) %>% 
         filter(complete.cases(Value)) %>% 
         pivot_wider(names_from = Statistic, values_from = Value) %>% 
         filter(complete.cases(Observed) & PKparameter %in% {{PKparameters}})
   )
   
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
   
   
   ## Setting things up for nonstandard evaluation --------------------------
   point_color_column <- rlang::enquo(point_color_column)
   point_shape_column <- rlang::enquo(point_shape_column)
   
   if(as_label(point_color_column) != "<empty>"){
      SO <- SO %>% mutate(point_color_column = {{point_color_column}})
      
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
            MyColors <- 
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
         MyColors <- point_color_set
         
         if(length(MyColors) < NumColorsNeeded){
            warning(paste("There are", NumColorsNeeded,
                          "unique values in the column you have specified for the colors, but you have only specified", 
                          length(MyColors), 
                          "colors to use. We will recycle the colors to get enough to display your data, but you probably will want to supply more colors and re-graph."), 
                    call. = FALSE)
            
            MyColors <- rep(point_color_set, 100)[1:NumColorsNeeded]
         }
      }
      
   } else {
      # Setting color to black if point_color_column unspecified
      point__color_set <- "black"
   }
   
   if(as_label(point_shape_column) != "<empty>"){
      SO <- SO %>% mutate(point_shape_column = {{point_shape_column}})
      
      if(class(SO$point_shape_column) != "factor"){
         Levels <- sort(unique(SO$point_shape_column))
         SO <- SO %>% 
            mutate(point_shape_column = factor(point_shape_column, levels = Levels))
      }
      
      NumShapesNeeded <- length(sort(unique(SO$point_shape_column)))
      
      if(any(complete.cases(point_shape))){
         if(NumShapesNeeded == 1){
            MyShapes <- point_shape[1]
         } else {
            if(length(point_shape) < NumShapesNeeded){
               # This odd syntax will work both when point_shape is a single value
               # and when it is multiple values.
               MyShapes <- rep(point_shape, NumShapesNeeded)[1:NumShapesNeeded] 
            } else if(length(point_shape) >= NumShapesNeeded){
               MyShapes <- point_shape[1:NumShapesNeeded] 
            }
         }
      } else {
         MyShapes <- c(15:19, 8, 3:7, 9:14)[1:NumShapesNeeded]
      }
   }
   
   
   ## Making graphs ---------------------------------------------------------
   G <- list()
   SO <- split(SO, f = SO$PKparameter)
   
   for(i in names(SO)){
      
      Limits <- c(
         round_down(min(c(SO[[i]]$Observed, SO[[i]]$Simulated), na.rm = T)),
         
         round_up(max(c(SO[[i]]$Observed, SO[[i]]$Simulated), na.rm = T)))
      
      G[[i]] <- ggplot()  +
         geom_line(data = Boundaries[["1"]][["Upper"]],
                   aes(x = Observed, y = Simulated),
                   linetype = boundary_line_types_straight[1],
                   color = "black",
                   linewidth = boundary_line_width)
      
      if(boundary_indicator == "lines"){
         if(str_detect(i, "ratio")){
            
            for(j_index in 2:length(Boundaries_num_guest)){ # <--- Note that this is by index and not name!!!!
               G[[i]] <- G[[i]] + 
                  geom_line(data = Guest[[j_index]][["Upper"]],
                            aes(x = Observed, y = Simulated),
                            color = boundary_color_set_guest[j_index], 
                            linewidth = boundary_line_width, 
                            linetype = boundary_line_types_guest[j_index]) +
                  geom_line(data = Guest[[j_index]][["Lower"]], 
                            aes(x = Observed, y = Simulated),
                            color = boundary_color_set_guest[j_index], 
                            linewidth = boundary_line_width, 
                            linetype = boundary_line_types_guest[j_index])
               
               if(j_index == length(Boundaries_num_guest)){
                  G[[i]] <- G[[i]] +
                     geom_line(data = GuestStraight[[j_index]][["Upper"]],
                               aes(x = Observed, y = Simulated),
                               color = boundary_color_set_guest[j_index + 1], 
                               linewidth = boundary_line_width, 
                               linetype = boundary_line_types_guest[j_index]) +
                     geom_line(data = GuestStraight[[j_index]][["Lower"]],
                               aes(x = Observed, y = Simulated),
                               color = boundary_color_set_guest[j_index + 1], 
                               linewidth = boundary_line_width, 
                               linetype = boundary_line_types_guest[j_index])
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
            
            for(j_index in 2:length(Boundaries_num_guest)){ # <--- Note that this is by index and not name!!!!
               G[[i]] <- G[[i]] +
                  geom_polygon(data = PolyGuest[[j_index]],
                               aes(x = Observed, y = Simulated), inherit.aes = F,
                               fill = boundary_color_set_guest[j_index], 
                               alpha = 0.2)
               
               if(j_index == length(Boundaries_num_guest)){
                  G[[i]] <- G[[i]] +
                     geom_polygon(data = PolyGuest[[j_index + 1]],
                                  aes(x = Observed, y = Simulated), inherit.aes = F,
                                  fill = boundary_color_set_guest[j_index + 1], 
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
      
      PossBreaks <- sort(c(10^(-3:6),
                           3*10^(-3:6),
                           5*10^(-3:6)))
      PossBreaks <- PossBreaks[PossBreaks >= Limits[1] &
                                  PossBreaks <= Limits[2]]
      
      if(length(PossBreaks) >= 5){
         MajBreaks <- 10^(-3:6)
         MinBreaks <- rep(1:9)*rep(10^(-3:6), each = 9)
      } else {
         MajBreaks <- c(10^(-3:6),
                        3*10^(-3:6),
                        5*10^(-3:6))
         MinBreaks <- rep(1:9)*rep(10^(-3:6), each = 9)
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
               scale_color_manual(values = MyColors, drop = FALSE), 
            
            # User has specified a column for point shape
            "FALSE TRUE" = 
               G[[i]] +
               geom_point(data = SO[[i]],
                          aes(x = Observed, y = Simulated, 
                              shape = point_shape_column),
                          size = ifelse(is.na(point_size), 2, point_size)) +
               scale_shape_manual(values = MyShapes, drop = FALSE),
            
            # User has specified both color and point shape columns
            "TRUE TRUE" = 
               G[[i]] + 
               geom_point(data = SO[[i]], aes(x = Observed, y = Simulated, 
                                              color = point_color_column, 
                                              shape = point_shape_column),
                          size = ifelse(is.na(point_size), 2, point_size)) +
               scale_color_manual(values = MyColors, drop = FALSE) +
               scale_shape_manual(values = MyShapes, drop = FALSE))
      
      G[[i]] <- G[[i]] + 
         xlab(axis_titles["x"]) +
         ylab(axis_titles["y"]) +
         scale_y_log10(breaks = MajBreaks, 
                       minor_breaks = MinBreaks, 
                       labels = scales::label_comma(MajBreaks)) +
         scale_x_log10(breaks = MajBreaks, 
                       minor_breaks = MinBreaks, 
                       labels = scales::label_comma(MajBreaks)) +
         coord_cartesian(xlim = Limits, ylim = Limits) + # this causes the shading to disappear for Guest curves. no idea why, but I think it's a bug w/coord_cartesian.
         ggtitle(PKexpressions[[i]]) +
         theme_bw() +
         theme(aspect.ratio = 1, 
               plot.title = element_text(hjust = 0.5),
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
      G <- G[[1]] + theme(legend.position = "none")
      
   } else {
      
      # ncol and nrow must both be specified or neither specified. Dealing with
      # that.
      NumCR <- paste(is.null(ncol), is.null(nrow))
      
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


