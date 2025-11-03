#' Make dissolution safe-space plots
#'
#' @description \code{VBE_safe_space_plot} makes dissolution safe-space plots
#'   comparing predicted dissolution values to observed and includes shading to
#'   indicate the safe space for bioequivalence.
#'
#'
#' @param VBE_dataframe a data.frame of virtual bioequivalence dissolution data
#'   with the following columns:
#'   \describe{
#'   \item{Type}{the type of formulation being tested. In the graph, you can
#'   change the color of the lines based on the values in the column "Type".}
#'   \item{SorO}{Are the data simulated or observed? The values in this column
#'   can be anything you prefer as long as there are only two of them, e.g.,
#'   "simulated" and "observed" or "hypothetical" and "actual". In the graph,
#'   you can change the point shape and the line type based on the values in
#'   the column "SorO".}
#'   \item{Time}{the time in hours}
#'   \item{Dissolution}{the percent dissolution. This should be specified as,
#'   e.g., 0.5 for 50\%.}
#'   \item{Limit}{This column will be used for specifying which data sets are
#'   the upper and lower limits of the safe space. Data sets that do \emph{not}
#'   describe the upper or lower limits should have NA in this column or be blank in the .csv file, and the
#'   upper- and lower-limit datasets should be specified as "upper" and "lower".}
#'   } For an example, please view the object "VBE_disso_example" by running
#'   this in your console: \code{view(VBE_disso_example)} and set up your data
#'   like that.
#' @param color_set the set of colors to use. Options: \describe{
#'
#'   \item{"default"}{a set of colors from Cynthia Brewer et al. from Penn State
#'   that are friendly to those with red-green colorblindness. The first three
#'   colors are green, orange, and purple. This can also be referred to as
#'   "Brewer set 2". If there are only two unique values in the "Type" column,
#'   then Brewer set 1 will be used since red and blue are still easily
#'   distinguishable but also more aesthetically pleasing than green and
#'   orange.}
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
#'   \item{"blues"}{a set of blues fading from sky to navy. Like
#'   "blue-green", this palette can be especially useful if you are comparing a
#'   systematic change in some continuous variable.}
#'
#'   \item{"greens"}{a set of greens fading from chartreuse to forest. Great for showing
#'   systematic changes in a continuous variable.}
#'
#'   \item{"purples"}{a set of purples fading from lavender to aubergine. Great for showing
#'   systematic changes in a continuous variable.}
#'
#'   \item{"reds"}{a set of reds from pink to brick. Great for showing
#'   systematic changes in a continuous variable.}
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
#'   which item in \code{Type} gets which color, you can supply a
#'   named vector. For example,
#'   you could do this: \code{color_set = c("Formulation A" = "dodgerblue3",
#'   "Formulation B" = "purple", "Test 1" = "#D8212D")}. If you'd
#'   like help creating a specific gradation of colors, please talk to a member
#'   of the R Working Group about how to do that using
#'   \link{colorRampPalette}.}}
#'
#' @param safe_space_color color to use for indicating the safe space; any color
#'   that's acceptable in R will be acceptable here.
#' @param safe_space_trans transparency to use for the safe space from 0 (fully
#'   transparent, i.e., invisible) to 1 (fully opaque)
#' @param linetypes the line type(s) to use for the graph. Possible options can
#'   be seen by typing \code{ggpubr::show_line_types()} into the console.
#' @param linewidths the line widths to use (numeric)
#' @param point_shapes the point shape(s) to use for the graph to indicate
#'   whether a given value was predicted or observed. Possible options can be
#'   seen by typing \code{ggpubr::show_point_shapes()} into the console. If you
#'   do not want points to show for one dataset, e.g., maybe you only want
#'   points for the observed data and just lines for the predicted, then set the
#'   point shape to NA.
#' @param point_sizes the point sizes (numeric)
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "Demographics comparisons.png". Acceptable graphical
#'   file extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or
#'   "svg". Do not include any slashes, dollar signs, or periods in the file
#'   name. Leaving this as NA means the file will not be saved to disk.
#' @param fig_height figure height in inches; default is 6
#' @param fig_width figure width in inches; default is 8
#' @param show_points_on_boundaries TRUE (default) or FALSE to include points
#'   on the boundaries of the dissolution safe space.
#'
#' @returns a ggplot2 graph
#' @export
#'
#' @examples
#' # Using example data included in the package
#' VBE_safe_space_plot(VBE_dataframe = VBE_disso_example)
#' 
#' # Setting some colors for actual and hypothetical datasets. First, let's check
#' # how many types of formulations we have.
#' VBE_disso_example %>% select(Type, SorO) %>% unique()
#' 
#' # Make a set of reds for the actual data and name them for which formulation you
#' # want them to represent
#' MyColors_actual <- reds(ncolors = 4, shade = "darker")
#' names(MyColors_actual) <- c("Formulation A",
#'                             "Formulation B",
#'                             "Formulation C",
#'                             "Formulation D")
#' 
#' # Make a set of blues for the hypothetical data and name these, too
#' MyColors_hyp <- blues(4)
#' names(MyColors_hyp) <- paste("Test", 1:4)
#' 
#' # Make the graph and save it
#' VBE_safe_space_plot(VBE_dataframe = VBE_disso_example,
#'                     color_set = c(MyColors_actual, MyColors_hyp),
#'                     safe_space_color = "gray80",
#'                     linetypes = c("solid", "longdash"),
#'                     save_graph = "VBE safe space.png",
#'                     fig_height = 4, fig_width = 6)
#' 
#' # Make the same graph except don't show the points and lines that mark the
#' # safe-space boundary
#' VBE_safe_space_plot(VBE_dataframe = VBE_disso_example,
#'                     color_set = c(MyColors_actual, MyColors_hyp),
#'                     safe_space_color = "gray80",
#'                     show_points_on_boundaries = FALSE, 
#'                     linetypes = c("solid", "longdash"),
#'                     save_graph = "VBE safe space v2.png",
#'                     fig_height = 4, fig_width = 6)
#' 
#' # Change some additional aesthetics 
#' VBE_safe_space_plot(VBE_dataframe = VBE_disso_example,
#'                     color_set = c(MyColors_actual, MyColors_hyp),
#'                     safe_space_color = "gray95",
#'                     point_shapes = c(16, 1), 
#'                     point_sizes = 3, 
#'                     linewidths = c(2, 0.5), 
#'                     show_points_on_boundaries = FALSE, 
#'                     linetypes = c("solid", "longdash"),
#'                     save_graph = "VBE safe space v3.png",
#'                     fig_height = 4, fig_width = 6)
#' 
#' 
#' 
VBE_safe_space_plot <- function(VBE_dataframe, 
                                color_set = NA, 
                                safe_space_color = NA, 
                                safe_space_trans = NA, 
                                linetypes = NA, 
                                linewidths = NA, 
                                point_shapes = NA, 
                                point_sizes = NA, 
                                show_points_on_boundaries = TRUE, 
                                save_graph = NA, 
                                fig_height = NA, 
                                fig_width = NA){
   
   # Error catching ------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
      
   if(all(is.na(color_set))){
      color_set <- "rainbow"
   }
   
   if(all(is.na(safe_space_color))){
      safe_space_color <- "#78C679"
   }
   safe_space_color <- make_color_set(color_set = safe_space_color, 
                                      num_colors = 1)
   
   if(all(is.na(as.numeric(safe_space_trans)))){
      safe_space_trans <- 0.5
   }
   safe_space_trans <- safe_space_trans[1]
   
   if(all(is.na(linetypes))){
      linetypes <- c("solid", "solid")
   }
   
   if(all(is.na(point_shapes))){
      point_shapes <- c(16, 17)
   }
   if(length(point_shapes) != 2){
      point_shapes <- rep(point_shapes, 2)[1:2]
   }
   
   if(all(is.na(point_sizes))){
      point_sizes <- c(2, 1)
   }
   if(length(point_sizes) != 2){
      point_sizes <- rep(point_sizes, 2)[1:2]
   }
   if(class(point_sizes) != "numeric"){
      warning(wrapn("The values for the argument 'point_sizes' must be numeric, and what you supplied is not. We will set the point sizes to the default values of 2 (observed) and 1 (predicted)."), 
              call. = FALSE)
      point_sizes <- c(2, 1)
   }
   
   if(all(is.na(linewidths))){
      linewidths <- c(0.5, 0.5) 
   }
   if(length(linewidths) != 2){
      linewidths <- rep(linewidths, 2)[1:2]
   }
   
   if("character" %in% class(VBE_dataframe) && 
      str_detect(VBE_dataframe[1], "csv")){
      VBE_dataframe <- read.csv(VBE_dataframe, 
                                na.strings = c("N/A", "NA", "", "na", "n/a"))
   }
   
   if("data.frame" %in% class(VBE_dataframe) == FALSE){
      stop(wrapn("We're having trouble with your input for the argument 'VBE_dataframe', which should be either a csv file name that we'll read or a data.frame. This is something else. Please check your input and try again."), 
           call. = FALSE)
   }
   
   if(nrow(VBE_dataframe) == 0){
      stop(wrapn("There are no data in what you supplied for VBE_dataframe. Please check your input and try again."), 
           call. = FALSE)
   }
   
   
   # Main body of function ---------------------------------------------------
   
   # Reshaping data to add a ribbon
   SSPolygon <- VBE_dataframe %>% 
      filter(complete.cases(Limit)) %>% 
      select(Time, SorO, Limit, Dissolution) %>% unique()
   
   SSPolygon <- split(SSPolygon, f = SSPolygon$Limit)
   SSPolygon$lower <- SSPolygon$lower %>% arrange(Time)
   SSPolygon$upper <- SSPolygon$upper %>% arrange(desc(Time))
   SSPolygon <- bind_rows(SSPolygon)
   
   Ncol <- sort(unique(VBE_dataframe$Type)) %>% length()
   color_set <- make_color_set(color_set = color_set, 
                               num_colors = Ncol)
   
   # Making sure that color_set is named so that, regardless of whether we show
   # points on boundaries, we still have the correct assignment of colors.
   if(is.null(names(color_set))){
      names(color_set) <- sort(unique(VBE_dataframe$Type))
   }
   
   if(show_points_on_boundaries == FALSE){
      VBE_dataframe <- VBE_dataframe %>%
         filter(is.na(Limit))
   }
   
   G <- ggplot(data = VBE_dataframe, 
               aes(x = Time, 
                   y = Dissolution, 
                   color = Type, 
                   fill = "Safe space", 
                   shape = SorO, 
                   linetype = SorO, 
                   linewidth = SorO, 
                   size = SorO)) +
      geom_polygon(data = SSPolygon,
                   aes(x = Time, y = Dissolution),
                   fill = safe_space_color,
                   alpha = safe_space_trans,
                   show.legend = TRUE,
                   key_glyph = draw_key_rect,
                   inherit.aes = F) +
      geom_line() +
      geom_point() +
      scale_fill_manual(
         values = safe_space_color) +
      scale_color_manual(values = color_set) +
      scale_linetype_manual(values = linetypes) +
      scale_linewidth_manual(values = linewidths) +
      scale_size_manual(values = point_sizes) +
      scale_shape_manual(values = point_shapes) +
      labs(shape = NULL, 
           linewidth = NULL,
           linetype = NULL, 
           size = NULL, 
           color = NULL, 
           fill = NULL) +
      guides(color = guide_legend(order = 1, 
                                  override.aes=list(fill =NA)), 
             shape = guide_legend(order = 2, 
                                  override.aes=list(fill = NA)), 
             linetype = guide_legend(order = 2), 
             linewidth = guide_legend(order = 2), 
             size = guide_legend(order = 2), 
             fill = guide_legend(order = 3, 
                                 override.aes = 
                                    list(color = NA, 
                                         alpha = safe_space_trans))) +
      xlab("Time (h)") + 
      ylab("Dissolution") +
      scale_y_continuous(labels=scales::percent) +
      theme_consultancy()
   
   if(complete.cases(save_graph)){
      
      if(is.na(fig_height)){
         fig_height <- 6
      }
      
      if(is.na(fig_width)){
         fig_width <- 8
      }
      
      FileName <- save_graph
      if(str_detect(FileName, "\\.")){
         # Making sure they've got a good extension
         Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
         FileName <- sub(paste0(".", Ext), "", FileName)
         if(Ext %in% c("eps", "ps", "jpeg", "tiff",
                       "png", "bmp", "svg", "jpg") == FALSE){
            warning(paste0("You have requested the graph's file extension be `",
                           Ext, "`, but we haven't set up that option. We'll save your graph as a `png` file instead.\n"),
                    call. = FALSE)
         }
         Ext <- ifelse(Ext %in% c("eps", "ps", "jpeg", "tiff",
                                  "png", "bmp", "svg", "jpg"),
                       Ext, "png")
         FileName <- paste0(FileName, ".", Ext)
      } else {
         FileName <- paste0(FileName, ".png")
         Ext <- "png"
      }
      
      ggsave(save_graph, plot = G, height = fig_height, width = fig_width, dpi = 300)
      
   }
   
   return(G)
   
}



