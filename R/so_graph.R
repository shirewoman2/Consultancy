#' Graph of simulated vs. observed PK -- NOT WORKING WHEN YOU SPECIFY SHAPE
#' COLUMN AT THE MOMENT
#'
#' \code{so_graph} makes a graph of simulated vs. observed PK, including
#' indicating where the predicted parameters fell within 1.5- or 2-fold of the
#' observed. When the PK parameter is a geometric mean ratio of the parameter in
#' the presence of an effector / the parameter in the absence of the effector,
#' the 1.5-fold line will be replaced with a curve as described in Guest Galetin
#' 2011 Drug Metab Dispos.
#'
#' @param PKtable a table in the same format as output from the function
#'   \code{\link{pksummary_mult}}
#' @param PKparameters any of the PK parameters included in the output from
#'   \code{\link{pksummary_mult}}; if left as NA, this will make graphs for each
#'   parameter included in \code{PKtable}. To see the full set of possible
#'   parameters, enter \code{view(PKParameterDefinitions)} into the console.
#' @param boundary_indicator how to indicate the boundaries of 1.5-fold and
#'   2-fold comparisons of simulated / observed. Options are "lines" (default)
#'   "fill" to get a shaded area, or "none" to remove any indicators of those
#'   boundaries. \strong{NOTE: There is a known bug within RStudio that causes
#'   filled semi-transparent areas like you get with the "fill" option to NOT
#'   get graphed for certain versions of RStudio.} To get around this, within
#'   RStudio, go to Tools --> Global Options --> General --> Graphics --> And
#'   then set "Graphics device: backend" to "AGG". Honestly, this is a better
#'   option for higher-quality graphics anyway!
#' @param boundary_color_set set of colors to use for indicating the 1.5-fold
#'   and 2-fold boundaries of the simulated / observed ratio. The default is
#'   "red black", which results in a black line at the 1.5-fold boundary and a
#'   red one at the 2-fold boundary. Other options are "red green", "muted red
#'   green" (a lighter, somewhat more muted red and green that work well for
#'   indicating the boundary when you're using shading instead of lines), and
#'   "black", which will result in only black lines or shading. You also can set
#'   this to any two colors you'd like, e.g., \code{boundary_color_set =
#'   c("yellow", "blue")}
#' @param boundary_line_width line width; default is 0.7. This only applies when
#'   \code{boundary_indicator} is set to "lines", the default.
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
#' so_graph(MyPKOutput)
#' so_graph(MyPKOutput, boundary_indicator = "fill")
#' 

so_graph <- function(PKtable, 
                     PKparameters = NA, 
                     boundary_indicator = "lines",
                     boundary_color_set = "red black", 
                     boundary_line_width = 0.7, 
                     point_color_column, 
                     point_color_set = "default",
                     legend_label_point_color = NA, 
                     point_shape_column, 
                     point_shape = NA,
                     point_size = NA,
                     legend_label_point_shape = NA, 
                     legend_position = "none",
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
   
   boundary_indicator <- ifelse(str_detect(tolower(boundary_indicator), "fil"), 
                                "fill", "lines")
   
   if(boundary_indicator == "fill" & length(boundary_color_set) == 1 & 
      !str_detect(boundary_color_set, " ")){
      warning("You have requested only one color for the fill, which means you'll only see a shaded rectangle for the 2-fold boundary. If you would also like to see the 1.5-fold or Guest-curve boundary, please add a second color.", 
              call. = FALSE)
   }
   
   if(any(is.na(PKparameters))){
      PKparameters <- names(PKtable)
      
      # Need to get the un-prettified names here
      PKparameters <- data.frame(PrettifiedNames = PKparameters) %>% 
         left_join(AllPKParameters %>% select(PrettifiedNames, PKparameter) %>% 
                      unique(), 
                   by = join_by(PrettifiedNames)) %>% 
         filter(complete.cases(PKparameter)) %>% 
         pull(PKparameter)
   }
   
   # Main body of function --------------------------------------------------
   
   ## Getting data arranged  ------------------------------------------------
   
   # Noting user's original preferences for a few things
   point_color_set_user <- point_color_set
   point_shape_user <- point_shape
   point_size_user <- point_size
   
   # Setting up data for boundaries on graphs
   Fold1.5_upper <- data.frame(Observed = 10^seq(-3, 6, length.out = 100)) %>% 
      mutate(LimitName = "upper", 
             Simulated = Observed * 1.5)
   Fold1.5_lower <- Fold1.5_upper %>% 
      mutate(LimitName = "lower", 
             Simulated = Observed / 1.5)
   
   Fold2_upper <- data.frame(Observed = 10^seq(-4, 6, length.out = 100)) %>% 
      mutate(LimitName = "upper", 
             Simulated = Observed * 2)
   Fold2_lower <- Fold2_upper %>% 
      mutate(LimitName = "lower", 
             Simulated = Observed / 2)
   Unity <- data.frame(Observed = 10^seq(-4, 6, length.out = 100)) %>% 
      mutate(Simulated = Observed)
   
   # Guest curves
   GuestCurve_upper <- data.frame(Observed = 10^seq(-4, 6, length.out = 1000)) %>% 
      mutate(Limit = ifelse(Observed >= 1, 
                            (1 + 2*(Observed - 1))/Observed,
                            (1 + 2*((1/Observed) - 1))/(1/Observed)),
             LimitName = "upper",
             Simulated = Observed * Limit)
   
   GuestCurve_lower <- GuestCurve_upper %>% 
      mutate(LimitName = "lower", 
             Simulated = Observed / Limit)
   
   
   # Setting up polygons
   Poly2x <- Fold2_upper %>% arrange(Observed) %>% 
      bind_rows(Fold1.5_upper %>% arrange(desc(Observed))) %>% 
      bind_rows(Fold1.5_lower %>% arrange(Observed)) %>% 
      bind_rows(Fold2_lower %>% arrange(desc(Observed)))
   
   Poly1.5x <- Fold1.5_upper %>% arrange(Observed) %>% 
      bind_rows(Fold1.5_lower %>% arrange(desc(Observed)))
   
   Poly2xGuest <- Fold2_upper %>% arrange(Observed) %>% 
      bind_rows(GuestCurve_upper %>% arrange(desc(Observed))) %>% 
      bind_rows(GuestCurve_lower %>% arrange(Observed)) %>% 
      bind_rows(Fold2_lower %>% arrange(desc(Observed)))
   
   Poly1.5xGuest <- GuestCurve_upper %>% arrange(Observed) %>% 
      bind_rows(GuestCurve_lower %>% arrange(desc(Observed)))
   
   
   if(length(boundary_color_set) == 1){
      BoundColors <- switch(boundary_color_set, 
                            "red green" = c("red", "#17A142"), 
                            "muted red green" = c("#E6A2A2", "#A4E4AF"),
                            "red black" = c("red", "black"),
                            "black" = c("black", "black"))
   } else {
      BoundColors <- boundary_color_set[1:2]
   }
   
   # Arranging input data
   
   # First, de-prettifying column names
   PKnames <- data.frame(OrigName = names(PKtable)) %>% 
      mutate(PrettifiedNames = sub("with .* ", "with effector ", OrigName)) %>% 
      left_join(AllPKParameters %>% filter(!PKparameter == "CLt_dose1") %>% 
                   select(PrettifiedNames, PKparameter) %>% unique(), 
                by = join_by(PrettifiedNames)) %>% 
      mutate(NewName = ifelse(is.na(PKparameter), OrigName, PKparameter))
   
   SO <- PKtable
   names(SO) <- PKnames$NewName
   
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
      
      if(class(SO$point_color_column) == "numeric"){
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
      
      if(class(SO$point_shape_column) == "numeric"){
         Levels <- sort(unique(SO$point_shape_column))
         SO <- SO %>% 
            mutate(point_shape_column = factor(point_shape_column, levels = Levels))
      }
      
      if(any(complete.cases(point_shape))){
         NumShapesNeeded <- length(sort(unique(SO$point_shape_column)))
         
         if(NumShapesNeeded == 1){
            MyShapes <- 16
         } else {
            if(length(point_shape) < NumShapesNeeded){
               # This odd syntax will work both when point_shape is a single value
               # and when it is multiple values.
               point_shape <- rep(point_shape, NumShapesNeeded)[1:NumShapesNeeded] 
            } else if(length(point_shape) < NumShapesNeeded){
               point_shape <- point_shape[1:NumShapesNeeded] 
            }
         }
      }
   }
   
   
   ## Making graphs ---------------------------------------------------------
   G <- list()
   SO <- split(SO, f = SO$PKparameter)
   
   for(i in names(SO)){
      
      Limits <- c(
         round_down(min(c(SO[[i]]$Observed, SO[[i]]$Simulated), na.rm = T)),
         
         round_up(max(c(SO[[i]]$Observed, SO[[i]]$Simulated), na.rm = T)))
      
      G[[i]] <- switch(paste(
         as_label(point_color_column) != "<empty>",
         as_label(point_shape_column) != "<empty>"),
         
         # User has NOT specified anything for point shape or color
         "FALSE FALSE" = ggplot(SO[[i]], aes(x = Observed, y = Simulated)), 
         
         # User has specified a column for point color
         "TRUE FALSE" = ggplot(SO[[i]], aes(x = Observed, y = Simulated, 
                                            color = point_color_column)) + 
            scale_color_manual(values = MyColors), 
         
         # User has specified a column for point shape
         "FALSE TRUE" = 
            switch(as.character(any(is.na(point_shape))), 
                   "TRUE" = ggplot(SO[[i]], aes(x = Observed, y = Simulated, 
                                                shape = point_shape_column)), 
                   "FALSE" = ggplot(SO[[i]], aes(x = Observed, y = Simulated, 
                                                 shape = point_shape_column)) +
                      scale_shape_manual(values = MyShapes)),
         
         # User has specified both color and point shape columns
         "TRUE TRUE" = 
            switch(as.character(any(is.na(point_shape))), 
                   "TRUE" = ggplot(SO[[i]], aes(x = Observed, y = Simulated, 
                                                color = point_color_column, 
                                                shape = point_shape_column)) +
                      scale_color_manual(values = MyColors), 
                   "FALSE" = ggplot(SO[[i]], aes(x = Observed, y = Simulated, 
                                                 color = point_color_column, 
                                                 shape = point_shape_column)) +
                      scale_color_manual(values = MyColors) +
                      scale_shape_manual(values = MyShapes))
      )
      
      G[[i]] <- G[[i]] +
         geom_line(data = Unity, linetype = "dashed", color = "black",
                   linewidth = boundary_line_width)
      
      if(str_detect(boundary_indicator, "line")){
         G[[i]] <- G[[i]] + 
            geom_line(data = Fold2_upper, color = BoundColors[1], linewidth = boundary_line_width) +
            geom_line(data = Fold2_lower, color = BoundColors[1], linewidth = boundary_line_width) +
            geom_line(data = switch(as.character(str_detect(i, "ratio")), 
                                    "TRUE" = GuestCurve_upper,
                                    "FALSE" = Fold1.5_upper), 
                      color = BoundColors[2], linewidth = boundary_line_width) +
            geom_line(data = switch(as.character(str_detect(i, "ratio")), 
                                    "TRUE" = GuestCurve_lower, 
                                    "FALSE" = Fold1.5_lower),
                      color = BoundColors[2], linewidth = boundary_line_width) 
      }
      
      if(str_detect(boundary_indicator, "fill")){
         G[[i]] <- G[[i]] +
            geom_polygon(data = switch(as.character(str_detect(i, "ratio")), 
                                       "TRUE" = Poly2xGuest, 
                                       "FALSE" = Poly2x), 
                         fill = BoundColors[1], alpha = 0.2) +
            geom_polygon(data = switch(as.character(str_detect(i, "ratio")),
                                       "TRUE" = Poly1.5xGuest,
                                       "FALSE" = Poly1.5x), 
                         fill = BoundColors[2], alpha = 0.2)
      }
      
      MaxMinRatio <- range(c(SO[[i]]$Observed, SO[[i]]$Simulated))
      MaxMinRatio <- MaxMinRatio[2] / MaxMinRatio[1]
      
      if(MaxMinRatio > 100){
         MajBreaks <- 10^(-3:6)
         MinBreaks <- rep(1:9)*rep(10^(-3:6), each = 9)
      } else {
         MajBreaks <- c(10^(-3:6),
                        3*10^(-3:6),
                        5*10^(-3:6))
         MinBreaks <- rep(1:9)*rep(10^(-3:6), each = 9)
      }
      
      G[[i]] <- G[[i]] + 
         geom_point(size = ifelse(is.na(point_size), 2, point_size)) +
         scale_y_log10(breaks = MajBreaks, minor_breaks = MinBreaks) +
         scale_x_log10(breaks = MajBreaks, minor_breaks = MinBreaks) +
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
         if(complete.cases(legend_label_point_color)){
            if(legend_label_point_color == "none"){    
               G[[i]] <- G[[i]] + labs(shape = NULL)
            } else {
               G[[i]] <- G[[i]] + labs(shape = legend_label_point_color)
            }
         } else {
            # This is when no legend_label_point_color has been specified.
            G[[i]] <- G[[i]] + labs(shape = as_label(point_color_column))
         } 
      }
   }
   
   if(length(G) == 1){
      G <- G[[1]] + theme(legend.position = "none")
      
   } else {
      G <-  ggpubr::ggarrange(plotlist = G, align = "hv", 
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
         Ext <- ifelse(Ext %in% c("eps", "ps", "jpeg", "tiff",
                                  "png", "bmp", "svg", "jpg"), 
                       Ext, "png")
         FileName <- paste0(FileName, ".", Ext)
      } else {
         FileName <- paste0(FileName, ".png")
         Ext <- "png"
      }
      
      # This is when they want any kind of graphical file format.
      ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
             plot = G)
      
   }
   
   return(G)
   
}


