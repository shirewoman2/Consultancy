#' Make dissolution safe space plots
#'
#' @description The function \code{VBE_safe_space_plot} makes dissolution ...
#' UNDER CONSTRUCTION
#'
#'
#' @param VBE_dataframe data.frame
#' @param color_set color set
#' @param safe_space_color color
#' @param safe_space_trans transparency
#' @param linetypes the line type(s) to use for the graph. Possible options
#'   can be seen by typing \code{ggpubr::show_line_types()} into the console. 
#' @param linewidths must be numeric
#' @param point_shapes the point shape(s) to use for the graph. Possible options
#'   can be seen by typing \code{ggpubr::show_point_shapes()} into the console. 
#' @param point_sizes point sizes. must be numeric. 
#' @param save_graph file name
#' @param fig_height default is 6
#' @param fig_width default is 8
#'
#' @returns a ggplot2 graph 
#' @export
#'
#' @examples
#' # None yet
#' 
VBE_safe_space_plot <- function(VBE_dataframe, 
                                color_set = NA, 
                                safe_space_color = NA, 
                                safe_space_trans = NA, 
                                linetypes = NA, 
                                linewidths = NA, 
                                point_shapes = NA, 
                                point_sizes = NA, 
                                save_graph = NA, 
                                fig_height = NA, 
                                fig_width = NA){
   
   # Error catching ------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   
   if(all(is.na(color_set))){
      color_set <- "rainbow"
   }
   
   if(all(is.na(safe_space_color))){
      safe_space_color <- "#78C679"
   }
   
   if(all(is.na(safe_space_trans))){
      safe_space_trans <- 0.5
   }
   
   if(all(is.na(linetypes))){
      linetypes <- c("solid", "solid")
   }
   
   if(all(is.na(point_shapes))){
      point_shapes <- c(16, 17)
   }
   
   if(all(is.na(point_sizes))){
      point_sizes <- c(2, 1)
   }
   
   if(all(is.na(linewidths))){
      linewidths <- c(0.5, 0.5) 
   }
   
   if(class(point_sizes) != "numeric"){
      warning(wrapn("The values for the argument 'point_sizes' must be numeric, and what you supplied is not. We will set the point sizes to the default values of 2 (observed) and 1 (predicted)."), 
              call. = FALSE)
      point_sizes <- c(2, 1)
   }
   
   
   # Main body of function ---------------------------------------------------
   
   Ncol <- unique(VBE_dataframe$Type) %>% length()
   
   # # Original code: 
   # color_set <- switch(color_set, 
   #                     "blues" = blues(Ncol), 
   #                     "greens" = greens(ncolors = Ncol, shade = "darker"), 
   #                     "purples" = purples(Ncol, shade = "darker"), 
   #                     "blueGreens" = blueGreens(Ncol), 
   #                     "rainbow" = rainbow(Ncol))
   
   # Revised code: The above is a good example of how to use "switch" inside a
   # function. However, this does not allow for manually specifying each of the
   # colors yourself and and it also does not perform any checks for whether
   # what someone has supplied is, in fact, a color. A function that does this
   # already exists inside the SimcypConsultancy package, so we'll use the
   # function make_color_set to do this. 
   color_set <- make_color_set(color_set = color_set, 
                               num_colors = Ncol)
   
   # Reshaping data to add a ribbon
   RibbonLimits <- VBE_dataframe %>% 
      filter(complete.cases(Limit)) %>% 
      select(Time, SorO, Limit, Dissolution) %>% unique() %>% 
      pivot_wider(names_from = Limit, values_from = Dissolution)
   
   G <- ggplot(data = VBE_dataframe, 
          aes(x = Time, y = Dissolution, 
              color = Type, shape = SorO, linetype = SorO, 
              linewidth = SorO, size = SorO)) +
      # NB: ggplot graphs are stacked layer upon layer, so, if we want the
      # shading to be underneath the points and lines, we will add the shading
      # first and the lines and points second and third.
      geom_ribbon(data = RibbonLimits, 
                  aes(x = Time, ymin = lower, ymax = upper), 
                  inherit.aes = FALSE, 
                  alpha = safe_space_trans, 
                  fill = safe_space_color, color = NA) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = color_set) +
      scale_linetype_manual(values = linetypes) +
      scale_linewidth_manual(values = linewidths) +
      scale_size_manual(values = point_sizes) +
      scale_shape_manual(values = point_shapes) +
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



