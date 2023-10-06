#' For comparing fm values, make a treemap as opposed to the standard and
#' arguably inferior pie chart
#'
#' Create a treemap -- filled rectangles -- to indicate the fraction of
#' metabolism due to various pathways. Rectangles are proportional in size to
#' the fm values and include labels for the drug-metabolizing enzyme involved
#' and the percentage that enzyme is responsible for. Any rectangles that would
#' be smaller than some user-specified cutoff (see argument "label_fm_cutoff")
#' will be pulled out underneath the graph for readability. Why bother with a
#' treemap as opposed to a pie chart? Rectangles are easier to compare visually
#' than the sizes of pie slices, making a treemap easier to understand and
#' interpret (reference: "The Visual Display of Quantitative Information" by
#' Edward Tufte, Professor Emeritus of Political Science, Statistics, and
#' Computer Science at Yale University).
#'
#'
#' @param fm_dataframe a data.frame containing columns for
#'   \describe{\item{DME}{the drug-metabolizing enzyme} \item{fm}{the fraction
#'   metaboilized}}
#' @param color_set the set of colors to use. Options are "default", which
#'   matches Simcyp colors in PowerPoint presentations and is rather nice, "set
#'   1" for Brewer set 1, "set 2" for Brewer set 2 (colorblind friendly),
#'   "rainbow", which works well when there are a LOT of fm's to visualize,
#'   "blue-green", which makes a pretty gradient of blues and greens, "blues",
#'   which makes a graident of blues, or "viridis", which is from the eponymous
#'   package by Simon Garnier and ranges colors from purple to blue to green to
#'   yellow in a manner that is "printer-friendly, perceptually uniform and easy
#'   to read by those with colorblindness", according to the package author.
#' @param label_fm_cutoff cutoff to use for having a label show up underneath
#'   the graph; default is 0.05. You may want to play around with this a bit in
#'   the final graphic file because how this graph looks is extremely dependent
#'   on its dimensions.
#' @param legend_nrow optionally specify the number of rows to use for the
#'   legend. If left as NA, there will be 1 row.
#' @param biggest_box_position where to put the biggest box. Defaults to "top
#'   left", and other options are "bottom left", "bottom right", and "top
#'   right".
#' @param graph_title optionally specify a title that will be centered across
#'   your graph
#' @param graph_title_size the font size for the graph title if it's included
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png". If you do not designate a
#'   file extension, it will be saved as a png file, but if you specify a
#'   different file extension, it will be saved as that file format. Acceptable
#'   extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg".
#'   Do not include any slashes, dollar signs, or periods in the file name.
#'   Leaving this as NA means the file will not be automatically saved to disk.
#' @param fig_height figure height in inches; default is 3
#' @param fig_width figure width in inches; default is 4
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
#'
#' Lenv_fm <- data.frame(DME = c("CYP3A4", "other CYPs", "aldehyde oxidase"),
#'                       fm = c(0.79, 0.20, 0.01))
#' fm_treemap(fm_dataframe = Lenv_fm)
#' fm_treemap(fm_dataframe = Lenv_fm, color_set = "blue-green")
#' fm_treemap(fm_dataframe = Lenv_fm, color_set = "set 1",
#'     label_fm_cutoff = 0.01)
#' 
fm_treemap <- function(fm_dataframe,
                       color_set = "default", 
                       label_fm_cutoff = 0.05,
                       legend_nrow = NA,
                       biggest_box_position = "top left",
                       graph_title = NA,
                       graph_title_size = 14, 
                       save_graph = NA, 
                       fig_height = 3, 
                       fig_width = 4){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
           call. = FALSE)
   }
   
   if(nrow(fm_dataframe) == 0){
      stop("Please check your input. The data.frame you supplied for fm_dataframe doesn't have any rows.", 
           call. = FALSE)
   }
   
   # Check whether treemapify has been installed. 
   if("treemapify" %in% installed.packages() == FALSE){
      stop("The function fm_treemap requires the package treemapify. Please run `install.packages('treemapify')`.", 
           call. = FALSE)
   }
   
   # Checking format of fm_dataframe
   if(all(c("fm", "DME") %in% names(fm_dataframe)) == FALSE){
      stop("fm_dataframe must contain columns titled 'fm' and 'DME', and one or both of those are not present.", 
           call. = FALSE)
   }
   
   if(biggest_box_position %in% c("bottom left", "top left", "bottom right", 
                                  "top right") == FALSE){
      warning(paste0("The only options for `biggest_box_position` are `bottom left`, `top left`, `bottom right`, or `top right`, and you supplied a value of `", 
                     biggest_box_position,
                     "``, which is not one of the options. We'll use the default of `bottom left`."), 
              call. = FALSE)
      biggest_box_position <- "top left"
   }
   
   # Adding labels 
   fm_dataframe <- fm_dataframe %>% 
      mutate(Label = paste0(DME, "\n", fm*100, "%"), 
             LabelLegend = paste0(DME, " ", fm*100, "%"), 
             Label = fct_reorder(Label, fm, .desc = TRUE), 
             LabelLegend = fct_reorder(LabelLegend, fm, .desc = TRUE)) %>% 
      arrange(Label)
   
   # Adding options for colors
   NumColors <- length(levels(fm_dataframe$Label))
   
   if(length(color_set) > 1){
      
      MyColors <- color_set
      
      if(length(MyColors) != NumColors){
         warning(paste("You supplied", length(color_set), "colors, but we need", 
                       NumColors, "colors. We will have to use the default color set instead.\n"), 
                 call. = FALSE)
         
         color_set <- "default"
      }
   } 
   
   if(length(color_set) == 1){
      
      if(color_set == "default" & NumColors > 5){
         color_set <- "viridis"
         warning("You requested the color_set 'default', which has 5 possible colors, but your graph requires more colors than that. The color set 'viridis' will be used instead.", 
                 call. = FALSE)
      }
      
      MyColors <- switch(color_set, 
                         "default" = SimcypColors, 
                         "set 1" = RColorBrewer::brewer.pal(NumColors, "Set1"),
                         "set 2" = RColorBrewer::brewer.pal(NumColors, "Dark2"),
                         "rainbow" = rainbow(NumColors), 
                         "blue-green" = blueGreens(NumColors),
                         "blues" = blues(NumColors),
                         "viridis" = viridis::viridis(NumColors))
      
      # Adjusting default colors based on number of levels to the combos I like
      # best. :-)
      if(color_set == "default" & NumColors > 1 & NumColors < 5){
         MyColors <- switch(as.character(NumColors), 
                            "2" = SimcypColors[2:3], 
                            "3" = SimcypColors[2:4], 
                            "4" = SimcypColors[2:5])
      }
      
   }
   
   names(MyColors) <- levels(fm_dataframe$LabelLegend)
   
   # Putting into the legend any fm's that are below the threshold
   G <- ggplot(fm_dataframe, aes(label = Label, area = fm, fill = LabelLegend)) +
      treemapify::geom_treemap(start = sub(" ", "", biggest_box_position)) +
      treemapify::geom_treemap_text(fontface = "bold", colour = "white", place = "centre", 
                                    min.size = 6, 
                                    start = sub(" ", "", biggest_box_position)) +
      scale_fill_manual(
         breaks = fm_dataframe$LabelLegend[which(fm_dataframe$fm < label_fm_cutoff)],
         values = MyColors) +
      theme(legend.title = element_blank(),
            legend.position = "bottom")
   
   if(complete.cases(legend_nrow)){
      G <- G + guides(fill = guide_legend(nrow = legend_nrow))
   }
   
   if(("character" %in% class(graph_title) && complete.cases(graph_title)) |
      "expression" %in% class(graph_title)){
      G <- G + ggtitle(graph_title) +
         theme(plot.title = element_text(hjust = 0.5, size = graph_title_size))
   }
   
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
      
      ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
             plot = G)
   }
   
   return(G)
   
}


