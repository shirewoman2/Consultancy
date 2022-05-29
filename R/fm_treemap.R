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
#'   "rainbow", which works well when there are a LOT of fm's to visualize, or
#'   "blue-green", which makes a pretty gradient of blues and greens.
#' @param label_fm_cutoff cutoff to use for having a label show up underneath
#'   the graph; default is 0.05. You may want to play around with this a bit in
#'   the final graphic file because how this graph looks is extremely dependent
#'   on its dimensions.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png". If you do not designate a
#'   file extension, it will be saved as a png file, but if you specify a
#'   different file extension, it will be saved as that file format. Acceptable
#'   extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg".
#'   Leaving this as NA means the file will not be automatically saved to disk.
#' @param fig_height figure height in inches; default is 3
#' @param fig_width figure width in inches; default is 4
#'
#' @return
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
                       save_graph = NA, 
                       fig_height = 3, 
                       fig_width = 4){
    
    # Checking format of fm_dataframe
    if(all(c("fm", "DME") %in% names(fm_dataframe)) == FALSE){
        stop("fm_dataframe must contain columns titled 'fm' and 'DME', and those are not present.")
    }
    
    # Adding labels 
    fm_dataframe <- fm_dataframe %>% 
        mutate(Label = paste0(DME, "\n", fm*100, "%"), 
               Label = fct_reorder(Label, fm, .desc = TRUE)) %>% 
        arrange(Label)
    
    # Adding options for colors
    NumColors <- length(levels(fm_dataframe$Label))
    
    colRainbow <- colorRampPalette(c("gray20", "antiquewhite4", "firebrick3",
                                     "darkorange", "green3", "seagreen3",
                                     "cadetblue", "dodgerblue3", "royalblue4",
                                     "darkorchid4"))
    
    blueGreen <- colorRampPalette(c("green3", "seagreen3", "cadetblue", 
                                    "dodgerblue3", "royalblue4"))
    MyColors <- switch(color_set, 
                       "default" = SimcypColors, 
                       "set 1" = RColorBrewer::brewer.pal(NumColors, "Set1"),
                       "set 2" = RColorBrewer::brewer.pal(NumColors, "Dark2"),
                       "rainbow" = colRainbow(NumColors), 
                       "blue-green" = blueGreen(NumColors))
    
    # Adjusting default colors based on number of levels to the combos I like
    # best. :-)
    if(color_set == "default" & NumColors > 1 & NumColors < 5){
        MyColors <- switch(as.character(NumColors), 
                           "2" = SimcypColors[2:3], 
                           "3" = SimcypColors[2:4], 
                           "4" = SimcypColors[2:5])
    }
    
    names(MyColors) <- levels(fm_dataframe$Label)
    
    # Putting into the legend any fm's that are below the threshold
    G <- ggplot(fm_dataframe, aes(label = Label, area = fm, fill = Label)) +
        treemapify::geom_treemap() +
        treemapify::geom_treemap_text(fontface = "bold", colour = "white", place = "centre", 
                          min.size = 6) +
        scale_fill_manual(
            breaks = fm_dataframe$Label[which(fm_dataframe$fm < label_fm_cutoff)],
            values = MyColors) +
        theme(
            legend.title = element_blank(),
            legend.position = "bottom"
        )
    
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


