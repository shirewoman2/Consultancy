
#' Make boxplots or boxplots overlaid with individual points
#'
#' \code{graph_boxplot} takes as input a data.frame of numeric values and the
#' categories those values are in and creates boxplots of those data. Options
#' are included for type of graph to make and some aesthetics.
#'
#' @param DF the data.frame you want to graph
#' @param category_column the name of the column with categorical data in quotes
#' @param value_column the name of the column with value data in quotes
#' @param facet1_column (optional) the name of a column by which you might break
#'   up your graph into small multiples. Please see the example if you're
#'   uncertain what this does.
#' @param facet2_column (optional) the name of a second column by which you
#'   might break up the graph into small multiples.
#' @param graph_type the type of graph to plot. Options: \describe{
#'   \item{"boxplot"}{standard boxplots or box-and-whisker plots}
#'
#'   \item{"jittered points"}{boxplots overlaid with points depicting each
#'   individual observation.}
#'
#'   \item{"jittered points, filled boxes"}{boxplots with fill color according
#'   to \code{category_column} and open circles for the jittered points}}
#' @param include_errorbars TRUE or FALSE on whether to include horizontal error
#'   bars on the whiskers
#' @param xlabel the label to  use for the x axis
#' @param ylabel the label to use for the y axis
#' @param color_set the set of colors to use. Options: \describe{
#'
#'   \item{"default"}{colors selected from the color brewer palette "set 1"}
#'
#'   \item{"ggplot2 default"}{the default set of colors used in ggplot2 graphs
#'   (ggplot2 is an R package for graphing.)}
#'
#'   \item{"black"}{black and white}
#'
#'   \item{"rainbow"}{colors selected from a rainbow palette. The default
#'   palette is limited to something like 6 groups, so if you have more than
#'   that, that's when this palette is most useful.}
#'
#'   \item{"blue-green"}{a set of blues and greens}
#'
#'   \item{"Brewer set 2"}{a set of colors from Cynthia Brewer et al. from Penn
#'   State that are friendly to those with red-green colorblindness}
#'
#'   \item{"Tableau"}{uses the standard Tableau palette; requires the "ggthemes"
#'   package}}
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png". If you leave off ".png", it
#'   will be saved as a png file, but if you specify a different file extension,
#'   it will be saved as that file format. Acceptable extensions are "eps",
#'   "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg". Leaving this as NA
#'   means the file will not be automatically saved to disk.
#' @param fig_height figure height in inches; default is 4
#' @param fig_width figure width in inches; default is 6
#'
#' @return
#' @import tidyverse
#' @export
#'
#' @examples
#' AUCs <- data.frame(AgeGroup = rep(LETTERS[1:5], each = 100),
#'                    AUC = c(rnorm(n = 100, mean = 10000, sd = 5000),
#'                            rnorm(n = 100, mean = 15000, sd = 5000),
#'                            rnorm(n = 100, mean = 20000, sd = 5000),
#'                            rnorm(n = 100, mean = 30000, sd = 5000),
#'                            rnorm(n = 100, mean = 33000, sd = 5000)))
#'
#' graph_boxplot(AUCs, category_column = AgeGroup, value_column = AUC)
#'
#' graph_boxplot(AUCs, category_column = AgeGroup, value_column = AUC,
#'                color_set = "rainbow")
#'
#' graph_boxplot(AUCs, category_column = AgeGroup, value_column = AUC,
#'                color_set = "blue-green", graph_type =  "jittered points")
#'
#' graph_boxplot(AUCs, category_column = AgeGroup, value_column = AUC,
#'               color_set = "Tableau", graph_type =  "jittered points, filled boxes")
#'
#' # Adding a couple of example columns to use the "facet" options.
#' AUCs$Sex <- c("M", "F")
#' AUCs$Metabolizer <- sample(c("poor", "extensive"), 100, replace = TRUE)
#'
#' graph_boxplot(AUCs, category_column = AgeGroup, value_column = AUC,
#'                facet1_column = Sex,
#'                color_set = "blue-green", graph_type =  "jittered points")
#'
#' graph_boxplot(AUCs, category_column = AgeGroup, value_column = AUC,
#'                facet1_column = Sex, facet2_column = Metabolizer,
#'                color_set = "blue-green", graph_type =  "jittered points")
#'
#' graph_boxplot(AUCs, category_column = AgeGroup, value_column = AUC,
#'               graph_type = "jittered points, filled boxes")
#'
#' # Saving the output
#' graph_boxplot(AUCs %>% filter(AgeGroup %in% c("A", "B")),
#'               category_column = AgeGroup, value_column = AUC,
#'               graph_type = "jittered points, filled boxes",
#'               color_set = "Brewer set 2", include_errorbars = TRUE,
#'               save_graph = "test boxplot.png")
#'
#'
graph_boxplot <- function(DF,
                          category_column,
                          value_column,
                          facet1_column,
                          facet2_column,
                          graph_type = "boxplot",
                          include_errorbars = FALSE,
                          xlabel = NA,
                          ylabel = NA,
                          color_set = "default",
                          save_graph = NA,
                          fig_width = 6, fig_height = 4){
    
    
    # Setting things up for nonstandard evaluation -------------------------
    
    # Defining pipe operator and bang bang
    `%>%` <- magrittr::`%>%`
    `!!` <- rlang::`!!`
    
    category_column <- rlang::enquo(category_column)
    value_column <- rlang::enquo(value_column)
    facet1_column <- rlang::enquo(facet1_column)
    facet2_column <- rlang::enquo(facet2_column)
    
    # Building graph layers -------------------------------------
    if(color_set == "black"){
        G <- ggplot(DF, aes(x = !!category_column, y = !!value_column))
    } else {
        G <- ggplot(DF, aes(x = !!category_column, y = !!value_column,
                            fill = !!category_column, color = !!category_column))
    }
    
    G <- G + theme(legend.position = "none")
    
    if(include_errorbars == TRUE){
        G <- G + stat_boxplot(geom = "errorbar", color = "black")
    }
    
    if(graph_type == "boxplot"){
        G <- G + geom_boxplot(color = "black")
    }
    
    if(graph_type == "jittered points"){
        
        JitterWidth = 0.5/length(unique(DF %>% pull(!!category_column)))
        
        G <- G + geom_boxplot(color = "black", fill = NA,
                              outlier.shape = NA) +
            geom_point(position = position_jitter(width = JitterWidth,
                                                  height = 0))
    }
    
    if(graph_type == "jittered points, filled boxes"){
        JitterWidth = 0.5/length(unique(DF %>% pull(!!category_column)))
        
        G <- G + geom_boxplot(color = "black", outlier.shape = NA) +
            geom_point(position = position_jitter(width = JitterWidth,
                                                  height = 0),
                       shape = 1, color = "black")
    }
    
    # Facets 
    G <- G +
        facet_grid(rows = vars(!!facet1_column),
                   cols = vars(!!facet2_column), 
                   scales = "free")
    
    if(complete.cases(xlabel)){
        G <- G + xlab(xlabel)
    } else {
        G <- G + xlab(rlang::as_label(category_column))
    }
    
    if(complete.cases(ylabel)){
        G <- G + ylab(ylabel)
    } else {
        G <- G + ylab(rlang::as_label(value_column))
    }
    
    # Adding some aesthetic preferences ------------------------------------
    G <- G + theme(
        panel.background = element_rect(fill="white", color=NA),
        panel.grid.minor.y = element_line(color = NA),
        panel.grid.minor.x = element_line(color = NA),
        panel.grid.major = element_line(colour = NA),
        plot.background = element_rect(fill="white", colour=NA),
        panel.border = element_rect(color="black", fill=NA),
        strip.background = element_rect(color=NA, fill="white"),
        axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black",
                                  face = "bold", size = 14),
        legend.background = element_rect(color=NA, fill=NA),
        legend.key = element_rect(color=NA, fill=NA))
    
    # Adding options for colors
    colRainbow <- colorRampPalette(c("gray20", "antiquewhite4", "firebrick3",
                                     "darkorange", "green3", "seagreen3",
                                     "cadetblue", "dodgerblue3", "royalblue4",
                                     "darkorchid4"))
    
    blueGreen <- colorRampPalette(c("green3", "seagreen3", "cadetblue", 
                                    "dodgerblue3", "royalblue4"))
    
    
    if(color_set == "default"){
        G <- G + scale_color_brewer(palette = "Set1") +
            scale_fill_brewer(palette="Set1")
    }
    
    if(color_set == "blue-green"){
        G <- G + 
            scale_color_manual(
                values = blueGreen(length(unique(DF %>% pull(!!category_column))))) +
            scale_fill_manual(
                values = blueGreen(length(unique(DF %>% pull(!!category_column)))))
    }
    
    if(color_set == "rainbow"){
        G <- G + scale_color_manual(
            values = colRainbow(length(unique(DF %>% pull(!!category_column))))) +
            scale_fill_manual(
                values = colRainbow(length(unique(DF %>% pull(!!category_column)))))
    }
    
    if(color_set == "Brewer set 2"){
        G <- G + scale_fill_brewer(palette = "Set2") +
            scale_color_brewer(palette = "Set2")
    }
    
    if(color_set == "Tableau"){
        G <- G + ggthemes::scale_color_tableau() +
            ggthemes::scale_fill_tableau()
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



