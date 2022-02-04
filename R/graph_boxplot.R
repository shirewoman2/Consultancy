
#' Make boxplots or boxplots overlaid with individual points
#'
#' \code{graph_boxplot} takes as input a data.frame of numeric values and the
#' categories those values are in and creates boxplots of those data. Options
#' are included for type of graph to make and some aesthetics.
#'
#' @param DF the data.frame you want to graph
#' @param category_column the name of the column with categorical data in quotes
#' @param value_column the name of the column with value data in quotes
#' @param facet_column1 (optional) the name of a column by which you might break
#'   up your graph into small multiples. Please see the example if you're
#'   uncertain what this does.
#' @param facet_column2 (optional) the name of a second column by which you
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
#' @param filename the file name for the output graph, e.g., "My pretty
#'   boxplot.png". If you don't want to automatically save the graph, leave this
#'   as NA.
#' @param outwidth the width in inches of the output graph
#' @param outheight the height in inches of the output graph
#'
#' @return
#' @import tidyverse
#' @export
#'
#' @examples
#' AUCs <- data.frame(AgeGroup = LETTERS[1:5],
#'                    AUC = rnorm(n = 500, mean = 20000, sd = 2000))
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC")
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'                color_set = "rainbow")
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'                color_set = "blue-green", graph_type =  "jittered points")
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'               color_set = "Tableau", graph_type =  "jittered points, filled boxes")
#'
#' # Adding a couple of example columns to use the "facet" options.
#' AUCs$Sex <- c("M", "F")
#' AUCs$Metabolizer <- sample(c("poor", "extensive"), 100, replace = TRUE)
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'                facet_column1 = "Sex",
#'                color_set = "blue-green", graph_type =  "jittered points")
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'                facet_column1 = "Sex", facet_column2 = "Metabolizer",
#'                color_set = "blue-green", graph_type =  "jittered points")
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'               graph_type = "jittered points, filled boxes")
#'
#' # Saving the output
#' graph_boxplot(AUCs %>% filter(AgeGroup %in% c("A", "B")),
#'               category_column = "AgeGroup", value_column = "AUC",
#'               graph_type = "jittered points, filled boxes",
#'               color_set = "Brewer set 2", include_errorbars = TRUE,
#'               filename = "test boxplot.png")
#'
#'
graph_boxplot <- function(DF,
                          category_column,
                          value_column,
                          facet_column1 = NA,
                          facet_column2 = NA,
                          graph_type = "boxplot",
                          include_errorbars = FALSE,
                          xlabel = NA,
                          ylabel = NA,
                          color_set = "default",
                          filename = NA,
                          outwidth = 6, outheight = 4){
    
    # Adding options for colors
    colRainbow <- colorRampPalette(c("gray20", "antiquewhite4", "firebrick3",
                                     "darkorange", "green3", "seagreen3",
                                     "cadetblue", "dodgerblue3", "royalblue4",
                                     "darkorchid4"))
    
    blueGreen <- colorRampPalette(c("green3", "seagreen3", "cadetblue", "dodgerblue3",
                                    "royalblue4"))
    
    # Maybe change this to tidyverse column name selection with "!!"?
    names(DF)[names(DF) == category_column] <- "CATCOL"
    names(DF)[names(DF) == value_column] <- "VALCOL"
    
    if(complete.cases(facet_column1)){
        names(DF)[names(DF) == facet_column1] <- "FACETCOL1"
    }
    
    if(complete.cases(facet_column2)){
        names(DF)[names(DF) == facet_column2] <- "FACETCOL2"
    }
    
    DF <- DF %>%
        select(any_of(c("CATCOL", "VALCOL", "FACETCOL1", "FACETCOL2")))
    
    # Building graph layers
    if(color_set == "black"){
        G <- ggplot(DF, aes(x = CATCOL, y = VALCOL))
    } else {
        G <- ggplot(DF, aes(x = CATCOL, y = VALCOL,
                            fill = CATCOL, color = CATCOL))
    }
    
    G <- G + theme(legend.position = "none")
    
    if(include_errorbars == TRUE){
        G <- G + stat_boxplot(geom = "errorbar", color = "black")
    }
    
    if(graph_type == "boxplot"){
        G <- G + geom_boxplot(color = "black")
    }
    
    if(graph_type == "jittered points"){
        
        JitterWidth = 0.5/length(unique(DF$CATCOL))
        
        G <- G + geom_boxplot(color = "black", fill = NA,
                              outlier.shape = NA) +
            geom_point(position = position_jitter(width = JitterWidth,
                                                  height = 0))
    }
    
    if(graph_type == "jittered points, filled boxes"){
        JitterWidth = 0.5/length(unique(DF$CATCOL))
        
        G <- G + geom_boxplot(color = "black", outlier.shape = NA) +
            geom_point(position = position_jitter(width = JitterWidth,
                                                  height = 0),
                       shape = 1, color = "black")
    }
    
    if(complete.cases(facet_column1)){
        if(is.na(facet_column2)){
            G <- G + facet_wrap(~ FACETCOL1)
        } else {
            G <- G + facet_grid(FACETCOL1 ~ FACETCOL2)
        }
    }
    
    if(complete.cases(xlabel)){
        G <- G + xlab(xlabel)
    } else {
        G <- G + xlab(category_column)
    }
    
    if(complete.cases(ylabel)){
        G <- G + ylab(ylabel)
    } else {
        G <- G + ylab(value_column)
    }
    
    # Adding some aesthetic preferences
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
        text = element_text(family = "Calibri"),
        legend.background = element_rect(color=NA, fill=NA),
        legend.key = element_rect(color=NA, fill=NA))
    
    if(color_set == "default"){
        G <- G + scale_color_brewer(palette = "Set1") +
            scale_fill_brewer(palette="Set1")
    }
    
    if(color_set == "blue-green"){
        G <- G + scale_color_manual(
            values = blueGreen(length(unique(DF$CATCOL)))) +
            scale_fill_manual(
                values = blueGreen(length(unique(DF$CATCOL))))
    }
    
    if(color_set == "rainbow"){
        G <- G + scale_color_manual(
            values = colRainbow(length(unique(DF$CATCOL)))) +
            scale_fill_manual(
                values = colRainbow(length(unique(DF$CATCOL))))
    }
    
    if(color_set == "Brewer set 2"){
        G <- G + scale_fill_brewer(palette = "Set2") +
            scale_color_brewer(palette = "Set2")
    }
    
    if(color_set == "Tableau"){
        G <- G + ggthemes::scale_color_tableau() +
            ggthemes::scale_fill_tableau()
    }
    
    if(complete.cases(filename)){
        print(G)
        ggsave(filename, height = outheight, width = outwidth, dpi = 600)
    }
    
    return(G)
    
}



