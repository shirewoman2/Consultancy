#' Make boxplots or boxplots overlaid with individual points
#'
#' \code{graph_boxplot} takes as input a data.frame of numeric values and the
#' categories those values are in and creates boxplots of those data. Options
#' are included for type of graph to make and some aesthetics.
#'
#' @param DF the data.frame you want to graph, unquoted
#' @param category_column the name of the column with categorical data, unquoted
#' @param value_column the name of the column with value data, unquoted
#' @param facet1_column (optional) the name of a column by which you might break
#'   up your graph into small multiples, unquoted. Please see the example if
#'   you're uncertain what this does.
#' @param facet2_column (optional) the name of a second column by which you
#'   might break up the graph into small multiples, unquoted.
#' @param graph_type the type of graph to plot. Options: \describe{
#'   \item{"boxplot"}{(default) standard boxplots, aka box-and-whisker plots}
#'
#'   \item{"jittered points"}{boxplots overlaid with points depicting each
#'   individual observation.}
#'
#'   \item{"jittered points, filled boxes"}{boxplots with fill color according
#'   to \code{category_column} and open circles for the jittered points}}
#' @param include_errorbars TRUE or FALSE (default) on whether to include
#'   horizontal error bars on the whiskers
#' @param x_axis_label optionally supply a character vector or an expression to
#'   use for the x axis label
#' @param y_axis_label optionally supply a character vector or an expression to
#'   use for the y axis label
#' @param color_set the set of colors to use. Options: \describe{
#'
#'   \item{"default"}{a set of colors from Cynthia Brewer et al. from Penn State
#'   that are friendly to those with red-green colorblindness. The first three
#'   colors are green, orange, and purple. This can also be referred to as
#'   "Brewer set 2".}
#'
#'   \item{"Brewer set 1"}{colors selected from the Brewer palette "set 1". The
#'   first three colors are red, blue, and green.}
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
#'   colorblindness", according to the package author}}
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png". If you leave off ".png", it
#'   will be saved as a png file, but if you specify a different file extension,
#'   it will be saved as that file format. Acceptable extensions are "eps",
#'   "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg". Do not include any slashes, dollar signs, or periods in the file name. Leaving this as NA
#'   means the file will not be automatically saved to disk.
#' @param fig_height figure height in inches; default is 4
#' @param fig_width figure width in inches; default is 6
#'
#' @return a ggplot2 graph
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
                          x_axis_label = NA,
                          y_axis_label = NA,
                          color_set = "default",
                          save_graph = NA,
                          fig_width = 6, fig_height = 4){
    
    
    # Error catching ----------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop(paste0(wrapn("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), "\n     library(tidyverse)\n\nand then try again."), call. = FALSE)
    }
    
    # Setting things up for nonstandard evaluation -------------------------
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
    
    if(complete.cases(x_axis_label)){
        G <- G + xlab(x_axis_label)
    } else {
        G <- G + xlab(rlang::as_label(category_column))
    }
    
    if(complete.cases(y_axis_label)){
        G <- G + ylab(y_axis_label)
    } else {
        G <- G + ylab(rlang::as_label(value_column))
    }
    
    # Adding some aesthetic preferences ------------------------------------
    G <- G +
        theme_consultancy() +
        theme(
            panel.grid.minor.y = element_line(color = NA),
            panel.grid.minor.x = element_line(color = NA),
            panel.grid.major = element_line(colour = NA),
            panel.border = element_rect(color="black", fill=NA))
    
    # Adding options for colors
    NumColors <- length(unique(DF %>% pull(!!category_column)))
    
    if(color_set == "default"){
        # Using "Dark2" b/c "Set2" is just really, really light. 
        G <- G + scale_color_brewer(palette = "Dark2") +
            scale_fill_brewer(palette="Dark2")
    }
    
    if(color_set == "blue-green"){
        G <- G + 
            scale_color_manual(values = blueGreens(NumColors)) +
            scale_fill_manual(values = blueGreens(NumColors))
    }
    
    if(color_set == "blues"){
        G <- G + 
            scale_color_manual(values = blues(NumColors)) +
            scale_fill_manual(values = blues(NumColors))
    }
    
    if(color_set == "rainbow"){
        G <- G + scale_color_manual(values = rainbow(NumColors)) +
            scale_fill_manual(values = rainbow(NumColors))
    }
    
    if(str_detect(tolower(color_set), "brewer.*2|set.*2")){
        # Using "Dark2" b/c "Set2" is just really, really light. 
        G <- G + scale_fill_brewer(palette = "Dark2") +
            scale_color_brewer(palette = "Dark2")
    }
    
    if(str_detect(tolower(color_set), "brewer.*1|set.*1")){
        G <- G + scale_fill_brewer(palette = "Set1") +
            scale_color_brewer(palette = "Set1")
    }
    
    if(color_set == "Tableau"){
        G <- G + ggthemes::scale_color_tableau() +
            ggthemes::scale_fill_tableau()
    }
    
    if(color_set == "viridis"){
        G <- G + viridis::scale_color_viridis(discrete = TRUE) +
            viridis::scale_fill_viridis(discrete = TRUE)
    }
    
    if(complete.cases(save_graph)){
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
        }
        
        ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
               plot = G)
    }
    
    return(G)
    
}



