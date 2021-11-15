
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
#'   \item{"boxplot with error bars"}{standard boxplots or box-and-whisker plots
#'   plus error bars on the whiskers}
#'
#'   \item{"jittered points"}{boxplots overlaid with points depicting each
#'   individual observation.}}
#' @param xlabel the label to  use for the x axis
#' @param ylabel the label to use for the y axis
#' @param color the set of colors to use. Options: \describe{
#'   \item{"default"}{colors selected from the color brewer palette "set 1"}
#'   \item{"black"}{black and white} \item{"rainbow"}{colors selected from a
#'   rainbow palette. The default palette is limited to something like 6 groups,
#'   so if you have more than that, that's when this palette is most useful.}
#'   \item{"blue-green"}{a set of blues and greens} \item{"Tableau"}{uses the
#'   standard Tableau palette; requires the "ggthemes" package}}
#'
#' @return
#' @import tidyverse
#' @import ggthemes
#' @export
#'
#' @examples
#' AUCs <- data.frame(AgeGroup = LETTERS[1:5],
#'                    AUC = rnorm(n = 500, mean = 20000, sd = 2000))
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC")
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'                xlabel = "Age group", graph_type = "boxplot with error bars")
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'                color = "rainbow")
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'                color = "blue-green", graph_type =  "jittered points")
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'               color = "Tableau", graph_type =  "jittered points")
#'
#' graph_boxplot(AUCs %>% filter(AgeGroup %in% c("A", "B", "C")),
#'                category_column = "AgeGroup", value_column = "AUC",
#'                graph_type =  "jittered points")
#'
#' # Adding a couple of example columns to use the "facet" options.
#' AUCs$Sex <- c("M", "F")
#' AUCs$Metabolizer <- sample(c("poor", "extensive"), 100, replace = TRUE)
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'                facet_column1 = "Sex",
#'                color = "blue-green", graph_type =  "jittered points")
#'
#' graph_boxplot(AUCs, category_column = "AgeGroup", value_column = "AUC",
#'                facet_column1 = "Sex", facet_column2 = "Metabolizer",
#'                color = "blue-green", graph_type =  "jittered points")
#'
#'
graph_boxplot <- function(DF,
                           category_column,
                           value_column,
                           facet_column1 = NA,
                           facet_column2 = NA,
                           graph_type = "boxplot",
                           xlabel = NA,
                           ylabel = NA,
                           color = "default"){

      # Adding a couple more options for colors
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
      if(color == "black"){
            G <- ggplot(DF, aes(x = CATCOL, y = VALCOL))
      } else {
            G <- ggplot(DF, aes(x = CATCOL, y = VALCOL,
                                fill = CATCOL, color = CATCOL))
      }

      G <- G + theme(legend.position = "none")

      if(graph_type == "boxplot"){
            G <- G + geom_boxplot(color = "black")
      }

      if(graph_type == "boxplot with error bars"){
            G <- G + stat_boxplot(geom = "errorbar", color = "black") +
                  geom_boxplot(color = "black")

      }

      if(graph_type == "jittered points"){

            JitterWidth = 0.5/length(unique(DF$CATCOL))

            G <- G + geom_boxplot(color = "black", fill = NA,
                                  outlier.shape = NA) +
                  geom_point(position = position_jitter(width = JitterWidth,
                                                        height = 0))
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
            legend.background = element_rect(color=NA, fill=NA),
            legend.key = element_rect(color=NA, fill=NA))

      if(color == "default"){
            G <- G + scale_fill_brewer(palette="Set1")
      }

      if(color == "blue-green"){
            G <- G + scale_color_manual(
                  values = blueGreen(length(unique(DF$CATCOL)))) +
                  scale_fill_manual(
                        values = blueGreen(length(unique(DF$CATCOL))))
      }

      if(color == "rainbow"){
            G <- G + scale_color_manual(
                  values = colRainbow(length(unique(DF$CATCOL)))) +
                  scale_fill_manual(
                        values = colRainbow(length(unique(DF$CATCOL))))
      }

      if(color == "Tableau"){
            G <- G + ggthemes::scale_color_tableau()
      }

      return(G)

}



