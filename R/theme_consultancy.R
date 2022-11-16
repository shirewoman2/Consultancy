#' Apply the standard Consultancy Team graphing styles to your graph
#'
#' \code{theme_consultancy} sets the following: \itemize{ \item{Plot background
#' is white; this includes the background for the legend and any facets and
#' their labels.} \item{Axis ticks, lines, text, and titles are black.} \item{If
#' it's a concentration-time plot or an enzyme-abundance plot that does
#' \emph{not} have any facets, then there will be a line on the x axis and a
#' line on the y axis but no overall border around the graph.} \item{If the
#' graph is facetted, the labels for the facets are not enclosed with a border.}
#' \item{Axis titles and, when applicable, the plot title are bold face.}
#' \item{Font sizes and margins are the same as in the ggplot2 theme
#' "theme_bw"}}
#'
#' @return
#' @export
#'
#' @examples
#' # Some example data to work with
#' MyData <- data.frame(X = rnorm(10), Y = runif(10))
#'
#' # Without applying this theme to a standard ggplot2 graph
#' ggplot(MyData, aes(x = X, y = Y)) + geom_point()
#'
#' # With application of this theme
#' ggplot(MyData, aes(x = X, y = Y)) + geom_point() +
#'        theme_consultancy()
#' 
theme_consultancy <- function(){
    theme(text = element_text(size = 11),
          rect = element_rect(fill = "white", color = "black"),
          plot.background = element_rect(fill="white", colour=NA),
          plot.title = element_text(face = "bold", size = rel(1.2), 
                                    margin = margin(0, 0, 5.5, 0)),
          panel.background = element_rect(fill="white", color=NA),
          strip.background = element_rect(color=NA, fill="white"),
          strip.text = element_text(size = rel(0.8),
                                    margin = margin(4.4, 4.4, 4.4, 4.4)),
          legend.key = element_rect(fill = "white", color = NA),
          legend.background = element_rect(color=NA, fill=NA),
          legend.text = element_text(size = rel(0.8)),
          axis.ticks = element_line(color = "black"),
          axis.text = element_text(color = "black",
                                   size = rel(0.8)),
          axis.text.x = element_text(margin = margin(2.2, 0, 0, 0)),
          axis.text.x.top = element_text(margin = margin(0, 0, 2.2, 0)),
          axis.text.y = element_text(margin = margin(0, 2.2, 0, 0)),
          axis.text.y.right = element_text(margin = margin(0, 0, 0, 2.2)),
          axis.title = element_text(color = "black", face = "bold"),
          axis.title.x = element_text(margin = margin(2.75, 0, 0, 0)),
          axis.title.x.top = element_text(margin = margin(0, 0, 2.75, 0)),
          axis.title.y = element_text(margin = margin(0, 2.75, 0, 0)),
          axis.title.y.right = element_text(margin = margin(0, 0, 0, 2.75)),
          axis.line.x.bottom = element_line(color = "black"),
          axis.line.y.left = element_line(color = "black"))
}

