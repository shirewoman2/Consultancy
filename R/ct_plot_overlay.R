#' Overlay multiple data sets onto a single concentration-time graph
#'
#' \code{ct_plot_overlay} is meant to be used in conjunction with
#' \code{\link{extractConcTime_mult}} to create single graphs with overlaid
#' concentration-time data from multiple Simcyp Simulator output files for easy
#' comparisons. UNDER CONSTRUCTION.
#'
#' @param conctime_DF the data.frame with multiple sets of concentration-time
#'   data
#' @param colorBy What column in \code{conctime_DF} should be used for coloring
#'   the lines and/or points on the graph? This should be unquoted, e.g.,
#'   \code{colorBy = Tissue}.
#' @param facet_column1 If you would like to break up your graph into small
#'   multiples, you can break the graphs up by up to two columns in
#'   \code{conctime_DF}. What should be the 1st column to break up the data by?
#'   This should be unquoted.
#' @param facet_column2 What should be the 2nd column to break up the data into
#'   small multiples by? This should be unquoted.
#'
#' @return
#' @export
#'
#' @examples
#' # No examples yet
#'
#'
ct_plot_overlay <- function(conctime_DF,
                            colorBy = File,
                            facet_column1 = NA,
                            facet_column2 = NA){

      colorBy <- rlang::enquo(colorBy)
      facet_column1 <- rlang::enquo(facet_column1)
      facet_column2 <- rlang::enquo(facet_column2)

      # You can leave one column as NA but not 2, so I'm only checking whether
      # facet_column2 is complete. I'm having trouble doing this -- nonstandard
      # evalution is not my forte -- so this was the most reliable method I came
      # up with. -LS
      Facet2_na <- sub("\\~", "", deparse(facet_column2)) == "NA"

      if(Facet2_na){
            ggplot(conctime_DF %>% filter(Trial == "mean"),
                   aes(x = Time, y = Conc, color = !!colorBy)) +
                  geom_line() +
                  facet_wrap(vars(!!facet_column1))
      } else {
            ggplot(conctime_DF %>% filter(Trial == "mean"),
                   aes(x = Time, y = Conc, color = !!colorBy)) +
                  geom_line() +
                  facet_wrap(vars(!!facet_column1, !!facet_column2))
      }
}

# ct_plot_overlay(conctime_DF = conctime_DF, facet_column1 = File, facet_column2 = Tissue)

