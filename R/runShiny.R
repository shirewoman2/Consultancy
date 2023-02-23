#' Launch the shiny app for making PK summary tables and concentration-time
#' plots
#'
#' \code{runShiny} will launch a graphical user interface for making PK summary
#' tables and concentration-time plots. All the arguments will be specified
#' within the app itself.
#'
#' @export
#'
#' @examples
#' runShiny()
#' 
runShiny <- function() {
    appDir <- system.file("shiny", "pk_plot_table", package = "SimcypConsultancy")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `SimcypConsultancy`.", call. = FALSE)
    }
    
    shiny::runApp(appDir, display.mode = "normal")

}