#' @export
runShiny <- function() {
    appDir <- system.file("shiny", "pk_plot_table", package = "SimcypConsultancy")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `SimcypConsultancy`.", call. = FALSE)
    }
    
    shiny::runApp(appDir, display.mode = "normal")

}