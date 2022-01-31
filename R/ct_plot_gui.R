#' Run the ct_plot function through a graphical user interface
#'
#' Opens the shiny app version of the function \code{\link{ct_plot}}.
#'
#' @return
#' @export
#'
#' @examples
#' # You don't need to specify any arguments to this function
#' # in the console. Just type the following:
#' shiny_ct_plot()
#'
ct_plot_gui <- function(){

      appDir <- system.file("shiny", "ct_plot_app", package = "SimcypConsultancy")

      if (appDir == "") {
            stop("Could not find shiny directory for the SimcypConsultancy package. Please try re-installing 'SimcypConsultancy'.", call. = FALSE)
      }

      shiny::runApp(appDir, display.mode = "normal")
}


