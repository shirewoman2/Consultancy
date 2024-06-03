#' Launch the shiny app for showing an annotated index of what the
#' SimcypConsultancy package can do
#'
#' \code{launch_package_index} will launch a graphical user interface for
#' showing various things you can do with the SimcypConsultancy package.
#'
#' @export
#'
#' @examples
#' launch_package_index()
#' 
launch_package_index <- function() {
   appDir <- system.file("shiny", "SimcypConsultancy_index", package = "SimcypConsultancy")
   if (appDir == "") {
      stop("Could not find example directory. Try re-installing `SimcypConsultancy`.", call. = FALSE)
   }
   
   shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
   
}