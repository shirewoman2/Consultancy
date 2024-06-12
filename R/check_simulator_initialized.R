#' check whether the Simcyp Simulator has been initialized
#'
#' INTERNAL. This is only for checking whether Simulator has been initialized
#' for functions that require the Simcyp package.
#'
#' @param verbose
#'
#' @return TRUE or FALSE for whether the Simcyp Simulator has been initialized
#'   with Simcyp::Initialise.
#'
#' @examples
#' check_simulator_initialized()
#' 
check_simulator_initialized <- function(){
   try({
      output <- .Call("R_IsInitialised", PACKAGE = "Simcyp")
      return(output)
   })
}

