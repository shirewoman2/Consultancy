#' Harmonize user input for PK parameters
#' 
#' INTERNAL PACKAGE USE
#'
#' @param PKparameters user input for PK parameters that might not perfectly
#'   follow required syntax for PK parameters from \code{AllPKParameters}
#'
#' @return harmonized PK parameters
#'
#' @examples
#' # None
#' 
harmonize_PK_names <- function(PKparameters){
   PKparameters <- sub("_ss", "_last", PKparameters)
   PKparameters <- sub("_first", "_dose1", PKparameters)
   PKparameters <- sub("AUCt_last", "AUCtau_last", PKparameters)
   PKparameters <- sub("AUCtau_dose1", "AUCt_dose1", PKparameters)
   PKparameters <- sub("AUCt_ratio_last", "AUCtau_ratio_last", PKparameters)
   
   return(PKparameters)
}


