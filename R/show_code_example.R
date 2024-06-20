#' IN PROGRESS - Show some examples for the code to use for making PK tables or
#' concentration-time plots
#'
#' @return Doesn't return anything; only shows messages with code examples 
#' @export
#'
#' @examples
#' # none 
show_code_example <- function(){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Main body of function ---------------------------------------------------
   
   message(str_wrap("What would you like to make? "))
   message("1. a PK table\n2. a concentration-time plot")
   Q1 <- readline("   ")
   
   
   switch(as.character(Q1), 
          "1" = message(paste0("pk_table(PKparameters = NA,\n         sim_data_files = c(\"file name A.xlsx\",\n                            \"file name B.xlsx\"),\n        compoundsToExtract = \"substrate\",\n        tissues = \"plasma\")")), 
          "2" = message(str_wrap("ct_plot(ct_dataframe = CT, figure_type = \"percentiles\", save_graph = \"My CT plot.docx\", existing_exp_details = Details)", 
                                 indent = 1, exdent = 3)))
   
}
