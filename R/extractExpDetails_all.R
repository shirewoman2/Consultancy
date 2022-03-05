#' Extract experimental details for multiple files at once
#'
#' \code{extractExpDetails_all} takes a list of simulator output files -- or all
#' the Excel files in the current directory if no files are specified -- and
#' collects all the experimental details for the simulations into a single
#' table. It optionally saves that table to a csv or Excel file. 
#'
#' @param sim_data_files a character vector of simulator output files or NA to
#'   extract experimental details for all the Excel files in the current folder.
#'   Note that, if some of your Excel files are not regular simulator output,
#'   e.g. they are sensitivity analyses or a file where you were doing some
#'   calculations, this will result in an error and return no data.
#' @param exp_details Experiment details you want to extract from the simulator
#'   output files using the function \code{\link{extractExpDetails}}. Options
#'   are \describe{
#'
#'   \item{"Summary tab"}{Extract details only from the "Summary tab" (default)}
#'
#'   \item{"Input Sheet"}{Extract details only from the "Input Sheet" tab}
#'
#'   \item{"population tab"}{Extract details about the population used (data
#'   come from the tab with the same name as the population simulated)}
#'
#'   \item{"all"}{Extract all possible parameters (default)}
#'
#'   \item{a string of the specific parameters you want}{For a complete list,
#'   type \code{data(AllExpDetails)} into the console. Parameters are reported
#'   with a suffix depending on which compound they pertain to: "_sub" for the
#'   substrate, "_met1" for the primary metabolite, "_met2" for the second
#'   primary metabolite, "_secmet" for the secondary metabolite, "_inhib" for
#'   the 1st inhibitor or inducer listed, "_inhib2" for the 2nd inhibitor or
#'   inducer listed, or "_inh1met" for the inhibitor 1 metabolite. An example of
#'   acceptable input: \code{c("pKa1_sub", "fa_inhib2", "Regimen_sub")}}}
#'
#' @param out_file the name of the output file to save this information. If left
#'   as NA, no file will be saved. You can enter either a csv (faster) or Excel
#'   file name, e.g., "My experimental details.csv" or "Experimental setup for
#'   my simulations.xlsx".
#'
#' @return Returns a data.frame of experimental details for simulator files
#' @import tidyverse
#' @import xlsx
#'
#' @export
#'
#' @examples
#'
#' # No examples yet
#' 
extractExpDetails_all <- function(sim_data_files = NA, 
                                  exp_details = "all", 
                                  out_file = NA){
    
    if(length(sim_data_files) == 1 && is.na(sim_data_files)){
        sim_data_files <- list.files(pattern = "xlsx")
    }
    
    MyDeets <- list()
    
    for(i in sim_data_files){
        MyDeets[[i]] <- extractExpDetails(sim_data_file = i, 
                                          exp_details = exp_details) %>% 
            as.data.frame() %>% 
            mutate(File = i) %>% 
            select(File, everything())
    }
    
    MyDeets <- bind_rows(MyDeets)
    
    if(complete.cases(out_file)){
        if(str_detect(out_file, "csv")){
            write.csv(MyDeets, file = out_file, row.names = F)
        } else {
            xlsx::write.xlsx(MyDeets, file = out_file, sheetName = "details")
        }
    }
    
    return(MyDeets)
}


