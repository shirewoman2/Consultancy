#' Extract experimental details for multiple files at once
#'
#' \code{extractExpDetails_mult} takes a character vector of simulator output
#' files -- or all the Excel files in the current directory if no files are
#' specified -- and collects experimental details for the simulations into a
#' single table. It optionally saves that table to a csv or Excel file.
#'
#' @param sim_data_files a character vector of simulator output files or NA to
#'   extract experimental details for all the Excel files in the current folder.
#'   Example of acceptable input: \code{c("sim1.xlsx", "sim2.xlsx")}. Note that,
#'   if some of your Excel files are not regular simulator output, e.g. they are
#'   sensitivity analyses or a file where you were doing some calculations, this
#'   will result in an error and return no data.
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
#'   \item{"Simcyp inputs"}{Extract all the details that you normally fill out
#'   on the "Simcyp inputs (and QC)" tab of a compound data sheet}
#'
#'   \item{"all"}{Extract all possible parameters (default). This is the slowest
#'   option in terms of processing time because it must read multiple Excel
#'   tabs.}
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
#' @param save_output optionally save the output by supplying a file name in
#'   quotes here, e.g., "My experimental details.csv". If you leave off ".csv",
#'   it will still be saved as a csv file.
#'
#' @return Returns a data.frame of experimental details for simulator files
#' @import tidyverse
#' @import xlsx
#'
#' @export
#'
#' @examples
#'
#' extractExpDetails_mult(
#'     sim_data_files =
#'         c("Example simulator output - SD MDZ + MD RTV.xlsx",
#'           "Example simulator output - MDZ + metabolites.xlsx",
#'           "Example simulator output - met1 met2 sec met1 inhib1.xlsx",
#'           "Example simulator output - met1 met2 sec met1.xlsx"),
#'     exp_details = "all",
#'     save_output = "My experimental details.csv")
#'  
extractExpDetails_mult <- function(sim_data_files = NA, 
                                   exp_details = "all", 
                                   save_output = NA){
    
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
    
    if(complete.cases(save_output)){
        if(str_detect(save_output, "\\.")){
            FileName <- sub("\\..*", ".csv", save_output)
        } else {
            FileName <- paste0(save_output, ".csv")
        }
        write.csv(MyDeets, FileName, row.names = F)
    }
    
    return(MyDeets)
}


