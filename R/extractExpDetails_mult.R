#' Extract experimental details for multiple files at once
#'
#' \code{extractExpDetails_mult} takes a character vector of simulator output
#' files -- or all the Excel files in the current directory if no files are
#' specified -- and collects experimental details for the simulations into a
#' single table. It optionally saves that table to a csv or Excel file.
#'
#' @param sim_data_files a character vector of simulator output files, each in
#'   quotes and encapsulated with \code{c(...)}, or NA to extract experimental
#'   details for \emph{all} the Excel files in the current folder. Example of
#'   acceptable input: \code{c("sim1.xlsx", "sim2.xlsx")}. Note that, if some of
#'   your Excel files are not regular simulator output, e.g. they are
#'   sensitivity analyses or a file where you were doing some calculations, this
#'   will result in an error and return no data.
#' @param exp_details experimental details you want to extract from the
#'   simulator output files using the function \code{\link{extractExpDetails}}.
#'   Options are \describe{
#'
#'   \item{"Summary tab"}{Extract details only from the "Summary tab"}
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
#'   \item{a string of the specific parameters you want, each in quotes and
#'   encapsulated with \code{c(...)},}{For a complete list, type
#'   \code{data(ExpDetailDefinitions); view(ExpDetailDefinitions)} into the
#'   console. Parameters are reported with a suffix depending on which compound
#'   they pertain to: "_sub" for the substrate, "_met1" for the primary
#'   metabolite, "_met2" for the second primary metabolite, "_secmet" for the
#'   secondary metabolite, "_inhib" for the 1st inhibitor or inducer listed,
#'   "_inhib2" for the 2nd inhibitor or inducer listed, or "_inh1met" for the
#'   inhibitor 1 metabolite. An example of acceptable input: \code{c("pKa1_sub",
#'   "fa_inhib2", "Regimen_sub")}}}
#'
#' @param save_output optionally save the output by supplying a file name in
#'   quotes here, e.g., "My experimental details.csv". If you leave off ".csv",
#'   it will still be saved as a csv file.
#' @param existing_exp_details (optional) a data.frame that contains previously
#'   extracted experimental details. This should NOT be in quotes. Because we
#'   can see scenarios where you might want to extract some experimental details
#'   and then run more simulations for comparisons, this function will
#'   \emph{add} data to that data.frame. It will \emph{not} overwrite existing
#'   data unless \code{overwrite} is set to TRUE.
#' @param overwrite TRUE or FALSE (default) on whether to re-extract the
#'   experimental details from output files that are already included in
#'   \code{existing_exp_details}. Since pulling data from Excel files is slow,
#'   by default, this will \emph{not} overwrite existing data and instead will
#'   only add data from any Excel files that aren't already included. A
#'   situation where you might want to set this to TRUE would be when you have
#'   changed input parameters for simulations and re-run them OR when you have
#'   extracted only some of the possible experimental details and you now would
#'   like more experimental details from each simulator output file.
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
                                   existing_exp_details = Deets, 
                                   overwrite = FALSE,
                                   save_output = NA){
    
    # If user did not supply files, then extract all the files in the current
    # folder that end in "xlsx".
    if(length(sim_data_files) == 1 && is.na(sim_data_files)){
        sim_data_files <- list.files(pattern = "xlsx$")
    }
    
    # print(quo_name(enquo(existing_exp_details))) # for bug fixing
    
    if(exists(substitute(existing_exp_details))){
        if(class(existing_exp_details)[1] == "list"){
            existing_exp_details <- bind_rows(existing_exp_details)
        }
        
        if("data.frame" %in% class(existing_exp_details)){
            if("File" %in% names(existing_exp_details) == FALSE){
                existing_exp_details$File <- "unknown file"
            }
            
            if(overwrite == FALSE){
                sim_data_files_topull <- setdiff(sim_data_files, 
                                                 existing_exp_details$File)
            } else {
                sim_data_files_topull <- sim_data_files
                existing_exp_details <- existing_exp_details %>%
                    filter(!File %in% existing_exp_details$File)
            }
        }
        
    } else {
        sim_data_files_topull <- sim_data_files
    }
    
    MyDeets <- list()
    
    for(i in sim_data_files_topull){
        message(paste("Extracting data from file =", i))
        MyDeets[[i]] <- extractExpDetails(sim_data_file = i, 
                                          exp_details = exp_details) %>% 
            as.data.frame() %>% 
            mutate(File = i) %>% 
            select(File, everything())
    }
    
    MyDeets <- bind_rows(MyDeets)
    
    if(exists(substitute(existing_exp_details))){
        MyDeets <- bind_rows(MyDeets, existing_exp_details)
    }
    
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


