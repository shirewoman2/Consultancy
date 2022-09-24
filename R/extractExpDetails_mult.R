#' Extract experimental details for multiple files at once
#'
#' \code{extractExpDetails_mult} takes a character vector of Simcyp Simulator
#' output files -- or all the Excel files in the current directory if no files
#' are specified -- and collects experimental details for the simulations into a
#' single table. It optionally saves that table to a csv or Excel file.
#'
#' @param sim_data_files a character vector of simulator output files, each in
#'   quotes and encapsulated with \code{c(...)}, or NA to extract experimental
#'   details for \emph{all} the Excel files in the current folder. Example of
#'   acceptable input: \code{c("sim1.xlsx", "sim2.xlsx")}. If some of your Excel
#'   files are not regular simulator output, e.g. they are sensitivity analyses
#'   or a file where you were doing some calculations, those files will be
#'   skipped.
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
#'   \strong{Note:} While information about custom dosing regimens \emph{can} be
#'   extracted by the function \code{\link{extractExpDetails}}, that information
#'   cannot easily be made to fit with the rest of the output for
#'   \code{extractExpDetails_mult}. That's because each simulator file and
#'   compound with a custom-dosing regimen will have its own data.frame with the
#'   time, time units, dose number, dose amount, dose units, and dose route. For
#'   that reason, custom-dosing information will largely be ignored here.
#'
#' @param existing_exp_details (optional) a data.frame that contains previously
#'   extracted experimental details. If this object \emph{does} exist, it should
#'   NOT be in quotes, e.g. \code{existing_exp_details = MyDeets}. Because we
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
#' @param annotate_output TRUE (default) or FALSE on whether to transpose the
#'   rows and columns in the output, making the output table longer instead of
#'   wider, and adding columns to the output for a) which compound the
#'   information pertains to (substrate, inhibitor, etc.), b) which section of
#'   the Simcyp Simulator this detail is found in (physchem, absorption,
#'   distribution, etc.), c) notes describing what the detail is, and d) which
#'   sheet in the Excel file the information was pulled from.
#' @param show_compound_col TRUE, FALSE, or "concatenate" (default) for whether
#'   to include in the results the column "Compound", which is the compound's
#'   specific name in each simulation. Why would you ever omit this? If you have
#'   a compound with a slightly different name across multiple simulations,
#'   e.g., "DrugX" and "Drug X", and "Drug X - reduced Ki", you'll get a new row
#'   for every possible combination of "Compound" and "Detail", which might not
#'   make for easy comparisons. For example, a Ki value for "DrugX" will be in
#'   one row and the same Ki value for "Drug X" will be on a separate row. Try
#'   setting this to TRUE when you have similarly named compounds that really
#'   should be compared and see how that compares to the default. If you set
#'   this to "concatenate", you'll get all the possible compound names together;
#'   for example, you might see "DrugX, Drug X, or Drug X - reduced Ki" listed
#'   as the compound.
#' @param omit_all_missing TRUE or FALSE (default) for whether to omit a detail
#'   if the values are NA for all files
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
                                   existing_exp_details = "none", 
                                   overwrite = FALSE,
                                   annotate_output = TRUE,
                                   show_compound_col = TRUE,
                                   omit_all_missing = FALSE, 
                                   save_output = NA){
    
    # Error catching ---------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    # If user did not supply files, then extract all the files in the current
    # folder that end in "xlsx".
    if(length(sim_data_files) == 1 && is.na(sim_data_files)){
        sim_data_files <- list.files(pattern = "xlsx$")
        sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
    }
    
    # Main body of function ---------------------------------------------------
    
    # print(quo_name(enquo(existing_exp_details))) # for bug fixing
    
    AnyExistingDeets <- exists(deparse(substitute(existing_exp_details)))
    
    if(AnyExistingDeets){
        if(class(existing_exp_details)[1] == "list"){
            existing_exp_details <- bind_rows(existing_exp_details)
        }
        
        if("data.frame" %in% class(existing_exp_details)){
            if(all(c("SimulatorSection", "Sheet") %in% names(Deets))){
                # This is when existing_exp_details has been annotated.
                # Ironically, need to de-annotate here to make this work well
                # with the rest of the function.
                FileOrder <- names(existing_exp_details)[str_detect(names(existing_exp_details), "xlsx")]
                existing_exp_details <- existing_exp_details %>% 
                    select(-any_of(c("SimulatorSection", "Sheet", "Notes",
                                     "CompoundID", "Compound"))) %>% 
                    pivot_longer(cols = -Detail, 
                                 names_to = "File", values_to = "Value") %>% 
                    pivot_wider(names_from = Detail, values_from = Value)
                
            } else if("File" %in% names(existing_exp_details) == FALSE){
                existing_exp_details$File <- paste("unknown file", 1:nrow(existing_exp_details))
                FileOrder <- existing_exp_details$File
            }
            
            if(overwrite == FALSE){
                sim_data_files_topull <- setdiff(sim_data_files, 
                                                 existing_exp_details$File)
                FileOrder <- unique(sim_data_files, existing_exp_details$File)
                
            } else {
                sim_data_files_topull <- sim_data_files
                existing_exp_details <- existing_exp_details %>%
                    filter(!File %in% existing_exp_details$File)
                FileOrder <- unique(sim_data_files, existing_exp_details$File)
            }
            
            FileOrder <- unique(c(FileOrder, sim_data_files_topull))
        }
        
    } else {
        sim_data_files_topull <- sim_data_files
        FileOrder <- sim_data_files
    }
    
    MyDeets <- list()
    CustomDosing <- c()
    
    for(i in sim_data_files_topull){
        message(paste("Extracting data from file =", i))
        MyDeets[[i]] <- extractExpDetails(sim_data_file = i, 
                                          exp_details = exp_details) 
        
        # Checking for custom dosing regimens and removing those data b/c they
        # have a different data structure and cannot easily be coerced into a
        # single row for each simulator file.
        if(any(str_detect(tolower(names(MyDeets[[i]])), "custom"))){
            CustomDosing[i] <- TRUE
            MyDeets[[i]] <- 
                MyDeets[[i]][!str_detect(tolower(names(MyDeets[[i]])), "custom")]
        } else {
            CustomDosing[i] <- FALSE
        }
        
        MyDeets[[i]] <- MyDeets[[i]] %>% 
            as.data.frame() %>% 
            mutate(File = i) %>% 
            select(File, everything())
    }
    
    if(any(CustomDosing) | 
       (AnyExistingDeets && 
        any(sapply(existing_exp_details %>% 
                   select(any_of(c("Dose_sub", "Dose_inhib", 
                                   "DoseInt_sub", "DoseInt_inhib"))), 
                   class) == "character"))){
        for(j in names(MyDeets)){
            MyDeets[[j]] <- MyDeets[[j]] %>% 
                mutate(across(.cols = any_of(c("Dose_sub", "Dose_inhib", 
                                               "DoseInt_sub", "DoseInt_inhib")), 
                              .fns = as.character))
        }
    }
    
    Out <- bind_rows(MyDeets) %>% 
        mutate(File = factor(File, levels = FileOrder)) %>% 
        arrange(File)
    
    if(AnyExistingDeets){
        if(annotate_output | all(sapply(existing_exp_details, class) == "character")){
            Out <- Out %>% mutate(across(.fns = as.character))
        } 
        
        Out <- bind_rows(Out, existing_exp_details)
    }
    
    # Removing anything that was all NA's if that's what user requested
    if(omit_all_missing){
        Keep <- 
            Out %>% summarize(across(.fns = function(.) all(is.na(.)))) %>% 
            pivot_longer(cols = -File, names_to = "ColName", values_to = "Val") %>% 
            filter(Val == FALSE) %>% pull(ColName)
        
        Out <- Out[, c("File", Keep)]
    }
    
    if(annotate_output){
        Out <- annotateDetails(Out, 
                               show_compound_col = show_compound_col, 
                               save_output = NA)
    }
    
    if(complete.cases(save_output)){
        
        if(str_detect(save_output, "\\.")){
            # If they specified a file extension, replace whatever they supplied
            # with csv b/c that's the only option for file format here.
            FileName <- sub("\\..*", ".csv", save_output)
        } else {
            # If they didn't specify file extension, make it csv.
            FileName <- paste0(save_output, ".csv")
        }
        
        write.csv(Out, FileName, row.names = F)
    }
    
    
    return(Out)
}


