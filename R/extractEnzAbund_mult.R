#' Pull enzyme-abundance data from multiple Simcyp Simulator output files
#'
#' \code{extractEnzAbund_mult} is meant to be used in conjunction with
#' \code{\link{enz_plot_overlay}} to create graphs from multiple Simcyp
#' Simulator output files or from multiple enzymes or tissues. If you list
#' multiple files, multiple tissues, and/or multiple enzymes to extract (see
#' notes on options below), this will extract \emph{all} possible variations of
#' them. For example, if you ask for data from the files "sim1.xlsx" and
#' "sim2.xlsx" and then also ask for the enzymes "CYP3A4" and "CYP2C9", you will
#' get the CYP3A4 and CYP2C9 abundance data from \emph{both} files.
#' \strong{NOTE:} If ANY of the Excel files you wish to extract data from are
#' open, this WILL CRASH and WILL NOT save whatever progress it has made so far.
#' Be sure to close all of the source Excel files.
#'
#' @param sim_data_files a character vector of simulator output files, each in
#'   quotes and encapsulated with \code{c(...)}, NA to extract enzyme-abundance
#'   data for \emph{all} the Excel files in the current folder, or "recursive"
#'   to extract enzyme-abundance data for \emph{all} the Excel files in the
#'   current folder and \emph{all} subfolders. Example of acceptable input:
#'   \code{c("sim1.xlsx", "sim2.xlsx")}. The path should be included with the
#'   file names if they are located somewhere other than your working directory.
#'   If some of your Excel files are not regular simulator output, e.g. they are
#'   sensitivity analyses or a file where you were doing some calculations,
#'   those files will be skipped.
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get all the details from the "Input
#'   Sheet" (e.g., when you ran extractExpDetails_mult you said
#'   \code{exp_details = "Input Sheet"} or \code{exp_details = "all"}), you can
#'   save some processing time by supplying that object here, unquoted. If left
#'   as NA, this function will run \code{extractExpDetails} behind the scenes to
#'   figure out some information about your experimental set up.
#' @param sim_enz_dataframe (optional) a data.frame that contains previously
#'   extracted enzyme-abundance data. This should NOT be in quotes. Because we
#'   can see scenarios where you might want to extract some enzyme-abundance
#'   data, play around with those data, and then later decide you want to pull
#'   more enzyme-abundance data for comparisons, this data.frame can already
#'   exist. When that is the case, this function will \emph{add} data to that
#'   data.frame. It will \emph{not} overwrite existing data unless
#'   \code{overwrite} is set to TRUE.
#' @param overwrite TRUE or FALSE (default) on whether to re-extract the
#'   enzyme-abundance data from output files that are already included in
#'   \code{sim_enz_dataframe}. Since pulling data from Excel files is slow, by
#'   default, this will \emph{not} overwrite existing data and instead will only
#'   add data from any Excel files that aren't already included. A situation
#'   where you might want to set this to TRUE would be when you have changed
#'   input parameters for simulations and re-run them.
#' @param enzymes the enzymes of interest, e.g., "CYP3A4" (default), "UGT1A1",
#'   etc. Any enzyme present in the simulator output should work fine. To
#'   request multiple enzymes, enclose them in \code{c(...)}, e.g.,
#'   \code{enzymes = c("CYP3A4", "CYP2D6", "CYP2C19")}. Spaces or hyphens in
#'   enzyme names will be ignored. Not case sensitive.
#' @param tissues From which tissues should the desired enzyme abundances be
#'   extracted? Options are "liver" (default), "gut", or "kidney". Note: If
#'   "gut" is selected, the output will return both colon and small intestine
#'   concentrations. To request multiple tissues, enclose them in \code{c(...)},
#'   e.g., \code{tissues = c("liver", "gut", "kidney")}
#' @param time_units_to_use time units to use so that all data will be
#'   comparable. Options are "hours" (default) or "minutes".
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated
#'   enzyme-abundance data? Options are "aggregate" (default), "individual", or
#'   "both". Aggregated data are not calculated here but are pulled from the
#'   simulator output rows labeled as "Population Statistics".
#' @param fromMultFunction INTERNAL USE ONLY. TRUE or FALSE on whether this is
#'   being called on by \code{\link{extractConcTime_mult}}.
#'
#' @return Returns a large data.frame with multiple sets of enzyme-abundance
#'   data, formatted the same way as output from the function
#'   \code{\link{extractEnzAbund}}
#' @export
#'
#' @examples
#' ConcTimeData <-
#'       extractEnzAbund_mult(
#'             sim_data_files = c("MyFile1.xlsx", "MyFile2.xlsx"),
#'             overwrite = FALSE,
#'             enzymes = c("CYP3A4", "CYP2C9"),
#'             tissues = c("liver", "gut", "kidney"))
#' 

extractEnzAbund_mult <- function(sim_data_files = NA,
                                 sim_enz_dataframe = EnzAbund,
                                 overwrite = FALSE,
                                 enzymes = "CYP3A4",
                                 tissues = "liver",
                                 time_units_to_use = "hours",
                                 returnAggregateOrIndiv = "aggregate", 
                                 existing_exp_details = NA, 
                                 fromMultFunction = FALSE, 
                                 ...){
    
    # Error catching -------------------------------------------------------
    
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    # Checking whether they've supplied extractConcTime args instead of
    # extractConctTime_mult args
    if("sim_data_file" %in% names(match.call()) &
       "sim_data_files" %in% names(match.call()) == FALSE){
        sim_data_files <- sys.call()$sim_data_file
    }
    
    if("enzyme" %in% names(match.call()) &
       "enzymes" %in% names(match.call()) == FALSE){
        enzymes <- sys.call()$enzyme
    }
    
    if("tissue" %in% names(match.call()) &
       "tissues" %in% names(match.call()) == FALSE){
        tissues <- sys.call()$tissue
    }
    
    
    # Main body of function -----------------------------------------------
    
    enzymesToExtract <- toupper(enzymes)
    
    sim_data_files <- unique(sim_data_files)
    
    # If user did not supply files, then extract all the files in the current
    # folder that end in "xlsx" or in all subfolders if they wanted it to be
    # recursive.
    if(length(sim_data_files) == 1 &&
       (is.na(sim_data_files) | sim_data_files == "recursive")){
       sim_data_files <- list.files(pattern = "xlsx$",
                                    recursive = (complete.cases(sim_data_files) &&
                                                    sim_data_files == "recursive"))
       sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
    }
    
    # Checking on what combinations of data the user has requested and what
    # data are already present in sim_enz_dataframe.
    Requested <- expand.grid(Tissue = tissues,
                             Enzyme = enzymesToExtract,
                             File = sim_data_files)
    
    if(exists(substitute(sim_enz_dataframe)) && 
       "data.frame" %in% class(sim_enz_dataframe) && 
       nrow(sim_enz_dataframe) > 0){
        if("File" %in% names(sim_enz_dataframe) == FALSE){
            sim_enz_dataframe$File <- "unknown file"
        }
        
        sim_enz_dataframe <- sim_enz_dataframe %>%
            mutate(ID = paste(File, Tissue, Enzyme))
        
        DataToFetch <- sim_enz_dataframe %>% select(File, Tissue, Enzyme) %>%
            unique() %>% mutate(ExistsAlready = TRUE) %>%
            right_join(Requested) %>%
            filter(is.na(ExistsAlready)) %>% select(-ExistsAlready) %>%
            mutate(ID = paste(File, Tissue, Enzyme))
        
        if(overwrite == FALSE){
            sim_data_files_topull <- unique(DataToFetch$File)
        } else {
            sim_data_files_topull <- sim_data_files
            sim_enz_dataframe <- sim_enz_dataframe %>%
                filter(!ID %in% DataToFetch$ID)
        }
    } else {
        DataToFetch <- Requested
        sim_data_files_topull <- sim_data_files
        sim_enz_dataframe <- data.frame()
    }
    
    if(length(sim_data_files_topull) == 0){
        message("There are no data to pull that are not already present in your current data.frame. Returning current data.frame.")
        return(sim_enz_dataframe)
    }
    
    
    ## Start of loop through files -------------------------------------------
    MultData <- list()
    
    for(ff in sim_data_files_topull){
        message(paste("Extracting data from file =", ff))
        MultData[[ff]] <- list()
        
        # Getting summary data for the simulation(s)
        if(class(existing_exp_details) == "logical"){ # logical when user has supplied NA
            Deets <- extractExpDetails(ff, exp_details = "Input Sheet")
        } else {
            Deets <- switch(as.character("File" %in% names(existing_exp_details)), 
                            "TRUE" = existing_exp_details, 
                            "FALSE" = deannotateDetails(existing_exp_details)) 
            
            if("data.frame" %in% class(Deets)){
                Deets <- Deets %>% filter(File == ff)
                
                if(nrow(Deets == 0)){
                    Deets <- extractExpDetails(sim_data_file = ff, 
                                               exp_details = "Input Sheet")
                }
            }
        }
        
        if(length(Deets) == 0){
            # Using "warning" instead of "stop" here b/c I want this to be able to
            # pass through to other functions and just skip any files that
            # aren't simulator output.
            warning(paste("The file", ff,
                          "does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file."), 
                    call. = FALSE)
            next()
        }
        
        # Each tissue will be on its own sheet in the Excel file, so each
        # will need their own iterations of the loop for reading different
        # sheets.
        for(j in tissues){
            
            message(paste("Extracting data for tissue =", j))
            
            MultData[[ff]][[j]] <- list()
            
            for(k in enzymesToExtract){
                
                message(paste("Extracting data for enzyme =", k))
                
                MultData[[ff]][[j]][[k]] <- extractEnzAbund(
                    sim_data_file = ff,
                    enzyme = k,
                    tissue = j,
                    returnAggregateOrIndiv = returnAggregateOrIndiv, 
                    existing_exp_details = Deets)
                
            } # end of enzyme k loop
            
            MultData[[ff]][[j]] <- bind_rows(MultData[[ff]][[j]])
            
        } # end of tissue j loop
        
        MultData[[ff]] <- bind_rows(MultData[[ff]])
        
        # When the particular combination of enzyme and tissue is not
        # available in that file, extractEnzAbund will return an empty
        # data.frame, which we don't want to be included in the final
        # data. Not adding info for File in that scenario b/c it would
        # add a row to what would have been an empty data.frame.
        if(nrow(MultData[[ff]]) > 0){
            MultData[[ff]] <- MultData[[ff]] %>% mutate(File = ff)
        }
        
        # MUST remove Deets or you can get the wrong info for each file!!!
        rm(Deets) 
        
    } # end of file ff loop
    
    MultData <- bind_rows(MultData)
    
    # all data together -------------------------------------------------
    sim_enz_dataframe <- bind_rows(sim_enz_dataframe, MultData) %>% 
        arrange(across(any_of(c("File", "Enzyme", "Inhibitor",
                                "Individual", "Trial", "Time")))) %>%
        select(any_of(c("Enzyme", "Tissue", "Inhibitor", "Species",
                        "Individual", "Trial", "Time", "Abundance",
                        "Time_units", "DoseNum_sub", "DoseNum_inhib1", 
                        "DoseNum_inhib2", "File")))
    
    if("DoseNum_inhib1" %in% names(sim_enz_dataframe) && 
       all(is.na(sim_enz_dataframe$DoseNum_inhib1))){
       sim_enz_dataframe <- sim_enz_dataframe %>% select(-DoseNum_inhib1)
    }
    
    if("DoseNum_inhib2" %in% names(sim_enz_dataframe) && 
       all(is.na(sim_enz_dataframe$DoseNum_inhib2))){
       sim_enz_dataframe <- sim_enz_dataframe %>% select(-DoseNum_inhib2)
    }
    
    
    
    return(sim_enz_dataframe)
    
}



