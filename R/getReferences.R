#' Pull references from a "DataRec" folder
#'
#' Get a list of all the pdf, Word, and Excel files in, e.g., the "DataRec"
#' folder and subfolder. Can be useful for the bibliography section of a report.
#'
#' @param main_directory the main directory from which you want a list of all
#'   the files, in quotes; default assumes the main directory is your current working
#'   directory
#' @param save_output optionally save the output by supplying a file name in
#'   quotes here, e.g., "My references.csv". If you leave off ".csv", it will
#'   still be saved as a csv file.
#'
#' @return A data.frame containing columns "Directory" for the directory and,
#'   where applicable, the subdirectory of a file, and "File" for the file name.
#'   Optionally save output to a csv file.
#'
#' @import tidyverse
#' @export
#' @examples
#' getReferences()
#' getReferences(save_output = "My referencs.csv")
#'
#' 
getReferences <- function(main_directory = ".", 
                          save_output = NA){
    
    # Check for "\" b/c people will probably paste the path from Windows
    main_directory <- gsub("\\\\", "/", main_directory)
    
    # If people *did* copy and paste the full path and it includes the "https"
    # part of the share point drive, that doesn't work well. Switch that.
    main_directory <- sub("https:..s08sharepoint.certara.com.sites.consult.",
                          SimcypDir$SharePtDir, main_directory)
    
    RefDirs <- list.dirs(main_directory)
    RefDirs <- RefDirs[!str_detect(RefDirs, "DataRec/Forms")]
    
    Refs <- list()
    
    for(i in RefDirs){
        
        MyFiles <- list.files(i, pattern = "pdf$|doc$|docx$|pptx|xls",
                              include.dirs = FALSE,
                              recursive = FALSE,
                              full.names = FALSE)
        
        if(length(MyFiles) > 0){
            Refs[[i]] <- data.frame(Directory = i,
                                    File = as.character(unlist(MyFiles)) )
        }
        
        rm(MyFiles)
    }
    
    if(length(Refs) == 0){
        stop("No references were found. Please check your directory.",
             call. = FALSE)
    }
    
    Refs <- bind_rows(Refs) %>%
        mutate(Directory = sub("\\.\\./DataRec", "DataRec", Directory)) %>%
        arrange(Directory, File)
    
    if(complete.cases(save_output)){
        if(str_detect(save_output, "\\.")){
            FileName <- sub("\\..*", ".csv", save_output)
        } else {
            FileName <- paste0(save_output, ".csv")
        }
        write.csv(Refs, FileName, row.names = F)
    }
    
    return(Refs)
}


