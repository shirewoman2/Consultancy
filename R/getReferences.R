#' Pull references from a "DataRec" folder
#'
#' Get a list of all the pdf, Word, Excel, PowerPoint, Simcyp Simulator
#' workspace, XML, jpg, png, or text files in, e.g., the "DataRec" folder and
#' subfolder. Can be useful for the bibliography section of a report.
#'
#' @param main_directory the main directory from which you want a list of all
#'   the files, in quotes; default assumes the main directory is your current
#'   working directory
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
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Main body of function -------------------------------------------------
   
   # Check for "\" b/c people will probably paste the path from Windows
   main_directory <- gsub("\\\\", "/", main_directory)
   
   RefDirs <- list.dirs(main_directory)
   RefDirs <- RefDirs[!str_detect(RefDirs, "DataRec/Forms")]
   
   Refs <- list.files(path = main_directory, 
                      pattern = "\\.(r|csv|wks|wksx|wksz|xlsx|pdf|png|jpg|txt|docx|doc|xml|pptx|xls)$",
                      recursive = TRUE, 
                      ignore.case = TRUE,
                      full.names = FALSE)
   
   Refs <- Refs[!str_detect(Refs, "^~")]
   
   if(length(Refs) == 0){
      stop("No references were found. Please check your directory.",
           call. = FALSE)
   }
   
   Refs <- data.frame(FullPath = ) %>% 
      mutate(File = basename(FullPath),
             Directory = dirname(FullPath)) %>% 
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


