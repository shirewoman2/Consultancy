#' Figure out what date/time stamps on Excel results files would be removed by
#' the function \code{\link{remove_file_timestamp}}
#'
#' \code{detect_file_timestamp} will find any date/time stamps that have EXACTLY
#' the same format as the Simcyp Autorunner's output. For example: "myfile -
#' 2023-10-31 13-29-15.xlsx" will be noted and, if you run
#' \code{\link{remove_file_timestamp}}, would become "myfile.xlsx". BE CAREFUL
#' because there's no undo button; your file names will be permanently changed.
#'
#' @param sim_data_files the Simcyp Simulator results Excel files to clean up.
#'   There are three options: \enumerate{\item{Leave this as NA (default)
#'   to remove the date/time stamp from all the Excel files in the current
#'   folder.}
#'
#'   \item{Set to "recursive" to remove the date/time stamp from all the Excel
#'   files in this and all subfolders.}
#'
#'   \item{Provide a character vector of the exact files whose date/time stamps
#'   you want to remove.}}
#' @param regex_to_match Optionally provide a text string to use for finding the
#'   correct files from which to remove the date/time stamp. For example,if your
#'   files all start with "abc1a-", list that here, in quotes, and anything that
#'   doesn't contain "abc1a-" will be ignored. Don't include the "xlsx" part of
#'   the file name, though, because we'll add that. Wildcards and other regex
#'   are acceptable.
#'
#' @return a data.frame with the original file names and what the new file names
#'   would be if you run \code{\link{remove_file_timestamp}}
#' @export
#'
#' @examples
#' # None yet
#'
#' 
detect_file_timestamp <- function(sim_data_files = NA,
                                  regex_to_match = NA){
   
   # Error catching ---------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), "\n     library(tidyverse)\n\nand then try again."), call. = FALSE)
   }
   
   # Main body of function ---------------------------------------------------
   
   if(length(sim_data_files) == 1 &&
      (is.na(sim_data_files) | sim_data_files == "recursive")){
      Regex <- ifelse(is.na(regex_to_match), 
                      "\\.xlsx$|\\.db$",
                      paste0(regex_to_match, ".*\\.xlsx$|", regex_to_match, ".*\\.db$"))
      sim_data_files <- list.files(pattern = "\\.xlsx$|\\.db$",
                                   recursive = (complete.cases(sim_data_files) &&
                                                   sim_data_files == "recursive"))
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   } 
   
   Changes <- data.frame(Original = sim_data_files) %>% 
      mutate(Revised = sub(" - [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2}", 
                           "", Original)) %>% 
      filter(str_detect(Original, " - [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2}"))
   
   if(any(duplicated(Changes$Revised))){
      warning("The following files would have the same name if you run remove_file_timestamp:\n", 
              str_c(Changes$Original[Changes$Revised %in% 
                                        Changes$Revised[duplicated(Changes$Revise)]], 
                    collapse = "\n"))
   }
   
   return(Changes)
}

