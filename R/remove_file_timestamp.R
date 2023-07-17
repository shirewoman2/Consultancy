#' Remove date/time stamps from Excel results files created by the Autorunner
#'
#' \code{remove_file_timestamp} will remove date/time stamps that have EXACTLY
#' the same format as the Simcyp Autorunner's output. For example: "myfile -
#' 2023-10-31 13-29-15.xlsx" will become "myfile.xlsx". BE CAREFUL because
#' there's no undo button; your file names will be permanently changed.
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
#' @param return_file_names TRUE or FALSE (default) for whether to return the
#'   original and revised file names. If you want to check what the new file
#'   names would be before running this, try running
#'   \code{\link{detect_file_timestamp}} first. 
#'
#' @return Does not return anything in R; only changes file names
#' @export
#'
#' @examples
#' # None yet
#'
#' 
remove_file_timestamp <- function(sim_data_files = NA,
                                  regex_to_match = NA, 
                                  return_file_names = FALSE){
   
   # Error catching ---------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Main body of function ---------------------------------------------------
   
   suppressWarnings(
      Changes <- detect_file_timestamp(sim_data_files = sim_data_files, 
                                       regex_to_match = regex_to_match))
   
   if(any(duplicated(Changes$Revised))){
      stop("We cannot proceed because the following files would have the same name:
", 
str_c(Changes$Original[Changes$Revised %in% 
                      Changes$Revised[duplicated(Changes$Revise)]], 
      collapse = "\n"))
   }
   
   for(i in 1:nrow(Changes)){
      file.rename(from = Changes$Original[i], 
                  to = Changes$Revised[i])
   }
   
   if(return_file_names)
      return(Changes)
}

