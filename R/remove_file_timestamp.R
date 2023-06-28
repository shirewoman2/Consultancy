#' Remove date/time stamps from Excel results files created by the Autorunner
#'
#' \code{remove_file_timestamp} will remove date/time stamps that have EXACTLY
#' the same format as the Simcyp Autorunner's. For example: "myfile - 2023-10-31
#' 13-29-15.xlsx" will become "myfile.xlsx". UNDER CONSTRUCTION.
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
#' @return Does not return anything in R; only changes file names
#' @export
#'
#' @examples
#' # None yet
#'
#' 
remove_file_timestamp <- function(sim_data_files = NA,
                                  regex_to_match = NA){
   
   # Error catching ---------------------------------------------------
   
   # FIXME
   
   # Main body of function ---------------------------------------------------
   
   if(length(sim_data_files) == 1 &&
      (is.na(sim_data_files) | sim_data_files == "recursive")){
      Regex <- ifelse(is.na(regex_to_match), 
                      "xlsx$",
                      paste0(regex_to_match, ".*\\.xlsx$"))
      sim_data_files <- list.files(pattern = "xlsx$",
                                   recursive = (complete.cases(sim_data_files) &&
                                                   sim_data_files == "recursive"))
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   } 
   
   Changes <- data.frame(Orig = sim_data_files) %>% 
      mutate(Revised = sub(" - [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2}", 
                           "", Orig)) %>% 
      filter(str_detect(Orig, " - [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2}"))
   
   for(i in 1:nrow(Changes)){
      file.rename(from = Changes$Orig[i], 
                  to = Changes$Revised[i])
   }
}

