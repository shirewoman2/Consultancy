#' Checks a file name for naming standards using the criteria laid out by the
#' Consultancy Support Team
#'
#' \code{check_file_name} checks whether a file name meets standard file-naming
#' conventions acceptable by various drug regulatory agencies. A warning message
#' will be given for any unacceptable formats.
#'
#' @param file_name the name(s) of the file(s) to check. This can be a single
#'   string, e.g., \code{file_name = "xlsx"} to match all files in the current
#'   working directory with that string of text in their names or it can be a
#'   character vector of one or more files that you want to check.
#'
#' @return a character vector 
#' @export
#'
#' @examples
#' # Good file name will give a message that things were ok.
#' check_file_name(file_name = "abc-5mg-qd.xlsx")
#'
#' # Bad file names will tell you what went wrong.
#' check_file_name(file_name = "Example simulator output MD.xlsx")
#' check_file_name(file_name = "abc_123_5mg.wksz")
#' 

check_file_name <- function(file_name){
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
      
   if(length(file_name) == 1){
      # If they supplied a single string, they probably want to check files that
      # match that.
      if(any(tolower(file_name) == "all")){
         file_name_touse <- list.files()
      } else {
         file_name_touse <- list.files(pattern = file_name)
         # removing hidden files
         file_name_touse <- file_name_touse[!str_detect(file_name_touse, "~")]
      }
   } else {
      file_name_touse <- c()
   }
   
   if(length(file_name) > 1 | length(file_name_touse) == 0){
      # If they supplied a character vector, then they probably want to check
      # that specific set of file names. This also will apply if they supplied
      # only a single file name that might not be present in the working
      # directory.
      file_name_touse <- file_name
   }
   
   file_name_touse <- basename(file_name_touse)
   
   subfun <- Vectorize(function(file_name_subfun){
      
      if(is.na(file_name_subfun)){return(NA)}
      
      results <- c("File name cannot")
      if(str_length(file_name_subfun) > 64) {
         results <- paste(results, "be more than 64 characters including file extension")
      }
      
      if(str_detect(tools::file_path_sans_ext(file_name_subfun), "\\.") == TRUE){
         results <- ifelse(nchar(results) == 16, 
                           paste(results,"contain periods"),
                           paste(results, "or", "contain periods"))
      }
      
      if(str_detect(file_name_subfun, "_") == TRUE){
         results <- ifelse(nchar(results) == 16, 
                           paste(results,"contain underscores"),
                           ifelse(nchar(results) == 68, 
                                  paste(results, "or contain underscores"),
                                  paste(results, "or underscores")))
      }
      
      if(str_count(file_name_subfun, "[A-Z]") > 0){
         results <- ifelse(nchar(results) == 16, 
                           paste(results,"contain capital letters"),
                           ifelse(nchar(results) == 68, 
                                  paste(results, "or contain capital letters"),
                                  paste(results, "or capital letters")))
      }
      
      if(str_count(file_name_subfun, " ") > 0) {
         results <- ifelse(nchar(results) == 16, 
                           paste(results,"contain spaces"),
                           ifelse(nchar(results) == 68, 
                                  paste(results, "or contain spaces"),
                                  paste(results, "or spaces")))
      }
      
      if(str_detect(file_name_subfun, "[!@#%^&*()+={}|;':,/<>?~`]")){
         results <- ifelse(nchar(results) == 16, 
                           paste(results,"contain symbols"),
                           ifelse(nchar(results) == 68, 
                                  paste(results, "or contain symbols"),
                                  paste(results, "or symbols")))
      }
      
      if(nchar(results) == 16){
         results <- "File name meets naming standards"
      }
      
      return(paste0(results, "."))
   })
   
   subfun(file_name_touse)
   
}








