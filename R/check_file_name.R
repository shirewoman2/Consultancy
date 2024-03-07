#' Check file names for naming standards
#'
#' \code{check_file_name} checks that files are named according to USFDA
#' file-naming protocols using Cade's checklist.
#'
#' @param file_name name of data file
#'
#' @return Only prints messages
#' @export
#'
#' @examples 
#'  
#' check_file_name(file_name = "Example simulator output MD.xlsx")
#' check_file_name(file_name = "abc1b-5mg-sd.xlsx")
#' check_file_name(file_name = "abc1b-5mg-sd-sdy101a-cohortb-increasedki-10x-decreased-indmax-2x.xlsx")
#'
#'
#' 
check_file_name <- function(file_name){
   
   Checks <- c("length" = str_length(file_name) > 64, 
               "period" = str_detect(tools::file_path_sans_ext(file_name), "\\."), 
               "underscore" = str_detect(file_name, "_"), 
               "case" = str_count(file_name, "[A-Z]") > 0, 
               "spaces" = str_count(file_name, " ") > 0, 
               # not sure how to check for [ ] in file_name.
               "symbols" = str_detect(tools::file_path_sans_ext(file_name),
                                          "[!@#%^&*()_+={}|;':,./<>?~`]"))
   
   if(Checks["length"]){
      print("File name must be less than 64 characters including file extension.")
   }
   if(Checks["period"]){
      print("File name cannot contain a period.")
   }
   if(Checks["underscore"]){
      print("File name cannot contain an underscore.")
   }
   if(Checks["case"]){
      print("File name cannot contain capital letters.")
   }
   if(Checks["spaces"]){
      print("File name cannot contain spaces.")
   } 
   if(Checks["symbols"]){
      print("File name cannot contain symbols.")
   } 
   
   if(all(Checks == FALSE)){
      print("Your file meets the naming standards")
   }  
   
   # We could optionally return Checks here to pass to a parent function s/a
   # check_file_names.
   
}

