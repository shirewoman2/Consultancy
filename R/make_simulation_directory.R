#' Make a directory of simulations indicating which simulation file is located
#' in which folder
#'
#' @param project_folder location of the project folder to search. Please note
#'   that R requires forward slashes ("/") in file paths rather than the back
#'   slashes Microsoft uses. If left as NA (default), we'll assume your current
#'   working directory is the top level of your project folders. This function
#'   \emph{only} searches for simulation files in the project folder and
#'   any subfolders. 
#' @param which_files which files to include in the directory of simulations.
#'   Options are:
#'   \describe{\item{NA}{all xlsx or wksz files in the current folder and any
#'   subfolders}
#'
#'   \item{"use existing_exp_details"}{only include simulations in the object
#'   supplied for \code{existing_exp_details}, which you can generate by running
#'   \code{\link{extractExpDetails_mult}}.}
#'
#'   \item{a single text string such as "abc"}{only include files with that
#'   specific text}
#'
#'   \item{a character vector of specific file names, e.g., \code{which_files =
#'   c("abc.xlsx", "def.xlsx")}}{only include files listed}}
#' @param existing_exp_details the output from running
#'   \code{\link{extractExpDetails_mult}}; only applicable when
#'   \code{which_files = "use existing_exp_details"}
#' @param save_output optionally specify either an Excel or csv file name for
#'   saving your simulation directory
#'
#' @return a data.frame of simulation files and their respective file paths
#' @export
#'
#' @examples
#' # none yet
#' 
make_simulation_directory <- function(project_folder = NA, 
                                      which_files = NA, 
                                      existing_exp_details = NA, 
                                      save_output = NA){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   if(length(project_folder) > 1){
      stop("You can only search one project folder at a time. Please check your input for `project_folder`.", 
           call. = FALSE)
   }
   
   # Main body of function ---------------------------------------------------
   
   if(is.na(project_folder)){
      project_folder <- getwd()
   }
   
   if(length(which_files) == 1){
      if(is.na(which_files)){
         
         which_files <- list.files(path = project_folder, 
                                   recursive = TRUE)
         
      } else if(which_files == "use existing_exp_details"){
         
         which_files <- existing_exp_details$MainDetails %>% 
            pull(File)
         
      } else {
         
         which_files <- list.files(path = project_folder, 
                                   pattern = which_files, 
                                   recursive = TRUE)
      }
   } else {
      # If length(which_files) > 1, then they have supplied a character vector
      # of files. If they didn't include ".xlsx" at the end, add that.
      which_files <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$", "", which_files), ".xlsx")
      
   } 
   
   which_files <- unique(which_files[!str_detect(which_files, "^~")])
   
   Directory <- data.frame(File = which_files, 
                           Path = as.character(NA))
   
   FoundFiles <- tibble(
      FoundFile = list.files(path = project_folder, 
                             recursive = TRUE, 
                             full.names = TRUE)) %>% 
      mutate(Path = dirname(FoundFile), 
             Path = ifelse(Path == ".", getwd(), Path), 
             File = basename(FoundFile))
   
   for(i in 1:nrow(Directory)){
      
      temp <- FoundFiles %>% 
         mutate(CheckName = str_detect(paste0("^", Directory$File[i], "$"), File)) %>% 
         filter(CheckName == TRUE) %>% select(-CheckName) %>% unique()
      
      if(nrow(temp) > 1){
         warning(paste0("The simulation file `", 
                        Directory$File[i], 
                        "` was found in more than one location."), 
                 call. = FALSE)
      }
      
      Directory$Path[i] <- str_comma(temp$Path)
      
   }
   
   Directory$Path[Directory$Path == ""] <- "FILE NOT FOUND"
   
   # Saving -----------------------------------------------------------------
   
   if(complete.cases(save_output)){
      
      # Checking whether they have specified just "xlsx" or just "csv" for
      # output b/c then, we'll use "Simulation directory" as file name.
      if(str_detect(sub("\\.", "", save_output), "^xlsx$|^csv$")){
         OutPath <- "."
         save_output <- paste0("Simulation directory.", sub("\\.", "", save_output))
      } else {
         # If they supplied something other than just "xlsx" or just "csv",
         # then check whether that file name is formatted appropriately.
         if(str_detect(basename(save_output), "\\..*")){
            if(str_detect(basename(save_output), "\\.xlsx") == FALSE){
               # If they specified a file extension that wasn't xlsx, make that
               # file extension be .csv
               save_output <- sub("\\..*", ".csv", save_output)
            }
         } else {
            # If they didn't specify a file extension at all, make it .csv. 
            save_output <- paste0(save_output, ".csv")
         }
         
         # Now that the file should have an appropriate extension, check what
         # the path and basename should be.
         OutPath <- dirname(save_output)
         save_output <- basename(save_output)
      }
      
      if(str_detect(save_output, "\\.csv")){
         write.csv(Directory, save_output, row.names = FALSE)
      } else {
         formatXL_head(Directory, save_output, sheet = "simulation directory")
      }
   }
   
   
   return(Directory)
   
}





