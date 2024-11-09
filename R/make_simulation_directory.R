#' Make a directory of simulations indicating which simulation file is located
#' in which folder
#'
#' @param project_folder location of the project folder to search. Please note
#'   that R requires forward slashes ("/") in file paths rather than the back
#'   slashes Microsoft uses. If left as NA (default), we'll assume your current
#'   working directory is the top level of your project folders. This function
#'   \emph{only} searches for simulation files in the project folder and any
#'   subfolders.
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
#' @param save_table optionally specify either an Excel or csv file name for
#'   saving your simulation directory
#' @param relative_paths TRUE (default) or FALSE for whether to put relative
#'   (rather than full) paths in the table
#' @param recursive TRUE (default) or FALSE for whether to recursively check for
#'   simulation files in subfolders
#'
#' @return a data.frame of simulation files and their respective file paths
#' @export
#'
#' @examples
#' # none yet
#' 
make_simulation_directory <- function(project_folder = NA, 
                                      recursive = TRUE, 
                                      which_files = NA, 
                                      existing_exp_details = NA, 
                                      relative_paths = TRUE, 
                                      save_table = NA){
   
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
                                   recursive = recursive)
         
      } else if(which_files == "use existing_exp_details"){
         
         which_files <- existing_exp_details$MainDetails %>% 
            pull(File)
         
      } else {
         
         which_files <- list.files(path = project_folder, 
                                   pattern = which_files, 
                                   recursive = recursive)
      }
   } else {
      # If length(which_files) > 1, then they have supplied a character vector
      # of files. If they didn't include ".xlsx" at the end, add that.
      which_files <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$", "", which_files), ".xlsx")
      
   } 
   
   which_files <- unique(which_files[!str_detect(which_files, "^~")])
   
   if(length(which_files) == 0){
      stop(wrapn("There are no files that match what the pattern of text you supplied for 'which_files', so we cannot make your simulation diratory."), 
           call. = FALSE)
   }
   
   Directory <- data.frame(File = which_files, 
                           Folder = as.character(NA)) %>% 
      # Only keeping xlsx, db, and wksz files
      filter(str_detect(File, "xlsx$|db$|wksz$"))
   
   if(nrow(Directory) == 0){
      stop(wrapn("No simulation files were found in the directory provided. We cannot make your simulation directory."), 
           call. = FALSE)
   }
   
   FoundFiles <- tibble(
      FoundFile = list.files(path = project_folder, 
                             pattern = "xlsx$|db$|wksz$", 
                             recursive = recursive, 
                             full.names = TRUE)) %>% 
      mutate(Folder = dirname(FoundFile), 
             Folder = ifelse(Folder == ".", getwd(), Folder), 
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
      
      Directory$Folder[i] <- str_comma(temp$Folder)
      
   }
   
   Directory$Folder[Directory$Folder == ""] <- "FILE NOT FOUND"
   
   if(relative_paths){
      Directory$Folder <- R.utils::getRelativePath(Directory$Folder)
   }
   
   # Formatting per FDA/Consultancy Team requirements
   Directory <- Directory %>% 
      mutate(Filename = sub("\\..*$", "", basename(File)), 
             Filetype = str_extract(File, "xlsx$|db$|wksz$")) %>% 
      select(Filename, Filetype, Folder) %>% 
      unique() %>% 
      group_by(Filename, Folder) %>% 
      summarize(Filetype = str_c(Filetype, collapse = ", ")) %>% 
      ungroup() %>% 
      select(Filename, Filetype, Folder) %>% 
      mutate(`Table/Figure` = "", 
             Comments = "", 
             Folder = case_when(Folder == "." ~ "", 
                                .default = Folder))
   
   
   # Saving -----------------------------------------------------------------
   
   if(complete.cases(save_table)){
      
      if(str_detect(save_table, "csv$")){
         write.csv(Directory, file = save_table, row.names = FALSE)
      } else {
         save_table_to_Excel(table = Directory, 
                             save_table = save_table, 
                             output_tab_name = "Simulation directory", 
                             center_top_row = FALSE)
      }
   }
   
   return(Directory)
   
}



