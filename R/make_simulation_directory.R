#' Make a directory of simulations indicating which simulation file is located
#' in which folder
#'
#' @description \code{make_simulation_directory} will create a data.frame of
#'   simulations in a given project folder, the associated XML files when
#'   applicable (you'll have to provide that information with the argument
#'   \code{existing_exp_details}), and, optionally, save that data.frame to an
#'   Excel file. It will also check whether the file names comply with USFDA and
#'   Consultancy Team standards and check whether the same simulation is saved
#'   in more than one place.
#'
#'   This is wicked fast.
#'
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
#'   subfolders OR, if you supply something for \code{existing_exp_details},
#'   the files included in that object}
#'
#'   \item{a single text string such as "abc"}{only include files with that
#'   specific text}
#'
#'   \item{a character vector of specific file names, e.g., \code{which_files =
#'   c("abc.xlsx", "def.xlsx")}}{only include files listed}}
#' @param existing_exp_details the output from running
#'   \code{\link{extractExpDetails_mult}}
#' @param save_table optionally specify either an Excel or csv file name for
#'   saving your simulation directory
#' @param relative_paths TRUE (default) or FALSE for whether to put relative
#'   (rather than full) paths in the table. This means "relative to your current
#'   working directory", so it matters what folder you're in when you call this
#'   function. We recommend setting this to TRUE. 
#' @param recursive TRUE (default) or FALSE for whether to recursively check for
#'   simulation files in subfolders
#' @param wrap_text TRUE or FALSE (default) for whether to wrap text in the
#'   Excel output
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
                                      save_table = NA, 
                                      wrap_text = FALSE){
   
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
   
   # Getting all file names
   AllFiles <- tibble(PathFile = list.files(path = project_folder, 
                                            recursive = recursive)) %>% 
      mutate(File = basename(PathFile))
   
   # Harmonizing input. This will be of class "NULL" if input for argument was
   # NA.
   existing_exp_details <- harmonize_details(existing_exp_details)
   
   if(length(which_files) == 1){
      if(is.na(which_files)){
         if("NULL" %in% class(existing_exp_details)){
            
            Directory <- tibble(PathFile = list.files(path = project_folder, 
                                                      recursive = recursive)) %>% 
               mutate(File = basename(PathFile)) %>% 
               filter(!str_detect(PathFile, "^~"))
            
         } else {
            Directory <- existing_exp_details$MainDetails %>% 
               mutate(File_orig = File, 
                      File = basename(File)) %>% 
               select(File, File_orig) %>% 
               # adding path 
               left_join(AllFiles, by = "File")
         }
         
      } else {
         Directory <- tibble(PathFile = list.files(path = project_folder, 
                                                   pattern = which_files, 
                                                   recursive = recursive)) %>% 
            mutate(File = basename(PathFile)) %>% 
            filter(!str_detect(PathFile, "^~"))
      }
      
   } else {
      # If length(which_files) > 1, then they have supplied a character vector
      # of files. If they didn't include ".xlsx" at the end, add that.
      Directory <-  tibble(PathFile = which_files) %>% 
         mutate(PathFile = case_when(str_detect(PathFile, "\\.xlsx|\\.db|\\.wksz") == 
                                        FALSE ~ paste0(PathFile, ".xlsx")), 
                File = basename(PathFile)) %>% 
         filter(!str_detect(PathFile, "^~"))
      
   } 
   
   # Checking for other file extensions and checking that file exists. 
   Directory <- Directory %>% 
      mutate(Folder = dirname(PathFile), 
             Folder = ifelse(Folder == ".", getwd(), Folder), 
             Filename = sub("\\.xlsx|\\.db|\\.wksz", "", File), 
             
             File.wksz = sub("\\.xlsx|\\.db|\\.wksz", ".wksz", PathFile), 
             File.wksz = case_when(file.exists(File.wksz) ~ File.wksz, 
                                   .default = NA), 
             
             File.xlsx = sub("\\.xlsx|\\.db|\\.wksz", ".xlsx", PathFile), 
             File.xlsx = case_when(file.exists(File.xlsx) ~ File.xlsx, 
                                   .default = NA), 
             
             File.db = sub("\\.xlsx|\\.db|\\.wksz", ".db", PathFile), 
             File.db = case_when(file.exists(File.db) ~ File.db, 
                                 .default = NA))
   
   if(length(sort(unique(c(Directory$File.wksz, 
                           Directory$File.xlsx, 
                           Directory$File.db)))) == 0){
      if("NULL" %in% class(existing_exp_details) == FALSE){
         stop(wrapn(paste0("We can't find any of the files in 'existing_exp_details'. Are you in the main folder for this project or have you set the argument 'project_folder' to that folder? We need to know the project folder to figure out where your files are. Please set the project folder and try again.")), 
              call. = FALSE)
      } else {
         stop(wrapn("There are no files that match what the pattern of text you supplied for 'which_files', so we cannot make your simulation diratory."), 
              call. = FALSE)
      }
   }
   
   # Checking for file name issues
   Directory <- Directory %>% 
      mutate(FileNameCheck = check_file_name(Filename))
   
   BadFileNames <- Directory %>% 
      filter(!FileNameCheck == "File name meets naming standards.")
   
   if(nrow(BadFileNames)> 0){
      BadFileNames <- BadFileNames %>% 
         mutate(Bad = paste0(File, ": ", FileNameCheck)) %>% 
         pull(Bad)
      
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"), "\n"),
              call. = FALSE)
   }
   
   Directory$DuplicateFileCheck <- duplicated(Directory$Filename, fromLast = T) |
      duplicated(Directory$Filename, fromLast = F)
   Directory <- Directory %>% 
      mutate(DuplicateFileCheck = case_when(DuplicateFileCheck == TRUE ~ "simulation file in multiple locations", 
                                            DuplicateFileCheck == FALSE ~ ""), 
             Folder = case_when(PathFile %in% AllFiles$PathFile == FALSE ~ "FILE NOT FOUND", 
                                .default = Folder))
   
   if(relative_paths){
      Directory$Folder <- R.utils::getRelativePath(Directory$Folder)
   }
   
   # Formatting per FDA/Consultancy Team requirements
   Directory <- Directory %>% 
      mutate(Filetype = paste0(if_else(complete.cases(File.wksz), "wksz, ", ""), 
                               if_else(complete.cases(File.xlsx), "xlsx, ", ""), 
                               if_else(complete.cases(File.db), "db", "")), 
             Filetype = sub(", $", "", Filetype)) %>% 
      select(Filename, Filetype, Folder, FileNameCheck, DuplicateFileCheck) %>% 
      unique() %>% 
      mutate(`Table/Figure` = "", 
             Comments = "", 
             Folder = case_when(Folder == "." ~ "", 
                                .default = Folder))
   
   if("NULL" %in% class(existing_exp_details) == FALSE){
      
      if("ObsOverlayFile" %in% names(existing_exp_details$MainDetails) &&
         any(complete.cases(existing_exp_details$MainDetails$ObsOverlayFile))){
         Directory <- Directory %>% 
            left_join(existing_exp_details$MainDetails %>% 
                         select(File, ObsOverlayFile) %>% 
                         mutate(Filename = sub("\\..*$", "", basename(File))) %>% 
                         rename("XML file used" = ObsOverlayFile) %>% 
                         select(-File), by = "Filename")
         
         if(relative_paths){
            
            Directory$`XML file used` <- gsub("\\\\", "/", Directory$`XML file used`)
            Directory$`XML file used` <- sub("C:/Users/.*/Certara.*Directory/", 
                                             "", Directory$`XML file used`)
            
            # # Dealing with possibly different user names b/c this mucks up
            # # getting the relative path
            # 
            # X <- gsub("\\\\", "/", Directory$`XML file used`)
            # Pattern <- gsub("Users/|/Certara", "",
            #                 str_extract(X, pattern = "Users.*Certara"))
            # Replacement <- rep(as.character(Sys.info()["user"]), length(Pattern))
            # # Directory$`XML file used` <- 
            #    
            #    str_replace(Pattern, Replacement, X)
            # 
            #    map()
            #    
            #    THIS IS COMPLETELY NOT WORKING. 
            #    
            # 
            # Directory$`XML file used` <- 
            #    R.utils::getRelativePath(Directory$`XML file used`)
            # 
            # Directory$`XML file used` <- gsub("../.*Working Directory/", "", Directory$`XML file used`)
         }
      }
   }
   
   if(any(Directory$FileNameCheck != "File name meets naming standards.")){
      Directory <- Directory %>% 
         mutate(FileNameCheck = case_when(
            FileNameCheck == "File name meets naming standards." ~ "", 
            .default = FileNameCheck))
   } else {
      Directory <- Directory %>% select(-FileNameCheck)
   }
   
   if(any(Directory$DuplicateFileCheck == "simulation file in multiple locations") == FALSE){
      Directory <- Directory %>% select(-DuplicateFileCheck)
   }
   
   # Setting column order 
   Directory <- Directory %>% 
      select(any_of(c("Filename", "Filetype", "Folder", 
                      "XML file used", "Table/Figure",
                      "Comments", "FileNameCheck", "DuplicateFileCheck")))
   
   
   # Saving -----------------------------------------------------------------
   
   if(complete.cases(save_table)){
      
      if(str_detect(save_table, "csv$")){
         write.csv(Directory, file = save_table, row.names = FALSE)
      } else {
         
         Highlighting <- list()
         
         if("FileNameCheck" %in% names(Directory)){
            Highlighting[["FileNameCheck"]] <- 
               list("rows" = which(Directory$FileNameCheck != ""), 
                    "columns" = which(names(Directory) == "FileNameCheck"))
         } 
         
         if(any(Directory$Folder == "FILE NOT FOUND")){
            Highlighting[["Folder"]] <- 
               list("rows" = which(Directory$Folder == "FILE NOT FOUND"), 
                    "columns" = which(names(Directory) == "Folder"))
         } 
         
         if("DuplicateFileCheck" %in% names(Directory)){
            Highlighting[["DuplicateFileCheck"]] <- 
               list("rows" = which(Directory$DuplicateFileCheck != ""), 
                    "columns" = which(names(Directory) == "DuplicateFileCheck"))
            
            warning(paste0(wrapn("The following simulation files were found in multiple locations: "), 
                           str_c(Directory$File[which(Directory$DuplicateFileCheck != "")], 
                                 collapse = "\n")), 
                    call. = FALSE)
         } 
         
         Highlighting <- Highlighting[which(lapply(Highlighting, length) > 0)]
         
         save_table_to_Excel(table = Directory, 
                             save_table = save_table, 
                             output_tab_name = "Simulation directory", 
                             center_top_row = FALSE, 
                             highlight_cells = list("yellow" = Highlighting), 
                             wrap_text = wrap_text)
      }
   }
   
   return(Directory)
   
}



