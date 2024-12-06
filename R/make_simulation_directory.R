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
#' @param sim_data_files which files to include in the directory of simulations.
#'   Options are:
#'   \describe{\item{NA}{all .xlsx, .db, or .wksz files in the current folder}
#'
#'   \item{"recursive"}{all .xlsx, .db, or .wksz files in the current folder and any
#'   subfolders}
#'
#'   \item{"use existing_exp_details"}{use the files included in whatever you
#'   supplied for \code{existing_exp_details}. This will fine those files, even
#'   if they're in subfolders, as long as they're somewhere in the project
#'   folder}
#'
#'   \item{a single text string such as "abc"}{include any files in the current
#'   folder or any folders below it that have that specific text}
#'
#'   \item{a character vector of specific file names, e.g., \code{sim_data_files =
#'   c("abc.xlsx", "def.xlsx")}}{only include the files listed. We'll figure
#'   out which subfolder they're in.}}
#'
#' @param existing_exp_details optionally supply the output from running
#'   \code{\link{extractExpDetails_mult}} to get only the simulation files
#'   included there in your simulation directory
#' @param save_table optionally specify either an Excel or csv file name for
#'   saving your simulation directory
#'
#' @return a data.frame of simulation files and their respective file paths
#' @export
#'
#' @examples
#' # none yet
#' 
make_simulation_directory <- function(project_folder = NA, 
                                      sim_data_files = NA, 
                                      existing_exp_details = NA, 
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
   
   # Getting all file names
   AllFiles <- tibble(PathFile = list.files(path = project_folder, 
                                            pattern = "\\.xlsx|\\.wksz|\\.db", 
                                            recursive = TRUE)) %>% 
      mutate(File = basename(PathFile))
   
   # Harmonizing input. This will be of class "NULL" if input for argument was
   # NA.
   existing_exp_details <- harmonize_details(existing_exp_details)
   
   if(length(sim_data_files) == 1){
      if(is.na(sim_data_files)){
         Directory <- tibble(PathFile = list.files(path = project_folder, 
                                                   pattern = "\\.xlsx|\\.wksz|\\.db", 
                                                   recursive = FALSE))
         
      } else if(tolower(sim_data_files) == "recursive"){
         Directory <- tibble(PathFile = list.files(path = project_folder, 
                                                   pattern = "\\.xlsx|\\.wksz|\\.db", 
                                                   recursive = TRUE))
         
      } else if(str_detect(tolower(sim_data_files), "existing|details")){
         if("NULL" %in% class(existing_exp_details)){
            warning(wrapn("You said to use what you supplied for the argument 'existing_exp_details' for the files in your simulation directory, but you did not actually supply anything for 'existing_exp_details'. We'll look at all the files in the project folder and any subfolders and return those instead."), 
                    call. = FALSE)
            
            Directory <- tibble(PathFile = list.files(path = project_folder, 
                                                      pattern = "\\.xlsx|\\.wksz|\\.db", 
                                                      recursive = TRUE)) 
            
         } else {
            
            Directory <- existing_exp_details$MainDetails %>% 
               mutate(File = basename(File)) %>% 
               select(File) %>% 
               # adding path 
               left_join(AllFiles, by = "File")
         }
      } else {
         # This is when they have supplied a character string to match
         Directory <- tibble(PathFile = list.files(path = project_folder, 
                                                   pattern = sim_data_files, 
                                                   recursive = recursive)) 
      }
      
   } else {
      # If length(sim_data_files) > 1, then they have supplied a character
      # vector of files. If they didn't include ".xlsx" at the end, add that.
      Directory <- tibble(File = sim_data_files) %>% 
         mutate(
            File = basename(PathFile), 
            File = case_when(str_detect(File, "\\.xlsx|\\.db|\\.wksz") == FALSE ~ 
                                paste0(File, ".xlsx"), 
                             .default = File)) %>% 
         # adding path 
         left_join(AllFiles, by = "File")
   } 
   
   # Removing temporary files and making sure that all File values are basename. 
   Directory <- Directory %>% 
      mutate(File = basename(PathFile)) %>% 
      filter(!str_detect(PathFile, "^~"))
   
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
         stop(wrapn("There are no files that match the pattern of text you supplied for 'sim_data_files', so we cannot make your simulation diratory."), 
              call. = FALSE)
      }
   }
   
   # Checking for file name issues
   suppressWarnings(
      Directory <- Directory %>% 
         mutate(FileNameCheck = check_file_name(Filename))
   )
   
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
   
   # Making relative paths
   Directory$Folder <- R.utils::getRelativePath(Directory$Folder, 
                                                relativeTo = project_folder)
   
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
                         select(-File), by = "Filename") %>% 
            mutate(ObsOverlayFile = R.utils::getRelativePath(ObsOverlayFile, 
                                                             relativeTo = project_folder), 
                   ObsOverlayFile = case_when(
                      str_detect(ObsOverlayFile, "Working Directory/XMLs") ~ 
                         sub(".*/XMLs", "XMLs", ObsOverlayFile))) %>% 
            rename("XML file used" = ObsOverlayFile)
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
                    "columns" = which(names(Directory) %in% c("Filename", "FileNameCheck")))
         } 
         
         if(any(Directory$Folder == "FILE NOT FOUND", na.rm = T)){
            Highlighting[["Folder"]] <- 
               list("rows" = which(Directory$Folder == "FILE NOT FOUND"), 
                    "columns" = which(names(Directory) %in% c("Folder", "Filename")))
         } 
         
         if("DuplicateFileCheck" %in% names(Directory)){
            Highlighting[["DuplicateFileCheck"]] <- 
               list("rows" = which(Directory$DuplicateFileCheck != ""), 
                    "columns" = which(names(Directory) %in% c("Filename", "DuplicateFileCheck")))
            
            warning(paste0(wrapn("The following simulation files were found in multiple locations: "), 
                           str_c(Directory$Filename[which(Directory$DuplicateFileCheck != "")], 
                                 collapse = "\n")), 
                    call. = FALSE)
         } 
         
         Highlighting <- Highlighting[which(lapply(Highlighting, length) > 0)]
         
         ColWidths <- guess_col_widths(DF = Directory, wrap = FALSE)
         ColWidths[ColWidths > 85] <- 85
         
         save_table_to_Excel(table = Directory, 
                             save_table = save_table, 
                             output_tab_name = "Simulation directory", 
                             center_top_row = FALSE, 
                             highlight_cells = list("yellow" = Highlighting), 
                             column_widths = ColWidths, 
                             wrap_text = FALSE)
      }
   }
   
   return(Directory)
   
}



