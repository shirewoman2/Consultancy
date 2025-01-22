#' Make a directory of simulations indicating which simulation file is located
#' in which subfolder
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
#'   working directory is the top level of your project folders.
#' @param sim_data_files which files to include in the directory of simulations.
#'   This only applies when you don't supply anything for the argument
#'   \code{existing_exp_details}. Options are:
#'   \describe{
#'
#'   \item{"recursive" (default)}{all .xlsx, .db, or .wksz files in the current folder and any
#'   subfolders}
#'
#'   \item{NA}{all .xlsx, .db, or .wksz files in the current folder}
#'
#'   \item{a single text string such as "dev"}{include any files in the current
#'   folder or any folders below it that have that specific text}
#'
#'   \item{a character vector of specific file names, e.g., \code{sim_data_files =
#'   c("abc.xlsx", "def.xlsx")}}{only include the files listed. We'll figure
#'   out which folder they're in.}}
#'
#' @param existing_exp_details optionally supply the output from running
#'   \code{\link{extractExpDetails_mult}} to get only the simulation files
#'   included there in your simulation directory. If you supply something here,
#'   whatever you supply for \code{sim_data_files} will be ignored. This will
#'   also be used to figure out which XML files go with which simulations.
#' @param save_table optionally specify an Excel file name for saving your
#'   simulation directory. If you don't include the file extension ".xlsx",
#'   we'll add it.
#' @param overwrite Should we overwrite if your Excel file already exists and
#'   already has a tab named "Simulation directory"? Options are "yes" to always
#'   overwrite, "no" to never overwrite, or "ask" (default), which means that we
#'   will ask you whether to overwrite and give you a chance to supply a
#'   different file name if the one you supplied already exists.
#'
#' @return a data.frame of simulation files and their respective file paths
#' @export
#'
#' @examples
#' # none yet
#' 
make_simulation_directory <- function(project_folder = NA, 
                                      sim_data_files = "recursive", 
                                      existing_exp_details = NA, 
                                      save_table = NA, 
                                      overwrite = "ask"){
   
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
      project_folder <- paste0(getwd(), "/")
   }
   
   # Making sure project folder ends with / so that we can paste the file to
   # that correctly.
   project_folder <- ifelse(str_detect(project_folder, "/$"), 
                            project_folder, 
                            paste0(project_folder, "/"))
   
   # Harmonizing input. This will be of class "NULL" if input for argument was
   # NA.
   existing_exp_details_orig <- existing_exp_details
   existing_exp_details <- harmonize_details(existing_exp_details)
   
   if(all(is.na(existing_exp_details_orig)) == FALSE){
      if("NULL" %in% class(existing_exp_details)){
         warning(wrapn("You supplied something for the argument 'existing_exp_details', but it's not in a format we expected. We'll look at all the files in the project folder and any subfolders and return those instead."), 
                 call. = FALSE)
         
         Directory <- tibble(
            PathFile = paste0(project_folder, 
                              list.files(path = project_folder, 
                                         pattern = "\\.xlsx|\\.wksz|\\.db", 
                                         recursive = TRUE))) 
         
      } else {
         
         NoFileExt <- sub("\\.xlsx$|\\.db$|\\.wksz$", "", 
                          basename(existing_exp_details$MainDetails$File))
         AllPossFiles <- paste0(rep(NoFileExt, each = 3), c(".xlsx", ".db", ".wksz"))
         
         Directory <- tibble(File = AllPossFiles) %>% 
            # adding path 
            left_join(tibble(
               PathFile = paste0(project_folder, 
                                 list.files(path = project_folder, 
                                            recursive = TRUE)), 
               File = basename(PathFile)), 
               by = "File")
      }
   } else if(length(sim_data_files) == 1){
      if(is.na(sim_data_files)){
         Directory <- tibble(
            PathFile = paste0(project_folder, 
                              list.files(path = project_folder, 
                                         pattern = "\\.xlsx|\\.wksz|\\.db", 
                                         recursive = TRUE)))
         
      } else if(tolower(sim_data_files) == "recursive"){
         Directory <- tibble(
            PathFile = paste0(project_folder, 
                              list.files(path = project_folder, 
                                         pattern = "\\.xlsx|\\.wksz|\\.db", 
                                         recursive = TRUE)))
         
      } else {
         # This is when they have supplied a character string to match
         Directory <- tibble(
            PathFile = paste0(project_folder, 
                              list.files(path = project_folder, 
                                         pattern = sim_data_files, 
                                         recursive = recursive))) 
      }
      
   } else {
      # If length(sim_data_files) > 1, then they have supplied a character
      # vector of files. If they didn't include ".xlsx" at the end, add that.
      Directory <- tibble(File = basename(sim_data_files)) %>% 
         mutate(
            File = case_when(str_detect(File, "\\.xlsx|\\.db|\\.wksz") == FALSE ~ 
                                paste0(File, ".xlsx"), 
                             .default = File)) %>% 
         # adding path 
         left_join(tibble(
            PathFile = paste0(project_folder, 
                              list.files(path = project_folder, 
                                         recursive = TRUE), 
                              File = basename(PathFile)), 
            by = "File"))
   } 
   
   if("File" %in% names(Directory) == FALSE){Directory$File <- Directory$PathFile}
   
   # Removing temporary files and making sure that all File values are basename. 
   Directory <- Directory %>% 
      mutate(File = case_when(complete.cases(PathFile) ~ basename(PathFile), 
                              .default = File)) %>% 
      filter(is.na(PathFile) | 
                (complete.cases(PathFile) & !str_detect(PathFile, "^~"))) %>% 
      mutate(Folder = dirname(PathFile), 
             Folder = ifelse(Folder == ".", getwd(), Folder), 
             Folder = if_else(file.exists(PathFile), Folder, "FILE NOT FOUND"),  
             Filename = sub("\\.xlsx|\\.db|\\.wksz", "", File),
             Filetype = str_extract(File, "xlsx$|wksz$|db$"))
   
   # Removing from consideration any files that were not included in
   # existing_exp_details, if that was supplied, if the folder is now "FILE NOT
   # FOUND" and if the Filetype is NOT xlsx. Removing these b/c we're the ones
   # who added them: They are just variations on the simulation file names that
   # we're checking to see if they exist, e.g., workspaces or database files.
   if("logical" %in% class(existing_exp_details) == FALSE){
      Directory <- Directory %>% 
         filter(!(Folder == "FILE NOT FOUND" & Filetype != "xlsx"))
   }
   
   if(length(Directory$Filename[Directory$Folder != "FILE NOT FOUND"]) == 0){
      # This will happen if they have supplied something for
      # existing_exp_details but those files aren't in the project folder.
      stop(wrapn(paste0("We can't find any of the files in 'existing_exp_details'. Are you in the main folder for this project or have you set the argument 'project_folder' to that folder? We need to know the project folder to figure out where your files are. Please set the project folder and try again.")), 
           call. = FALSE)
   }
   
   if(nrow(Directory) == 0){
      stop(wrapn("There are no files that match the pattern of text you supplied for 'sim_data_files', so we cannot make your simulation diratory."), 
           call. = FALSE)
   }
   
   Directory <- Directory %>% 
      group_by(Filename, Folder) %>% 
      summarize(Filetype = str_c(Filetype, collapse = ", ")) %>% 
      ungroup() %>% 
      mutate(`Table/Figure` = "", 
             Comments = "")
   
   # Checking for file name issues
   suppressWarnings(
      Directory <- Directory %>% 
         mutate(FileNameCheck = check_file_name(Filename))
   )
   
   BadFileNames <- Directory %>% 
      filter(!FileNameCheck == "File name meets naming standards.")
   
   if(nrow(BadFileNames)> 0){
      BadFileNames <- BadFileNames %>% 
         mutate(Bad = paste0(Filename, ": ", FileNameCheck)) %>% 
         pull(Bad)
      
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"), "\n"),
              call. = FALSE)
   }
   
   Directory$DuplicateFileCheck <- duplicated(Directory$Filename, fromLast = T) |
      duplicated(Directory$Filename, fromLast = F)
   Directory <- Directory %>% 
      mutate(DuplicateFileCheck = case_when(DuplicateFileCheck == TRUE ~ "simulation file in multiple locations", 
                                            DuplicateFileCheck == FALSE ~ ""))
   
   # Making relative paths
   Directory$Folder <- R.utils::getRelativePath(Directory$Folder, 
                                                relativeTo = project_folder)
   
   # Formatting per FDA/Consultancy Team requirements
   Directory <- Directory %>% 
      mutate(Folder = case_when(Folder == "." ~ "", 
                                .default = Folder)) %>% 
      select(Filename, Filetype, Folder, `Table/Figure`, Comments, FileNameCheck, DuplicateFileCheck) %>% 
      unique()
   
   if("NULL" %in% class(existing_exp_details) == FALSE){
      
      if("ObsOverlayFile" %in% names(existing_exp_details$MainDetails)){
         if(any(complete.cases(existing_exp_details$MainDetails$ObsOverlayFile))){
            Directory <- Directory %>% 
               left_join(existing_exp_details$MainDetails %>% 
                            select(File, ObsOverlayFile) %>% 
                            mutate(Filename = sub("\\..*$", "", basename(File))) %>% 
                            select(-File), by = "Filename") %>% 
               mutate(ObsOverlayFile = R.utils::getRelativePath(ObsOverlayFile, 
                                                                relativeTo = project_folder), 
                      ObsOverlayFile = case_when(
                         str_detect(ObsOverlayFile, "Working Directory/XMLs") ~ 
                            sub(".*/XMLs", "XMLs", ObsOverlayFile),
                         .default = ObsOverlayFile), 
                      XMLFileNameCheck = check_file_name(ObsOverlayFile), 
                      XMLFileNameCheck = case_when(
                         XMLFileNameCheck == "File name meets naming standards." ~ "", 
                         is.na(XMLFileNameCheck) ~ "", 
                         .default = sub("File name", "XML file name", XMLFileNameCheck))) %>% 
               rename("XML file used" = ObsOverlayFile)
            
            ObsOverlayKnown <- TRUE
         }
      } else {
         # This is when they have existing_exp_details but they must not have
         # had the workspaces available when they ran extractExpDetails_mult
         # b/c there is no info on any observed overlay files. Warning in
         # that case.
         warning(wrapn("You have supplied something for the argument 'existing_exp_details', but it does not include any information about the observed overlay files that were used with these simulations. To get that information, when you run 'extractExpDetails_mult', your Excel simulation results files must be in the same folder as the workspaces because we need both to figure out which XML file went with which simulation."), 
                 call. = FALSE)
         
         ObsOverlayKnown <- FALSE
         Directory$XMLFileNameCheck <- ""
      }
   } else {
      ObsOverlayKnown <- FALSE
      Directory$XMLFileNameCheck <- ""
   }
   
   if(ObsOverlayKnown == FALSE){
      # Checking for XML files in both the project directory and in any folder
      # that includes "XML" below the folder ending in "Working Directory", e.g., 
      # for the project folder whose full path would be 
      
      # "C:/Users/myname/Certara/Simcyp PBPKConsult ABC1A - Modelling Working Directory/Modelling"
      
      # look in the folder here: 
      # "C:/Users/myname/Certara/Simcyp PBPKConsult ABC1A - Modelling Working Directory/XMLs"
      
      ProjectFullPath <- R.utils::getAbsolutePath(pathname = project_folder)
      PossibleXMLPath <- str_extract(ProjectFullPath, "^.*Working Directory/Model(l)?ing/")
      PossibleXMLPath <- sub("/Modelling|/Modeling", "", PossibleXMLPath)
      PossibleXMLPath <- list.dirs(path = PossibleXMLPath, recursive = FALSE)
      PossibleXMLPath <- PossibleXMLPath[str_detect(tolower(PossibleXMLPath), "xml")]
      
      XMLs <- list()
      for(path in c(project_folder, PossibleXMLPath)){
         XMLs[[path]] <- tibble(PathFile = list.files(path = path, 
                                                      pattern = "\\.xml$",
                                                      full.names = TRUE, 
                                                      recursive = TRUE))
      }
      
      XMLs <- bind_rows(XMLs) 
      
      if(nrow(XMLs) > 0){
         
         XMLs <- XMLs %>% 
            mutate(Filetype = "xml", 
                   PathFile = R.utils::getRelativePath(PathFile, relativeTo = project_folder), 
                   Filename = basename(PathFile), 
                   Filename = sub("\\.xml$", "", Filename), 
                   Folder = dirname(PathFile), 
                   Folder = ifelse(Folder == ".", "", Folder), 
                   # hacking this by adding the longest of the possible extensions
                   # b/c regulators need the character length to include the
                   # extension.
                   FileNameCheck = check_file_name(paste0(Filename, ".wksz"))) %>% 
            select(Filename, Folder, Filetype)
         
         Directory <- bind_rows(Directory, 
                                tibble(Filename = c(rep(NA, 5), 
                                                    "Possible XML files for this project that we found either in the the project folder or in the 'XMLs' folder associated with this project:")), 
                                XMLs)
         
      }
   }
   
   # Simplifying FileNameCheck column 
   Directory <- Directory %>% 
      mutate(FileNameCheck = case_when(
         FileNameCheck == "File name meets naming standards." ~ "", 
         .default = FileNameCheck))
   
   # Combining main and XML file name checks into 1 column
   Directory <- Directory %>% 
      mutate(FileNameCheck = case_when(
         FileNameCheck == "" & XMLFileNameCheck == "" ~ "", 
         FileNameCheck != "" & XMLFileNameCheck == "" ~ FileNameCheck, 
         FileNameCheck == "" & XMLFileNameCheck != "" ~ XMLFileNameCheck, 
         FileNameCheck != "" & XMLFileNameCheck != "" ~ paste(FileNameCheck, XMLFileNameCheck))) 
   
   if(all(Directory$FileNameCheck == "", na.rm = T)){
      Directory <- Directory %>% select(-FileNameCheck)
   }
   
   if(any(Directory$DuplicateFileCheck == 
          "simulation file in multiple locations", na.rm = T) == FALSE){
      Directory <- Directory %>% select(-DuplicateFileCheck)
   }
   
   # Setting column order 
   Directory <- Directory %>% 
      select(any_of(c("Filename", "Filetype", "Folder", 
                      "XML file used", "Table/Figure",
                      "Comments", "FileNameCheck", "DuplicateFileCheck"))) %>% 
      rename("File type" = Filetype, 
             "File name" = Filename)
   
   
   # Saving -----------------------------------------------------------------
   
   if(complete.cases(save_table)){
      
      Highlighting <- list()
      
      if("FileNameCheck" %in% names(Directory)){
         Highlighting[["FileNameCheck"]] <- 
            list("rows" = which(Directory$FileNameCheck != ""), 
                 "columns" = which(names(Directory) %in% c("File name", "FileNameCheck")))
      } 
      
      if(any(Directory$Folder == "FILE NOT FOUND", na.rm = T)){
         Highlighting[["Folder"]] <- 
            list("rows" = which(Directory$Folder == "FILE NOT FOUND"), 
                 "columns" = which(names(Directory) %in% c("File name", "Folder")))
      } 
      
      if("DuplicateFileCheck" %in% names(Directory)){
         Highlighting[["DuplicateFileCheck"]] <- 
            list("rows" = which(Directory$DuplicateFileCheck != ""), 
                 "columns" = which(names(Directory) %in% c("File name", "DuplicateFileCheck")))
         
         warning(paste0(wrapn("The following simulation files were found in multiple locations: "), 
                        str_c(Directory$`File name`[which(Directory$DuplicateFileCheck != "")], 
                              collapse = "\n")), 
                 call. = FALSE)
      } 
      
      Highlighting <- Highlighting[which(lapply(Highlighting, length) > 0)]
      
      BoldRow <- which(Directory$`File name` == "Possible XML files for this project that we found either in the the project folder or in the 'XMLs' folder associated with this project:")[1] # there shall be only one. 
      if(any(is.na((BoldRow)))){
         Bold <- NA
      } else {
         Bold <- list(list("rows" = BoldRow, 
                           "columns" = 1))
      }
      
      BoldRow <- ifelse(length(BoldRow) == 0, NA, BoldRow)
      
      ColWidths <- guess_col_widths(DF = Directory, wrap = FALSE)
      ColWidths[ColWidths > 85] <- 85
      
      save_table_to_Excel(table = Directory, 
                          save_table = save_table, 
                          overwrite = overwrite, 
                          output_tab_name = "Simulation directory", 
                          center_top_row = FALSE, 
                          highlight_cells = list("yellow" = Highlighting), 
                          bold_cells = Bold, 
                          column_widths = ColWidths, 
                          wrap_text = FALSE)
   }
   
   return(Directory)
   
}



