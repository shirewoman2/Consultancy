#' Make a directory of simulations indicating which simulation files are present
#' in a given folder and which XML observed overlay files were associated with
#' those simulation files
#'
#' @description The function \code{make_simulation_directory} will create a
#'   data.frame of simulations in a given project folder, the associated XML
#'   files when applicable (you'll have to provide that information with the
#'   argument \code{existing_exp_details} or else ask this function to read all
#'   the workspace files to find them), and, optionally, save that data.frame to
#'   an Excel file. It will also check whether the file names comply with USFDA
#'   and Consultancy Team standards and check whether the same simulation is
#'   saved in more than one place.
#'
#'   This is wicked fast.*
#'
#'   *Caveat: Unless you search all the workspaces for XML observed overlay
#'   data files. Then it takes about 2 seconds per simulation.
#'
#' @param project_folder location of the project folder to search. Please note
#'   that R requires forward slashes ("/") in file paths rather than the back
#'   slashes Microsoft uses. If left as NA (default), we'll assume your current
#'   working directory is the top level of your project folder.
#' @param sim_data_files which files to include in the directory of simulations.
#'   This only applies when you don't supply anything for the argument
#'   \code{existing_exp_details}. Options are:
#'   \describe{
#'
#'   \item{"recursive" (default)}{all .xlsx, .db, .wksz and, if requested, all
#'   .sft, .siva, .cvbe, or .phxproj files in the current folder and any subfolders}
#'
#'   \item{NA}{all .xlsx, .db, .wksz and, if requested, all .sft, .siva, .cvbe, or .phxproj files in
#'   the current folder}
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
#' @param search_workspaces_for_obsfile TRUE or FALSE (default) for whether to
#'   search through any workspace files and check for possible XML overlay
#'   files. This runs considerably slower when set to TRUE and will take about
#'   an additional 2 seconds per simulation. If you have supplied something for
#'   'existing_exp_details', you don't need this and we'll ignore it if you set
#'   this to TRUE.
#' @param simfile_path_option How would you like to see the path for each
#'   simulation file? Options are: \describe{
#'
#'   \item{"full path" ("full" also works)}{Show the entire path}
#'
#'   \item{"relative" (default)}{Show the path relative to the project folder}}
#'
#' @param obsfile_path_option How would you like to see the observed overlay XML
#'   file path? Options are: \describe{
#'
#'   \item{"full path" (default, "full" also works)}{Show the entire path}
#'
#'   \item{"basename"}{Only show the file name (the basename)
#'   of the XML overlay file and NOT any of the path. Why would you want
#'   this? Say you've moved all your XML observed overlay files from their
#'   original locations when you ran the simulations into a new folder that
#'   you'll be sharing with the client at the end of the project. This way, you
#'   can show which simulation had which XML observed overlay file without the
#'   confusion of including the original path.}
#'
#'   \item{"relative"}{Show the path relative to the project folder}}
#'
#' @param save_table optionally specify an Excel file name for saving your
#'   simulation directory.
#' @param overwrite Should we overwrite if your Excel file already exists and
#'   already has a tab named "Simulation directory"? Options are "yes" to always
#'   overwrite, "no" to never overwrite, or "ask" (default), which means that we
#'   will ask you whether to overwrite and give you a chance to supply a
#'   different file name if the one you supplied already exists.
#' @param report_progress "yes", "no" (default), or "some" for whether to report
#'   progress on reading workspaces to find observed XML overlay files. This
#'   only applies when you have set \code{search_workspaces_for_obsfile} to
#'   TRUE. Setting this to "yes" will report a message in the console for each
#'   workspace as it is read. Setting this to "some" will report a message
#'   saying when it has started and when it has finished reading workspaces, and
#'   setting this to "no" will give no progress messages.
#' @param include_possible_obsfiles TRUE (default) or FALSE for whether to
#'   include in the output simulation directory possible XML
#'   observed-data-overlay files. This will be ignored if you have supplied
#'   something for the argument \code{existing_exp_details} because we'll just
#'   figure out which observed files you have from that.
#' @param figure_and_table_assignment optionally supply a data.frame of which
#'   simulation file matches which figure and table in a report. We won't do
#'   anything with this beyond use this information to fill in the column
#'   "Table/Figure" in the simulation directory. To supply this information,
#'   make a data.frame with a column titled "Filename", which must contain the
#'   simulation file names (file extensions are optional and will ultimately be
#'   ignored anyway) and a column titled "Table/Figure", which should contain
#'   the name of any table or figure in a report that shows data from that
#'   simulation file. Please use one row per simulation. If there are multiple
#'   figures or tables for each simulation, you can either list one figure or
#'   table per row or you can list them together, separated by commas. Here is
#'   an example: \code{data.frame(File = c("01-abc.xlsx", "02-abc.xlsx",
#'   "03-abc.xlsx", "03-abc.xlsx"), `Table/Figure` = c("Figure 1, Figure 2,
#'   Table 1", "Figure 3", "Figure 4", "Table 2"))} The output from running
#'   \code{\link{find_report_files}} will work great here. Any simulations that
#'   are not present in your simulation directory will be ignored.
#' @param include_related_filetypes TRUE or FALSE (default) for whether to
#'   include other possibly related file types in the directory. In addition to
#'   workspaces and Excel or database files, these would be files with
#'   extensions .sft, .siva, .cvbe, or .phxproj.
#'
#' @return a data.frame of simulation files and their respective file paths
#' @export
#'
#' @examples
#' make_simulation_directory(
#'    project_folder = "C:/Users/Buffy/Project ABC/Modelling",
#'    save_table = "Simulation directory for project ABC.xlsx")

make_simulation_directory <- function(project_folder = NA, 
                                      sim_data_files = "recursive", 
                                      simfile_path_option = "relative", 
                                      existing_exp_details = NA, 
                                      search_workspaces_for_obsfile = FALSE, 
                                      include_possible_obsfiles = TRUE, 
                                      obsfile_path_option = "full path", 
                                      figure_and_table_assignment = NA, 
                                      include_related_filetypes = FALSE, 
                                      report_progress = "no", 
                                      save_table = NA, 
                                      overwrite = "ask"){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
   
   if(length(project_folder) > 1){
      stop("You can only search one project folder at a time. Please check your input for `project_folder`.", 
           call. = FALSE)
   }
   
   if("logical" %in% class(existing_exp_details) == FALSE & 
      search_workspaces_for_obsfile == TRUE){
      # NB: Decided against giving this warning for now. 
      
      # warning(wrapn("You requested that we search through workspaces to look for XML observed data overlay file names, but you also supplied something for 'existing_exp_details', which should already have that infomation. We will save you some time and *not* search through your workspaces for the XML observed data overlay file name."), 
      #         call. = F)
      search_workspaces_for_obsfile <- FALSE
   }
   
   simfile_path_option <- tolower(simfile_path_option)[1]
   simfile_path_option <- case_match(simfile_path_option, 
                                     "full" ~ "full path", 
                                     "full path" ~ "full path", 
                                     "fullpath" ~ "full path", 
                                     "relative" ~ "relative")
   
   if(is.na(simfile_path_option)){
      warning(wrapn("For the argument 'simfile_path_option', you requested something other than 'full path' or 'relative', which are the only available options. We'll use the default value of 'relative'."), 
              call. = FALSE)
   }
   
   obsfile_path_option <- tolower(obsfile_path_option)[1]
   obsfile_path_option <- case_match(obsfile_path_option, 
                                     "full" ~ "full path", 
                                     "full path" ~ "full path", 
                                     "fullpath" ~ "full path", 
                                     "basename" ~ "basename", 
                                     "base name" ~ "basename", 
                                     "base" ~ "basename", 
                                     "relative" ~ "relative")
   
   if(is.na(obsfile_path_option)){
      warning(wrapn("For the argument 'obsfile_path_option', you requested something other than 'full path', 'basename', or 'relative', which are the only available options. We'll use the default value of 'full path'."), 
              call. = FALSE)
   }
   
   report_progress <- tolower(report_progress)[1]
   report_progress <- case_match(report_progress, 
                                 "yes" ~ "yes", 
                                 "y" ~ "yes", 
                                 "n" ~ "no", 
                                 "no" ~ "no", 
                                 "some" ~ "some", 
                                 "report some" ~ "some")
   if(is.na(report_progress)){
      # Not giving a warning if they supplied an invalid value b/c not important
      # and I don't want to overwhelm people w/warnings.
      report_progress <- "no"
   }
   
   if(any(complete.cases(figure_and_table_assignment))){
      
      if(all(c("File name", "File type", "Table/Figure") %in% 
             names(figure_and_table_assignment))){
         figure_and_table_assignment <- figure_and_table_assignment %>% 
            rename(Filename = "File name") %>% 
            select(Filename, `Table/Figure`)
      } else {
         
         FigTabNames <- 
            tibble(Orig = tolower(names(figure_and_table_assignment))) %>% 
            mutate(Rev = case_when(
               str_detect(Orig, "file|workspace|sim") ~ "Filename", 
               str_detect(Orig, "table|fig") ~ "Table/Figure"))
         
         if(length(which(FigTabNames$Rev == "Filename")) > 1){
            warning(wrapn("In what you have provided for 'figure_and_table_assignment', you have more than one column that looks like it contains the file name. We don't know which to use, so we'll have to ignore your input for 'figure_and_table_assignment'."), 
                    call. = FALSE)
            
            figure_and_table_assignment <- NA
         } else if(length(which(FigTabNames$Rev == "Table/Figure")) > 1){
            warning(wrapn("In what you have provided for 'figure_and_table_assignment', you have more than one column that looks like it contains the table and figure names. We don't know which to use, so we'll have to ignore your input for 'figure_and_table_assignment'."), 
                    call. = FALSE)
            
            figure_and_table_assignment <- NA
         } else {
            
            figure_and_table_assignment <- figure_and_table_assignment %>% 
               as_tibble() 
            names(figure_and_table_assignment) <- tolower(names(figure_and_table_assignment))
            
            names(figure_and_table_assignment)[
               names(figure_and_table_assignment) == FigTabNames$Orig[FigTabNames$Rev == "Filename"]] <- "Filename"
            
            names(figure_and_table_assignment)[
               names(figure_and_table_assignment) == FigTabNames$Orig[FigTabNames$Rev == "Table/Figure"]] <- "Table/Figure"
            
            figure_and_table_assignment %>% 
               select(Filename, `Table/Figure`)
         }
      }
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
                                         recursive = TRUE))) 
      }
      
   } else {
      # If length(sim_data_files) > 1, then they have supplied a character
      # vector of files. If they didn't include ".xlsx" at the end, add that.
      Directory <- 
         tibble(File = sim_data_files) %>% 
         mutate(File = case_when(str_detect(File, "\\.xlsx|\\.db|\\.wksz") == FALSE ~ 
                                    paste0(File, ".xlsx"), 
                                 .default = File), 
                File = basename(File)) %>% 
         # adding path 
         left_join(tibble(
            PathFile = paste0(project_folder, 
                              list.files(path = project_folder, 
                                         recursive = TRUE)), 
            File = basename(PathFile)), 
            by = "File")
   } 
   
   if("File" %in% names(Directory) == FALSE){
      Directory$File <- Directory$PathFile
   }
   
   # Removing temporary files and making sure that all File values are basename. 
   Directory <- Directory %>% 
      mutate(File = case_when(complete.cases(PathFile) ~ basename(PathFile), 
                              .default = File)) %>% 
      filter(is.na(PathFile) | 
                (complete.cases(PathFile) & !str_detect(PathFile, "^~"))) %>% 
      filter(str_detect(File, "xlsx$|wksz$|db$")) %>% 
      mutate(Folder = dirname(PathFile), 
             Folder = ifelse(Folder == ".", getwd(), Folder), 
             Folder = if_else(file.exists(PathFile), Folder, "FILE NOT FOUND"),  
             Folder = case_when(Folder == "FILE NOT FOUND" & nchar(PathFile) > 260 ~ 
                                   "FILE PATH TOO LONG",
                                .default = Folder), 
             Filename = sub("\\.xlsx|\\.db|\\.wksz", "", File),
             Filetype = str_extract(File, "xlsx$|wksz$|db$"))
   
   # Adding Filename to existing_exp_details since we may need to match up later
   if("NULL" %in% class(existing_exp_details) == FALSE){
      existing_exp_details$MainDetails <- existing_exp_details$MainDetails %>% 
         mutate(Filename = sub("\\.xlsx|\\.db|\\.wksz", "", basename(File)))
   }
   
   # Removing from consideration any files that were not included in
   # existing_exp_details, if that was supplied, or if sim_data_files was
   # "recursive" or NA if the folder is now "FILE NOT FOUND" and if the Filetype
   # is NOT xlsx. Removing these b/c we're the ones who added them: They are
   # just variations on the simulation file names that we're checking to see if
   # they exist, e.g., workspaces or database files.
   if((any(c("logical", "NULL") %in% class(existing_exp_details)) == FALSE) | 
      (length(sim_data_files) == 1 && 
       (is.na(sim_data_files) || sim_data_files == "recursive" | 
        !str_detect(sim_data_files, "[^\\.]|\\.xlsx")))){
      Directory <- Directory %>% 
         filter(!(Folder == "FILE NOT FOUND" & Filetype != "xlsx"))
   }
   
   if(length(Directory$Filename[Directory$Folder != "FILE NOT FOUND"]) == 0){
      # This will happen if they have supplied something for
      # existing_exp_details but those files aren't in the project folder.
      stop(wrapn(paste0("We can't find any of the files in your project folder. Are you in the main folder for this project or have you set the argument 'project_folder' to that folder? Please set the project folder and try again.")), 
           call. = FALSE)
   }
   
   if(nrow(Directory) == 0){
      stop(wrapn("There are no files that match the pattern of text you supplied for 'sim_data_files', so we cannot make your simulation diratory."), 
           call. = FALSE)
   }
   
   # We don't need the simulation directory file name to show up in the
   # simulation directory b/c that's just way too meta.
   if(complete.cases(save_table)){
      Directory <- Directory %>% 
         filter(Filename != save_table)
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
   
   # Formatting per FDA/Consultancy Team requirements
   Directory <- Directory %>% 
      mutate(Folder = case_when(Folder == "." ~ "", 
                                .default = Folder)) %>% 
      select(Filename, Filetype, Folder, `Table/Figure`, Comments, FileNameCheck, DuplicateFileCheck) %>% 
      unique()
   
   ## Dealing with obs overlay XML files --------------------------------------
   
   if("NULL" %in% class(existing_exp_details) == FALSE){
      
      if("ObsOverlayFile" %in% names(existing_exp_details$MainDetails)){
         if(any(complete.cases(existing_exp_details$MainDetails$ObsOverlayFile))){
            
            Directory <- Directory %>% 
               left_join(existing_exp_details$MainDetails %>% 
                            select(Filename, ObsOverlayFile), by = "Filename")
            
            ObsOverlayKnown <- TRUE
            
         } else {
            
            ObsOverlayKnown <- FALSE
            Directory$XMLFileNameCheck <- ""
            
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
         Directory$ObsOverlayFile <- as.character(NA)
      }
   } else {
      ObsOverlayKnown <- FALSE
      Directory$XMLFileNameCheck <- ""
      Directory$ObsOverlayFile <- as.character(NA)
   }
   
   if(search_workspaces_for_obsfile){
      
      Directory$ObsOverlayFile <- as.character(NA)
      
      if(report_progress == "some"){
         message("Starting to read workspaces for observed XML overlay file names...\n")
      }
      
      for(ff in 1:nrow(Directory)){
         
         if(Directory$Folder[ff] %in% c("check file name - no folder found", 
                                        "FILE PATH TOO LONG", 
                                        "FILE NOT FOUND")){
            next
         }
         
         MyWksp <- paste0(
            case_when(Directory$Folder[ff] == "" ~ "", 
                      .default = paste0(Directory$Folder[ff], "/")),
            Directory$Filename[ff], ".wksz")
         
         if(file.exists(MyWksp) == FALSE){
            warning(wrapn(paste0("Cannot find file: ", MyWksp)), 
                    call. = FALSE)
         } else {
            if(report_progress == "yes"){
               message(paste0("Reading '", MyWksp, "'"))
            }
            
            Directory$ObsOverlayFile[ff] <- get_obs_file(workspace = MyWksp)
         }
      }
      
      if(report_progress == "some"){
         message("... done.")
      }
      
      ObsOverlayKnown <- TRUE
   }
   
   if(ObsOverlayKnown){
      
      # Need the basename to check for file naming standards. 
      Directory <- Directory %>% 
         mutate(ObsOverlayFile = gsub("\\\\", "/", ObsOverlayFile), 
                ObsOverlayBaseName = sub("\\..*$", "", basename(ObsOverlayFile))) %>% 
         arrange(Folder, Filename)
      
      if(obsfile_path_option == "relative"){
         
         Directory <- Directory %>% 
            mutate(ObsOverlayFile = R.utils::getRelativePath(ObsOverlayFile, 
                                                             relativeTo = project_folder), 
                   ObsOverlayFile = sub("C:/Users.*Certara/Simcyp PBPKConsult BMG BMG43A - Model(l)?ing Working Directory/",
                                        "", ObsOverlayFile), 
                   
                   ObsOverlayFile = case_when(
                      str_detect(ObsOverlayFile, "Working Directory/XMLs") ~ 
                         sub(".*/XMLs", "XMLs", ObsOverlayFile),
                      .default = ObsOverlayFile))
      }
      
      Directory <- Directory %>% 
         mutate(XMLFileNameCheck = check_file_name(ObsOverlayBaseName), 
                XMLFileNameCheck = case_when(
                   XMLFileNameCheck == "File name meets naming standards." ~ "", 
                   is.na(XMLFileNameCheck) ~ "", 
                   .default = sub("File name", "XML file name", XMLFileNameCheck))) 
      
      if(obsfile_path_option == "basename"){
         Directory <- Directory %>% 
            select(-ObsOverlayFile) %>% 
            rename("XML file used" = ObsOverlayBaseName)
      } else {
         Directory <- Directory %>% 
            rename("XML file used" = ObsOverlayFile) %>% 
            select(-ObsOverlayBaseName)
      }
   } else if(include_possible_obsfiles){
      
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
   
   # Making relative paths
   if(simfile_path_option == "relative"){
      Directory$Folder <- 
         R.utils::getRelativePath(Directory$Folder,
                                  relativeTo = project_folder)
   }
   
   if("XML file used" %in% names(Directory)){
      if(obsfile_path_option == "basename"){
         Directory$`XML file used` <-
            basename(Directory$`XML file used`)
         
      } else if(obsfile_path_option == "relative"){
         Directory$`XML file used` <- 
            R.utils::getRelativePath(Directory$`XML file used`,
                                     relativeTo = project_folder)
      }
      
   }
   
   ## Adding any known figure and table assignments -------------------------
   
   if(any(complete.cases(figure_and_table_assignment))){
      
      figure_and_table_assignment <- figure_and_table_assignment %>% 
         mutate(
            Filename = basename(str_trim(Filename)), 
            Filename = sub("\\.xlsx|\\.wksz|\\.db", "", Filename), 
            # Cleaning up possible punctuation issues
            `Table/Figure` = gsub(" and| or| \\&", "", str_trim(`Table/Figure`)), 
            `Table/Figure` = gsub("  {1,}", " ", `Table/Figure`), 
            `Table/Figure` = gsub(",T", ", T", `Table/Figure`), 
            `Table/Figure` = gsub(",F", ", F", `Table/Figure`), 
            `Table/Figure` = gsub("table", "Table", `Table/Figure`), 
            `Table/Figure` = gsub("figure", "Figure", `Table/Figure`), 
            `Table/Figure` = gsub(" ", ", ", `Table/Figure`), 
            `Table/Figure` = gsub("Figure, ", "Figure ", `Table/Figure`), 
            `Table/Figure` = gsub("Table, ", "Table ", `Table/Figure`), 
            `Table/Figure` = gsub(",, ", ", ", `Table/Figure`)) %>% 
         separate_longer_delim(cols = `Table/Figure`, 
                               delim = ", ") %>% 
         mutate(Num = as.numeric(str_extract(`Table/Figure`, "[0-9]{1,}$")), 
                `Table/Figure` = str_extract(`Table/Figure`, "Table|Figure")) %>% 
         arrange(Filename, `Table/Figure`, Num) %>% 
         mutate(`Table/Figure` = paste(`Table/Figure`, Num)) %>% 
         group_by(Filename) %>% 
         # summarize(`Table/Figure` = length(unique(`Table/Figure`)))
         summarize(`Table/Figure` = str_c(unique(`Table/Figure`), collapse = ", ")) %>% 
         ungroup() 
      
      Directory <- Directory %>% 
         select(-`Table/Figure`) %>% 
         left_join(figure_and_table_assignment, 
                   by = "Filename")
      
   }
   
   
   ## Setting column order --------------------------------------------------
   
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
      
      if(any(Directory$Folder %in% c("FILE NOT FOUND", 
                                     "FILE PATH TOO LONG"), na.rm = T)){
         Highlighting[["Folder"]] <- 
            list("rows" = which(Directory$Folder %in% c("FILE NOT FOUND", 
                                                        "FILE PATH TOO LONG")), 
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



