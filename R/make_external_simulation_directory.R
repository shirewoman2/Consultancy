# Functions for creating simulation directories

# Find references to simulations in a Word report ---------------------------------

#' Find references to simulations in a Word report
#'
#' @description \code{find_sims_in_report} reads a Word report into memory,
#'   finds all references to simulations in that Word report, and then looks at
#'   the text preceding each reference to determine which figure or table shows
#'   the results of each simulation listed in that report.
#'
#'   For finding simulations, it looks for the text "Source simulated data:" and
#'   then all Excel files listed in that same paragraph. References to
#'   simulations MUST include the ".xlsx" file extension for R to find it.
#'
#'   For determining which figure or table each reference is for, it looks for
#'   the immediate paragraph before each simulation reference that starts with
#'   "Figure X" or "Table X" where X is a number.
#'
#' @param report_name the file name of the report, including the path if it's
#'   not in your current working directory
#' @param save_sims_in_report optionally save the list of the references it
#'   found to a csv file so that you can check that the simulations and which
#'   figure or table they refer to appear to be correct. For example, if you
#'   spot a typo here, you can find it and fix it in your report and then re-run
#'   this.
#'
#' @returns an object containing the report text
#' @export
#'
#' @examples
#' ReportRefs <- find_sims_in_report(report_name = "abc-1a PBPK report.docx")
#' 
find_sims_in_report <- function(report_name, 
                                save_sims_in_report = NA){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Check file extension
   if(str_detect(report_name, "\\.docx$") == FALSE){
      stop(SimcypConsultancy:::wrapn("Your file must have the extension '.docx'."))
   }
   
   
   # Main body of function ---------------------------------------------------
   
   message(paste0("Reading '", report_name, "'. This reads about 4-5 report pages per second."))
   
   OfficerObj <- officer::read_docx(report_name)
   
   # This will take a little bit
   BodyText <- officer::docx_summary(OfficerObj, remove_fields = TRUE)
   
   # Looking for text indicating tables and figures. 
   TextMatching <- tibble(
      Index = 1:nrow(BodyText), 
      Text = BodyText$text, 
      FigTab = str_extract(Text, "^(Table|Figure).[0-9]{1,}"), 
      File = as.character(str_extract_all(Text, "[Ss]ource simulated data: [a-z|0-9]{1,}[^ ].*.xlsx"))) %>% 
      mutate(
         File = gsub("[Ss]ource simulated data: |,| and", "", File), 
         File = case_when(File == "character(0)" ~ NA, 
                          .default = File),
         # placeholder
         `Table/Figure` = as.character(NA)
      )
   # NB: 1 row in the tibble = 1 paragraph
   
   # Looping through the places where there is an Excel file listed and finding the
   # figure or table that was listed either in the same paragraph (index will be
   # the same) or the most-recent paragraph before that Excel file's paragraph
   # (index will be the last one before the current index).
   for(i in TextMatching$Index[complete.cases(TextMatching$File)]){
      
      PrevFigTab <- TextMatching %>% 
         filter(Index <= i & complete.cases(FigTab)) %>% 
         arrange(desc(Index)) %>% 
         slice(1) %>% 
         pull(FigTab)
      
      TextMatching$`Table/Figure`[which(TextMatching$Index == i)] <- PrevFigTab
      
      rm(PrevFigTab)
      
   }
   
   # Next, need to separate the Excel files at all the spaces so that we can get
   # one row per Excel file.
   TextMatching <- TextMatching %>% 
      filter(complete.cases(`Table/Figure`)) %>% 
      separate_longer_delim(cols = File, delim = " ") %>% 
      separate_wider_delim(cols = `Table/Figure`, 
                           delim = " ", 
                           names = c("FigureOrTable", "Number"), 
                           cols_remove = FALSE) %>% 
      mutate(Number = as.numeric(Number)) %>% 
      arrange(FigureOrTable, Number) %>% 
      mutate(`Table/Figure` = factor(`Table/Figure`, levels = unique(`Table/Figure`)))
   
   # Mention in R console how many unique output names were found in the report.
   # This may be useful for people, especially if they know how many simulations
   # they are expecting in the report.
   message(SimcypConsultancy:::wrapn(paste0(
      "Found ", 
      TextMatching$File %>% sort() %>% unique() %>% length(), 
      " unique simulations referenced in the text.")))
   
   Out <- TextMatching %>% 
      select(Index, Text, File, `Table/Figure`)
   
   if(complete.cases(save_sims_in_report[1])){
      
      # Check file extension
      Ext <- str_extract(save_sims_in_report, "\\.*$")
      if(length(Ext) > 0 & Ext != ".csv"){
         warning(SimcypConsultancy:::wrapn("The only acceptable option for saving the table of simulations found is as a csv file."), 
                 call. = FALSE)
      }
      
      FileName <- paste0(sub(Ext, "", save_sims_in_report), ".csv")
      
      write.csv(Out, FileName, row.names = F)
   }
   
   return(Out)
   
}



# Find the location of files listed in the report -----------------------------

#' Find the location of all the simulation files included in a report
#'
#' @description \code{find_report_file_locations} looks for simulation files
#'   that were included in a report. To do this, it relies on input from running
#'   \code{\link{find_sims_in_report}}.
#'
#' @param sims_in_report output from running \code{\link{find_sims_in_report}},
#'   a data.frame with columns titled "Index" (paragraph number), "Text" (the
#'   text in that paragraph), "File" the simulation Excel output file name, and
#'   "Table/Figure" for the matched figure or table where the simulation
#'   appears.
#' @param project_folder location of the project folder to search. Please note
#'   that R requires forward slashes ("/") in file paths rather than the back
#'   slashes Microsoft uses. If left as NA (default), we'll assume your current
#'   working directory is the top level of your project folder.
#' @param save_draft_simulation_directory optionally specify an Excel file name
#'   for saving your simulation directory.
#' @param overwrite Should we overwrite if your Excel file already exists and
#'   already has a tab named "Simulation directory"? Options are "yes" to always
#'   overwrite, "no" to never overwrite, or "ask" (default), which means that we
#'   will ask you whether to overwrite and give you a chance to supply a
#'   different file name if the one you supplied already exists.
#' @returns a draft simulation directory as a data.frame
#' @export
#'
#' @examples
#' DraftSimDirectory <- find_report_file_locations(
#'    sims_in_report = ReportRefs,
#'    project_folder = "C:/Users/MyName/abc-1a/Modeling",
#'    save_draft_simulation_directory = "abc-1a draft sim directory.xlsx")
#'
#' 

# alternative name: find_report_files? Discussed on 8/5/25 and thought that
# "find_report_file_locations" was clearer.
find_report_file_locations <- function(sims_in_report, 
                                       project_folder = NA, 
                                       save_draft_simulation_directory = NA, 
                                       overwrite = "ask"){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if("data.frame" %in% class(sims_in_report) == FALSE ||
      all(c("Index", "Text", "File", "Table/Figure") %in% 
          names(sims_in_report)) == FALSE){
      stop(SimcypConsultancy:::wrapn("You do not appear to have provided output from running the function 'find_sims_in_report' as input for the argument 'sims_in_report', which is what we need to proceed."), 
           call. = FALSE)
   }
   
   if("character" %in% class(sims_in_report)){
      Ext <- str_extract(sims_in_report, "\\.*$")
      
      if(is.na(Ext)){
         sims_in_report <- paste0(sims_in_report, ".csv")
      } else {
         if(Ext != ".csv"){
            stop(SimcypConsultancy:::wrapn("The input for the argument 'sims_in_report' must be either a data.frame or a .csv file, and this is something else. We cannot proceed."), 
                 call. = FALSE)
         }
      }
      
      sims_in_report <- read.csv(sims_in_report)
      
   } else if("data.frame" %in% class(sims_in_report) == FALSE){
      stop(SimcypConsultancy:::wrapn("The input for the argument 'sims_in_report' must be either a data.frame or a .csv file, and this is something else. We cannot proceed."), 
           call. = FALSE)
   }
   
   sims_in_report <- sims_in_report %>% as_tibble()
   names(sims_in_report) <- tolower(names(sims_in_report))
   
   SimsInReportNames <- 
      tibble(Orig = names(sims_in_report)) %>% 
      mutate(Rev = case_when(
         str_detect(Orig, "file|workspace|sim") ~ "File", 
         str_detect(Orig, "table|fig") ~ "Table/Figure", 
         str_detect(Orig, "index") ~ "Index", 
         str_detect(Orig, "text") ~ "Text", 
         .default = Orig))
   
   sims_in_report <- sims_in_report[, SimsInReportNames$Orig]
   names(sims_in_report) <- SimsInReportNames$Rev
   
   if(all(c("Index", "Text", "File", "Table/Figure") %in% 
          names(sims_in_report)) == FALSE){
      stop(SimcypConsultancy:::wrapn("We need the input for the argument 'sims_in_report' to be formatted like the output from running 'find_sims_in_report', and you have provided something else. We cannot proceed."), 
           call. = FALSE)
   }
   
   sims_in_report <- sims_in_report %>% 
      select(Index, Text, File, `Table/Figure`)
   
   # Main body of function ----------------------------------------------------
   
   message("Looking for your simulation files.")
   
   # Getting the .xlsx and the matching .wksz version of these. At some point,
   # we should probably add .db files. 
   Out <- make_simulation_directory(
      project_folder = project_folder, 
      sim_data_files = c(sort(unique(sims_in_report$File)),
                         sub("xlsx", "wksz", sort(unique(sims_in_report$File)))), 
      simfile_path_option = "full path", 
      obsfile_path_option = "full path", 
      existing_exp_details = NA, 
      search_workspaces_for_obsfile = T, 
      report_progress = "yes", 
      include_possible_obsfiles = F, 
      figure_and_table_assignment = sims_in_report %>% select(File, `Table/Figure`), 
      overwrite = overwrite, 
      save_table = save_draft_simulation_directory)
   
   return(Out)
   
}



# Copy files -----------------------------------------------------------------

#' Copy all the files in a draft simulation directory to a different folder
#'
#' @description We strongly recommend providing \strong{full file paths} here or
#'   you might not be copying what you think you're copying.
#'
#' @param draft_simulation_directory This function was written with the idea
#'   that what you supply here is the output from running either
#'   \code{\link{make_simulation_directory}} or
#'   \code{\link{find_report_file_locations}}, either of which will find all of
#'   the simulation files (both workspaces and Excel outputs as well as any
#'   observed XML overlay files) in a project folder. You can use this to copy
#'   files in general, though, as long as what you supply for
#'   draft_simulation_directory includes a column "File name" with the file
#'   names and a column "Folder" that lists the path where that file is located
#'   (relative paths from your current working directory are fine).
#' @param sim_directory_folder folder where you want to put the copies of your
#'   simulation files.
#' @param create_folder If \code{sim_directory_folder} does not exist, would you
#'   like it to be created? Options are "yes", "no", or "ask" (default).
#' @param overwrite If any of your simulation files already exist in
#'   \code{sim_directory_folder}, would you like them to be overwritten? Options
#'   are "yes", "no", and "ask" (default).
#'
#' @returns Does not return any R objects; only copies files
#' @export
#'
#' @examples
#' copy_report_files(
#'    draft_simulation_directory = DraftSimDirectory,
#'    sim_directory_folder = "C:/Users/MyName/abc-1a/Modeling/Final sims")

copy_report_files <- function(draft_simulation_directory, 
                              sim_directory_folder, 
                              create_folder = "ask", 
                              overwrite = "ask"){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if("data.frame" %in% class(draft_simulation_directory) == FALSE ||
      all(c("File name", "Folder") %in% 
          names(draft_simulation_directory)) == FALSE){
      stop(SimcypConsultancy:::wrapn("You do not appear to have provided output from running either 'make_simulation_directory' or 'find_report_file_locations' for the argument 'draft_simulation_directory', which is what we need to proceed. Please check your input and try again."), 
           call. = FALSE)
   }
   
   if("character" %in% class(draft_simulation_directory)){
      Ext <- str_extract(draft_simulation_directory, "\\.*$")
      
      if(is.na(Ext)){
         draft_simulation_directory <- paste0(draft_simulation_directory, ".xlsx")
      } else {
         if(Ext %in% c(".xlsx", ".csv") == FALSE){
            stop(SimcypConsultancy:::wrapn("The input for the argument 'draft_simulation_directory' must be a data.frame, an Excel file, or a .csv file, and this is something else. We cannot proceed."), 
                 call. = FALSE)
         }
      }
      
      if(Ext == ".xlsx"){
         draft_simulation_directory <-
            readxl::read_xlsx(draft_simulation_directory, 
                              sheet = "Simulation directory")
      } else {
         draft_simulation_directory <- read.csv(draft_simulation_directory)
      }
      
   } else if("data.frame" %in% class(draft_simulation_directory) == FALSE){
      stop(SimcypConsultancy:::wrapn("The input for the argument 'draft_simulation_directory' must be a data.frame, an Excel file, or a .csv file, and this is something else. We cannot proceed."), 
           call. = FALSE)
   }
   
   draft_simulation_directory <- draft_simulation_directory %>% as_tibble()
   names(draft_simulation_directory) <- tolower(names(draft_simulation_directory))
   
   DraftSimDirNames <- 
      tibble(Orig = names(draft_simulation_directory), 
             Rev = Orig)
   
   DraftSimDirNames$Rev[which(
      str_detect(DraftSimDirNames$Orig, "file|workspace|sim") & 
         !str_detect(DraftSimDirNames$Orig, "type|xml|check"))][1] <- "File name"
   
   DraftSimDirNames$Rev[which(
      str_detect(DraftSimDirNames$Orig, "xml"))][1] <- "XML file used"
   
   DraftSimDirNames$Rev[which(
      str_detect(DraftSimDirNames$Orig, "folder"))][1] <- "Folder"
   
   draft_simulation_directory <- draft_simulation_directory[, DraftSimDirNames$Orig]
   names(draft_simulation_directory) <- DraftSimDirNames$Rev
   
   if(all(c("File name", "Folder", "XML file used") %in% 
          names(draft_simulation_directory)) == FALSE){
      stop(SimcypConsultancy:::wrapn("We need the input for the argument 'draft_simulation_directory' to be formatted like the output from running 'find_report_file_locations', and you have provided something else. We cannot proceed."), 
           call. = FALSE)
   }
   
   draft_simulation_directory <- draft_simulation_directory %>% 
      select(`File name`, Folder, `XML file used`)
   
   # Ensuring that `File name` is, in fact, a basename and that we don't have
   # any needless, easily removed replicates.
   draft_simulation_directory <- draft_simulation_directory %>% 
      mutate(`File name` = basename(`File name`)) %>% unique()
   
   if(any(duplicated(draft_simulation_directory$`File name`))){
      Dups <- draft_simulation_directory %>% 
         filter(duplicated(`File name`)) %>% 
         pull(`File name`) %>% unique()
      
      Duplicates <- draft_simulation_directory %>% 
         filter(`File name` %in% Dups) %>% 
         select(`File name`, Folder) %>% 
         arrange(`File name`)
      
      view(Duplicates)
      
      stop(SimcypConsultancy:::wrapn("Some of the same simulation files exist in multiple subfolders, so we would overwrite the 2nd one we copy into your sim_directory_folder. We cannot proceed until you remove or rename the duplicate files shown."), 
           call. = FALSE)
   }
   
   create_folder <- tolower(as.character(create_folder[1]))
   create_folder <- case_match(create_folder, 
                               "y" ~ "yes", 
                               "yes" ~ "yes", 
                               "t" ~ "yes", 
                               "true" ~ "yes", 
                               "n" ~ "no", 
                               "no" ~ "no", 
                               "ask" ~ "ask")
   
   overwrite <- tolower(overwrite[1])
   overwrite <- case_match(overwrite, 
                           "y" ~ "yes", 
                           "yes" ~ "yes", 
                           "t" ~ "yes", 
                           "true" ~ "yes", 
                           "n" ~ "no", 
                           "no" ~ "no", 
                           "ask" ~ "ask")
   
   
   # Main body of function ----------------------------------------------------
   
   # Making the full paths for copying and making sure that user name in file
   # matches current user name if that's part of the path.
   CurrentUser <- Sys.info()["user"]
   
   FilesToCopy <- draft_simulation_directory %>% 
      rename(FullPath.xml = `XML file used`) %>% 
      mutate(FullPath.xlsx = paste0(Folder, "/", basename(`File name`), ".xlsx"), 
             FullPath.wksz = paste0(Folder, "/", basename(`File name`), ".wksz"), 
             NewPath.xlsx = paste0(sim_directory_folder, "/", basename(`File name`), ".xlsx"), 
             NewPath.wksz = paste0(sim_directory_folder, "/", basename(`File name`), ".wksz"), 
             NewPath.xml = 
                case_when(is.na(FullPath.xml) ~ NA, 
                          .default = paste0(sim_directory_folder, "/", basename(FullPath.xml)))) %>% 
      select(`File name`, matches("FullPath|NewPath")) %>% 
      mutate(across(.cols = matches("Path"), 
                    .fns = \(x) {
                       str_replace(string = x,
                                   pattern = "Users/.*/Certara", 
                                   replacement = paste0("Users/", CurrentUser, "/Certara"))}))  
   
   
   ## Making folder as needed ------------------------------------------------
   
   if(file.exists(sim_directory_folder) == FALSE){
      if(create_folder == "no"){
         stop(SimcypConsultancy:::wrapn(paste0("The folder '", 
                                               sim_directory_folder, 
                                               "' does not exist, and you requested that we not create it. We cannot copy your simulation files.")), 
              call. = FALSE)
      } else if(create_folder == "yes"){
         dir.create(sim_directory_folder)
      } else if(create_folder == "ask"){
         message(str_wrap(paste0("\nThe folder you requested for copying your simulation files into, '", 
                                 sim_directory_folder, 
                                 "', does not exist. Ok to create it? Options are 'yes' or 'no'")))
         Q_create_folder <- readline("   ")
         message("\n")
         
         Q_create_folder <- tolower(as.character(Q_create_folder[1]))
         Q_create_folder <- case_match(Q_create_folder, 
                                       "y" ~ "yes", 
                                       "yes" ~ "yes", 
                                       "t" ~ "yes", 
                                       "true" ~ "yes", 
                                       "n" ~ "no", 
                                       "no" ~ "no", 
                                       "f" ~ "no", 
                                       "false" ~ "no")
         
         if(is.na(Q_create_folder)){
            stop(SimcypConsultancy:::wrapn("We did not understand your input. Please try again."), 
                 call. = FALSE)
         }
         
         if(Q_create_folder == "yes"){
            dir.create(sim_directory_folder)
         } else {
            stop(SimcypConsultancy:::wrapn(paste0("The folder '", 
                                                  sim_directory_folder, 
                                                  "' does not exist, and you requested that we not create it. We cannot copy your simulation files.")), 
                 call. = FALSE)
         }
      }
   }
   # Folder now exists. 
   
   ## Checking for existing files that would be overwritten ------------------
   
   FilesAlreadyInFolder <- c(FilesToCopy$FullPath.xlsx, 
                             FilesToCopy$FullPath.wksz, 
                             FilesToCopy$FullPath.xml)
   
   FilesAlreadyInFolder <- FilesAlreadyInFolder[
      file.exists(c(FilesToCopy$NewPath.xlsx, 
                    FilesToCopy$NewPath.wksz, 
                    FilesToCopy$NewPath.xml))]
   
   if(length(FilesAlreadyInFolder) > 0){
      
      if(overwrite == "no"){
         
         stop(paste0(SimcypConsultancy:::wrapn("Some of the files you're attempting to copy already exist in the destination folder, and you asked us not to overwrite them. We cannot proceed until you remove or rename the following file:"), 
                     str_c(FilesAlreadyInFolder, collapse = "\n")), 
              call. = FALSE)
         
      } else if(overwrite == "ask"){
         message(str_wrap(paste0("\nSome of the files you're attempting to copy already exist in the destination folder '", 
                                 sim_directory_folder, 
                                 "'. Ok to overwrite those files? Options are 'yes' or 'no'")))
         Q_overwrite <- readline("   ")
         message("\n")
         
         Q_overwrite <- tolower(as.character(Q_overwrite[1]))
         Q_overwrite <- case_match(Q_overwrite, 
                                   "y" ~ "yes", 
                                   "yes" ~ "yes", 
                                   "t" ~ "yes", 
                                   "true" ~ "yes", 
                                   "n" ~ "no", 
                                   "no" ~ "no", 
                                   "f" ~ "no", 
                                   "false" ~ "no")
         
         if(is.na(Q_overwrite)){
            stop(SimcypConsultancy:::wrapn("We did not understand your input. Please try again."), 
                 call. = FALSE)
         }
         
         if(Q_overwrite == "no"){
            stop(paste0(SimcypConsultancy:::wrapn("Some of the files you're attempting to copy already exist in the destination folder, and you asked us not to overwrite them. We cannot proceed until you remove or rename the following file:"), 
                        str_c(FilesAlreadyInFolder, collapse = "\n")), 
                 call. = FALSE)
            
         }
      }
   }
   
   
   ## Checking for files that do not exist -----------------------------------
   
   FilesToCopy_wksz <- FilesToCopy %>% 
      select(`File name`, matches("wksz")) %>% 
      filter(complete.cases(FullPath.wksz) & 
                complete.cases(`File name`)) %>% 
      filter(`File name` != "") %>% 
      mutate(Exists = file.exists(FullPath.wksz))
   
   if(any(FilesToCopy_wksz$Exists == FALSE)){
      warning(paste0(SimcypConsultancy:::wrapn("The following workspaces could not be found and thus will not be copied:"), 
                     str_c(FilesToCopy_wksz$FullPath.wksz[
                        FilesToCopy_wksz$Exists == FALSE], 
                        collapse = "\n")), 
              call. = FALSE)
   }
   
   FilesToCopy_xlsx <- FilesToCopy %>% 
      select(`File name`, matches("xlsx")) %>% 
      filter(complete.cases(FullPath.xlsx) & 
                complete.cases(`File name`)) %>% 
      filter(`File name` != "") %>% 
      mutate(Exists = file.exists(FullPath.xlsx))
   
   if(any(FilesToCopy_xlsx$Exists == FALSE)){
      warning(paste0(SimcypConsultancy:::wrapn("The following Excel outputs could not be found and thus will not be copied:"), 
                     str_c(FilesToCopy_xlsx$FullPath.xlsx[
                        FilesToCopy_xlsx$Exists == FALSE], 
                        collapse = "\n")), 
              call. = FALSE)
   }
   
   FilesToCopy_xml <- FilesToCopy %>% 
      select(`File name`, matches("xml")) %>% 
      filter(complete.cases(FullPath.xml) & 
                complete.cases(`File name`)) %>% 
      filter(`File name` != "") %>% 
      mutate(Exists = file.exists(FullPath.xml))
   
   if(any(FilesToCopy_xml$Exists == FALSE)){
      warning(paste0(SimcypConsultancy:::wrapn("The following XML observed-data-overlay files could not be found and thus will not be copied:"), 
                     str_c(FilesToCopy_xml$FullPath.xml[
                        FilesToCopy_xml$Exists == FALSE], 
                        collapse = "\n")), 
              call. = FALSE)
   }
   
   
   ## Copying files ----------------------------------------------------------
   
   message("Copying workspaces")
   
   invisible(
      file.copy(from = FilesToCopy_wksz$FullPath.wksz, 
                to = FilesToCopy_wksz$NewPath.wksz, 
                overwrite = TRUE))
   
   message("Copying Excel outputs")
   
   invisible(
      file.copy(from = FilesToCopy_xlsx$FullPath.xlsx, 
                to = FilesToCopy_xlsx$NewPath.xlsx))
   
   message("Copying any XML observed-data-overlay files")
   
   invisible(
      file.copy(from = FilesToCopy_xml$FullPath.xml,
                to = FilesToCopy_xml$NewPath.xml))
   
}




# Making final directory -----------------------------------------------------

#' Make a simulation directory of just the files in a project folder and nothing
#' else
#'
#' @description \code{make_external_simulation_directory} is a wrapper for
#'   \code{\link{make_simulation_directory}} that sets all the arguments up
#'   correctly when you have all your simulations and XML observed-data-overlay
#'   files in a single folder and you don't need to search for any observed data
#'   files or anything fancy. Instead, this is the scenario where you just want
#'   a simulation directory of \emph{exactly} what's in the project folder.
#'
#' @param project_folder location of the project folder to search. Please note
#'   that R requires forward slashes ("/") in file paths rather than the back
#'   slashes Microsoft uses. If left as NA (default), we'll assume your current
#'   working directory is the top level of your project folder.
#' @param save_table optionally specify an Excel file name for saving your
#'   simulation directory.
#' @param overwrite Should we overwrite if your Excel file already exists and
#'   already has a tab named "Simulation directory"? Options are "yes" to always
#'   overwrite, "no" to never overwrite, or "ask" (default), which means that we
#'   will ask you whether to overwrite and give you a chance to supply a
#' @param draft_simulation_directory the output from running either
#'   \code{\link{make_simulation_directory}} or \code{\link{find_report_file_locations}},
#'   either of which will find all of the simulation files (both workspaces and
#'   Excel outputs as well as any observed XML overlay files) in a project
#'   folder. This MUST include the following columns:
#'
#'   \itemize{\item{"File name" with the simulation file names}
#'
#'   \item{"File type" with the file extensions}
#'
#'   \item{"XML file used" with the XML observed-data overlay file}
#'
#'   \item{"Table/Figure" with the table and figure that simulation appears in}}
#'
#'   If you have only run \code{\link{make_simulation_directory}}, this will not
#'   include the figures and tables unless you have added those manually.
#'
#' @returns a simulation directory as a data.frame; optionally saves that
#'   data.frame to Excel
#' @export
#'
#' @examples
#' make_external_simulation_directory(
#'    project_folder = "C:/Users/Buffy/Project ABC/Modelling",
#'    draft_sim_directory = DraftSimDirectory,
#'    save_table = "Simulation directory for project ABC.xlsx")

make_external_simulation_directory <- function(
      project_folder, 
      draft_simulation_directory, 
      save_table = NA, 
      overwrite = "ask"){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   
   if("data.frame" %in% class(draft_simulation_directory) == FALSE ||
      all(c("File name", "Folder") %in% 
          names(draft_simulation_directory)) == FALSE){
      stop(SimcypConsultancy:::wrapn("You do not appear to have provided output from running either 'make_simulation_directory' or 'find_report_file_locations' for the argument 'draft_simulation_directory', which is what we need to proceed. Please check your input and try again."), 
           call. = FALSE)
   }
   
   if("character" %in% class(draft_simulation_directory)){
      Ext <- str_extract(draft_simulation_directory, "\\.*$")
      
      if(is.na(Ext)){
         draft_simulation_directory <- paste0(draft_simulation_directory, ".xlsx")
      } else {
         if(Ext %in% c(".xlsx", ".csv") == FALSE){
            stop(SimcypConsultancy:::wrapn("The input for the argument 'draft_simulation_directory' must be a data.frame, an Excel file, or a .csv file, and this is something else. We cannot proceed."), 
                 call. = FALSE)
         }
      }
      
      if(Ext == ".xlsx"){
         draft_simulation_directory <-
            readxl::read_xlsx(draft_simulation_directory, 
                              sheet = "Simulation directory")
      } else {
         draft_simulation_directory <- read.csv(draft_simulation_directory)
      }
      
   } else if("data.frame" %in% class(draft_simulation_directory) == FALSE){
      stop(SimcypConsultancy:::wrapn("The input for the argument 'draft_simulation_directory' must be a data.frame, an Excel file, or a .csv file, and this is something else. We cannot proceed."), 
           call. = FALSE)
   }
   
   draft_simulation_directory <- draft_simulation_directory %>% as_tibble()
   names(draft_simulation_directory) <- tolower(names(draft_simulation_directory))
   
   DraftSimDirNames <- 
      tibble(Orig = names(draft_simulation_directory)) %>% 
      mutate(Rev = case_when(
         str_detect(Orig, "file|workspace|sim") & 
            !str_detect(Orig, "type|xml") ~ "File name", 
         str_detect(Orig, "file|workspace|sim") & 
            str_detect(Orig, "type") ~ "File type", 
         str_detect(Orig, "xml") ~ "XML file used",
         str_detect(Orig, "table|fig") ~ "Table/Figure", 
         str_detect(Orig, "comments|notes") ~ "Comments")) %>% 
      filter(complete.cases(Rev))
   
   draft_simulation_directory <- draft_simulation_directory[, DraftSimDirNames$Orig]
   names(draft_simulation_directory) <- DraftSimDirNames$Rev
   
   if(all(c("File name", "File type", "XML file used", "Table/Figure") %in% 
          names(draft_simulation_directory)) == FALSE){
      stop(SimcypConsultancy:::wrapn("We need the input for the argument 'draft_simulation_directory' to be formatted like the output from running 'find_report_file_locations', and you have provided something else. We cannot proceed."), 
           call. = FALSE)
   }
   
   draft_simulation_directory <- draft_simulation_directory %>% 
      select(`File name`, `File type`, `XML file used`, `Table/Figure`, Comments)
   
   
   # Main body of function ----------------------------------------------------
   
   Out <-  
      make_simulation_directory(
         project_folder = project_folder, 
         sim_data_files = c(paste0(draft_simulation_directory$`File name`, ".xlsx"), 
                            paste0(draft_simulation_directory$`File name`, ".wksz")), 
         simfile_path_option = "relative", 
         existing_exp_details = NA, 
         search_workspaces_for_obsfile = FALSE, 
         include_possible_obsfiles = FALSE, 
         obsfile_path_option = "basename", 
         figure_and_table_assignment = draft_simulation_directory, 
         report_progress = "no", 
         save_table = NA)
   
   Out <- Out %>% 
      filter(complete.cases(`File name`) & 
                `File name` %in% draft_simulation_directory$`File name` &
                `File name` != "") %>% 
      select(-Folder) %>% 
      left_join(draft_simulation_directory %>% 
                   select(`File name`, `XML file used`), 
                by = "File name") %>% 
      mutate(`XML file used` = basename(`XML file used`)) %>% 
      select(`File name`, `File type`, `XML file used`, `Table/Figure`, 
             Comments) %>% 
      unique()
   
   
   # Saving -----------------------------------------------------------------
   
   if(complete.cases(save_table)){
      
      ColWidths <- guess_col_widths(DF = Out, wrap = FALSE)
      ColWidths[ColWidths > 85] <- 85
      
      save_table_to_Excel(table = Out, 
                          save_table = save_table, 
                          overwrite = overwrite, 
                          output_tab_name = "Simulation directory", 
                          center_top_row = FALSE, 
                          column_widths = ColWidths, 
                          wrap_text = FALSE) 
      
   }
   
   return(Out)
   
}



