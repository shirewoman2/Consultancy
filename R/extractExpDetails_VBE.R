#' Extract details about the experimental design of a VBE simulation - UNDER
#' CONSTRUCTION
#'
#' @description \code{extractExpDetails} looks up experimental design details
#'   from a Simcyp Simulator output file.
#'
#'   For detailed instructions and examples, please see the SharePoint file
#'   "Simcyp PBPKConsult R Files - Simcyp PBPKConsult R Files/SimcypConsultancy
#'   function examples and instructions/Checking simulation experimental
#'   details/Checking-simulation-experimental-details.docx". (Sorry, we are
#'   unable to include a link to it here.)
#'
#' @param sim_data_files a character vector of simulator output files, each in
#'   quotes and encapsulated with \code{c(...)}, NA to extract experimental
#'   details for \emph{all} the Excel files in the current folder, or
#'   "recursive" to extract experimental details for \emph{all} the Excel files
#'   in the current folder and \emph{all} subfolders. Example of acceptable
#'   input: \code{sim_data_files = c("sim1.xlsx", "sim2.xlsx")}. If some of your
#'   Excel files are not regular simulator output, e.g. they are sensitivity
#'   analyses or a file where you were doing some calculations, those files will
#'   be skipped. \strong{A note:} There are just a few items that we will
#'   attempt to extract from the matching workspace file (actually, no, it
#'   won't. This is placeholder text for when we do implement that; at present,
#'   it will only look at Excel files); for that information, we will look for a
#'   workspace file that is named \emph{identically} to the Excel file except
#'   for the file extension. It will ignore the date/time stamp that the
#'   autorunner adds as long as that stamp is in a format like this: "myfile -
#'   2023-10-31 07-23-15.xlsx".
#' @param existing_exp_details (optional) a data.frame that contains previously
#'   extracted experimental details. If this object \emph{does} exist, it should
#'   NOT be in quotes, e.g. \code{existing_exp_details = MyDeets}. Because we
#'   can see scenarios where you might want to extract some experimental details
#'   and then run more simulations for comparisons, this function will
#'   \emph{add} data to that data.frame. It will \emph{not} overwrite existing
#'   data unless \code{overwrite} is set to TRUE.
#' @param overwrite TRUE or FALSE (default) on whether to re-extract the
#'   experimental details from output files that are already included in
#'   \code{existing_exp_details}. Since pulling data from Excel files is slow,
#'   by default, this will \emph{not} overwrite existing data and instead will
#'   only add data from any Excel files that aren't already included. A
#'   situation where you might want to set this to TRUE would be when you have
#'   changed input parameters for simulations and re-run them OR when you have
#'   extracted only some of the possible experimental details and you now would
#'   like more experimental details from each simulator output file.
#'
#' @return Returns a named list of simulation experimental details for simulator
#'   files
#' @export
#' 
extractExpDetails_VBE <- function(sim_data_files, 
                                  existing_exp_details = NA, 
                                  overwrite = FALSE,
                                  sheet_names = NA,
                                  ...){
   
   # Error catching ---------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
      
   # Checking whether they've supplied extractExpDetails args instead of
   # extractExpDetails_mult args
   if("sim_data_file" %in% names(match.call()) &
      "sim_data_files" %in% names(match.call()) == FALSE){
      sim_data_files <- sys.call()$sim_data_file
   }
   
   # If user did not supply files, then extract all the files in the current
   # folder that end in "xlsx" or in all subfolders if they wanted it to be
   # recursive.
   if(length(sim_data_files) == 1 &&
      (is.na(sim_data_files) | sim_data_files == "recursive")){
      sim_data_files <- list.files(pattern = "\\.xlsx$",
                                   recursive = (complete.cases(sim_data_files) &&
                                                   sim_data_files == "recursive"))
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   }
   
   # If they didn't include ".xlsx" at the end of the file name, assume
   # they want the more-developed option and add "xlsx".
   MissingExt <- which(str_detect(sim_data_files, "\\.xlsx$") == FALSE)
   sim_data_files[MissingExt] <- 
      sub("\\.wksz$|\\.dscw$", ".xlsx", sim_data_files[MissingExt])
   
   # Making sure that all the files exist before attempting to pull data
   if(any(file.exists(sim_data_files) == FALSE)){
      MissingSimFiles <- sim_data_files[
         which(file.exists(sim_data_files) == FALSE)]
      
      # This can happen if the file name is too long.
      TooLong <- intersect(MissingSimFiles, list.files(pattern = "xlsx"))
      
      MissingSimFiles <- setdiff(MissingSimFiles, TooLong)
      
      if(length(TooLong) > 0){
         warning(wrapn(paste0("The file(s) ", 
                              str_comma(paste0("`", TooLong, "`")), 
                              " has/have a file path that is too long, so we cannot extract any information about the simulation.")), 
                 call. = FALSE)
         sim_data_files <- setdiff(sim_data_files, TooLong)   
      }
      
      if(length(MissingSimFiles) > 0){
         # Removing files that do not exist. 
         warning(paste0("The following simulation files are not present and will be ignored:\n", 
                        str_c(paste0("  ", MissingSimFiles), collapse = "\n")), 
                 call. = FALSE)
         sim_data_files <- setdiff(sim_data_files, MissingSimFiles)
      }
   }
   
   # Make it so that, if they supply NA, NULL, or "none" for
   # existing_exp_details, all of those will work. Note to coders: It was REALLY
   # HARD to get this to work with just the perfect magical combination of
   # exists and suppressWarnings, etc.
   
   # If user supplied an unquoted object, this checks whether that object
   # exists. However, if they supplied NA or NULL, this throws an error. 
   Recode_existing_exp_details <- suppressWarnings(
      try(exists(deparse(substitute(existing_exp_details))) == FALSE, silent = TRUE))
   
   # If they got an error, then the class of Recode_X will be "try-error", and
   # then we want Recode_X to be TRUE.
   if(suppressWarnings("try-error" %in% class(Recode_existing_exp_details))){
      Recode_existing_exp_details <- TRUE
   }
   
   if(Recode_existing_exp_details || length(existing_exp_details) == 0){
      existing_exp_details <- "none"
   }
   
   AnyExistingDeets <- exists(deparse(substitute(existing_exp_details)))
   
   if(AnyExistingDeets){
      
      existing_exp_details <- harmonize_details(existing_exp_details)
      
      if(overwrite == FALSE){
         sim_data_files_topull <- 
            unique(setdiff(sim_data_files, existing_exp_details$MainDetails$File))
      } else {
         sim_data_files_topull <- unique(sim_data_files)
         existing_exp_details <- filter_sims(existing_exp_details, 
                                             which_sims = sim_data_files_topull, 
                                             include_or_omit = "omit")
      }
   } else {
      sim_data_files_topull <- unique(sim_data_files)
   }
   
   
   # STARTING LOOP THROUGH FILES HERE -----------------------------------------
   
   Out <- list()
   
   for(sim_data_file in sim_data_files_topull){
      
      # Checking that the file is, indeed, a simulator output file.
      if(all(is.na(sheet_names))){
         SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                                error = openxlsx::getSheetNames(sim_data_file))
      } else {
         SheetNames <- sheet_names
      }
      
      if(any(str_detect(SheetNames, "Treatment [0-9]")) == FALSE){
         # Using "warning" instead of "stop" here b/c I want this to be able to
         # pass through to extractExpDetails_mult and just skip any files that
         # aren't simulator output.
         warning(wrapn(paste0("The file '", sim_data_file,
                              "' does not appear to be a Simcyp Simulator output Excel file for a VBE simulation. We cannot return any information for this file.")), 
                 call. = FALSE)
         return(list()) 
      } 
      
      # Checking for file name issues
      CheckFileNames <- check_file_name(sim_data_file)
      BadFileNames <- CheckFileNames[!CheckFileNames == "File name meets naming standards."]
      if(length(BadFileNames)> 0){
         BadFileNames <- paste0(names(BadFileNames), ": ", BadFileNames)
         warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                        str_c(paste0("     ", BadFileNames), collapse = "\n"),
                        "\n"), 
                 call. = FALSE)
      }
      
      
      # Main body of function ----------------------------------------------------
      
      VBEDeets <- AllExpDetails %>% filter(DataSource == "VBE Input") %>%
         rename(Deet = Detail) %>% arrange(Deet)
      # NB: At this point, NOT giving the user the option of asking for anything
      # but ALL the VBE info.
      
      # sub function for finding correct cell
      pullValue <- function(deet, tab){
         
         # Setting up regex to search
         ToDetect <- VBEDeets %>% 
            filter(Deet == deet) %>% pull(Regex_row)
         NameCol <- 1
         Row <- which(str_detect(tab[, NameCol] %>% pull(), ToDetect)) +
            (VBEDeets %>% filter(Deet == deet) %>% pull(OffsetRows))
         
         if(length(Row) == 0){
            Val <- NA
         } else {
            
            Val <- tab[Row,
                       VBEDeets$ValueCol[
                          which(VBEDeets$Deet == deet)]] %>% pull()
         }
         
         suppressWarnings(
            Val <- switch(VBEDeets$Class[VBEDeets$Deet == deet], 
                          "character" = as.character(Val),
                          "numeric" = as.numeric(Val))
         )
         
         if(length(Val) > 1){
            Val <- str_comma(Val)
         }
         
         # Tidying up some specific idiosyncracies of simulator output
         Val <- ifelse(length(Val) == 0 || 
                          (complete.cases(Val) & Val == "n/a"), NA, Val)
         Val <- ifelse(str_detect(deet, "^Unit"),
                       str_trim(gsub("\\(unbound\\)|\\(blood\\)|\\(unbound blood\\)|Dose \\(|\\)|CMax \\(|TMax \\(|AUC \\(|CL \\(Dose/AUC\\)\\(|\\(blood\\)",
                                     "", Val)), Val)
         
         return(Val)
      }
      
      MainDetails <- list()
      
      # Noting all sheet names. This saves time for later data extraction and
      # also helps with debugging and coding in general. 
      MainDetails[["SheetNames"]] <- str_c(paste0("`", SheetNames, "`"),
                                           collapse = " ")
      
      
      ## Trial-design info -------------------------------------------------------
      
      TDinfo <- VBEDeets %>% filter(ColsChangeWithCmpd == FALSE)
      
      Input <- suppressMessages(tryCatch(
         readxl::read_excel(path = sim_data_file, sheet = "Input",
                            col_names = FALSE),
         error = openxlsx::read.xlsx(sim_data_file, sheet = "Input",
                                     colNames = FALSE)))
      # If openxlsx read the file, the names are different. Fixing.
      if(names(Input)[1] == "X1"){
         names(Input) <- paste0("...", 1:ncol(Input))
      }
      
      if(nrow(TDinfo) > 0){ # <- This should always be T, but leaving this in in case later we want to allow user to request only specific details. 
         for(i in TDinfo$Deet){
            MainDetails[[i]] <- pullValue(i, tab = Input)
         }
      }
      
      
      ## Treatment info -------------------------------------------------------
      
      # There should be a "Treatment X" tab for each treatment, so the tab names
      # should match the treatments listed in the Input tab.
      TxSheets <- SheetNames[str_detect(SheetNames, "^Treatment [0-9]")]
      TxInfo <- VBEDeets %>% filter(ColsChangeWithCmpd == TRUE)
      
      # Reading sheets
      TxTabs <- list()
      for(tx in TxSheets){
         TxTabs[[tx]] <- 
            suppressMessages(tryCatch(
               readxl::read_excel(path = sim_data_file, sheet = tx,
                                  col_names = FALSE),
               error = openxlsx::read.xlsx(sim_data_file, sheet = tx,
                                           colNames = FALSE)))
         
         # If openxlsx read the file, the names are different. Fixing.
         if(names(TxTabs[[tx]])[1] == "X1"){
            names(TxTabs[[tx]]) <- paste0("...", 1:ncol(TxTabs[[tx]]))
         }
      }
      
      MainDetails <- as_tibble(MainDetails) %>% mutate(File = sim_data_file)
      Out[[sim_data_file]] <- list()
      
      for(i in ExpDetailListItems){
         Out[[sim_data_file]][[i]] <- list()
      }
      
      Treatments <- list()
      
      for(tx in TxSheets){
         
         ### Info on Input tab ------------------------------------------------
         StartRow <- which(Input$...1 == tx)
         EndRow <- which(is.na(Input$...1))
         EndRow <- EndRow[EndRow > StartRow][1] - 1
         if(is.na(EndRow)){EndRow <- nrow(Input)}
         
         for(i in TxInfo$Deet){
            Treatments[[tx]][[i]] <- pullValue(i, tab = Input[StartRow:EndRow, ])
         }
         
         Treatments[[tx]]$Treatment <- tx
         
         rm(StartRow, EndRow)
         
         ### Info on treatment tab ----------------------------------------------
         
         InputInfo <- extractInputTab(deets = "all",
                                      sim_data_file = sim_data_file, 
                                      sheet = tx, 
                                      CustomDosing = FALSE)
         
         for(i in setdiff(ExpDetailListItems, "MainDetails")){
            if("data.frame" %in% class(InputInfo[[i]]) &&
               nrow(InputInfo[[i]]) > 0){
               InputInfo[[i]] <- InputInfo[[i]] %>% 
                  mutate(File = sim_data_file, 
                         Treatment = tx) %>% 
                  select(File, Treatment, everything())
               
               Out[[sim_data_file]][[i]] <- 
                  bind_rows(Out[[sim_data_file]][[i]], InputInfo[[i]])
            }
         }
         
         suppressMessages(
            Treatments[[tx]] <- 
               left_join(
                  as_tibble(Treatments[[tx]]), 
                  as_tibble(InputInfo[which(map(InputInfo, length) %>% 
                                               unlist() == 1)]) %>% 
                     mutate(Treatment = tx)) %>% 
               mutate(File = sim_data_file)
         )
         
         rm(InputInfo)
         
      }
      
      Treatments <- bind_rows(Treatments)
      suppressMessages(
         MainDetails <- MainDetails %>% 
            left_join(Treatments))
      
      
      ## Adding general sim info -------------------------------------------------
      
      # Time units should always be h for VBE sims; I don't believe you can
      # change that.
      MainDetails[["Units_tmax"]] <- "h"
      
      if("StartDayTime_sub" %in% names(MainDetails) &&
         any(complete.cases(MainDetails$StartDayTime_sub))){
         MainDetails[["StartHr_sub"]] <-
            difftime_sim(time1 = MainDetails$SimStartDayTime,
                         time2 = MainDetails$StartDayTime_sub)
      }
      
      # Noting when workspace, if there is a matching one, was last changed.
      WorkspaceFile <- sub("xlsx", "cvbe", sim_data_file)
      # Removing the file timestamp if there was one b/c that won't be part of the
      # workspace file name.
      WorkspaceFile <- sub(" - [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2}", 
                           "", WorkspaceFile)
      
      MainDetails$Workspace_date_last_saved <- 
         ifelse(file.exists(WorkspaceFile), 
                file.info(WorkspaceFile)$mtime %>% as.Date() %>% as.character(),
                NA)
      
      # Noting when this was run. 
      MainDetails$expDetails_TimeStamp <- Sys.time()
      
      ## Returning ---------------------------------------------------------------
      
      Out[[sim_data_file]]$MainDetails <- MainDetails
      
      Out[[sim_data_file]] <-
         harmonize_details(Out[[sim_data_file]][
            which(map(Out[[sim_data_file]], length) %>% unlist() > 1)])
      
      rm(MainDetails, Treatments, TDinfo, TxInfo, TxTabs, Input)
      
      # ENDING LOOP THROUGH FILES HERE ----------------------------------------
   }
   
   MyDeets <- Out
   Out <- c(list(existing_exp_details), MyDeets)
   names(Out)[1] <- ifelse(names(Out)[1] == "", "existing", names(Out)[1])
   # Retaining only files that were simulations.
   Out <- Out[which(sapply(Out, \(x) all(is.null(names(x))) == FALSE))]
   
   if(length(Out) == 0){
      warning("No Simcyp Simulator results could be found.\n", 
              call. = FALSE)
      return()
   }
   
   Out <- Out[which(sapply(Out, FUN = function(x) all(x != "none")))]
   
   # If MyDeets was length 0, which will happen if there are no new simulations
   # to extract, then skip the class check b/c it won't work.
   if(length(MyDeets) > 0){
      # Tried EVERYTHING I COULD THINK OF to avoid doing this next bit as multiple
      # loops, but NOTHING worked.
      Classes <- list()
      
      for(i in names(Out)){
         TEMP <- sapply(Out[[i]][["MainDetails"]], class)
         Classes[[i]] <- data.frame(Detail = names(TEMP),
                                    Class = as.character(TEMP),
                                    File = i)
         rm(TEMP)
      }
      
      Classes <- bind_rows(Classes) %>% 
         mutate(Class = ifelse(Class == "logical", NA, Class)) %>% 
         group_by(Detail) %>% 
         summarize(Problem = length(sort(unique(Class))) > 1) %>% 
         filter(Problem)
      
      if(nrow(Classes) > 0){
         for(i in names(Out)){
            Out[[i]][["MainDetails"]] <- Out[[i]][["MainDetails"]] %>% 
               mutate(across(.cols = any_of(Classes$Detail), 
                             .fns = as.character))
         }
      }
   }
   
   Out <- Out %>% list_transpose() %>% 
      map(.f = bind_rows) %>% 
      map(.f = remove_rownames)
   
   if(length(Out) == 0 | nrow(Out$MainDetails) == 0){
      stop("It was not possible to extract any simulation experimental details.")
   }
   
   # Sorting to help organize output
   Out$MainDetails <- Out$MainDetails %>% select(File, everything())
   
   
   # Returning --------------------------------------------------------------
   
   return(Out)
   
}

