#' Extract details about the experimental design
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
#' @param sim_data_file name of the Excel file containing the simulator output,
#'   in quotes
#'
#' @returns a named list
#' @export
#'
extractExpDetails_VBE <- function(sim_data_file){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   sim_data_file <- paste0(sub("\\.wksz$|\\.cvbe$|\\.dscw$|\\.xlsx$", "", sim_data_file), ".xlsx")
   
   # Checking that the file is, indeed, a simulator output file.
   SheetNames <- tryCatch(readxl::excel_sheets(sim_data_file),
                          error = openxlsx::getSheetNames(sim_data_file))
   
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
   
   Out <- list()
   
   # Noting all sheet names. This saves time for later data extraction and
   # also helps with debugging and coding in general. 
   Out[["SheetNames"]] <- str_c(paste0("`", SheetNames, "`"), collapse = " ")
   
   
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
         Out[[i]] <- pullValue(i, tab = Input)
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
   
   Out$Treatments <- list()
   
   for(tx in TxSheets){
      
      ### Info on Input tab ------------------------------------------------
      StartRow <- which(Input$...1 == tx)
      EndRow <- which(is.na(Input$...1))
      EndRow <- EndRow[EndRow > StartRow][1] - 1
      if(is.na(EndRow)){EndRow <- nrow(Input)}
      
      for(i in TxInfo$Deet){
         Out$Treatments[[tx]][[i]] <- pullValue(i, tab = Input[StartRow:EndRow, ])
      }
      
      rm(StartRow, EndRow)
      
      ### Info on treatment tab ----------------------------------------------
      
      InputInfo <- extractInputTab(deets = MyInputDeets,
                                   sim_data_file = sim_data_file, 
                                   sheet = tx, 
                                   CustomDosing = CustomDosing)
      
      Out$Treatments[[tx]] <- c(Out$Treatments[[tx]], InputInfo)
      
   }
   
   
   ## Adding general sim info -------------------------------------------------
   if("StartDayTime_sub" %in% names(Out) &&
      complete.cases(Out$StartDayTime_sub)){
      Out[["StartHr_sub"]] <- difftime_sim(time1 = Out$SimStartDayTime,
                                           time2 = Out$StartDayTime_sub)
   }
   
   # Noting when workspace, if there is a matching one, was last changed.
   WorkspaceFile <- sub("xlsx", "cvbe", sim_data_file)
   # Removing the file timestamp if there was one b/c that won't be part of the
   # workspace file name.
   WorkspaceFile <- sub(" - [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2}", 
                        "", WorkspaceFile)
   
   Out$Workspace_TimeLastModified <- 
      ifelse(file.exists(WorkspaceFile), 
             as.character(file.info(WorkspaceFile)$mtime), NA)
   
   # Noting when this was run. 
   Out$expDetails_TimeStamp <- Sys.time()
   
   
   ## Returning ---------------------------------------------------------------
   
   # Splitting this up into main details -- a data.frame -- and then,
   # separately, whatever items need to be lists, e.g., custom dosing regimens
   # and dissolution profiles. 
   suppressMessages(
      Main <- as_tibble(Out[which(sapply(Out, length) == 1)])
   )
   
   # Making absolutely sure that File included in Main. When we run
   # harmonize_details, it will add it to the other items whenever there is at
   # least 1 row, but we need it in Main to do that.
   Main$File <- sim_data_file 
   
   Out <- list(MainDetails = Main, 
               DissolutionProfiles = DissoProfs,
               ReleaseProfiles = ReleaseProfs, 
               ConcDependent_fup = CDfupProfs, 
               ConcDependent_BP = CDBPProfs, 
               pH_dependent_solubility = pHSol, 
               VBE = Out[["Treatments"]])
   
   Out <- harmonize_details(Out)
   
   
   # Returning --------------------------------------------------------------
   
   ItemsToCheck <- setdiff(names(Out), "VBE")
   ItemsToCheck <- unlist(lapply(Out[ItemsToCheck], is.null)) == FALSE
   
   for(j in ItemsToCheck){
      Out[[j]] <- Out[[j]] %>% 
         mutate(File = sim_data_file) %>% 
         select(File, everything())
   }
   
   return(Out)
   
}

