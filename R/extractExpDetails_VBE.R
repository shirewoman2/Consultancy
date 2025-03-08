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
   
   Out$Treatments <- list()
   
   for(tx in TxSheets){
      
      ### Info on Input tab ------------------------------------------------
      StartRow <- which(Input$...1 == tx)
      EndRow <- which(is.na(Input$...1))
      EndRow <- EndRow[EndRow > StartRow][1] - 1
      if(is.na(EndRow)){EndRow <- nrow(Input)}
      
      for(i in TxInfo$Deet){
         Out[[i]] <- pullValue(i, tab = Input[StartRow:EndRow, ])
      }
      
      
      Out$Treatments[[tx]]
      
      
      
   }

   
   
}

