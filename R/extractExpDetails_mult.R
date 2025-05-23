#' Extract simulation experimental details for multiple files at once
#'
#' \code{extractExpDetails_mult} takes a character vector of Simcyp Simulator
#' output files -- or all the Excel files in the current directory if no files
#' are specified -- and collects experimental details for the simulations into a
#' single table. It optionally saves that table to a csv or Excel file. For
#' detailed instructions and examples, please see the SharePoint file "Simcyp
#' PBPKConsult R Files - Simcyp PBPKConsult R Files/SimcypConsultancy function
#' examples and instructions/Checking simulation experimental
#' details/Checking-simulation-experimental-details.docx". (Sorry, we are unable
#' to include a link to it here.)
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
#'   attempt to extract from the matching workspace file; for that information,
#'   we will look for a workspace file that is named \emph{identically} to the
#'   Excel file except for the file extension. It will ignore the date/time
#'   stamp that the autorunner adds as long as that stamp is in a format like
#'   this: "myfile - 2023-10-31 07-23-15.xlsx".
#' @param exp_details experimental details you want to extract from the
#'   simulator output files using the function \code{\link{extractExpDetails}}.
#'   Options are \describe{
#'
#'   \item{"Summary tab"}{Extract details only from the "Summary tab"}
#'
#'   \item{"Input Sheet"}{Extract details only from the "Input Sheet" tab}
#'
#'   \item{"population tab"}{Extract details about the population used (data
#'   come from the tab with the same name as the population simulated)}
#'
#'   \item{"Simcyp inputs"}{Extract all the details that you normally fill out
#'   on the "Simcyp inputs (and QC)" tab of a compound data sheet plus trial
#'   design information}
#'
#'   \item{"workspace"}{Extract a limited set of details directly
#'   from the Simcyp Simulator workspace files. The set of possible details may
#'   be viewed by entering \code{view(AllWorkspaceDetails)} in the console. This
#'   \emph{only} works when each workspace file name perfectly matches its
#'   corresponding Excel results file name and is located in the same folder.
#'   Otherwise, this step in the data extraction will be skipped.}
#'
#'   \item{"all"}{Extract all possible parameters (default). This is the slowest
#'   option in terms of processing time because it must read multiple Excel
#'   tabs.}}
#'
#'   \strong{Note:} While information about custom dosing regimens \emph{can} be
#'   extracted by the function \code{\link{extractExpDetails}}, that information
#'   cannot easily be made to fit with the rest of the output for
#'   \code{extractExpDetails_mult}. That's because each simulator file and
#'   compound with a custom-dosing regimen will have its own data.frame with the
#'   time, time units, dose number, dose amount, dose units, and dose route. For
#'   that reason, custom-dosing information will largely be ignored here.
#'
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
#'
#' @export
#'
#' @examples
#'
#' extractExpDetails_mult(
#'     sim_data_files =
#'         c("Example simulator output - SD MDZ + MD RTV.xlsx",
#'           "Example simulator output - MDZ + metabolites.xlsx",
#'           "Example simulator output - met1 met2 sec met1 inhib1.xlsx",
#'           "Example simulator output - met1 met2 sec met1.xlsx"),
#'     exp_details = "all")
#'  
extractExpDetails_mult <- function(sim_data_files = NA, 
                                   exp_details = "all", 
                                   existing_exp_details = NA, 
                                   overwrite = FALSE,
                                   ...){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), "\n     library(tidyverse)\n\nand then try again."), call. = FALSE)
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
      sim_data_files <- list.files(pattern = "\\.xlsx$|\\.db$",
                                   recursive = (complete.cases(sim_data_files) &&
                                                   sim_data_files == "recursive"))
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   }
   
   # If they didn't include ".xlsx" or ".db" at the end of the file name, assume
   # they want the more-developed option and add "xlsx".
   MissingExt <- which(str_detect(sim_data_files, "\\.xlsx$|\\.db$") == FALSE)
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
   
   
   # Main body of function ---------------------------------------------------
   
   # print(quo_name(enquo(existing_exp_details))) # for bug fixing
   
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
   
   MyDeets <- list()
   
   for(i in sim_data_files_topull){ 
      message(paste("Extracting simulation experimental details from file =", i))
      
      if(str_detect(i, "\\.db$")){
         MyDeets[[i]] <- extractExpDetails_DB(sim_data_file = i) 
      } else {
         
         # Getting sheet names 1st b/c that will determine whether to use _vbe version
         SheetNames <- tryCatch(readxl::excel_sheets(i),
                                error = openxlsx::getSheetNames(i))
         
         if(any(str_detect(SheetNames, "Treatment [0-9]"))){
            MyDeets[[i]] <- extractExpDetails_VBE(
               sim_data_file = i, 
               exp_details = exp_details, 
               sheet_names = SheetNames) 
            
            if(length(MyDeets[[i]]) == 0){
               MyDeets[[i]] <- NULL
               next
            }
            
            # Noting that this was a VBE simulation
            MyDeets[[i]]$MainDetails$VBEsim <- TRUE
            
         } else {
            MyDeets[[i]] <- extractExpDetails(
               sim_data_file = i, 
               exp_details = exp_details, 
               sheet_names = SheetNames) 
            
            if(length(MyDeets[[i]]) == 0){
               MyDeets[[i]] <- NULL
               next
            }
            
            # Noting that this was NOT a VBE simulation
            MyDeets[[i]]$MainDetails$VBEsim <- FALSE
         }
      }
   }
   
   if(length(MyDeets) > 0){
      MyDeets <- MyDeets[which(sapply(MyDeets, \(x) length(x) > 0))]
   } else if(length(existing_exp_details) > 0){
      return(existing_exp_details)
   }
   
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
   
   # Having a TON of trouble with list_transpose for reasons I cannot fathom,
   # but it's making RStudio crash. Trying a different approach. 
   
   # Out <- Out %>% list_transpose() %>% 
   #    map(.f = bind_rows) %>% 
   #    map(.f = remove_rownames)
   
   TEMP <- list()
   for(ii in ExpDetailListItems){
      for(ff in names(Out)){
         TEMP[[ii]] <- bind_rows(map(Out, ii))
      }
   }
   
   Out <- TEMP
   
   if(length(Out) == 0 | nrow(Out$MainDetails) == 0){
      stop("It was not possible to extract any simulation experimental details.")
   }
   
   # Sorting to help organize output
   Out$MainDetails <- Out$MainDetails %>% select(File, everything())
   
   return(Out)
}


