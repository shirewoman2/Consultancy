#' Extract experimental details for multiple files at once
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
#'   tabs.}
#'
#'   \item{a string of the specific parameters you want, each in quotes and
#'   encapsulated with \code{c(...)},}{For a complete list, type
#'   \code{data(ExpDetailDefinitions); view(ExpDetailDefinitions)} into the
#'   console. Parameters are reported with a suffix depending on which compound
#'   they pertain to: "_sub" for the substrate, "_met1" for the primary
#'   metabolite, "_met2" for the second primary metabolite, "_secmet" for the
#'   secondary metabolite, "_inhib" for the 1st inhibitor or inducer listed,
#'   "_inhib2" for the 2nd inhibitor or inducer listed, or "_inh1met" for the
#'   inhibitor 1 metabolite. An example of acceptable input: \code{c("pKa1_sub",
#'   "fa_inhib2", "Regimen_sub")}}}
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
#' @param annotate_output TRUE (default) or FALSE on whether to transpose the
#'   rows and columns in the output, making the output table longer instead of
#'   wider, and adding columns to the output for a) which compound the
#'   information pertains to (substrate, inhibitor, etc.), b) which section of
#'   the Simcyp Simulator this detail is found in (physchem, absorption,
#'   distribution, etc.), c) notes describing what the detail is, and d) which
#'   sheet in the Excel file the information was pulled from. Please see
#'   \code{annotateDetails} for ways to sift through and organize this output to
#'   find what you need.
#' @param save_output optionally save the output by supplying a csv or Excel
#'   file name in quotes here, e.g., "Simulation details.csv" or "Simulation
#'   details.xlsx".  Do not include any slashes, dollar signs, or periods in the file name. If you leave off the file extension, it will be saved as a
#'   csv file.
#'
#' @return Returns a data.frame of experimental details for simulator files
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
#'     exp_details = "all",
#'     save_output = "My experimental details.csv")
#'  
extractExpDetails_mult <- function(sim_data_files = NA, 
                                   exp_details = "all", 
                                   existing_exp_details = NA, 
                                   overwrite = FALSE,
                                   annotate_output = FALSE,
                                   save_output = NA, 
                                   ...){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
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
      sim_data_files <- list.files(pattern = "xlsx$",
                                   recursive = (complete.cases(sim_data_files) &&
                                                   sim_data_files == "recursive"))
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   sim_data_files <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$", "", sim_data_files), ".xlsx")
   
   # Making sure that all the files exist before attempting to pull data
   if(any(file.exists(sim_data_files) == FALSE)){
      MissingSimFiles <- sim_data_files[
         which(file.exists(sim_data_files) == FALSE)]
      warning(paste0("The file(s) ", 
                     str_comma(paste0("`", MissingSimFiles, "`")), 
                     " is/are not present, so we cannot extract any information about the simulation experimental details.\n"), 
              call. = FALSE)
      sim_data_files <- setdiff(sim_data_files, MissingSimFiles)
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
   
   if(Recode_existing_exp_details){
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
         existing_exp_details$MainDetails <- existing_exp_details$MainDetails %>%
            filter(!File %in% existing_exp_details$MainDetails$File)
      }
   } else {
      sim_data_files_topull <- unique(sim_data_files)
   }
   
   MyDeets <- list()
   
   for(i in sim_data_files_topull){ 
      message(paste("Extracting simulation experimental details from file =", i))
      MyDeets[[i]] <- extractExpDetails(sim_data_file = i, 
                                        exp_details = exp_details) 
   }
   
   if(length(MyDeets) > 0){
      MyDeets <- MyDeets[which(sapply(MyDeets, \(x) length(x) > 0))]
   }
   Out <- c(list(existing_exp_details), MyDeets)
   # Retaining only files that were simulations.
   Out <- Out[which(sapply(Out, \(x) all(is.null(names(x))) == FALSE))]
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
      
      for(i in names(Out)){
         Out[[i]][["MainDetails"]] <- Out[[i]][["MainDetails"]] %>% 
            mutate(across(.cols = Classes$Detail, 
                          .fns = as.character))
      }
   }
   
   Out <- Out %>% list_transpose() %>% 
      map(.f = bind_rows) 
   
   if(length(Out) == 0 | nrow(Out$MainDetails) == 0){
      stop("It was not possible to extract any simulation experimental details.")
   }
   
   # Sorting to help organize output
   Out$MainDetails <- Out$MainDetails %>% select(File, everything())
   
   if(annotate_output){
      Out <- annotateDetails(Out, 
                             save_output = save_output)
   } else if(complete.cases(save_output)){
      FileName <- save_output
      if(str_detect(FileName, "\\.")){
         # Making sure they've got a good extension
         Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
         FileName <- sub(paste0(".", Ext), "", FileName)
         Ext <- ifelse(Ext %in% c("csv", "xlsx"), 
                       Ext, "csv")
         FileName <- paste0(FileName, ".", Ext)
      } else {
         FileName <- paste0(FileName, ".csv")
         Ext <- "csv"
      }
      
      switch(Ext, 
             "csv" = write.csv(as.data.frame(Out$MainDetails), FileName, row.names = F), 
             "xlsx" = formatXL_head(as.data.frame(Out$MainDetails), 
                                    FileName, 
                                    sheet = "Simulation experimental details"))
   }
   
   return(Out)
}


