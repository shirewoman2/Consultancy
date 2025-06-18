#' Pull concentration-time data from multiple Simcyp Simulator output files
#'
#' \code{extractConcTime_mult} is meant to be used in conjunction with
#' \code{\link{ct_plot_overlay}} or \code{\link{ct_plot_mult}} to create graphs
#' from multiple Simcyp Simulator output files. If you list multiple files,
#' multiple tissues, and/or multiple compounds to extract (see options below),
#' this will extract \emph{all} possible variations of them. For example, if you
#' ask for data from the files "sim1.xlsx" and "sim2.xlsx" and then also ask for
#' "substrate" and "primary metabolite 1", you will get the substrate and
#' primary metabolite 1 data from \emph{both} files. \strong{NOTE:} If ANY of
#' the Excel files you wish to extract data from are open, this WILL CRASH and
#' WILL NOT save whatever progress it has made so far. Be sure to close all of
#' the source Excel files.
#'
#' \strong{Regarding dose intervals for observed data:} The observed data files
#' don't include information on dosing intervals or dose numbers, which makes it
#' a little tricky to figure out which dose number a given time in an observed
#' data file should have. If the compound IDs in the simulated data match those
#' in the observed data, we will assume that the dosing intervals are the same.
#' This was coded with the assumption that the dosing interval would be a round
#' number (subjects aren't getting dosed on the half hour, for example), so if
#' that's not the case, these dose number assignments will be off.
#'
#' @param sim_data_files a character vector of simulator output files, each in
#'   quotes and encapsulated with \code{c(...)}, NA to extract
#'   concentration-time data for \emph{all} the Excel files in the current
#'   folder, or "recursive" to extract concentration-time data for \emph{all}
#'   the Excel files in the current folder and \emph{all} subfolders. Example of
#'   acceptable input: \code{c("sim1.xlsx", "sim2.xlsx")}. The path should be
#'   included with the file names if they are located somewhere other than your
#'   working directory. If some of your Excel files are not regular simulator
#'   output, e.g. they are sensitivity analyses or a file where you were doing
#'   some calculations, those files will be skipped.
#' @param obs_to_sim_assignment the assignment of which observed files go with
#'   which simulated files. (NA, which is the default, means no observed data
#'   will be extracted.) There are four ways to supply this:
#'
#'   \describe{\item{"use existing_exp_details"}{If you have already
#'   extracted the simulation experimental details with the function
#'   \code{\link{extractExpDetails_mult}} and you included observed data overlay
#'   files in your simulations, as long as those XML files have their
#'   corresponding Excel files in the \emph{same} location, we can use that
#'   information to figure out which observed Excel file should go with which
#'   simulation. Note that this \strong{does} require you to supply something
#'   for the argument \code{existing_exp_details} to work. This has been set up
#'   to look for that location even if the user who ran the simulation is
#'   different from the user extracting the data, e.g., if the original path was
#'   something like "C:/Users/FridaKahlo/Rose project simulations" and the
#'   current username (the result from running Sys.info()["user"]) is
#'   "DiegoRivera", this will look in the folder
#'   "C:/Users/DiegoRivera/Rose project simulations" for your XML files. This is
#'   assuming that the file path starts with "C:/Users/CurrentUserName/..." and
#'   will fail to change the username if that is not the case.}
#'
#'   \item{a character vector of the observed data files, each in
#'   quotes and encapsulated with \code{c(...)}}{If all the observed data can be
#'   compared to all the simulated data, then an example of acceptable input
#'   would be: \code{obs_to_sim_assignment = "obsdata1.xlsx"}. However, if you
#'   would like to specify which observed file goes with which simulated file,
#'   you can do this with a named character vector, e.g.,
#'   \code{c("obsdata1.xlsx" = "simfileA.xlsx", "obsdata2.xlsx" =
#'   "simfileB.xlsx")}. If one observed file needs to match more than one
#'   simulated file but not \emph{all} the simulated files, you can do that by
#'   separating the simulated files with commas, e.g.,
#'   \code{obs_to_sim_assignment = c("obs data 1.xlsx" = "mdz-5mg-qd.xlsx,
#'   mdz-5mg-qd-fa08.xlsx", "obs data 2.xlsx" = "mdz-5mg-qd-cancer.xlsx,
#'   mdz-5mg-qd-cancer-fa08.xlsx")}. Pay close attention to the position of
#'   commas and quotes there! This can get a bit confusing, in our opinions, so
#'   you may want to try the other options if you need to link specific observed
#'   and simulated files; they can be easier to follow but require more typing.}
#'
#'   \item{a data.frame with one column for the observed files and one column
#'   for the simulated files they each match}{The data.frame must have column
#'   names of "ObsFile" and "File" for the observed and simulated files,
#'   respectively. Here's an example of acceptable input:
#'   \code{obs_to_sim_assignment = data.frame(ObsFile = c("obsdata1.xlsx",
#'   "obsdata2.xlsx"), File = c("simfileA.xlsx", "simfileB.xlsx"))} Each row
#'   should contain one observed file and one simulated file, so if you want to
#'   compare a single observed file to multiple simulated files, you'll need to
#'   repeat the observed file, e.g., \code{obs_to_sim_assignment =
#'   data.frame(ObsFile = c("obsdata1.xlsx", "obsdata2.xlsx", "obsdata2.xlsx",
#'   "obsdata2.xlsx"), File = c("simfileA.xlsx", "simfileB.xlsx",
#'   "simfileC.xlsx", "simfileD.xlsx"))}}
#'
#'   \item{a csv file with one column for the observed files and one column for
#'   the simulated files they each match}{The setup of this csv file should be
#'   just like that described for supplying a data.frame, so one row for each
#'   pair of simulated and observed files you want to compare to each other.
#'   Supply this as a character string, like this: \code{obs_to_sim_assignment =
#'   "My obs to sim assignments.csv"}}}
#'
#'   For whichever option you choose, the observed files' paths should be
#'   included if they are located somewhere other than your working directory.
#'   The observed data files should be for the Excel file that it is
#'   \emph{ready} to be converted to an XML file, not the file that contains
#'   only the digitized time and concentration data. This function assumes that
#'   the dosing intervals for the observed data match those in the simulated
#'   data. See "Details" for more info.
#' @param ct_dataframe (optional) a data.frame that contains previously
#'   extracted concentration-time data. This should NOT be in quotes. Because we
#'   can see scenarios where you might want to extract some concentration-time
#'   data, play around with those data, and then later decide you want to pull
#'   more concentration-time data for comparisons, this data.frame can already
#'   exist. When that is the case, this function will \emph{add} data to that
#'   data.frame. It will \emph{not} overwrite existing data unless
#'   \code{overwrite} is set to TRUE. However, it also will NOT open any
#'   simulation files that already exist and look for any possible new tissues
#'   and compounds. If you want to add new tissues and compounds that you
#'   previously did NOT extract without overwriting the concentration-time data
#'   you already have, we recommend running a separate instance of
#'   \code{extractConcTime_mult} and then using \code{\link{dplyr::bind_rows}}
#'   to add the new data to the existing ct_dataframe.
#' @param overwrite TRUE or FALSE (default) on whether to re-extract the
#'   concentration-time data from output files that are already included in
#'   \code{ct_dataframe}. Since pulling data from Excel files is slow, by
#'   default, this will \emph{not} overwrite existing data and instead will only
#'   add data from any Excel files that aren't already included. A situation
#'   where you might want to set this to TRUE would be when you have changed
#'   input parameters for simulations and re-run them.
#' @param adjust_obs_time TRUE or FALSE (default) for whether to adjust the time
#'   listed in the observed data file to match the last dose administered. This
#'   only applies to multiple-dosing regimens. If TRUE, the graph will show the
#'   observed data overlaid with the simulated data such that the dose in the
#'   observed data was administered at the same time as the last dose in the
#'   simulated data. If FALSE, the observed data will start at whatever times
#'   are listed in the Excel file.
#' @param tissues From which tissue(s) should the desired concentrations be
#'   extracted? Default is plasma for typical plasma concentration-time data.
#'   Other options are "blood" or any tissues included in "Sheet Options",
#'   "Tissues" in the simulator. All possible options:
#'   \describe{
#'   \item{First-order absorption models}{"plasma", "blood", "unbound blood",
#'   "unbound plasma", "additional organ", "adipose", "bone", "brain",
#'   "feto-placenta", "gut tissue", "heart", "kidney", "liver", "lung", "muscle",
#'   "pancreas", "peripheral blood", "peripheral plasma", "peripheral unbound
#'   blood", "peripheral unbound plasma", "portal vein blood", "portal vein
#'   plasma", "portal vein unbound blood", "portal vein unbound plasma", "skin",
#'   or "spleen".}
#'
#'   \item{ADAM-models}{"stomach", "duodenum", "jejunum I",
#'   "jejunum II", "jejunum III" (only applies to rodents), "jejunum IV" (only
#'   applies to rodents), "ileum I", "ileum II", "ileum III", "ileum IV", "colon",
#'   "faeces", "gut tissue", "cumulative absorption", "cumulative fraction
#'   released", or "cumulative dissolution".}}
#'
#'   Not case sensitive. Acceptable
#'   input is all tissues desired as a character vector, e.g., \code{tissues =
#'   c("plasma", "blood", "liver")} or, if you want all possible tissues and
#'   you've got some time to kill, "all". That will make R check for all sorts
#'   of possible permutations of tab names, so it does take a while. NOTE: If
#'   you want PD input or PD response for the compoundsToExtract, tissue will be
#'   ignored.
#' @param compoundsToExtract For which compounds do you want to extract
#'   concentration-time data? Options are:
#'
#'   \itemize{
#'   \item{"all" (default) for all the typical compounds in a simulation: 
#'   substrate, perpetrators, metabolites, etc.}
#'   \item{"substrate"}
#'   \item{"primary metabolite 1"}
#'   \item{"primary metabolite 2"}
#'   \item{"secondary metabolite"}
#'   \item{"inhibitor 1" -- this can be an inducer, inhibitor, activator, or
#'   suppresesor, but it's labeled as "Inhibitor 1" in the simulator}
#'   \item{"inhibitor 2" for the 2nd inhibitor listed in the simulation}
#'   \item{"inhibitor 1 metabolite" for the primary metabolite of inhibitor 1}
#'   \item{"PD input"}
#'   \item{"PD response"}
#'   \item{"intact ADC" for DAR1-DARmax for an antibody-drug conjugate;
#'   observed data with DV listed as "Conjugated Protein Plasma Total" will
#'   match these simulated data}
#'   \item{"conjugated payload"; observed data with DV listed as
#'   "Conjugated Drug Plasma Total" will match these simulated data}
#'   \item{"total antibody" for DAR0-DARmax for an ADC; observed data with DV
#'   listed as "Total Protein Conjugate Plasma Total" will match these simulated data}
#'   \item{"released payload" for the released drug from an ADC, which shows up
#'   as "Sub Pri Met1" in Simulator output files.}
#'   \item{"therapeutic protein" for mAb concentrations alone}
#'   \item{"therapeutic protein and TMDD complex" for mAb concentrations
#'   including when bound to the target}
#'   }
#'
#'   \strong{Note:} If your compound is a therapeutic protein or ADC, we haven't
#'   tested this very thoroughly, so please be extra careful to check that
#'   you're getting the correct data.
#' @param ... other arguments passed to the function
#'   \code{\link{extractConcTime}}
#' @param conc_units_to_use concentration units to use so that all data will be
#'   comparable. Options are the same as the ones in the Excel form for PE data
#'   entry. Default is "ng/mL". Note: ADAM model data concentration units are
#'   not converted because there are simply too many units to manage easily, so
#'   please check that the units are what you expected in the end.
#' @param time_units_to_use time units to use so that all data will be
#'   comparable. Options are "hours" (default), "days", "weeks", or "minutes".
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated
#'   concentration-time data? Options are "aggregate" (default), "individual",
#'   or "both". Aggregated data are not calculated here but are pulled from the
#'   simulator output rows labeled as "Population Statistics".
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get all the details from the "Input
#'   Sheet" (e.g., when you ran extractExpDetails_mult you said
#'   \code{exp_details = "Input Sheet"} or \code{exp_details = "all"}), you can
#'   save some processing time by supplying that object here, unquoted. If left
#'   as NA, this function will run \code{extractExpDetails} behind the scenes to
#'   figure out some information about your experimental set up.
#' @param obs_data_files TO BE DEPRECATED. This is the same argument as
#'   obs_to_sim_assignment; we just renamed it to try to be clearer about what
#'   the argument does and in what order you should list the files.
#'
#' @return Returns a large data.frame with multiple sets of concentration-time
#'   data, formatted the same way as output from the function
#'   \code{\link{extractConcTime}}
#' @export
#'
#' @examples
#' ConcTimeData <-
#'       extractConcTime_mult(
#'             sim_data_files = c("MyFile1.xlsx", "MyFile2.xlsx"),
#'             ct_dataframe = ConcTimeData,
#'             overwrite = FALSE,
#'             tissues = "unbound plasma")
#' 

extractConcTime_mult <- function(sim_data_files = NA,
                                 obs_to_sim_assignment = NA,
                                 ct_dataframe = NA,
                                 overwrite = FALSE,
                                 tissues = "plasma",
                                 compoundsToExtract = "all",
                                 conc_units_to_use = "ng/mL",
                                 time_units_to_use = "hours",
                                 returnAggregateOrIndiv = "aggregate",
                                 adjust_obs_time = FALSE,
                                 existing_exp_details = NA,
                                 obs_data_files = NA, 
                                 ...){
   
   # Error catching -------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Checking whether they've supplied extractConcTime args instead of
   # extractConctTime_mult args
   if("sim_data_file" %in% names(match.call()) &
      "sim_data_files" %in% names(match.call()) == FALSE){
      sim_data_files <- sys.call()$sim_data_file
   }
   
   if("obs_data_file" %in% names(match.call()) &
      "obs_data_files" %in% names(match.call()) == FALSE){
      obs_data_files <- sys.call()$obs_data_file
   }
   
   if("tissue" %in% names(match.call()) &
      "tissues" %in% names(match.call()) == FALSE){
      tissues <- sys.call()$tissue
   }
   
   if("compoundToExtract" %in% names(match.call()) &
      "compoundsToExtract" %in% names(match.call()) == FALSE){
      compoundsToExtract <- sys.call()$compoundToExtract
   }
   
   compoundsToExtract_orig <- compoundsToExtract
   compoundsToExtract <- tolower(compoundsToExtract)
   compoundsToExtract <- sub("released payload", "primary metabolite 1", 
                             compoundsToExtract)
   
   MainCompoundIDs <- AllRegCompounds$CompoundID
   
   ADCCompoundIDs <- AllCompounds %>% 
      filter(CompoundType == "ADC") %>% 
      pull(CompoundID)
   # NB: Released payload included w/MainCompoundIDs as primary metabolite 1
   
   PossCmpd <- c(MainCompoundIDs, ADCCompoundIDs,
                 "pd response", "pd input", "all")
   
   if(any(compoundsToExtract %in% PossCmpd == FALSE)){
      
      BadCmpds <- tibble(Orig = compoundsToExtract_orig, 
                         Rev = compoundsToExtract) %>% 
         filter(Rev %in% setdiff(compoundsToExtract, PossCmpd)) %>% 
         pull(Orig)
      
      warning(wrapn(paste0(
         "The compound(s) ", 
         str_comma(paste0("'", BadCmpds, "'")),
         " is/are not among the possible componds to extract and will be ignored. The possible compounds to extract are only exactly these: ",
         str_comma(paste0("'", PossCmpd, "'")))), 
         call. = FALSE)
      
      compoundsToExtract <- intersect(compoundsToExtract, PossCmpd)
   }
   
   if(length(compoundsToExtract) == 0){
      stop(wrapn("You have not supplied any valid compounds to extract. Please check your input and try again."), 
           call. = FALSE)
   }
   
   if(any(complete.cases(obs_data_files))){
      if(any(complete.cases(obs_to_sim_assignment[1]))){
         warning("You specified values for both the `obs_to_sim_assignment` argument and for the soon-to-be-deprecated `obs_data_files` argument. Only the value for `obs_to_sim_assignment` will be used.\n", 
                 call. = FALSE)
      } else {
         warning("You used the soon-to-be-deprecated argument `obs_data_files` to indicate which observed files to use. Please use the argument `obs_to_sim_assignment` in the future. Please see the help file for more information.\n", 
                 call. = FALSE) 
         obs_to_sim_assignment <- obs_data_files
      }
   }
   
   # If they used the American spelling of feces, change to the British version
   # for compatibility with simulator output
   tissues <- sub("feces", "faeces", tissues)
   
   tissue_input <- tissues
   if(all(tissue_input == "all")){
      tissues <- unique(AllTissues %>% filter(ModelType != "ADC") %>% 
                           pull(Tissue))
   }
   
   # Make it so that, if they supply NA, NULL, or "none" for ct_dataframe, all
   # of those will work. Note to coders: It was REALLY HARD to get this to work
   # with just the perfect magical combination of exists and suppressWarnings,
   # etc.
   
   # If user supplied an unquoted object, this checks whether that object
   # exists. However, if they supplied NA or NULL, this throws an error. Also,
   # "exists" does not work with operators, so, if the user supplied an item in
   # a list, e.g., they split ct_dataframe by whether it was a development or
   # verification sim or something, supplying, e.g., CT[[i]] will result in
   # FALSE here.
   Recode_ct_dataframe <- suppressWarnings(
      try(exists(deparse(substitute(ct_dataframe))) == FALSE, silent = TRUE))
   
   # If they got an error, then the class of Recode_X will be "try-error", and
   # then we want Recode_X to be TRUE.
   if(suppressWarnings("try-error" %in% class(Recode_ct_dataframe))){
      Recode_ct_dataframe <- TRUE
   }
   
   if(Recode_ct_dataframe){
      ct_dataframe <- "none"
   }
   
   
   # Main body of function -----------------------------------------------
   
   # tic(msg = "Main body of function - mult")
   
   # If user did not supply files, then extract all the files in the current
   # folder that end in "xlsx" or in all subfolders if they wanted it to be
   # recursive.
   if(length(sim_data_files) == 1 &&
      (is.na(sim_data_files) | sim_data_files == "recursive")){
      sim_data_files <- list.files(pattern = "\\.xlsx$|db$",
                                   recursive = (complete.cases(sim_data_files) &&
                                                   sim_data_files == "recursive"))
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   }
   
   # If they didn't include ".xlsx" or ".db" at the end of the file name, assume
   # they want the more-developed option and add "xlsx".
   MissingExt <- which(str_detect(sim_data_files, "\\.xlsx$|\\.db$") == FALSE)
   sim_data_files[MissingExt] <- 
      sub("\\.wksz$|\\.dscw$", ".xlsx", sim_data_files[MissingExt])
   
   sim_data_files <- unique(sim_data_files)
   
   # Checking on what combinations of data the user has requested and what
   # data are already present in ct_dataframe.
   if(compoundsToExtract[1] == "all"){
      compoundsToExtract_orig <- "all"
      compoundsToExtract <- c(MainCompoundIDs, ADCCompoundIDs)
   } else {
      compoundsToExtract_orig <- compoundsToExtract
   }
   
   Requested <- expand_grid(Tissue = tissues,
                            CompoundID = compoundsToExtract,
                            File = sim_data_files) %>% 
      mutate(ID = paste(File, Tissue, CompoundID))
   
   # Checking for existing conc-time data
   if(exists(deparse(substitute(ct_dataframe))) && 
      "logical" %in% class(ct_dataframe) == FALSE &&
      "data.frame" %in% class(ct_dataframe) && 
      nrow(ct_dataframe) > 0){
      
      if("File" %in% names(ct_dataframe) == FALSE){
         ct_dataframe$File <- "unknown file"
      }
      
      ct_dataframe <- ct_dataframe %>%
         mutate(ID = paste(File, Tissue, CompoundID))
      
      if(overwrite == FALSE){
         
         DataToFetch <- Requested %>% 
            filter(!File %in% ct_dataframe$File)
         
         sim_data_files_topull <- unique(as.character(DataToFetch$File))
         
      } else {
         
         DataToFetch <- Requested
         
         sim_data_files_topull <- unique(sim_data_files)
         ct_dataframe <- ct_dataframe %>%
            filter(!File %in% DataToFetch$File)
      }
      
   } else {
      
      # This is when there's no existing data, so we're just getting everything. 
      DataToFetch <- Requested
      sim_data_files_topull <- sim_data_files
      ct_dataframe <- data.frame()
      
   }
   
   if(length(sim_data_files_topull) == 0){
      message("There are no data to pull that are not already present in your current data.frame. Returning current data.frame.")
      return(ct_dataframe)
   }
   
   # Making sure that all the files exist before attempting to pull data
   MissingSimFiles <- sim_data_files_topull[
      which(file.exists(sim_data_files_topull) == FALSE)]
   
   if(length(MissingSimFiles) > 0){
      
      warning(paste0("The simulation file(s) ", 
                     str_comma(paste0("`", MissingSimFiles, "`")), 
                     " is/are not present and thus will not be extracted.\n"), 
              call. = FALSE)
      sim_data_files_topull <- setdiff(sim_data_files_topull, MissingSimFiles)
   }
   
   # Dealing w/changed name for Tissue_subsection
   if(nrow(ct_dataframe) > 0 & 
      "subsection_ADAM" %in% names(ct_dataframe) && 
      "Tissue_subtype" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$Tissue_subtype <- ct_dataframe$subsection_ADAM
   }
   
   ## Getting exp details as needed --------------------------------------------
   
   if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA and we have not yet extracted any details
      existing_exp_details <- 
         extractExpDetails_mult(sim_data_files = sim_data_files, 
                                exp_details = "Summary and Input", 
                                existing_exp_details = existing_exp_details)
      
   } else {
      
      existing_exp_details <- harmonize_details(existing_exp_details)
      
      PossFiles <- 
         switch(as.character("DBFile" %in% names(existing_exp_details$MainDetails)), 
                "TRUE" = c(existing_exp_details$MainDetails$File, 
                           existing_exp_details$MainDetails$DBFile, 
                           sub("\\.xlsx", ".db", existing_exp_details$MainDetails$File), 
                           sub("\\.db", ".xlsx", existing_exp_details$MainDetails$DBFile)), 
                "FALSE" = c(existing_exp_details$MainDetails$File, 
                            sub("\\.xlsx", ".db", existing_exp_details$MainDetails$File)))
      
      if(all(sim_data_files %in% PossFiles) == FALSE){
         existing_exp_details <- 
            extractExpDetails_mult(sim_data_files = sim_data_files, 
                                   exp_details = "Summary and Input", 
                                   existing_exp_details = existing_exp_details)
         
      } else {
         # If there's a file extension mismatch, change it in
         # existing_exp_details b/c all the information should be the same.
         
         # First, check whether file is included in existing_exp_details b/c it
         # could be present as BOTH xlsx and db files.
         ExtMismatch <- setdiff(sim_data_files, 
                                existing_exp_details$MainDetails$File)
         
         # Next, only change file extensions for any files that are not already
         # included in existing_exp_details.
         WhichXLtoDB <- which(
            existing_exp_details$MainDetails$File %in% sim_data_files == FALSE & 
               sub("\\.xlsx", ".db", existing_exp_details$MainDetails$File) %in% ExtMismatch)
         existing_exp_details$MainDetails$File[WhichXLtoDB] <- 
            sub("\\.xlsx", ".db", existing_exp_details$MainDetails$File[WhichXLtoDB])
         
         WhichDBtoXL <- which(
            existing_exp_details$MainDetails$File %in% sim_data_files == FALSE & 
               existing_exp_details$MainDetails$File %in% sub("\\.db", ".xlsx", ExtMismatch))
         existing_exp_details$MainDetails$File[WhichDBtoXL] <- 
            sub("\\.db", ".xlsx", existing_exp_details$MainDetails$File[WhichDBtoXL])
      }
   }
   
   sim_data_files_topull <- intersect(sim_data_files_topull, 
                                      existing_exp_details$MainDetails$File)
   
   # If it wasn't a Simulator file, it will not be in sim_data_files_topull.
   # Yes, this return was included above, but including it there AND here should
   # minimize the amount of redundant data extraction and thus improve speed.
   if(length(sim_data_files_topull) == 0){
      message("There are no data to pull that are not already present in your current data.frame. Returning current data.frame.")
      return(ct_dataframe)
   }
   
   # If the file is a Simulator output file, we should have it now. Checking and
   # removing any that are not.
   
   PossFiles <- 
      switch(as.character("DBFile" %in% names(existing_exp_details$MainDetails)), 
             "TRUE" = c(existing_exp_details$MainDetails$File, 
                        existing_exp_details$MainDetails$DBFile), 
             "FALSE" = existing_exp_details$MainDetails$File)
   
   if(all(sim_data_files_topull %in% PossFiles) == FALSE){
      
      BadFiles <- setdiff(sim_data_files_topull, PossFiles)
      
      warning(paste0("The following files were requested but do not appear to be Simcyp Simulator files and will be ignored:\n", 
                     str_c(BadFiles, collapse = "\n")), 
              call. = FALSE)
      sim_data_files_topull <- intersect(sim_data_files_topull, PossFiles)
   }
   
   ## Dealing with possible observed data assignments -------------------------
   if("character" %in% class(obs_to_sim_assignment) && 
      any(str_detect(tolower(obs_to_sim_assignment), "use existing|use.*details"))){
      
      ### SCENARIO A: Match w/expdetails -------------------------------------
      
      if("logical" %in% class(existing_exp_details)){
         warning("You requested that we match observed data to simulated data based on `existing_exp_details`, but you haven't supplied anything for `existing_exp_details`. We cannot extract any observed data.\n", 
                 call. = FALSE)
         ObsAssign <- data.frame()
         obs_to_sim_assignment <- NA
         
      } else {
         
         existing_exp_details <- harmonize_details(existing_exp_details)
         
         if(all(sim_data_files_topull %in% 
                existing_exp_details$MainDetails$File) == FALSE){
            existing_exp_details <- extractExpDetails_mult(
               sim_data_files = sim_data_files, 
               existing_exp_details = existing_exp_details,
               exp_details = "Summary and Input")
         }
         
         if("ObsOverlayFile" %in% 
            names(existing_exp_details$MainDetails) == FALSE){
            
            warning(wrapn("The observed data overlay file was not included in 'existing_exp_details', so we don't know which observed data files to use for the simulated files. We cannot extract any observed data."), 
                    call. = FALSE)
            ObsAssign <- data.frame()
            obs_to_sim_assignment <- NA
            
         } else {
            
            # Make this work for whoever the current user is, even if the XML obs file
            # path was for someone else. This will normalize paths ONLY when the full
            # path is present and starts w/"Users". Otherwise, keeping the original input
            # just b/c I don't want to change the input from basename to full path
            # unexpectedly.
            if(any(complete.cases(existing_exp_details$MainDetails$ObsOverlayFile))){
               existing_exp_details$MainDetails$ObsOverlayFile[
                  which(str_detect(existing_exp_details$MainDetails$ObsOverlayFile, "Users"))] <- 
                  normalizePath(existing_exp_details$MainDetails$ObsOverlayFile[
                     which(str_detect(existing_exp_details$MainDetails$ObsOverlayFile, "Users"))], 
                     winslash = "/", mustWork = FALSE)
            }
            
            existing_exp_details$MainDetails$ObsOverlayFile <-
               str_replace(existing_exp_details$MainDetails$ObsOverlayFile, 
                           "Users/(?<=\\/)[^\\/]+(?=\\/)", 
                           paste0("Users/", Sys.info()["user"]))
            
            
            ObsAssign <- existing_exp_details$MainDetails %>% 
               select(File, ObsOverlayFile) %>% 
               rename(ObsFile = ObsOverlayFile) %>% 
               mutate(
                  ObsFile = gsub("\\\\", "/", ObsFile), 
                  ObsFile = sub("Users/.*/Certara", 
                                paste0("Users/", Sys.info()["user"], 
                                       "/Certara"), ObsFile), 
                  # Noting whether obs file was from V24+ and thus embedded, which
                  # means we don't know path.
                  V24plusObsFile = str_detect(ObsFile, "embedded"), 
                  ObsFile = case_when(
                     V24plusObsFile == TRUE ~
                        paste0(sub("\\(embedded\\)", "", ObsFile), 
                               ".xml"), 
                     .default = ObsFile), 
                  # Checking that the file exists.
                  Exists = file.exists(ObsFile), 
                  ObsFile_xlsx = sub("\\.xml$", ".xlsx", ObsFile), 
                  ObsFile_xml = sub("\\.xlsx$", ".xml", ObsFile), 
                  Exists_xlsx = file.exists(ObsFile_xlsx), 
                  Exists_xml = file.exists(ObsFile_xml), 
                  ObsFileToUse = case_when(
                     Exists_xlsx ~ ObsFile_xlsx, 
                     Exists_xml ~ ObsFile_xml, 
                     Exists ~ ObsFile), 
                  ObsFileToUseExists = file.exists(ObsFileToUse))
            
            MissingV24ObsFiles <- 
               ObsAssign %>% filter(ObsFileToUseExists == FALSE &
                                       V24plusObsFile == TRUE)
            
            if(nrow(MissingV24ObsFiles) > 0){
               
               message(wrapn("The observed overlay XML file(s) for the following simulation(s) could not be found:"))
               Problem <- capture.output(print(MissingV24ObsFiles %>% 
                                                  select(File, ObsFile), row.names = FALSE))
               message(str_c(Problem, collapse = "\n"))
               message(wrapn("Please note that, in Simcyp Simulator versions 24 and above, the path of the observed file is not saved with the workspace, so we won't be able to extract the data if it's not in the same folder."))
               
            }
            
            # To get data from the xml file, we need to have the Simcyp package
            # installed and it has to be a version of Simcyp that has the
            # ReadPEData function, which was added around V22 or maybe V23. I
            # can't seem to incorporate this into the case_when evaluation, so
            # doing this separately.
            if(length(find.package("Simcyp", quiet = TRUE)) > 0 &&
               "ReadPEData" %in% getNamespaceExports("Simcyp") == FALSE){
               ObsAssign$Exists_xml <- FALSE
            } 
            
            MissingObsFile <- ObsAssign %>% 
               filter(complete.cases(ObsFile) & is.na(ObsFileToUse)) %>% 
               pull(ObsFileToUse) %>% unique()
            
            if(length(MissingObsFile) > 0 && complete.cases(MissingObsFile)){
               warning(wrapn(paste0(
                  "The observed data file(s) ", str_comma(
                     paste0("'", MissingObsFile, "'")), 
                  " is/are not present and will be skipped.")), 
                  call. = FALSE)
            }
            
            # Now that we've figured out whether to use the xlsx or
            # xml version, set the correct one for ObsFile. 
            ObsAssign <- ObsAssign %>% 
               mutate(ObsFile = ObsFileToUse) %>% 
               filter(complete.cases(ObsFileToUse) &
                         File %in% sim_data_files_topull)
            
            # Checking whether we can find any obs files 
            if(nrow(ObsAssign) == 0){
               warning(wrapn("You requested that we use what you provided for 'existing_exp_details' to match observed data to simulated, but either you didn't include any observed data with your workspace(s) or we couldn't find any of your observed data files. We will not be able to extract any observed data."), 
                       call. = FALSE)
               ObsAssign <- data.frame()
               obs_to_sim_assignment <- NA
            } 
         }
      }
   } else if("logical" %in% class(obs_to_sim_assignment)){
      
      ### SCENARIO B: No obs match -------------------------------------------
      
      # This is when the user has not specified anything for
      # obs_to_sim_assignment.
      ObsAssign <- data.frame()
      
   } else {
      
      ### SCENARIO C: Match w/user-specified files ------------------------------
      
      if("character" %in% class(obs_to_sim_assignment)){
         if(any(str_detect(obs_to_sim_assignment, ".csv$"))){
            # user has supplied a csv file for designating obs and sim
            # assignments.
            ObsAssign <- read.csv(obs_to_sim_assignment) %>% unique()
         } else {
            # Separating obs_to_sim_assignment so that it will work well
            # with each simulator file. I wanted this to be obs 1st and then
            # sim 2nd b/c it will often be the case that you would want to
            # compare multiple sim files to the same obs data, so I wanted
            # the value (sim file) to be able to be something convoluted
            # with commas I could separate. For *here*, though, inside the
            # actual function, it works better if things are named by the
            # sim file. Splitting up the character vector to get things
            # separated by sim file.
            
            # Making sure that the split pattern will work in case the user
            # omitted spaces.
            obs_to_sim_assignment <- gsub(",[^ ]", ", ", obs_to_sim_assignment)
            if(length(names(obs_to_sim_assignment)) > 0){
               ObsAssign <- as.data.frame(str_split(obs_to_sim_assignment, pattern = ", ", 
                                                    simplify = TRUE)) %>% 
                  mutate(ObsFile = names(obs_to_sim_assignment)) %>% 
                  pivot_longer(cols = -ObsFile, names_to = "V", values_to = "File") %>% 
                  select(File, ObsFile) %>% unique() %>% 
                  filter(File != "")
               
            } else {
               
               # NB: This is when the user wants a single obs file to match all
               # sim files they're extracting. 
               ObsAssign <- data.frame(ObsFile = obs_to_sim_assignment, 
                                       File = sim_data_files)
            }
         }
         
      } else if("data.frame" %in% class(obs_to_sim_assignment)){
         # This is when the user has supplied a data.frame for
         # obs_to_sim_assignment.
         ObsAssign <- obs_to_sim_assignment %>% unique()
      }
      
      # Tidying up a few things. Checking column names and dealing with any
      # misspecification of caps by user
      names(ObsAssign) <- toupper(names(ObsAssign))
      
      if("FILE" %in% names(ObsAssign) == FALSE && 
         "SIMFILE" %in% names(ObsAssign)){
         ObsAssign <- ObsAssign %>% rename(FILE = SIMFILE)
      } else if(all(c("FILE", "SIMFILE") %in% names(ObsAssign))){
         ObsAssign <- ObsAssign %>% select(-File) %>% rename(FILE = SIMFILE)
      }
      
      if(all(c("FILE", "OBSFILE") %in% names(ObsAssign)) == FALSE){
         warning("You have specified values for `obs_to_sim_assignment`, but it's not clear which should be for the observed files and which for the simulated files. Please check the help file for acceptable input. For now, we will not extract data from any observed data files.\n", 
                 call. = FALSE)
         ObsAssign <- NA
      } else {
         
         # Now that column names should be correct, converting to the case I like
         # for ease of coding
         ObsAssign <- ObsAssign %>% rename(File = FILE, ObsFile = OBSFILE) %>% 
            select(File, ObsFile)
         
         if(any(duplicated(ObsAssign$File))){ 
            Dups <- ObsAssign$File[duplicated(ObsAssign$File)] %>% unique()
            warning(wrapn(paste0("You have more than one observed data file assigned to the simulator files ",
                                 str_comma(paste0("`", Dups, "`")),
                                 ". This function can only handle one observed file per simulator file, so only the first observed file listed will be used.")),
                    call. = FALSE)
            ObsAssign <- ObsAssign[!duplicated(ObsAssign$File), ]
         }
         
         if(any(complete.cases(ObsAssign$File))){
            MissingFiles <- setdiff(ObsAssign$File,
                                    unique(c(sim_data_files, ct_dataframe$File)))
            if(length(MissingFiles) > 0){
               warning(paste0(wrapn(
                  "When you assigned observed data files to simulator files with the argument `obs_to_sim_assignment`, you included simulator files that are *not* included in `sim_data_files`. We cannot include these observed data files in the output data because we don't know which simulator files they belong with. The problem simulator files is/are: "), 
                  str_comma(MissingFiles), ", which is/are set to match the following observed files: ",
                  str_comma(names(obs_to_sim_assignment[
                     which(obs_to_sim_assignment %in%
                              unique(c(sim_data_files, ct_dataframe$File)) == FALSE)])), 
                  ".\n"), 
                  call. = FALSE)
               
               ObsAssign <-
                  ObsAssign %>% 
                  filter(File %in% unique(c(sim_data_files, ct_dataframe$File)))
            }
            
            # Making sure obs files exist before trying to pull data from them
            MissingObsFiles <- ObsAssign %>% 
               mutate(Exists = file.exists(ObsFile)) %>% 
               filter(complete.cases(ObsFile) & 
                         Exists == FALSE) %>% 
               pull(ObsFile) %>% unique()
            
            if(length(MissingObsFiles) > 0){
               
               warning(wrapn(paste0(
                  "The observed data file(s) ", 
                  str_comma(paste0("`", MissingObsFiles, "`")), 
                  " is/are not present and thus will not be extracted.")), 
                  call. = FALSE)
               ObsAssign <- ObsAssign %>% filter(!ObsFile %in% MissingObsFiles)
            }
         }
      } 
   }
   
   # End of error catching for obs_to_sim_assignment. ObsAssign should now be a
   # data.frame with columns File and ObsFile or else a completely empty
   # data.frame.
   if(nrow(ObsAssign) > 0){
      ObsAssign <- ObsAssign %>% filter(complete.cases(File) &
                                           complete.cases(ObsFile) &
                                           File %in% sim_data_files_topull) 
   }
   # toc(log = TRUE)
   
   ## Start of loop through files ------------------------------------------
   MultData <- list()
   
   # tic(msg = "start of loop through files - mult")
   
   for(ff in sim_data_files_topull){
      message(paste("Extracting concentration-time data from file =", ff))
      MultData[[ff]] <- list()
      
      Deets <- filter_sims(existing_exp_details, ff, "include")$MainDetails
      
      # Probably don't need this any more since we now filter out non-Simulator
      # files earlier than here, but it shouldn't hurt anything.
      if(nrow(Deets) == 0){
         # Using "warning" instead of "stop" here b/c I want this to be able to
         # pass through to other functions and just skip any files that
         # aren't simulator output.
         warning(wrapn(paste0("The file '", ff,
                              "' does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file.")), 
                 call. = FALSE)
         next()
      }
      
      # Names of compounds requested for checking whether the data exist
      CompoundCheck <- c("substrate" = Deets$Substrate,
                         "inhibitor 1" = ifelse("Inhibitor1" %in% names(Deets), 
                                                Deets$Inhibitor1, NA),
                         "inhibitor 1 metabolite" = ifelse("Inhibitor1Metabolite" %in% names(Deets), 
                                                           Deets$Inhibitor1Metabolite, NA),
                         "inhibitor 2" = ifelse("Inhibitor2" %in% names(Deets), 
                                                Deets$Inhibitor2, NA),
                         "primary metabolite 1" = ifelse("PrimaryMetabolite1" %in% names(Deets), 
                                                         Deets$PrimaryMetabolite1, NA),
                         "primary metabolite 2" = ifelse("PrimaryMetabolite2" %in% names(Deets), 
                                                         Deets$PrimaryMetabolite2, NA),
                         "secondary metabolite" = ifelse("SecondaryMetabolite" %in% names(Deets), 
                                                         Deets$SecondaryMetabolite, NA), 
                         "pd input" = "pd input", 
                         "pd response" = "pd response")
      
      # NB: Asking whether it's an ADCSimulation or an ADCSimulation_sub, the
      # former for back compatibility w/package versions < 2.6.0
      
      if("ADCSimulation_sub" %in% names(Deets) == FALSE){
         if("ADCSimulation" %in% names(Deets)){
            Deets$ADCSimulation_sub <- Deets$ADCSimulation
         } else {
            Deets$ADCSimulation_sub <- FALSE
         }
      }
      
      if(any(Deets$ADCSimulation_sub, na.rm = TRUE)){
         ToAdd <- AllCompounds %>% 
            filter(CompoundType == "ADC" & 
                      CompoundID != "primary metabolite 1") %>% 
            pull(CompoundID)
         names(ToAdd) <- ToAdd
         CompoundCheck <- c(CompoundCheck,
                            ToAdd)
         # NB: NOT including "released payload" here b/c it's coded as primary
         # metabolite 1 in the outputs. Will change this at the end.
         rm(ToAdd)
      }
      
      if(compoundsToExtract_orig[1] == "all"){
         compoundsToExtract_n <- intersect(
            names(CompoundCheck)[complete.cases(CompoundCheck)], 
            compoundsToExtract)
      } else {
         # If the requested compound is not present in the Excel file, remove
         # it from consideration.
         compoundsToExtract_n <- intersect(compoundsToExtract,
                                           names(CompoundCheck)[complete.cases(CompoundCheck)])
      }
      
      if(compoundsToExtract_orig[1] != "all" &&
         all(compoundsToExtract %in% compoundsToExtract_n) == FALSE){
         warning(paste0("For the file ", ff, ", the compound(s) ",
                        str_comma(setdiff(compoundsToExtract, compoundsToExtract_n)),
                        " was/were not available.\n"),
                 call. = FALSE)
      }
      
      # Determining obs file to use as applicable
      MyObsFile <- ObsAssign$ObsFile[ObsAssign$File == ff]
      if(length(MyObsFile) == 0){MyObsFile <- NA}
      
      # Each tissue will be on its own sheet in the Excel file, so each
      # will need their own iterations of the loop for reading different
      # sheets.
      for(j in tissues){
         
         message(paste("     for tissue =", j))
         # Depending on both the tissue AND which compound the user requests,
         # that could be on multiple sheets or on a single sheet. Figuring out
         # which sheet to read.
         
         # Extracting solid tissue or plasma/blood data? Sheet format differs. A
         # few notes: 
         
         # a) blood, plasma, and unbound versions of each have substrate,
         # inhibitor 1, inhibitor 2, and inhibitor 1 metabolite concentrations
         # on one tab and then any substrate metabolites each have their own
         # tab. Calling this tissue type "systemic". 
         
         # b) most solid tissues have only substrate and inhibitor 1
         # concentrations available. Only one tab is available per tissue.
         # Calling this tissue type "tissue".
         
         # c) portal vein and liver have all concs for all compounds on a single
         # tab, but there are separate tabs for plasma, unbound plasma, blood,
         # and unbound blood for portal vein. Calling this tissue type "liver".
         
         # d) feces have only compound ID per tab. I think the only compounds
         # available for feces are substrate and inhibitor 1. Calling this
         # tissue type "faeces".
         
         # e) Concentrations for parts of ADCs are available in plasma for sure
         # and maybe blood (need to check) and then "conjugated payload"
         # and "intact ADC" are also available in lymph. 
         
         TissueType <- case_when(
            str_detect(j, "plasma|blood|peripheral") ~ "systemic",
            str_detect(j, "portal|liver") ~ "liver",
            str_detect(j, "faeces") ~ "faeces", 
            TRUE ~ "tissue")
         
         if(TissueType == "tissue"){
            # If the tissue type is a solid tissue except for feces and this was
            # done with the regular simulator, then any compound concentrations
            # available will be on that sheet and that requires only one
            # iteration of the loop. Feces are special and there's one tab for
            # substrate and one for inhibitor 1. 
            
            if(str_detect(ff, "\\.db$")){
               MultData[[ff]][[j]] <- extractConcTime_DB(
                  sim_data_file = ff,
                  # obs_data_file = MyObsFile, 
                  compoundToExtract = intersect(compoundsToExtract_n,
                                                c("substrate", "inhibitor 1")),
                  tissue = j,
                  returnAggregateOrIndiv = returnAggregateOrIndiv, 
                  existing_exp_details = existing_exp_details)
               
            } else {
               
               MultData[[ff]][[j]] <- extractConcTime(
                  sim_data_file = ff,
                  obs_data_file = MyObsFile, 
                  compoundToExtract = intersect(compoundsToExtract_n,
                                                c("substrate", "inhibitor 1")),
                  tissue = j,
                  returnAggregateOrIndiv = returnAggregateOrIndiv, 
                  fromMultFunction = TRUE, 
                  existing_exp_details = existing_exp_details)
            }
            
         } else if(TissueType == "faeces"){
            
            if(str_detect(ff, "\\.db$")){
               MultData[[ff]][[j]] <- extractConcTime_DB(
                  sim_data_file = ff,
                  # obs_data_file = MyObsFile, 
                  compoundToExtract = intersect(compoundsToExtract_n,
                                                c("substrate", "inhibitor 1")),
                  tissue = j,
                  returnAggregateOrIndiv = returnAggregateOrIndiv, 
                  existing_exp_details = existing_exp_details)
               
            } else {
               if("substrate" %in% compoundsToExtract_n){
                  MultData[[ff]][[j]][["substrate"]] <- extractConcTime(
                     sim_data_file = ff,
                     obs_data_file = MyObsFile, 
                     compoundToExtract = "substrate",
                     tissue = j,
                     returnAggregateOrIndiv = returnAggregateOrIndiv, 
                     fromMultFunction = TRUE, 
                     existing_exp_details = existing_exp_details)
               }
               
               if("inhibitor 1" %in% compoundsToExtract_n){
                  MultData[[ff]][[j]][["inhibitor 1"]] <- extractConcTime(
                     sim_data_file = ff,
                     obs_data_file = MyObsFile, 
                     compoundToExtract = "inhibitor 1",
                     tissue = j,
                     returnAggregateOrIndiv = returnAggregateOrIndiv, 
                     fromMultFunction = TRUE, 
                     existing_exp_details = existing_exp_details)
               }
            }
            
            MultData[[ff]][[j]] <- bind_rows(MultData[[ff]][[j]])
            
            
         } else if(TissueType == "liver"){
            
            if(str_detect(ff, "\\.db$")){
               MultData[[ff]][[j]] <- extractConcTime_DB(
                  sim_data_file = ff,
                  # obs_data_file = MyObsFile, 
                  compoundToExtract = intersect(compoundsToExtract_n,
                                                c("substrate", "inhibitor 1")),
                  tissue = j,
                  returnAggregateOrIndiv = returnAggregateOrIndiv, 
                  existing_exp_details = existing_exp_details)
               
            } else {
               
               if(Deets$SimulatorUsed == "Simcyp Discovery"){
                  
                  # Liver tissue has all compounds together in the main
                  # simulator but on separate tabs in Discovery. This is just a
                  # hack to make things work anyway.
                  LiverDisc <- list()
                  
                  for(cc in compoundsToExtract_n){
                     LiverDisc[[cc]] <- extractConcTime(
                        sim_data_file = ff,
                        obs_data_file = MyObsFile, 
                        compoundToExtract = cc,
                        tissue = j,
                        returnAggregateOrIndiv = returnAggregateOrIndiv, 
                        fromMultFunction = TRUE, 
                        existing_exp_details = existing_exp_details)
                  }
                  
                  MultData[[ff]][[j]] <- bind_rows(LiverDisc)
                  rm(LiverDisc)
                  
               } else {
                  
                  MultData[[ff]][[j]] <-
                     extractConcTime(
                        sim_data_file = ff,
                        obs_data_file = MyObsFile, 
                        compoundToExtract = compoundsToExtract_n,
                        tissue = j,
                        returnAggregateOrIndiv = returnAggregateOrIndiv, 
                        fromMultFunction = TRUE, 
                        existing_exp_details = existing_exp_details)
               }
            }
            
         } else if(TissueType == "systemic"){
            # If TissueType is systemic, then substrate and
            # inhibitor concs are on the same sheet, but metabolite
            # concs are elsewhere.
            CompoundTypes <-
               data.frame(PossCompounds = PossCmpd) %>%
               mutate(Type = case_when(
                  
                  PossCompounds %in%
                     c("substrate", "inhibitor 1",
                       "inhibitor 2",
                       "inhibitor 1 metabolite", 
                       AllCompounds %>% 
                          filter(CompoundType == "ADC" & 
                                    CompoundID != "released payload") %>% 
                          pull(CompoundID)) ~ "substrate",
                  
                  PossCompounds == "released payload" ~ "primary metabolite 1", 
                  
                  PossCompounds %in% c("pd input", "pd response") ~ "PD", 
                  
                  .default = PossCompounds)) %>%
               filter(PossCompounds %in% compoundsToExtract_n)
            
            MultData[[ff]][[j]] <- list()
            
            for(k in unique(CompoundTypes$Type)){
               
               # print(paste("CompoundTypes$Type k =", k))
               compoundsToExtract_k <-
                  CompoundTypes %>% filter(Type == k) %>%
                  pull(PossCompounds)
               
               if(str_detect(ff, "\\.db$")){
                  for(l in compoundsToExtract_k){
                     
                     MultData[[ff]][[j]][[k]][[l]] <-
                        extractConcTime_DB(
                           sim_data_file = ff,
                           # obs_data_file = MyObsFile, 
                           compoundToExtract = l,
                           tissue = j,
                           returnAggregateOrIndiv = returnAggregateOrIndiv, 
                           existing_exp_details = existing_exp_details) 
                  }
                  
                  MultData[[ff]][[j]][[k]] <- bind_rows(MultData[[ff]][[j]][[k]])
                  
               } else {
                  MultData[[ff]][[j]][[k]] <-
                     extractConcTime(
                        sim_data_file = ff,
                        obs_data_file = MyObsFile, 
                        compoundToExtract = compoundsToExtract_k,
                        tissue = j,
                        returnAggregateOrIndiv = returnAggregateOrIndiv, 
                        fromMultFunction = TRUE, 
                        existing_exp_details = existing_exp_details)
               }
               
               if(nrow(MultData[[ff]][[j]][[k]]) == 0){
                  warning(wrapn(paste0("No data could be found in the simulation '", 
                                       ff, 
                                       "' for the ", k, " (or the other compounds that are on that same tab) in ", 
                                       j, ".")), 
                          call. = FALSE)
               }
               
               rm(compoundsToExtract_k)
            }
            
            MultData[[ff]][[j]] <- bind_rows(MultData[[ff]][[j]])
            
         }
         
         # When the particular combination of compound and tissue is not
         # available in that file, extractConcTime will return an empty
         # data.frame, which we don't want to be included in the final data. Not
         # adding info for File in that scenario b/c it would add a row to what
         # would have been an empty data.frame.
         if(nrow(MultData[[ff]][[j]]) > 0){
            
            MultData[[ff]][[j]] <- MultData[[ff]][[j]] %>%
               mutate(File = ff)
            
            # Adjusting conc units as requested.
            
            # Adding some NA values to Deets as needed for convert_units to
            # work w/out generating a ton of warnings.
            MissingCols <- setdiff(paste0("MW", 
                                          c("_sub", "_met1", "_met2", "_secmet",
                                            "_inhib", "_inhib2", "_inhib1met")), 
                                   names(Deets))
            
            if(length(MissingCols) > 0){
               Deets <- Deets %>% 
                  bind_cols(as.data.frame(matrix(
                     data = NA, ncol = length(MissingCols), 
                     dimnames = list(NULL, MissingCols))))
            }
            
            # Need to handle ADAM, AdvBrainModel, and PD data differently b/c
            # different units
            SpecialTissue <- c("stomach", "duodenum", "jejunum I",
                               "jejunum II", "ileum I", "ileum II",
                               "ileum III", "ileum IV", "colon", "faeces", 
                               "gut tissue", "cumulative absorption", 
                               "cumulative fraction released",
                               "cumulative dissolution", 
                               "PD")
            
            SpecialSubsection <- c("undissolved compound", 
                                   "dissolution rate of solid state", 
                                   "free compound in lumen", 
                                   "total compound in lumen", 
                                   "Heff", 
                                   "absorption rate", 
                                   "unreleased compound in faeces", 
                                   "dissolved compound", 
                                   "luminal CLint", 
                                   "cumulative fraction of compound absorbed", 
                                   "cumulative fraction of compound dissolved", 
                                   "enterocyte concentration", 
                                   # Below are technically AdvBrainModel but
                                   # using SpecialTissue b/c that object name is
                                   # already set up. Note that this omits
                                   # AdvBrainModel tissues that just have mass
                                   # per volume units.
                                   "Kp,uu,brain", 
                                   "Kp,uu,ICF", 
                                   "Kp,uu,ISF")
            
            MultData[[ff]][[j]] <- MultData[[ff]][[j]] %>% 
               mutate(SpecialUnits = (Tissue %in% SpecialTissue & 
                                         CompoundID != "PD input") |
                         Tissue_subtype %in% SpecialSubsection)
            
            MultData[[ff]][[j]] <- split(MultData[[ff]][[j]], 
                                         MultData[[ff]][[j]]$SpecialUnits)
            
            if("FALSE" %in% names(MultData[[ff]][[j]]) &&
               nrow(MultData[[ff]][[j]][["FALSE"]]) > 0){
               MultData[[ff]][[j]][["FALSE"]] <- MultData[[ff]][[j]][["FALSE"]] %>% 
                  convert_units(
                     DF_with_good_units = NA, 
                     conc_units = conc_units_to_use,
                     time_units = time_units_to_use, 
                     MW = c("substrate" = Deets$MW_sub, 
                            "inhibitor 1" = Deets$MW_inhib,
                            "primary metabolite 1" = Deets$MW_met1, 
                            "primary metabolite 2" = Deets$MW_met2, 
                            "inhibitor 2" = Deets$MW_inhib2, 
                            "inhibitor 1 metabolite" = Deets$MW_inhib1met, 
                            "secondary metabolite" = Deets$MW_secmet, 
                            "conjugated payload" = as.numeric(Deets$MW_sub) + 
                               as.numeric(Deets$MW_met1), 
                            "total antibody" = Deets$MW_sub, 
                            "released payload" = Deets$MW_met1))
            }
            
            MultData[[ff]][[j]] <- bind_rows(MultData[[ff]][[j]])
            
         }
         
         MultData[[ff]][[j]] <- bind_rows(MultData[[ff]][[j]])
         
      }
      
      MultData[[ff]] <- bind_rows(MultData[[ff]])
      
      # MUST remove Deets or you can get the wrong info for each file!!!
      rm(Deets, CompoundCheck, compoundsToExtract_n) 
      
   }  
   
   MultData <- bind_rows(MultData)
   
   # toc(log = TRUE)
   
   # all data together -------------------------------------------------
   
   # tic(msg = "all data together - mult")
   
   # Dealing with custom dosing regimens
   if("Dose_sub" %in% names(ct_dataframe) &
      "Dose_sub" %in% names(MultData) &&
      (class(ct_dataframe$Dose_sub) == "character" |
       class(MultData$Dose_sub) == "character")){
      
      MultData$Dose_sub <- as.character(MultData$Dose_sub)
      ct_dataframe$Dose_sub <- as.character(ct_dataframe$Dose_sub)
      
   }
   
   if("Dose_inhib" %in% names(ct_dataframe) &
      "Dose_inhib" %in% names(MultData) &&
      (class(ct_dataframe$Dose_inhib) == "character" |
       class(MultData$Dose_inhib) == "character")){
      
      MultData$Dose_inhib <- as.character(MultData$Dose_inhib)
      ct_dataframe$Dose_inhib <- as.character(ct_dataframe$Dose_inhib)
      
   }
   
   if("Dose_inhib2" %in% names(ct_dataframe) &
      "Dose_inhib2" %in% names(MultData) &&
      (class(ct_dataframe$Dose_inhib2) == "character" |
       class(MultData$Dose_inhib2) == "character")){
      
      MultData$Dose_inhib2 <- as.character(MultData$Dose_inhib2)
      ct_dataframe$Dose_inhib2 <- as.character(ct_dataframe$Dose_inhib2)
      
   }
   
   ct_dataframe <- bind_rows(ct_dataframe, MultData) %>% 
      select(-any_of(c("ID", "Breaks"))) %>% 
      arrange(across(any_of(c("File", "Compound", "Inhibitor", "Simulated",
                              "Individual", "Trial", "Time")))) %>%
      select(any_of(c("Compound", "CompoundID", "Inhibitor", "Simulated",
                      "Species", "Tissue", "Individual", "Trial",
                      "Simulated", "IndivOrAgg", 
                      "Time", "Conc", "SD_SE",
                      "Time_units", "Conc_units", "Tissue_subtype", "DoseNum",
                      "DoseInt", "Dose_sub", "Dose_inhib", "Dose_inhib2", 
                      "File", "ObsFile", "subsection_ADAM"))) %>% 
      unique()
   
   # toc(log = TRUE)
   
   return(ct_dataframe)
   
}


