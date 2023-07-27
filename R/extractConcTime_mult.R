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
#'   for the argument \code{existing_exp_details} to work.}
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
#'   "Tissues" in the simulator. All possible options:\describe{
#'   \item{First-order absorption models}{"plasma", "blood", "unbound blood",
#'   "unbound plasma", "additional organ", "adipose", "bone", "brain",
#'   "feto-placenta", "GI tissue", "heart", "kidney", "liver", "lung", "muscle",
#'   "pancreas", "peripheral blood", "peripheral plasma", "peripheral unbound
#'   blood", "peripheral unbound plasma", "portal vein blood", "portal vein
#'   plasma", "portal vein unbound blood", "portal vein unbound plasma", "skin",
#'   or "spleen".} \item{ADAM-models}{"stomach", "duodenum", "jejunum I",
#'   "jejunum II", "ileum I", "ileum II", "ileum III", "ileum IV", "colon",
#'   "faeces", "gut tissue", "cumulative absorption", "cumulative fraction
#'   released", or "cumulative dissolution".}} Not case sensitive. Acceptable
#'   input is all tissues desired as a character vector, e.g., \code{tissues =
#'   c("plasma", "blood", "liver")}.
#' @param compoundsToExtract For which compound do you want to extract
#'   concentration-time data? Options are: \itemize{\item{"all" (default) for
#'   all the possible compounds in the simulation (substrate, metabolites,
#'   inhibitors, and ADC-related compounds)} \item{"substrate" (default),}
#'   \item{"primary metabolite 1",} \item{"primary metabolite 2",}
#'   \item{"secondary metabolite",} \item{"inhibitor 1" -- this can be an
#'   inducer, inhibitor, activator, or suppresesor, but it's labeled as
#'   "Inhibitor 1" in the simulator,} \item{"inhibitor 2" for the 2nd inhibitor
#'   listed in the simulation,} \item{"inhibitor 1 metabolite" for the primary
#'   metabolite of inhibitor 1,} \item{"conjugated protein" for DAR1-DARmax for
#'   an antibody-drug conjugate; observed data with DV listed as "Conjugated
#'   Protein Plasma Total" will match these simulated data,} \item{"total
#'   protein" for DAR0-DARmax for an ADC; observed data with DV listed as "Total
#'   Protein Conjugate Plasma Total" will match these simulated data, or}
#'   \item{"released payload" for the released drug from an ADC, which shows up
#'   as primary metabolite 1 in Simulator output files}} Input to this argument
#'   should be all desired compounds as a character vector, e.g.,
#'   \code{c("substrate", "primary metabolite 1")}. \strong{Note: If your
#'   compound is a therapeutic protein or ADC, we haven't tested this very
#'   thoroughly, so please be extra careful to check that you're getting the
#'   correct data.}
#' @param ... other arguments passed to the function
#'   \code{\link{extractConcTime}}
#' @param conc_units_to_use concentration units to use so that all data will be
#'   comparable. Options are the same as the ones in the Excel form for PE data
#'   entry. Default is "ng/mL". Note: ADAM model data concentration units are
#'   not converted because there are simply too many units to manage easily, so
#'   please check that the units are what you expected in the end.
#' @param time_units_to_use time units to use so that all data will be
#'   comparable. Options are "hours" (default), "days", or "minutes".
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
   
   compoundsToExtract <- tolower(compoundsToExtract)
   
   MainCompoundIDs <- c("substrate", "primary metabolite 1", "primary metabolite 2",
                        "secondary metabolite",
                        "inhibitor 1", "inhibitor 2", "inhibitor 1 metabolite",
                        "inhibitor 2 metabolite")
   
   ADCCompoundIDs <- c("total protein", "conjugated protein", 
                       "released payload")
   
   PossCmpd <- c(MainCompoundIDs, ADCCompoundIDs, "all")
   
   if(any(compoundsToExtract %in% PossCmpd == FALSE)){
      warning(paste0("The compound(s) ", 
                     str_comma(paste0("`", setdiff(compoundsToExtract, PossCmpd), "`")),
                     " is/are not among the possible componds to extract and will be ignored. The possible compounds to extract are only exactly these: ",
                     str_comma(paste0("`", PossCmpd, "`"))), 
              call. = FALSE)
      compoundsToExtract <- intersect(compoundsToExtract, PossCmpd)
   }
   
   if(any(complete.cases(obs_data_files))){
      if(any(complete.cases(obs_to_sim_assignment[1]))){
         warning("You specified values for both the `obs_to_sim_assignment` argument and for the soon-to-be-deprecated `obs_data_files` argument. Only the value for `obs_to_sim_assignment` will be used.", 
                 call. = FALSE)
      } else {
         warning("You used the soon-to-be-deprecated argument `obs_data_files` to indicate which observed files to use. Please use the argument `obs_to_sim_assignment` in the future. Please see the help file for more information.", 
                 call. = FALSE) 
         obs_to_sim_assignment <- obs_data_files
      }
   }
   
   # TO DO: Add option of extracting all possible tissues or all ADAM tissues.
   
   # Main body of function -----------------------------------------------
   
   sim_data_files <- unique(sim_data_files)
   
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
   
   # If user has not included "xlsx" in file name, add that.
   sim_data_files[str_detect(sim_data_files, "xlsx$") == FALSE] <-
      paste0(sim_data_files[str_detect(sim_data_files, "xlsx$") == FALSE], 
             ".xlsx")
   
   # Checking on what combinations of data the user has requested and what
   # data are already present in ct_dataframe.
   if(compoundsToExtract[1] == "all"){
      compoundsToExtract_orig <- "all"
      compoundsToExtract <- c(MainCompoundIDs, ADCCompoundIDs)
   } else {
      compoundsToExtract_orig <- compoundsToExtract
   }
   
   Requested <- expand.grid(Tissue = tissues,
                            CompoundID = compoundsToExtract,
                            File = sim_data_files) %>% 
      mutate(ID = paste(File, Tissue, CompoundID))
   
   # Checking for existing conc-time data
   if("logical" %in% class(ct_dataframe) == FALSE &&
      exists(substitute(ct_dataframe)) && 
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
   if(any(file.exists(sim_data_files_topull) == FALSE)){
      MissingSimFiles <- sim_data_files_topull[
         which(file.exists(sim_data_files_topull) == FALSE)]
      warning(paste0("The file(s) ", 
                     str_comma(paste0("`", MissingSimFiles, "`")), 
                     " is/are not present and thus will not be extracted."), 
              call. = FALSE)
      sim_data_files_topull <- setdiff(sim_data_files_topull, MissingSimFiles)
   }
   
   # Tidying and error catching for any observed data
   if("character" %in% class(obs_to_sim_assignment) && 
      obs_to_sim_assignment == "use existing_exp_details"){
      if("logical" %in% class(existing_exp_details)){
         warning("You requested that we match observed data to simulated data based on `existing_exp_details`, but you haven't supplied anything for `existing_exp_details`. We cannot extract any observed data.", 
                 call. = FALSE)
         ObsAssign <- list()
      } else {
         Deets <- switch(as.character("File" %in% names(existing_exp_details)), 
                         "TRUE" = existing_exp_details, 
                         "FALSE" = deannotateDetails(existing_exp_details))
         
         if("ObsOverlayFile" %in% names(Deets) == FALSE){
            warning("The observed data overlay file was not included in `existing_exp_details`, so we don't know which observed data files to use for the simulated files. We cannot extact any observed data.", 
                    call. = FALSE)
            ObsAssign <- list()
         } else {
            ObsAssign <- Deets %>% select(File, ObsOverlayFile) %>% 
               rename(ObsFile = ObsOverlayFile) %>% 
               mutate(ObsFile = sub("\\.xml$", ".xlsx", ObsFile), 
                      ObsFile = gsub("\\\\", "/", ObsFile), 
                      ObsFile = sub("Users/.*/Certara", 
                                    paste0("Users/", Sys.info()["user"], 
                                           "/Certara"), ObsFile))
            
            if(any(file.exists(ObsAssign$ObsFile) == FALSE)){
               warning(paste0("We couldn't find the following observed data Excel files and thus cannot extract their data:", 
                              str_c(ObsAssign$ObsFile[file.exists(ObsAssign$ObsFile) == FALSE], 
                                    collapse = "\n")), 
                       call. = FALSE)
            }
            
            ObsAssign <- ObsAssign %>% filter(file.exists(ObsFile))
            
            if(nrow(ObsAssign) == 0){
               warning("We can't find the Excel files that match the observed overlay files in your simulations. We cannot extract any observed data.", 
                       call. = FALSE)
               ObsAssign <- list()
            } else {
               ObsAssign <- split(ObsAssign, f = ObsAssign$File)
            }
         }
      }
   } else if("logical" %in% class(obs_to_sim_assignment)){
      # this is when the user has not specified anything for
      # obs_to_sim_assignment.
      ObsAssign <- list()
      
   } else {
      
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
               ObsAssign <- data.frame(ObsFile = obs_to_sim_assignment, 
                                       File = NA) %>% unique()
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
         warning("You have specified values for `obs_to_sim_assignment`, but it's not clear which should be for the observed files and which for the simulated files. Please check the help file for acceptable input. For now, we will not extract data from any observed data files.", 
                 call. = FALSE)
         ObsAssign <- NA
      } else {
         
         # Now that column names should be correct, converting to the case I like
         # for ease of coding
         ObsAssign <- ObsAssign %>% rename(File = FILE, ObsFile = OBSFILE) %>% 
            select(File, ObsFile)
         
         if(any(duplicated(ObsAssign$File))){ 
            Dups <- ObsAssign$File[duplicated(ObsAssign$File)]
            warning(paste0("You have more than one observed data file assigned to the simulator files ",
                           str_comma(paste0("`", Dups, "`")),
                           ". This function can only handle one observed file per simulator file, so only the first observed file listed will be used."),
                    call. = FALSE)
            ObsAssign <- ObsAssign[!duplicated(ObsAssign$File), ]
         }
         
         if(any(complete.cases(ObsAssign$File))){
            MissingFiles <- setdiff(ObsAssign$File,
                                    unique(c(sim_data_files, ct_dataframe$File)))
            if(length(MissingFiles) > 0){
               warning(paste0("When you assigned observed data files to simulator files with the argument `obs_to_sim_assignment`, you included simulator files that are *not* included in `sim_data_files`. We cannot include these observed data files in the output data because we don't know which simulator files they belong with. The problem simulator files is/are: ", 
                              str_comma(MissingFiles), ", which is/are set to match the following observed files ",
                              str_comma(names(obs_to_sim_assignment[
                                 which(obs_to_sim_assignment %in%
                                          unique(c(sim_data_files, ct_dataframe$File)) == FALSE)])), 
                              "."), 
                       call. = FALSE)
               
               ObsAssign <-
                  ObsAssign %>% 
                  filter(File %in% unique(c(sim_data_files, ct_dataframe$File)))
            }
            
            # Making sure obs files exist before trying to pull data from them
            if(any(file.exists(ObsAssign$ObsFile) == FALSE)){
               
               MissingObsFiles <- ObsAssign$ObsFile[
                  which(file.exists(ObsAssign$ObsFile) == FALSE)]
               warning(paste0("The file(s) ", 
                              str_comma(paste0("`", MissingObsFiles, "`")), 
                              " is/are not present and thus will not be extracted."), 
                       call. = FALSE)
               ObsAssign <- ObsAssign %>% filter(!ObsFile %in% MissingObsFiles)
            }
            
            ObsAssign <- split(ObsAssign, f = ObsAssign$File)
         } else {
            ObsAssign <- list() # This is for when all sim files should use the same obs file
         }
      } 
   }
   
   
   ## Start of loop through files ------------------------------------------
   MultData <- list()
   
   for(ff in sim_data_files_topull){
      message(paste("Extracting data from file =", ff))
      MultData[[ff]] <- list()
      
      # Getting summary data for the simulation(s)
      if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA
         Deets <- extractExpDetails(ff, exp_details = "Input Sheet")
      } else {
         Deets <- switch(as.character("File" %in% names(existing_exp_details)), 
                         "TRUE" = existing_exp_details, 
                         "FALSE" = deannotateDetails(existing_exp_details)) 
         
         if("data.frame" %in% class(Deets)){
            Deets <- Deets %>% filter(File == ff)
            
            if(nrow(Deets) == 0){
               Deets <- extractExpDetails(sim_data_file = ff, 
                                          exp_details = "Input Sheet")
            }
         }
      }
      
      if(length(Deets) == 0){
         # Using "warning" instead of "stop" here b/c I want this to be able to
         # pass through to other functions and just skip any files that
         # aren't simulator output.
         warning(paste("The file", ff,
                       "does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file."), 
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
                                                         Deets$SecondaryMetabolite, NA))
      
      if(Deets$ADCSimulation){
         CompoundCheck <- c(CompoundCheck, 
                            "conjugated protein" = "conjugated protein", 
                            "total protein" = "total protein", 
                            "released payload" = "released payload")
      }
      
      if(compoundsToExtract_orig[1] == "all"){
         compoundsToExtract_n <- names(CompoundCheck)[complete.cases(CompoundCheck)]
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
                        " was/were not available."),
                 call. = FALSE)
      }
      
      # Each tissue will be on its own sheet in the Excel file, so each
      # will need their own iterations of the loop for reading different
      # sheets.
      for(j in tissues){
         
         message(paste("Extracting data for tissue =", j))
         # Depending on both the tissue AND which compound the user
         # requests, that could be on multiple sheets or on a single
         # sheet. Figuring out which sheet to read.
         
         # Extracting tissue or plasma/blood data? Sheet format differs.
         TissueType <- ifelse(str_detect(j, "plasma|blood|portal|peripheral"),
                              "systemic", "tissue")
         
         # Checking whether an observed file can be included for this
         # iteration
         if(is.null(ObsAssign[[ff]])){
            MyObsFile <- NA
         } else {
            MyObsFile <- ObsAssign[[ff]]$ObsFile
         }
         
         if(TissueType == "tissue"){
            # If the tissue type is a solid tissue, then any
            # compound concentrations available will be on that
            # sheet and that requires only one iteration of the
            # loop.
            
            MultData[[ff]][[j]] <- extractConcTime(
               sim_data_file = ff,
               obs_data_file = MyObsFile,
               compoundToExtract = compoundsToExtract_n,
               tissue = j,
               returnAggregateOrIndiv = returnAggregateOrIndiv, 
               fromMultFunction = TRUE, 
               existing_exp_details = as.data.frame(Deets) %>% filter(File == ff))
            
            # When the particular combination of compound and tissue is not
            # available in that file, extractConcTime will return an empty
            # data.frame, which we don't want to be included in the final
            # data. Not adding info for File in that scenario b/c it would
            # add a row to what would have been an empty data.frame.
            if(nrow(MultData[[ff]][[j]]) > 0){
               MultData[[ff]][[j]] <-
                  MultData[[ff]][[j]] %>%
                  mutate(File = ff)
               
               # Need to handle ADAM data specially
               ADAMtissue <- c("stomach", "duodenum", "jejunum I",
                               "jejunum II", "ileum I", "ileum II",
                               "ileum III", "ileum IV", "colon", "faeces", 
                               "gut tissue", "cumulative absorption", 
                               "cumulative fraction released",
                               "cumulative dissolution")
               if(any(MultData[[ff]][[j]]$Tissue %in% ADAMtissue)){
                  CT_adam <- MultData[[ff]][[j]] %>% 
                     filter(Tissue %in% ADAMtissue)
                  
                  
                  CT_nonadam <- MultData[[ff]][[j]] %>% 
                     filter(Tissue %in% ADAMtissue == FALSE)
                  
                  if(nrow(CT_nonadam) > 0){
                     CT_nonadam <- CT_nonadam %>% 
                        match_units(DF_to_adjust = CT_nonadam,
                                    goodunits = list("Conc_units" = conc_units_to_use,
                                                     "Time_units" = time_units_to_use))
                  }
                  
                  MultData[[ff]][[j]] <- bind_rows(CT_adam, CT_nonadam)
                  
               } else {
                  
                  MultData[[ff]][[j]] <-
                     match_units(DF_to_adjust = MultData[[ff]][[j]],
                                 goodunits = list("Conc_units" = conc_units_to_use,
                                                  "Time_units" = time_units_to_use))
               }
            }
            
         } else {
            # If TissueType is systemic, then substrate and
            # inhibitor concs are on the same sheet, but metabolite
            # concs are elsewhere.
            CompoundTypes <-
               data.frame(PossCompounds = PossCmpd) %>%
               mutate(Type = ifelse(PossCompounds %in%
                                       c("substrate", "inhibitor 1",
                                         "inhibitor 2",
                                         "inhibitor 1 metabolite"),
                                    "substrate", PossCompounds)) %>%
               filter(PossCompounds %in% compoundsToExtract_n)
            
            MultData[[ff]][[j]] <- list()
            
            for(k in unique(CompoundTypes$Type)){
               
               # print(paste("CompoundTypes$Type k =", k))
               compoundsToExtract_k <-
                  CompoundTypes %>% filter(Type == k) %>%
                  pull(PossCompounds)
               
               MultData[[ff]][[j]][[k]] <-
                  extractConcTime(
                     sim_data_file = ff,
                     obs_data_file = MyObsFile,
                     compoundToExtract = compoundsToExtract_k,
                     tissue = j,
                     returnAggregateOrIndiv = returnAggregateOrIndiv, 
                     fromMultFunction = TRUE, 
                     existing_exp_details = Deets)
               
               # When the particular combination of compound and
               # tissue is not available in that file,
               # extractConcTime will return an empty data.frame,
               # which we don't want to be included in the final
               # data. Not adding info for File in that scenario
               # b/c it would add a row to what would have been
               # an empty data.frame.
               if(nrow(MultData[[ff]][[j]][[k]]) > 0){
                  MultData[[ff]][[j]][[k]] <-
                     MultData[[ff]][[j]][[k]] %>%
                     mutate(File = ff)
                  
                  MolWts <- c("substrate" = Deets$MW_sub, 
                              "primary metabolite 1" = Deets$MW_met1, 
                              "primary metabolite 2" = Deets$MW_met2,
                              "secondary metabolite" = Deets$MW_secmet,
                              "inhibitor 1"= Deets$MW_inhib,
                              "inhibitor 2" = Deets$MW_inhib2,
                              "inhibitor 1 metabolite" = Deets$MW_inhib1met)
                  
                  
                  MultData[[ff]][[j]][[k]] <-
                     match_units(DF_to_adjust = MultData[[ff]][[j]][[k]],
                                 goodunits = list("Conc_units" = conc_units_to_use,
                                                  "Time_units" = time_units_to_use), 
                                 MW = MolWts[complete.cases(MolWts)])
               }
               
               rm(compoundsToExtract_k)
            }
            
            MultData[[ff]][[j]] <- bind_rows(MultData[[ff]][[j]])
         }
      }
      
      MultData[[ff]] <- bind_rows(MultData[[ff]])
      
      # MUST remove Deets or you can get the wrong info for each file!!!
      rm(Deets, CompoundCheck, compoundsToExtract_n, MyObsFile) 
      
   }
   
   MultData <- bind_rows(MultData)
   # if(nrow(MultData) > 0 & complete.cases(obs_to_sim_assignment[1])){
   #     # If they specified observed data files, it's better to use those than
   #     # to use the data included with the simulation. There's more information
   #     # that way.
   #     MultData <- MultData %>% filter(Simulated == TRUE)
   # }
   
   # Observed data ------------------------------------------------------
   
   if((any(complete.cases(obs_to_sim_assignment)) & length(ObsAssign) == 0) |   # <--- scenario when one obs file should be used for all sim files
      ("ObsFile" %in% names(MultData) &&                                        # <--- scenario when some obs files were already extracted but some were not b/c they weren't assigned to a sim file
       length(setdiff(names(ObsAssign), unique(MultData$File))) > 0 &&
       any(complete.cases(obs_to_sim_assignment)))){
      MultObsData <- list()
      if(overwrite){
         ct_dataframe <- ct_dataframe %>% filter(!ObsFile %in%
                                                    bind_rows(ObsAssign) %>% 
                                                    pull(ObsFile) %>% 
                                                    unique())
         for(ff in names(ObsAssign)){
            message(paste("Extracting data from observed data file =", ff))
            MultObsData[[ff]] <- extractObsConcTime(ff)
         }
      } else {
         # If user wanted one obs file for all sim files, then ObsAssign
         # *was* a zero-length list. Changing that to work here.
         if(length(ObsAssign) == 0){
            ObsAssign <- data.frame(ObsFile = obs_to_sim_assignment)
         }
         
         for(ff in setdiff(bind_rows(ObsAssign) %>% pull(ObsFile),
                           unique(MultData$ObsFile))){
            MultObsData[[ff]] <- extractObsConcTime(ff)
         }
      }
      
      # Removing any compounds not included in user request. If user requested
      # "all" for compoundsToExtract, need to change that to a character
      # vector from here on in the function.
      if(compoundsToExtract_orig[1] == "all"){
         compoundsToExtract <- c("substrate", "inhibitor 1",
                                 "inhibitor 2", "inhibitor 1 metabolite",
                                 "primary metabolite 1",
                                 "primary metabolite 2",
                                 "secondary metabolite", "UNKNOWN")
      }
      
      # At this point, if the observed data were already present in
      # ct_dataframe, MultObsData will be a list of length 0 and we don't need
      # to pull any more information for the observed data.
      if(length(MultObsData) > 0){
         MultObsData <- bind_rows(MultObsData) %>% 
            mutate(Simulated = FALSE) %>% 
            filter(CompoundID %in% compoundsToExtract)
         
         # We can we add a dose number and/or dose interval to these data if the
         # compound IDs match the simulated data. We're going to assume that the
         # dose timings are the same as in the simulated data.
         ObsCompoundID <- MultObsData %>% pull(CompoundID) %>% unique()
         SimDoseInfo <- MultData %>%
            filter(CompoundID %in% ObsCompoundID) %>% 
            group_by(CompoundID, Inhibitor, DoseInt, DoseNum) %>% 
            summarize(TimeRounded = round(min(Time)))
         
         # If there is only "InhbitorX" and "none" in the column "Inhibitor" for
         # the simulated data and there's only "inhibitor 1" and no other
         # effectors, then it's safe to assume that "inhibitor 1" should be
         # labeled as "InhibitorX" in the observed data, too.
         SimEffector <- unique(MultData$Inhibitor)
         SimEffector <- SimEffector[!SimEffector == "none"]
         if(length(SimEffector) == 1 &&
            "inhibitor 1" %in% MultData$CompoundID &&
            any(c("inhibitor 2", "inhibitor 1 metabolite") %in% 
                MultData$CompoundID) == FALSE){
            MultObsData$Inhibitor[MultObsData$Inhibitor != "none"] <- SimEffector
         }
         
         # Similarly, if there's only one value for Compound for each CompoundID,
         # then assigning that value to the observed data.
         CompoundCheck <- MultData %>% group_by(CompoundID) %>% 
            summarize(OneCompound = length(unique(Compound)) == 1) %>% 
            filter(OneCompound == TRUE) %>% pull(CompoundID)
         
         if(length(CompoundCheck) > 0){
            MultObsData <- MultObsData %>% 
               left_join(MultData %>% filter(CompoundID %in% CompoundCheck) %>% 
                            select(Compound, CompoundID) %>% unique(), 
                         by = "CompoundID")
         }
         
         # If there are both SD and MD data for a given CompoundID, which is the
         # case when DoseInt is sometimes NA and sometimes has a value, then give
         # user a warning about that but don't try to assign dose numbers to obs
         # data since we have no way of knowing which is which.
         MultRegimenCheck <- SimDoseInfo %>% group_by(CompoundID) %>% 
            summarize(MultRegimens = length(unique(DoseInt)) > 1) %>% 
            filter(MultRegimens == TRUE)
         
         if(nrow(MultRegimenCheck) > 0){
            warning(paste0("It looks like you have both single-dose and multiple-dose simulated data present for the compound(s) ",
                           str_comma(MultRegimenCheck$CompoundID), 
                           ". We thus cannot safely assign the observed data for that/those compound(s) to any particular dose number since we don't know which simulated files the observed data match. Output will include both simulated and observed data, but the observed data will have NA values for DoseInt and DoseNum for those compounds."),
                    call. = FALSE)
         } else {
            SimDoseInfo_list <- 
               split(SimDoseInfo %>%
                        filter(CompoundID %in% MultRegimenCheck$CompoundID == FALSE),
                     f = list(SimDoseInfo$CompoundID, 
                              SimDoseInfo$Inhibitor))
            SimDoseInfo_list <- SimDoseInfo_list[which(
               sapply(SimDoseInfo_list, FUN = nrow) > 0)]
            MultObsData <- split(MultObsData, 
                                 f = list(MultObsData$CompoundID, 
                                          MultObsData$Inhibitor))
            # Only include MultObsData if there was >= 1 row
            MultObsData <- MultObsData[which(sapply(MultObsData, nrow) > 0)]
            
            # We have now figured out for the observed data what the
            # CompoundID(s) was/were and whether there was an inhibitor present.
            # Next, we will match the observed data for that compound ID and
            # inhibitor with the simulated data for that compound ID and
            # inhibitor.
            for(i in intersect(names(SimDoseInfo_list), names(MultObsData))){
               
               if(all(is.na(SimDoseInfo_list[[i]]$DoseInt)) &&
                  nrow(SimDoseInfo_list[[i]]) == 1){
                  # This is when it was a single dose and there was only
                  # one dosing time: t = 0. If there's more than 1 row
                  # here for SimDoseInfo_list[[i]], then everything was
                  # single dose but dosing started at different times. Not
                  # setting DoseNum here then b/c that's tricky to figure
                  # out which start time matches which observed file and
                  # not sure it's worth the time trying to figure it out.
                  # For the other scenario, when there's only one start
                  # time, setting dose number to 1 for any times after
                  # dose administration and to 0 for any time before then.
                  DoseTime <- SimDoseInfo_list[[i]]$TimeRounded
                  MultObsData[[i]] <- MultObsData[[i]] %>% 
                     mutate(DoseNum = ifelse(Time >= {{DoseTime}}, 1, 0))
                  rm(DoseTime)
                  
               } else {
                  
                  NumScenarios <- SimDoseInfo_list[[i]] %>% group_by(TimeRounded) %>% 
                     summarize(NumScenarios = n()) %>% pull(NumScenarios)
                  
                  if(all(NumScenarios == 1)){
                     # This is the scenario when there were multiple
                     # doses. The dosing interval doesn't have to be the
                     # same each time (ok if it's custom dosing), but
                     # there must be only one value for TimeRounded for
                     # each DoseNum for us to be able to assign which
                     # observed data went with which dose number.
                     
                     Check <- SimDoseInfo_list[[i]] %>% group_by(DoseNum) %>% 
                        summarise(SingleDoseTime = n() == 1)
                     
                     if(all(Check$SingleDoseTime)){
                        SimDoseInfo_list[[i]] <- SimDoseInfo_list[[i]] %>% 
                           ungroup() %>% 
                           mutate(Breaks = as.character(
                              cut(TimeRounded, breaks = TimeRounded, right = FALSE)))
                        
                        MultObsData[[i]] <- MultObsData[[i]] %>% 
                           mutate(TimeRounded = round(Time),
                                  Breaks = as.character(
                                     cut(Time, breaks = SimDoseInfo_list[[i]]$TimeRounded, 
                                         right = FALSE))) %>% 
                           left_join(SimDoseInfo_list[[i]] %>% select(Breaks, DoseInt, DoseNum), 
                                     by = c("Breaks"))
                        
                        # Checking for when the simulation ends right at the last dose
                        # b/c then, setting that number to 1 dose lower
                        if(length(MultObsData[[i]] %>%
                                  filter(DoseNum == max(MultObsData[[i]]$DoseNum)) %>%
                                  pull(Time) %>% unique()) == 1){
                           MyMaxDoseNum <- max(MultObsData[[i]]$DoseNum)
                           MultObsData[[i]] <- MultObsData[[i]] %>%
                              mutate(DoseNum = ifelse(DoseNum == MyMaxDoseNum,
                                                      MyMaxDoseNum - 1, DoseNum))
                        }
                     }
                  }
               }
            }
            
            MultObsData <- bind_rows(MultObsData)
            MultObsData <- match_units(DF_to_adjust = MultObsData,
                                       goodunits = list("Conc_units" = conc_units_to_use,
                                                        "Time_units" = time_units_to_use))
         }
         
      }
      
      # If there were only one observed file and it wasn't named with a
      # specific simulated file, then it presumably should go with ALL the sim
      # files that we just extracted, so setting that file for "ObsFile" in
      # the simulated data.
      if(length(obs_to_sim_assignment) == 1 &
         is.null(names(obs_to_sim_assignment))){
         MultData$ObsFile <- obs_to_sim_assignment
         
         # For the simulated files we just added, adding one copy of observed
         # data per File that doesn't already have it. Note that this only
         # happens if there was only one file listed for
         # obs_to_sim_assignment. 
         MultObsData <- MultData %>% select(File, ObsFile) %>% unique() %>% 
            left_join(MultObsData, by = join_by(ObsFile),
                      multiple = "all")
         
         # MultObsData <- calc_dosenumber(MultObsData, 
         #                                existing_exp_details = Deets)
         
      }
      
      ct_dataframe <- bind_rows(ct_dataframe, MultObsData)
      
   }
   
   # all data together -------------------------------------------------
   
   ct_dataframe <- bind_rows(ct_dataframe, MultData) %>% 
      select(-any_of(c("ID", "Breaks"))) %>% 
      arrange(across(any_of(c("File", "Compound", "Inhibitor", "Simulated",
                              "Individual", "Trial", "Time")))) %>%
      select(any_of(c("Compound", "CompoundID", "Inhibitor", "Simulated",
                      "Species", "Tissue", "Individual", "Trial",
                      "Simulated", "Time", "Conc",
                      "Time_units", "Conc_units", "subsection_ADAM", "DoseNum",
                      "DoseInt", "File", "ObsFile"))) %>% 
      unique()
   
   return(ct_dataframe)
   
}


