#' INTERNAL - Figure out observed CT files to user for extractConcTime
#'
#' @param sim_data_files sim_data_files
#' @param obs_to_sim_assignment obs_to_sim_assignment or obs_data_file 
#' @param existing_exp_details existing_exp_details
#'
#' @return data.frame
#'
#' @examples
#' # none
get_obs_assignments <- function(sim_data_files, 
                                obs_to_sim_assignment = NA, 
                                existing_exp_details = NA){
   
   if("character" %in% class(obs_to_sim_assignment) && 
      any(str_detect(tolower(obs_to_sim_assignment), "use existing|use.*details"))){
      
      ### SCENARIO A: Match w/expdetails -------------------------------------
      
      if("logical" %in% class(existing_exp_details)){
         warning("You requested that we match observed data to simulated data based on `existing_exp_details`, but you haven't supplied anything for `existing_exp_details`. We cannot extract any observed data.\n", 
                 call. = FALSE)
         ObsAssign <- data.frame()
         obs_to_sim_assignment <- NA
         
      } else {
         
         existing_exp_details <- filter_sims(existing_exp_details, 
                                             sim_data_files, "include")
         existing_exp_details <- harmonize_details(existing_exp_details)
         
         if(all(sim_data_files %in% existing_exp_details$MainDetails$File) == FALSE){
            existing_exp_details <- extractExpDetails_mult(sim_data_files = sim_data_files, 
                                                           existing_exp_details = existing_exp_details,
                                                           exp_details = "Summary and Input")
         }
         
         if("ObsOverlayFile" %in% names(existing_exp_details$MainDetails) == FALSE){
            warning("The observed data overlay file was not included in `existing_exp_details`, so we don't know which observed data files to use for the simulated files. We cannot extract any observed data.\n", 
                    call. = FALSE)
            ObsAssign <- data.frame()
            obs_to_sim_assignment <- NA
         } else {
            
            # Make this work for whoever the current user is, even if the XML
            # obs file path was for someone else.
            existing_exp_details$MainDetails$ObsOverlayFile <- 
               sub("Users\\\\.*\\\\Certara", 
                   paste0("Users\\\\", Sys.info()["user"], "\\\\Certara"), 
                   existing_exp_details$MainDetails$ObsOverlayFile)
            
            ObsAssign <- existing_exp_details$MainDetails %>% 
               select(File, ObsOverlayFile) %>% 
               rename(ObsFile = ObsOverlayFile) %>% 
               mutate(ObsFile = sub("\\.xml$", ".xlsx", ObsFile), 
                      ObsFile = gsub("\\\\", "/", ObsFile), 
                      ObsFile = sub("Users/.*/Certara", 
                                    paste0("Users/", Sys.info()["user"], 
                                           "/Certara"), ObsFile))
            
            if(nrow(ObsAssign %>% filter(complete.cases(ObsFile) & 
                                         File %in% sim_data_files)) == 0){
               ObsAssign <- data.frame()
               obs_to_sim_assignment <- NA
            } else {
               if(any(file.exists(ObsAssign$ObsFile[
                  complete.cases(ObsAssign$ObsFile) &
                  ObsAssign$File %in% sim_data_files]) == FALSE)){
                  warning(paste0("We couldn't find the following observed data Excel files and thus cannot extract their data:", 
                                 str_c(ObsAssign$ObsFile[
                                    file.exists(ObsAssign$ObsFile[
                                       complete.cases(ObsAssign$ObsFile) &
                                          ObsAssign$File %in% sim_data_files]) == FALSE], 
                                    collapse = "\n"), "\n"), 
                          call. = FALSE)
               }
               
               ObsAssign <- ObsAssign %>% filter(file.exists(ObsFile) & 
                                                    File %in% sim_data_files)
               
               if(nrow(ObsAssign) == 0){
                  warning("We can't find the Excel files that match the observed overlay files in your simulations. We cannot extract any observed data.\n", 
                          call. = FALSE)
                  ObsAssign <- data.frame()
                  obs_to_sim_assignment <- NA
               } 
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
            Dups <- ObsAssign$File[duplicated(ObsAssign$File)]
            warning(paste0("You have more than one observed data file assigned to the simulator files ",
                           str_comma(paste0("`", Dups, "`")),
                           ". This function can only handle one observed file per simulator file, so only the first observed file listed will be used.\n"),
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
                              ".\n"), 
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
                              str_comma(paste0("`", unique(MissingObsFiles), "`")), 
                              " is/are not present and thus will not be extracted.\n"), 
                       call. = FALSE)
               ObsAssign <- ObsAssign %>% filter(!ObsFile %in% MissingObsFiles)
            }
            
         }
      } 
   }
   
   # End of error catching for obs_to_sim_assignment. ObsAssign should now be a
   # data.frame with columns File and ObsFile or else a completely empty
   # data.frame.
   if(length(ObsAssign) > 0){
      ObsAssign <- ObsAssign %>% filter(complete.cases(File) &
                                           complete.cases(ObsFile) &
                                           File %in% sim_data_files) 
   }
   
   return(ObsAssign)
   
}


