#' Change a limited set of trial-design parameters in Simcyp Simulator workspace
#' files. UNDER CONSTRUCTION.
#'
#' \code{change_wksz_trial_design} changes a few trial-design parameters such as
#' the number of subjects, the percent female, the dose or dose interval, etc.
#' It does \emph{not} change populations because there are simply too many
#' parameters to change in an automatic fashion in R like this and also because
#' the Simulator starting with V22 can change populations for you with its
#' workflow function. It also doesn't turn on or off a fixed trial design
#' because we just haven't figured out how to get that to work right. This will
#' change \emph{all} the workspace files provided to have the \emph{same}
#' trial-design parameters you list here. USE WITH CAUTION. THIS PERMANENTLY
#' CHANGES WORKSPACES.
#'
#' @param sim_workspace_files the set of workspace files to modify; must end in
#'   ".wksz" if you're specifying individual files. Leave as NA to change all
#'   the workspaces in the current folder or set to "recursive" to change all
#'   the workspaces in the current folder and \emph{all} the subfolders below
#'   it. BE CAREFUL. This function changes workspaces, so please be certain
#'   you're making the changes you want. We recommend keeping a backup copy of
#'   the original workspaces until you're sure the new ones are set up how you
#'   want.
#' @param new_sim_workspace_files optionally specify the new workspace file
#'   names to use. If left as NA, the original workspace will be overwritten.
#'   Otherwise, specify a character vector of file names to use, e.g.,
#'   \code{new_sim_workspace_files = c("new file 1.wksz", "new file
#'   2.wksz")}
#' @param fix_xml_paths TRUE (default) or FALSE to automatically fix any
#'   discrepancies in user name on the SharePoint folder path for observed
#'   overlay data files or fixed trial design files. This can be useful if, say,
#'   someone else ran the development and verification simulations and you're
#'   now running the application simulations.
#' @param trial_design_parameters_to_set optionally specify a data.frame of
#'   trial design parameters to set. Acceptable column names are columns named
#'   \emph{exactly} like the subsequent argument names here and also must
#'   include the columns "sim_workspace_files" and "new_sim_workspace_files".
#' @param NumTrials number of trials to use. If you set this you MUST also set
#'   the number of subjects per trial.
#' @param NumSubjTrial number of subjects per trial to use. If you set this you
#'   MUST also set the number of trials.
#' @param PercFemale percent of subjects who are female
#' @param Age_min minimum age
#' @param Age_max maximum age
#' @param SimDuration study duration in hours
#' @param activate_inhibitor1 TRUE or FALSE for whether to turn on inhibitor 1.
#'   (At this point, this function will only use parameters for substrate or
#'   inhibitor 1.)
#' @param Dose_sub dose of substrate to use (mg)
#' @param Dose_inhib dose of inhibitor 1 to use (mg)
#' @param DoseInt_sub dose interval for the substrate in hours. If you would
#'   like to have a single-dose regimen, set this to "single dose". Does not
#'   allow for custom-dosing regimens.
#' @param DoseInt_inhib dose interval for inhibitor 1 in hours. If you would
#'   like to have a single-dose regimen, set this to "single dose". Does not
#'   allow for custom-dosing regimens.
#' @param StartHr_sub start time for administering substrate in hours -- This
#'   may be tricky to do! May need to also adjust StartDayTime_sub!
#' @param StartHr_inhib start time for administering inhibitor 1 in hours
#' @param NumTimePts number of time points to use ("Number of Time Samples" in
#'   the Simulator)
#' @param DoseRoute_sub Dose route for substrate
#' @param DoseRoute_inhib Dose route for inhibitor 1
#' @param ObsOverlayFile Observed overlay XML file name
#' @param UseObservedData Include the observed data in the results? TRUE or
#'   FALSE
#' @param Units_dose_sub type of substrate dose administered. Options: "mg",
#'   "mg/kg".
#' @param Units_dose_inhib type of inhibitor 1 dose administered. Options: "mg",
#'   "mg/kg".
#' @param CYP3A4_ontogeny_profile CYP3A4 ontogeny profile to use? Options are
#'   "1" for profile 1 (default in the Simulator) and "2" for profile 2 (based
#'   on Upreti Wahlstrom 2016 J Clin Pharm).
#'
#' @return does not return anything in R but saves workspace files
#' @export
#'
#' @examples
#' # None yet
#' 
change_wksz_trial_design <- function(sim_workspace_files = NA,
                                     new_sim_workspace_files = NA,
                                     fix_xml_paths = TRUE,
                                     trial_design_parameters_to_set = NA,
                                     NumTrials = "no change", 
                                     NumSubjTrial = "no change", 
                                     PercFemale = "no change", 
                                     Age_min = "no change", 
                                     Age_max = "no change", 
                                     SimDuration = "no change", 
                                     activate_inhibitor1 = "no change",
                                     Dose_sub = "no change", 
                                     Units_dose_sub = "no change", 
                                     DoseRoute_sub = "no change",
                                     Dose_inhib = "no change",
                                     Units_dose_inhib = "no change", 
                                     DoseRoute_inhib = "no change",
                                     DoseInt_sub = "no change", 
                                     DoseInt_inhib = "no change", 
                                     StartHr_sub = "no change", 
                                     StartHr_inhib = "no change", 
                                     NumTimePts = "no change", 
                                     ObsOverlayFile = "no change", 
                                     UseObservedData = "no change",
                                     CYP3A4_ontogeny_profile = "no change"){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If they left sim_workspace_files as NA and did not supply something for
   # that in trial_design_parameters_to_set, then they want to apply this function to all
   # the workspaces in the current folder. Getting the names of the workspaces.
   if("logical" %in% class(trial_design_parameters_to_set) | 
      ("data.frame" %in% class(trial_design_parameters_to_set) && 
       "sim_workspace_files" %in% names(trial_design_parameters_to_set) == FALSE)){
      sim_workspace_files <- sim_workspace_files[complete.cases(sim_workspace_files)]
      if(length(sim_workspace_files) == 0){
         sim_workspace_files <- list.files(pattern = ".wksz")
      } else if(any(sim_workspace_files == "recursive")){
         sim_workspace_files <- list.files(pattern = ".wksz", recursive = TRUE)
      }
      
      # If they did not provide a value for new_sim_workspace_files, then they
      # must want the orginal file names to be overwritten.
      if(all(is.na(new_sim_workspace_files))){
         new_sim_workspace_files <- sim_workspace_files
      }
      
   } else {
      # If they didn't provide sim_workspace_files separately but it's in
      # trial_design_parameters_to_set, then use that.
      if(length(sim_workspace_files) == 1 && is.na(sim_workspace_files) &
         "sim_workspace_files" %in% names(trial_design_parameters_to_set)){
         sim_workspace_files <- trial_design_parameters_to_set$sim_workspace_files
      }
   }
   
   if(length(sim_workspace_files) == 0){
      stop("No workspace files could be found to change.")
   }
   
   # Checking for mismatches between new and old file name lengths
   if(any(complete.cases(new_sim_workspace_files)) &&
      (length(new_sim_workspace_files) != length(sim_workspace_files))){
      stop(paste("You have provided", length(sim_workspace_files), "original workspace file names and",
                 length(new_sim_workspace_files),
                 "new workspace file names. You must provide the same number of original and new file names for this function to work or else list NA for `new_sim_workspace_files`, in which case the original workspaces will be overwritten with the new parameters."),
           call. = FALSE)
   }
   
   if("logical" %in% class(trial_design_parameters_to_set) == FALSE ){
      if("data.frame" %in% class(trial_design_parameters_to_set) == FALSE){
         stop("You have supplied an object for `trial_design_parameters_to_set`, but it's not a data.frame, which is what it needs to be. Please check your input and the help file and try again.", 
              call. = FALSE)
      }
      
      Changes <- trial_design_parameters_to_set %>% 
         select(any_of(c("sim_workspace_files", 
                         "new_sim_workspace_files",
                         "NumTrials",
                         "NumSubjTrial", 
                         "PercFemale",
                         "Age_min",
                         "Age_max",
                         "SimDuration", 
                         "activate_inhibitor1",
                         "Dose_sub", 
                         "Units_dose_sub",
                         "DoseRoute_sub",
                         "Dose_inhib",
                         "Units_dose_inhib",
                         "DoseRoute_inhib",
                         "DoseInt_sub", 
                         "DoseInt_inhib",
                         "StartHr_sub",
                         "StartHr_inhib",
                         "NumTimePts", 
                         "ObsOverlayFile",
                         "UseObservedData", 
                         "CYP3A4_ontogeny_profile")))
      
   } else {
      
      # Collecting changes to make into a data.frame if it's not already.
      Changes <- list(sim_workspace_files = sim_workspace_files,
                      new_sim_workspace_files = new_sim_workspace_files,
                      NumTrials = NumTrials, 
                      NumSubjTrial = NumSubjTrial, 
                      PercFemale = PercFemale, 
                      Age_min = Age_min, 
                      Age_max = Age_max, 
                      SimDuration = SimDuration, 
                      activate_inhibitor1 = activate_inhibitor1,
                      Dose_sub = Dose_sub,
                      Units_dose_sub = Units_dose_sub,
                      DoseRoute_sub = DoseRoute_sub,
                      Dose_inhib = Dose_inhib,
                      Units_dose_inhib = Units_dose_inhib, 
                      DoseRoute_inhib = DoseRoute_inhib,
                      DoseInt_sub = DoseInt_sub, 
                      DoseInt_inhib = DoseInt_inhib, 
                      StartHr_sub = StartHr_sub, 
                      StartHr_inhib = StartHr_inhib, 
                      NumTimePts = NumTimePts, 
                      ObsOverlayFile = ObsOverlayFile, 
                      CYP3A4_ontogeny_profile = CYP3A4_ontogeny_profile)
      
      # Checking lengths of arguments
      ChangeLength <- unlist(lapply(Changes, length))
      ChangeLength <- ChangeLength[!ChangeLength == 1]
      
      if(length(unique(ChangeLength)) > 1){
         stop("You have not provided the same number of inputs for each of the arguments from `NumTrials` onward, and all of these must have the same number of inputs or have just a single value, which will be repeated as needed. Please check this and try again.", 
              call. = FALSE)
      }
      
      Changes <- as.data.frame(Changes)
   }
   
   # FIXME - Will need to think about how best to add file names to Changes.
   # # Adding the file names to Changes if they're not already present.
   # if("sim_workspace_files" %in% names(Changes) == FALSE){
   #    # Assuming that, if there are multiple rows in Changes, user wants all of
   #    # those changes applied to all possible workspace files.
   #    Changes <- left_join(Changes, 
   #                         expand_grid(sim_workspace_files = sim_workspace_files, 
   #                                     enzymes = enzymes), 
   #                         by = "enzymes")
   # }
   
   if("new_sim_workspace_files" %in% names(Changes) == FALSE){
      Changes <- Changes %>% 
         left_join(data.frame(sim_workspace_files = sim_workspace_files, 
                              new_sim_workspace_files = new_sim_workspace_files), 
                   by = "sim_workspace_files")
   }
   
   # At this point, regardless of whether they provided a data.frame to
   # trial_design_parameters_to_set or filled out the arguments individually, we
   # should have a data.frame called Changes that includes all interactions AND
   # the original and revised file names.
   
   if(("NumSubjTrial" %in% names(Changes) & 
       "NumTrials" %in% names(Changes) == FALSE)){
      stop("You have specified a value for NumSubjTrial but not for NumTrials, and we need both of those to be set.", 
           call. = FALSE)
   }
   
   if(("NumTrials" %in% names(Changes) & 
       "NumSubjTrial" %in% names(Changes) == FALSE)){
      stop("You have specified a value for NumTrials but not for NumSubjTrial, and we need both of those to be set.", 
           call. = FALSE)
   }
   
   # If they've set the number of trials or number of subjects per trial, we
   # also need to set the number in the population.
   if("NumSubjTrial" %in% names(Changes)){
      suppressWarnings(
         Changes <- Changes %>% 
            mutate(PopSize = case_when(NumTrials != "no change" & 
                                          NumSubjTrial != "no change" ~ 
                                          as.character(as.numeric(NumTrials) * 
                                                          as.numeric(NumSubjTrial)), 
                                       TRUE ~ "no change"))
      )
   }
   
   # Main body of function ---------------------------------------------------
   
   # If they didn't include the file suffix, add that. Replace any "xlsx" file
   # extensions with "wksz". Clean up "Value" column and set Level5 tag names.
   Changes <- Changes %>% 
      mutate(sim_workspace_files = paste0(sub("\\.xlsx|\\.wksz", "", 
                                              sim_workspace_files), ".wksz"), 
             new_sim_workspace_files = paste0(sub("\\.xlsx|\\.wksz", "", 
                                                  new_sim_workspace_files), ".wksz"), 
             ObsOverlayFile = gsub("/", "\\\\", ObsOverlayFile), 
             across(.cols = everything(), .fns = as.character)) %>% 
      pivot_longer(cols = -c(sim_workspace_files, new_sim_workspace_files), 
                   names_to = "Detail", 
                   values_to = "Value") %>% 
      filter(Value != "no change") %>% 
      mutate(CompoundID = str_extract(Detail, "_sub|_inhib$")) %>%
      # Setting tag names
      left_join(AllExpDetails %>% filter(DataSource == "workspace or database") %>% 
                   select(Detail, Level1, Level2, Level3, Level4, Level5, XMLswitch), 
                by = "Detail") %>% 
      mutate(Level2 = case_when(CompoundID == "_sub" & Level2 == "CompoundIDNum" ~ "1", 
                                CompoundID == "_inhib$" & Level2 == "CompoundIDNum" ~ "2", 
                                TRUE ~ Level2))
   
   # Always need to have levels 1-5 columns, even if they aren't important for
   # these changes.
   if("Level3" %in% names(Changes) == FALSE){Changes$Level3 <- NA}
   if("Level4" %in% names(Changes) == FALSE){Changes$Level4 <- NA}
   if("Level5" %in% names(Changes) == FALSE){Changes$Level5 <- NA}
   
   if(any(is.na(Changes$Value[Changes$Detail == "PopSize"]))){
      stop("There's something wrong with your input for either NumTrials or NumSubjTrial because we're getting a missing value for the total population for at least one of your simulations. You must supply a number for both NumTrials and NumSubjTrial", 
           call. = FALSE)
   }
   
   # FIXME Will need to do something to check for bad inputs. This won't work.
   # # Check for bad inputs. 
   # InputCheck <- Changes %>% 
   #    mutate(ValNum = suppressWarnings(as.numeric(Value)), 
   #           Value = ifelse(Value == "NA", NA, Value),
   #           ValCheck = complete.cases(ValNum) |
   #              (is.na(Value) & is.na(ValNum)) |
   #              Value %in% c("no change", "true", "false", "on", "off"))
   # 
   # if(all(InputCheck$ValCheck) == FALSE){
   #    stop(paste0("There is a problem with some of your input. Depending on the parameter, acceptable values are numbers, NA, `no change`, `on`, or `off`. Please check your input for the following parameters:\n",
   #                str_c(InputCheck$Detail[InputCheck$ValCheck == FALSE], collapse = "\n")), 
   #         call. = FALSE)
   # }
   
   # Dealing with parameters where the human-readable input value from the user
   # needs to be coded to the correct value for the XML file.
   Changes <- Changes %>% 
      mutate(
         # Setting the dose units as needed
         Value = case_when(
            Detail %in% c("Units_dose_sub", "Units_dose_inhib") & 
               Value == "mg" ~ "1", 
            Detail %in% c("Units_dose_sub", "Units_dose_inhib") & 
               Value == "mg/kg" ~ "2", 
            # Setting the CYP3A4 ontogeny profile to the correct numbers
            Detail == "CYP3A4_ontogeny_profile" &
               Value == "1" ~ "0", 
            Detail == "CYP3A4_ontogeny_profile" &
               Value == "2" ~ "1", 
            TRUE ~ Value))
   
   # Grouping by new workspace
   Changes <- split(Changes, f = Changes$new_sim_workspace_files)
   
   for(i in names(Changes)){
      
      if(length(unique(Changes[[i]]$sim_workspace_files)) != 1){
         stop(paste0("You have more than one original workspace file for the new workspace file `", 
                     i, "`. We don't know which original workspace values you want to use in the new workspace file. Please check your input and the help file and try again with only one original workspace for any new workspace file."), 
              call. = FALSE)
      }
      
      workspace_xml <- XML::xmlTreeParse(unique(Changes[[i]]$sim_workspace_files),
                                         useInternal = TRUE)
      RootNode <- XML::xmlRoot(workspace_xml)
      
      # Setting the number of doses as needed. 
      if(any(str_detect(Changes[[i]]$Detail, "DoseInt"))){
         NumDosesNeeded_sub <- Changes[[i]] %>% filter(DetailOrig == "DoseInt_sub")
         if(nrow(NumDosesNeeded_sub) > 0){
            NumDosesNeeded_sub <- ifelse(NumDosesNeeded_sub$Value == "single dose", 
                                         "1", NumDosesNeeded_sub$Value)
            Changes[[i]] <- bind_rows(
               Changes[[i]], 
               data.frame(
                  sim_workspace_files = unique(Changes[[i]]$sim_workspace_files), 
                  new_sim_workspace_files = unique(Changes[[i]]$new_sim_workspace_files), 
                  Detail = "NumDoses_sub",
                  Value = NumDosesNeeded_sub, 
                  Level1 = "SimulationData", 
                  Level2 = "idNumberDoses1"))
         }
         
         NumDosesNeeded_inhib <- Changes[[i]] %>% filter(DetailOrig == "DoseInt_inhib")
         if(nrow(NumDosesNeeded_inhib) > 0){
            NumDosesNeeded_inhib <- ifelse(NumDosesNeeded_inhib$Value == "single dose", 
                                           "1", NumDosesNeeded_inhib$Value)
            Changes[[i]] <- bind_rows(
               Changes[[i]], 
               data.frame(
                  sim_workspace_files = unique(Changes[[i]]$sim_workspace_files), 
                  new_sim_workspace_files = unique(Changes[[i]]$new_sim_workspace_files), 
                  Detail = "NumDoses_inhib",
                  Value = NumDosesNeeded_inhib, 
                  Level1 = "SimulationData", 
                  Level2 = "idNumberDoses2"))
         }
         
         rm(NumDosesNeeded_inhib, NumDosesNeeded_sub)
      }
      
      # Setting the simulation end day as needed
      if("SimDuration" %in% Changes[[i]]$Detail){
         EndDay <- as.numeric(Changes[[i]]$Value[which(Changes[[i]]$Detail == "SimDuration")])
         EndDay <- as.character(EndDay %/% 24)
         
         Changes[[i]] <- bind_rows(
            Changes[[i]], 
            data.frame(
               sim_workspace_files = unique(Changes[[i]]$sim_workspace_files), 
               new_sim_workspace_files = unique(Changes[[i]]$new_sim_workspace_files), 
               Detail = "StudyEndDay",
               Value = EndDay, 
               Level1 = "SimulationData", 
               Level2 = "idStudyEndDay"))
      }
      
      # Dealing w/all other changes
      for(j in 1:nrow(Changes[[i]])){
         suppressWarnings(
            MyChanges <- Changes[[i]][j, ] %>% 
               mutate(Level2 = ifelse(complete.cases(as.numeric(Level2)), 
                                      as.numeric(Level2), Level2))
         )
         DeetLevels <- t(MyChanges[, paste0("Level", 1:5)])
         DeetLevels <- min(which(is.na(DeetLevels))) - 1
         
         if(DeetLevels == 1){
            XML::xmlValue(RootNode[[MyChanges$Level1]]) <- 
               MyChanges$Value
            
         } else if(DeetLevels == 2){
            XML::xmlValue(RootNode[[MyChanges$Level1]][[MyChanges$Level2]]) <- 
               MyChanges$Value
            
         } else if(DeetLevels == 3){
            XML::xmlValue(RootNode[[MyChanges$Level1]][[MyChanges$Level2]][[
               MyChanges$Level3]]) <- 
               MyChanges$Value
            
         } else if(DeetLevels == 4){
            XML::xmlValue(RootNode[[MyChanges$Level1]][[MyChanges$Level2]][[
               MyChanges$Level3]][[MyChanges$Level4]]) <- 
               MyChanges$Value
            
         } else if(DeetLevels == 5){
            XML::xmlValue(RootNode[[MyChanges$Level1]][[MyChanges$Level2]][[
               MyChanges$Level3]][[MyChanges$Level4]][[MyChanges$Level5]]) <- 
               MyChanges$Value
         }
         
         rm(DeetLevels, MyChanges)
      }
      
      ## Checking for observed overlay or fixed trial design files -------------
      
      if(fix_xml_paths){
         # If XML file is listed and the path doesn't match b/c the workspace path
         # has a different user b/c of SharePoint, fix that.
         TEMP <- change_xml_path(workspace_objects = list(workspace_xml), 
                                 save_workspaces = FALSE)
         workspace_xml <- TEMP[[1]]
      }
      
      ## Saving -------------------------------------------------------------
      XML::saveXML(workspace_xml, file = "temp.xml")
      print(paste0("Saving `", i, "`"))
      R.utils::gzip(filename = "temp.xml", 
                    destname = i,
                    remove = TRUE, overwrite = TRUE)
      
      rm(workspace_xml, RootNode)
   }
}


