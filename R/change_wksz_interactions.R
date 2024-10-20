#' Change interaction parameters in Simcyp Simulator workspace files
#'
#' @description \code{change_wksz_interactions} changes interaction parameters
#'   such as Kapp, Ki, or IndMax in Simulator workspace files. Currently only
#'   set up to change CYP, P-gp, and a few UGT parameters, but we can add more
#'   options upon request. A few notes: \enumerate{\item{Pay attention to the "switch"
#'   arguments, which are set up just like in the Simulator but are easy to
#'   overlook.} \item{This will automatically fix any XML file paths such as
#'   observed data overlay files or fixed trial design XML files to match the
#'   current user's path on SharePoint. If you don't want that, set
#'   \code{fix_xml_paths} to FALSE.} \item{\strong{WARNING:} Do NOT mix Simulator
#'   versions in a single call. If you need more than one Simulator version, run
#'   this separately for each. It seems to have trouble otherwise, and I haven't
#'   discerned exactly where that trouble arises. - LSh}}
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
#' @param interactions_to_set a data.frame of interaction parameters to use
#'   instead of filling in each of the arguments below (compoundID through
#'   Ind_slope) with character vectors of values. The only columns that will
#'   this function will pay any attention to are "sim_workspace_files",
#'   "new_sim_workspace_files", or any of "compoundID", "enzymes",
#'   "competitive_inhibition_switch", "Ki", "Ki_fumic", "TDI_switch", "Kapp",
#'   "kinact", "Kapp_fumic", "induction_IndC50_switch", "IndMax", "IndC50",
#'   "IndC50_fuinc", "induction_IndMax_switch", "Ind_slope", and "Ind_gamma".
#'   Please see the notes on each of the arguments below to see what values to
#'   use for this data.frame. This can be an easier way to set things up when
#'   you have a lot of information to keep track of or when you're setting
#'   parameters for multiple enzymes or when you want some parameters for some
#'   workspaces and other parameters for other workspaces. If you leave off the
#'   column "sim_workspace_files", we'll assume you want the same parameters for
#'   all the workspaces. If you leave off the column "new_sim_workspace_files",
#'   we'll assume you want to overwrite the existing workspaces.
#' @param compoundID compoundID whose parameters you want to change. Options
#'   are: \itemize{\item{"substrate",} \item{"primary metabolite 1",}
#'   \item{"primary metabolite 2",} \item{"secondary metabolite",}
#'   \item{"inhibitor 1" (default) -- this can be an inducer, inhibitor,
#'   activator, or suppresesor, but it's labeled as "Inhibitor 1" in the
#'   simulator,} \item{"inhibitor 2" for the 2nd perpetrator listed in the
#'   simulation,} \item{"inhibitor 1 metabolite" for the primary metabolite of
#'   inhibitor 1}}
#' @param enzymes enzymes whose interaction parameters you want to change, e.g.,
#'   \code{enzymes = c("CYP3A4", "CYP3A5")}. Note: This is currently only set up
#'   to change CYP, some UGT, P-gp, BCRP, MATE1, and MATE2-K interaction
#'   parameters. If you need to change interaction parameters for other enzymes
#'   or transporters, please talk to Laura Shireman.
#' @param tissues tissues where the enzyme of interest may be found. Leave as
#'   "all" if not applicable (CYPs and UGTS) or where you want all the possible
#'   tissues. For transporters, where the Simulator allows you to set different
#'   values for different tissues, you can alternatively set this to
#'   "intestine", "liver", or "kidney" if you don't want to change it in all
#'   possible tissues.
#' @param competitive_inhibition_switch turn competitive inhibition "on" or
#'   "off" or set to "no change" to leave the original value
#' @param Ki Ki value to use for competitive inhibition (uM) or set to "no
#'   change" to leave the original value. If set to NA, value will be set to
#'   1E+06.
#' @param Ki_fumic fu,mic value to use for competitive inhibition or set to "no
#'   change" to leave the original value. If set to NA, value will be set to 1.
#' @param TDI_switch turn time-dependent inactivation "on" or "off" or set to
#'   "no change" (default) to leave the original value
#' @param Kapp Kapp value to use for TDI (uM) or set to "no change" (default) to
#'   leave the original value. If set to NA, value will be set to 1E+06.
#' @param kinact kinact value to use for TDI (1/h) or set to "no change"
#'   (default) to leave the original value. If set to NA, value will be set to
#'   0.
#' @param Kapp_fumic fu,mic value to use for TDI or set to "no change" (default)
#'   to leave as the original value. If set to NA, value will be set to 1.
#' @param induction_IndMax_switch set to "on" when there is either a) no
#'   induction (IndMax is 1) or b) when there \emph{is} induction and you're
#'   using any induction model except the slope model. Set to "off" when there
#'   is induction but you're using the "slope" model to describe it. Set to "no
#'   change" (default) to leave as the original value.
#' @param IndMax a number for the maximum fold induction to use or set to "no
#'   change" (default) to leave the original value.
#' @param induction_IndC50_switch turn "on" or "off" the use of an induction
#'   IndC50 value or set to "no change" (default) to leave the original value
#' @param IndC50 IndC50 value to use (uM) or set to "no change" (default) to
#'   leave the original value. If set to NA, value will be set to 1E+06.
#' @param IndC50_fuinc fu,mic value to use for the IndC50 incubation or set to
#'   "no change" (default) to leave the original value. If set to NA, value will
#'   be set to 1.
#' @param Ind_gamma value to use for gamma aka the Hill coefficient for
#'   induction or set to "no change" (default) to leave the original value. If
#'   set to NA, value will be set to 1.
#' @param Ind_slope value to use for slope in induction or set to "no change"
#'   (default) to leave the original value. If left as NA, value will be set to
#'   0.
#'
#' @return does not return anything in R but saves workspace files
#' @export
#'
#' @examples
#' # None yet
#' 
change_wksz_interactions <- function(sim_workspace_files = NA,
                                     new_sim_workspace_files = NA,
                                     fix_xml_paths = TRUE,
                                     
                                     interactions_to_set = NA,
                                     
                                     compoundID = "inhibitor 1",
                                     enzymes, 
                                     tissues = "all",
                                     
                                     competitive_inhibition_switch = "no change",
                                     Ki = "no change", 
                                     Ki_fumic = "no change",
                                     
                                     TDI_switch = "no change",
                                     Kapp = "no change",
                                     kinact = "no change", 
                                     Kapp_fumic = "no change",
                                     
                                     induction_IndMax_switch = "no change",
                                     IndMax = "no change", 
                                     induction_IndC50_switch = "no change",
                                     IndC50 = "no change", 
                                     IndC50_fuinc = "no change", 
                                     Ind_gamma = "no change",
                                     Ind_slope = "no change"){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If they left sim_workspace_files as NA and did not supply something for
   # that in interactions_to_set, then they want to apply this function to all
   # the workspaces in the current folder. Getting the names of the workspaces.
   if(class(interactions_to_set) == "logical" | 
      ("data.frame" %in% class(interactions_to_set) && 
       "sim_workspace_files" %in% names(interactions_to_set) == FALSE)){
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
      # interactions_to_set, then use that.
      if(length(sim_workspace_files) == 1 && is.na(sim_workspace_files) &
         "sim_workspace_files" %in% names(interactions_to_set)){
         sim_workspace_files <- interactions_to_set$sim_workspace_files
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
   
   if(class(interactions_to_set) != "logical"){
      if("data.frame" %in% class(interactions_to_set) == FALSE){
         stop("You have supplied an object for `interactions_to_set`, but it's not a data.frame, which is what it needs to be. Please check your input and the help file and try again.", 
              call. = FALSE)
      }
      
      if(all(c("compoundID", "enzymes") %in% names(interactions_to_set)) == FALSE){
         stop(paste0("You have supplied a data.frame for `interactions_to_set`, but it is missing the column(s) ", 
                     str_comma(setdiff(c("compoundID", "enzymes"), 
                                       names(interactions_to_set))), 
                     ". Please check your input and the help file and try again."), 
              call. = FALSE)
      }
      
      Changes <- interactions_to_set %>% 
         select(compoundID, enzymes, 
                any_of(c("sim_workspace_files", "new_sim_workspace_files",
                         "tissues",
                         "competitive_inhibition_switch", "Ki", "Ki_fumic",
                         "TDI_switch", "Kapp", "kinact", "Kapp_fumic",
                         "induction_IndC50_switch", "IndMax", "IndC50",
                         "IndC50_fuinc", "Ind_gamma", "induction_IndMax_switch",
                         "Ind_slope")))
      
   } else {
      
      # Collecting changes to make into a data.frame if it's not already.
      Changes <- list(compoundID = compoundID,
                      enzymes = enzymes,
                      tissues = tissues,
                      competitive_inhibition_switch = competitive_inhibition_switch,
                      Ki = Ki,
                      Ki_fumic = Ki_fumic,
                      TDI_switch = TDI_switch,
                      Kapp = Kapp,
                      kinact = kinact,
                      Kapp_fumic = Kapp_fumic,
                      induction_IndC50_switch = induction_IndC50_switch,
                      IndMax = IndMax, 
                      IndC50 = IndC50,
                      IndC50_fuinc = IndC50_fuinc,
                      Ind_gamma = Ind_gamma,
                      induction_IndMax_switch = induction_IndMax_switch,
                      Ind_slope = Ind_slope)
      
      # Checking lengths of arguments
      ChangeLength <- unlist(lapply(Changes, length))
      ChangeLength <- ChangeLength[!ChangeLength == 1]
      
      if(length(unique(ChangeLength)) > 1){
         stop("You have not provided the same number of inputs for each of the arguments `compoundID` through `Ind_gamma`, and all of these must have the same number of inputs or have just a single value, which will be repeated for all enzymes, compounds, and workspaces as needed. Please check this and try again.", 
              call. = FALSE)
      }
      
      Changes <- as.data.frame(Changes)
   }
   
   # Adding the file names to Changes if they're not already present.
   if("sim_workspace_files" %in% names(Changes) == FALSE){
      # Assuming that, if there are multiple rows in Changes, user wants all of
      # those changes applied to all possible workspace files.
      Changes <- left_join(Changes, 
                           expand_grid(sim_workspace_files = sim_workspace_files, 
                                       enzymes = unique(Changes$enzymes)), 
                           by = "enzymes")
   }
   
   if("new_sim_workspace_files" %in% names(Changes) == FALSE){
      if(any(complete.cases(new_sim_workspace_files))){
         Changes <- Changes %>% 
            left_join(data.frame(sim_workspace_files = sim_workspace_files, 
                                 new_sim_workspace_files = new_sim_workspace_files), 
                      by = "sim_workspace_files")
      } else {
         Changes$new_sim_workspace_files <- Changes$sim_workspace_files
      }
   }
   
   if("tissues" %in% names(Changes) == FALSE){
      Changes$tissues <- tissues
   }
   
   # Expanding DF for tissues if they listed tissues = "all".
   if(("tissues" %in% names(Changes) &&
       all(Changes$tissues == "all")) | 
      (all(tissues == "all") & 
       "tissues" %in% names(Changes) == FALSE)){
      Changes <- Changes %>% rename(tissues_orig = tissues) %>% 
         left_join(data.frame(tissues_orig = "all", 
                              tissues = c("kidney", "liver", "intestine")), 
                   relationship = "many-to-many", 
                   by = "tissues_orig") %>% 
         # no tissue specification for DME. Removing that to avoid duplicates.
         mutate(tissues = ifelse(enzymes %in% c("P-gp", "BCRP", 
                                                "MATE1", "MATE2-K"), 
                                 tissues, NA)) %>% 
         select(-tissues_orig) %>% 
         unique()
   }
   
   # At this point, regardless of whether they provided a data.frame to
   # interactions_to_set or filled out the arguments individually, we should
   # have a data.frame called Changes that includes all interactions AND the
   # original and revised file names.
   
   
   # subfun for V23+ data extraction ------------------------------------------
   
   # For some reason, you have to unzip the workspaces 1st if they're V23 or
   # later. Not sure what changed.
   unzip1st_fun <- function(workspace){
      R.utils::gunzip(workspace, destname = "TEMP.wks", remove = FALSE)
      workspace_xml <- XML::xmlTreeParse("TEMP.wks", useInternal = TRUE)
      file.remove("TEMP.wks")
      return(workspace_xml)
   }
   
   # Main body of function ---------------------------------------------------
   
   # If they didn't include the file suffix, add that. Replace any "xlsx" file
   # extensions with "wksz". Clean up "Value" column and set Level5 tag names.
   Changes <- Changes %>% 
      mutate(sim_workspace_files = paste0(sub("\\.xlsx|\\.wksz", "", sim_workspace_files), ".wksz"), 
             new_sim_workspace_files = paste0(sub("\\.xlsx|\\.wksz", "", new_sim_workspace_files), ".wksz"), 
             # Adjusting enzyme names as needed. 
             enzymes = sub("P-gp|ABCB1", "Pgp", enzymes), 
             tissues = tolower(tissues)) %>% 
      mutate(across(.cols = everything(), .fns = as.character)) %>% 
      pivot_longer(cols = -c(compoundID, enzymes, tissues, sim_workspace_files, 
                             new_sim_workspace_files), 
                   names_to = "Parameter", 
                   values_to = "Value") %>% 
      filter(Value != "no change") %>% 
      # Dealing with NAs depending on what the parameter is
      mutate(OrigNA = is.na(Value),
             Value = case_when(is.na(Value) & 
                                  Parameter %in% c("Ki", "Kapp", "IndC50") ~ "1000000", 
                               is.na(Value) &
                                  Parameter %in% c("Ki_fumic", "Kapp_fumic", 
                                                   "IndMax", "IndC50_fuinc", 
                                                   "Ind_gamm") ~ "1", 
                               is.na(Value) & 
                                  Parameter %in% c("kinact", "Ind_slope") ~ "0", 
                               TRUE ~ Value),
             
             # Dealing with switches to make them T or F instead of off or on
             Value = case_when(Parameter %in% c("competitive_inhibition_switch", 
                                                "TDI_switch",
                                                "induction_IndC50_switch") &
                                  Value == "on" ~ "true", 
                               Parameter %in% c("competitive_inhibition_switch", 
                                                "TDI_switch",
                                                "induction_IndC50_switch") &
                                  Value == "off" ~ "false", 
                               
                               Parameter == "induction_IndMax_switch" &
                                  str_detect(enzymes, "CYP|UGT") &
                                  Value == "on" ~ "false",  # <-- This turns OFF the induction slope switch
                               Parameter == "induction_IndMax_switch" &
                                  str_detect(enzymes, "Pgp") &
                                  Value == "on" ~ "true", # <-- This turns ON the IndMax switch
                               
                               Parameter == "induction_IndMax_switch" &
                                  str_detect(enzymes, "CYP|UGT") &
                                  Value == "off" ~ "true",  # <-- This turns ON the induction slope switch
                               Parameter == "induction_IndMax_switch" &
                                  str_detect(enzymes, "Pgp") &
                                  Value == "off" ~ "false", # <-- This turns OFF the IndMax switch
                               TRUE ~ Value),
             
             # setting the level 5 tag names b/c they change depending on the
             # scenario.
             Level5 = case_when(Parameter == "competitive_inhibition_switch" &
                                   str_detect(enzymes, "CYP|UGT") ~ "KiSwitch", 
                                Parameter == "competitive_inhibition_switch" &
                                   str_detect(enzymes, "Pgp|BCRP|MATE") ~ "KiEnabled", 
                                
                                Parameter == "Ki_fumic" &
                                   str_detect(enzymes, "CYP|UGT") ~ "Fumic",
                                Parameter == "Ki_fumic" &
                                   str_detect(enzymes, "Pgp|BCRP|MATE") ~ "KiFuinc",
                                
                                Parameter == "TDI_switch" ~ "KappSwitch",
                                Parameter == "kinact" ~ "Kinact",
                                
                                Parameter == "induction_IndMax_switch" &
                                   str_detect(enzymes, "CYP|UGT") ~ "IndSlopeSwitch",
                                Parameter == "induction_IndMax_switch" &
                                   str_detect(enzymes, "Pgp|BCRP|MATE") ~ "Ind_max_switch",
                                
                                Parameter == "Ind_slope" &
                                   str_detect(enzymes, "CYP|UGT") ~ "IndSlope",
                                Parameter == "induction_IndMax_switch" &
                                   str_detect(enzymes, "Pgp|BCRP|MATE") ~ "Ind_slope",
                                
                                Parameter == "induction_IndC50_switch" &
                                   str_detect(enzymes, "CYP|UGT") ~ "IndC50Switch",
                                Parameter == "induction_IndC50_switch" &
                                   str_detect(enzymes, "Pgp") ~ "Ind_C50_switch",
                                
                                Parameter == "IndC50" &
                                   str_detect(enzymes, "Pgp|BCRP|MATE") ~ "Ind_C50",
                                
                                Parameter == "Ind_gamma" &
                                   str_detect(enzymes, "CYP|UGT") ~ "Y",
                                Parameter == "Ind_gamma" &
                                   str_detect(enzymes, "Pgp|BCRP|MATE") ~ "Induction_Hill",
                                
                                TRUE ~ Parameter))
   
   # Check for bad inputs. 
   InputCheck <- Changes %>% 
      mutate(ValNum = suppressWarnings(as.numeric(Value)), 
             Value = ifelse(Value == "NA", NA, Value),
             ValCheck = complete.cases(ValNum) |
                (is.na(Value) & is.na(ValNum)) |
                Value %in% c("no change", "true", "false", "on", "off"))
   
   if(all(InputCheck$ValCheck) == FALSE){
      stop(paste0("There is a problem with some of your input. Depending on the parameter, acceptable values are numbers, NA, `no change`, `on`, or `off`. Please check your input for the following parameters:\n",
                  str_c(InputCheck$Parameter[InputCheck$ValCheck == FALSE], collapse = "\n")), 
           call. = FALSE)
   }
   
   # Grouping by new workspace
   Changes <- split(Changes, f = Changes$new_sim_workspace_files)
   
   for(i in names(Changes)){
      
      if(length(unique(Changes[[i]]$sim_workspace_files)) != 1){
         stop(paste0("You have more than one original workspace file for the new workspace file `", 
                     i, "`. We don't know which original workspace values you want to use in the new workspace file. Please check your input and the help file and try again with only one original workspace for any new workspace file."), 
              call. = FALSE)
      }
      
      workspace_xml <- tryCatch(
         XML::xmlTreeParse(unique(Changes[[i]]$sim_workspace_files),
                           useInternal = TRUE), 
         
         error = unzip1st_fun(unique(Changes[[i]]$sim_workspace_files)))
      
      RootNode <- XML::xmlRoot(workspace_xml)
      
      # NB: I originally set this up to do a check here for whether they had
      # specified the switches correctly. It quickly became unmanageable in
      # complexity. They'll need to just make sure they set them right. May
      # return to this later, though.
      
      for(j in 1:nrow(Changes[[i]])){
         
         CompoundIDnum <- switch(Changes[[i]]$compoundID[j], 
                                 "substrate" = 1, 
                                 "inhibitor 1" = 2, 
                                 "inhibitor 2" = 3, 
                                 "inhibitor 3" = 4, 
                                 "primary metabolite 1" = 5, 
                                 "inhibitor 1 metabolite" = 6,
                                 "secondary metabolite" = 7, 
                                 "primary metabolite 2" = 8)
         
         EnzIntRoutes <- switch(str_extract(Changes[[i]]$enzymes[j], 
                                            "CYP|UGT|Pgp|BCRP|OATP1B|MATE1|MATE2-K"), 
                                "CYP" = "CYPInteractionRoutes", 
                                "UGT" = "UGTInteractionRoutes", 
                                "Pgp" = switch(Changes[[i]]$tissues[j], 
                                               "liver" = "LiverTransporterSet", 
                                               "kidney" = "KidneyTransporterSet",
                                               "intestine" = "GutTransporterSet"), 
                                "BCRP" = switch(Changes[[i]]$tissues[j], 
                                                "liver" = "LiverTransporterSet", 
                                                "kidney" = "KidneyTransporterSet",
                                                "intestine" = "GutTransporterSet"), 
                                "MATE1" = switch(Changes[[i]]$tissues[j], 
                                                 "liver" = "LiverTransporterSet", 
                                                 "kidney" = "KidneyTransporterSet"), 
                                "MATE2-K" = "KidneyTransporterSet", 
                                "OATP1B" = "LiverTransporterSet")
         
         # If that particular tissue is not available, EnzIntRoutes will be
         # null. Skipping in that case.
         if(is.null(EnzIntRoutes)){
            next
         }
         
         EnzNum_Enzyme <- switch(Changes[[i]]$enzymes[j],
                                 # !!! WARNING: I have checked this for V22.
                                 # Double check that this applies for other
                                 # versions. -LSh
                                 
                                 "CYP1A1" = 13,
                                 "CYP1A2" = 0, 
                                 "CYP2A6" = 1, 
                                 "CYP2B6" = 2,
                                 "CYP2C18" = 5, 
                                 "CYP2C19" = 6,
                                 "CYP2C8" = 3, 
                                 "CYP2C9" = 4, 
                                 "CYP2D6" = 7, 
                                 "CYP2E1" = 8, 
                                 "CYP2J2" = 9,
                                 "CYP3A4" = 10, 
                                 "CYP3A5" = 11, 
                                 "CYP3A7" = 12, 
                                 
                                 "UGT1A1" = 1, 
                                 "UGT1A3" = 2, 
                                 "UGT1A4" = 3, 
                                 "UGT1A5" = 4, 
                                 "UGT1A6" = 5, 
                                 "UGT1A7" = 6, 
                                 "UGT1A8" = 7, 
                                 "UGT1A9" = 8, 
                                 "UGT1A10" = 9, 
                                 "UGT2B4" = 10, 
                                 "UGT2B7" = 11, 
                                 "UGT2B10" = 12, 
                                 "UGT2B11" = 13, 
                                 "UGT2B15" = 14, 
                                 "UGT2B17" = 15, 
                                 "UGT2B28" = 16, 
                                 "User UGT1" = 17, 
                                 "Pgp" =  switch(Changes[[i]]$tissues[j], 
                                                 "liver" = 18, 
                                                 "kidney" = 19,
                                                 "intestine" = 10), 
                                 "BCRP" = switch(Changes[[i]]$tissues[j], 
                                                 "liver" = 21, 
                                                 "kidney" = 22,
                                                 "intestine" = 12), 
                                 "MATE1" = switch(Changes[[i]]$tissues[j], 
                                                  "liver" = 22, 
                                                  "kidney" = 23), 
                                 "MATE2-K" = 24, # only in kidney
                                 "OATP1B1" =  6, # only in liver
                                 "OATP1B3" = 7 # only in liver
         )
         
         # Next check is for DME only and NOT for transporters. 
         if(str_detect(Changes[[i]]$enzymes[j], "CYP|UGT")){
            # Need to check what the index is for each enzyme b/c sometimes, for
            # reasons that utterly mystify and infuriate me, the index and the
            # enzyme number DON'T MATCH. Why use an enzyme number if you're going
            # to move around the position?!?
            EnzymeIndex <- 1:length(
               names(RootNode[["Compounds"]][[CompoundIDnum]][[EnzIntRoutes]]))
            
            for(enz_i in EnzymeIndex){
               names(EnzymeIndex)[enz_i] <- 
                  XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[EnzIntRoutes]][[enz_i]][["Enzyme"]])
            }
            
            EnzNum <- EnzymeIndex[as.character(EnzNum_Enzyme)]
         } else {
            EnzNum <- EnzNum_Enzyme
         }
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            EnzIntRoutes]][[EnzNum]][[Changes[[i]]$Level5[j]]]) <-
            Changes[[i]]$Value[j]
         
         rm(EnzIntRoutes, EnzNum, CompoundIDnum)
         
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


