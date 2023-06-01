#' Change interaction parameters in Simcyp Simulator workspace files
#'
#' \code{change_wksz_interactions} changes interaction parameters such as Kapp
#' or Ki or IndMax in workspace files (.wksz files) for then running with the
#' Simcyp Simulator. This will change \emph{all} the workspace files provided to
#' have the \emph{same} interaction parameters listed here. Currently only set
#' up to change CYP parameters, but we'll add more options soon. UNDER
#' CONSTRUCTION.
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
#' @param interactions_to_set a data.frame of interaction parameters to use
#'   instead of filling in each of the arguments below (compoundID through
#'   Ind_slope) with character vectors of values. The only columns that will
#'   this function will pay any attention to are "sim_workspace_files",
#'   "new_sim_workspace_files", or any of "compoundID", "enzymes",
#'   "competitive_inhibition_switch", "Ki", "Ki_fumic", "TDI_switch", "Kapp",
#'   "kinact", "Kapp_fumic", "induction_IndC50_switch", "IndMax", "IndC50",
#'   "IndC50_fuinc", "induction_slope_switch", "Ind_slope", and "Ind_gamma".
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
#'   simulator,} \item{"inhibitor 2" for the 2nd effector listed in the
#'   simulation,} \item{"inhibitor 1 metabolite" for the primary metabolite of
#'   inhibitor 1}}
#' @param enzymes enzymes whose interaction parameters you want to change, e.g.,
#'   \code{enzymes = c("CYP3A4", "CYP3A5")}. Note: This is currently only set up
#'   to change CYP interaction parameters. If you need to change interaction
#'   parameters for other enzymes or transporters, please talk to Laura
#'   Shireman.
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
#' @param induction_IndC50_switch turn "on" or "off" the use of an induction
#'   IndC50 value or set to "no change" (default) to leave the original value
#' @param IndC50 IndC50 value to use (uM) or set to "no change" (default) to
#'   leave the original value. If set to NA, value will be set to 1E+06.
#' @param IndC50_fuinc fu,mic value to use for the IndC50 incubation or set to
#'   "no change" (default) to leave the original value. If set to NA, value will
#'   be set to 1.
#' @param IndMax a number for the maximum fold induction to use or set to "no
#'   change" (default) to leave the original value. Note that IndMax doesn't
#'   require a switch to turn it on (there's no toggle switch for it in the
#'   Simulator).
#' @param Ind_gamma value to use for gamma aka the Hill coefficient for
#'   induction or set to "no change" (default) to leave the original value. If
#'   set to NA, value will be set to 1.
#' @param induction_slope_switch turn "on" or "off" the use of a slope value for
#'   describing induction or set to "no change" (default) to leave the original
#'   value
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
                                     
                                     interactions_to_set = NA,
                                     
                                     compoundID = "inhibitor 1",
                                     enzymes, 
                                     
                                     competitive_inhibition_switch = "no change",
                                     Ki = "no change", 
                                     Ki_fumic = "no change",
                                     
                                     TDI_switch = "no change",
                                     Kapp = "no change",
                                     kinact = "no change", 
                                     Kapp_fumic = "no change",
                                     
                                     induction_IndC50_switch = "no change",
                                     IndC50 = "no change", 
                                     IndC50_fuinc = "no change", 
                                     IndMax = "no change", 
                                     Ind_gamma = "no change",
                                     induction_slope_switch = "no change",
                                     Ind_slope = "no change"){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If they left sim_workspace_files as NA and did not supply something for
   # interactions_to_set, then they want to apply this function to all the
   # workspaces in the current folder. Getting the names of the workspaces.
   if(class(interactions_to_set) == "logical"){
      sim_workspace_files <- sim_workspace_files[complete.cases(sim_workspace_files)]
      if(length(sim_workspace_files) == 0){
         sim_workspace_files <- list.files(pattern = ".wksz")
      } else if(any(sim_workspace_files == "recursive")){
         sim_workspace_files <- list.files(pattern = ".wksz", recursive = TRUE)
      }
   } else {
      if(length(sim_workspace_files) == 1 && is.na(sim_workspace_files)){
         sim_workspace_files <- interactions_to_set$sim_workspace_files
      }
   }
   
   if(length(sim_workspace_files) == 0){
      stop("No workspace files could be found to change.")
   }
   
   # If they did not provide a value for new_sim_workspace_files, then they
   # must want the orginal file names to be overwritten.
   if(all(is.na(new_sim_workspace_files))){
      new_sim_workspace_files <- sim_workspace_files
   }
   
   # If they didn't include the file suffix, add that. Replace any "xlsx" file
   # extensions with "wksz".
   if(any(str_detect(sim_workspace_files, "wksz") == FALSE)){
      sim_workspace_files <- paste0(sub("\\.xlsx", "", sim_workspace_files), ".wksz")
   }
   if(any(str_detect(new_sim_workspace_files, "wksz") == FALSE)){
      new_sim_workspace_files <- paste0(sub("\\.xlsx", "", new_sim_workspace_files), ".wksz")
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
                         "competitive_inhibition_switch", "Ki", "Ki_fumic",
                         "TDI_switch", "Kapp", "kinact", "Kapp_fumic",
                         "induction_IndC50_switch", "IndMax", "IndC50",
                         "IndC50_fuinc", "Ind_gamma", "induction_slope_switch",
                         "Ind_slope")))
      
   } else {
      
      # Collecting changes to make into a data.frame if it's not already.
      Changes <- list(compoundID = compoundID,
                      enzymes = enzymes,
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
                      induction_slope_switch = induction_slope_switch,
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
                                       enzymes = enzymes), 
                           by = "enzymes")
   }
   
   if("new_sim_workspace_files" %in% names(Changes) == FALSE){
      Changes <- Changes %>% 
         left_join(data.frame(sim_workspace_files = sim_workspace_files, 
                              new_sim_workspace_files = new_sim_workspace_files), 
                   by = "sim_workspace_files")
   }
   
   
   # Main body of function ---------------------------------------------------
   
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
      
      Changes_i <- Changes[[i]] %>% 
         mutate(across(.cols = everything(), 
                       .fns = as.character)) %>% 
         pivot_longer(cols = -c(compoundID, enzymes, sim_workspace_files, 
                                new_sim_workspace_files), 
                      names_to = "Parameter", 
                      values_to = "Value") %>% 
         filter(Value != "no change")
      
      # If they specified a value for something but didn't turn the switch on,
      # fix that for them. I mean for the "switch" arguments mainly to be an
      # opportunity to turn off interactions they no longer want, and I think
      # people will not think to turn switches ON when they include new
      # interaction parameters. Note that IndMax does not require a switch, so
      # it's filtered out here. In fact, only parameters that have a switch
      # associated with them are included.
      ArgSwitchCheck <- Changes_i %>% 
         filter(!Parameter %in% c("IndMax", "Ki_fumic", "Kapp_fumic", 
                                  "IndC50_fuinc", "Ind_gamma"))
      
      if(any(str_detect(tolower(ArgSwitchCheck$Parameter), "switch"))){
         Changes_i <- Changes_i %>% 
            mutate(Value = case_when(str_detect(tolower(Parameter), "switch") &
                                        Value == "on" ~ "true", 
                                     str_detect(tolower(Parameter), "switch") &
                                        Value == "off" ~ "false", 
                                     TRUE ~ Value))
      } else {
         # This is when they forgot to include anything about the switches,
         # which will probably be often. 
         ArgSwitchCheck <- ArgSwitchCheck %>% 
            mutate(Switch = case_when(
               Parameter %in% c("Ki") ~ "competitive_inhibition_switch",
               Parameter %in% c("Kapp", "kinact") ~ "TDI_switch", 
               Parameter %in% c("IndC50") ~ "IndC50_switch", 
               Parameter %in% c("Ind_slope") ~ "Ind_slope_switch")) %>% 
            mutate(NewValueForSwitch = ifelse(complete.cases(Value),
                                              "true", "false"))
         
         Changes_i <- bind_rows(
            Changes_i, 
            ArgSwitchCheck %>% 
               select(compoundID, enzymes, Switch, NewValueForSwitch,
                      sim_workspace_files, new_sim_workspace_files) %>% 
               unique() %>% 
               rename(Parameter = Switch, 
                      Value = NewValueForSwitch))
      }
      
      for(j in 1:nrow(Changes_i)){
         
         CompoundIDnum <- switch(Changes_i$compoundID[j], 
                                 "substrate" = 1, 
                                 "inhibitor 1" = 2, 
                                 "inhibitor 2" = 3, 
                                 "inhibitor 3" = 4, 
                                 "primary metabolite 1" = 5, 
                                 "inhibitor 1 metabolite" = 6,
                                 "secondary metabolite" = 7, 
                                 "primary metabolite 2" = 8)
         
         EnzIntRoutes <- switch(str_extract(Changes_i$enzymes[j], 
                                            "CYP|UGT"), 
                                "CYP" = "CYPInteractionRoutes", 
                                "UGT" = "UGTInteractionRoutes")
         
         EnzNum <- switch(Changes_i$enzymes[j],
                          "CYP1A2" = 1, 
                          "CYP2A6" = 2, 
                          "CYP2B6" = 3,
                          "CYP2C8" = 4, 
                          "CYP2C9" = 5, 
                          "CYP2C18" = 6, 
                          "CYP2C19" = 7,
                          "CYP2D6" = 8, 
                          "CYP2E1" = 9, 
                          "CYP2J2" = 10,
                          "CYP3A4" = 11, 
                          "CYP3A5" = 12, 
                          "CYP3A7" = 13)
         
         # Making a whole bunch of "if" statements b/c I'm not proficient enough
         # w/XML files to use "switch" well here. What would be a better way to
         # do this?!? There must be a better one!
         
         ## Competitive inhibition -----------------------------------------
         if(as.character(Changes_i[j, "Parameter"]) == "competitive_inhibition_switch"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["KiSwitch"]]) <-
               as.character(Changes_i[j, "Value"])
         }
         
         if(as.character(Changes_i[j, "Parameter"]) == "Ki"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["Ki"]]) <-
               ifelse(is.na(as.character(Changes_i[j, "Value"])),
                      "1000000", as.character(Changes_i[j, "Value"]))
         }
         
         if(as.character(Changes_i[j, "Parameter"]) == "Ki_fumic"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["Fumic"]]) <-
               ifelse(is.na(as.character(Changes_i[j, "Value"])),
                      1, as.character(Changes_i[j, "Value"]))
         }
         
         
         ## TDI --------------------------------------------------------
         if(as.character(Changes_i[j, "Parameter"]) == "TDI_switch"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["KappSwitch"]]) <-
               as.character(Changes_i[j, "Value"])
         }
         
         if(as.character(Changes_i[j, "Parameter"]) == "Kapp"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["Kapp"]]) <-
               ifelse(is.na(as.character(Changes_i[j, "Value"])),
                      "1000000", as.character(Changes_i[j, "Value"]))
         }
         
         if(as.character(Changes_i[j, "Parameter"]) == "kinact"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["Kinact"]]) <-
               ifelse(is.na(as.character(Changes_i[j, "Value"])),
                      0, as.character(Changes_i[j, "Value"]))
         }
         
         if(as.character(Changes_i[j, "Parameter"]) == "Kapp_fumic"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["Kapp_fumic"]]) <-
               ifelse(is.na(as.character(Changes_i[j, "Value"])),
                      1, as.character(Changes_i[j, "Value"]))
         }
         
         ## Induction ----------------------------------------------------
         
         # No switch for Indmax
         if(as.character(Changes_i[j, "Parameter"]) == "IndMax"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["IndMax"]]) <- 
               ifelse(is.na(as.character(Changes_i[j, "Value"])), 
                      1, as.character(Changes_i[j, "Value"]))
         }
         
         if(as.character(Changes_i[j, "Parameter"]) == "induction_IndC50_switch"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["IndC50Switch"]]) <- 
               as.character(Changes_i[j, "Value"])
         }
         
         if(as.character(Changes_i[j, "Parameter"]) == "IndC50"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["IndC50"]]) <- 
               ifelse(is.na(as.character(Changes_i[j, "Value"])),
                      "1000000", as.character(Changes_i[j, "Value"]))
         }
         
         if(as.character(Changes_i[j, "Parameter"]) == "IndC50_fuinc"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["IndC50_fuinc"]]) <-
               ifelse(is.na(as.character(Changes_i[j, "Value"])),
                      1, as.character(Changes_i[j, "Value"]))
         }
         
         if(as.character(Changes_i[j, "Parameter"]) == "Ind_gamma"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["Y"]]) <- 
               ifelse(is.na(as.character(Changes_i[j, "Value"])),
                      1, as.character(Changes_i[j, "Value"]))
         }
         
         if(as.character(Changes_i[j, "Parameter"]) == "induction_slope_switch"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["IndSlopeSwitch"]]) <-
               as.character(Changes_i[j, "Value"])
         }
         
         if(as.character(Changes_i[j, "Parameter"]) == "Ind_slope"){
            XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
               EnzIntRoutes]][[EnzNum]][["IndSlope"]]) <- 
               ifelse(is.na(as.character(Changes_i[j, "Value"])), 
                      0, as.character(Changes_i[j, "Value"]))
         }
         
         rm(EnzIntRoutes, EnzNum, CompoundIDnum)
         
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


