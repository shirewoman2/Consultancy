#' Extract experimental details for multiple Simcyp Simulator workspace files at
#' once
#'
#' \code{extractExpDetails_XML} takes a character vector of Simcyp Simulator or
#' Simcyp Discovery workspaces -- or all the workspace files in the current
#' directory if no files are specified -- and collects experimental details for
#' the simulations into a single table. It optionally saves that table to a csv
#' or Excel file.
#' 

#' @param sim_workspace_files a character vector of simulator files, each in
#'   quotes and encapsulated with \code{c(...)}, NA to extract experimental
#'   details for \emph{all} the Simulator or Discovery workspace files in the
#'   current folder, or "recursive" to extract experimental details for
#'   \emph{all} the Simulator or Discovery workspace files in the current folder
#'   and \emph{all} subfolders. Example of acceptable input: \code{c("sim1.wksz",
#'   "sim2.wksz")}.
#' @param exp_details experimental details you want to extract from the
#'   simulator workspace files; currently "all" is the only acceptable input and
#'   anything else will be ignored. These are much more limited than the options
#'   for the function \code{\link{extractExpDetails}} and currently only include
#'   a handful of items such as calculated Peff,human, predicted Vss, the XML
#'   overlay file used for observed data, and the XML file used for a
#'   fixed-trial design.
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
#' @param save_output optionally save the output by supplying a csv or Excel
#'   file name in quotes here, e.g., "Simulation details.csv" or "Simulation
#'   details.xlsx".  Do not include any slashes, dollar signs, or periods in the
#'   file name. If you leave off the file extension, it will be saved as a csv
#'   file.
#'
#' @return Returns a data.frame of the experimental details
#' @export
#'
#' @examples
#'
#' # None yet
#' 



extractExpDetails_XML <- function(sim_workspace_files = NA,
                                  compoundsToExtract = "all",
                                  exp_details = "all", 
                                  save_output = NA){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If user did not supply files, then extract all the files in the current
   # folder that end in "wksz" or in all subfolders if they wanted it to be
   # recursive.
   if(length(sim_workspace_files) == 1 &&
      (is.na(sim_workspace_files) | sim_workspace_files == "recursive")){
      sim_workspace_files <- list.files(pattern = "wksz$|dscw$",
                                        recursive = (complete.cases(sim_workspace_files) &&
                                                        sim_workspace_files == "recursive"))
      sim_workspace_files <- sim_workspace_files[!str_detect(sim_workspace_files, "^~")]
   }
   
   # If they didn't include ".wksz" or ".dscw" at the end, add that.
   WkspFilesNoExt <- sub("( - [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}-[0-9]{2}-[0-9]{2})?(\\.xlsx|\\.dscw|\\.wksz)$",
                         "", sim_workspace_files)
   WkspFile <- list("Simulator" = paste0(WkspFilesNoExt, ".wksz"), 
                    "Discovery" = paste0(WkspFilesNoExt, ".dscw"))
   WkspFile$Simulator <- WkspFile$Simulator[which(file.exists(WkspFile$Simulator))]
   WkspFile$Discovery <- WkspFile$Discovery[which(file.exists(WkspFile$Discovery))]
   WkspFile <- as.character(unlist(WkspFile))
   
   # Warning when file doesn't exist
   MissingSimFiles <- WkspFilesNoExt[
      sapply(WkspFilesNoExt, FUN = function(x){any(str_detect(WkspFile, x))}) == FALSE]
   
   if(length(MissingSimFiles) > 0){
      warning(paste0("The file(s) ", 
                     str_comma(paste0("`", MissingSimFiles, "`")), 
                     " is/are not present and thus will not be extracted.\n"), 
              call. = FALSE)
      return()
   }
   
   sim_workspace_files <- WkspFile
   
   # Checking for file name issues
   CheckFileNames <- check_file_name(sim_workspace_files)
   BadFileNames <- CheckFileNames[!CheckFileNames == "File name meets naming standards."]
   if(length(BadFileNames)> 0){
      BadFileNames <- paste0(names(BadFileNames), ": ", BadFileNames)
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"), "\n"), 
              call. = FALSE)
   }
   
   # Checking compound IDs
   compoundsToExtract <- tolower(compoundsToExtract)
   PossCmpd <- c(AllRegCompounds$CompoundID[
      complete.cases(AllRegCompounds$DetailNames)], "all")
   
   if(any(compoundsToExtract %in% PossCmpd == FALSE)){
      warning(paste0("The compound(s) ", 
                     str_comma(paste0("`", setdiff(compoundsToExtract, PossCmpd), "`")),
                     " is/are not among the possible componds to extract and will be ignored. The possible compounds to extract are only exactly these: ",
                     str_comma(paste0("`", PossCmpd, "`")), "\n"), 
              call. = FALSE)
      compoundsToExtract <- intersect(compoundsToExtract, PossCmpd)
   }
   
   if(any(compoundsToExtract == "all")){
      compoundsToExtract <- AllRegCompounds$CompoundID[
         complete.cases(AllRegCompounds$DetailNames)]
   } else {
      compoundsToExtract <- intersect(compoundsToExtract, 
                                      AllRegCompounds$CompoundID[
                                         complete.cases(AllRegCompounds$DetailNames)])
   }
   
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
   
   XMLDeets <- AllExpDetails %>% filter(DataSource == "workspace or database")
   
   if("all" %in% exp_details){
      exp_details <- XMLDeets %>% pull(Detail)
   }
   
   CompoundDetails <- XMLDeets %>% 
      filter(DataSource == "workspace or database" & Level1 == "Compounds" & 
                !Detail %in% c("Substrate", "Inhibitor1", "Inhibitor2", 
                               "PrimaryMetabolite1", "PrimaryMetabolite2", 
                               "SecondaryMetabolite", "Inhibitor1Metabolite")) %>% 
      pull(Detail)
   
   PopulationDetails <- XMLDeets %>% 
      filter(DataSource == "workspace or database" & Level1 == "Populations")
   
   Deets <- list()
   
   for(i in sim_workspace_files){
      
      Deets[[i]] <- list()
      
      Deets[[i]]$Workspace_TimeLastModified <- as.character(file.info(i)$mtime)
      Deets[[i]]$SimulatorUsed <- ifelse(str_detect(i, "wksz"), 
                                         "Simcyp Simulator", "Simcyp Discovery")
      
      # Adjusting which details to pull based on which simulator was used.
      if(Deets[[i]]$SimulatorUsed == "Simcyp Simulator"){
         exp_details <- intersect(
            exp_details, 
            
            AllExpDetails %>% 
               filter(DataSource == "workspace or database" & 
                         SimulatorAvailability %in% c("Simulator and Discovery", 
                                                      "Simulator only")) %>% 
               pull(Detail)
         )
      } else if(Deets[[i]]$SimulatorUsed == "Simcyp Discovery"){
         exp_details <- intersect(
            exp_details, 
            
            AllExpDetails %>% 
               filter(DataSource == "workspace or database" & 
                         SimulatorAvailability %in% c("Simulator and Discovery", 
                                                      "Discovery only")) %>% 
               pull(Detail)
         )
      }
      
      workspace_xml <- tryCatch(XML::xmlTreeParse(i, useInternal = TRUE), 
                                error = unzip1st_fun(i))
      
      RootNode <- XML::xmlRoot(workspace_xml)
      
      
      ## Compound-specific info -------------------------------------------------
      
      if(any(exp_details %in% CompoundDetails)){
         
         # Extracting anything under "Compounds" on level 1 here.
         
         for(j in compoundsToExtract){
            
            CompoundNum <- switch(j, 
                                  "substrate" = 1, 
                                  "primary metabolite 1" = 5,
                                  "primary metabolite 2" = 8,
                                  "secondary metabolite" = 7,
                                  "inhibitor 1" = 2,
                                  "inhibitor 2" = 3, 
                                  "inhibitor 1 metabolite" = 6)
            
            Suffix <- AllRegCompounds %>% filter(CompoundID == j) %>% 
               pull(Suffix)
            
            # Check whether that compound was activated and skip if not. 
            if(as.logical(XML::xmlValue(RootNode[["SimulationData"]][[
               paste0("idInhEnabled", CompoundNum)]])) == FALSE){
               next
            }
            
            exp_details_cmpd <- 
               exp_details[exp_details %in% CompoundDetails &
                              exp_details %in% (XMLDeets %>% filter(CompoundID == j) %>% 
                                                   pull(Detail))]
            
            for(k in exp_details_cmpd){
               
               DeetInfo <- XMLDeets %>% 
                  filter(DataSource == "workspace or database" & Detail == k)
               DeetLevels <- t(DeetInfo[, paste0("Level", 1:7)])
               if(all(complete.cases(DeetLevels))){
                  DeetLevels <- "7"
               } else {
                  DeetLevels <- as.character(min(which(is.na(DeetLevels))) - 1)
               }
               
               # Check for a switch b/c that will change what tag we extract
               if(complete.cases(DeetInfo$XMLswitch)){
                  
                  # Check whether switch is on, i.e., set to "true" or "1". The
                  # switch and the new tag to use both will be at the same level
                  # as the original value. (At least, that's what I've
                  # encountered so far.) If the switch is ON, then use the id
                  # listed in the column "SwitchTo".
                  SwitchPosition <- 
                     switch(DeetLevels, 
                            # There shouldn't be anything that's only 1 or 2 here
                            "3" =  XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                               DeetInfo$XMLswitch]]), 
                            
                            "4" = XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                               DeetInfo$Level3]][[DeetInfo$Level4]]), 
                            
                            "5" = XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                               DeetInfo$Level3]][[DeetInfo$Level4]][[
                                  DeetInfo$Level5]]), 
                            
                            "6" = XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                               DeetInfo$Level3]][[DeetInfo$Level4]][[
                                  DeetInfo$Level5]][[DeetInfo$Level6]]),
                            
                            "7" = XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                               DeetInfo$Level3]][[DeetInfo$Level4]][[
                                  DeetInfo$Level5]][[DeetInfo$Level6]][[
                                     DeetInfo$Level7]]))
                  
                  if(SwitchPosition %in% c("1", "true")){
                     DeetInfo[, switch(DeetLevels, 
                                       "3" = "Level3", 
                                       "4" = "Level4", 
                                       "5" = "Level5",
                                       "6" = "Level6", 
                                       "7" = "Level7")] <- DeetInfo$SwitchTo
                  }
               }
               
               suppressWarnings(
                  DeetValue <- 
                     case_when(
                        # There shouldn't be anything that's only 1 or 2 here
                        DeetLevels == "3" ~
                           XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                              DeetInfo$Level3]]), 
                        
                        DeetLevels == "4" ~ 
                           XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                              DeetInfo$Level3]][[
                                 DeetInfo$Level4]]), 
                        
                        DeetLevels == "5" & 
                           k %in% c("Transporter_Gut_ABCB1_P_gp_MDR1_Apical_RAFREF", 
                                    "Transporter_Gut_ABCB1_P_gp_system") ~ 
                           XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                              DeetInfo$Level3]][[
                                 as.numeric(DeetInfo$Level4)]][[
                                    DeetInfo$Level5]]), 
                        
                        DeetLevels == "5" & 
                           !k %in% c("Transporter_Gut_ABCB1_P_gp_MDR1_Apical_RAFREF", 
                                     "Transporter_Gut_ABCB1_P_gp_system") ~ 
                           XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                              DeetInfo$Level3]][[
                                 DeetInfo$Level4]][[
                                    DeetInfo$Level5]]), 
                        
                        DeetLevels == "6" ~ 
                           XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                              DeetInfo$Level3]][[
                                 DeetInfo$Level4]][[
                                    DeetInfo$Level5]][[
                                       DeetInfo$Level6]]), 
                        
                        DeetLevels == "7" & 
                           k %in% c("ParticleSizeD10", 
                                    "ParticleSizeD50", 
                                    "ParticleSizeD90") ~ 
                           XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                              DeetInfo$Level3]][[
                                 DeetInfo$Level4]][[
                                    DeetInfo$Level5]][[
                                       as.numeric(DeetInfo$Level6)]][[
                                          DeetInfo$Level7]]), 
                        
                        DeetLevels == "7" & 
                           !k %in% c("ParticleSizeD10", 
                                     "ParticleSizeD50", 
                                     "ParticleSizeD90") ~ 
                           XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                              DeetInfo$Level3]][[
                                 DeetInfo$Level4]][[
                                    DeetInfo$Level5]][[
                                       DeetInfo$Level6]][[
                                          DeetInfo$Level7]]))
               )
               
               # Decoding as necessary. Add to the options for k as needed.
               DeetValue <- case_when(
                  str_detect(k, "Abs_model") ~  
                     case_match(DeetValue, 
                                "0" ~ "1st order", 
                                "2" ~ "ADAM"), 
                  
                  str_detect(k, "DistributionModel") ~ 
                     case_match(DeetValue, 
                                "1" ~ "Full PBPK Model", 
                                "0" ~ "Minimal PBPK Model"), 
                  
                  str_detect(k, "DoseRoute") ~ 
                     case_match(DeetValue, 
                                "1" ~ "i.v. bolus", 
                                "2" ~ "Oral", 
                                .default = DeetValue), 
                  
                  str_detect(k, "Formulation") ~ 
                     case_match(DeetValue, 
                                "0" ~ "solution", 
                                "1" ~ "solid", 
                                "2" ~ "solution with precipitation", 
                                "3" ~ "suspension", 
                                .default = DeetValue), 
                  
                  str_detect(k, "MetabolicSystemInVitro") ~ 
                     case_match(DeetValue, 
                                "0" ~ "recombinant", 
                                "1" ~ "HLM"), 
                  
                  str_detect(k, "Permeability_reference_lock") ~
                     case_match(DeetValue, 
                                "true" ~ "locked", 
                                "false" ~ "unlocked"), 
                  
                  TRUE ~ DeetValue)
               
               suppressWarnings(
                  DeetValue <- switch(DeetInfo$Class, 
                                      "numeric" = as.numeric(DeetValue), 
                                      "character" = as.character(DeetValue))
               )
               
               # Adjusting when things need to be set to NA when they don't
               # apply. Peff is complicated, so it's separate from this.
               if(str_detect(k, "Qgut_userinput") & 
                  XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][["QGutSwitch"]]) == "1"){
                  DeetValue <- NA
               }
               
               Deets[[i]][[k]] <- DeetValue 
               
               rm(DeetInfo, DeetLevels, DeetValue)
            }
            rm(CompoundNum)
            
            # Peff and its complications... 
            
            # predicted or user input?
            if(paste0("Peff_pred_or_user", Suffix) %in% names(Deets[[i]]) &&
               Deets[[i]][[paste0("Peff_pred_or_user", Suffix)]] == "1"){
               Deets[[i]][[paste0("Peff_pred_or_user", Suffix)]] <- "predicted"
               
               Deets[[i]][[paste0("Peff_human", Suffix)]] <- 
                  paste("NOT SURE IF THIS VALUE IS RELIABLE:", 
                        Deets[[i]][[paste0("Peff_human", Suffix)]], 
                        "We're having trouble finding this number reliably in the workspace.")
               
               # predicted with Papp or MechPeff?
               if(paste0("Peff_prediction_method", Suffix) %in% names(Deets[[i]]) &&
                  Deets[[i]][[paste0("Peff_prediction_method", Suffix)]] == "5"){
                  Deets[[i]][[paste0("Peff_prediction_method", Suffix)]] <- 
                     "predicted with Papp"
                  
                  # Not currently including anything on whether it was passive
                  # or passive and active or anything on the slope and
                  # intercept, but that info would go here. 
                  
                  Deets[[i]][[paste0("Peff_user_input_Ptrans0", Suffix)]] <- as.character(NA)
                  Deets[[i]][[paste0("Peff_MechPeff_totalvsfree", Suffix)]] <- as.character(NA)
                  
               } else if(paste0("Peff_prediction_method", Suffix) %in% names(Deets[[i]]) &&
                         Deets[[i]][[paste0("Peff_prediction_method", Suffix)]] == "6"){
                  Deets[[i]][[paste0("Peff_prediction_method", Suffix)]] <- 
                     "predicted with MechPeff"
                  
                  Deets[[i]][[paste0("Peff_MechPeff_totalvsfree", Suffix)]] <- 
                     ifelse(Deets[[i]][[paste0("Peff_MechPeff_totalvsfree", Suffix)]] == "0", 
                            "total", "free")
                  
               } else {
                  Deets[[i]][[paste0("Peff_prediction_method", Suffix)]] <- 
                     paste("Apologies, but you have a value we don't know how to decode. Value listed in workspace or database:", 
                           Deets[[i]][[paste0("Peff_prediction_method", Suffix)]])
                  
                  Deets[[i]][[paste0("Peff_user_input_Ptrans0", Suffix)]] <- as.character(NA)
                  Deets[[i]][[paste0("Peff_MechPeff_totalvsfree", Suffix)]] <- as.character(NA)
               }
               
            } else if(all(c(paste0("Peff_pred_or_user", Suffix), 
                            paste0("Peff_prediction_method", Suffix), 
                            paste0("Peff_user_input_Ptrans0", Suffix), 
                            paste0("Peff_MechPeff_totalvsfree", Suffix)) %in% names(Deets[[i]]))){
               
               Deets[[i]][[paste0("Peff_pred_or_user", Suffix)]] <- "user input"
               Deets[[i]][[paste0("Peff_prediction_method", Suffix)]] <- as.character(NA)
               Deets[[i]][[paste0("Peff_user_input_Ptrans0", Suffix)]] <- as.character(NA)
               Deets[[i]][[paste0("Peff_MechPeff_totalvsfree", Suffix)]] <- as.character(NA)
               
            }
            
            # Removing things that do not apply, e.g., ADAM-model parameters
            # when it was a 1st-order absorption model
            if(Deets[[i]][[paste0("Abs_model", Suffix)]] == "1st order"){
               Keep <- setdiff(names(Deets[[i]]), 
                               
                               AllExpDetails$Detail[
                                  AllExpDetails$ADAMParameter == TRUE])
               
               Deets[[i]] <- Deets[[i]][Keep]
            }
         }
      }
      
      
      ## Other general info -------------------------------------------------
      
      if(length(setdiff(exp_details, CompoundDetails)) >= 1){
         
         # Extracting anything that was NOT under "Compounds" on level 1 here.
         
         for(m in setdiff(exp_details, CompoundDetails)){
            
            DeetInfo <- XMLDeets %>% 
               filter(DataSource == "workspace or database" & Detail == m) %>% 
               mutate(Level2 = ifelse(m %in% PopulationDetails, 
                                      as.numeric(Level2), as.character(Level2)))
            DeetLevels <- t(DeetInfo[, paste0("Level", 1:7)])
            DeetLevels <- as.character(min(which(is.na(DeetLevels))) - 1)
            
            if(DeetLevels == 0){next} # This is 0 when it's only set up for getting database parameters and not for extracting from workspace.
            
            DeetValue <- 
               switch(DeetLevels, 
                      "2" = XML::xmlValue(RootNode[[
                         DeetInfo$Level1]][[
                            DeetInfo$Level2]]),
                      
                      "3" =  XML::xmlValue(RootNode[[
                         DeetInfo$Level1]][[
                            DeetInfo$Level2]][[
                               DeetInfo$Level3]]), 
                      
                      "4" = XML::xmlValue(RootNode[[
                         DeetInfo$Level1]][[
                            DeetInfo$Level2
                         ]][[
                            DeetInfo$Level3]][[
                               DeetInfo$Level4]]), 
                      
                      "5" = XML::xmlValue(RootNode[[
                         DeetInfo$Level1]][[
                            DeetInfo$Level2]][[
                               DeetInfo$Level3]][[
                                  DeetInfo$Level4]][[
                                     DeetInfo$Level5]]), 
                      
                      "6" = XML::xmlValue(RootNode[[
                         DeetInfo$Level1]][[
                            DeetInfo$Level2]][[
                               DeetInfo$Level3]][[
                                  DeetInfo$Level4]][[
                                     DeetInfo$Level5]][[
                                        DeetInfo$Level6]]), 
                      
                      "7" = XML::xmlValue(RootNode[[
                         DeetInfo$Level1]][[
                            DeetInfo$Level2]][[
                               DeetInfo$Level3]][[
                                  DeetInfo$Level4]][[
                                     DeetInfo$Level5]][[
                                        DeetInfo$Level6]][[
                                           DeetInfo$Level7]]))
            
            DeetValue <- switch(DeetInfo$Class, 
                                "numeric" = as.numeric(DeetValue), 
                                "character" = as.character(DeetValue))
            
            # There will be some cases where we don't switch *to* some other
            # value but just need to set the value to NA or adjust things in
            # some way. Dealing with those here.
            if(m == "ObsOverlayFile"){
               if(XML::xmlValue(RootNode[["GraphsData"]][["UseObservedData"]]) == "false"){
                  DeetValue <- NA
               } 
            }
            
            if(m == "FixedTrialDesignFile" & 
               XML::xmlValue(RootNode[["SimulationData"]][["FixedIndividualTrialDesign"]]) == "false"){
               DeetValue <- NA
            }
            
            # Also dealing with instances where the value in the XML file is
            # coded to make it clear what the actual value in the simulation was
            # and to match what the user would see in the Excel results. NB: I
            # was first attempting to do this with case_when(m == ...) and then
            # case_match for each, but that requires all the data types to be
            # the same, which they are NOT. Thus the multiple if and if else
            # statments.
            if(m == "CYP3A4_ontogeny_profile"){
               DeetValue <- case_match(DeetValue, 
                                       "0" ~ "CYP3A4 Profile 1", 
                                       "1" ~ "CYP3A4 Profile 2")
            } 
            
            Deets[[i]][[DeetInfo$Detail]] <- DeetValue
            
            rm(DeetInfo, DeetLevels, DeetValue)
         }
      }
      
      # Adding compound names separately. 
      for(j in c("Substrate", "Inhibitor1", "Inhibitor2", 
                 "PrimaryMetabolite1", "PrimaryMetabolite2", 
                 "SecondaryMetabolite", "Inhibitor1Metabolite")){
         
         CompoundNum <- switch(j, 
                               "Substrate" = 1, 
                               "PrimaryMetabolite1" = 5,
                               "PrimaryMetabolite2" = 8,
                               "SecondaryMetabolite" = 7,
                               "Inhibitor1" = 2,
                               "Inhibitor2" = 3, 
                               "Inhibitor1Metabolite" = 6)
         
         # Check whether that compound was activated and skip if not. 
         if(as.logical(XML::xmlValue(RootNode[["SimulationData"]][[
            paste0("idInhEnabled", CompoundNum)]])) == FALSE){
            next
         }
         
         Deets[[i]][[j]] <- 
            XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][["idName"]])
      }
      
      Deets[[i]] <- as.data.frame(Deets[[i]]) %>% mutate(Workspace = i)
      
      rm(workspace_xml, RootNode)
      
   }
   
   Deets <- bind_rows(Deets)
   Deets[Deets == ""] <- NA
   
   # Sorting to help organize output
   Deets <- Deets %>% select(Workspace, everything())
   
   # Saving & harmonizing ------------------------------------------------------
   
   if(complete.cases(save_output)){
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
             "csv" = write.csv(as.data.frame(Deets), FileName, row.names = F), 
             "xlsx" = openxlsx::write.xlsx(as.data.frame(Out$MainDetails), 
                                           FileName))
   }
   
   # Temporarily calling "Workspace" "File" so that we can harmonize... 
   Out <- harmonize_details(Deets %>% rename(File = Workspace))
   
   # ...and then changing it back.
   Out <- map(Out, 
              .f = \(x) if(is.null(nrow(x)) == FALSE){
                 x %>% rename(Workspace = "File")})
   
   return(Out)
   
}


