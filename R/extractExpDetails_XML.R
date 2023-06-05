#' Extract experimental details for multiple Simcyp Simulator workspace files at
#' once
#'
#' \code{extractExpDetails_XML} takes a character vector of Simcyp Simulator
#' workspaces -- or all the workspace files in the current directory if no files
#' are specified -- and collects experimental details for the simulations into a
#' single table. It optionally saves that table to a csv or Excel file.
#' 

#' @param sim_workspace_files a character vector of simulator output files, each
#'   in quotes and encapsulated with \code{c(...)}, NA to extract experimental
#'   details for \emph{all} the Simulator workspace files in the current folder,
#'   or "recursive" to extract experimental details for \emph{all} the Simulator
#'   workspace files in the current folder and \emph{all} subfolders. Example 
#'   of acceptable input: \code{c("sim1.wksz", "sim2.wksz")}.
#' @param exp_details experimental details you want to extract from the
#'   simulator workspace files; currently "all" is the only acceptable input and
#'   anything else will be ignored. These are much more limited than the options
#'   for the function \code{\link{extractExpDetails}} and currently only include
#'   calculated Peff,human, predicted Vss, the XML overlay file used for
#'   observed data, and the XML file used for a fixed-trial design.
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
#'   details.xlsx". If you leave off the file extension, it will be saved as a
#'   csv file.
#'
#' @return Returns a data.frame of the experimental details
#' @export
#'
#' @examples
#'
#' # None yet
#' 



extractExpDetails_XML <- function(sim_workspace_files,
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
   if(length(sim_data_files) == 1 &&
      (is.na(sim_data_files) | sim_data_files == "recursive")){
      sim_data_files <- list.files(pattern = "wksz$",
                                   recursive = (complete.cases(sim_data_files) &&
                                                   sim_data_files == "recursive"))
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   }
   
   # If they didn't include ".wksz" at the end, add that.
   sim_workspace_files[str_detect(sim_workspace_files, "\\.wksz$") == FALSE] <-
      paste0(sim_workspace_files[str_detect(sim_workspace_files, "\\.wksz$") == FALSE], 
             ".wksz")
   
   # Making sure that all the files exist before attempting to pull data
   if(any(file.exists(sim_workspace_files) == FALSE)){
      MissingSimFiles <- sim_workspace_files[
         which(file.exists(sim_workspace_files) == FALSE)]
      warning(paste0("The file(s) ", 
                     str_comma(paste0("`", MissingSimFiles, "`")), 
                     " is/are not present and thus will not be extracted."), 
              call. = FALSE)
      sim_workspace_files <- setdiff(sim_workspace_files, MissingSimFiles)
   }
   
   # Checking compound IDs
   compoundsToExtract <- tolower(compoundsToExtract)
   
   MainCompoundIDs <- c("substrate", "primary metabolite 1", "primary metabolite 2",
                        "secondary metabolite",
                        "inhibitor 1", "inhibitor 2", "inhibitor 1 metabolite",
                        "inhibitor 2 metabolite")
   
   PossCmpd <- c(MainCompoundIDs, "all")
   
   if(any(compoundsToExtract %in% PossCmpd == FALSE)){
      warning(paste0("The compound(s) ", 
                     str_comma(paste0("`", setdiff(compoundsToExtract, PossCmpd), "`")),
                     " is/are not among the possible componds to extract and will be ignored. The possible compounds to extract are only exactly these: ",
                     str_comma(paste0("`", PossCmpd, "`"))), 
              call. = FALSE)
      compoundsToExtract <- intersect(compoundsToExtract, PossCmpd)
   }
   
   if(any(compoundsToExtract == "all")){
      compoundsToExtract <- MainCompoundIDs
      
      # HACK: For now, since I'm not sure which details are available for
      # which compound IDs, only setting this function up to work for
      # substrate, inhibitor 1, and inhibitor 2.
      compoundsToExtract <- c("substrate", "inhibitor 1", "inhibitor 2")
   }
   
   
   # Main body of function ---------------------------------------------------
   
   if("all" %in% exp_details){
      exp_details <- AllExpDetails %>% filter(Sheet == "workspace XML file") %>% 
         pull(Detail)
   }
   
   CompoundDetails <- AllExpDetails %>% 
      filter(Sheet == "workspace XML file" & Level1 == "Compounds" & 
                !Detail %in% c("Substrate", "Inhibitor1", "Inhibitor2", 
                               "PrimaryMetabolite1", "PrimaryMetabolite2", 
                               "SecondaryMetabolite", "Inhibitor1Metabolite")) %>% 
      pull(Detail)
   
   Deets <- list()
   
   for(i in sim_workspace_files){
      
      Deets[[i]] <- list()
      
      workspace_xml <- XML::xmlTreeParse(i, useInternal = TRUE)
      RootNode <- XML::xmlRoot(workspace_xml)
      
      if(any(exp_details %in% CompoundDetails)){
         
         for(j in compoundsToExtract){
            
            CompoundNum <- switch(j, 
                                  "substrate" = 1, 
                                  "primary metabolite 1" = 5,
                                  "primary metabolite 2" = 8,
                                  "secondary metabolite" = 7,
                                  "inhibitor 1" = 2,
                                  "inhibitor 2" = 3, 
                                  "inhibitor 1 metabolite" = 6)
            
            
            for(k in CompoundDetails){
               
               DeetInfo <- AllExpDetails %>% 
                  filter(Sheet == "workspace XML file" & Detail == k)
               DeetLevels <- t(DeetInfo[, paste0("Level", 1:5)])
               DeetLevels <- as.character(min(which(is.na(DeetLevels))) - 1)
               
               DeetValue <- 
                  switch(DeetLevels, 
                         # There shouldn't be anything that's only 1 or 2 here
                         "3" =  XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                            DeetInfo$Level3]]), 
                         "4" = XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                            DeetInfo$Level4]]), 
                         "5" = XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][[
                            DeetInfo$Level3]][[
                               DeetInfo$Level4]][[
                                  DeetInfo$Level5]]))
               
               DeetValue <- switch(DeetInfo$Class, 
                                   "numeric" = as.numeric(DeetValue), 
                                   "character" = as.character(DeetValue))
               
               Deets[[i]][[gsub("_X$", switch(j, 
                                              "substrate" = "_sub", 
                                              "primary metabolite 1" = "_met1",
                                              "primary metabolite 2" = "_met2",
                                              "secondary metabolite" = "_secmet",
                                              "inhibitor 1" = "_inhib",
                                              "inhibitor 2" = "_inhib2", 
                                              "inhibitor 1 metabolite" = "_inhib1met"),
                                k)]] <- DeetValue 
               
               rm(DeetInfo, DeetLevels, DeetValue)
            }
            rm(CompoundNum)
         }
      }
      
      if(length(setdiff(exp_details, CompoundDetails)) >= 1){
         for(m in setdiff(exp_details, CompoundDetails)){
            
            DeetInfo <- AllExpDetails %>% 
               filter(Sheet == "workspace XML file" & Detail == m)
            DeetLevels <- t(DeetInfo[, paste0("Level", 1:5)])
            DeetLevels <- as.character(min(which(is.na(DeetLevels))) - 1)
            
            DeetValue <- 
               switch(DeetLevels, 
                      "2" = XML::xmlValue(RootNode[[DeetInfo$Level1]][[
                         DeetInfo$Level2]]), 
                      "3" =  XML::xmlValue(RootNode[[DeetInfo$Level1]][[
                         DeetInfo$Level2]][[DeetInfo$Level3]]), 
                      "4" = XML::xmlValue(RootNode[[DeetInfo$Level1]][[
                         DeetInfo$Level2]][[DeetInfo$Level3]][[DeetInfo$Level4]]), 
                      "5" = XML::xmlValue(RootNode[[DeetInfo$Level1]][[
                         DeetInfo$Level2]][[DeetInfo$Level3]][[DeetInfo$Level4]][[
                            DeetInfo$Level5]]))
            
            DeetValue <- switch(DeetInfo$Class, 
                                "numeric" = as.numeric(DeetValue), 
                                "character" = as.character(DeetValue))
            
            Deets[[i]][[DeetInfo$Detail]] <- DeetValue
            
            rm(DeetInfo, DeetLevels, DeetValue)
         }
      }
      
      # Adding compound names separately. Trying to figure out how to tell if
      # these are "on" in the simulation, but I am really struggling to find
      # that tag name.
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
         
         Deets[[i]][[j]] <- 
            XML::xmlValue(RootNode[["Compounds"]][[CompoundNum]][["idName"]])
      }
      
      Deets[[i]] <- as.data.frame(Deets[[i]]) %>% mutate(Workspace = i)
      
      rm(workspace_xml, RootNode)
      
   }
   
   Deets <- bind_rows(Deets)
   Deets[Deets == ""] <- NA
   
   return(Deets)
   
}
