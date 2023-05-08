#' Change interaction parameters in Simcyp Simulator workspace files
#'
#' \code{change_wksz_interactions} changes interaction parameters such as Kapp
#' or Ki or IndMax in workspace files (.wksz files) for then running with the
#' Simcyp Simulator. This will change \emph{all} the workspace files provided to
#' have the \emph{same} interaction parameters listed here. Please note that if
#' you want to change one interaction parameter for a given CYP, you must must
#' set \emph{all} the interaction parameters for that CYP to what you want. For
#' example, say your drug has a Ki of 1 uM and that it should have an IndMax of
#' 2, but Indmax is not the correct value in your workspace file. You will need
#' to set \code{IndMax = 2} AND ALSO \code{Ki = 1}. Set the value to NA to use
#' the default Simcyp Simulator value. UNDER CONSTRUCTION.
#'
#' @param sim_workspace_files the set of workspace files to modify; must end in
#'   ".wksz"
#' @param new_sim_workspace_files optionally specify the new workspace file
#'   names to use. If left as NA, the original workspace will be overwritten.
#'   Otherwise, specify a character vector of file names to use, e.g.,
#'   \code{new_sim_workspace_files = c("new file 1.wksz", "new file
#'   2.wksz")}
#' @param interactions_to_set a data.frame of interaction parameters to use
#'   instead of filling in each of the arguments below (CompoundID through
#'   Ind_slope) with character vectors of values. Columns MUST match the names
#'   of those arguments, e.g., "CompoundID", "CYP",
#'   "competitive_inhibition_switch", "Ki", "Ki_fumic", "TDI_switch", "Kapp",
#'   "kinact", "Kapp_fumic", "induction_IndC50_switch", "IndMax", "IndC50",
#'   "IndC50_fuinc", "induction_slope_switch", "Ind_slope", and "Ind_gamma".
#' @param CompoundID Compound ID whose parameters you want to change. Options
#'   are: \itemize{\item{"substrate",} \item{"primary metabolite 1",}
#'   \item{"primary metabolite 2",} \item{"secondary metabolite",}
#'   \item{"inhibitor 1" (default) -- this can be an inducer, inhibitor,
#'   activator, or suppresesor, but it's labeled as "Inhibitor 1" in the
#'   simulator,} \item{"inhibitor 2" for the 2nd inhibitor listed in the
#'   simulation,} \item{"inhibitor 1 metabolite" for the primary metabolite of
#'   inhibitor 1}}
#' @param CYP CYP whose interaction parameters you want to change. Note: This is
#'   currently only set up to change CYP interaction parameters. If you need to
#'   change interaction parameters for some other enzyme or transporter, please
#'   talk to Laura Shireman.
#' @param competitive_inhibition_switch turn competitive inhibition "on" or
#'   "off"
#' @param Ki Ki value to use for competitive inhibition (uM); if left as NA,
#'   value will be set to 1E+06.
#' @param Ki_fumic fu,mic value to use for competitive inhibition; if left as
#'   NA, value will be set to 1.
#' @param TDI_switch turn time-dependent inactivation "on" or "off" (default)
#' @param Kapp Kapp value to use for TDI (uM); if left as NA, value will be set
#'   to 1E+06.
#' @param kinact kinact value to use for TDI (1/h); if left as NA, value will be
#'   set to 0.
#' @param Kapp_fumic fu,mic value to use for TDI; if left as NA, value will be
#'   set to 1.
#' @param induction_IndC50_switch turn "on" or "off" (default) the use of an
#'   induction IndC50 value
#' @param IndMax maximum fold induction to use
#' @param IndC50 IndC50 value to use (uM); if left as NA, value will be set to
#'   1E+06.
#' @param IndC50_fuinc fu,mic value to use for the IndC50 incubation; if left as
#'   NA, value will be set to 1.
#' @param Ind_gamma value to use for gamma aka the Hill coefficient for
#'   induction; if left as NA, value will be set to 1.
#' @param induction_slope_switch turn "on" or "off" (default) the use of a slope
#'   value for describing induction
#' @param Ind_slope value to use for slope in induction; if left as NA, value
#'   will be set to 0.
#'
#' @return does not return anything in R but saves workspace files
#' @export
#'
#' @examples
#' # None yet
#' 
change_wksz_interactions <- function(sim_workspace_files,
                                     new_sim_workspace_files = NA,
                                     
                                     interactions_to_set = NA,
                                     
                                     CompoundID = "inhibitor 1",
                                     CYP = NA, 
                                     
                                     competitive_inhibition_switch = "off",
                                     Ki = NA, 
                                     Ki_fumic = NA,
                                     
                                     TDI_switch = "off",
                                     Kapp = NA,
                                     kinact = NA, 
                                     Kapp_fumic = NA,
                                     
                                     induction_IndC50_switch = "off",
                                     IndMax = NA, 
                                     IndC50 = NA, 
                                     IndC50_fuinc = NA, 
                                     Ind_gamma = NA,
                                     induction_slope_switch = "off",
                                     Ind_slope = NA){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If they didn't include the file suffix, add that. Replace any "xlsx" file extensions with "wksz".
   if(any(str_detect(sim_workspace_files, "wksz") == FALSE)){
      sim_workspace_files <- paste0(sub("\\.xlsx", "", sim_workspace_files), ".wksz")
   }
   if(any(complete.cases(new_sim_workspace_files)) && 
      any(str_detect(new_sim_workspace_files, "wksz") == FALSE)){
      new_sim_workspace_files <- paste0(sub("\\.xlsx", "", new_sim_workspace_files), ".wksz")
   }
   
   if(any(complete.cases(new_sim_workspace_files)) &&
      (length(new_sim_workspace_files) != length(sim_workspace_files)) &
      length(sim_workspace_files) != 1){
      stop(paste("You have provided", length(sim_workspace_files), "original workspace file names and", 
                 length(new_sim_workspace_files), 
                 "new workspace file names. You must provide the same number of original and new file names for this function to work or else list just one value for `sim_workspace_files`, in which case the original workspace will be used as a template for the new ones."),
           call. = FALSE)
   }
   
   # If they did not provide a value for new_sim_workspace_files, then they
   # must want the orginial file names to be overwritten.
   if(any(is.na(new_sim_workspace_files))){
      new_sim_workspace_files <- sim_workspace_files
   }
   
   FilesToChange <- data.frame(Orig = sim_workspace_files, 
                               New = new_sim_workspace_files)
   # LEFT OFF HERE. RETURN TO THIS. Need to decide how I want this to work.
   # Should user specify one orig file and then multiple new files and then
   # adjust each param accordingly? Should all params for all files be same?
   # Think about how best to set this up.
      
   if(class(interactions_to_set) != "logical"){
      if("data.frame" %in% class(interactions_to_set) == FALSE){
         stop("You have supplied an object for `interactions_to_set`, but it's not a data.frame, which is what it needs to be. Please check your input and the help file and try again.", 
              call. = FALSE)
      }
      
      if(all(c("CompoundID", "CYP","competitive_inhibition_switch", "Ki", 
               "Ki_fumic", "TDI_switch", "Kapp", "kinact", "Kapp_fumic",
               "induction_IndC50_switch", "IndMax", "IndC50",
               "IndC50_fuinc", "induction_slope_switch", "Ind_slope", 
               "Ind_gamma") %in% names(interactions_to_set)) == FALSE){
         stop("You have supplied a data.frame for `interactions_to_set`, but the following columns are missing and must be added for this function to work:", 
              str_comma(setdiff(c("CompoundID", "CYP","competitive_inhibition_switch", "Ki", 
                                  "Ki_fumic", "TDI_switch", "Kapp", "kinact", "Kapp_fumic",
                                  "induction_IndC50_switch", "IndMax", "IndC50",
                                  "IndC50_fuinc", "induction_slope_switch", "Ind_slope", 
                                  "Ind_gamma"), 
                                names(interactions_to_set))),
              "Please check your input and the help file and try again.", 
              call. = FALSE)
      }
   } else {
      
      # If they did not supply a data.frame, convert the args into a data.frame.
      interactions_to_set <- list("CompoundID" = CompoundID,
                                  "CYP" = CYP,
                                  "competitive_inhibition_switch" = competitive_inhibition_switch,
                                  "Ki" = Ki, 
                                  "Ki_fumic" = Ki_fumic, 
                                  "TDI_switch" = TDI_switch, 
                                  "Kapp" = Kapp, 
                                  "kinact" = kinact,
                                  "Kapp_fumic" = Kapp_fumic,
                                  "induction_IndC50_switch" = induction_IndC50_switch,
                                  "IndMax" = IndMax,
                                  "IndC50" = IndC50,
                                  "IndC50_fuinc" = IndC50_fuinc,
                                  "induction_slope_switch" = induction_slope_switch,
                                  "Ind_slope" = Ind_slope, 
                                  "Ind_gamma" = Ind_gamma)
      
      ArgLengthCheck <- sapply(interactions_to_set, length)
      ArgLengthCheck <- ArgLengthCheck[!ArgLengthCheck == 1]
      
      if(length(unique(ArgLengthCheck)) > 1){
         stop("You have not provided the same number of inputs for each of the arguments `CompoundID` through `Ind_gamma`, and all of these must have the same number of inputs or have just a single value, which will be repeated for all CYPs. Please check this and try again.", 
              call. = FALSE)
      }
      
      interactions_to_set <- as.data.frame(interactions_to_set)
      
   }
   
   # interactions_to_set should now be a data.frame and should have the same
   # number of rows as there are output workspace files. Checking that here.
   if((any(complete.cases(new_sim_workspace_files)) &
       nrow(interactions_to_set) != length(new_sim_workspace_files)) |
      (all(is.na(new_sim_workspace_files) &
           nrow(interactions_to_set) != length(sim_workspace_files))) ){
      stop("You have provided a different number of options for at least some of the parameters than you have output file names. We don't know which parameters you want to use for which output file. (For example, maybe you've listed 2 values for Kapp but only have one output file name.) Please check your input and the help file and try again.", 
           call. = FALSE)
   }
   
   # Adding the file names to interactions_to_set
   interactions_to_set$Orig <- FilesToChange$Orig
   interactions_to_set$New <- FilesToChange$New
   
   
   # Main body of function ---------------------------------------------------
   
   for(i in 1:nrow(FilesToChange)){
      
      workspace_xml <- XML::xmlTreeParse(FilesToChange$Orig[i], useInternal = TRUE)
      RootNode <- XML::xmlRoot(workspace_xml)
      
      for(j in 1:nrow(interactions_to_set)){
         
         Ints <- interactions_to_set[j, ]
         
         CompoundIDnum <- switch(Ints$CompoundID, 
                                 "substrate" = 1, 
                                 "inhibitor 1" = 2, 
                                 "inhibitor 2" = 3, 
                                 "inhibitor 3" = 4, 
                                 "primary metabolite 1" = 5, 
                                 "inhibitor 1 metabolite" = 6,
                                 "secondary metabolite" = 7, 
                                 "primary metabolite 2" = 8)
         CYP <- sub("CYP", "", Ints$CYP)
         CYPnum <- switch(CYP,
                          "1A2" = 1, 
                          "2A6" = 2, 
                          "2B6" = 3,
                          "2C8" = 4, 
                          "2C9" = 5, 
                          "2C18" = 6, 
                          "2C19" = 7,
                          "2D6" = 8, 
                          "2E1" = 9, 
                          "2J2" = 10,
                          "3A4" = 11, 
                          "3A5" = 12, 
                          "3A7" = 13)
         
         # If they specified a value for something but didn't turn the switch
         # on, fix that for them. I mean for the "switch" arguments mainly to be
         # an opportunity to turn off interactions they no longer want. Note
         # that IndMax does not require a switch.
         ArgSwitchCheck <- Ints %>% select(-matches("switch"), -IndMax) %>% 
            pivot_longer(cols = -c("CompoundID", "CYP"), 
                         names_to = "Param", values_to = "Value") %>% 
            mutate(ParamSet = case_when(Param %in% c("Ki", "Ki_fumic") ~ "Competitive",
                                        
                                        Param %in% c("Kapp", "kinact", 
                                                     "Kapp_fumic") ~ "TDI", 
                                        
                                        Param %in% c("IndC50", "IndC50_fuinc",
                                                     "Ind_gamma") ~ "IndC50", 
                                        
                                        Param %in% c("Ind_slope") ~ "IndSlope")) %>% 
            left_join(data.frame(ParamSet = c("Competitive", "TDI", 
                                              "IndC50", "IndSlope"), 
                                 Switch = c(Ints$competitive_inhibition_switch, 
                                            Ints$TDI_switch, 
                                            Ints$IndC50_switch, 
                                            Ints$Ind_slope_switch)), 
                      by = "ParamSet")
         
         ArgSwitchCheck <- ArgSwitchCheck %>% filter(Switch == "off") %>% 
            group_by(ParamSet) %>% 
            summarize(Check = any(complete.cases(Value))) %>% 
            ungroup() %>% 
            filter(Check == TRUE)
         
         
         if("Competitive" %in% ArgSwitchCheck$ParamSet){
            Ints$competitive_inhibition_switch <- "on"
         }
         
         if("TDI" %in% ArgSwitchCheck$ParamSet){
            Ints$TDI_switch <- "on"
         }
         
         if("IndC50" %in% ArgSwitchCheck$ParamSet){
            Ints$induction_IndC50_switch <- "on"
         }
         
         if("IndSlope" %in% ArgSwitchCheck$ParamSet){
            Ints$induction_slope_switch <- "on"
         }
         
         
         ## Competitive inhibition -----------------------------------------
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["KiSwitch"]]) <- 
            ifelse(Ints$competitive_inhibition_switch == "on", "true", "false")
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["Ki"]]) <- 
            ifelse(is.na(Ints$Ki), "1000000", Ints$Ki)
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["Fumic"]]) <- 
            ifelse(is.na(Ints$Ki_fumic), 1, Ints$Ki_fumic)
         
         
         ## TDI --------------------------------------------------------
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["KappSwitch"]]) <- 
            ifelse(Ints$TDI_switch == "on", "true", "false")
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["Kapp"]]) <-
            ifelse(is.na(Ints$Kapp), "1000000", Ints$Kapp)
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["Kinact"]]) <- 
            ifelse(is.na(Ints$kinact), 0, Ints$kinact)
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["Kapp_fumic"]]) <-
            ifelse(is.na(Ints$Kapp_fumic), 1, Ints$Kapp_fumic)
         
         ## Induction ----------------------------------------------------
         
         # No switch for Indmax
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["IndMax"]]) <- 
            ifelse(is.na(Ints$IndMax), 1, Ints$IndMax)
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["IndC50Switch"]]) <- 
            ifelse(Ints$induction_IndC50_switch == "on", "true", "false")
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["IndC50"]]) <- 
            ifelse(is.na(Ints$IndC50), "1000000", Ints$IndC50)
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["IndC50_fuinc"]]) <-
            ifelse(is.na(Ints$IndC50_fuinc), 1, Ints$IndC50_fuinc)
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["Y"]]) <- 
            ifelse(is.na(Ints$Ind_gamma), 1, Ints$Ind_gamma)
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["IndSlopeSwitch"]]) <-
            ifelse(Ints$induction_slope_switch == "on", "true", "false")
         
         XML::xmlValue(RootNode[["Compounds"]][[CompoundIDnum]][[
            "CYPInteractionRoutes"]][[CYPnum]][["IndSlope"]]) <- 
            ifelse(is.na(Ints$Ind_slope), 0, Ints$Ind_slope)
         
         rm(Ints, CYPnum, CompoundIDnum, CYP)
         
      }
      
      ## Saving -------------------------------------------------------------
      XML::saveXML(workspace_xml, file = "temp.xml")
      R.utils::gzip(filename = "temp.xml", 
                    destname = FilesToChange$New[i],
                    remove = TRUE, overwrite = TRUE)
      
      rm(workspace_xml, RootNode)
   }
}


