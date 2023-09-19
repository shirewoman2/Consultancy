#' Set the user name in the path of observed data overlay XML files and fixed
#' trial design XML files
#'
#' \code{change_xml_path} changes the path of XML files that are included in
#' simulations, which can be useful if one person originally created the
#' workspace and a separate person then wants to run the files since the XML
#' file paths on SharePoint include the users's name. UNDER CONSTRUCTION.
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
#' @param workspace_objects instead of workspace files, specify a list of R
#'   objects for the workspaces you wish to modify. The overall object should be
#'   a list, even if it has a length of 1, but each item in the list should be
#'   of the class "XMLInternalDocument", e.g., the R object you get when you run
#'   \code{XML::xmlTreeParse(...)} on a workspace file. If you're not sure what
#'   we mean here, you probably don't want this option; it's mainly for internal
#'   use in the package.
#' @param new_xml_path specify the new path for the XML files. Options:
#'   \describe{\item{"only change user" (default)}{leaves intact all of the path
#'   except for the user name, which is useful when, say, Lisa has made a
#'   workspace on SharePoint but Hannah would now like to run those files}
#'   \item{state the new folder path}{useful when you want complete control over
#'    the folder location. Don't include the XML file name -- only the folder
#'   where it's located, enclosed with quotes. It should contain ONLY forward
#'   slashes, so the usual way of specifying paths in R. We'll fix it to be
#'    Windows compliant for you.}}
#' @param save_workspaces TRUE (default) or FALSE for whether to actually save
#'   the workspaces. Set this to FALSE if you would instead like to have the
#'   workspaces be returned as an R object (generally when you're piping this
#'   function into another one).
#'
#' @return saves workspace files or returns an R object of workspaces
#' @export
#'
#' @examples
#' # None yet
#' 
change_xml_path <- function(sim_workspace_files = NA,
                            new_sim_workspace_files = NA,
                            workspace_objects = NA,
                            new_xml_path = "only change user", 
                            save_workspaces = TRUE){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if(class(workspace_objects) != "list"){
      sim_workspace_files <- sim_workspace_files[complete.cases(sim_workspace_files)]
      if(length(sim_workspace_files) == 0){
         sim_workspace_files <- list.files(pattern = ".wksz")
      } else if(any(sim_workspace_files == "recursive")){
         sim_workspace_files <- list.files(pattern = ".wksz", recursive = TRUE)
      }
   }
   
   # If they did not provide a value for new_sim_workspace_files, then they
   # must want the orginal file names to be overwritten.
   if(all(is.na(new_sim_workspace_files))){
      new_sim_workspace_files <- sim_workspace_files
   }
   
   if(length(sim_workspace_files) == 0 & class(workspace_objects) != "list"){
      return("No workspace files could be found to change.")
   }
   
   # Checking for mismatches between new and old file name lengths
   if(any(complete.cases(new_sim_workspace_files)) &&
      (class(workspace_objects) == "list" && 
       length(new_sim_workspace_files) != length(workspace_objects)) |
      (class(workspace_objects) != "list" && 
       length(new_sim_workspace_files) != length(sim_workspace_files))){
      stop(paste("You have provided", length(sim_workspace_files), "original workspace file names and",
                 length(new_sim_workspace_files),
                 "new workspace file names. You must provide the same number of original and new file names for this function to work or else list NA for `new_sim_workspace_files`, in which case the original workspaces will be overwritten with the new parameters."),
           call. = FALSE)
   }
   
   # Main body of function ---------------------------------------------------
   
   if(class(workspace_objects) == "list"){
      MyXMLs <- workspace_objects
   } else {
      MyXMLs <- list()
   }
   
   NewXMLs <- switch(as.character(class(workspace_objects) == "list"), 
                     "TRUE" = 1:length(MyXMLs),
                     "FALSE" = sim_workspace_files)
   
   for(i in NewXMLs){
      
      if(class(workspace_objects) != "list"){
         workspace_xml <- XML::xmlTreeParse(i, useInternal = TRUE)
      } else {
         workspace_xml <- MyXMLs[[i]]
      }
      RootNode <- XML::xmlRoot(workspace_xml)
      
      ## observed data path
      Xbackslash <- XML::xmlValue(RootNode[["GraphsData"]][["ObservedDataPath"]])
      Pattern <- switch(as.character(new_xml_path == "only change user"), 
                        "TRUE" = gsub("Users\\\\|\\\\Certara", "",
                                      str_extract(Xbackslash, pattern = "Users.*Certara")), 
                        "FALSE" = sub(new_xml_path, "", new_xml_path)) # FIX THIS
      
      ReplaceWith <- switch(as.character(new_xml_path == "only change user"), 
                            "TRUE" = Sys.info()["user"], 
                            "FALSE" = new_xml_path) # FIX THIS
      
      if(XML::xmlValue(RootNode[["GraphsData"]][["UseObservedData"]]) == "true"){
         XML::xmlValue(RootNode[["GraphsData"]][["ObservedDataPath"]]) <- 
            sub(Pattern, ReplaceWith, Xbackslash)
      }
      
      if(XML::xmlValue(RootNode[["SimulationData"]][["FixedIndividualTrialDesign"]]) == 
         "true"){
         XML::xmlValue(RootNode[["SimulationData"]][["FixedIndividualDataPath"]]) <- 
            sub(Pattern, ReplaceWith, Xbackslash)
      }
      
      if(save_workspaces){
         XML::saveXML(workspace_xml, file = "temp.xml")
         print(paste0("Saving `", i, "`"))
         R.utils::gzip(filename = "temp.xml", 
                       destname = i,
                       remove = TRUE, overwrite = TRUE)
         
      } else {
         MyXMLs[[i]] <- workspace_xml
      }
      
      rm(workspace_xml, RootNode)
      
   }
   
   if(save_workspaces == FALSE){
      return(MyXMLs)
   }
}




