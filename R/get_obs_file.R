#' Get the observed XML overlay file from a Simcyp Simulator workspace
#'
#' @param workspace the workspace file
#'
#' @returns a character vector of the observed XML overlay file and its path
#'   relative to the working directory
#' @export
#'
get_obs_file <- function(workspace){
   
   if(length(workspace) != 1){
      warning(wrapn("You must supply a single workspace to get the observed XML overlay file."), 
              call. = FALSE)
      return(NA)
   }
   
   if(file.exists(workspace) == FALSE){
      return(NA)
   }
   
   if(file.exists("TEMP.wks.tmp")){file.remove("TEMP.wks.tmp")}
   
   # For some reason, you have to unzip the workspaces 1st if they're V23 or
   # later. Not sure what changed.
   unzip1st_fun <- function(workspace){
      R.utils::gunzip(workspace, destname = "TEMP.wks", remove = FALSE)
      workspace_xml <- XML::xmlTreeParse("TEMP.wks", useInternal = TRUE)
      file.remove("TEMP.wks")
      return(workspace_xml)
   }
   
   suppressWarnings(
      workspace_xml <- tryCatch(XML::xmlTreeParse(workspace, useInternal = TRUE), 
                                error = unzip1st_fun(workspace))
   )
   
   RootNode <- XML::xmlRoot(workspace_xml)
   
   UseObs <- as.logical(XML::xmlValue(RootNode[["GraphsData"]][["UseObservedData"]]))
   
   if(UseObs){
      Out <- XML::xmlValue(RootNode[["GraphsData"]][["ObservedDataPath"]])
   } else {
      Out <- NA
   }
   
   return(Out)
}


