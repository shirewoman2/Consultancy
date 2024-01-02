#' Harmonize user input for PK parameters
#' 
#' INTERNAL PACKAGE USE
#'
#' @param PKparameters user input for PK parameters that might not perfectly
#'   follow required syntax for PK parameters from \code{AllPKParameters}
#'
#' @return harmonized PK parameters
#'
#' @examples
#' # None
#' 
harmonize_PK_names <- function(PKparameters){
   
   # suffix challenges
   PKparameters <- sub("_ss", "_last", PKparameters)
   PKparameters <- sub("_first", "_dose1", PKparameters)
   
   # Dealing with possible case issues
   PKparameters <- data.frame(Orig = tolower(PKparameters)) %>% 
      left_join(
         data.frame(Rev = unique(c(AllPKParameters$PKparameter,
                                   AllPKParameters$BasePKparameter))) %>% 
            mutate(Orig = tolower(Rev)), 
         by = "Orig") %>% 
      mutate(NoCaseProbs = ifelse(complete.cases(Rev), 
                                  Rev, Orig)) %>% 
      pull(NoCaseProbs)
   
   # other miscellaneous errors
   PKparameters <- sub("cl(_)?hep", "CL_hep", PKparameters)
   PKparameters <- sub("cl(_)?po", "CL_po", PKparameters)
   PKparameters <- sub("withinhib", "withInhib", PKparameters)
   PKparameters <- sub("um1_", "umI_", PKparameters)
   PKparameters <- sub("um2_", "umII_", PKparameters)
   PKparameters <- sub("um3_", "umIII_", PKparameters)
   PKparameters <- sub("um4_", "umIV_", PKparameters)
   PKparameters <- sub("AUCt_last", "AUCtau_last", PKparameters)
   PKparameters <- sub("AUCtau_dose1", "AUCt_dose1", PKparameters)
   PKparameters <- sub("AUCt_ratio_last", "AUCtau_ratio_last", PKparameters)
   PKparameters <- sub("_[iI]nhib", "_withInhib", PKparameters)
   
   return(PKparameters)
}


