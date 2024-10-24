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
   
   # Remove extra spaces
   PKparameters <- str_trim(PKparameters)
   
   # hyphens instead of underscores
   PKparameters <- gsub("-", "_", PKparameters)
   
   # suffix challenges
   PKparameters <- sub("_ss", "_last", PKparameters)
   PKparameters <- sub("_first", "_dose1", PKparameters)
   
   # other misc errors
   PKparameters <- sub("AUCt_last", "AUCtau_last", PKparameters)
   PKparameters <- sub("AUC_last", "AUCtau_last", PKparameters)
   PKparameters <- sub("AUC_inf", "AUCinf", PKparameters)
   PKparameters <- sub("AUC_t", "AUCt", PKparameters)
   PKparameters <- sub("AUCtau_dose1", "AUCt_dose1", PKparameters)
   PKparameters <- sub("AUCt_ratio_last", "AUCtau_ratio_last", PKparameters)
   PKparameters <- sub("_[iI]nhib", "_withInhib", PKparameters)
   PKparameters <- sub("withinhib", "withInhib", PKparameters)
   
   # If the user switched the order of "ratio" and "last" or "dose1", fix that.
   PKparameters <- sub("dose1_ratio", "ratio_dose1", PKparameters)
   PKparameters <- sub("last_ratio", "ratio_last", PKparameters)
   
   # Dealing with possible case issues
   PKparameters <- data.frame(Orig = PKparameters) %>% 
      mutate(Orig_lower = tolower(Orig)) %>% 
      left_join(
         data.frame(Rev = unique(c(AllPKParameters$PKparameter,
                                   sub("_dose1|_last", "", AllPKParameters$PKparameter)))) %>% 
            mutate(Orig_lower = tolower(Rev)), 
         by = "Orig_lower") %>% 
      mutate(NoCaseProbs = ifelse(complete.cases(Rev), 
                                  Rev, Orig)) %>% 
      pull(NoCaseProbs)
   
   # other miscellaneous errors for parameters that aren't the main ones
   PKparameters <- sub("cl(_)?hep", "CL_hep", PKparameters)
   PKparameters <- sub("cl(_)?po", "CL_po", PKparameters)
   PKparameters <- sub("um1_", "umI_", PKparameters)
   PKparameters <- sub("um2_", "umII_", PKparameters)
   PKparameters <- sub("um3_", "umIII_", PKparameters)
   PKparameters <- sub("um4_", "umIV_", PKparameters)
   PKparameters <- sub("auc tab", "AUC tab", PKparameters)
   PKparameters <- sub("absorption tab", "Absorption tab", PKparameters)
   
   return(PKparameters)
}


