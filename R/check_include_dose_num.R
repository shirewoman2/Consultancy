#' INTERNAL PACKAGE USE ONLY. Check on whether to include the dose number in the
#' PK table headings
#'
#' @param PK table of PK data or col names of that table
#' @param include_dose_num meant to be a pass through when this is T or F and
#'   only check when this is NA
#'
#' @return T or F for whether to include dose numbers
#'
#' @examples
#' # none
check_include_dose_num <- function(PK, 
                                   include_dose_num){
   
   include_dose_num <- include_dose_num[1]
   
   if(complete.cases(include_dose_num) &&
      "logical" %in% class(include_dose_num)){
      return(include_dose_num)
   }
   
   if("data.frame" %in% class(PK)){
      PKparameters <- names(PK)
   } else if("character" %in% class(PK)){
      PKparameters <- PK
   } else {
      warning("We're trying to check whether to include dose numbers in the PK table headings, and we're not sure because we got input data we weren't expecting. We'll set include_dose_num to TRUE.\n", 
              call. = FALSE)
      return(TRUE)
   }
   
   PKparameters <- prettify_column_names(PKparameters, return_which_are_PK = TRUE)
   
   DoseCheck <- c("first" = any(str_detect(PKparameters, "dose1|Dose 1")), 
                  "user-defined" = any(str_detect(PKparameters, "dose1|Dose 1|last|Last dose")) == FALSE, 
                  "last" = any(str_detect(PKparameters, "last|Last dose")))
   include_dose_num <- length(which(DoseCheck)) > 1
   
   if(is.na(include_dose_num)){
      warning("Something is amiss with your input for `include_dose_num`, which should be NA, TRUE, or FALSE. We'll assume you meant for it to be TRUE.", 
              call. = FALSE)
      include_dose_num <- TRUE
   }
   
   return(include_dose_num)
}

