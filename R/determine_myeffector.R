#' Determine the (optionally pretty) name of all effectors included in a
#' simulation, all concatenated nicely
#'
#' @param Deets experimental details for only the single sim_data_file in
#'   question rather than for all possible files that may have been contained in
#'   existing_exp_details
#' @param prettify_compound_names pass through from parent function
#'
#' @export
#'
#' @examples
#' # None yet
#' 
determine_myeffector <- function(Deets, prettify_compound_names){
   
   Deets <- harmonize_details(Deets)
   Deets <- Deets$MainDetails
   
   MyEffector <- 
      c("Inhibitor1" = switch(as.character("Inhibitor1" %in% names(Deets)), 
                              "TRUE" = str_comma(Deets$Inhibitor1), 
                              "FALSE" = as.character(NA)), 
        "Inhibitor2" = switch(as.character("Inhibitor2" %in% names(Deets)), 
                              "TRUE" = str_comma(Deets$Inhibitor2), 
                              "FALSE" = as.character(NA)))
   
   if(any(complete.cases(MyEffector))){
      MyEffector <- str_comma(MyEffector[complete.cases(MyEffector)])
      
      if(class(prettify_compound_names) == "logical" &&
         prettify_compound_names){
         MyEffector <- prettify_compound_name(MyEffector)
      }
      
      if(class(prettify_compound_names) == "character" &
         "effector" %in% names(prettify_compound_names)){
         names(prettify_compound_names)[
            str_detect(tolower(names(prettify_compound_names)), 
                       "effector")][1] <- "effector"
         MyEffector <- prettify_compound_names["effector"]
      }
   } else {
      MyEffector <- "none"
   }
   
   return(MyEffector)
   
}