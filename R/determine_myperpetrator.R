#' Determine the (optionally pretty) name of all perpetrators included in a
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
determine_myperpetrator <- function(Deets, prettify_compound_names){
   
   Deets <- harmonize_details(Deets)
   Deets <- Deets$MainDetails
   
   MyPerpetrator <- 
      c("Inhibitor1" = switch(as.character("Inhibitor1" %in% names(Deets)), 
                              "TRUE" = str_comma(Deets$Inhibitor1), 
                              "FALSE" = as.character(NA)), 
        "Inhibitor1Metabolite" = switch(as.character("Inhibitor1Metabolite") %in%
                                           names(Deets), 
                                        "TRUE" = str_comma(Deets$Inhibitor1Metabolite), 
                                        "FALSE" = as.character(NA)), 
        "Inhibitor2" = switch(as.character("Inhibitor2" %in% names(Deets)), 
                              "TRUE" = str_comma(Deets$Inhibitor2), 
                              "FALSE" = as.character(NA)))
   
   if(any(complete.cases(MyPerpetrator))){
      MyPerpetrator <- str_comma(MyPerpetrator[complete.cases(MyPerpetrator)])
      
      if(class(prettify_compound_names) == "logical" &&
         prettify_compound_names){
         MyPerpetrator <- prettify_compound_name(MyPerpetrator)
      }
      
      if(class(prettify_compound_names) == "character" &
         "perpetrator" %in% names(prettify_compound_names)){
         names(prettify_compound_names)[
            str_detect(tolower(names(prettify_compound_names)), 
                       "perpetrator")][1] <- "perpetrator"
         MyPerpetrator <- prettify_compound_names["perpetrator"]
      }
   } else {
      MyPerpetrator <- "none"
   }
   
   return(MyPerpetrator)
   
}