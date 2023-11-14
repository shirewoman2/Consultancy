#' FOR INTERNAL PACKAGE USE. Take as input an existing data.frame or list of
#' experimental details and make it be a list with standard items
#'
#' \code{harmonize_details} takes as input an object created from running
#' extractExpDetails, extractExpDetails_mult, annotateDetails, or
#' deannotateDetails and makes it conform to the data structure the rest of the
#' package needs: a list with the following items, which can be added to in the
#' future: 1) MainDetails, 2) CustomDosing, 3) DissolutionProfiles, and 4)
#' ReleaseProfiles.
#'
#' @param existing_exp_details an object created from running extractExpDetails,
#'   extractExpDetails_mult, annotateDetails, or deannotateDetails
#'
#' @return Returns a list
#'
#' @examples
#' # none

harmonize_details <- function(existing_exp_details){
   
   # Setting up data structure -----------------------------------------------
   
   # At the end of this function, the output object should be a list with the
   # named items saved in the object ExpDetailListItems
   
   if("list" %in% class(existing_exp_details)){
      if("MainDetails" %in% names(existing_exp_details)){
         
         # This is when they've run extractExpDetails with versions >= 2.8.0.
         # For this scenario, we need to do 2 things: 1. check that they also
         # have any other ExpDetailListItems they should have. 2. Make sure that
         # the details are not annotated (since that's generally what's required
         # for most functions) and then we can return the full list, in order.
         
         if("Detail" %in% names(existing_exp_details$MainDetails)){
            # This is when it has already been annotated previously. Need to
            # deannotate 1st.
            existing_exp_details <- deannotateDetails(existing_exp_details, 
                                                      apply_class = FALSE)
         }
         
         # Checking for whether custom dosing is multiple items b/c I did that
         # for a hot minute while I was figuring out how this should work
         if(any(c("CustomDosing_sub", "CustomDosing_inhib", 
                  "CustomDosing_inhib2") %in% names(existing_exp_details))){
            
            for(item in c("CustomDosing_sub", "CustomDosing_inhib", 
                          "CustomDosing_inhib2")){
               if(nrow(existing_exp_details[[item]]) > 0){
                  existing_exp_details[[item]] <- existing_exp_details[[item]] %>% 
                     mutate(CompoundID = AllCompounds$CompoundID[
                        which(AllCompounds$Suffix == sub("CustomDosing", "", item))]) %>% 
                     rename_with(.cols = everything(), 
                                 .fn = function(x) sub("_sub|_inhib|_inhib2", "", x))
               }
            }
            
            CustomDosing <- bind_rows(existing_exp_details[
               c("CustomDosing_sub", "CustomDosing_inhib", "CustomDosing_inhib2")]) %>% 
               left_join(Details$MainDetails %>% 
                            select(File, Substrate, Inhibitor1, Inhibitor2) %>% 
                            pivot_longer(cols = -File, 
                                         names_to = "DetailNames", 
                                         values_to = "Compound") %>% 
                            left_join(AllCompounds %>%
                                         select(DetailNames, CompoundID), 
                                      by = "DetailNames"), 
                         by = c("File", "CompoundID")) %>% 
               select(File, CompoundID, Compound, Time, Time_units, DoseNum, 
                      Dose, Dose_units, DoseRoute)
            
            existing_exp_details$CustomDosing_sub <-  NULL
            existing_exp_details$CustomDosing_inhib <- NULL
            existing_exp_details$CustomDosing_inhib2 <- NULL
            
            existing_exp_details$CustomDosing <- CustomDosing
            
         }
         
         itemstoadd <- setdiff(ExpDetailListItems, names(existing_exp_details))
         append_items <- lapply(itemstoadd, function(x) return(NULL))
         names(append_items) <- itemstoadd
         existing_exp_details <- c(existing_exp_details, append_items)
         
         return(existing_exp_details[ExpDetailListItems])
         
      } else {
         
         # This is when they may have saved the output from
         # extractExpDetails from package versions < 2.8.0, which is when I
         # changed the output from extractExpDetails from sometimes being a list
         # & sometimes being a data.frame to ALWAYS being a list.
         return(list(Main = as.data.frame(existing_exp_details[which(sapply(existing_exp_details, length) == 1)]), 
                     CustomDosing = NULL,
                     DissolutionProfiles = NULL,
                     ReleaseProfiles = NULL))
         
      }
      
   } else if("data.frame" %in% class(existing_exp_details)){
      
      # This is when they may have saved the output from extractExpDetails_mult
      # or annotateDetails or deannotateDetails from package versions < 2.8.0,
      # which is when I changed the output from extractExpDetails from sometimes
      # being a list & sometimes being a data.frame to ALWAYS being a list.
      
      existing_exp_details <- list(MainDetails = existing_exp_details)
      
      itemstoadd <- setdiff(ExpDetailListItems, names(existing_exp_details))
      append_items <- lapply(itemstoadd, function(x) return(NULL))
      names(append_items) <- itemstoadd
      existing_exp_details <- c(existing_exp_details, append_items)
      
      if(all(c("SimulatorSection", "Sheet") %in% names(existing_exp_details$MainDetails))){
         # This is when existing_exp_details has been annotated. Need to
         # de-annotate here to make this work well with the rest of the
         # function.
         existing_exp_details <- deannotateDetails(existing_exp_details)
         
      }
      
      return(existing_exp_details[ExpDetailListItems])
   }
}

