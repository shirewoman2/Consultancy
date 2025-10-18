#' FOR INTERNAL PACKAGE USE. Take as input an existing data.frame or list of
#' experimental details and make it be a list with standard items
#'
#' \code{harmonize_details} takes as input an object created from running
#' extractExpDetails, extractExpDetails_mult, annotateDetails, or
#' deannotateDetails and makes it conform to the data structure the rest of the
#' package needs: a list with the following items, which can be added to in the
#' future: 1) MainDetails, 2) CustomDosing, 3) DissolutionProfiles, 4)
#' ReleaseProfiles, 5) ConcDependent_fup, and 6) ConcDependent_BP. 
#'
#' @param existing_exp_details an object created from running extractExpDetails,
#'   extractExpDetails_mult, annotateDetails, or deannotateDetails
#'
#' @return Returns a list
#'
#' @examples
#' # none

harmonize_details <- function(existing_exp_details){
   
   # First: Make sure everything is unique
   if("list" %in% class(existing_exp_details)){
      existing_exp_details <- map(existing_exp_details, .f = unique)
   } else if("data.frame" %in% class(existing_exp_details)){
      existing_exp_details <- unique(existing_exp_details)
   }
   
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
                     mutate(CompoundID = AllRegCompounds$CompoundID[
                        which(AllRegCompounds$Suffix == sub("CustomDosing", "", item))]) %>% 
                     rename_with(.cols = everything(), 
                                 .fn = function(x) sub("_sub|_inhib|_inhib2", "", x))
               }
            }
            
            CustomDosing <- bind_rows(existing_exp_details[
               c("CustomDosing_sub", "CustomDosing_inhib", "CustomDosing_inhib2")])
            
            if(nrow(CustomDosing) > 0){
               CustomDosing <- CustomDosing %>% 
                  left_join(Details$MainDetails %>% 
                               select(File, Substrate, Inhibitor1, Inhibitor2) %>% 
                               pivot_longer(cols = -File, 
                                            names_to = "DetailNames", 
                                            values_to = "Compound") %>% 
                               left_join(AllRegCompounds %>%
                                            select(DetailNames, CompoundID), 
                                         by = "DetailNames"), 
                            by = c("File", "CompoundID")) %>% 
                  select(File, CompoundID, Compound, Time, Time_units, DoseNum, 
                         Dose, Dose_units, DoseRoute)
               
            } else {
               CustomDosing <- tibble()
            }
            
            existing_exp_details$CustomDosing_sub <-  tibble()
            existing_exp_details$CustomDosing_inhib <- tibble()
            existing_exp_details$CustomDosing_inhib2 <- tibble()
            
            existing_exp_details$CustomDosing <- CustomDosing
            
         }
         
         itemstoadd <- setdiff(ExpDetailListItems, names(existing_exp_details))
         append_items <- lapply(itemstoadd, function(x) return(tibble()))
         names(append_items) <- itemstoadd
         existing_exp_details <- c(existing_exp_details, append_items)
         
         # # Check whether MainDetails includes SheetNames b/c need it for other
         # # functions now.
         # if(("SheetNames" %in% names(existing_exp_details$MainDetails) && 
         #     any(is.na(existing_exp_details$MainDetails$SheetNames)) |
         #     any(existing_exp_details$MainDetails$SheetNames == "`NA`", na.rm = T)) | 
         #    "SheetNames" %in% names(existing_exp_details$MainDetails) == FALSE){
         #    
         #    for(i in existing_exp_details$MainDetails$File){
         #       if(file.exists(i) & 
         #          !str_detect(i, "\\.db$|\\.wksz")){
         #          SheetNames <- tryCatch(readxl::excel_sheets(i),
         #                                 error = openxlsx::getSheetNames(i))
         #       } else { SheetNames <- NA}
         #       
         #       existing_exp_details$MainDetails$SheetNames[
         #          existing_exp_details$MainDetails$File == i] <- 
         #          str_c(paste0("`", SheetNames, "`"), collapse = " ")
         #       rm(SheetNames)
         #    }
         # }
         
         # Adding missing, necessary list items
         Missing1 <- setdiff(
            paste0(rep(c("DoseInt", "Dose", "DoseRoute", "Regimen", "NumDoses"),
                       each = 3), 
                   c("_sub", "_inhib", "_inhib2")), 
            names(existing_exp_details$MainDetails))
         
         if(length(Missing1) > 0){
            Missing <- as.data.frame(matrix(data = NA, 
                                            ncol = length(Missing1), 
                                            nrow = nrow(existing_exp_details$MainDetails), 
                                            dimnames = list(NULL, Missing1)))
            
            existing_exp_details$MainDetails <- 
               cbind(existing_exp_details$MainDetails, Missing)
         }
         
         # Making DoseInt_x and Dose_x numeric all the time. We'll get custom dosing
         # info from Regimen_x and DoseRexisting_exp_details$MainDetailse_x.
         suppressWarnings(
            existing_exp_details$MainDetails <- existing_exp_details$MainDetails %>% 
               mutate(DoseInt_sub = as.numeric(DoseInt_sub), 
                      DoseInt_inhib = as.numeric(DoseInt_inhib), 
                      DoseInt_inhib2 = as.numeric(DoseInt_inhib2), 
                      
                      Dose_sub = as.numeric(Dose_sub), 
                      Dose_inhib = as.numeric(Dose_inhib), 
                      Dose_inhib2 = as.numeric(Dose_inhib2), 
                      
                      # At this point, DoseInt_x and Dose_x will be NA if it's a
                      # custom dosing regimen. Setting the regimen to
                      # "multiple". We'll use that downstream for checking for
                      # appropriate PK parameters, etc.
                      Regimen_sub = 
                         case_when(is.na(DoseInt_sub) & 
                                      (complete.cases(DoseRoute_sub) &
                                          DoseRoute_sub == "custom dosing") ~ "Multiple Dose",
                                   .default = Regimen_sub), 
                      
                      Regimen_inhib = 
                         case_when(is.na(DoseInt_inhib) & 
                                      (complete.cases(DoseRoute_inhib) &
                                          DoseRoute_inhib == "custom dosing") ~ "Multiple Dose",
                                   .default = Regimen_inhib), 
                      
                      Regimen_inhib2 = 
                         case_when(is.na(DoseInt_inhib2) & 
                                      (complete.cases(DoseRoute_inhib2) &
                                          DoseRoute_inhib2 == "custom dosing") ~ "Multiple Dose",
                                   .default = Regimen_inhib2))
         )
         
         # Harmonizing dosing. Note that this must come AFTER making sure that
         # the list has items named MainDetails and named CustomDosing!
         existing_exp_details <- harmonize_dosing(existing_exp_details)
         
         # Making sure the order is correct
         existing_exp_details <- existing_exp_details[ExpDetailListItems]
         
         return(existing_exp_details[ExpDetailListItems])
         
      } else {
         
         # This is when they may have saved the output from
         # extractExpDetails from package versions < 2.8.0, which is when I
         # changed the output from extractExpDetails from sometimes being a list
         # & sometimes being a data.frame to ALWAYS being a list.
         
         Out <- list(MainDetails = as.data.frame(existing_exp_details[
            which(sapply(existing_exp_details, length) == 1)]))
         
         for(i in ExpDetailListItems[2:length(ExpDetailListItems)]){
            Out[[i]] <- tibble()
         }
         
         # # Check whether MainDetails includes SheetNames b/c need it for other
         # # functions now.
         # if(("SheetNames" %in% names(existing_exp_details$MainDetails) && 
         #     any(is.na(existing_exp_details$MainDetails$SheetNames)) |
         #     any(existing_exp_details$MainDetails$SheetNames == "`NA`", na.rm = T)) | 
         #    "SheetNames" %in% names(existing_exp_details$MainDetails) == FALSE){
         #    
         #    SheetNames <- as.character(c())
         #    for(i in existing_exp_details$MainDetails$File){
         #       if(file.exists(i)){
         #          SheetNames[i] <- tryCatch(readxl::excel_sheets(i),
         #                                    error = openxlsx::getSheetNames(i))
         #       } else { SheetNames[i] <- NA}
         #       
         #       existing_exp_details$MainDetails$SheetNames[
         #          existing_exp_details$MainDetails$File == i] <- 
         #          str_c(paste0("`", SheetNames, "`"), collapse = " ")
         #    }
         # }
         
         # Adding missing, necessary list items
         Missing1 <- setdiff(
            paste0(rep(c("DoseInt", "Dose", "DoseRoute", "Regimen", "NumDoses"),
                       each = 3), 
                   c("_sub", "_inhib", "_inhib2")), 
            names(existing_exp_details$MainDetails))
         
         if(length(Missing1) > 0){
            Missing <- as.data.frame(matrix(data = NA, 
                                            ncol = length(Missing1), 
                                            nrow = nrow(existing_exp_details$MainDetails), 
                                            dimnames = list(NULL, Missing1)))
            
            existing_exp_details$MainDetails <- 
               cbind(existing_exp_details$MainDetails, Missing)
         }
         
         # Making DoseInt_x and Dose_x numeric all the time. We'll get custom dosing
         # info from Regimen_x and DoseRexisting_exp_details$MainDetailse_x.
         suppressWarnings(
            existing_exp_details$MainDetails <- existing_exp_details$MainDetails %>% 
               mutate(DoseInt_sub = as.numeric(DoseInt_sub), 
                      DoseInt_inhib = as.numeric(DoseInt_inhib), 
                      DoseInt_inhib2 = as.numeric(DoseInt_inhib2), 
                      
                      Dose_sub = as.numeric(Dose_sub), 
                      Dose_inhib = as.numeric(Dose_inhib), 
                      Dose_inhib2 = as.numeric(Dose_inhib2), 
                      
                      # At this point, DoseInt_x and Dose_x will be NA if it's a
                      # custom dosing regimen. Setting the regimen to
                      # "multiple". We'll use that downstream for checking for
                      # appropriate PK parameters, etc.
                      Regimen_sub = 
                         case_when(is.na(DoseInt_sub) & 
                                      (complete.cases(DoseRoute_sub) &
                                          DoseRoute_sub == "custom dosing") ~ "Multiple Dose",
                                   .default = Regimen_sub), 
                      
                      Regimen_inhib = 
                         case_when(is.na(DoseInt_inhib) & 
                                      (complete.cases(DoseRoute_inhib) &
                                          DoseRoute_inhib == "custom dosing") ~ "Multiple Dose",
                                   .default = Regimen_inhib), 
                      
                      Regimen_inhib2 = 
                         case_when(is.na(DoseInt_inhib2) & 
                                      (complete.cases(DoseRoute_inhib2) &
                                          DoseRoute_inhib2 == "custom dosing") ~ "Multiple Dose",
                                   .default = Regimen_inhib2))
         )
         
         # Harmonizing dosing. Note that this must come AFTER making sure that
         # the list has items named MainDetails and named CustomDosing!
         existing_exp_details <- harmonize_dosing(existing_exp_details)
         
         # Making sure the order is correct
         existing_exp_details <- existing_exp_details[ExpDetailListItems]
         
         return(Out)
         
      }
      
   } else if("data.frame" %in% class(existing_exp_details)){
      
      # This is when they may have saved the output from extractExpDetails_mult
      # or annotateDetails or deannotateDetails from package versions < 2.8.0,
      # which is when I changed the output from extractExpDetails from sometimes
      # being a list & sometimes being a data.frame to ALWAYS being a list.
      
      existing_exp_details <- list(MainDetails = existing_exp_details)
      
      itemstoadd <- setdiff(ExpDetailListItems, names(existing_exp_details))
      append_items <- lapply(itemstoadd, function(x) return(tibble()))
      names(append_items) <- itemstoadd
      existing_exp_details <- c(existing_exp_details, append_items)
      
      if(all(c("SimulatorSection", "Sheet") %in% names(existing_exp_details$MainDetails))){
         # This is when existing_exp_details has been annotated. Need to
         # de-annotate here to make this work well with the rest of the
         # function.
         existing_exp_details <- deannotateDetails(existing_exp_details)
         
      }
      
      # # Check whether MainDetails includes SheetNames b/c need it for other
      # # functions now.
      # if(("SheetNames" %in% names(existing_exp_details$MainDetails) && 
      #     any(is.na(existing_exp_details$MainDetails$SheetNames)) |
      #     any(existing_exp_details$MainDetails$SheetNames == "`NA`", na.rm = T)) | 
      #    "SheetNames" %in% names(existing_exp_details$MainDetails) == FALSE){
      #    
      #    SheetNames <- as.character(c())
      #    for(i in existing_exp_details$MainDetails$File[
      #       str_detect(existing_exp_details$MainDetails$File, "xlsx$")]){
      #       if(file.exists(i)){
      #          SheetNames[i] <- 
      #             str_c(paste0("'", 
      #                          tryCatch(readxl::excel_sheets(i),
      #                                   error = openxlsx::getSheetNames(i)), 
      #                          "'"), collapse = " ")
      #       } else { SheetNames[i] <- NA}
      #       
      #       existing_exp_details$MainDetails$SheetNames[
      #          existing_exp_details$MainDetails$File == i] <- SheetNames[i]
      #       
      #    }
      # }
      
      # Adding missing, necessary list items
      Missing1 <- setdiff(
         paste0(rep(c("DoseInt", "Dose", "DoseRoute", "Regimen", "NumDoses"),
                    each = 3), 
                c("_sub", "_inhib", "_inhib2")), 
         names(existing_exp_details$MainDetails))
      
      if(length(Missing1) > 0){
         Missing <- as.data.frame(matrix(data = NA, 
                                         ncol = length(Missing1), 
                                         nrow = nrow(existing_exp_details$MainDetails), 
                                         dimnames = list(NULL, Missing1)))
         
         existing_exp_details$MainDetails <- 
            cbind(existing_exp_details$MainDetails, Missing)
      }
      
      # Making DoseInt_x and Dose_x numeric all the time. We'll get custom dosing
      # info from Regimen_x and DoseRexisting_exp_details$MainDetailse_x.
      suppressWarnings(
         existing_exp_details$MainDetails <- existing_exp_details$MainDetails %>% 
            mutate(DoseInt_sub = as.numeric(DoseInt_sub), 
                   DoseInt_inhib = as.numeric(DoseInt_inhib), 
                   DoseInt_inhib2 = as.numeric(DoseInt_inhib2), 
                   
                   Dose_sub = as.numeric(Dose_sub), 
                   Dose_inhib = as.numeric(Dose_inhib), 
                   Dose_inhib2 = as.numeric(Dose_inhib2), 
                   
                   # At this point, DoseInt_x and Dose_x will be NA if it's a
                   # custom dosing regimen. Setting the regimen to
                   # "multiple". We'll use that downstream for checking for
                   # appropriate PK parameters, etc.
                   Regimen_sub = 
                      case_when(is.na(DoseInt_sub) & 
                                   (complete.cases(DoseRoute_sub) &
                                       DoseRoute_sub == "custom dosing") ~ "Multiple Dose",
                                .default = Regimen_sub), 
                   
                   Regimen_inhib = 
                      case_when(is.na(DoseInt_inhib) & 
                                   (complete.cases(DoseRoute_inhib) &
                                       DoseRoute_inhib == "custom dosing") ~ "Multiple Dose",
                                .default = Regimen_inhib), 
                   
                   Regimen_inhib2 = 
                      case_when(is.na(DoseInt_inhib2) & 
                                   (complete.cases(DoseRoute_inhib2) &
                                       DoseRoute_inhib2 == "custom dosing") ~ "Multiple Dose",
                                .default = Regimen_inhib2))
      )
      
      # Harmonizing dosing. Note that this must come AFTER making sure that the
      # list has items named MainDetails and named CustomDosing! Skipping this
      # step when crucial info unavailable, which is sometimes the case when
      # it's from an XML file.
      if(all(sapply(c("Dose", "DoseRoute", "DoseInt", "StartHr", "NumDoses", 
                      "SimStartDayTime", "Units_dose_sub"), 
                    \(x) any(str_detect(
                       names(existing_exp_details$MainDetails), x))))){
         existing_exp_details <- harmonize_dosing(existing_exp_details)
      }
      
      # Making sure the order is correct
      existing_exp_details <- existing_exp_details[ExpDetailListItems]
      
      return(existing_exp_details[ExpDetailListItems])
   }
}

