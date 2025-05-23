#' FOR INTERNAL USE ONLY: Extract PK data from a generic AUC tab, e.g., NOT one
#' formatted like the "AUC" tab from Simcyp V21 and earlier
#'
#' @param PKparameters parameters to extract; character vector only (a set of
#'   parameters like in extractPK is not acceptable here)
#' @param sim_data_file simulator output Excel file to read
#' @param Sheet sheet to read from the Excel file
#' @param PKset which set of PK parameters to use. Options: AUC0, AUClast, AUCX
#' @param UserSpecified TRUE or FALSE for whether this was a user-specified
#'   sheet, in which case we don't know which dose it is
#' @param Deets output from running extractExpDetails on the provided
#'   sim_data_file and setting \code{Deets <- existing_exp_details$MainDetails}
#' @param includeTrialInfo TRUE or FALSE
#' @param DataCheck DataCheck data.frame
#' @param PKparameters_orig original PK parameters from extractPK function
#' @param compoundToExtract For which compound do you want to extract
#'   PK data? Options are: \itemize{\item{"substrate"
#'   (default),} \item{"primary metabolite 1",} \item{"primary metabolite 2",}
#'   \item{"secondary metabolite",} \item{"inhibitor 1" -- this can be an
#'   inducer, inhibitor, activator, or suppresesor, but it's labeled as
#'   "Inhibitor 1" in the simulator,} \item{"inhibitor 2" for the 2nd inhibitor
#'   listed in the simulation,} \item{"inhibitor 1 metabolite" for the primary
#'   metabolite of inhibitor 1}}

#'
#' @return a list for use with the extractPK function
#'
#' @examples
#' # none
#' 
extractAUCXtab <- function(PKparameters, 
                           PKparameters_orig, 
                           compoundToExtract = "substrate",
                           sim_data_file,
                           Sheet, 
                           PKset,
                           UserSpecified,
                           Deets, 
                           DataCheck,
                           includeTrialInfo){
   
   Out_ind <- list()
   Out_agg <- list()
   
   AUCX_xl <- suppressMessages(
      readxl::read_excel(path = sim_data_file, sheet = Sheet,
                         col_names = FALSE))
   
   # Finding the last row of the individual data
   EndRow_ind <- which(AUCX_xl$...2 == "Statistics")[1] - 3
   
   if(length(EndRow_ind) == 0){
      # Using "warning" instead of "stop" here b/c I want this to be
      # able to pass through to other functions and just skip any
      # files that aren't simulator output.
      warning(wrapn(paste0("It appears that you don't have any aggregate data in your simulator output file ",
                     sim_data_file, "; was this a population-representative simulation? This function only really works well when there are aggregate data present, so this file will be skipped.")),
              call. = FALSE)
      return(list())
   } 
   
   # Finding the aggregate data rows 
   StartRow_agg <- which(AUCX_xl$...2 == "Statistics")[1] + 2
   EndRow_agg <- which(AUCX_xl$...2 == "Std Dev")
   EndRow_agg <- EndRow_agg[which(EndRow_agg > StartRow_agg)][1]
   EndRow_agg <- ifelse(is.na(EndRow_agg), nrow(AUCX_xl), EndRow_agg)
   
   if(UserSpecified){
      PKparameters <- unique(sub("_dose1|_last", "", PKparameters))
   }
   
   # Storing whether to include AUCinf extrapolation warnings
   Warn_if_noAUCinf_col <- as.logical(c())
   
   # Looping through parameters and extracting values
   for(i in PKparameters){
      
      # Using regex to find the correct column. See
      # data(AllPKParameters) for all the possible parameters as well
      # as what regular expressions are being searched for each. 
      ToDetect <- AllPKParameters %>% 
         mutate(SearchText = ifelse(compoundToExtract %in% c("inhibitor 1",
                                                             "inhibitor 1 metabolite",
                                                             "inhibitor 2") &
                                       SwitchWhenInhib != "", 
                                    SwitchWhenInhib, SearchText))
      if(UserSpecified){
         # If tab was user specified, need to remove the suffix indicating
         # which dose number it was b/c we don't know
         ToDetect <- ToDetect %>% 
            mutate(PKparameter = sub("_dose1|_last", "", PKparameter)) 
      }
      
      ToDetect <- ToDetect %>% 
         filter(Sheet == switch(PKset, 
                                "AUC0" = "AUC0", 
                                "AUClast" = "AUCX", 
                                "AUCX" = "AUCX") & PKparameter == i) %>% 
         select(PKparameter, SearchText) %>% 
         unique()
      
      if(nrow(ToDetect) == 0){
         PKparameters <- setdiff(PKparameters, i)
         next
      }
      
      # Looking for the regular expression specific to this parameter
      # i. 
      if(i == "DoseAdministered"){
         ColNum <- which(str_detect(as.vector(t(AUCX_xl[2, ])),
                                    ToDetect$SearchText))[1]
      } else {
         ColNum <- which(str_detect(as.vector(t(AUCX_xl[2, ])),
                                    ToDetect$SearchText) &
                            !str_detect(as.vector(t(AUCX_xl[2, ])), 
                                        "/kg"))[1]
      }
      
      if(length(ColNum) == 0 || is.na(ColNum)){
         # Adding a condition for checking whether user requested a set of
         # parameters b/c we don't really need the warning if they did.
         # They'll get what they get! :-D
         if(any(PKparameters_orig %in% c("all", "AUC tab", "Absorption tab")) == FALSE){
            warning(wrapn(paste0("The column with information for ", i,
                           " on the tab '", Sheet, "' cannot be found in the file '", 
                           sim_data_file, "'.")), 
                    call. = FALSE)
            
            Warn_if_noAUCinf_col[i] <- FALSE
         } else {
            Warn_if_noAUCinf_col[i] <- TRUE
         }
         
         suppressMessages(rm(ToDetect, ColNum))
         PKparameters <- setdiff(PKparameters, i)
         
         next
      } else {
         Warn_if_noAUCinf_col[i] <- TRUE
      }
      
      suppressWarnings(
         Out_ind[[i]] <- AUCX_xl[3:EndRow_ind, ColNum] %>%
            pull(1) %>% as.numeric
      )
      
      suppressWarnings(
         Out_agg[[i]] <- AUCX_xl[StartRow_agg:EndRow_agg, ColNum] %>%
            pull(1) %>% as.numeric()
      )
      names(Out_agg[[i]]) <- AUCX_xl[StartRow_agg:EndRow_agg, 2] %>%
         pull(1)
      
      DataCheck <- DataCheck %>%
         bind_rows(data.frame(PKparam = i, 
                              Tab = Sheet,
                              SearchText = ToDetect$SearchText,
                              Column = ColNum, 
                              StartRow_agg = StartRow_agg,
                              EndRow_agg = EndRow_agg,
                              StartRow_ind = 3,
                              EndRow_ind = EndRow_ind))
   }   
   
   # If no parameters were found, then return an empty list. 
   if(length(Out_agg) == 0){
      return(list())
   }
   
   if(includeTrialInfo & length(PKparameters) > 0){
      # Subject and trial info
      SubjTrial_AUCX <- AUCX_xl[3:EndRow_ind, 1:2] %>%
         rename("Individual" = ...1, "Trial" = ...2)
      
      Out_ind[["AUCXtab"]] <- cbind(SubjTrial_AUCX,
                                    bind_rows(Out_ind[PKparameters]))
   }
   
   # suppressWarnings(rm(StartRow_agg, EndRow_agg, EndRow_ind, Sheet))
   
   TimeInterval <- data.frame(
      File = sim_data_file, 
      Sheet = Sheet, 
      PKparameter = names(Out_agg), 
      Interval = gsub("\\(|\\)", "", str_extract(AUCX_xl[1, 1], "from.*"))) %>% 
      mutate(Interval = gsub("\\.00 ", " ", Interval))
   
   Out <- list("Out_ind" = Out_ind,
               "Out_agg" = Out_agg, 
               "DataCheck" = DataCheck, 
               "TimeInterval" = TimeInterval,
               "WarnAUCinf" = all(Warn_if_noAUCinf_col))
   
   return(Out)
   
}


