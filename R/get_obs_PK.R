#' INTERNAL - For use w/pksummary_table.
#'
#' @param observed_PK observed_PK
#' @param mean_type mean_type
#' @param sim_data_file sim_data_file
#'
#' @return data.frame
#'
#' @examples
#' # NA
get_obs_PK <- function(observed_PK, 
                       mean_type, 
                       sim_data_file, 
                       PKparameters){
   
   # Cleaning up and harmonizing observed data
   if("data.frame" %in% class(observed_PK)){
      # Convert to long format as needed
      if(any(tolower(names(observed_PK)) %in% 
             tolower(AllPKParameters$PKparameter))){
         
         # If "File" isn't already present as a column, adding it to use for
         # joining later. 
         if("File" %in% str_to_title(names(observed_PK)) == FALSE){
            observed_PK$File <- sim_data_file
         }
         
         # Set aside variability columns for a moment to pivot correctly 
         if(any(str_detect(names(observed_PK), "_CV|_GCV"))){ # FIXME - need to harmonize and then check for CV or GV, I think. 
            observed_PK_var <- observed_PK %>% 
               select(any_of(c("File",
                               paste0(c(AllPKParameters$PKparameter, 
                                        sub("_dose1|_last", "", AllPKParameters$PKparameter)),
                                      "_CV"), 
                               paste0(c(AllPKParameters$PKparameter, 
                                        sub("_dose1|_last", "", AllPKParameters$PKparameter)),
                                      "_GCV")))) %>% 
               pivot_longer(cols = -File, 
                            names_to = "PKparameter", 
                            values_to = "CV") %>% 
               mutate(PKparameter = sub("_CV|_GCV", "",  PKparameter))
         } else {
            observed_PK_var <- data.frame(File = sim_data_file, 
                                          PKparameter = NA)
         }
         
         observed_PK <- observed_PK %>% 
            select(-any_of(paste0(c(AllPKParameters$PKparameter, 
                                    tolower(AllPKParameters$PKparameter), 
                                    AllPKParameters$PKparameter_nodosenum, 
                                    tolower(AllPKParameters$PKparameter_nodosenum)), "_CV"))) %>% 
            pivot_longer(cols = any_of(c(AllPKParameters$PKparameter, 
                                         tolower(AllPKParameters$PKparameter), 
                                         AllPKParameters$PKparameter_nodosenum, 
                                         tolower(AllPKParameters$PKparameter_nodosenum))), 
                         names_to = "PKparameter", 
                         values_to = "Value") %>% 
            left_join(observed_PK_var, by = c("File", "PKparameter"))
      } 
      
      # If they've included several possibilities for mean types, need to get
      # ONLY the appropriate one.
      if("value" %in% tolower(names(observed_PK))){
         names(observed_PK)[which(tolower(names(observed_PK)) == "value")] <- "Value"
      } else if(any(tolower(c("GeoMean", "Mean", "Median")) %in% 
                    tolower(names(observed_PK)))){
         
         # Dealing with any inconsistencies in capitalization. 
         names(observed_PK)[which(tolower(names(observed_PK)) == "geomean")] <- "Geomean"
         names(observed_PK)[which(tolower(names(observed_PK)) == "mean")] <- "Mean"
         names(observed_PK)[which(tolower(names(observed_PK)) == "median")] <- "Median"
         
         # Need to have columns for any of those that don't already exist for
         # case_when to work.
         MissingCols <- setdiff(c("Geomean", "Mean", "Median"), 
                                names(observed_PK))
         
         if(length(MissingCols) > 0){
            observed_PK <- observed_PK %>% 
               bind_cols(as.data.frame(matrix(data = NA, 
                                              ncol = length(MissingCols),
                                              dimnames = list(NULL, MissingCols))))
         }
         
         observed_PK <- observed_PK %>% 
            mutate(Value = case_when(
               {mean_type} == "geometric" & !str_detect(PKparameter, "tmax") ~ Geomean, 
               {mean_type} == "arithmetic" & !str_detect(PKparameter, "tmax") ~ Mean, 
               str_detect(PKparameter, "tmax") ~ Median))
      }
      
      if(tolower("GeoCV") %in% tolower(names(observed_PK)) &
         mean_type == "geometric"){
         names(observed_PK)[which(tolower(names(observed_PK)) == "geocv")] <- "CV"
      }
      
      if(tolower("ArithCV") %in% tolower(names(observed_PK)) &
         mean_type == "arithmetic"){
         names(observed_PK)[which(tolower(names(observed_PK)) == "arithcv")] <- "CV"
      }
      
      # Checking for variability names
      VarNames <- c("var", "variability", "cv")
      VarNames <- VarNames %in% tolower(names(observed_PK))
      names(VarNames) <- c("var", "variability", "cv")
      
      if(length(VarNames[VarNames]) > 1){
         warning("In your observed data, more than one thing is labeled as being the observed variability, so we don't know which do use. We'll ignore anything that looks like it is the observed CV or variability.\n", 
                 call. = FALSE)
         
         observed_PK <- observed_PK %>% 
            select(-any_of(c("[vV]ar|[vV]ariability|cv|CV")))
         
         VarNames <- FALSE
      } else {
         names(observed_PK)[which(tolower(names(observed_PK)) %in% names(VarNames))] <- "CV"
      }
      
      observed_PK <- observed_PK %>% filter(complete.cases(Value))
      if(nrow(observed_PK) == 0){
         return(data.frame(PKparameter = PKparameters))
      }
      
      # Harmonizing PK parameter names
      observed_PK$PKparameter <- harmonize_PK_names(observed_PK$PKparameter)
      
      names(observed_PK)[str_detect(tolower(names(observed_PK)), 
                                    "tab|sheet")] <- "Tab"
      
      observed_PK <- observed_PK %>% 
         select(any_of(c("File", "Tab", "PKparameter", "Value", "CV"))) %>%  
         # Only keeping parameters that we've set up data extraction for,
         # and only keeping complete.cases of obs data
         filter(PKparameter %in% c(AllPKParameters$PKparameter, 
                                   sub("_dose1|_last", "", AllPKParameters$PKparameter)) &
                   complete.cases(Value))
      
      if("File" %in% names(observed_PK)){
         observed_PK <- observed_PK %>%
            # If they didn't include ".xlsx" at the end, add that.
            mutate(File = ifelse(str_detect(File, "xlsx$"), 
                                 File, paste0(File, ".xlsx")), 
                   # Need to adjust a few things b/c of challenges w/file path when this
                   # is called from rmarkdown files.
                   BaseNameFile = basename(as.character(File))) %>% 
            filter(str_detect(BaseNameFile, basename(sim_data_file))) 
         
      } else {
         # If File is not in the column names, then assume that it's the
         # same as sim_data_file anyway.
         observed_PK$File <- sim_data_file
      }
      
      # Checking that they haven't provided more than one value for a given PK
      # parameter for this sim_data_file. If they have, we don't know which
      # observed data to compare.
      ObsFileCheck <- observed_PK %>% 
         unique() %>% group_by(File, PKparameter) %>% 
         summarize(NVals = n())
      
      if(any(ObsFileCheck$NVals > 1)){
         warning("You have supplied more than one value for a given PK parameter for this simulator output file, so we don't know which one to use. We will not be able to include observed data in your table.", 
                 call. = FALSE)
         observed_PK <- data.frame()
      } 
      
      if(nrow(observed_PK) < 1){
         warning("None of the supplied observed PK were for the supplied sim_data_file. We cannot make any comparisons between simulated and observed PK.", 
                 call. = FALSE)
         observed_PK <- NA
      }  
      
      # Removing file name here. We should have already filtered to get only the
      # appropriate files, and it's messing up joining with sim data later.
      MyObsPK <- observed_PK %>% select(-File)
      
   } else {
      MyObsPK <- data.frame(PKparameter = PKparameters)
   }
   
   return(MyObsPK)
   
}


