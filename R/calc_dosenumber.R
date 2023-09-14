#' Calculate the dose number for each time in a set of concentration-time data
#' given the experimental details for that simulation
#'
#' \code{calc_dosenumber} is mainly for internal use in the SimcypConsultancy
#' package and will calculate which dose number a given time should be if you
#' provide the ct_dataframe and the extracted simulation experimental details
#' (the output from \code{\link{extractExpDetails}}). This is meant to be used
#' for a single simulation at a time or a set of concentration-time data where
#' the dosing regimen is IDENTICAL for all simulations.
#'
#' @param ct_dataframe a data.frame of concentration-time data from running
#'   either \code{\link{extractConcTime}} or \code{\link{extractConcTime_mult}}
#' @param existing_exp_details the output from running
#'   \code{\link{extractExpDetails}} or \code{\link{extractExpDetails_mult}} on
#'   the same simulations that were used for making \code{ct_dataframe}. Note:
#'   If you would like to have this calculate dose numbers on data where you
#'   don't have simulation experimental details (example: observed data), then
#'   please supply a single-row data.frame with the following columns, using
#'   substrate dosing info as an example:
#'   \describe{\item{File}{(character) each Simulator Excel file name (check
#'   that the file names match perfectly!) or "all" if you want the same dosing
#'   regimen for all files} \item{DoseInt_sub}{(numeric) the dosing interval in
#'   hours} \item{StartHr_sub}{(numeric) the start time for the substrate;
#'   probably 0} \item{NumDoses_sub}{(numeric) the number of doses overall}
#'   \item{Regimen_sub}{(character) presumably "Multiple Dose" but "Single Dose"
#'   is also acceptable}} If you want the dose number for other compound IDs, 
#'   then replace "_sub" with, e.g., "_inhib". Please run 
#'   \code{view(AllCompounds)} to see acceptable suffixes for each compound ID.
#'
#' @return Output is a data.frame of concentration-time data with
#' @export
#'
#' @examples
#' # None yet
#' 
calc_dosenumber <- function(ct_dataframe, 
                            existing_exp_details){
   
   # Error catching and setting up --------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Checking whether existing_exp_details needs to be de-annotated
   if("data.frame" %in% class(existing_exp_details) &
      "File" %in% names(existing_exp_details) == FALSE){
      existing_exp_details <- deannotateDetails(existing_exp_details)
   }
   
   if("File" %in% names(existing_exp_details) == FALSE){
      if(nrow(existing_exp_details) > 1){
         stop("You haven't included the file name in `existing_exp_details`, and `existing_exp_details` has more than one row, so we don't know which files should have which dosing regimen. We cannot assign any dose numbers.", 
              call. = FALSE)
      } else {
         warning("There isn't a column called `File` in `existing_exp_details`, so we'll assume you want the same dosing regimen applied to all files in `ct_dataframe`.", 
                 call. = FALSE)
         
         existing_exp_details$File <- "all"
      }
   }
   
   # Checking for any custom dosing. Custom dosing will only show up when you
   # get exp details for one file at a time, atm. That means that, at present,
   # if there was any custom dosing at all, that must mean that I'm only dealing
   # with one file. In the future, I'll need to adjust this to deal with
   # possibly multiple files.
   if(any(paste0("CustomDosing", AllCompounds$Suffix) %in% 
          names(existing_exp_details))){
      AnyCustDosing <- TRUE
      CD <- existing_exp_details[str_detect(names(existing_exp_details), 
                                            "CustomDosing")]
   } else {
      AnyCustDosing <- FALSE
   }
   
   if("list" %in% class(existing_exp_details)){
      existing_exp_details <-
         as.data.frame(existing_exp_details[
            which(lapply(existing_exp_details, class) %in% 
                     c("logical", "character", "numeric"))])
   }
   
   if(all(existing_exp_details$File == "all", na.rm = T)){
      if(nrow(existing_exp_details) > 1){
         stop("You have set the file names in `existing_exp_details` to `all`, but `existing_exp_details` has more than one row, so we don't know which files should have which dosing regimen. We cannot assign any dose numbers.", 
              call. = FALSE)
      } 
      
      existing_exp_details <- existing_exp_details %>% 
         select(-File) %>% 
         mutate(Placeholder = "A") %>% 
         left_join(data.frame(File = unique(ct_dataframe$File), 
                              Placeholder = "A"), 
                   by = "Placeholder") %>% 
         select(-Placeholder)
   }
   
   # Checking that all files are present in existing_exp_details and giving
   # warning if not.
   FileCheck <- setdiff(unique(ct_dataframe$File), 
                        existing_exp_details$File)
   if(length(FileCheck) > 0){
      warning(paste0("The following files are not present in `existing_exp_details`, so we can't determine what the dose numbers were and will ignore these data: ", 
                     str_c(FileCheck, collapse = "\n")), 
              call. = FALSE)
   }
   
   # Main body of function -------------------------------------------------
   
   # Adding some NA values to existing_exp_details as needed for the next bit to
   # work w/out generating a ton of warnings.
   MissingCols <- setdiff(paste0(rep(c("DoseInt", "StartHr", "Regimen", 
                                       "NumDoses"), each = 3), 
                                 c("_sub", "_inhib", "_inhib2")), 
                          names(existing_exp_details))
   
   if(length(MissingCols) > 0){
      existing_exp_details <- existing_exp_details %>% 
         bind_cols(as.data.frame(matrix(data = NA, 
                                        ncol = length(MissingCols),
                                        dimnames = list(NULL, MissingCols))))
   }
   
   # Splitting existing_exp_details and ct_dataframe by file and going through 1
   # file at a time.
   existing_exp_details <- split(existing_exp_details,
                                 f = existing_exp_details$File)
   
   ct_dataframe <- split(ct_dataframe, 
                         f = ct_dataframe$File)
   
   for(i in intersect(names(existing_exp_details), 
                      names(ct_dataframe))){
      
      # Getting dose regimen info
      MyIntervals <- 
         c("substrate" = existing_exp_details[[i]]$DoseInt_sub,
           "primary metabolite 1" = existing_exp_details[[i]]$DoseInt_sub,
           "primary metabolite 2" = existing_exp_details[[i]]$DoseInt_sub,
           "secondary metabolite" = existing_exp_details[[i]]$DoseInt_sub,
           "inhibitor 1" = existing_exp_details[[i]]$DoseInt_inhib,
           "inhibitor 1 metabolite" = existing_exp_details[[i]]$DoseInt_inhib,
           "inhibitor 2" = existing_exp_details[[i]]$DoseInt_inhib2, 
           "UNKNOWN" = NA)
      
      MyStartTimes <- 
         c("substrate" = existing_exp_details[[i]]$StartHr_sub,
           "primary metabolite 1" = existing_exp_details[[i]]$StartHr_sub,
           "primarymetabolite 2" = existing_exp_details[[i]]$StartHr_sub,
           "secondary metabolite" = existing_exp_details[[i]]$StartHr_sub,
           "inhibitor 1" = existing_exp_details[[i]]$StartHr_inhib,
           "inhibitor 2" = existing_exp_details[[i]]$StartHr_inhib2,
           "inhibitor 1 metabolite" = existing_exp_details[[i]]$StartHr_inhib, 
           "UNKNOWN" = NA)
      
      MyMaxDoseNum <- 
         c("substrate" = ifelse(existing_exp_details[[i]]$Regimen_sub == "Single Dose", 
                                1, existing_exp_details[[i]]$NumDoses_sub),
           "primary metabolite 1" = ifelse(existing_exp_details[[i]]$Regimen_sub == "Single Dose", 
                                           1, existing_exp_details[[i]]$NumDoses_sub),
           "primarymetabolite 2" = ifelse(existing_exp_details[[i]]$Regimen_sub == "Single Dose", 
                                          1, existing_exp_details[[i]]$NumDoses_sub),
           "secondary metabolite" = ifelse(existing_exp_details[[i]]$Regimen_sub == "Single Dose", 
                                           1, existing_exp_details[[i]]$NumDoses_sub),
           "inhibitor 1" = ifelse(existing_exp_details[[i]]$Regimen_inhib == "Single Dose", 
                                  1, existing_exp_details[[i]]$NumDoses_inhib),
           "inhibitor 2" = ifelse(existing_exp_details[[i]]$Regimen_inhib2 == "Single Dose", 
                                  1, existing_exp_details[[i]]$NumDoses_inhib2),
           "inhibitor 1 metabolite" = ifelse(existing_exp_details[[i]]$Regimen_inhib == "Single Dose", 
                                             1, existing_exp_details[[i]]$NumDoses_inhib), 
           "UNKNOWN" = NA)
      
      # Converting data to numeric while also retaining names
      suppressWarnings(
         MyIntervals <- sapply(MyIntervals, FUN = as.numeric))
      suppressWarnings(
         MyStartTimes <- sapply(MyStartTimes, FUN = as.numeric))
      suppressWarnings(
         MyMaxDoseNum <- sapply(MyMaxDoseNum, FUN = as.numeric))
      
      ct_dataframe[[i]] <- ct_dataframe[[i]] %>%
         mutate(StartHr = MyStartTimes[CompoundID],
                TimeSinceDose1 = Time - StartHr,
                DoseInt = MyIntervals[CompoundID],
                MaxDoseNum = MyMaxDoseNum[CompoundID],
                DoseNum = (Time - StartHr) %/% DoseInt + 1,
                # Taking care of possible artifacts
                DoseNum = ifelse(DoseNum < 0, 0, DoseNum),
                DoseNum = ifelse(DoseNum > MaxDoseNum, MaxDoseNum, DoseNum),
                # If it was a single dose, make everything after StartHr dose
                # 1 and everything before StartHr dose 0. If it was a single
                # dose, then DoseInt is NA.
                DoseNum = ifelse(is.na(DoseInt),
                                 ifelse(TimeSinceDose1 < 0, 0, 1), DoseNum))
      
      # Checking for any custom dosing
      if(AnyCustDosing){
         CDCompounds <-
            data.frame(CDnames = names(CD), 
                       CompoundSuffix = str_extract(names(CD),
                                                    "_sub|_inhib(2)?")) %>% 
            mutate(CompoundID = recode(CompoundSuffix, 
                                       "_sub" = "substrate", 
                                       "_inhib" = "inhibitor 1", 
                                       "_inhib2" = "inhibitor 2")) %>% 
            unique()
         
         if(any(unique(ct_dataframe[[i]]$CompoundID) %in% CDCompounds$CompoundID)){
            
            ct_dataframe[[i]] <- split(ct_dataframe[[i]], f = ct_dataframe[[i]]$CompoundID)
            
            for(j in CDCompounds$CDnames){
               
               jj <- CDCompounds$CompoundID[CDCompounds$CDnames == j]
               
               # message(paste("CDCompounds$CompoundID j =", j))
               if(max(ct_dataframe[[i]][[jj]]$Time) > max(CD[[j]]$Time)){
                  CD[[j]] <- CD[[j]] %>% 
                     # Need this next bit for using cut function appropriately
                     bind_rows(data.frame(Time = max(ct_dataframe[[i]][[jj]]$Time) + 1, 
                                          DoseNum = max(CD[[j]]$DoseNum)))
               }
               
               # If there was a loading dose or something (not really sure what
               # this would be), then there are two dose numbers listed for t0.
               # Removing the earlier one so that this will work.
               if(any(duplicated(CD[[j]]$Time))){
                  warning(paste0("There were multiple dose numbers listed at the same time for the ",
                                 j," in the file ", sim_data_file, 
                                 "; did you mean for that to be the case? For now, the dose number at that duplicated time will be set to the 2nd dose number listed."),
                          call. = FALSE)
                  TimeToRemove <- which(duplicated(
                     CD[[j]]$Time, fromLast = TRUE))
                  CD[[j]] <- CD[[j]] %>% slice(-TimeToRemove)
               }
               
               CD[[j]]$Breaks <-
                  as.character(cut(CD[[j]]$Time, breaks = CD[[j]]$Time,
                                   right = FALSE))
            }
            
            ct_dataframe[[i]][[jj]]$DoseNum <- NULL
            ct_dataframe[[i]][[jj]]$Breaks <-
               as.character(cut(ct_dataframe[[i]][[jj]]$Time,
                                breaks = CD[[j]]$Time,
                                right = FALSE))
            
            ct_dataframe[[i]][[jj]] <- ct_dataframe[[i]][[jj]] %>% 
               left_join(CD[[j]] %>% select(Breaks, DoseNum), 
                         by = "Breaks")
            
         }
         
         ct_dataframe[[i]] <- bind_rows(ct_dataframe[[i]])
      }
      
      # Checking for when the simulation ends right at the last dose b/c
      # then, setting that number to 1 dose lower
      if(length(ct_dataframe[[i]] %>% filter(DoseNum == max(ct_dataframe[[i]]$DoseNum)) %>%
                pull(Time) %>% unique()) == 1){
         MyMaxDoseNum <- max(ct_dataframe[[i]]$DoseNum)
         ct_dataframe[[i]] <- ct_dataframe[[i]] %>%
            mutate(DoseNum = ifelse(DoseNum == MyMaxDoseNum,
                                    MyMaxDoseNum - 1, DoseNum))
      }
      
      ct_dataframe[[i]] <- ct_dataframe[[i]] %>% select(-any_of(c("MaxDoseNum", "Breaks")))
      
      rm(MyIntervals, MyStartTimes, MyMaxDoseNum)
      # Later, if we expand extractExpDetails_mult to return the custom dosing
      # info, we'll need to also rm all the custom dosing info for each file
      # here. For now, not necessary and might mess things up. 
      
   }
   
   ct_dataframe <- bind_rows(ct_dataframe)
   
   return(ct_dataframe)
}


