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
#' @return Output is a data.frame of concentration-time data with the calculated
#'   dose number included
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
   
   Deets <- harmonize_details(existing_exp_details)[["MainDetails"]]
   
   if(all(Deets$File == "all", na.rm = T)){
      
      # Since the "if" statement can be T when "File" is not present in the
      # data.frame, making SURE that it is "all" here.
      Deets$File <- "all"
      
      if(nrow(Deets) > 1){
         stop("You have set the file names in `Deets` to `all`, but `Deets` has more than one row, so we don't know which files should have which dosing regimen. We cannot assign any dose numbers.", 
              call. = FALSE)
      } 
      
      if("File" %in% names(ct_dataframe) == FALSE){
         # This can happen if you're hacking function to get obs data dose
         # numbers.
         ct_dataframe$File <- "all"
      }
      
      Deets <- Deets %>% 
         select(-File) %>% 
         mutate(Placeholder = "A") %>% 
         left_join(data.frame(File = unique(ct_dataframe$File), 
                              Placeholder = "A"), 
                   by = "Placeholder") %>% 
         select(-Placeholder) %>% 
         mutate(File = "all")
   }
   
   # Checking that all files are present in Deets and giving
   # warning if not.
   FileCheck <- setdiff(unique(ct_dataframe$File), 
                        Deets$File)
   if(length(FileCheck) > 0){
      warning(paste0("The following files are not present in `Deets`, so we can't determine what the dose numbers were and will ignore these data: ", 
                     str_c(FileCheck, collapse = "\n")), 
              call. = FALSE)
   }
   
   # Main body of function -------------------------------------------------
   
   # Adding some NA values to Deets as needed for the next bit to
   # work w/out generating a ton of warnings.
   MissingCols <- setdiff(paste0(rep(c("DoseInt", "StartHr", "Regimen", 
                                       "NumDoses"), each = 3), 
                                 c("_sub", "_inhib", "_inhib2")), 
                          names(Deets))
   
   if(length(MissingCols) > 0){
      Deets <- Deets %>% 
         bind_cols(as.data.frame(matrix(data = NA, 
                                        ncol = length(MissingCols),
                                        dimnames = list(NULL, MissingCols))))
   }
   
   # Splitting Deets and ct_dataframe by file and going through 1
   # file at a time.
   Deets <- split(Deets,
                  f = Deets$File)
   
   ct_dataframe <- split(ct_dataframe, 
                         f = ct_dataframe$File)
   
   for(i in intersect(names(Deets), 
                      names(ct_dataframe))){
      
      # Getting dose regimen info
      MyIntervals <- 
         c("substrate" = Deets[[i]]$DoseInt_sub,
           "primary metabolite 1" = Deets[[i]]$DoseInt_sub,
           "primary metabolite 2" = Deets[[i]]$DoseInt_sub,
           "secondary metabolite" = Deets[[i]]$DoseInt_sub,
           "inhibitor 1" = Deets[[i]]$DoseInt_inhib,
           "inhibitor 1 metabolite" = Deets[[i]]$DoseInt_inhib,
           "inhibitor 2" = Deets[[i]]$DoseInt_inhib2, 
           "UNKNOWN" = NA)
      
      MyStartTimes <- 
         c("substrate" = Deets[[i]]$StartHr_sub,
           "primary metabolite 1" = Deets[[i]]$StartHr_sub,
           "primarymetabolite 2" = Deets[[i]]$StartHr_sub,
           "secondary metabolite" = Deets[[i]]$StartHr_sub,
           "inhibitor 1" = Deets[[i]]$StartHr_inhib,
           "inhibitor 2" = Deets[[i]]$StartHr_inhib2,
           "inhibitor 1 metabolite" = Deets[[i]]$StartHr_inhib, 
           "UNKNOWN" = NA)
      
      MyMaxDoseNum <- 
         c("substrate" = ifelse(Deets[[i]]$Regimen_sub == "Single Dose", 
                                1, Deets[[i]]$NumDoses_sub),
           "primary metabolite 1" = ifelse(Deets[[i]]$Regimen_sub == "Single Dose", 
                                           1, Deets[[i]]$NumDoses_sub),
           "primarymetabolite 2" = ifelse(Deets[[i]]$Regimen_sub == "Single Dose", 
                                          1, Deets[[i]]$NumDoses_sub),
           "secondary metabolite" = ifelse(Deets[[i]]$Regimen_sub == "Single Dose", 
                                           1, Deets[[i]]$NumDoses_sub),
           "inhibitor 1" = ifelse(Deets[[i]]$Regimen_inhib == "Single Dose", 
                                  1, Deets[[i]]$NumDoses_inhib),
           "inhibitor 2" = ifelse(Deets[[i]]$Regimen_inhib2 == "Single Dose", 
                                  1, Deets[[i]]$NumDoses_inhib2),
           "inhibitor 1 metabolite" = ifelse(Deets[[i]]$Regimen_inhib == "Single Dose", 
                                             1, Deets[[i]]$NumDoses_inhib), 
           "UNKNOWN" = NA)
      
      MyDose <- 
         c("substrate" = Deets[[i]]$Dose_sub,
           "primary metabolite 1" = NA,
           "primarymetabolite 2" = NA,
           "secondary metabolite" = NA,
           "inhibitor 1" = Deets[[i]]$Dose_inhib,
           "inhibitor 2" = Deets[[i]]$Dose_inhib2,
           "inhibitor 1 metabolite" = NA, 
           "UNKNOWN" = NA)
      
      # Converting data to numeric while also retaining names
      suppressWarnings(
         MyIntervals <- sapply(MyIntervals, FUN = as.numeric))
      suppressWarnings(
         MyStartTimes <- sapply(MyStartTimes, FUN = as.numeric))
      suppressWarnings(
         MyMaxDoseNum <- sapply(MyMaxDoseNum, FUN = as.numeric))
      suppressWarnings(
         MyDose <- sapply(MyDose, FUN = as.numeric))
      
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
                                 ifelse(TimeSinceDose1 < 0, 0, 1), DoseNum), 
                Dose_sub = MyDose["substrate"],
                Dose_inhib = MyDose["inhibitor 1"], 
                Dose_inhib2 = MyDose["inhibitor 2"])
      
      # Checking for any custom dosing
      if(is.null(existing_exp_details$CustomDosing) == FALSE &&
         nrow(existing_exp_details$CustomDosing) > 0){
         
         # Need match the conc-time compound ID with the CustomDosing data.frame
         # based on the compound that was DOSED for this to work with
         # metabolites.
         ct_dataframe[[i]] <- ct_dataframe[[i]] %>% 
            mutate(DosedCompoundID =
                      case_when(CompoundID %in% c("substrate", 
                                                  "primary metabolite 1",
                                                  "primary metabolite 2", 
                                                  "secondary metabolite") ~ "substrate", 
                                CompoundID %in% c("inhibitor 1", 
                                                  "inhibitor 1 metabolite") ~ "inhibitor 1", 
                                CompoundID %in% c("inhibitor 2") ~ "inhibitor 2"))
         
         ct_dataframe[[i]] <- split(ct_dataframe[[i]],
                                    f = ct_dataframe[[i]]$DosedCompoundID)
         
         CD_i <- existing_exp_details$CustomDosing %>% filter(File == i) %>% 
            mutate(Dose_sub = case_when(complete.cases(Dose) & 
                                           CompoundID == "substrate" ~ Dose), 
                   Dose_inhib = case_when(complete.cases(Dose) & 
                                             CompoundID == "inhibitor 1" ~ Dose), 
                   Dose_inhib2 = case_when(complete.cases(Dose) & 
                                              CompoundID == "inhibitor 2" ~ Dose)) %>% 
            select(-Dose)
         
         if(nrow(CD_i) > 0){
            
            CD_i <- split(CD_i, f = CD_i$CompoundID)
            
            for(j in intersect(names(CD_i), names(ct_dataframe[[i]]))){
               
               if(max(ct_dataframe[[i]][[j]]$Time) > max(CD_i[[j]]$Time)){
                  CD_i[[j]] <- CD_i[[j]] %>% 
                     # Need this next bit for using cut function appropriately
                     bind_rows(data.frame(Time = max(ct_dataframe[[i]][[j]]$Time) + 1, 
                                          DoseNum = max(CD_i[[j]]$DoseNum)))
               }
               
               # If there was a loading dose or something (not really sure what
               # this would be), then there are two dose numbers listed for t0.
               # Removing the earlier one so that this will work.
               if(any(duplicated(CD_i[[j]]$Time))){
                  warning(wrapn(paste0("There were multiple dose numbers listed at the same time for the ",
                                 j," in the file ", i, 
                                 "; did you mean for that to be the case? For now, the dose number at that duplicated time will be set to the 2nd dose number listed.")),
                          call. = FALSE)
                  TimeToRemove <- which(duplicated(
                     CD_i[[j]]$Time, fromLast = TRUE))
                  CD_i[[j]] <- CD_i[[j]] %>% slice(-TimeToRemove)
               }
               
               CD_i[[j]]$Breaks <-
                  as.character(cut(CD_i[[j]]$Time,
                                   breaks = CD_i[[j]]$Time,
                                   right = FALSE))
               
               ct_dataframe[[i]][[j]]$DoseNum <- NULL
               ct_dataframe[[i]][[j]]$Breaks <-
                  as.character(cut(ct_dataframe[[i]][[j]]$Time,
                                   breaks = CD_i[[j]]$Time,
                                   right = FALSE))
               
               ct_dataframe[[i]][[j]] <- ct_dataframe[[i]][[j]] %>% 
                  select(-Dose_sub, -Dose_inhib, -Dose_inhib2) %>% 
                  left_join(CD_i[[j]] %>% 
                               select(Breaks, DoseNum, Dose_units,
                                      Dose_sub, Dose_inhib, Dose_inhib2), 
                            by = "Breaks")
            }
         }
         
         rm(CD_i)
         
      }
      
      ct_dataframe[[i]] <- bind_rows(ct_dataframe[[i]])
      
      # Checking for when the simulation ends right at the last dose b/c
      # then, setting that number to 1 dose lower
      if(length(ct_dataframe[[i]] %>% filter(DoseNum == max(ct_dataframe[[i]]$DoseNum)) %>%
                pull(Time) %>% unique()) == 1){
         MyMaxDoseNum <- max(ct_dataframe[[i]]$DoseNum)
         ct_dataframe[[i]] <- ct_dataframe[[i]] %>%
            mutate(DoseNum = ifelse(DoseNum == MyMaxDoseNum,
                                    MyMaxDoseNum - 1, DoseNum))
      }
      
      ct_dataframe[[i]] <- ct_dataframe[[i]] %>%
         select(-any_of(c("MaxDoseNum", "Breaks")))
      
      rm(MyIntervals, MyStartTimes, MyMaxDoseNum)
      
   }
   
   ct_dataframe <- bind_rows(ct_dataframe)
   
   return(ct_dataframe)
   
}


