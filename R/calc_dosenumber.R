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
#'   We have not set up this function to check that your files match in order to
#'   allow for some flexibility here. If you supply \code{existing_exp_details}
#'   with more than one simulator output file, we'll just use the 1st one.
#'
#' @return Output is a data.frame of concentration-time data with
#' @export
#'
#' @examples
#' # None yet
#' 
calc_dosenumber <- function(ct_dataframe, 
                            existing_exp_details){
    
    # Error catching ------------------------------------------------------
    
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    # Checking whether existing_exp_details needs to be de-annotated
    if("data.frame" %in% class(existing_exp_details) &
       "File" %in% names(existing_exp_details) == FALSE){
        existing_exp_details <- deannotateDetails(existing_exp_details)
        
        # For now at least, I'm NOT setting this up to check whether all the files
        # had the same dosing regimen or were, in fact, included in ct_dataframe b/c
        # I want some flexibility in how this is applied. This will assume that the
        # dosing regimen info in existing_exp_details pertains to ALL the data in
        # ct_dataframe. Just using whatever is the 1st file in existing_exp_details
        # to get the dosing regimen information.
        existing_exp_details <- existing_exp_details[1, ]
        
    }
    
    # Main body of function -------------------------------------------------
    # Getting dose regimen info
    MyIntervals <- 
        c("substrate" = existing_exp_details$DoseInt_sub,
          "primary metabolite 1" = existing_exp_details$DoseInt_sub,
          "primary metabolite 2" = existing_exp_details$DoseInt_sub,
          "secondary metabolite" = existing_exp_details$DoseInt_sub,
          "inhibitor 1" = ifelse(is.null(existing_exp_details$DoseInt_inhib),
                                 NA, existing_exp_details$DoseInt_inhib),
          "inhibitor 1 metabolite" = ifelse(is.null(existing_exp_details$DoseInt_inhib),
                                            NA, existing_exp_details$DoseInt_inhib),
          "inhibitor 2" = ifelse(is.null(existing_exp_details$DoseInt_inhib2),
                                 NA, existing_exp_details$DoseInt_inhib2), 
          "UNKNOWN" = NA)
    
    MyStartTimes <- 
        c("substrate" = existing_exp_details$StartHr_sub,
          "primary metabolite 1" = existing_exp_details$StartHr_sub,
          "primarymetabolite 2" = existing_exp_details$StartHr_sub,
          "secondary metabolite" = existing_exp_details$StartHr_sub,
          "inhibitor 1" = ifelse(is.null(existing_exp_details$StartHr_inhib), NA,
                                 existing_exp_details$StartHr_inhib),
          "inhibitor 2" = ifelse(is.null(existing_exp_details$StartHr_inhib2), NA,
                                 existing_exp_details$StartHr_inhib2),
          "inhibitor 1 metabolite" = ifelse(is.null(existing_exp_details$StartHr_inhib), NA,
                                            existing_exp_details$StartHr_inhib), 
          "UNKNOWN" = NA)
    
    MyMaxDoseNum <- 
        c("substrate" = ifelse(existing_exp_details$Regimen_sub == "Single Dose", 
                               1, existing_exp_details$NumDoses_sub),
          "primary metabolite 1" = ifelse(existing_exp_details$Regimen_sub == "Single Dose", 
                                          1, existing_exp_details$NumDoses_sub),
          "primarymetabolite 2" = ifelse(existing_exp_details$Regimen_sub == "Single Dose", 
                                         1, existing_exp_details$NumDoses_sub),
          "secondary metabolite" = ifelse(existing_exp_details$Regimen_sub == "Single Dose", 
                                          1, existing_exp_details$NumDoses_sub),
          "inhibitor 1" = ifelse(is.null(existing_exp_details$NumDoses_inhib), NA,
                                 ifelse(existing_exp_details$Regimen_inhib == "Single Dose", 
                                        1, existing_exp_details$NumDoses_inhib)),
          "inhibitor 2" = ifelse(is.null(existing_exp_details$NumDoses_inhib2), NA,
                                 ifelse(existing_exp_details$Regimen_inhib2 == "Single Dose", 
                                        1, existing_exp_details$NumDoses_inhib2)),
          "inhibitor 1 metabolite" = ifelse(is.null(existing_exp_details$NumDoses_inhib), NA,
                                            ifelse(existing_exp_details$Regimen_inhib == "Single Dose", 
                                                   1, existing_exp_details$NumDoses_inhib)), 
          "UNKNOWN" = NA)
    
    # Converting data to numeric while also retaining names
    suppressWarnings(
        MyIntervals <- sapply(MyIntervals, FUN = as.numeric))
    suppressWarnings(
        MyStartTimes <- sapply(MyStartTimes, FUN = as.numeric))
    suppressWarnings(
        MyMaxDoseNum <- sapply(MyMaxDoseNum, FUN = as.numeric))
    
    ct_dataframe <- ct_dataframe %>%
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
    if(any(str_detect(names(existing_exp_details), "CustomDosing"))){
        CDCompounds <-
            data.frame(CompoundSuffix = 
                           str_extract(names(existing_exp_details)[str_detect(names(existing_exp_details),
                                                                              "CustomDosing")],
                                       "_sub|_inhib(2)?")) %>% 
            mutate(CompoundID = recode(CompoundSuffix, "_sub" = "substrate", 
                                       "_inhib" = "inhibitor 1", 
                                       "_inhib2" = "inhibitor 2"))
        
        if(any(unique(ct_dataframe$CompoundID) %in% CDCompounds$CompoundID)){
            
            Dosing <- list()
            # This is kind of a disaster... Looking for a better way to code this.
            
            for(j in CDCompounds$CompoundID){
                
                # message(paste("CDCompounds$CompoundID j =", j))
                Dosing[[j]] <-
                    existing_exp_details[[paste0("CustomDosing", 
                                                 CDCompounds$CompoundSuffix[CDCompounds$CompoundID == j])]] %>% 
                    mutate(CompoundID = CDCompounds$CompoundID[CDCompounds$CompoundID == j])
                
                if(max(ct_dataframe$Time) > max(Dosing[[j]]$Time)){
                    Dosing[[j]] <- Dosing[[j]] %>% 
                        bind_rows(data.frame(Time = max(ct_dataframe$Time) + 1, 
                                             DoseNum = max(Dosing[[j]]$DoseNum)))
                }
                
                # If there was a loading dose or something (not really sure what
                # this would be), then there are two dose numbers listed for t0.
                # Removing the earlier one so that this will work.
                if(any(duplicated(Dosing[[j]]$Time))){
                    warning(paste0("There were multiple dose numbers listed at the same time for the ",
                                   j," in the file ", sim_data_file, 
                                   "; did you mean for that to be the case? For now, the dose number at that duplicated time will be set to the 2nd dose number listed."),
                            call. = FALSE)
                    TimeToRemove <- which(duplicated(
                        Dosing[[j]]$Time, fromLast = TRUE))
                    Dosing[[j]] <- Dosing[[j]] %>% slice(-TimeToRemove)
                }
                
                Dosing[[j]]$Breaks <-
                    as.character(cut(Dosing[[j]]$Time, breaks = Dosing[[j]]$Time,
                                     right = FALSE))
            }
            
            OrigCompounds <- unique(ct_dataframe$CompoundID)
            ct_dataframe <- ct_dataframe %>% 
                mutate(CD = ifelse(CompoundID %in% CDCompounds$CompoundID, 
                                   CompoundID, "not CD"))
            
            MyData <- list()
            MyData[["not CD"]] <- ct_dataframe %>% filter(CD == "not CD")
            
            for(j in unique(ct_dataframe$CD)[!unique(ct_dataframe$CD) == "not CD"]){
                
                # message(paste("CDCompounds$CompoundID (not CD) j =", j))
                MyData[[j]] <- ct_dataframe %>% filter(CD == j) %>% select(-DoseNum)
                # This should make the right breaks for each possible compound
                # with custom dosing. They should match the breaks in the
                # appropriate list item in Dosing.
                MyData[[j]]$Breaks <-
                    as.character(cut(MyData[[j]]$Time, breaks = Dosing[[j]]$Time,
                                     right = FALSE))
                
                suppressMessages(
                    MyData[[j]] <- MyData[[j]] %>% 
                        left_join(Dosing[[j]] %>% select(CompoundID, Breaks, DoseNum))
                )
                
            }
            
            ct_dataframe <- bind_rows(MyData)
            if(length(setdiff(unique(OrigCompounds),
                              unique(ct_dataframe$CompoundID))) > 0){
                warning("PROBLEM WITH CUSTOM DOSING! Please tell Laura Shireman if you see this message.",
                        call. = FALSE)
            }
        }
    }
    
    # Checking for when the simulation ends right at the last dose b/c
    # then, setting that number to 1 dose lower
    if(length(ct_dataframe %>% filter(DoseNum == max(ct_dataframe$DoseNum)) %>%
              pull(Time) %>% unique()) == 1){
        MyMaxDoseNum <- max(ct_dataframe$DoseNum)
        ct_dataframe <- ct_dataframe %>%
            mutate(DoseNum = ifelse(DoseNum == MyMaxDoseNum,
                                    MyMaxDoseNum - 1, DoseNum))
    }
    
    return(ct_dataframe)
    
}
