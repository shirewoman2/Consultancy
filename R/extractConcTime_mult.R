#' Pull concentration-time data from multiple Simcyp Simulator output files
#'
#' \code{extractConcTime_mult} is meant to be used in conjunction with
#' \code{\link{ct_plot_overlay}} or \code{\link{ct_plot_mult}} to create graphs
#' from multiple Simcyp Simulator output files. \strong{A couple of
#' notes:}\enumerate{\item{If ANY of the Excel files you wish to extract data
#' from are saved on SharePoint and are open, this WILL CRASH and WILL NOT save
#' whatever progress it has made so far. Be sure to close all of the source
#' Excel files.} \item{If you list multiple files, multiple tissues, and/or
#' multiple compounds to extract (see options below), this will extract
#' \emph{all} possible variations of them. For example, if you ask for
#' "sim1.xlsx" and "sim2.xlsx" and then also ask for "substrate" and "primary
#' metabolite 1", you will get the substrate and primary metabolite 1 data from
#' \emph{both} files.}}
#'
#' \strong{Regarding dose intervals for observed data:} The observed data files
#' don't include information on dosing intervals or dose numbers, which makes it
#' a little tricky to figure out which dose number a given time in an observed
#' data file should have. If the compound IDs in the simulated data match those
#' in the observed data, we will assume that the dosing intervals are the same.
#' This was coded with the assumption that the dosing interval would be a round
#' number (subjects aren't getting dosed on the half hour, for example), so if
#' that's not the case, these dose number assignments will be off.
#'
#' @param sim_data_files a character vector of simulator output files, each in
#'   quotes and encapsulated with \code{c(...)}, or, if left as NA, \emph{all}
#'   the Excel files in the current directory. Example of acceptable input:
#'   \code{c("sim1.xlsx", "sim2.xlsx")}. The path should be included with the
#'   file names if they are located somewhere other than your working directory.
#'   If some of your Excel files are not regular simulator output, e.g. they are
#'   sensitivity analyses or a file where you were doing some calculations,
#'   those files will be skipped.
#' @param obs_data_files a character vector of the observed data filess, each in
#'   quotes and encapsulated with \code{c(...)}, that you'd like to compare,
#'   e.g., \code{c("obsdata1.xlsx", "obsdata2.xlsx")}. The path should be
#'   included with the file names if they are located somewhere other than your
#'   working directory. This is the file that it is ready to be converted to an
#'   XML file, not the file that contains only the digitized time and
#'   concentration data. This function assumes that the dosing intervals for the
#'   observed data match those in the simulated data. See "Details" for more
#'   info.
#' @param ct_dataframe (optional) a data.frame that contains previously
#'   extracted concentration-time data. This should NOT be in quotes. Because we
#'   can see scenarios where you might want to extract some concentration-time
#'   data, play around with those data, and then later decide you want to pull
#'   more concentration-time data for comparisons, this data.frame can already
#'   exist. When that is the case, this function will \emph{add} data to that
#'   data.frame. It will \emph{not} overwrite existing data unless
#'   \code{overwrite} is set to TRUE.
#' @param overwrite TRUE or FALSE (default) on whether to re-extract the
#'   concentration-time data from output files that are already included in
#'   \code{ct_dataframe}. Since pulling data from Excel files is slow, by
#'   default, this will \emph{not} overwrite existing data and instead will only
#'   add data from any Excel files that aren't already included. A situation
#'   where you might want to set this to TRUE would be when you have changed
#'   input parameters for simulations and re-run them.
#' @param tissues From which tissue(s) should the desired concentrations be
#'   extracted? The default is plasma for typical plasma concentration-time
#'   data. Other options are "blood" or any tissues included in "Sheet Options",
#'   "Tissues" in the simulator. All possible options: "plasma", "blood",
#'   "unbound blood", "unbound plasma", "additional organ", "adipose", "bone",
#'   "brain", "feto-placenta", "GI tissue", "heart", "kidney", "liver", "lung",
#'   "muscle", "pancreas", "peripheral blood", "peripheral plasma", "peripheral
#'   unbound blood", "peripheral unbound plasma", "portal vein blood", "portal
#'   vein plasma", "portal vein unbound blood", "portal vein unbound plasma",
#'   "skin", or "spleen". Not case sensitive. Acceptable input is all tissues
#'   desired as a character vector, e.g., \code{c("plasma", "blood", "liver")}.
#' @param compoundsToExtract For which compound(s) do you want to extract
#'   concentration-time data? Options are: \itemize{\item{"substrate"
#'   (default),} \item{"primary metabolite 1",} \item{"primary metabolite 2",}
#'   \item{"secondary metabolite",} \item{"inhibitor 1" (this can be an inducer,
#'   inhibitor, activator, or suppresesor, but it's labeled as "Inhibitor 1" in
#'   the simulator),} \item{"inhibitor 2" for the 2nd inhibitor listed in the
#'   simulation,} \item{"inhibitor 1 metabolite" for the primary metabolite of
#'   inhibitor 1, or} \item{"all" for all possible compounds in the
#'   simulation.}} Input to this argument should be all desired compounds as a
#'   character vector, e.g., \code{c("substrate", "primary metabolite 1")}.
#' @param ... other arguments passed to the function
#'   \code{\link{extractConcTime}}
#' @param conc_units_to_use concentration units to use so that all data will be
#'   comparable. Options are the same as the ones in the Excel form for PE data
#'   entry. Default is "ng/mL". Note: ADAM model data concentration units are
#'   not converted because there are simply too many units to manage easily, so
#'   please check that the units are what you expected in the end.
#' @param time_units_to_use time units to use so that all data will be
#'   comparable. Options are "hours" (default) or "minutes".
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated
#'   concentration-time data? Options are "aggregate" (default), "individual",
#'   or "both". Aggregated data are not calculated here but are pulled from the
#'   simulator output rows labeled as "Population Statistics".
#'
#' @return Returns a large data.frame with multiple sets of concentration-time
#'   data, formatted the same way as output from the function
#'   \code{\link{extractConcTime}}
#' @export
#'
#' @examples
#' ConcTimeData <-
#'       extractConcTime_mult(
#'             sim_data_files = c("MyFile1.xlsx", "MyFile2.xlsx"),
#'             ct_dataframe = "ConcTimeData",
#'             overwrite = FALSE,
#'             tissue = "unbound plasma") # Note that "tissue" is passed to "extractConcTime".
#' 

extractConcTime_mult <- function(sim_data_files = NA,
                                 obs_data_files = NA,
                                 ct_dataframe = ConcTime,
                                 overwrite = FALSE,
                                 tissues = "plasma",
                                 compoundsToExtract = "substrate",
                                 conc_units_to_use = "ng/mL",
                                 time_units_to_use = "hours",
                                 returnAggregateOrIndiv = "aggregate",
                                 ...){
    
    # Error catching -------------------------------------------------------
    
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    
    # Main body of function -----------------------------------------------
    
    compoundsToExtract <- tolower(compoundsToExtract)
    
    if(length(sim_data_files) == 1 && is.na(sim_data_files)){
        # If left as NA, pull all the files in this folder. 
        sim_data_files <- list.files(pattern = "xlsx")
        sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
    }
    
    # Checking on what combinations of data the user has requested and what
    # data are already present in ct_dataframe.
    Requested <- expand.grid(Tissue = tissues,
                             CompoundID = compoundsToExtract,
                             File = sim_data_files)
    
    if(exists(substitute(ct_dataframe)) && 
       "data.frame" %in% class(ct_dataframe)){
        if("File" %in% names(ct_dataframe) == FALSE){
            ct_dataframe$File <- "unknown file"
        }
        
        ct_dataframe <- ct_dataframe %>%
            mutate(ID = paste(File, Tissue, CompoundID))
        
        DataToFetch <- ct_dataframe %>% select(File, Tissue, CompoundID) %>%
            unique() %>% mutate(ExistsAlready = TRUE) %>%
            right_join(Requested) %>%
            filter(is.na(ExistsAlready)) %>% select(-ExistsAlready) %>%
            mutate(ID = paste(File, Tissue, CompoundID))
        
        if(overwrite == FALSE){
            sim_data_files_topull <- unique(DataToFetch$File)
        } else {
            sim_data_files_topull <- sim_data_files
            ct_dataframe <- ct_dataframe %>%
                filter(!ID %in% DataToFetch$ID)
        }
    } else {
        DataToFetch <- Requested
        sim_data_files_topull <- sim_data_files
        ct_dataframe <- data.frame()
    }
    
    if(length(sim_data_files_topull) == 0){
        message("There are no data to pull that are not already present in your current data.frame. Returning current data.frame.")
        return(ct_dataframe)
    }
    
    MultData <- list()
    
    for(f in sim_data_files_topull){
        message(paste("Extracting data from file =", f))
        MultData[[f]] <- list()
        
        # Getting summary data for the simulation(s)
        Deets <- extractExpDetails(f, exp_details = "Input Sheet")
        
        # Names of compounds requested for checking whether the data exist
        CompoundCheck <- c("substrate" = Deets$Substrate,
                           "inhibitor 1" = Deets$Inhibitor1,
                           "inhibitor 1 metabolite" = Deets$Inhibitor1Metabolite,
                           "inhibitor 2" = Deets$Inhibitor2,
                           "primary metabolite 1" = Deets$PrimaryMetabolite1,
                           "primary metabolite 2" = Deets$PrimaryMetabolite2,
                           "secondary metabolite" = Deets$SecondaryMetabolite)
        
        if(compoundsToExtract[1] == "all"){
            compoundsToExtract_n <- names(CompoundCheck)[complete.cases(CompoundCheck)]
        } else {
            # If the requested compound is not present in the Excel file, remove
            # it from consideration.
            compoundsToExtract_n <- intersect(compoundsToExtract,
                                              names(CompoundCheck)[complete.cases(CompoundCheck)])
        }
        
        if(compoundsToExtract[1] != "all" &&
           all(compoundsToExtract %in% compoundsToExtract_n) == FALSE){
            warning(paste0("For the file ", f, ", the compound(s) ",
                           str_comma(setdiff(compoundsToExtract, compoundsToExtract_n)),
                           " was/were not available."),
                    call. = FALSE)
        }
        
        # Each tissue will be on its own sheet in the Excel file, so each
        # will need their own iterations of the loop for reading different
        # sheets.
        for(j in tissues){
            
            message(paste("Extracting data for tissue =", j))
            # Depending on both the tissue AND which compound the user
            # requests, that could be on multiple sheets or on a single
            # sheet. Figuring out which sheet to read.
            
            # Extracting tissue or plasma/blood data? Sheet format differs.
            TissueType <- ifelse(str_detect(j, "plasma|blood|portal|peripheral"),
                                 "systemic", "tissue")
            
            if(TissueType == "tissue"){
                # If the tissue type is a solid tissue, then any
                # compound concentrations available will be on that
                # sheet and that requires only one iteration of the
                # loop.
                
                MultData[[f]][[j]] <- extractConcTime(
                    sim_data_file = f,
                    obs_data_file = NA,
                    compoundToExtract = compoundsToExtract_n,
                    tissue = j,
                    returnAggregateOrIndiv = returnAggregateOrIndiv, 
                    fromMultFunction = TRUE, 
                    expdetails = Deets)
                
                # When the particular combination of compound and tissue is not
                # available in that file, extractConcTime will return an empty
                # data.frame, which we don't want to be included in the final
                # data. Not adding info for File in that scenario b/c it would
                # add a row to what would have been an empty data.frame.
                if(nrow(MultData[[f]][[j]]) > 0){
                    MultData[[f]][[j]] <-
                        MultData[[f]][[j]] %>%
                        mutate(File = f)
                    
                    # Need to handle ADAM data specially
                    ADAMtissue <- c("stomach", "duodenum", "jejunum i",
                                    "jejunum ii", "ileum i", "ileum ii",
                                    "ileum iii", "ileum iv", "colon", "faeces")
                    if(any(MultData[[f]][[j]]$Tissue %in% ADAMtissue)){
                        CT_adam <- MultData[[f]][[j]] %>% 
                            filter(Tissue %in% ADAMtissue)
                        
                        
                        CT_nonadam <- MultData[[f]][[j]] %>% 
                            filter(Tissue %in% ADAMtissue == FALSE)
                        
                        if(nrow(CT_nonadam) > 0){
                            CT_nonadam <- CT_nonadam %>% 
                                match_units(DF_to_adjust = CT_nonadam,
                                            goodunits = list("Conc_units" = conc_units_to_use,
                                                             "Time_units" = time_units_to_use))
                        }
                        
                        MultData[[f]][[j]] <- bind_rows(CT_adam, CT_nonadam)
                        
                    } else {
                        
                        MultData[[f]][[j]] <-
                            match_units(DF_to_adjust = MultData[[f]][[j]],
                                        goodunits = list("Conc_units" = conc_units_to_use,
                                                         "Time_units" = time_units_to_use))
                    }
                }
                
            } else {
                # If TissueType is systemic, then substrate and
                # inhibitor concs are on the same sheet, but metabolite
                # concs are elsewhere.
                CompoundTypes <-
                    data.frame(PossCompounds =
                                   c("substrate", "inhibitor 1",
                                     "inhibitor 2", "inhibitor 1 metabolite",
                                     "primary metabolite 1",
                                     "primary metabolite 2",
                                     "secondary metabolite")) %>%
                    mutate(Type = ifelse(PossCompounds %in%
                                             c("substrate", "inhibitor 1",
                                               "inhibitor 2", "inhibitor 1 metabolite"),
                                         "substrate", PossCompounds)) %>%
                    filter(PossCompounds %in% compoundsToExtract_n)
                
                MultData[[f]][[j]] <- list()
                
                for(k in unique(CompoundTypes$Type)){
                    
                    # print(paste("CompoundTypes$Type k =", k))
                    compoundsToExtract_k <-
                        CompoundTypes %>% filter(Type == k) %>%
                        pull(PossCompounds)
                    
                    MultData[[f]][[j]][[k]] <-
                        extractConcTime(
                            sim_data_file = f,
                            obs_data_file = NA,
                            compoundToExtract = compoundsToExtract_k,
                            tissue = j,
                            returnAggregateOrIndiv = returnAggregateOrIndiv, 
                            fromMultFunction = TRUE, 
                            expdetails = Deets)
                    
                    # When the particular combination of compound and
                    # tissue is not available in that file,
                    # extractConcTime will return an empty data.frame,
                    # which we don't want to be included in the final
                    # data. Not adding info for File in that scenario
                    # b/c it would add a row to what would have been
                    # an empty data.frame.
                    if(nrow(MultData[[f]][[j]][[k]]) > 0){
                        MultData[[f]][[j]][[k]] <-
                            MultData[[f]][[j]][[k]] %>%
                            mutate(File = f)
                        
                        MultData[[f]][[j]][[k]] <-
                            match_units(DF_to_adjust = MultData[[f]][[j]][[k]],
                                        goodunits = list("Conc_units" = conc_units_to_use,
                                                         "Time_units" = time_units_to_use))
                    }
                    rm(compoundsToExtract_k)
                }
                
                MultData[[f]][[j]] <- bind_rows(MultData[[f]][[j]])
            }
        }
        
        MultData[[f]] <- bind_rows(MultData[[f]])
        
        # MUST remove Deets or you can get the wrong info for each file!!!
        rm(Deets, CompoundCheck, compoundsToExtract_n) 
        
    }
    
    MultData <- bind_rows(MultData)
    if(nrow(MultData) > 0 & complete.cases(obs_data_files[1])){
        # If they specified observed data files, it's better to use those than
        # to use the data included with the simulation. There's more information
        # that way.
        MultData <- MultData %>% filter(Simulated == TRUE)
    }
    
    # Observed data ------------------------------------------------------
    if(length(setdiff(obs_data_files, unique(ct_dataframe$ObsFile))) > 0 &&
       any(complete.cases(obs_data_files))){
        MultObsData <- list()
        if(overwrite){
            ct_dataframe <- ct_dataframe %>% filter(!ObsFile %in% obs_data_files)
            for(f in obs_data_files){
                message(paste("Extracting data from observed data file =", f))
                MultObsData[[f]] <- extractObsConcTime(f)
            }
        } else {
            for(f in setdiff(obs_data_files, unique(ct_dataframe$ObsFile))){
                MultObsData[[f]] <- extractObsConcTime(f)
            }
        }
        
        # Removing any compounds not included in user request
        MultObsData <- bind_rows(MultObsData) %>% 
            mutate(Simulated = FALSE) %>% 
            filter(CompoundID %in% compoundsToExtract)
        
        # We can we add a dose number and/or dose interval to these data if the
        # compound IDs match the simulated data. We're going to assume that the
        # dose timings are the same as in the simulated data.
        ObsCompoundID <- MultObsData %>% pull(CompoundID) %>% unique()
        SimDoseInfo <- bind_rows(ct_dataframe, MultData) %>%
            filter(CompoundID %in% ObsCompoundID) %>% 
            group_by(CompoundID, Inhibitor, DoseInt, DoseNum) %>% 
            summarize(TimeRounded = round(min(Time)))
        
        # If there is only "InhbitorX" and "none" in the column "Inhibitor" for
        # the simulated data and there's only "inhibitor 1" and no other
        # effectors, then it's safe to assume that "inhibitor 1" should be
        # labeled as "InhibitorX" in the observed data, too.
        SimEffector <- unique(MultData$Inhibitor)
        SimEffector <- SimEffector[!SimEffector == "none"]
        if(length(SimEffector) == 1 &&
           "inhibitor 1" %in% SimDoseInfo$CompoundID &&
           any(c("inhibitor 2", "inhibitor 1 metabolite") %in% 
               SimDoseInfo$CompoundID) == FALSE){
            MultObsData$Inhibitor[MultObsData$Inhibitor != "none"] <- SimEffector
        }
        
        # Similarly, if there's only one value for Compound for each CompoundID,
        # then assigning that value to the observed data.
        CompoundCheck <- MultData %>% group_by(CompoundID) %>% 
            summarize(OneCompound = length(unique(Compound)) == 1) %>% 
            filter(OneCompound == TRUE) %>% pull(CompoundID)
        
        if(length(CompoundCheck) > 0){
            MultObsData <- MultObsData %>% 
                left_join(MultData %>% filter(CompoundID %in% CompoundCheck) %>% 
                              select(Compound, CompoundID) %>% unique(), 
                          by = "CompoundID")
        }
        
        # If there are both SD and MD data for a given CompoundID, which is the
        # case when DoseInt is sometimes NA and sometimes has a value, then give
        # user a warning about that but don't try to assign dose numbers to obs
        # data since we have no way of knowing which is which.
        MultRegimenCheck <- SimDoseInfo %>% group_by(CompoundID) %>% 
            summarize(MultRegimens = length(unique(DoseInt)) > 1) %>% 
            filter(MultRegimens == TRUE)
        
        if(nrow(MultRegimenCheck) > 0){
            warning(paste0("It looks like you have both single-dose and multiple-dose simulated data present for the compound(s) ",
                           str_comma(MultRegimenCheck$CompoundID), 
                           ". We thus cannot safely assign the observed data for that/those compound(s) to any particular dose number since we don't know which simulated files the observed data match. Output will include both simulated and observed data, but the observed data will have NA values for DoseInt and DoseNum for those compounds."),
                    call. = FALSE)
        } else {
            SimDoseInfo_list <- 
                split(SimDoseInfo %>%
                          filter(CompoundID %in% MultRegimenCheck$CompoundID == FALSE),
                      f = list(SimDoseInfo$CompoundID, 
                               SimDoseInfo$Inhibitor))
            SimDoseInfo_list <- SimDoseInfo_list[which(
                sapply(SimDoseInfo_list, FUN = nrow) > 0)]
            MultObsData <- split(MultObsData, 
                                 f = list(MultObsData$CompoundID, 
                                          MultObsData$Inhibitor))
            
            for(i in intersect(names(SimDoseInfo_list), names(MultObsData))){
                
                if(all(is.na(SimDoseInfo_list[[i]]$DoseInt)) &&
                   nrow(SimDoseInfo_list[[i]]) == 1){
                    # This is when it was a single dose and there was only one
                    # dosing time. If there's more than 1 row here for
                    # SimDoseInfo_list[[i]], then everything was single dose but
                    # dosing started at different times. Not setting DoseNum
                    # here then b/c that's tricky to figure out which start time
                    # matches which observed file and not sure it's worth the
                    # time trying to figure it out. For the other scenario, when
                    # there's only one start time, setting dose number to 1 for
                    # any times after dose administration and to 0 for any time
                    # before then.
                    DoseTime <- SimDoseInfo_list[[i]]$TimeRounded
                    MultObsData[[i]] <- MultObsData[[i]] %>% 
                        mutate(DoseNum = ifelse(Time >= {{DoseTime}}, 1, 0))
                    rm(DoseTime)
                    
                } else {
                    
                    # This is the scenario when there were multiple doses. The
                    # dosing interval doesn't have to be the same each time (ok
                    # if it's custom dosing), but there must be only one value
                    # for TimeRounded for each DoseNum for us to be able to
                    # assign which observed data went with which dose number.
                    
                    Check <- SimDoseInfo_list[[i]] %>% group_by(DoseNum) %>% 
                        summarise(SingleDoseTime = n() == 1)
                    
                    if(all(Check$SingleDoseTime)){
                        SimDoseInfo_list[[i]] <- SimDoseInfo_list[[i]] %>% 
                            ungroup() %>% 
                            mutate(Breaks = as.character(
                                cut(TimeRounded, breaks = TimeRounded, right = FALSE)))
                        
                        MultObsData[[i]] <- MultObsData[[i]] %>% 
                            mutate(TimeRounded = round(Time),
                                   Breaks = as.character(
                                       cut(Time, breaks = SimDoseInfo_list[[i]]$TimeRounded, 
                                           right = FALSE))) %>% 
                            left_join(SimDoseInfo_list[[i]] %>% select(Breaks, DoseInt, DoseNum), 
                                      by = c("Breaks"))
                        
                        # Checking for when the simulation ends right at the last dose
                        # b/c then, setting that number to 1 dose lower
                        if(length(MultObsData[[i]] %>%
                                  filter(DoseNum == max(MultObsData[[i]]$DoseNum)) %>%
                                  pull(Time) %>% unique()) == 1){
                            MyMaxDoseNum <- max(MultObsData[[i]]$DoseNum)
                            MultObsData[[i]] <- MultObsData[[i]] %>%
                                mutate(DoseNum = ifelse(DoseNum == MyMaxDoseNum,
                                                        MyMaxDoseNum - 1, DoseNum))
                        }
                    }
                }
            }
            
            MultObsData <- bind_rows(MultObsData)
        }
        
        ct_dataframe <- bind_rows(ct_dataframe, MultObsData)
        
    }
    
    # all data together -------------------------------------------------
    ct_dataframe <- bind_rows(ct_dataframe, MultData) %>% 
        select(-any_of(c("ID", "Breaks"))) %>% 
        arrange(across(any_of(c("File", "Compound", "Inhibitor", "Simulated",
                                "Individual", "Trial", "Time")))) %>%
        select(any_of(c("Compound", "CompoundID", "Inhibitor", "Simulated",
                        "Species", "Tissue", "Individual", "Trial",
                        "Simulated", "Time", "Conc",
                        "Time_units", "Conc_units", "subsection_ADAM", "DoseNum",
                        "DoseInt", "File", "ObsFile")))
    
    return(ct_dataframe)
    
}


