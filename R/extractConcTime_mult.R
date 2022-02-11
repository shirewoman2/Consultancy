#' Pull concentration-time data from multiple Simcyp Simulator output files
#'
#' \code{extractConcTime_mult} is meant to be used in conjunction with
#' \code{\link{ct_plot_overlay}} to create single graphs with overlaid
#' concentration-time data from multiple Simcyp Simulator output files for easy
#' comparisons. \strong{A couple of notes:}\enumerate{\item{If ANY of the Excel
#' files you wish to extract data from are open, this WILL CRASH and WILL NOT
#' save whatever progress it has made so far. BE SURE TO CLOSE ALL OF THE
#' SOURCE-DATA EXCEL FILES.} \item{If you list multiple files, multiple tissues,
#' and/or multiple compounds to extract (see options below), this will extract
#' \emph{all} possible variations of them. For example, if you ask for "File A"
#' and "File B" and then also ask for "substrate" and "primary metabolite 1",
#' you will get the substrate and primary metabolite 1 data from \emph{both}
#' files.}}
#'
#' @param sim_data_files a character vector of the files you'd like to compare,
#'   e.g., \code{c("MyFile1.xlsx", "MyFile2.xlsx")}. The path should be included
#'   with the file names if they are located somewhere other than your working
#'   directory.
#' @param obs_data_files UNDER CONSTRUCTION and not yet working. a character
#'   vector of the observed data files that you'd like to compare, e.g.,
#'   \code{c("MyObsFile1.xlsx", "MyObsFile2.xlsx")}. The path should be included
#'   with the file names if they are located somewhere other than your working
#'   directory. This is the file that it is ready to be converted to an XML
#'   file, not the file that contains only the digitized time and concentration
#'   data. The names of the observed data files are piped into
#'   \code{\link{extractObsConcTime}}.
#' @param conctime_DF the data.frame that will contain the output. Because we
#'   can see scenarios where you might want to extract some concentration-time
#'   data, play around with those data, and then later decide you want to pull
#'   more concentration-time data for comparisons, this data.frame can already
#'   exist. When that is the case, this function will \emph{add} data to that
#'   data.frame. It will \emph{not} overwrite existing data unless
#'   \code{overwrite} is set to TRUE.
#' @param overwrite TRUE or FALSE on whether to re-extract the
#'   concentration-time data from output files that are already included in
#'   \code{conctime_DF}. Since pulling data from Excel files is slow, by
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
#'   "skin", or "spleen". Not case sensitive. List all tissues desired as a
#'   character vector, e.g., \code{c("plasma", "blood", "liver")}.
#' @param compoundsToExtract For which compound(s) do you want to extract
#'   concentration-time data? Options are "substrate" (default), "primary
#'   metabolite 1", "primary metabolite 2", "secondary metabolite", "inhibitor
#'   1" (this can be an inducer, inhibitor, activator, or suppresesor, but it's
#'   labeled as "Inhibitor 1" in the simulator), "inhibitor 2" for the 2nd
#'   inhibitor listed in the simulation, or "inhibitor 1 metabolite" for the
#'   primary metabolite of inhibitor 1. List all desired compounds as a
#'   character vector, e.g., \code{c("substrate", "primary metabolite 1")}.
#'   \emph{Note:} The simulator will report up to one metabolite for the 1st
#'   inhibitor but no other inhibitor metabolites. (Someone please correct me if
#'   that's wrong! -LS)
#' @param ... other arguments passed to the function
#'   \code{\link{extractConcTime}}
#' @param conc_units_to_use concentration units to use so that all data will be
#'   comparable. Options are the same as the ones in the Excel form for PE data
#'   entry. Default is "ng/mL".
#' @param time_units_to_use time units to use so that all data will be
#'   comparable. Options are "hours" or "minutes". Default is "hours".
#' @param returnAggregateOrIndiv
#'
#' @return a large data.frame with multiple sets of concentration-time data,
#'   formatted the same way as output from the function
#'   \code{\link{extractConcTime}}
#' @export
#'
#' @examples
#' ConcTimeData <-
#'       extractConcTime_mult(
#'             sim_data_files = c("MyFile1.xlsx", "MyFile2.xlsx"),
#'             conctime_DF = "ConcTimeData",
#'             overwrite = FALSE,
#'             tissue = "unbound plasma") # Note that "tissue" is passed to "extractConcTime".
#' 

extractConcTime_mult <- function(sim_data_files,
                                 obs_data_files = NA,
                                 conctime_DF,
                                 overwrite = FALSE,
                                 tissues = "plasma",
                                 compoundsToExtract = "substrate",
                                 conc_units_to_use = "ng/mL",
                                 time_units_to_use = "hours",
                                 returnAggregateOrIndiv = "aggregate",
                                 ...){
    
    # Checking on what combinations of data the user has requested and what
    # data are already present in conctime_DF.
    Requested <- expand.grid(Tissue = tissues,
                             CompoundID = compoundsToExtract,
                             File = sim_data_files)
    
    if(exists(substitute(conctime_DF))){
        if("File" %in% names(conctime_DF) == FALSE){
            conctime_DF$File <- "unknown file"
        }
        
        conctime_DF <- conctime_DF %>%
            mutate(ID = paste(File, Tissue, CompoundID))
        
        DataToFetch <- conctime_DF %>% select(File, Tissue, CompoundID) %>%
            unique() %>% mutate(ExistsAlready = TRUE) %>%
            right_join(Requested) %>%
            filter(is.na(ExistsAlready)) %>% select(-ExistsAlready) %>%
            mutate(ID = paste(File, Tissue, CompoundID))
        
        if(overwrite == FALSE){
            sim_data_files_topull <- unique(DataToFetch$File)
        } else {
            sim_data_files_topull <- sim_data_files
            conctime_DF <- conctime_DF %>%
                filter(!ID %in% DataToFetch$ID)
        }
    } else {
        DataToFetch <- Requested
        sim_data_files_topull <- sim_data_files
        conctime_DF <- data.frame()
    }
    
    MultData <- list()
    
    for(n in sim_data_files_topull){
        
        MultData[[n]] <- list()
        
        # Getting summary data for the simulation(s)
        Deets <- extractExpDetails(n, exp_details = "Input Sheet")
        
        # Names of compounds requested for checking whether the data exist
        CompoundCheck <- c("substrate" = Deets$Substrate,
                           "inhibitor 1" = Deets$Inhibitor1,
                           "inhibitor 1 metabolite" = Deets$Inhibitor1Metabolite,
                           "inhibitor 2" = Deets$Inhibitor2,
                           "primary metabolite 1" = Deets$PrimaryMetabolite1,
                           "primary metabolite 2" = Deets$PrimaryMetabolite2,
                           "secondary metabolite" = Deets$SecondaryMetabolite)
        
        # If the requested compound is not present in the Excel file, remove
        # it from consideration.
        compoundsToExtract_n <- intersect(compoundsToExtract,
                                          names(CompoundCheck)[complete.cases(CompoundCheck)])
        
        if(all(compoundsToExtract %in% compoundsToExtract_n) == FALSE){
            warning(paste0("For the file ", n, ", only the ",
                           str_comma(compoundsToExtract_n),
                           " was/were available."))
        }
        
        # Each tissue will be on its own sheet in the Excel file, so each
        # will need their own iterations of the loop for reading different
        # sheets.
        for(j in tissues){
            
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
                
                MultData[[n]][[j]] <- extractConcTime(
                    sim_data_file = n,
                    obs_data_file = NA,
                    obs_inhibitor_data_file = NA,
                    compoundToExtract = compoundsToExtract_n,
                    tissue = j,
                    returnAggregateOrIndiv = returnAggregateOrIndiv)
                
                # When the particular combination of compound and
                # tissue is not available in that file,
                # extractConcTime will return an empty data.frame,
                # which we don't want to be included in the final
                # data. Not adding info for File in that scenario
                # b/c it would add a row to what would have been
                # an empty data.frame.
                if(nrow(MultData[[n]][[j]]) > 0){
                    MultData[[n]][[j]] <-
                        MultData[[n]][[j]] %>%
                        mutate(File = n)
                    
                    MultData[[n]][[j]] <-
                        match_units(DF_to_adjust = MultData[[n]][[j]],
                                    goodunits = list("Conc_units" = conc_units_to_use,
                                                     "Time_units" = time_units_to_use))
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
                
                MultData[[n]][[j]] <- list()
                
                for(k in unique(CompoundTypes$Type)){
                    
                    compoundsToExtract_k <-
                        CompoundTypes %>% filter(Type == k) %>%
                        pull(PossCompounds)
                    
                    MultData[[n]][[j]][[k]] <-
                        extractConcTime(
                            sim_data_file = n,
                            obs_data_file = NA,
                            obs_inhibitor_data_file = NA,
                            compoundToExtract = compoundsToExtract_k,
                            tissue = j,
                            returnAggregateOrIndiv = returnAggregateOrIndiv)
                    
                    # When the particular combination of compound and
                    # tissue is not available in that file,
                    # extractConcTime will return an empty data.frame,
                    # which we don't want to be included in the final
                    # data. Not adding info for File in that scenario
                    # b/c it would add a row to what would have been
                    # an empty data.frame.
                    if(nrow(MultData[[n]][[j]][[k]]) > 0){
                        MultData[[n]][[j]][[k]] <-
                            MultData[[n]][[j]][[k]] %>%
                            mutate(File = n)
                        
                        MultData[[n]][[j]][[k]] <-
                            match_units(DF_to_adjust = MultData[[n]][[j]][[k]],
                                        goodunits = list("Conc_units" = conc_units_to_use,
                                                         "Time_units" = time_units_to_use))
                    }
                    rm(compoundsToExtract_k)
                }
                
                MultData[[n]][[j]] <- bind_rows(MultData[[n]][[j]])
            }
        }
        
        # rm(Deets) # I had removed Deets here for safety just to make sure that
        # it didn't get applied to the wrong file, but it was giving me an error
        # when I had removed it here, and, now that it's commented out, it's
        # working. Really check that this is applying the correct details to the
        # correct files. I don't see why it's a problem to remove Deets here.
        MultData[[n]] <- bind_rows(MultData[[n]])
    }
    
    MultData <- bind_rows(MultData) %>% filter(Simulated == TRUE)
    
    # Observed data ------------------------------------------------------
    if(length(obs_data_files) > 0 && any(complete.cases(obs_data_files))){
        MultObsData <- list()
        if(overwrite){
            conctime_DF <- conctime_DF %>% filter(!File %in% obs_data_files)
            for(n in obs_data_files){
                MultObsData[[n]] <- extractObsConcTime(n)
            }
        } else {
            for(n in setdiff(obs_data_files, unique(conctime_DF$File))){
                MultObsData[[n]] <- extractObsConcTime(n)
            }
        }
        conctime_DF <- bind_rows(conctime_DF, bind_rows(MultObsData))
    }
    
    # LEFT OFF HERE for obs data. I need a way to determine what the compound
    # is for each of the observed files and which obs data go with which sim
    # file. Really not sure how to do that.
    
    # all data together -------------------------------------------------
    conctime_DF <- bind_rows(conctime_DF, MultData) %>% select(-any_of("ID"))
    
    return(conctime_DF)
    
}



