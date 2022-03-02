#' Pull concentration-time data from multiple Simcyp Simulator output files NOT WORKING. DO NOT TRUST THIS.
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
#' @param sim_obs_dataframe the data.frame that will contain the output. Because
#'   we can see scenarios where you might want to extract some
#'   concentration-time data, play around with those data, and then later decide
#'   you want to pull more concentration-time data for comparisons, this
#'   data.frame can already exist. When that is the case, this function will
#'   \emph{add} data to that data.frame. It will \emph{not} overwrite existing
#'   data unless \code{overwrite} is set to TRUE.
#' @param overwrite TRUE or FALSE on whether to re-extract the
#'   concentration-time data from output files that are already included in
#'   \code{sim_obs_dataframe}. Since pulling data from Excel files is slow, by
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
#'   entry. Default is "ng/mL". NOTE: ADAM model data concentration units are
#'   not converted because there are simply too many units to manage easily, so
#'   please check that the units are what you expected in the end.
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
#'             sim_obs_dataframe = "ConcTimeData",
#'             overwrite = FALSE,
#'             tissue = "unbound plasma") # Note that "tissue" is passed to "extractConcTime".
#' 

extractConcTime_mult <- function(sim_data_files,
                                 obs_data_files = NA,
                                 sim_obs_dataframe = "ConcTime",
                                 overwrite = FALSE,
                                 tissues = "plasma",
                                 compoundsToExtract = "substrate",
                                 conc_units_to_use = "ng/mL",
                                 time_units_to_use = "hours",
                                 returnAggregateOrIndiv = "aggregate",
                                 ...){
    
    # Adding object to note that this is from mult function
    FromMultFunction <- TRUE
    
    # Checking on what combinations of data the user has requested and what
    # data are already present in sim_obs_dataframe.
    Requested <- expand.grid(Tissue = tissues,
                             CompoundID = compoundsToExtract,
                             File = sim_data_files)
    
    if(exists(substitute(sim_obs_dataframe)) && "data.frame" %in% class(sim_obs_dataframe)){
        if("File" %in% names(sim_obs_dataframe) == FALSE){
            sim_obs_dataframe$File <- "unknown file"
        }
        
        sim_obs_dataframe <- sim_obs_dataframe %>%
            mutate(ID = paste(File, Tissue, CompoundID))
        
        DataToFetch <- sim_obs_dataframe %>% select(File, Tissue, CompoundID) %>%
            unique() %>% mutate(ExistsAlready = TRUE) %>%
            right_join(Requested) %>%
            filter(is.na(ExistsAlready)) %>% select(-ExistsAlready) %>%
            mutate(ID = paste(File, Tissue, CompoundID))
        
        if(overwrite == FALSE){
            sim_data_files_topull <- unique(DataToFetch$File)
        } else {
            sim_data_files_topull <- sim_data_files
            sim_obs_dataframe <- sim_obs_dataframe %>%
                filter(!ID %in% DataToFetch$ID)
        }
    } else {
        DataToFetch <- Requested
        sim_data_files_topull <- sim_data_files
        sim_obs_dataframe <- data.frame()
    }
    
    MultData <- list()
    
    for(f in sim_data_files_topull){
        print(paste("file f =", f))
        print(paste("FromMultFunction =", FromMultFunction, "at line 140 of extractConcTime_mult"))
        MultData[[f]] <- list()
        
        # Getting summary data for the simulation(s)
        Deets <- extractExpDetails(f, exp_details = "Input Sheet")
        print(paste("eCTm: Deets should exist here, line 145. Deets$Substrate =",
                    Deets$Substrate))
        print(Deets$Inhibitor1)
        
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
            warning(paste0("For the file ", f, ", only the ",
                           str_comma(compoundsToExtract_n),
                           " was/were available."))
        }
        
        # Each tissue will be on its own sheet in the Excel file, so each
        # will need their own iterations of the loop for reading different
        # sheets.
        for(j in tissues){
            
            print(paste("tissue j =", j))
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
                    obs_inhibitor_data_file = NA,
                    compoundToExtract = compoundsToExtract_n,
                    tissue = j,
                    returnAggregateOrIndiv = returnAggregateOrIndiv)
                
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
                    
                    print(paste("CompoundTypes$Type k =", k))
                    compoundsToExtract_k <-
                        CompoundTypes %>% filter(Type == k) %>%
                        pull(PossCompounds)
                    
                    print(paste("FromMultFunction =", FromMultFunction, "at line 261 of extractConcTime_mult"))
                    
                    MultData[[f]][[j]][[k]] <-
                        extractConcTime(
                            sim_data_file = f,
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
        print(paste("nrow of MultData for file", f, "=", nrow(MultData[[f]])))
        
        # rm(Deets) # MUST remove Deets or you can get the wrong info for each file!!!
        
    }
    
    
    MultData <- bind_rows(MultData)
    if(nrow(MultData) > 0){
        print("nrow(MultData) > 0")
        MultData <- MultData %>% filter(Simulated == TRUE)
    }
    
    # Observed data ------------------------------------------------------
    if(length(obs_data_files) > 0 && any(complete.cases(obs_data_files))){
        MultObsData <- list()
        if(overwrite){
            sim_obs_dataframe <- sim_obs_dataframe %>% filter(!File %in% obs_data_files)
            for(f in obs_data_files){
                MultObsData[[f]] <- extractObsConcTime(f)
            }
        } else {
            for(f in setdiff(obs_data_files, unique(sim_obs_dataframe$File))){
                MultObsData[[f]] <- extractObsConcTime(f)
            }
        }
        sim_obs_dataframe <- bind_rows(sim_obs_dataframe, bind_rows(MultObsData))
    }
    
    # LEFT OFF HERE for obs data. I need a way to determine what the compound
    # is for each of the observed files and which obs data go with which sim
    # file. Really not sure how to do that.
    
    # all data together -------------------------------------------------
    sim_obs_dataframe <- bind_rows(sim_obs_dataframe, MultData) %>% select(-any_of("ID"))
    
    return(sim_obs_dataframe)
    
}


