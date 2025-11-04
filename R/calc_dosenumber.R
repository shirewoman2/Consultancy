#' Calculate the dose number for each time in a set of concentration-time data
#' given the experimental details for that simulation
#'
#' @description \code{calc_dosenumber} is mainly for internal use in the
#'   SimcypConsultancy package and will calculate which dose number a given time
#'   should be if you provide the ct_dataframe and the extracted simulation
#'   experimental details (the output from \code{\link{extractExpDetails}}).
#'
#' @param ct_dataframe a data.frame of concentration-time data from running
#'   either \code{\link{extractConcTime}} or \code{\link{extractConcTime_mult}}
#' @param existing_exp_details the output from running
#'   \code{\link{extractExpDetails}} or \code{\link{extractExpDetails_mult}} on
#'   the same simulations that were used for making \code{ct_dataframe}. Note:
#'   If you would like to have this calculate dose numbers on data where you do
#'   \emph{not} have simulation experimental details (example: observed data),
#'   then please supply a single-row data.frame with the following columns,
#'   using substrate dosing info as an example:
#'   \describe{
#'   
#'   \item{File}{(character) each Simulator Excel file name (check
#'   that the file names match perfectly!) or "all" if you want the same dosing
#'   regimen for all files} 
#'   
#'   \item{Dose_sub}{(numeric) the dose amount} 
#'   
#'   \item{DoseInt_sub}{(numeric) the dosing interval in
#'   hours} 
#'   
#'   \item{StartHr_sub}{(numeric) the start time for the substrate;
#'   probably 0} 
#'   
#'   \item{NumDoses_sub}{(numeric) the number of doses overall}} 
#'   
#'   If you want the dose number for other compound IDs,
#'   then replace "_sub" with, e.g., "_inhib". Please run
#'   \code{view(AllCompounds)} to see acceptable suffixes for each compound
#'   ID.
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
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
   
   suppressWarnings(
      existing_exp_details <- harmonize_details(existing_exp_details))
   
   if(is.null(existing_exp_details)){
      stop(wrapn("We cannot make sense of what you have provided for the argument 'existing_exp_details', so we don't know how to figure out which times had which dose numbers. Please check your input and try again."), 
           call. = FALSE)
   }
   
   # only keeping pertinent info
   existing_exp_details <- 
      filter_sims(which_object = existing_exp_details, 
                  which_sims = c(unique(ct_dataframe$File), "all"), 
                  include_or_omit = "include")
   
   Cmpd_in_CT <- AllCompounds %>% 
      filter(CompoundID %in% unique(ct_dataframe$CompoundID))
   
   
   ## Checking that user has provided all info needed ------------------------
   
   # Noting whether Dosing data.frame were initially present b/c that changes
   # what inforamtion we need. We're NOT going to overwrite it if not. We'll
   # only adjust units as needed in that case.
   DosingDFOrigPresent <- ncol(existing_exp_details$Dosing) > 0
   
   if(DosingDFOrigPresent == FALSE){
      
      # Some columns must be both present AND have values. Checking that. 
      ColsNeeded4Dosing <- expand_grid(
         Item1 = c("Dose", "DoseInt", "StartHr", "NumDoses"), 
         Suffix = unique(Cmpd_in_CT$DosedCompoundSuffix)) %>% 
         mutate(Item = paste0(Item1, Suffix)) %>% 
         pull(Item) %>% unique()
      
      ColsNeeded4Dosing_missingcol <-
         setdiff(ColsNeeded4Dosing, 
                 names(existing_exp_details$MainDetails))
      
      ColsNeeded4Dosing_NAvals <- 
         existing_exp_details$MainDetails %>%
         select(all_of(ColsNeeded4Dosing[
            # Don't need DoseInt_X to be complete here b/c could be NA if it's
            # single dose
            !str_detect(ColsNeeded4Dosing, "DoseInt")])) %>%
         summarize(across(.cols = everything(), .fns = \(x) any(is.na(x)))) %>% 
         pivot_longer(cols = everything(), 
                      names_to = "Cols", 
                      values_to = "AnyNA") %>% 
         filter(AnyNA == TRUE) %>% 
         pull(Cols)
      
      if(length(ColsNeeded4Dosing_missingcol) > 0 | 
         length(ColsNeeded4Dosing_NAvals) > 0){
         stop(paste0(wrapn("We do not have sufficient information in what you provided for 'existing_exp_details' to calculate the dose numbers for all the compounds present in your concentration-time data. Specifically, we are missing the following pieces of information:"), 
                     str_c(paste0("   ", c(ColsNeeded4Dosing_missingcol, 
                                           ColsNeeded4Dosing_NAvals)),
                           collapse = "\n")), 
              call. = FALSE)
      }
      
      # Need to have compound name to fill out dosenumber; just one more check
      # that things are lining up correctly.
      ColsNeeded4Compounds <- AllCompounds %>% 
         filter(CompoundID %in% unique(ct_dataframe$CompoundID)) %>% 
         pull(DetailNames)
      
      if(all(ColsNeeded4Compounds %in% 
             names(existing_exp_details$MainDetails)) == FALSE){
         
         # Compound names must match what's in ct_dataframe. 
         CompoundNames <- ct_dataframe %>% 
            select(File, CompoundID, Compound) %>% unique() %>% 
            left_join(AllCompounds %>% select(CompoundID, DetailNames), 
                      by = "CompoundID") %>% 
            select(-CompoundID) %>% 
            pivot_wider(names_from = DetailNames, 
                        values_from = Compound)
         
         existing_exp_details$MainDetails <- existing_exp_details$MainDetails %>% 
            left_join(CompoundNames, by = "File") 
         
      }
      
      # If user supplied just the minimum amount of info required to get this to
      # work, we'll still be missing a few pieces of information for
      # harmonize_details to have accurately created a "Dosing" data.frame.
      # Adding that information here as needed and then harmonizing dosing to
      # create "Dosing" dataframe, which we'll need lower down. We need to know
      # the time units, and harmonizing dosing will work best if the time units
      # there match the time units in the conc-time data, regardless of what was
      # originally in the simulation. We will also be replacing the time units
      # in the Dosing data.frame, so this will be ok to only set for MainDetails
      # here. Setting time_units accordingly.
      existing_exp_details$MainDetails$Units_tmax <-
         unique(ct_dataframe$Time_units)
      
      ColsNeeded4Harmonization <- expand_grid(
         Item1 = c("DoseRoute", "Units_dose", "Dose"), 
         Suffix = unique(Cmpd_in_CT$DosedCompoundSuffix)) %>% 
         mutate(Item = paste0(Item1, Suffix)) %>% 
         pull(Item) %>% unique()
      
      MissingInfo <- setdiff(
         c("File", "SimStartDayTime", ColsNeeded4Harmonization), 
         names(existing_exp_details$MainDetails))
      
      if(length(MissingInfo) > 0){
         
         # ONLY adding cols that are ok to hack for this purpose. Note that this
         # will replace whatever existed for the Dosing data.frame with whatever
         # it creates here. That's fine since we're not returning
         # existing_exp_details. It could be that the Dosing data.frame doesn't
         # contain ALL the info needed for ALL the compounds present in the
         # conc-time data, so this will address that.
         existing_exp_details$MainDetails <- existing_exp_details$MainDetails %>% 
            cbind(
               as_tibble(matrix(data = "placeholder", 
                                ncol = length(MissingInfo), 
                                dimnames = list(NULL, MissingInfo))))
      }
      
      # There are a few parameters where we need to have complete cases
      # (although the exact value doesn't matter) where we might have NA if the
      # user provided a DF for existing_exp_details. Adding those.
      existing_exp_details$MainDetails <- existing_exp_details$MainDetails %>% 
         mutate(across(.cols = matches("DoseRoute"), 
                       .fns = \(x) "placeholder"))
      
      existing_exp_details <- 
         harmonize_dosing(existing_exp_details = existing_exp_details)
      
   } else {
      # FIXME: Do we need to adjust time units? ?
   }
   
   Deets <- existing_exp_details$MainDetails
   
   if(all(Deets$File == "all", na.rm = T)){
      
      # Since the "if" statement can be T when "File" is not present in the
      # data.frame, making SURE that it is "all" here.
      Deets$File <- "all"
      
      if(nrow(Deets) > 1){
         stop(wrapn("The file names in what you have supplied for the argument 'existing_exp_details' are all set to the same value or are missing, and there appears to be information for more than one simulation file. We don't know which files should have which dosing regimen, so we cannot assign any dose numbers."), 
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
      warning(paste0(wrapn("The following files are not present in what you supplied for 'existing_exp_details', so we can't determine what the dose numbers were and will ignore these data: "), 
                     str_c(FileCheck, collapse = "\n")), 
              call. = FALSE)
   }
   
   # Main body of function -------------------------------------------------
   
   AllDosedCompounds <- AllCompounds %>% 
      filter(!CompoundID %in% c("endogenous"))
   
   DosedCompoundIDs <- AllDosedCompounds$DosedCompoundID
   names(DosedCompoundIDs) <- AllDosedCompounds$CompoundID
   
   DosedCompoundSuffixes <- AllCompounds$DosedCompoundSuffix
   names(DosedCompoundSuffixes) <- AllCompounds$CompoundID
   
   # Adding some NA values to Deets as needed for the next bit to work w/out
   # generating a ton of warnings.
   MissingCols <- setdiff(
      paste0(rep(c("DoseInt", "StartHr", "Regimen", 
                   "NumDoses", "BolusDose", 
                   "InfusionDose"), 
                 each = nrow(AllDosedCompounds)), 
             AllDosedCompounds$DosedCompoundSuffix), 
      
      names(Deets)) %>% unique()
   
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
   
   Dosing <- existing_exp_details$Dosing
   Dosing <- split(Dosing, 
                   f = Dosing$File)
   
   ct_dataframe <- split(ct_dataframe, 
                         f = ct_dataframe$File)
   
   for(i in intersect(names(Deets), 
                      names(ct_dataframe))){
      
      ## Getting dose regimen info ------------------------------------------
      
      # Need match the conc-time compound ID with the compound that was DOSED
      # for this to work for metabolites.
      
      ct_dataframe[[i]] <- ct_dataframe[[i]] %>% 
         mutate(DosedCompoundID = DosedCompoundIDs[CompoundID], 
                DosedCompoundID = case_when(
                   is.na(DosedCompoundID) ~ "UNKNOWN", 
                   .default = DosedCompoundID))
      
      ct_dataframe[[i]] <- split(ct_dataframe[[i]],
                                 f = ct_dataframe[[i]]$DosedCompoundID)
      
      Dosing[[i]] <- split(Dosing[[i]], 
                           list(Dosing[[i]]$CompoundID))
      
      for(j in names(ct_dataframe[[i]])){
         
         if(is.null(Dosing[[i]][[j]])){ next }
         
         Dosing[[i]][[j]] <- Dosing[[i]][[j]] %>% 
            # Need this next bit for using cut function appropriately
            bind_rows(
               data.frame(
                  Time = max(ct_dataframe[[i]][[j]]$Time) + 1, 
                  DoseNum = max(Dosing[[i]][[j]]$DoseNum)))
         
         if(0 %in% Dosing[[i]][[j]]$Time == FALSE){
            Dosing[[i]][[j]] <- Dosing[[i]][[j]] %>% 
               bind_rows(data.frame(Time = 0, DoseNum = 0))
         }
         
         Dosing[[i]][[j]] <- Dosing[[i]][[j]] %>% 
            fill(any_of(c("File", "CompoundID", "Compound", "Time_units", 
                          "Dose_units")),
                 .direction = "down") %>% 
            arrange(File, Time) %>% unique()
         
         # If there was a loading dose or something (not really sure what this
         # would be), then there are two dose numbers listed for t0. Removing
         # the earlier one so that this will work.
         if(any(duplicated(Dosing[[i]][[j]]$Time))){
            warning(wrapn(paste0(
               "There were multiple dose numbers listed at the same time for the ",
               j," in the file ", i, 
               "; did you mean for that to be the case? For now, the dose number at that duplicated time will be set to the 2nd dose number listed.")),
               call. = FALSE)
            TimeToRemove <- which(duplicated(
               Dosing[[i]][[j]]$Time, fromLast = TRUE))
            Dosing[[i]][[j]] <- Dosing[[i]][[j]] %>% slice(-TimeToRemove)
         }
         
         Dosing[[i]][[j]]$Breaks <-
            as.character(cut(Dosing[[i]][[j]]$Time,
                             breaks = Dosing[[i]][[j]]$Time,
                             right = FALSE))
         
         ct_dataframe[[i]][[j]]$DoseNum <- NULL
         ct_dataframe[[i]][[j]]$Breaks <-
            as.character(cut(ct_dataframe[[i]][[j]]$Time,
                             breaks = Dosing[[i]][[j]]$Time,
                             right = FALSE))
         
         ct_dataframe[[i]][[j]] <- ct_dataframe[[i]][[j]] %>% 
            select(-any_of(c("Dose_sub", "Dose_inhib", "Dose_inhib2"))) %>% 
            left_join(Dosing[[i]][[j]] %>% 
                         select(-Compound) %>% 
                         filter(complete.cases(Breaks)) %>% 
                         mutate(CompoundID = paste0("Dose", DosedCompoundSuffixes[CompoundID])) %>% 
                         pivot_wider(names_from = CompoundID, 
                                     values_from = Dose) %>% 
                         select(Breaks, DoseNum, Dose_units,
                                any_of(c("Dose_sub", "Dose_inhib",
                                         "Dose_inhib2"))), 
                      by = "Breaks")
      }
      
      ct_dataframe[[i]] <- bind_rows(ct_dataframe[[i]])
      
   }
   
   ct_dataframe <- bind_rows(ct_dataframe)
   
   return(ct_dataframe)
   
}


