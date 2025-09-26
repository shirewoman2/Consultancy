#' Extract enzyme abundance data from a simulator output Excel file
#'
#' Extracts enzyme abundance data from a simulator output Excel file. The
#' appropriate tab must be present in the output file. For detailed instructions
#' and examples, please see the SharePoint file "Simcyp PBPKConsult R Files -
#' Simcyp PBPKConsult R Files/SimcypConsultancy function examples and
#' instructions/Enzyme abundance plots/Enzyme-abundance-plot-examples.docx".
#' (Sorry, we are unable to include a link to it here.)
#'
#' \strong{Note:} Unlike the similar function for extracting drug
#' concentrations, \code{\link{extractConcTime}}, This has not been set up yet
#' to get the dose number for a custom-dosing regimen.
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   enzyme-abundance-time data, in quotes
#' @param enzyme the enzyme of interest, e.g., "CYP3A4" (default), "UGT1A1",
#'   etc. Spaces or hyphens in enzyme names will be ignored. Not case sensitive.
#' @param tissue From which tissue should the desired enzyme abundance be
#'   extracted? Options are "liver" (default), "gut", "ADAM gut", or "kidney".
#'   Note: If "gut" is selected, the output will return both colon and small
#'   intestine concentrations but not ADAM-model gut-segmental enzyme levels.
#'   Using "ADAM gut" will get individual gut segment levels in the stomach
#'   through the colon.
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated
#'   enzyme abundance data? Options are "individual", "aggregate", or "both"
#'   (default). Aggregated data are not calculated here but are pulled from the
#'   simulator output rows labeled as "mean".
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#'
#' @return A data.frame of enzyme abundance with time with the following
#'   columns: \describe{
#'
#'   \item{Enzyme}{the enzyme whose abundance is listed}
#'
#'   \item{Tissue}{the tissue}
#'
#'   \item{Substrate}{what the substrate was in the simulation}
#'
#'   \item{Inhibitor}{what the perpetrator was in the simulation if there was one}
#'
#'   \item{Individual}{the individual for the given profile, which will be a
#'   number for a simulated individual or will be "obs" or "obs+inhibitor" for
#'   observed data, "mean" for the mean data, "geomean" for the geometric mean
#'   data, or "per5" or "per95" for the 5th and 95th percentile data.}
#'
#'   \item{Trial}{the trial number for that set of simulations or "obs", "mean",
#'   etc. for the observed or aggregate data}
#'
#'   \item{Time}{the time since the first dose}
#'
#'   \item{Abundance}{abundance of the enzyme listed}
#'
#'   \item{Time_units}{units used for time}
#'
#'   \item{Dose_num_x}{the dose number for the substrate (suffix is "_sub"),
#'   inhibitor 1 ("_inhib1"), or inhibitor 2 ("_inhib2")}
#'
#'   \item{Dose_int_x}{the dosing interval for that compound. This will be NA
#'   for custom-dosing regimens.}
#'
#'   \item{TimeSinceDose1_x}{time since dose 1 for that compound}
#'
#'   }
#'
#'
#' @export
#' @examples
#' extractEnzAbund(sim_data_file = "../Example simulator output MD.xlsx",
#'                 enzyme = "CYP3A4", tissue = "liver")
#'
#'                 
extractEnzAbund <- function(sim_data_file,
                            enzyme = "CYP3A4",
                            tissue = "liver",
                            returnAggregateOrIndiv = "both", 
                            existing_exp_details = NA){
   
   # Error catching --------------------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   sim_data_file <- ifelse(str_detect(sim_data_file, "xlsx$"), 
                           sim_data_file, paste0(sim_data_file, ".xlsx"))
   
   # Checking for file name issues
   CheckFileNames <- check_file_name(sim_data_file)
   BadFileNames <- CheckFileNames[!CheckFileNames == "File name meets naming standards."]
   if(length(BadFileNames)> 0){
      BadFileNames <- paste0(names(BadFileNames), ": ", BadFileNames)
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"), "\n"),
              call. = FALSE)
   }
   
   if(any(c(length(returnAggregateOrIndiv) < 1,
            length(returnAggregateOrIndiv) > 2,
            any(unique(returnAggregateOrIndiv) %in% c("aggregate", "individual", "both") == FALSE)))) {
      stop("returnAggregateOrIndiv must be 'aggregate', 'individual', or 'both'.",
           call. = FALSE)
   }
   
   if(length(tissue) != 1){
      warning(wrapn("You must enter one and only one tissue option. (Default is liver.)"),
              call. = FALSE)
      return(data.frame())
   }
   
   tissue <- tolower(tissue)
   if(tissue %in% c("gut", "liver", "kidney", "adam gut") == FALSE){
      warning(wrapn("The tissue you entered is not one of the options. Please select one of 'gut', 'ADAM gut', 'liver', or 'kidney' for the tissue."),
              call. = FALSE)
      return(data.frame())
   }
   
   enzyme <- gsub(" |_|-", "", toupper(enzyme))
   
   # Checking that what they're asking for is possible
   LiverEnz <- c(paste0("CYP", c("1A1", "1A2", "2A6", "2B6", "2C8", "2C9", "2C18", 
                                 "2C19", "2D6", "2E1", "2J2", "3A4", "3A5", "3A7")), 
                 paste0("UGT", c(paste0("1A", c(1,3:10)), 
                                 paste0("2B", c(4, 7, 10, 11, 15, 17, 28)), 
                                 " User Defined")))
   
   GutEnz <- c(paste0("CYP", c("2C9", "2C19", "2D6", "2J2", "3A4", "3A5")), 
               paste0("UGT", c(paste0("1A", c(1,3:10)), 
                               paste0("2B", c(4, 7, 10, 11, 15, 17, 28)), 
                               " User Defined")))
   
   KidneyEnz <- paste0("UGT", c(paste0("1A", c(1,3:10)), 
                                paste0("2B", c(4, 7, 10, 11, 15, 17, 28)), 
                                " User Defined"))
   
   if(tissue == "liver" & enzyme %in% LiverEnz == FALSE){
      warning(wrapn(paste0("You requested ", enzyme, " levels in the liver, which is not among the possible outputs from the Simcyp Simulator, so we cannot return any data.")), 
              call. = FALSE)
      return(data.frame())
   }
   
   if(tissue %in% c("gut", "adam gut") & enzyme %in% GutEnz == FALSE){
      warning(wrapn(paste0("You requested ", enzyme, " levels in the gut, which is not among the possible outputs from the Simcyp Simulator, so we cannot return any data.")), 
              call. = FALSE)
      return(data.frame())
   }
   
   if(tissue == "kidney" & enzyme %in% KidneyEnz == FALSE){
      warning(wrapn(paste0("You requested ", enzyme, " levels in the kidney, which is not among the possible outputs from the Simcyp Simulator, so we cannot return any data.")), 
              call. = FALSE)
      return(data.frame())
   }
   
   
   # Main body of function ----------------------------------------------------------------
   
   # Getting summary data for the simulation(s)
   if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA
      Deets <- extractExpDetails(sim_data_file, exp_details = "Summary and Input")[["MainDetails"]]
   } else {
      
      Deets <- filter_sims(existing_exp_details, sim_data_file, "include")
      Deets <- harmonize_details(Deets)[["MainDetails"]] %>% 
         filter(File == sim_data_file)
      
      if(nrow(Deets) == 0){
         Deets <- extractExpDetails(sim_data_file, exp_details = "Summary and Input")[["MainDetails"]]
      }
   }
   
   if(is.null(Deets) || nrow(Deets) == 0){
      # Using "warning" instead of "stop" here b/c I want this to be able to
      # pass through to other functions and just skip any files that
      # aren't simulator output.
      warning(wrapn(paste0("The file '", sim_data_file,
                           "' does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file.")), 
              call. = FALSE)
      return()
   }
   
   
   if(Deets$PopRepSim == "Yes"){
      warning(paste0("The simulator file supplied, `", 
                     sim_data_file, 
                     "`, is for a population-representative simulation and thus doesn't have any aggregate data. Please be warned that some plotting functions will not work well without aggregate data.\n"),
              call. = FALSE)
   }
   
   # Check whether MainDetails includes SheetNames and adding if not
   if(("SheetNames" %in% names(Deets) &&
       any(is.na(Deets$SheetNames)) |
       any(Deets$SheetNames == "`NA`", na.rm = T)) |
      "SheetNames" %in% names(Deets) == FALSE){
      
      for(i in Deets$File){
         if(file.exists(i) &
            !str_detect(i, "\\.db$|\\.wksz")){
            SheetNames <- tryCatch(readxl::excel_sheets(i),
                                   error = openxlsx::getSheetNames(i))
         } else { SheetNames <- NA}
         
         Deets$SheetNames[
            Deets$File == i] <-
            str_c(paste0("`", SheetNames, "`"), collapse = " ")
         rm(SheetNames)
      }
   }
   
   # Figuring out which sheet to extract and dealing with case since that
   # apparently changes between Simulator versions.
   AllSheets <- gsub("`", "", str_split_1(Deets$SheetNames, pattern = "` `"))
   SheetToExtract <- data.frame(Sheet = AllSheets, 
                                SheetLower = tolower(AllSheets)) %>% 
      filter(SheetLower == paste(tolower(enzyme), 
                                 switch(tissue,
                                        "liver" = "(liver)",
                                        "gut" = "(gut)",
                                        "adam gut" = "(gut segmental levels)", 
                                        "kidney" = "(kidney)"))) %>% 
      pull(Sheet)
   
   if(length(SheetToExtract) == 0){
      warning(wrapn(paste0("The simulator output file `", 
                           sim_data_file, "` does not appear to have the sheet we need for the enzyme abundances requested. We were looking for a sheet titled `",
                           paste(toupper(enzyme), switch(tissue,
                                                         "liver" = "(liver)",
                                                         "gut" = "(gut)",
                                                         "adam gut" = "(Gut segmental levels)", 
                                                         "kidney" = "(kidney)")), 
                           "` and could not find it, so these data cannot be returned.")), 
              call. = FALSE)
      return(data.frame())
   }
   
   # Reading in simulated abundance-time profile data
   sim_data_xl <- suppressMessages(
      readxl::read_excel(path = sim_data_file,
                         sheet = SheetToExtract,
                         col_names = FALSE))
   
   # If the tissue was gut, there are separate data sets for small
   # intestine and colon or for gut and colon depending on simulator
   # version or maybe on whether it was an ADAM model (still figuring out
   # what causes "SI" to be replaced with "Gut" in output). Checking for
   # that.
   GutParts <- c(
      "colon", "small intestine", "stomach", 
      "duodenum", "jejunum I", "jejunum II", 
      "ileum I", "ileum II", "ileum III", 
      "ileum IV", "colon")[
         c(
            any(str_detect(tolower(sim_data_xl$...1), "\\(colon\\)"), na.rm = T),
            any(str_detect(tolower(sim_data_xl$...1), "\\(si\\)|\\(gut\\)"), na.rm = T), 
            any(str_detect(tolower(sim_data_xl$...1), "\\(stomach\\)"), na.rm = T), 
            any(str_detect(tolower(sim_data_xl$...1), "\\(duodenum\\)"), na.rm = T),
            any(str_detect(tolower(sim_data_xl$...1), "\\(jejunum i\\)"), na.rm = T),
            any(str_detect(tolower(sim_data_xl$...1), "\\(ileum i\\)"), na.rm = T),
            any(str_detect(tolower(sim_data_xl$...1), "\\(ileum ii\\)"), na.rm = T),
            any(str_detect(tolower(sim_data_xl$...1), "\\(ileum iii\\)"), na.rm = T),
            any(str_detect(tolower(sim_data_xl$...1), "\\(ileum iv\\)"), na.rm = T),
            any(str_detect(tolower(sim_data_xl$...1), "\\(colon\\)"), na.rm = T))
      ]
   
   # Extracting aggregate data ---------------------------------------------
   if(any(c("aggregate", "both") %in% returnAggregateOrIndiv)){
      
      if(length(GutParts) > 1 & tissue %in% c("gut", "adam gut")){
         
         sim_data_mean <- list()
         
         # mean data
         StartRow_agg <- which(sim_data_xl$...1 == "Population Statistics")
         StartRow_indiv <- which(sim_data_xl$...1 == "Individual Statistics")
         TimeRows <- which(str_detect(sim_data_xl$...1, "^Time "))
         TimeRows <- TimeRows[TimeRows > StartRow_agg & 
                                 TimeRows < StartRow_indiv]
         
         # Figuring out which rows contain which data
         FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                 which(1:nrow(sim_data_xl) > TimeRows[length(TimeRows)]))[1]
         FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
         NamesToCheck <- tolower(sim_data_xl$...1[TimeRows[1]:(FirstBlank-1)])
         
         # Need to note which rows are for which gut part.
         GutRows <- list()
         
         SIrows <- which(str_detect(NamesToCheck, "\\(si\\)|\\(gut\\)")) +
            TimeRows[1]-1
         SITimeRow <- TimeRows[which((TimeRows + 1) %in% SIrows)]
         # Looking for the next blank row after SITimeRow
         SIEndRow <- which(is.na(sim_data_xl$...1))
         SIEndRow <- SIEndRow[SIEndRow > SITimeRow][1] - 1
         if(length(SITimeRow) > 0){
            GutRows[["small intestine"]] <- SITimeRow:SIEndRow
         }
         
         # NB: This regex should work for both ADAM-model and also
         # non-ADAM-model colon abundances
         Colonrows <- which(str_detect(NamesToCheck, "\\(colon\\)")) +
            TimeRows[1]-1
         ColonTimeRow <- TimeRows[which((TimeRows + 1) %in% Colonrows)]
         # Looking for the next blank row after ColonTimeRow
         ColonEndRow <- which(is.na(sim_data_xl$...1))
         ColonEndRow <- ColonEndRow[ColonEndRow > ColonTimeRow][1] - 1
         if(length(ColonTimeRow) > 0){
            GutRows[["colon"]] <- ColonTimeRow:ColonEndRow
         }
         
         Stomachrows <- which(str_detect(NamesToCheck, "\\(stomach\\)")) +
            TimeRows[1]-1
         StomachTimeRow <- TimeRows[which((TimeRows + 1) %in% Stomachrows)]
         # Looking for the next blank row after StomachTimeRow
         StomachEndRow <- which(is.na(sim_data_xl$...1))
         StomachEndRow <- StomachEndRow[StomachEndRow > StomachTimeRow][1] - 1
         if(length(StomachTimeRow) > 0){
            GutRows[["stomach"]] <- StomachTimeRow:StomachEndRow
         }
         
         Duodenumrows <- which(str_detect(NamesToCheck, "\\(duodenum\\)")) +
            TimeRows[1]-1
         DuodenumTimeRow <- TimeRows[which((TimeRows + 1) %in% Duodenumrows)]
         # Looking for the next blank row after DuodenumTimeRow
         DuodenumEndRow <- which(is.na(sim_data_xl$...1))
         DuodenumEndRow <- DuodenumEndRow[DuodenumEndRow > DuodenumTimeRow][1] - 1
         if(length(DuodenumTimeRow) > 0){
            GutRows[["duodenum"]] <- DuodenumTimeRow:DuodenumEndRow
         }
         
         JejunumIrows <- which(str_detect(NamesToCheck, "\\(jejunum i\\)")) +
            TimeRows[1]-1
         JejunumITimeRow <- TimeRows[which((TimeRows + 1) %in% JejunumIrows)]
         # Looking for the next blank row after JejunumITimeRow
         JejunumIEndRow <- which(is.na(sim_data_xl$...1))
         JejunumIEndRow <- JejunumIEndRow[JejunumIEndRow > JejunumITimeRow][1] - 1
         if(length(JejunumITimeRow) > 0){
            GutRows[["jejunum I"]] <- JejunumITimeRow:JejunumIEndRow
         }
         
         JejunumIIrows <- which(str_detect(NamesToCheck, "\\(jejunum ii\\)")) +
            TimeRows[1]-1
         JejunumIITimeRow <- TimeRows[which((TimeRows + 1) %in% JejunumIIrows)]
         # Looking for the next blank row after JejunumIITimeRow
         JejunumIIEndRow <- which(is.na(sim_data_xl$...1))
         JejunumIIEndRow <- JejunumIIEndRow[JejunumIIEndRow > JejunumIITimeRow][1] - 1
         if(length(JejunumIITimeRow) > 0){
            GutRows[["jejunum II"]] <- JejunumIITimeRow:JejunumIIEndRow
         }
         
         IleumIrows <- which(str_detect(NamesToCheck, "\\(ileum i\\)")) +
            TimeRows[1]-1
         IleumITimeRow <- TimeRows[which((TimeRows + 1) %in% IleumIrows)]
         # Looking for the next blank row after IleumITimeRow
         IleumIEndRow <- which(is.na(sim_data_xl$...1))
         IleumIEndRow <- IleumIEndRow[IleumIEndRow > IleumITimeRow][1] - 1
         if(length(IleumITimeRow) > 0){
            GutRows[["ileum I"]] <- IleumITimeRow:IleumIEndRow
         }
         
         IleumIIrows <- which(str_detect(NamesToCheck, "\\(ileum ii\\)")) +
            TimeRows[1]-1
         IleumIITimeRow <- TimeRows[which((TimeRows + 1) %in% IleumIIrows)]
         # Looking for the next blank row after IleumIITimeRow
         IleumIIEndRow <- which(is.na(sim_data_xl$...1))
         IleumIIEndRow <- IleumIIEndRow[IleumIIEndRow > IleumIITimeRow][1] - 1
         if(length(IleumIITimeRow) > 0){
            GutRows[["ileum II"]] <- IleumIITimeRow:IleumIIEndRow
         }
         
         IleumIIIrows <- which(str_detect(NamesToCheck, "\\(ileum iii\\)")) +
            TimeRows[1]-1
         IleumIIITimeRow <- TimeRows[which((TimeRows + 1) %in% IleumIIIrows)]
         # Looking for the next blank row after IleumIIITimeRow
         IleumIIIEndRow <- which(is.na(sim_data_xl$...1))
         IleumIIIEndRow <- IleumIIIEndRow[IleumIIIEndRow > IleumIIITimeRow][1] - 1
         if(length(IleumIIITimeRow) > 0){
            GutRows[["ileum III"]] <- IleumIIITimeRow:IleumIIIEndRow
         }
         
         IleumIVrows <- which(str_detect(NamesToCheck, "\\(ileum iv\\)")) +
            TimeRows[1]-1
         IleumIVTimeRow <- TimeRows[which((TimeRows + 1) %in% IleumIVrows)]
         # Looking for the next blank row after IleumIVTimeRow
         IleumIVEndRow <- which(is.na(sim_data_xl$...1))
         IleumIVEndRow <- IleumIVEndRow[IleumIVEndRow > IleumIVTimeRow][1] - 1
         if(length(IleumIVTimeRow) > 0){
            GutRows[["ileum IV"]] <- IleumIVTimeRow:IleumIVEndRow
         }
         
         # Checking for inhibitor
         PerpPresent <- any(str_detect(NamesToCheck, "with inh"), na.rm = TRUE)
         
         rm(NamesToCheck)
         
         for(i in GutParts){
            
            # Checking which cells contain mean, 5th, and 95th
            # percentile data.
            NamesToCheck <- tolower(sim_data_xl$...1[GutRows[[i]]])
            
            RowsToUse <-
               c("mean" = which(str_detect(NamesToCheck,
                                           "enzyme value.*mean") &
                                   !str_detect(NamesToCheck, "with inh")) +
                    GutRows[[i]][1]-1,
                 "per5" = which(str_detect(NamesToCheck,
                                           "enzyme.* 5th percentile") &
                                   !str_detect(NamesToCheck, "with inh")) +
                    GutRows[[i]][1]-1,
                 "per95" = which(str_detect(NamesToCheck,
                                            "enzyme.*95th percentile") &
                                    !str_detect(NamesToCheck, "with inh")) +
                    GutRows[[i]][1]-1)
            
            sim_data_mean[[i]] <- sim_data_xl[c(GutRows[[i]][1], RowsToUse), ] %>%
               t() %>%
               as.data.frame() %>% slice(-(1:3)) %>%
               mutate_all(as.numeric)
            names(sim_data_mean[[i]]) <- c("Time", names(RowsToUse))
            sim_data_mean[[i]] <- sim_data_mean[[i]] %>%
               pivot_longer(names_to = "Trial", values_to = "Abundance", cols = -Time) %>%
               mutate(Enzyme = enzyme,
                      Tissue = i)
            
            rm(RowsToUse, NamesToCheck)
            
            if(PerpPresent){
               
               # Checking which cells contain mean, 5th, and 95th
               # percentile data.
               NamesToCheck <- tolower(sim_data_xl$...1[GutRows[[i]]])
               
               RowsToUse <-
                  c("mean" = which(str_detect(NamesToCheck,
                                              "enzyme value with inh mean")) +
                       GutRows[[i]][1]-1,
                    "per5" = which(str_detect(NamesToCheck,
                                              "enzyme value with inh 5th percentile")) +
                       GutRows[[i]][1]-1,
                    "per95" = which(str_detect(NamesToCheck,
                                               "enzyme value with inh 95th percentile")) +
                       GutRows[[i]][1]-1)
               
               sim_data_mean_inhib <- sim_data_xl[c(GutRows[[i]][1], RowsToUse), ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric)
               names(sim_data_mean_inhib) <- c("Time", names(RowsToUse))
               sim_data_mean_inhib <- sim_data_mean_inhib %>%
                  pivot_longer(names_to = "Trial",
                               values_to = "Abundance",
                               cols = -Time) %>%
                  mutate(Enzyme = enzyme,
                         Tissue = i,
                         PerpPresent = TRUE)
               
               sim_data_mean[[i]] <- bind_rows(sim_data_mean[[i]],
                                               sim_data_mean_inhib) %>%
                  mutate(PerpPresent = ifelse(is.na(PerpPresent),
                                              FALSE, PerpPresent))
               rm(NamesToCheck, RowsToUse, sim_data_mean_inhib)
            }
         }
         
         sim_data_mean <- bind_rows(sim_data_mean)
         
      } else {
         
         # non-gut tissue
         TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
         TimeRow <- TimeRow[TimeRow > which(sim_data_xl$...1 == "Population Statistics")][1]
         
         # Figuring out which rows contain which data
         FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                 which(1:nrow(sim_data_xl) > TimeRow))[1]
         FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
         NamesToCheck <- tolower(sim_data_xl$...1[TimeRow:(FirstBlank-1)])
         
         RowsToUse <- c(
            "mean" =
               which(str_detect(NamesToCheck, "mean") &
                        !str_detect(NamesToCheck,
                                    "geometric|with inh")) +
               TimeRow-1,
            "per5" =
               which(str_detect(NamesToCheck," 5(th)? percentile") &
                        !str_detect(NamesToCheck, "with inh|95")) +
               TimeRow-1,
            "per95" =
               which(str_detect(NamesToCheck, " 95(th)? percentile") &
                        !str_detect(NamesToCheck, "with inh")) +
               TimeRow-1,
            "per10" =
               which(str_detect(NamesToCheck," 10(th)? percentile") &
                        !str_detect(NamesToCheck, "with inh")) +
               TimeRow-1,
            "per90" =
               which(str_detect(NamesToCheck, " 90(th)? percentile") &
                        !str_detect(NamesToCheck, "with inh")) +
               TimeRow-1,
            "geomean" =
               which(str_detect(NamesToCheck, "geometric mean") &
                        !str_detect(NamesToCheck, "with inh")) +
               TimeRow-1,
            "median" =
               which(str_detect(NamesToCheck, "median") &
                        !str_detect(NamesToCheck, "with inh")) +
               TimeRow-1)
         
         sim_data_mean <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
            t() %>%
            as.data.frame() %>% slice(-(1:3)) %>%
            mutate_all(as.numeric)
         names(sim_data_mean) <- c("Time", names(RowsToUse))
         sim_data_mean <- sim_data_mean %>%
            pivot_longer(names_to = "Trial",
                         values_to = "Abundance",
                         cols = -Time) %>%
            mutate(Enzyme = enzyme,
                   Tissue = tissue)
         rm(RowsToUse)
         
         # Checking for inhibitor
         PerpPresent <- any(str_detect(NamesToCheck, "with inh"), na.rm = TRUE)
         if(PerpPresent){
            
            RowsToUse <- c(
               "mean" =
                  which(str_detect(NamesToCheck, "mean") &
                           !str_detect(NamesToCheck, "geometric") &
                           str_detect(NamesToCheck, "with inh")) +
                  TimeRow-1,
               "per5" =
                  which(str_detect(NamesToCheck," 5(th)? percentile") &
                           !str_detect(NamesToCheck, "95") &
                           str_detect(NamesToCheck, "with inh")) +
                  TimeRow-1,
               "per95" =
                  which(str_detect(NamesToCheck, " 95(th)? percentile") &
                           str_detect(NamesToCheck, "with inh")) +
                  TimeRow-1,
               "per10" =
                  which(str_detect(NamesToCheck," 10(th)? percentile") &
                           str_detect(NamesToCheck, "with inh")) +
                  TimeRow-1,
               "per90" =
                  which(str_detect(NamesToCheck, " 90(th)? percentile") &
                           str_detect(NamesToCheck, "with inh")) +
                  TimeRow-1,
               "geomean" =
                  which(str_detect(NamesToCheck, "geometric mean") &
                           str_detect(NamesToCheck, "with inh")) +
                  TimeRow-1,
               "median" =
                  which(str_detect(NamesToCheck, "median") &
                           str_detect(NamesToCheck, "with inh")) +
                  TimeRow-1)
            
            sim_data_mean_inhib <-
               sim_data_xl[c(TimeRow, RowsToUse), ] %>%
               t() %>%
               as.data.frame() %>% slice(-(1:3))
            names(sim_data_mean_inhib) <- c("Time", names(RowsToUse))
            sim_data_mean_inhib <- sim_data_mean_inhib %>%
               mutate_all(as.numeric) %>%
               pivot_longer(names_to = "Trial",
                            values_to = "Abundance",
                            cols = -Time) %>%
               mutate(Enzyme = enzyme,
                      Tissue = tissue,
                      PerpPresent = TRUE)
            
            sim_data_mean <- bind_rows(sim_data_mean,
                                       sim_data_mean_inhib) %>%
               mutate(PerpPresent = ifelse(is.na(PerpPresent),
                                           FALSE, PerpPresent))
            rm(RowsToUse)
         }
         rm(TimeRow)
      }
   }
   
   
   # Extracting individual data --------------------------------------------
   if(any(c("individual", "both") %in% returnAggregateOrIndiv)){
      
      if(length(GutParts) > 1 & tissue %in% c("gut", "adam gut")){
         
         sim_data_ind <- list()
         
         StartRow_ind <- which(sim_data_xl$...1 == "Individual Statistics")
         TimeRows <- which(str_detect(sim_data_xl$...1, "^Time "))
         TimeRows <- TimeRows[TimeRows > StartRow_ind][1:2]
         
         # Figuring out which rows contain which data
         FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                 which(1:nrow(sim_data_xl) > TimeRows[2]))[1]
         FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
         NamesToCheck <- tolower(sim_data_xl$...1[TimeRows[1]:nrow(sim_data_xl)])
         
         TimeRows <- which(str_detect(sim_data_xl$...1, "^Time "))
         TimeRows <- TimeRows[TimeRows > StartRow_indiv]
         
         # Figuring out which rows contain which data
         NamesToCheck <- tolower(sim_data_xl$...1[TimeRows[1]:nrow(sim_data_xl)])
         
         # Need to note which rows are for which gut part.
         GutRows <- list()
         
         SIrows <- which(str_detect(NamesToCheck, "\\(si\\)|\\(gut\\)")) +
            TimeRows[1]-1
         SITimeRow <- intersect(TimeRows+1, SIrows) - 1
         # Looking for the next blank row after SITimeRow
         SIEndRow <- which(is.na(sim_data_xl$...1))
         SIEndRow <- SIEndRow[which(SIEndRow > SITimeRow)][1] - 1
         # The above doesn't work if the last row is the last row of in the file, so
         # catching that exception.
         SIEndRow <- ifelse(is.na(SIEndRow), nrow(sim_data_xl), SIEndRow)
         if(length(SITimeRow) > 0){
            GutRows[["small intestine"]] <- SITimeRow:SIEndRow
         }
         
         # NB: This regex should work for both ADAM-model and also
         # non-ADAM-model colon abundances
         Colonrows <- which(str_detect(NamesToCheck, "\\(colon\\)")) +
            TimeRows[1]-1
         ColonTimeRow <- intersect(TimeRows+1, Colonrows) - 1
         # Looking for the next blank row after ColonTimeRow
         ColonEndRow <- which(is.na(sim_data_xl$...1))
         ColonEndRow <- ColonEndRow[which(ColonEndRow > ColonTimeRow)][1] - 1
         # The above doesn't work if the last row is the last row of in the file, so
         # catching that exception.
         ColonEndRow <- ifelse(is.na(ColonEndRow), nrow(sim_data_xl), ColonEndRow)
         if(length(ColonTimeRow) > 0){
            GutRows[["colon"]] <- ColonTimeRow:ColonEndRow
         }
         
         Stomachrows <- which(str_detect(NamesToCheck, "\\(stomach\\)")) +
            TimeRows[1]-1
         StomachTimeRow <- intersect(TimeRows+1, Stomachrows) - 1
         # Looking for the next blank row after StomachTimeRow
         StomachEndRow <- which(is.na(sim_data_xl$...1))
         StomachEndRow <- StomachEndRow[which(StomachEndRow > StomachTimeRow)][1] - 1
         # The above doesn't work if the last row is the last row of in the file, so
         # catching that exception.
         StomachEndRow <- ifelse(is.na(StomachEndRow), nrow(sim_data_xl), StomachEndRow)
         if(length(StomachTimeRow) > 0){
            GutRows[["stomach"]] <- StomachTimeRow:StomachEndRow
         }
         
         Duodenumrows <- which(str_detect(NamesToCheck, "\\(duodenum\\)")) +
            TimeRows[1]-1
         DuodenumTimeRow <- intersect(TimeRows+1, Duodenumrows) - 1
         # Looking for the next blank row after DuodenumTimeRow
         DuodenumEndRow <- which(is.na(sim_data_xl$...1))
         DuodenumEndRow <- DuodenumEndRow[which(DuodenumEndRow > DuodenumTimeRow)][1] - 1
         # The above doesn't work if the last row is the last row of in the file, so
         # catching that exception.
         DuodenumEndRow <- ifelse(is.na(DuodenumEndRow), nrow(sim_data_xl), DuodenumEndRow)
         if(length(DuodenumTimeRow) > 0){
            GutRows[["duodenum"]] <- DuodenumTimeRow:DuodenumEndRow
         }
         
         JejunumIrows <- which(str_detect(NamesToCheck, "\\(jejunum i\\)")) +
            TimeRows[1]-1
         JejunumITimeRow <- intersect(TimeRows+1, JejunumIrows) - 1
         # Looking for the next blank row after JejunumITimeRow
         JejunumIEndRow <- which(is.na(sim_data_xl$...1))
         JejunumIEndRow <- JejunumIEndRow[which(JejunumIEndRow > JejunumITimeRow)][1] - 1
         # The above doesn't work if the last row is the last row of in the file, so
         # catching that exception.
         JejunumIEndRow <- ifelse(is.na(JejunumIEndRow), nrow(sim_data_xl), JejunumIEndRow)
         if(length(JejunumITimeRow) > 0){
            GutRows[["jejunum I"]] <- JejunumITimeRow:JejunumIEndRow
         }
         
         JejunumIIrows <- which(str_detect(NamesToCheck, "\\(jejunum ii\\)")) +
            TimeRows[1]-1
         JejunumIITimeRow <- intersect(TimeRows+1, JejunumIIrows) - 1
         # Looking for the next blank row after JejunumIITimeRow
         JejunumIIEndRow <- which(is.na(sim_data_xl$...1))
         JejunumIIEndRow <- JejunumIIEndRow[which(JejunumIIEndRow > JejunumIITimeRow)][1] - 1
         # The above doesn't work if the last row is the last row of in the file, so
         # catching that exception.
         JejunumIIEndRow <- ifelse(is.na(JejunumIIEndRow), nrow(sim_data_xl), JejunumIIEndRow)
         if(length(JejunumIITimeRow) > 0){
            GutRows[["jejunum II"]] <- JejunumIITimeRow:JejunumIIEndRow
         }
         
         IleumIrows <- which(str_detect(NamesToCheck, "\\(ileum i\\)")) +
            TimeRows[1]-1
         IleumITimeRow <- intersect(TimeRows+1, IleumIrows) - 1
         # Looking for the next blank row after IleumITimeRow
         IleumIEndRow <- which(is.na(sim_data_xl$...1))
         IleumIEndRow <- IleumIEndRow[which(IleumIEndRow > IleumITimeRow)][1] - 1
         # The above doesn't work if the last row is the last row of in the file, so
         # catching that exception.
         IleumIEndRow <- ifelse(is.na(IleumIEndRow), nrow(sim_data_xl), IleumIEndRow)
         if(length(IleumITimeRow) > 0){
            GutRows[["ileum I"]] <- IleumITimeRow:IleumIEndRow
         }
         
         IleumIIrows <- which(str_detect(NamesToCheck, "\\(ileum ii\\)")) +
            TimeRows[1]-1
         IleumIITimeRow <- intersect(TimeRows+1, IleumIIrows) - 1
         # Looking for the next blank row after IleumIITimeRow
         IleumIIEndRow <- which(is.na(sim_data_xl$...1))
         IleumIIEndRow <- IleumIIEndRow[which(IleumIIEndRow > IleumIITimeRow)][1] - 1
         # The above doesn't work if the last row is the last row of in the file, so
         # catching that exception.
         IleumIIEndRow <- ifelse(is.na(IleumIIEndRow), nrow(sim_data_xl), IleumIIEndRow)
         if(length(IleumIITimeRow) > 0){
            GutRows[["ileum II"]] <- IleumIITimeRow:IleumIIEndRow
         }
         
         IleumIIIrows <- which(str_detect(NamesToCheck, "\\(ileum iii\\)")) +
            TimeRows[1]-1
         IleumIIITimeRow <- intersect(TimeRows+1, IleumIIIrows) - 1
         # Looking for the next blank row after IleumIIITimeRow
         IleumIIIEndRow <- which(is.na(sim_data_xl$...1))
         IleumIIIEndRow <- IleumIIIEndRow[which(IleumIIIEndRow > IleumIIITimeRow)][1] - 1
         # The above doesn't work if the last row is the last row of in the file, so
         # catching that exception.
         IleumIIIEndRow <- ifelse(is.na(IleumIIIEndRow), nrow(sim_data_xl), IleumIIIEndRow)
         if(length(IleumIIITimeRow) > 0){
            GutRows[["ileum III"]] <- IleumIIITimeRow:IleumIIIEndRow
         }
         
         IleumIVrows <- which(str_detect(NamesToCheck, "\\(ileum iv\\)")) +
            TimeRows[1]-1
         IleumIVTimeRow <- intersect(TimeRows+1, IleumIVrows) - 1
         # Looking for the next blank row after IleumIVTimeRow
         IleumIVEndRow <- which(is.na(sim_data_xl$...1))
         IleumIVEndRow <- IleumIVEndRow[which(IleumIVEndRow > IleumIVTimeRow)][1] - 1
         # The above doesn't work if the last row is the last row of in the file, so
         # catching that exception.
         IleumIVEndRow <- ifelse(is.na(IleumIVEndRow), nrow(sim_data_xl), IleumIVEndRow)
         if(length(IleumIVTimeRow) > 0){
            GutRows[["ileum IV"]] <- IleumIVTimeRow:IleumIVEndRow
         }
         
         # Checking for inhibitor
         PerpPresent <- any(str_detect(NamesToCheck, "with inh"), na.rm = TRUE)
         
         rm(NamesToCheck)
         
         for(i in GutParts){
            
            RowsToUse <- intersect(
               GutRows[[i]],
               GutRows[[i]][which(!str_detect(tolower(sim_data_xl$...1[GutRows[[i]]]),
                                              "with inh"))])
            
            sim_data_ind[[i]] <- sim_data_xl[RowsToUse, ] %>%
               t() %>%
               as.data.frame() %>% slice(-(1:3)) %>%
               mutate_all(as.numeric)%>%
               rename(Time = "V1")
            
            SubjTrial <- sim_data_xl[RowsToUse[-1], 2:3] %>%
               rename(Individual = ...2, Trial = ...3) %>%
               mutate(SubjTrial = paste0("ID", Individual, "_", Trial))
            
            names(sim_data_ind[[i]])[2:ncol(sim_data_ind[[i]])] <- SubjTrial$SubjTrial
            
            sim_data_ind[[i]] <- sim_data_ind[[i]] %>%
               pivot_longer(names_to = "SubjTrial", values_to = "Abundance",
                            cols = -Time) %>%
               mutate(Enzyme = enzyme,
                      Tissue = i,
                      SubjTrial = sub("ID", "", SubjTrial)) %>%
               separate(SubjTrial, into = c("Individual", "Trial"),
                        sep = "_") %>%
               mutate(across(.cols = c("Individual", "Trial"),
                             .fns = as.numeric))
            
            rm(RowsToUse)
            
            if(PerpPresent){
               
               RowsToUse <- GutRows[[i]][which(
                  str_detect(tolower(sim_data_xl$...1[GutRows[[i]]]),
                             "with inh"))]
               
               sim_data_ind_inhib <- sim_data_xl[c(GutRows[[i]][1], RowsToUse), ] %>%
                  t() %>%
                  as.data.frame() %>% slice(-(1:3)) %>%
                  mutate_all(as.numeric)%>%
                  rename(Time = "V1")
               
               SubjTrial <- sim_data_xl[RowsToUse, 2:3] %>%
                  rename(Individual = ...2, Trial = ...3) %>%
                  mutate(SubjTrial = paste0("ID", Individual, "_", Trial))
               
               names(sim_data_ind_inhib)[2:ncol(sim_data_ind_inhib)] <- SubjTrial$SubjTrial
               
               sim_data_ind_inhib <- sim_data_ind_inhib %>%
                  pivot_longer(names_to = "SubjTrial", values_to = "Abundance",
                               cols = -Time) %>%
                  mutate(Enzyme = enzyme,
                         Tissue = i,
                         PerpPresent = TRUE,
                         SubjTrial = sub("ID", "", SubjTrial)) %>%
                  separate(SubjTrial, into = c("Individual", "Trial"),
                           sep = "_") %>%
                  mutate(across(.cols = c("Individual", "Trial"),
                                .fns = as.numeric))
               
               sim_data_ind[[i]] <- bind_rows(sim_data_ind[[i]],
                                              sim_data_ind_inhib) %>%
                  mutate(PerpPresent = ifelse(is.na(PerpPresent),
                                              FALSE, PerpPresent))
               rm(RowsToUse, sim_data_ind_inhib)
            }
         }
         
         sim_data_ind <- bind_rows(sim_data_ind)
         
      } else {
         
         # individual data
         StartRow_ind <- which(sim_data_xl$...1 == "Individual Statistics")
         TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
         TimeRow <- TimeRow[TimeRow > StartRow_ind][1]
         
         # Figuring out which rows contain which data
         FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                                 which(1:nrow(sim_data_xl) > TimeRow))[1]
         FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
         NamesToCheck <- tolower(sim_data_xl$...1[TimeRow:nrow(sim_data_xl)])
         
         RowsToUse <- which(!str_detect(NamesToCheck, "with inh")) + TimeRow-1
         
         sim_data_ind <- sim_data_xl[RowsToUse, ] %>%
            t() %>%
            as.data.frame() %>% slice(-(1:3)) %>%
            mutate_all(as.numeric) %>%
            rename(Time = "V1")
         
         SubjTrial <- sim_data_xl[RowsToUse[-1], 2:3] %>%
            rename(Individual = ...2, Trial = ...3) %>%
            mutate(SubjTrial = paste0("ID", Individual, "_", Trial))
         
         names(sim_data_ind)[2:ncol(sim_data_ind)] <- SubjTrial$SubjTrial
         
         sim_data_ind <- sim_data_ind %>%
            pivot_longer(names_to = "SubjTrial",
                         values_to = "Abundance",
                         cols = -Time) %>%
            mutate(Enzyme = enzyme,
                   Tissue = tissue,
                   SubjTrial = sub("ID", "", SubjTrial)) %>%
            separate(SubjTrial, into = c("Individual", "Trial"),
                     sep = "_") %>%
            mutate(across(.cols = c("Individual", "Trial"),
                          .fns = as.numeric))
         
         rm(RowsToUse)
         
         # Checking for inhibitor
         PerpPresent <- any(str_detect(NamesToCheck, "with inh"), na.rm = TRUE)
         
         if(PerpPresent){
            
            RowsToUse <- which(str_detect(NamesToCheck, "with inh")) + TimeRow-1
            
            sim_data_ind_inhib <-
               sim_data_xl[c(TimeRow, RowsToUse), ] %>%
               t() %>%
               as.data.frame() %>% slice(-(1:3)) %>%
               mutate_all(as.numeric) %>%
               rename(Time = "V1")
            
            names(sim_data_ind_inhib)[
               2:ncol(sim_data_ind_inhib)] <- SubjTrial$SubjTrial
            
            sim_data_ind_inhib <- sim_data_ind_inhib %>%
               pivot_longer(names_to = "SubjTrial",
                            values_to = "Abundance",
                            cols = -Time) %>%
               mutate(Enzyme = enzyme,
                      Tissue = tissue,
                      PerpPresent = TRUE,
                      SubjTrial = sub("ID", "", SubjTrial)) %>%
               separate(SubjTrial, into = c("Individual", "Trial"),
                        sep = "_") %>%
               mutate(across(.cols = c("Individual", "Trial"),
                             .fns = as.numeric))
            
            sim_data_ind <- bind_rows(sim_data_ind,
                                      sim_data_ind_inhib) %>%
               mutate(PerpPresent = ifelse(is.na(PerpPresent),
                                           FALSE, PerpPresent))
            
            rm(RowsToUse)
         }
         rm(TimeRow, NamesToCheck)
      }
   }
   
   # Putting everything together ------------------------------------------
   
   TimeUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "^Time"))][1]
   TimeUnits <- ifelse(TimeUnits == "Time (h)", "Hours", "Minutes")
   
   Data <- list()
   
   if(any(c("aggregate", "both") %in% returnAggregateOrIndiv)){
      Data[["agg"]] <- sim_data_mean %>%
         arrange(Trial, Time) %>% 
         mutate(IndivOrAgg = "aggregate", 
                Simulated = TRUE)
   }
   
   if(any(c("individual", "both") %in% returnAggregateOrIndiv)){
      Data[["indiv"]] <- sim_data_ind %>%
         mutate(Individual = as.character(Individual),
                Trial = as.character(Trial), 
                Simulated = TRUE, 
                IndivOrAgg = "individual") %>%
         arrange(Individual, Time)
   }
   
   Data <- bind_rows(Data)
   
   if("individual" %in% returnAggregateOrIndiv){
      Data <- Data %>%
         mutate(Individual = ifelse(is.na(Individual), Trial, Individual))
   }
   
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
   
   # Adding DoseNumber so that we can skip extractExpDetails in ct_plot when
   # the user requests a specific dose.
   MyIntervals <- 
      c("substrate" = Deets$DoseInt_sub,
        "primary metabolite 1" = Deets$DoseInt_sub,
        "primary metabolite 2" = Deets$DoseInt_sub,
        "secondary metabolite" = Deets$DoseInt_sub,
        "inhibitor 1" = ifelse(is.null(Deets$DoseInt_inhib),
                               NA, Deets$DoseInt_inhib),
        "inhibitor 1 metabolite" = ifelse(is.null(Deets$DoseInt_inhib),
                                          NA, Deets$DoseInt_inhib),
        "inhibitor 2" = ifelse(is.null(Deets$DoseInt_inhib2),
                               NA, Deets$DoseInt_inhib2))
   
   MyStartTimes <- 
      c("substrate" = Deets$StartHr_sub,
        "primary metabolite 1" = Deets$StartHr_sub,
        "primarymetabolite 2" = Deets$StartHr_sub,
        "secondary metabolite" = Deets$StartHr_sub,
        "inhibitor 1" = ifelse(is.null(Deets$StartHr_inhib), NA,
                               Deets$StartHr_inhib),
        "inhibitor 2" = ifelse(is.null(Deets$StartHr_inhib2), NA,
                               Deets$StartHr_inhib2),
        "inhibitor 1 metabolite" = ifelse(is.null(Deets$StartHr_inhib), NA,
                                          Deets$StartHr_inhib))
   
   MyMaxDoseNum <- 
      c("substrate" = ifelse(Deets$Regimen_sub == "Single Dose", 
                             1, Deets$NumDoses_sub),
        "primary metabolite 1" = ifelse(Deets$Regimen_sub == "Single Dose", 
                                        1, Deets$NumDoses_sub),
        "primarymetabolite 2" = ifelse(Deets$Regimen_sub == "Single Dose", 
                                       1, Deets$NumDoses_sub),
        "secondary metabolite" = ifelse(Deets$Regimen_sub == "Single Dose", 
                                        1, Deets$NumDoses_sub),
        "inhibitor 1" = ifelse(is.null(Deets$NumDoses_inhib), NA,
                               ifelse(Deets$Regimen_inhib == "Single Dose", 
                                      1, Deets$NumDoses_inhib)),
        "inhibitor 2" = ifelse(is.null(Deets$NumDoses_inhib2), NA,
                               ifelse(Deets$Regimen_inhib2 == "Single Dose", 
                                      1, Deets$NumDoses_inhib2)),
        "inhibitor 1 metabolite" = ifelse(is.null(Deets$NumDoses_inhib), NA,
                                          ifelse(Deets$Regimen_inhib == "Single Dose", 
                                                 1, Deets$NumDoses_inhib)))
   
   # Converting data to numeric while also retaining names
   suppressWarnings(
      MyIntervals <- sapply(MyIntervals, FUN = as.numeric))
   suppressWarnings(
      MyStartTimes <- sapply(MyStartTimes, FUN = as.numeric))
   suppressWarnings(
      MyMaxDoseNum <- sapply(MyMaxDoseNum, FUN = as.numeric))
   
   Data <- Data %>%
      mutate(StartHr_sub = MyStartTimes["substrate"],
             TimeSinceDose1_sub = Time - StartHr_sub,
             DoseInt_sub = MyIntervals["substrate"],
             MaxDoseNum_sub = MyMaxDoseNum["substrate"],
             DoseNum_sub = Time %/% DoseInt_sub + 1,
             # Taking care of possible artifacts
             DoseNum_sub = ifelse(DoseNum_sub < 0, 0, DoseNum_sub),
             DoseNum_sub = ifelse(DoseNum_sub > MaxDoseNum_sub, 
                                  MaxDoseNum_sub, DoseNum_sub),
             # If it was a single dose, make everything after StartHr dose
             # 1 and everything before StartHr dose 0. if it was a single
             # dose, then DoseInt is NA.
             DoseNum_sub = ifelse(is.na(DoseInt_sub),
                                  ifelse(TimeSinceDose1_sub < 0, 0, 1), DoseNum_sub),
             StartHr_inhib1 = MyStartTimes["inhibitor 1"],
             TimeSinceDose1_inhib1 = Time - StartHr_inhib1,
             DoseInt_inhib1 = MyIntervals["inhibitor 1"],
             MaxDoseNum_inhib1 = MyMaxDoseNum["inhibitor 1"],
             DoseNum_inhib1 = Time %/% DoseInt_inhib1 + 1,
             # Taking care of possible artifacts
             DoseNum_inhib1 = ifelse(DoseNum_inhib1 < 0, 0, DoseNum_inhib1),
             DoseNum_inhib1 = ifelse(DoseNum_inhib1 > MaxDoseNum_inhib1, 
                                     MaxDoseNum_inhib1, DoseNum_inhib1),
             # If it was a single dose, make everything after StartHr dose
             # 1 and everything before StartHr dose 0. if it was a single
             # dose, then DoseInt is NA.
             DoseNum_inhib1 = ifelse(is.na(DoseInt_inhib1),
                                     ifelse(TimeSinceDose1_inhib1 < 0, 0, 1), DoseNum_inhib1),
             StartHr_inhib2 = MyStartTimes["inhibitor 2"],
             TimeSinceDose1_inhib2 = Time - StartHr_inhib2,
             DoseInt_inhib2 = MyIntervals["inhibitor 2"],
             MaxDoseNum_inhib2 = MyMaxDoseNum["inhibitor 2"],
             DoseNum_inhib2 = Time %/% DoseInt_inhib2 + 1,
             # Taking care of possible artifacts
             DoseNum_inhib2 = ifelse(DoseNum_inhib2 < 0, 0, DoseNum_inhib2),
             DoseNum_inhib2 = ifelse(DoseNum_inhib2 > MaxDoseNum_inhib2, 
                                     MaxDoseNum_inhib2, DoseNum_inhib2),
             # If it was a single dose, make everything after StartHr dose
             # 1 and everything before StartHr dose 0. if it was a single
             # dose, then DoseInt is NA.
             DoseNum_inhib2 = ifelse(is.na(DoseInt_inhib2),
                                     ifelse(TimeSinceDose1_inhib2 < 0, 0, 1), DoseNum_inhib2))
   
   # # Checking for any custom dosing
   # if(any(str_detect(names(Deets), "CustomDosing"))){
   #     CDCompounds <-
   #         data.frame(CompoundSuffix = 
   #                        str_extract(names(Deets)[str_detect(names(Deets),
   #                                                            "CustomDosing")],
   #                                    "_sub|_inhib(2)?")) %>% 
   #         mutate(CompoundID = recode(CompoundSuffix, "_sub" = "substrate", 
   #                                    "_inhib" = "inhibitor 1", 
   #                                    "_inhib2" = "inhibitor 2"))
   #     
   #     Dosing <- list()
   #     # This is kind of a disaster... Looking for a better way to code this.
   #     
   #     for(j in CDCompounds$CompoundID){
   #         Dosing[[j]] <-
   #             Deets[[paste0("CustomDosing", 
   #                           CDCompounds$CompoundSuffix[CDCompounds$CompoundID == j])]] %>% 
   #             mutate(CompoundID = CDCompounds$CompoundID[CDCompounds$CompoundID == j])
   #         
   #         if(max(Data$Time) > max(Dosing[[j]]$Time)){
   #             Dosing[[j]] <- Dosing[[j]] %>% 
   #                 bind_rows(data.frame(Time = max(Data$Time) + 1, 
   #                                      DoseNum = max(Dosing[[j]]$DoseNum)))
   #         }
   #         
   #         Dosing[[j]]$Breaks <-
   #             as.character(cut(Dosing[[j]]$Time, breaks = Dosing[[j]]$Time,
   #                              right = FALSE))
   #     }
   #     
   #     # LEFT OFF HERE - Not sure how best to deal with this since enzyme
   #     # abundances are different from concentrations of specific compounds. Also
   #     # not sure it's going to be that important.
   #     
   #     
   #     MyData <- list()
   #     MyData[["not CD"]] <- Data %>% filter(CD == "not CD")
   #     
   #     for(j in unique(Data$CD)[!unique(Data$CD) == "not CD"]){
   #         MyData[[j]] <- Data %>% filter(CD == j) %>% select(-DoseNum)
   #         # This should make the right breaks for each possible compound
   #         # with custom dosing. They should match the breaks in the
   #         # appropriate list item in Dosing.
   #         MyData[[j]]$Breaks <-
   #             as.character(cut(MyData[[j]]$Time, breaks = Dosing[[j]]$Time,
   #                              right = FALSE))
   #         
   #         MyData[[j]] <- MyData[[j]] %>% 
   #             left_join(Dosing[[j]] %>% select(CompoundID, Breaks, DoseNum))
   #         
   #     }
   #     
   #     Data <- bind_rows(MyData)
   #     if(length(setdiff(unique(OrigCompounds),
   #                       unique(Data$CompoundID))) > 0){
   #         warning("PROBLEM WITH CUSTOM DOSING! Please tell Laura Shireman if you see this message.")
   #     }
   # }
   
   
   # Checking for when the simulation ends right at the last dose b/c
   # then, setting that number to 1 dose lower
   if(length(Data %>% filter(DoseNum_sub == max(Data$DoseNum_sub)) %>%
             pull(Time) %>% unique()) == 1){
      MyMaxDoseNum_sub <- max(Data$DoseNum_sub)
      Data <- Data %>%
         mutate(DoseNum_sub = ifelse(DoseNum_sub == MyMaxDoseNum_sub,
                                     MyMaxDoseNum_sub - 1, DoseNum_sub))
   }
   
   if(length(Data %>% filter(DoseNum_inhib1 == max(Data$DoseNum_inhib1)) %>%
             pull(Time) %>% unique()) == 1){
      MyMaxDoseNum_inhib1 <- max(Data$DoseNum_inhib1)
      Data <- Data %>%
         mutate(DoseNum_inhib1 = ifelse(DoseNum_inhib1 == MyMaxDoseNum_inhib1,
                                        MyMaxDoseNum_inhib1 - 1, DoseNum_inhib1))
   }
   
   if(length(Data %>% filter(DoseNum_inhib2 == max(Data$DoseNum_inhib2)) %>%
             pull(Time) %>% unique()) == 1){
      MyMaxDoseNum_inhib2 <- max(Data$DoseNum_inhib2)
      Data <- Data %>%
         mutate(DoseNum_inhib2 = ifelse(DoseNum_inhib2 == MyMaxDoseNum_inhib2,
                                        MyMaxDoseNum_inhib2 - 1, DoseNum_inhib2))
   }
   
   # Noting exactly what the perpetrators were
   AllPerpetrators <- c(Deets$Inhibitor1, Deets$Inhibitor2,
                        Deets$Inhibitor1Metabolite)
   AllPerpetrators <- AllPerpetrators[complete.cases(AllPerpetrators)]
   
   # Finalizing, tidying, selecting only useful columns
   Data <- Data %>%
      mutate(Time_units = tolower({{TimeUnits}}),
             File = sim_data_file,
             Inhibitor = ifelse(PerpPresent,
                                str_comma(AllPerpetrators), "none"), 
             Substrate = Deets$Substrate, 
             # Adding a little bit more info
             Species = Deets$Species) %>%
      arrange(across(any_of(c("File", "Enzyme", "Tissue",
                              "Substrate", "Inhibitor",
                              "Individual", "Trial", "Time")))) %>%
      select(any_of(c("Enzyme", "Tissue", "Substrate", "Inhibitor", 
                      "Simulated", "IndivOrAgg", "Species", 
                      "Individual", "Trial", "Time", "Abundance",
                      "Time_units", 
                      "DoseNum_sub", "Dose_int_sub", "TimeSinceDose1_sub",
                      "DoseNum_inhib1", "Dose_int_inhib1", "TimeSinceDose1_inhib1",
                      "DoseNum_inhib2", "Dose_int_inhib2", "TimeSinceDose1_inhib2", 
                      "File")))
   
   return(Data)
   
}
