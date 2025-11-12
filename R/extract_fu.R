#' Extract fu,plasma values that change with concentration or with time from a
#' Simulator output Excel file. UNDER CONSTRUCTION.
#'
#' @description \code{extract_fu} extracts the fraction unbound in plasma from a
#' simulator output Excel file. A tab named something like "fu Profile (Sub)"
#' must be present. 
#' 
#' \itemize{\item{This currently only extracts data for the substrate, but if
#' you have an example with a different compound, please talk to Laura Shireman.}
#' 
#' \item{This has only been set up for baseline simulations without 
#' perpetrators. Please tell Laura Shireman if you need a DDI version.}}
#'
#' @param sim_data_files the Simcyp Simulator Excel results files containing the
#'   simulated time-dependent fu data, in quotes
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#'
#' @return a data.frame
#'
#' @export
#' 


extract_fu <- function(sim_data_files,
                       existing_exp_details = NA){
   
   # Error catching --------------------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
   
   # If user did not supply files, then extract all the files in the current
   # folder that end in "xlsx" or in all subfolders if they wanted it to be
   # recursive.
   if(length(sim_data_files) == 1 &&
      (is.na(sim_data_files) | sim_data_files == "recursive")){
      sim_data_files <- list.files(pattern = "\\.xlsx$",
                                   recursive = (complete.cases(sim_data_files) &&
                                                   sim_data_files == "recursive"))
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   }
   
   # If they didn't include ".xlsx" at the end of the file name, add "xlsx".
   MissingExt <- which(str_detect(sim_data_files, "\\.xlsx$") == FALSE)
   sim_data_files[MissingExt] <- 
      sub("\\.wksz$|\\.dscw$", ".xlsx", sim_data_files[MissingExt])
   
   sim_data_files <- unique(sim_data_files)
   
   # Checking for file name issues
   CheckFileNames <- check_file_name(sim_data_files)
   BadFileNames <- CheckFileNames[!CheckFileNames == "File name meets naming standards."]
   if(length(BadFileNames)> 0){
      BadFileNames <- paste0(names(BadFileNames), ": ", BadFileNames)
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"), "\n"),
              call. = FALSE)
   }
   
   
   # Main body of function ----------------------------------------------------------------
   
   extract_fu_subfun <- function(sim_data_file){
      
      # This is currently only set up for the substrate, but we may change that
      # later. For now, setting the object compoundID as a placeholder since it
      # will only be "substrate" at the moment.
      compoundID <- "substrate"
      
      # Getting summary data for the simulation(s)
      
      if("logical" %in% class(existing_exp_details)){
         Deets <- extractExpDetails(sim_data_file,
                                    exp_details = "Summary and Input")[["MainDetails"]]
         
      } else {
         
         Deets <- filter_sims(existing_exp_details, sim_data_file, "include")
         Deets <- harmonize_details(Deets)[["MainDetails"]] %>% 
            filter(File == sim_data_file)
         
         if(nrow(Deets) == 0){
            Deets <- extractExpDetails(sim_data_file,
                                       exp_details = "Summary and Input")[["MainDetails"]]
         }
      }
      
      if(Deets$PopRepSim == "Yes"){
         warning(wrapn(paste0("The simulator file supplied, '", 
                              sim_data_file, 
                              "', is for a population-representative simulation and thus doesn't have any aggregate data. Please be warned that some plotting functions will not work well without aggregate data.")),
                 call. = FALSE)
      }
      
      # Figuring out which sheet to extract and dealing with case since that
      # apparently changes between Simulator versions.
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
      
      # Checking that the file is, indeed, a simulator output file.
      SheetNames <- gsub("`", "", str_split_1(Deets$SheetNames, "` `"))
      
      if(all(c("Input Sheet", "Summary") %in% SheetNames) == FALSE){
         # Using "warning" instead of "stop" here b/c I want this to be able to
         # pass through to other functions and just skip any files that
         # aren't simulator output.
         warning(wrapn(paste("The file", sim_data_file,
                             "does not appear to be a Simcyp Simulator output Excel file. We cannot return any information for this file.")), 
                 call. = FALSE)
         return(data.frame())
      }
      
      SheetToExtract <- SheetNames[
         str_detect(tolower(SheetNames), "fu profile \\(sub\\)")]
      
      if(length(SheetToExtract) == 0){
         warning(wrapn(paste0("The simulator output file provided, '", 
                              sim_data_file, 
                              "', does not appear to have a sheet titled 'fu profile (Sub)', which is what we need for extracting dynamic fu values.")),
                 call. = FALSE)
         return(data.frame())
      }
      
      # Reading in simulated abundance-time profile data
      sim_data_xl <- suppressMessages(
         readxl::read_excel(path = sim_data_file,
                            sheet = SheetToExtract,
                            col_names = FALSE))
      
      # Extracting aggregate data ---------------------------------------------
      
      StartRow_agg <- which(sim_data_xl$...1 == "Population Statistics")
      TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
      TimeRow <- TimeRow[TimeRow > StartRow_agg][1]
      
      # Figuring out which rows contain which data
      FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                              which(1:nrow(sim_data_xl) > TimeRow))[1]
      FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl), FirstBlank)
      NamesToCheck <- sim_data_xl$...1[TimeRow[1]:(FirstBlank-1)]
      
      RowsToUse <- which(str_detect(NamesToCheck, "^fu "))
      
      IDs_agg <- data.frame(ColOrig = paste0("V", 1:(length(RowsToUse) + 1)), 
                            ID = c("Time", NamesToCheck[RowsToUse])) %>% 
         mutate(Trial = str_trim(str_extract(tolower(ID),
                                             "(geometric)? mean| 5(th)? percentile| 95(th)? percentile|median")), 
                Trial = case_when(Trial == "5th percentile" ~ "per5", 
                                  Trial == "95th percentile" ~ "per95", 
                                  Trial == "geometric mean" ~ "geomean",
                                  TRUE ~ Trial))
      
      sim_data_mean <- sim_data_xl[c(TimeRow, RowsToUse + TimeRow - 1), ] %>% 
         t() %>%
         as.data.frame() %>% slice(-(1:3)) %>%
         mutate_all(as.numeric) %>% 
         rename(Time = V1) %>% 
         pivot_longer(names_to = "ColOrig", values_to = "fu", 
                      cols = -Time) %>%
         left_join(IDs_agg, by = "ColOrig")
      
      # In V24, it's not obvious which row in the population data is the mean
      # data (it ends up being the one labeled as just "fu") and it's even less
      # obvious what kind of summary stat was used. After getting the individual
      # data, if there is no arithmetic or geometric mean included, calculate
      # them. 
      
      # Extracting individual data --------------------------------------------
      
      StartRow_indiv <- which(sim_data_xl$...1 == "Individual Statistics")
      TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))
      TimeRow <- TimeRow[TimeRow > StartRow_indiv][1]
      
      # Figuring out which rows contain which data
      FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                              which(1:nrow(sim_data_xl) > TimeRow))[1]
      FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl) + 1, FirstBlank)
      
      RowsToUse <- which(str_detect(sim_data_xl$...1, "^fu$"))
      RowsToUse <- RowsToUse[RowsToUse > StartRow_indiv & RowsToUse < FirstBlank]
      
      IDs_indiv <- data.frame(ColOrig = paste0("V", 1:(length(RowsToUse) + 1)), 
                              ID = c("Time", t(sim_data_xl[RowsToUse, 1])), 
                              Individual = sim_data_xl[c(TimeRow, RowsToUse), 2], 
                              Trial = sim_data_xl[c(TimeRow, RowsToUse), 3]) %>% 
         rename(Individual = ...2, 
                Trial = ...3)
      
      sim_data_indiv <- sim_data_xl[c(TimeRow, RowsToUse), ] %>% 
         t() %>%
         as.data.frame() %>% slice(-(1:3)) %>%
         mutate_all(as.numeric) %>% 
         rename(Time = V1) %>% 
         pivot_longer(names_to = "ColOrig", values_to = "fu", 
                      cols = -Time) %>%
         left_join(IDs_indiv, by = "ColOrig")
      
      # Calculating any missing summary stats --------------------------------
      
      StatsToCalc <- c("mean", "geomean") %in% 
         unique(sim_data_mean$Trial) == FALSE
      names(StatsToCalc) <- c("mean", "geomean")
      
      StatsToCalc <- names(StatsToCalc[StatsToCalc == TRUE])
      
      TOADD <- sim_data_indiv %>% 
         group_by(Time) %>% 
         summarize(mean = mean(fu), 
                   geomean = gm_mean(fu)) %>% 
         ungroup() %>% 
         pivot_longer(cols = -Time, 
                      names_to = "Trial", 
                      values_to = "fu") %>% 
         filter(Trial %in% StatsToCalc)
      
      sim_data_mean <- bind_rows(sim_data_mean, 
                                 TOADD)
      
      rm(TOADD)
      
      # Putting everything together ------------------------------------------
      
      TimeUnits <- sim_data_xl$...1[which(str_detect(sim_data_xl$...1, "^Time"))][1]
      TimeUnits <- ifelse(TimeUnits == "Time (h)", "hours", "minutes")
      
      Data <- list()
      
      Data[["agg"]] <- sim_data_mean %>%
         mutate(IndivOrAgg = "aggregate") %>% 
         arrange(Trial, Time)
      
      Data[["indiv"]] <- sim_data_indiv %>%
         mutate(Individual = as.character(Individual),
                Trial = as.character(Trial), 
                IndivOrAgg = "individual") %>%
         arrange(Individual, Time)
      
      Data <- bind_rows(Data) %>%
         mutate(Individual = ifelse(is.na(Individual), Trial, Individual), 
                Simulated = TRUE, 
                Tissue = "plasma", 
                Inhibitor = "none")
      
      Data <- calc_dosenumber(
         ct_dataframe = Data %>% 
            rename(Conc = fu) %>% 
            mutate(File = ff, 
                   CompoundID = compoundID),
         existing_exp_details = existing_exp_details) %>% 
         rename(fu = Conc)
      
      # Adding compound name
      MyCompound <- 
         c("substrate" = Deets$Substrate,
           "primary metabolite 1" = Deets$PrimaryMetabolite1,
           "primary metabolite 2" = Deets$PrimaryMetabolite2,
           "secondary metabolite" = Deets$SecondaryMetabolite,
           "inhibitor 1" = Deets$Inhibitor1, 
           "inhibitor 1 metabolite" = Deets$Inhibitor1Metabolite, 
           "inhibitor 2" = Deets$Inhibitor2)
      
      # Finalizing, tidying, selecting only useful columns
      Data <- Data %>%
         mutate(File = sim_data_file,
                Compound = MyCompound[compoundID], 
                CompoundID = compoundID) %>%
         arrange(across(any_of(c("CompoundID", "Compound",
                                 "Individual", "Trial", "Time")))) %>%
         select(any_of(c("Compound", "CompoundID", 
                         "Individual", "Trial", "Time", "fu", "Time_units", 
                         "DoseNum", "File"))) %>% 
         purrr::discard(~all(is.na(.)))
      
      return(Data)
   }
   
   Out <- list()
   for(ff in sim_data_files){
      Out[[ff]] <- extract_fu_subfun(sim_data_file = ff)
   }
   
   Out <- bind_rows(Out)
   
   return(Out)
   
}


