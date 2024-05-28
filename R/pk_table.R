#' Title
#'
#' @param sim_data_files 
#' @param compoundsToExtract 
#' @param tissues 
#' @param PKparameters 
#' @param PKorder 
#' @param sheet_PKparameters 
#' @param existing_exp_details 
#' @param mean_type 
#' @param use_median_for_tmax 
#' @param includeCV 
#' @param includeSD 
#' @param includeConfInt 
#' @param includeMedian 
#' @param includeRange 
#' @param includePerc 
#' @param includeTrialMeans 
#' @param concatVariability 
#' @param variability_format 
#' @param convert_conc_units 
#' @param include_dose_num 
#' @param add_header_for_DDI 
#' @param rounding 
#' @param prettify_columns 
#' @param extract_forest_data 
#' @param checkDataSource 
#' @param highlight_gmr_colors 
#' @param highlight_so_cutoffs 
#' @param highlight_so_colors 
#' @param save_table 
#' @param single_table 
#' @param page_orientation 
#' @param fontsize 
#' @param ... 
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' # none yet
#' 
pk_table <- function(sim_data_files = NA, 
                     compoundsToExtract = "substrate",
                     tissues = "plasma", 
                     PKparameters = NA,
                     PKorder = "default", 
                     sheet_PKparameters = NA, 
                     existing_exp_details = NA, 
                     mean_type = NA, 
                     use_median_for_tmax = TRUE, 
                     includeCV = TRUE,
                     includeSD = FALSE,
                     includeConfInt = TRUE,
                     includeMedian = FALSE, 
                     includeRange = FALSE,
                     includePerc = FALSE, 
                     includeTrialMeans = FALSE, 
                     concatVariability = FALSE, 
                     variability_format = "to",
                     convert_conc_units = NA, 
                     include_dose_num = NA,
                     add_header_for_DDI = TRUE, 
                     rounding = NA,
                     prettify_columns = TRUE, 
                     prettify_compound_names = TRUE, 
                     extract_forest_data = FALSE, 
                     checkDataSource = TRUE, 
                     highlight_gmr_colors = NA, 
                     highlight_so_cutoffs = NA, 
                     highlight_so_colors = "yellow to red", 
                     save_table = NA, 
                     single_table = FALSE,
                     page_orientation = "portrait", 
                     fontsize = 11, 
                     ...){
   
   # Error catching ----------------------------------------------------------
   
   ## General error catching --------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Checking whether they've supplied pksummary_table args instead of
   # pksummary_mult args
   if("sim_data_file" %in% names(match.call()) &
      "sim_data_files" %in% names(match.call()) == FALSE){
      sim_data_files <- sys.call()$sim_data_file
   }
   
   if("compoundToExtract" %in% names(match.call()) &
      "compoundsToExtract" %in% names(match.call()) == FALSE){
      compoundsToExtract <- sys.call()$compoundToExtract
   }
   
   if("tissue" %in% names(match.call()) &
      "tissues" %in% names(match.call()) == FALSE){
      tissues <- sys.call()$tissue
   }
   
   
   ## Checking files and sheets -----------------------------------------------
   
   # Noting sim_data_files input for later and making sure that we're only
   # extracting each file once
   sim_data_files <- unique(sim_data_files)
   sim_data_files_input <- sim_data_files
   
   # Make sure file extension is xlsx. 
   sim_data_files <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$|\\.docx$|\\.db$", "", 
                                sim_data_files), ".xlsx")
   # Remove artifacts when there's no file specified. 
   sim_data_files <- setdiff(sim_data_files, "NA.xlsx")
   
   # If user did not supply files, then extract all the files in the current
   # folder that end in "xlsx" or in all subfolders if they wanted it to be
   # recursive.
   
   if(length(sim_data_files) == 1 &&
      (is.na(sim_data_files) | sim_data_files == "recursive")){
      sim_data_files <- list.files(pattern = "xlsx$",
                                   recursive = (complete.cases(sim_data_files) &&
                                                   sim_data_files == "recursive"))
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   }
   
   # Making sure that all the files exist before attempting to pull data
   if(all(complete.cases(sim_data_files)) && 
      any(file.exists(sim_data_files) == FALSE)){
      MissingSimFiles <- sim_data_files[
         which(file.exists(sim_data_files) == FALSE)]
      warning(paste0("The file(s) ", 
                     str_comma(paste0("`", MissingSimFiles, "`")), 
                     " is/are not present and thus will not be extracted.
                     "), 
              call. = FALSE)
      sim_data_files <- setdiff(sim_data_files, MissingSimFiles)
   }
   
   # sheet_PKparameters should be length 1 and not be named b/c, if they want
   # more than 1, they need to supply it to PKparameters.
   if(length(sheet_PKparameters) > 1){
      stop(str_wrap("The value for sheet_PKparameters must be only 1 item, and it looks like you have more than that. If you want to specify multiple sheets to use for PK parameters, please specify them by supplying a data.frame to the argument `PKparameters`. You can see examples for how to supply this by running `make_PK_example_input()`."), 
           call. = FALSE)
   }
   
   ## Checking CompoundID, Tissues --------------------------------------------
   
   # Check for appropriate input for compound ID
   compoundsToExtract <- tolower(compoundsToExtract)
   if(any(compoundsToExtract == "all")){
      compoundsToExtract <- AllCompounds$CompoundID
   }
   
   if(any(compoundsToExtract %in% AllCompounds$CompoundID == FALSE)){
      warning(paste0("The compound(s) ", 
                     str_comma(paste0("`", setdiff(compoundsToExtract, AllCompounds$CompoundID), "`")),
                     " is/are not among the possible componds to extract and will be ignored. The possible compounds to extract are only exactly these: ",
                     str_comma(paste0("`", AllCompounds$CompoundID, "`")), "
                     "), 
              call. = FALSE)
      compoundsToExtract <- intersect(compoundsToExtract, AllCompounds$CompoundID)
   }
   
   tissues <- tolower(tissues)
   PossTissues <- c("plasma", "unbound plasma", "blood", "unbound blood", 
                    "peripheral plasma", "peripheral blood")
   if(any(tissues %in% PossTissues == FALSE)){
      warning("You have not supplied a permissible value for tissue. Options are `plasma`, `unbound plasma`, `blood`, `unbound blood`, `peripheral plasma`, or `peripheral blood`. The PK parameters will be for plasma.\n", 
              call. = FALSE)
      tissues <- intersect(tissues, PossTissues)
   }
   
   ## Checking existing_exp_details ------------------------------------------
   
   # Getting experimental details for the simulation(s) as needed. For checking
   # on existing_exp_details, make sure that the order is 1) harmonize anything
   # that already exists and THEN 2) extract any missing info b/c this will make
   # sure that we get everything we might need. NB: "Deets" in all pksummary
   # functions means ONLY the experimental details for the single file in
   # question -- either the only file for pksummary_table or the specific file
   # we're dealing with in that iteration of the loop in pksummary_mult. By
   # contrast, existing_exp_details will include ALL experimental details
   # provided or extracted inside the function.
   if("logical" %in% class(existing_exp_details) == FALSE){ # logical when user has supplied NA
      existing_exp_details <- harmonize_details(existing_exp_details)
   }
   
   # This will get details for any files that weren't already included. 
   existing_exp_details <- extractExpDetails_mult(
      sim_data_files = sim_data_files,
      exp_details = "Summary and Input", 
      existing_exp_details = existing_exp_details)
   
   # extractExpDetails will check whether the Excel file provided was, in fact,
   # a Simulator output file and return a list of length 0 if not. Checking for
   # that here.
   if(any(sim_data_files %in% existing_exp_details$MainDetails$File)){
      
      NotSims <- setdiff(sim_data_files, existing_exp_details$MainDetails$File)
      
      warning(paste0("The file(s) ", str_comma(NotSims),
                     " is/are not Simulator output files and will be skipped.\n", call. = FALSE))
      sim_data_files <- intersect(sim_data_files, existing_exp_details$MainDetails$File)
   }
   
   if(any(sim_data_files %in% existing_exp_details$MainDetails$File[
      existing_exp_details$MainDetails$PopRepSim == "Yes"])){
      
      PopRepSims <- setdiff(sim_data_files, 
                            existing_exp_details$MainDetails$File[
                               existing_exp_details$MainDetails$PopRepSim == "Yes"])
      
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          PopRepSims, "omit")
      
      warning(paste0("The file(s) ", str_comma(PopRepSims),
                     " is/are population representative simulations and thus have no aggregate PK data. They will be skipped.\n", call. = FALSE))
      sim_data_files <- intersect(sim_data_files, existing_exp_details$MainDetails$File)
   }
   
   ## Harmonizing PKparameters -------------------------------------------------
   
   PKparameters <- tidy_input_PK(PKparameters = PKparameters, 
                                 sim_data_files = sim_data_files, 
                                 compoundsToExtract = compoundsToExtract, 
                                 sheet_PKparameters = sheet_PKparameters, 
                                 existing_exp_details = existing_exp_details)
   
   sim_data_files <- sort(unique(c(sim_data_files, 
                                   PKparameters$File)))
   
   ## Misc arg error catching -------------------------------------------------
   
   PKorder <- tolower(PKorder)
   if(PKorder %in% c("default", "user specified") == FALSE){
      warning("You have not supplied a permissible value for the order of PK parameters. Options are `default` or `user specified`. The default PK parameter order will be used.", 
              call. = FALSE)
      PKorder <- "default"
   }
   
   # Checking mean type input syntax
   if(complete.cases(mean_type)){
      if(mean_type %in% c("geometric", "arithmetic", 
                          "arithmetic for most, geometric for ratios") == FALSE){
         if(mean_type == "mean"){
            warning(paste0(str_wrap("Technically, the input for mean_type should be `geometric` (default), `arithmetic`, or `arithmetic for most, geometric for ratios`. You specified a mean type of `mean`, so we think you want arithmetic means. If that's incorrect, please set mean_type to `geometric`."),
                           "\n"), call. = FALSE)
         }
         
         mean_type <- case_when(str_detect(tolower(mean_type), "geo") & 
                                   !(str_detect(tolower(mean_type), "arith") &
                                        str_detect(tolower(mean_type), "geo")) ~ "geometric", 
                                mean_type == "mean" ~ "arithmetic", 
                                str_detect(tolower(mean_type), "arith") &
                                   str_detect(tolower(mean_type), "geo") ~ "arithmetic for most, geometric for ratios")
         
         if(mean_type %in% c("geometric", "arithmetic", 
                             "arithmetic for most, geometric for ratios") == FALSE){
            warning("You specified something other than `geometric` (default) `arithmetic`, or `arithmetic for most, geometric for ratios` for the mean type, so we're not sure what you would like. We'll use the default of geometric means.\n", 
                    call. = FALSE)
            
            mean_type <- "geometric"
         }
      }
      
   } else {
      mean_type <- "geometric"
   }
   
   # Now that mean type input is harmonized, set mean type for all scenarios. 
   MeanType <- 
      ifelse(mean_type %in% c("arithmetic", 
                              "arithmetic for most, geometric for ratios"),
             "arithmetic", "geometric")
   GMR_mean_type <- 
      ifelse(mean_type %in% c("geometric", 
                              "arithmetic for most, geometric for ratios"),
             "geometric", "arithmetic")
   
   # Make sure that input to variability_format is ok
   if(variability_format %in% c("to", "hyphen", "brackets", "parentheses") == FALSE){
      warning("The input for variability_format is not among the acceptable options, which are `to`, `hyphen`, `brackets` for square brackets, or `parentheses` for the eponymous symbol if you're an American and a bracket if you're British. We'll use the default of `to`.\n", 
              call. = FALSE)
      variability_format <- "to"
   }
   
   # Checking rounding
   rounding <- tolower(rounding[1])
   rounding <- sub("signif ", "significant ", rounding)
   rounding <- ifelse(is.na(rounding), "consultancy", rounding)
   if(str_detect(rounding, "consultancy|none|significant|round|word only") == FALSE){
      warning(paste0(str_wrap("You have entered something for the rounding argument other than the available options. We'll set this to the default, `Consultancy`. Please check the help file for details."), 
                     "\n"), call. = FALSE)
   }
   
   if(class(prettify_compound_names) == "character" &&
      is.null(names(prettify_compound_names))){
      warning("You have supplied values for `prettify_compound_names` but not assigned them with compound IDs. That means we don't know which one is the substrate and which one is the perpetrator(s). For now, we'll try our best to prettify the compound names, but if the result is not what you want, please supply a named character vector for what you want to use for the substrate and what you want to use for the perpetrator.", 
              call. = FALSE)
      prettify_compound_names <- TRUE
   }
   
   if(class(prettify_compound_names) == "character"){
      if(any(str_detect(names(prettify_compound_names), "inhibitor"))){
         names(prettify_compound_names)[
            which(str_detect(names(prettify_compound_names), "inhibitor"))] <- "perpetrator"
      }
      
      if("substrate" %in% names(prettify_compound_names) == FALSE){
         warning("The compound IDs you supplied for `prettify_compound_names` must include compound IDs of `substrate` and, if there are any perpetrators, `perpetrator` for the compounds to be prettified as requested. For now, we'll just try our best to prettify the compound names, but if the result is not what you want, please supply a named character vector for what you want to use for the substrate and what you want to use for the perpetrator.", 
                 call. = FALSE)
         prettify_compound_names <- TRUE
      }
   }
   
   if(complete.cases(highlight_gmr_colors) && 
      tolower(highlight_gmr_colors[1]) == "lisa"){highlight_gmr_colors = "traffic"}
   if(complete.cases(highlight_so_colors) &&
      tolower(highlight_so_colors[1]) == "lisa"){highlight_so_colors = "traffic"}
   
   if(any(complete.cases(highlight_gmr_colors)) &&
      highlight_gmr_colors[1] %in% c("yellow to red", "green to red", "traffic") == FALSE){
      if(length(highlight_gmr_colors) != 4){
         warning("We need 4 colors for highlighting geometric mean ratios, one each for negligible, weak, moderate, and strong interactions, and you have provided a different number of colors. We'll use yellow to red values for highlighting these.\n", 
                 call. = FALSE)
         highlight_gmr_colors <- "yellow to red"
      } else if(is.matrix(col2rgb(highlight_gmr_colors)) == FALSE){
         warning("The values you used for highlighting geometric mean ratios are not all valid colors in R. We'll used the default colors instead.\n", 
                 call. = FALSE)
         highlight_gmr_colors <- "yellow to red"
      } 
   }
   
   if(any(complete.cases(highlight_so_colors)) &&
      highlight_so_colors[1] %in% c("yellow to red", "green to red", "traffic") == FALSE &&
      is.matrix(col2rgb(highlight_so_colors)) == FALSE){
      warning("The values you used for highlighting S/O values are not all valid colors in R. We'll used the default colors instead.\n", 
              call. = FALSE)
      highlight_so_colors <- "yellow to red"
   } 
   
   # If they said "save_output" instead of "save_table", fix that.
   if("save_output" %in% names(match.call())){
      save_table <- sys.call()$save_output
   }
   
   page_orientation <- tolower(page_orientation)[1]
   if(page_orientation %in% c("portrait", "landscape") == FALSE){
      warning("You must specify `portrait` or `landscape` for the argument page_orientation, and you've specified something else. We'll use the default of `portrait`.\n", 
              call. = FALSE)
   }
   
   
   # Main body of function --------------------------------------------------
   
   ## Getting simulated data ------------------------------------------------
   MyPKResults <- list()
   PKpulled <- list()
   PKrequested <- list()
   OutQC <- list()
   FD <- list()
   CheckDoseInt <- list()
   
   PKparameters <- split(PKparameters, 
                         f = PKparameters$File)
   
   for(i in sim_data_files){
      
      MyPKResults[[i]] <- list()
      PKpulled[[i]] <- list()
      PKrequested[[i]] <- list()
      OutQC[[i]] <- list()
      FD[[i]] <- list()
      CheckDoseInt[[i]] <- list()
      
      message(paste0("Extracting PK data from `", i, "`"))
      
      Deets <- existing_exp_details$MainDetails %>% filter(File == i)
      
      # Discovery simulations have only 1 tissue. Need to adjust for that. NB: I
      # considered checking for this in the tidy_input_PK function, but it's
      # much easier to include it here since it's specific to each simulation
      # and also not something that often applies AND also may require
      # re-extracting details.
      if(Deets$SimulatorUsed == "Simcyp Discovery"){
         if("PKTissue_Discovery" %in% names(Deets) == FALSE){
            Deets <- extractExpDetails(sim_data_file = i, 
                                       exp_details = "Summary and Input")
            Deets <- Deets[["MainDetails"]]
         }
         
         PKparameters[[i]] <- PKparameters[[i]] %>% 
            filter(Tissue %in% Deets$PKTissue_Discovery)
      }
      
      PKparameters[[i]] <- split(PKparameters[[i]], 
                                 f = PKparameters[[i]]$CompoundID)
      
      for(j in names(PKparameters[[i]])){
         
         message(paste("     for compound =", j))
         
         MyPKResults[[i]][[j]] <- list()
         PKpulled[[i]][[j]] <- list()
         PKrequested[[i]][[j]] <- list()
         OutQC[[i]][[j]] <- list()
         FD[[i]][[j]] <- list()
         CheckDoseInt[[i]][[j]] <- list()
         
         PKparameters[[i]][[j]] <- split(PKparameters[[i]][[j]],
                                         f = PKparameters[[i]][[j]]$Tissue)
         
         for(k in names(PKparameters[[i]][[j]])){
            message(paste("          for tissue =", k))
            
            # Sheet is often NA, which messes up split. Temporarily filling in
            # NA values with "default".
            PKparameters[[i]][[j]][[k]]$Sheet[
               is.na(PKparameters[[i]][[j]][[k]]$Sheet)] <- "default"
            
            PKparameters[[i]][[j]][[k]] <- split(PKparameters[[i]][[j]][[k]], 
                                                 f = PKparameters[[i]][[j]][[k]]$Sheet)
            
            MyPKResults[[i]][[j]][[k]] <- list()
            PKpulled[[i]][[j]][[k]] <- list()
            PKrequested[[i]][[j]][[k]] <- list()
            OutQC[[i]][[j]][[k]] <- list()
            FD[[i]][[j]][[k]] <- list()
            CheckDoseInt[[i]][[j]][[k]] <- list()
            
            for(ss in names(PKparameters[[i]][[j]][[k]])){
               suppressWarnings(
                  temp <- 
                     pk_table_subfun(
                        sim_data_file = i, 
                        PKparameters = PKparameters[[i]][[j]][[k]][[ss]], 
                        existing_exp_details = existing_exp_details, 
                        convert_conc_units = convert_conc_units,
                        MeanType = MeanType, 
                        GMR_mean_type = GMR_mean_type, 
                        includeTrialMeans = includeTrialMeans, 
                        use_median_for_tmax = use_median_for_tmax)
               )
               
               if(length(temp) == 0){
                  warning(paste0(str_wrap(
                     paste0("There were no possible PK parameters to be extracted for the ",
                            j, " in ", k, " for the simulation `", i,
                            "` on the ", 
                            ifelse(is.na(ss), 
                                   "regular sheet for the 1st or last-dose PK", 
                                   paste0("sheet `", ss, "`")), 
                            ". Please check your input for 'PKparameters'. For example, check that you have not requested steady-state parameters for a single-dose simulation.")),
                     "\n"), call. = FALSE)
                  next
               }
               
               # Formatting to account for variability preferences
               VarOptions <- c("CV" = includeCV & MeanType == "arithmetic", 
                               "GCV" = includeCV & MeanType == "geometric",
                               "CI90_low" = includeConfInt,
                               "CI90_high" = includeConfInt, 
                               "CI95_low" = includeConfInt,
                               "CI95_high" = includeConfInt, 
                               "per5" = includePerc, 
                               "per95" = includePerc, 
                               "MinMean" = includeTrialMeans, 
                               "MaxMean" = includeTrialMeans, 
                               "min" = includeRange, 
                               "max" = includeRange, 
                               "SD" = includeSD, 
                               "median" = includeMedian)
               VarOptions <- names(VarOptions)[which(VarOptions)]
               VarOptions <- intersect(VarOptions, temp$PK$Stat)
               
               MyPKResults[[i]][[j]][[k]][[ss]] <- temp$PK %>%
                  filter(Stat %in% c(VarOptions, 
                                     switch(MeanType, 
                                            "geometric" = "geomean", 
                                            "arithmetic" = "mean")))
               
               PKpulled[[i]][[j]][[k]][[ss]] <-
                  data.frame(File = i, 
                             CompoundID = j, 
                             Tissue = k, 
                             PKpulled = temp$PKpulled, 
                             Sheet = ss)
               
               PKrequested[[i]][[j]][[k]][[ss]] <-
                  data.frame(File = i, 
                             CompoundID = j, 
                             Tissue = k, 
                             PKrequested = temp$PKrequested, 
                             Sheet = ss)
               
               if(checkDataSource){
                  OutQC[[i]][[j]][[k]][[ss]] <- temp$QC
               } 
               
               FD[[i]][[j]][[k]][[ss]] <- temp$ForestData
             
               CheckDoseInt[[i]][[j]][[k]][[s]] <- temp$CheckDoseInt
               
            }
            
            PKparameters[[i]][[j]][[k]] <- bind_rows(PKparameters[[i]][[j]][[k]])
            MyPKResults[[i]][[j]][[k]] <- bind_rows(MyPKResults[[i]][[j]][[k]])
            PKpulled[[i]][[j]][[k]] <- bind_rows(PKpulled[[i]][[j]][[k]])
            PKrequested[[i]][[j]][[k]] <- bind_rows(PKrequested[[i]][[j]][[k]])
            OutQC[[i]][[j]][[k]] <- bind_rows(OutQC[[i]][[j]][[k]])
            FD[[i]][[j]][[k]] <- bind_rows(FD[[i]][[j]][[k]])
            CheckDoseInt[[i]][[j]][[k]] <- bind_rows(CheckDoseInt[[i]][[j]][[k]])
         }
         
         PKparameters[[i]][[j]] <- bind_rows(PKparameters[[i]][[j]])
         MyPKResults[[i]][[j]] <- bind_rows(MyPKResults[[i]][[j]])
         PKpulled[[i]][[j]] <- bind_rows(PKpulled[[i]][[j]])
         PKrequested[[i]][[j]] <- bind_rows(PKrequested[[i]][[j]])
         OutQC[[i]][[j]] <- bind_rows(OutQC[[i]][[j]])
         FD[[i]][[j]] <- bind_rows(FD[[i]][[j]])
         CheckDoseInt[[i]][[j]] <- bind_rows(CheckDoseInt[[i]][[j]])
         
      }
      
      PKparameters[[i]] <- bind_rows(PKparameters[[i]])
      MyPKResults[[i]] <- bind_rows(MyPKResults[[i]])
      PKpulled[[i]] <- bind_rows(PKpulled[[i]])
      PKrequested[[i]] <- bind_rows(PKrequested[[i]])
      OutQC[[i]] <- bind_rows(OutQC[[i]])
      FD[[i]] <- bind_rows(FD[[i]])
      CheckDoseInt[[i]] <- bind_rows(CheckDoseInt[[i]])
      
   }
   
   if(length(MyPKResults) == 0){
      warning("No PK data could be found in the files ", 
              str_comma(paste0("`", sim_data_files, "`")), "\n",
              call. = FALSE)
      return(list())
   }
   
   PKparameters <- bind_rows(PKparameters) %>% 
      mutate(Sheet = ifelse(Sheet == "default", NA, Sheet))
   MyPKResults <- bind_rows(MyPKResults)
   PKpulled <- bind_rows(PKpulled)
   PKrequested <- bind_rows(PKrequested)
   OutQC <- bind_rows(OutQC)
   FD <- bind_rows(FD)
   CheckDoseInt <- bind_rows(CheckDoseInt)
   
   
   # Formatting --------------------------------------------------------------
   
   # Formatting and selecting only rows where there are data. Also removing any
   # PKparameters where we have only observed data, which can happen if user
   # specifies a PK parameter w/out a dose number but then does not specify a
   # custom interval tab. That messes up other formatting down the line
   # w/writing to Word.
   MyPKResults <- MyPKResults %>%
      mutate(Value = if_else(str_detect(Stat, "CV"), 
                             round_opt(100*Value, rounding),
                             round_opt(Value, rounding)))
   
   MyPKResults <- MyPKResults %>% 
      filter(Stat %in% c(case_match(MeanType, 
                                    "geometric" ~ "geomean",
                                    "arithmetic" ~ "mean", 
                                    "median" ~ "median"),
                         "CI90_low", "CI90_high", "CI95_low", "CI95_high",
                         "min", "max", "per5", "per95", 
                         case_match(MeanType, 
                                    "geometric" ~ "GCV",
                                    "arithmetic" ~ "CV", 
                                    "median" ~ NA),
                         "MinMean", "MaxMean", 
                         "S_O_TM_MinMean", "S_O_TM_MaxMean",
                         "S_O", "SD", "median"))
   
   # If there were any observed data that were a range, e.g., for tmax, then put
   # the range on a single line, even if user did not request concatVariability
   # b/c it just makes things so much easier.
   if(nrow(MyPKResults %>% filter(Stat == "min" & SorO == "Obs")) > 0){
      
      MyPKResults <- MyPKResults %>% 
         pivot_wider(names_from = Stat, 
                     values_from = Value) %>% 
         mutate(min = case_when(complete.cases(min) & 
                                   complete.cases(max) ~ 
                                   switch(variability_format, 
                                          "to" = paste(min, "to", max), 
                                          "hyphen" = paste(min, "-", max), 
                                          "brackets" = paste0("[", min, ", ", max, "]"),
                                          "parentheses" = paste0("(", min, ", ", max, ")")), 
                                TRUE ~ min)) %>% 
         select(-max) %>% 
         pivot_longer(cols = -c(PKParam, SorO), 
                      names_to = "Stat", 
                      values_to = "Value") %>% 
         mutate(Stat = ifelse(Stat == "min", 
                              switch(MeanType, 
                                     "geometric" = "GCV", 
                                     "arithmetic" = "CV"), 
                              Stat))
   }
   
   # Checking for any PK parameters where there are no simulated data.
   GoodPKParam <- MyPKResults %>% 
      filter(Stat == switch(MeanType, 
                            "geometric" = "geomean", 
                            "arithmetic" = "mean") &
                SorO == "Sim" &
                complete.cases(Value)) %>% pull(PKParam) %>% unique()
   
   MyPKResults <- MyPKResults %>%
      filter(PKParam %in% GoodPKParam & 
                complete.cases(Value)) %>% unique() %>% 
      pivot_wider(names_from = PKParam, values_from = Value) %>% 
      mutate(SorO = factor(SorO, levels = c("Sim", "Obs", "S_O", "S_O_TM")), 
             Stat = factor(Stat, levels = c("mean", "geomean", "median",
                                            "CV", "GCV", 
                                            "min", "max",
                                            "CI90_low", "CI90_high", "CI95_low", 
                                            "CI95_high", "per5", "per95",
                                            "MinMean", "MaxMean", 
                                            "SD", "S_O", 
                                            "S_O_TM_MinMean", 
                                            "S_O_TM_MaxMean"))) %>% 
      arrange(SorO, Stat) %>% 
      filter(if_any(.cols = -c(Stat, SorO), .fns = complete.cases)) %>% 
      mutate(across(.cols = everything(), .fns = as.character)) 
   
   rm(GoodPKParam)
   
   # Putting trial means into appropriate format
   if(includeTrialMeans){
      TM <- MyPKResults %>% 
         filter(Stat %in% c("MinMean", "MaxMean")) %>%
         summarize(across(.cols = -c(Stat, SorO),
                          .fns = function(x) {paste(x[1], "to", x[2])}))
      
      MyPKResults <- MyPKResults %>%
         filter(Stat != "MaxMean")
      
      MyPKResults[which(MyPKResults$Stat == "MinMean"), 
                  3:ncol(MyPKResults)] <- TM
      
      
      TM_SO <- MyPKResults %>% 
         filter(Stat %in% c("S_O_TM_MinMean", "S_O_TM_MaxMean")) %>%
         summarize(across(.cols = -c(Stat, SorO),
                          .fns = function(x) {paste(x[1], "to", x[2])}))
      
      if(nrow(TM_SO) > 0){
         MyPKResults <- MyPKResults %>%
            filter(Stat != "S_O_TM_MaxMean")
         
         MyPKResults[which(MyPKResults$Stat == "S_O_TM_MinMean"), 
                     3:ncol(MyPKResults)] <- TM_SO
      }
   }
   
   # Concatenating the rows w/lower and upper limits of variability when
   # requested
   if(concatVariability){
      
      # Note: When multiple options are chosen for variability type,
      # concatVariability doesn't work. Will need to fix this later.
      VarRows <- list("ConfInt90" = c("CI90_low", "CI90_high"), 
                      "ConfInt95" = c("CI95_low", "CI95_high"),
                      "Perc" = c("per5", "per95"), 
                      "Range" = c("min", "max"))
      VarRows <- VarRows[sapply(VarRows, function(x) unlist(x[[1]])) %in% VarOptions]
      
      VarRows[["obs"]] <- c("CIL_obs", "CIU_obs")
      for(j in names(VarRows)){
         temp <- MyPKResults %>%
            filter(Stat %in% as.character(unlist(VarRows[[j]]))) %>%
            mutate(across(.cols = !matches("Stat"),
                          .fns = function(x) {
                             ifelse(all(complete.cases(c(x[1], x[2]))),
                                    switch(variability_format, 
                                           "to" = paste(x[1], "to", x[2]),
                                           "hyphen" = paste(x[1], "-", x[2]),
                                           "brackets" = paste0("[", x[1], ", ", x[2], "]"), 
                                           "parentheses" = paste0("(", x[1], ", ", x[2], ")")),
                                    NA)}),
                   Stat = switch(j,
                                 "ConfInt90" = "CI90concat",
                                 "ConfInt95" = "CI95concat",
                                 "Perc" = "per95concat",
                                 "Range" = "Rangeconcat",
                                 "obs" = "CIobsconcat"))
         
         MyPKResults[which(MyPKResults$Stat == VarRows[[j]][1]), ] <-
            temp[1, ]
         MyPKResults <- MyPKResults %>% filter(Stat != VarRows[[j]][2])
         rm(temp)
      }
   }
   
   # Renaming statistics to match what's in template
   StatNames <- c("geomean" = "Simulated",
                  "mean" = "Simulated",
                  "GCV" = "CV%",
                  "CV" = "CV%",
                  "SD" = "Standard deviation",
                  "CI90_low" = "90% CI - Lower",
                  "CI90_high" = "90% CI - Upper",
                  "CI90concat" = "90% CI",
                  "CI95_low" = "95% CI - Lower",
                  "CI95_high" = "95% CI - Upper",
                  "CI95concat" = "95% CI",
                  "per5" = "5th Percentile",
                  "per95" = "95th Percentile",
                  "per95concat" = "5th to 95th Percentile",
                  "min" = "Minimum", 
                  "max" = "Maximum",
                  "median" = "Median",
                  "Rangeconcat" = "Range",
                  # "geomean_obs" = "Observed",
                  # "CV_obs" = "CV%",
                  # "CIL_obs" = "observed CI - Lower",
                  # "CIU_obs" = "observed CI - Upper",
                  # "CIobsconcat" = "Observed CI",
                  "S_O" = "S/O",
                  "S_O_TM_MinMean" = "S/O range for trial means",
                  "MinMean" = "Range of trial means")
   
   MyPKResults <- MyPKResults %>%
      mutate(Statistic = as.character(Stat),
             Statistic = StatNames[Statistic], 
             Statistic = ifelse(SorO == "Obs" & Statistic == "Simulated", 
                                "Observed", Statistic)) %>%
      select(-Stat) %>%
      select(Statistic, everything())
   
   # setting levels for PK parameters so that they're in a nice order. This
   # requires values for PKToPull, as does saving output to Word. PKToPull could
   # be empty depending on user input, so adjusting for that here.
   PKToPull <- names(MyPKResults)[
      names(MyPKResults) %in% c(AllPKParameters$PKparameter, 
                                sub("_dose1|_last", "", AllPKParameters$PKparameter))]
   
   PKlevels <- switch(PKorder, 
                      
                      # the default scenario
                      "default" =
                         bind_rows(AllPKParameters, 
                                   AllPKParameters %>% 
                                      mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
                                             SortOrder = SortOrder + 25)) %>% # This should work based on how I've got the SortOrder set up in AllPKParameters.
                         select(PKparameter, SortOrder) %>% 
                         arrange(SortOrder) %>%
                         pull(PKparameter) %>% unique(), 
                      
                      # user wants a specific order but using default tabs
                      "user specified" = PKparameters)
   
   # Checking for whether to include AUCinf, AUCt, or both for dose 1 based on
   # what user requested initially and whether there were any problems with
   # extrapolation. If there were problems with extrapolation for either
   # AUCinf_dose1 OR for AUCinf_dose1_withInhib, then we want only AUCt values
   # b/c need to be able to make the correct comparison. If there were no
   # problems, then we only want AUCinf values unless they speicifcally
   # requested AUCt.
   
   AUCParam <- PKToPull[str_detect(PKToPull, "AUC.*_dose1")]
   NonAUCParam <- setdiff(PKToPull, AUCParam)
   
   # Any time AUCinf_dose1 and AUCinf_dose1_withInhib are both included in
   # PKToPull, only retain any AUCt_X that were specfically requested.
   if("AUCinf_dose1" %in% PKToPull & 
      "AUCt_dose1" %in% PKrequested$PKrequested == FALSE){
      AUCParam <- setdiff(AUCParam, "AUCt_dose1")
   }
   
   if("AUCinf_dose1_withInhib" %in% PKToPull & 
      "AUCt_dose1_withInhib" %in% PKrequested$PKrequested == FALSE){
      AUCParam <- setdiff(AUCParam, "AUCt_dose1_withInhib")
   }
   
   if("AUCinf_ratio_dose1" %in% PKToPull & 
      "AUCt_ratio_dose1" %in% PKrequested$PKrequested == FALSE){
      AUCParam <- setdiff(AUCParam, "AUCt_ratio_dose1")
   }
   
   PKToPull <- c(AUCParam, NonAUCParam)
   PKToPull <- factor(PKToPull, levels = PKlevels)
   PKToPull <- sort(unique(PKToPull))
   
   # Getting columns in a good order
   MyPKResults <- MyPKResults %>%
      select(any_of(c("Statistic", as.character(PKToPull))))
   
   # Optionally adding final column names
   if(prettify_columns){
      
      # Checking position of columns with custom intervals.
      CustomIntCols <- which(
         PKToPull %in% sub("_dose1|_last", "", AllPKParameters$PKparameter))
      
      # If user specified tab, then need to adjust PK parameters here, too.
      if(any(complete.cases(PKparameters$Sheet)) & 
         any(str_detect(PKpulled$PKpulled, "_dose1|_last")) == FALSE){
         AllPKParameters_mod <- 
            AllPKParameters %>% select(PKparameter, PrettifiedNames) %>% 
            mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
                   PrettifiedNames = str_trim(sub("Last dose|Dose 1", "", 
                                                  PrettifiedNames))) %>% 
            unique()
         
         # We don't know whether an AUC was actually AUCtau, so make it AUCt.
         # First, though, if there are multiples of AUCt and AUCtau, remove the
         # AUCtau or we'll get replicate columns. Since the user specified a
         # sheet here, we do know that AUCt and AUCtau would be the same column
         # getting pulled twice. Not sure where I messed up that code, but this
         # fixes that problem.
         if(all(c("AUCtau", "AUCt") %in% PKToPull) |
            all(c("AUCtau_withInhib", "AUCt_withInhib") %in% PKToPull) |
            all(c("AUCtau_ratio", "AUCt_ratio") %in% PKToPull)){
            
            PKToPull <- PKToPull[!str_detect(PKToPull, "AUCtau")]
            MyPKResults <- MyPKResults %>% 
               select(!matches("AUCtau"))
         }
         
         PKToPull <- sub("AUCtau", "AUCt", PKToPull)
         
         suppressMessages(
            PrettyCol <- data.frame(PKparameter = PKToPull) %>% 
               left_join(AllPKParameters_mod) %>% 
               pull(PrettifiedNames)
         )
      } else {
         suppressMessages( 
            PrettyCol <- data.frame(PKparameter = PKToPull) %>% 
               left_join(
                  bind_rows(AllPKParameters, 
                            AllPKParameters %>% 
                               mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
                                      PrettifiedNames = sub("Dose 1 |Last dose ", "", PrettifiedNames))) %>% 
                     select(PKparameter, PrettifiedNames)) %>% 
               unique() %>% 
               pull(PrettifiedNames)
         )
      }
      
      # Adding time interval to any data that came from custom AUC interval
      # sheets.
      if(any(complete.cases(PKparameters$Sheet)) &
         length(MyPKResults_all$TimeInterval) > 0){
         IntToAdd <- MyPKResults_all$TimeInterval %>% 
            filter(Sheet %in% PKparameters$Sheet) %>% 
            pull(Interval)
         
         UnitsToAdd <- str_extract(PrettyCol[CustomIntCols], 
                                   " \\(h\\)| \\(ng/mL(.h)?\\)| \\(L/h\\)")
         UnitsToAdd[is.na(UnitsToAdd)] <- ""
         
         PrettyCol[CustomIntCols] <- 
            sub(" \\(h\\)| \\(ng/mL(.h)?\\)| \\(L/h\\)", "", PrettyCol[CustomIntCols])
         PrettyCol[CustomIntCols] <- paste0(PrettyCol[CustomIntCols], 
                                            " for interval ", IntToAdd, 
                                            UnitsToAdd)
      }
      
      # Adjusting units as needed.
      if("Units_AUC" %in% names(Deets) && complete.cases(Deets$Units_AUC)){
         PrettyCol <- sub("\\(ng/mL.h\\)", paste0("(", Deets$Units_AUC, ")"), PrettyCol)
      }
      if("Units_CL" %in% names(Deets) && complete.cases(Deets$Units_CL)){
         PrettyCol <- sub("\\(L/h\\)", paste0("(", Deets$Units_CL, ")"), PrettyCol)
      }
      if("Units_Cmax" %in% names(Deets) && complete.cases(Deets$Units_Cmax)){
         PrettyCol <- sub("\\(ng/mL\\)", paste0("(", Deets$Units_Cmax, ")"), PrettyCol)
      }
      if("Units_tmax" %in% names(Deets) && complete.cases(Deets$Units_tmax)){
         PrettyCol <- sub("\\(h\\)", paste0("(", Deets$Units_tmax, ")"), PrettyCol)
      }
      PrettyCol <- gsub("ug/mL", "µg/mL", PrettyCol)
      
      MyPerpetrator <- determine_myperpetrator(Deets, prettify_compound_names)
      
      if(any(complete.cases(MyPerpetrator))){
         PrettyCol <- sub("perpetrator", MyPerpetrator, PrettyCol)
      }
      
      names(MyPKResults) <- c("Statistic", PrettyCol)
      
      
   } else if(any(complete.cases(PKparameters$Sheet)) & 
             any(str_detect(PKpulled$PKpulled, "_dose1|_last")) == FALSE){
      # This is when it's a user-defined sheet but we're not prettifying column
      # names. We don't know whether an AUC was actually AUCtau, so make it
      # AUCt.
      PKToPull <- sub("AUCtau", "AUCt", PKToPull)
   }
   
   if(is.na(include_dose_num)){
      # Dropping dose number depending on input. First, checking whether they have
      # both dose 1 and last-dose data.
      DoseCheck <- c("first" = any(str_detect(names(MyPKResults), "Dose 1")), 
                     "last" = any(str_detect(names(MyPKResults), "Last dose")))
      
      # Next, checking whether they have a mix of custom AUC intervals and
      # regular b/c need to retain dose num in that case.
      if(any(PKparameters %in% 
             c(setdiff(unique(AllPKParameters$PKparameter_nodosenum), 
                       unique(AllPKParameters$PKparameter)), 
               setdiff(unique(AllPKParameters$PrettifiedNames_nodosenum), 
                       unique(AllPKParameters$PrettifiedNames))))){
         DoseCheck <- TRUE
      }
      
      include_dose_num <- all(DoseCheck)
   }
   
   # include_dose_num now should be either T or F no matter what, so checking
   # that.
   if(is.logical(include_dose_num) == FALSE){
      warning("Something is amiss with your input for `include_dose_num`, which should be NA, TRUE, or FALSE. We'll assume you meant for it to be TRUE.", 
              call. = FALSE)
      include_dose_num <- TRUE
   }
   
   if(include_dose_num == FALSE){
      names(MyPKResults) <- sub("Dose 1 |Last dose ", "", names(MyPKResults))
   }
   
   if(checkDataSource){
      
      ColsToInclude <- c("PKparam", "File", "Tab", 
                         switch(MeanType,
                                "arithmetic" = "mean",
                                "geometric" = "geomean"))
      
      if(length(PKToPull) > 0 && any(str_detect(PKToPull, "tmax"), na.rm = T)){
         ColsToInclude <- c(ColsToInclude, "min", "max", "median")
      }
      
      if(includeConfInt){
         ColsToInclude <- c(ColsToInclude, "CI90_low", "CI90_high")
      }
      
      if(includeCV){
         ColsToInclude <- c(ColsToInclude, 
                            switch(MeanType, 
                                   "arithmetic" = "CV", 
                                   "geometric" = "GCV"))
      }
      
      if(includePerc){
         ColsToInclude <- c(ColsToInclude, "per5", "per95")
      }
      
      if(includeRange){
         ColsToInclude <- c(ColsToInclude, "max", "min")
      }
      
      if(includeMedian){
         ColsToInclude <- c(ColsToInclude, "median")
      }
      
      if(includeSD){
         ColsToInclude <- c(ColsToInclude, "SD")
      }
      
      OutQC <- MyPKResults_all$QC %>% 
         select(PKparam, File, matches(ColsToInclude))
      
   }
   
   PKpulled <- PKToPull # Need to rename here for consistency w/other pksummary functions and Rmd files.
   
   # Saving --------------------------------------------------------------
   if(complete.cases(save_table)){
      
      # Checking whether they have specified just "docx" or just "csv" for
      # output b/c then, we'll use sim_data_file as file name. This allows us
      # to determine what the path should be, too, for either sim_data_file or
      # for some specified file name.
      if(str_detect(sub("\\.", "", save_table), "^docx$|^csv$")){
         OutPath <- dirname(sim_data_file)
         save_table <- sub("xlsx", 
                           # If they included "." at the beginning of the
                           # file exension, need to remove that here.
                           sub("\\.", "", save_table),
                           basename(sim_data_file))
      } else {
         # If they supplied something other than just "docx" or just "csv",
         # then check whether that file name is formatted appropriately.
         
         if(str_detect(basename(save_table), "\\..*")){
            if(str_detect(basename(save_table), "\\.docx") == FALSE){
               # If they specified a file extension that wasn't docx, make that
               # file extension be .csv
               save_table <- sub("\\..*", ".csv", save_table)
            }
         } else {
            # If they didn't specify a file extension at all, make it .csv. 
            save_table <- paste0(save_table, ".csv")
         }
         
         # Now that the file should have an appropriate extension, check what
         # the path and basename should be.
         OutPath <- dirname(save_table)
         save_table <- basename(save_table)
      }
      
      if(str_detect(save_table, "docx")){ 
         # This is when they want a Word file as output
         
         # May need to change the working directory temporarily, so
         # determining what it is now
         CurrDir <- getwd()
         
         OutPath <- dirname(save_table)
         if(OutPath == "."){
            OutPath <- getwd()
         }
         
         FileName <- basename(save_table)
         FromCalcPKRatios <- FALSE
         TemplatePath <- switch(page_orientation, 
                                "landscape" = system.file("Word/landscape_report_template.dotx",
                                                          package="SimcypConsultancy"), 
                                "portrait" = system.file("Word/report_template.dotx",
                                                         package="SimcypConsultancy"))
         
         rmarkdown::render(system.file("rmarkdown/templates/pk-summary-table/skeleton/skeleton.Rmd",
                                       package="SimcypConsultancy"), 
                           output_format = rmarkdown::word_document(reference_docx = TemplatePath), 
                           output_dir = OutPath, 
                           output_file = FileName, 
                           quiet = TRUE)
         # Note: The "system.file" part of the call means "go to where the
         # package is installed, search for the file listed, and return its
         # full path.
         
      } else {
         # This is when they want a .csv file as output. In this scenario,
         # changing the value "simulated" in the list of stats to include
         # whether it was arithmetic or geometric b/c that info is included
         # in the Word file but not in the table itself.
         MyPKResults <- MyPKResults %>% 
            mutate(Statistic = sub("Simulated", 
                                   paste("Simulated", MeanType, "mean"), Statistic))
         WarningDF <- data.frame(Col1 = "WARNING:",
                                 Col2 = "This table was saved to a csv file, and Excel automatically drops any trailing zeroes. Please check your sig figs to make sure you haven't inadvertently dropped a trailing zero.")
         names(WarningDF) <- names(MyPKResults)[1:2]
         
         write.csv(bind_rows(MyPKResults, WarningDF),
                   paste0(OutPath, "/", save_table), row.names = F)
      }
   }
   
   Out <- list("Table" = MyPKResults)
   
   if(checkDataSource){
      Out[["QC"]] <- OutQC
      
      if(complete.cases(save_table)){
         
         write.csv(OutQC, sub(".csv|.docx", " - QC.csv", save_table), row.names = F)
         
      }
   }
   
   if(extract_forest_data){
      Out[["ForestData"]] <- FD
      
      if(complete.cases(save_table)){ 
         write.csv(OutQC, sub(".csv|.docx", " - forest data.csv", save_table), row.names = F)
      }
   }
   
   if(return_PK_pulled){
      Out[["PKpulled"]] <- PKpulled
   }
   
   if(length(Out) == 1){
      Out <- Out[["Table"]]
   }
   
   if(CheckDoseInt$message == "mismatch" & any(str_detect(PKpulled, "_last"))){
      warning("The time used for integrating the AUC for the last dose was not the same as the dosing interval.\n", 
              call. = FALSE)
   }
   
   return(Out)
   
}



