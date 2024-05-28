#' Tidy input PK data as needed
#'
#' INTERNAL PACKAGE USE ONLY. Reads (as needed), harmonizes and tidies observed
#' PK input. Output is a data.frame of PK parameters with columns File, Sheet,
#' CompoundID, Tissue, Value, and Variability. The columns Sheet, Value, and
#' Variability may contain NAs.
#'
#' @param PKparameters whatever they have supplied for PKparameters for
#'   pksummary_table or pksummary_mult. This can be a csv file, a character
#'   vector, or NA.
#' @param report_input_file probably not in use
#' @param sheet_report probably not in use
#' @param sim_data_files a character vector of file names. This should no longer
#'   be NA or "recursive".
#' @param compoundsToExtract the values they supplied for compoundsToExtract in
#'   the parent function.
#' @param tissues the values they supplied for tissues in the parent function
#' @param sheet_PKparameters the values they supplied for sheet_PKparameters in
#'   the parent function
#'
#' @return a data.frame
#'
#' @examples
#' # nope 
tidy_input_PK <- function(PKparameters, 
                          sim_data_files = NA, 
                          compoundsToExtract = "substrate",
                          tissues = "plasma", 
                          sheet_PKparameters = NA,
                          report_input_file = NA, 
                          sheet_report = NA, 
                          existing_exp_details = NA){
   
   # Error catching ----------------------------------------------------------
   
   # Main body of function ---------------------------------------------------
   
   # Reading in any observed data, tidying those data, and harmonizing all the
   # possible places they could have specified which PK parameters they want and
   # which sheets they want those PK data to come from.
   
   ## OPTION: REPORT INPUT FILE ---------------------------------------------
   
   # Note: I don't think anyone is currently using this. Low priority for
   # debugging.
   
   if(complete.cases(report_input_file)){
      
      # If they didn't include ".xlsx" at the end of whatever they supplied for
      # report_input_file, add that.
      report_input_file <- ifelse(str_detect(report_input_file, "xlsx$"), 
                                  report_input_file, paste0(report_input_file, ".xlsx"))
      
      if(is.na(sheet_report)){
         warning("You must supply a value for `sheet_report` if you supply a report input file.", 
                 call. = FALSE)
         return(list())
      }
      
      sectionInfo <- getSectionInfo(report_input_file = report_input_file,
                                    sheet_report = sheet_report)
      
      if(complete.cases(sim_data_file) & sim_data_file != sectionInfo$File){
         warning(paste0("The value supplied for `sim_data_file` was `", 
                        sim_data_file, 
                        "``, but the value you supplied in the report input file `",
                        report_input_file, "` was `", 
                        sectionInfo$File,
                        "`. The file listed in the report input file will be used."), 
                 call. = FALSE)
      }
      
      sim_data_file <- sectionInfo$sim_data_file
      # Should we add an error catch here for when user fills out
      # report_input_file but doesn't include any observed data to compare?
      # Maybe not. If the user doesn't want to include any obs data there,
      # just fill out sim_data_file.
      
      # If they supplied both a report_input_file and PKparameters, warn the
      # user that this will preferentially read the report_input_file.
      if(complete.cases(PKparameters[1])){
         warning("You have supplied both a report input file and, separately, observed data. The report input file will be used preferentially and the observed data will be ignored.", 
                 call. = FALSE)
      }
      
      PKparameters <- as.data.frame(sectionInfo$ObsData)
      
   } 
   
   ## OPTION: CHARACTER VECTOR OF SOME KIND ------------------------------------
   
   # Could be a character vector or could be a file to read. Checking. 
   if(any(complete.cases(PKparameters)) && 
      "character" %in% class(PKparameters)){
      
      if(str_detect(PKparameters, "csv|xlsx")){
         
         PKparameters <- switch(str_extract(PKparameters, "csv|xlsx"), 
                                "csv" = read.csv(PKparameters, na.strings = "NA"), 
                                "xlsx" = xlsx::read.xlsx(PKparameters, 
                                                         sheetName = "observed PK"))
         
         # If there's anything named anything like "File", use that for the
         # "File" column. This is useful to deal with capitalization mismatches
         # and also because, if the user saves the file as certain kinds of csv
         # files, R has trouble importing and will add extra symbols to the 1st
         # column name.
         names(PKparameters)[str_detect(tolower(names(PKparameters)), "file")][1] <- 
            "File"
         
      } else {
         # This is when they have supplied a character vector of PK parameters. 
         PKparameters <- data.frame(
            File = sort(unique(sim_data_files)), 
            PKparameters = harmonize_PK_names(PKparameters))
      }
   } else if("data.frame" %in% class(PKparameters) == FALSE){
      # This is when PKparameters is NA. 
      PKparameters <- data.frame(PKparameter = PKparameters, 
                                 File = sim_data_files)
   }
   
   # PKparameters should now be a data.frame for all input options. 
   
   if("data.frame" %in% class(PKparameters) == FALSE){
      stop("Tell Laura Shireman that there's a problem with input for PKparameters.")
   }
   
   # Check whether data format matches template file Mackenzie and I have been
   # working on and harmonize column names as needed. 
   if(all(c("Cohort ID", 
            "Compound PK pertain to",
            "Tissue", 
            "Tab to get data from for a user-defined AUC interval", 
            "Concentration units",
            "AUC interval",
            "PK parameter", 
            "Aggregate value", 
            "Variability value", 
            "Summary statistic used for the aggregate value", 
            "Summary statistic used for the variability", 
            "PK parameter with dose number when applicable", 
            "PK parameter for R code",
            "Validation check - good PK value", 
            "Validation check - tab", 
            "Input validation check", 
            "QC initials", 
            "QC date", 
            "Simulation file",
            "Notes") %in% names(PKparameters))){
      
      PKparameters <- PKparameters %>% 
         rename(CompoundID = `Compound PK pertain to`, 
                CohortID = `Cohort ID`, 
                Value = `Aggregate value`, 
                Variability = `Variability value`, 
                # MeanType = `Summary statistic used for the aggregate value`, # <--- Not using these for now other than for QCing. 
                # VarType = `Summary statistic used for the variability`, 
                PKparameter = `PK parameter for R code`, 
                File = `Simulation file`, 
                Tab = `Tab to get data from for a user-defined AUC interval`) %>% 
         select(File, Tab, CohortID, Tissue, CompoundID, 
                PKparameter, Value, Variability)
      
      # Dealing w/possibly multiple files for the same cohort since user may
      # be making multiple comparisons. 
      suppressWarnings(
         Expand <- PKparameters %>% 
            select(CohortID, CompoundID, Tissue, File) %>% 
            mutate(File = str_split(File, pattern = ",( )?")) %>% 
            unnest(File) %>% 
            filter(complete.cases(File))
      )
      
      PKparameters <- PKparameters %>% 
         select(-File) %>% unique() %>% 
         left_join(Expand, 
                   by = c("CohortID", "Tissue", "CompoundID"),
                   relationship = "many-to-many") %>% 
         unique()
   }
   
   
   ## Checking for wide vs long input ---------------------------------------
   
   # Checking whether data in long or wide format. 
   Wide <- any(names(PKparameters) %in% 
                  c(AllPKParameters$PKparameter, 
                    tolower(AllPKParameters$PKparameter), 
                    AllPKParameters$PKparameter_nodosenum, 
                    tolower(AllPKParameters$PKparameter_nodosenum)))
   
   if(Wide){
      
      if("File" %in% names(PKparameters) == FALSE){
         
         if(nrow(PKparameters) == 1){
            # If there is only one value for each PK parameter, then use that
            # set of PK data to compare to ALL of the simulated data.
            PKparameters <- bind_cols(PKparameters, "File" = sim_data_files)
         } else {
            # If there is more than one value for each PK parameter, though,
            # then we don't know what to compare. Give an error message and
            # omit the S/O rows.
            warning(paste0(str_wrap(
               "When you supply a csv file or a data.frame for `PKparameters`, you must either include a column titled 'File' with the PK so that this function knows which simulator output files to use or you must submit only one set of PK parameters and we'll compare them to *all* the simulated files. We don't know what to compare here, so we will use all the PK parameters you listed for all the possible simulations and we will omit the observed data."), 
               "\n"), 
               call. = FALSE)
            
            # We can only use the 1st row here b/c it's not clear what they
            # want.
            PKparameters <- PKparameters[1, ]
         }
      }
      
      # At this point, they have a DF that is wide by the PK parameter and
      # they may or may not have specified sheet, tissue, etc. Reshaping. 
      PKparam_cols <- names(PKparameters)[
         names(PKparameters) %in% c(AllPKParameters$PKparameter, 
                                    tolower(AllPKParameters$PKparameter), 
                                    AllPKParameters$PKparameter_nodosenum, 
                                    tolower(AllPKParameters$PKparameter_nodosenum))]
      PKparameters <- PKparameters %>% 
         pivot_longer(cols = any_of(PKparam_cols), 
                      names_to = "PKparameter", 
                      values_to = "Value")
      
      # FIXME - Deal w/ instances when they have specified variability in the PK
      # parameter name, e.g. AUCinf_dose1__CV
      
      # PKparameters should now be long by PK parameter instead of wide. 
      
   } 
   
   ## Making names consistent ------------------------------------------------
   
   ### PKparameters ------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "pkparameter"] <- "PKparameter"
   if("PKparameter" %in% names(PKparameters) == FALSE &
      any(c("pkparam", "param", "parameter") %in%
          tolower(names(PKparameters)))){
      
      ColToUse <- which(tolower(names(PKparameters)) %in% 
                           c("pkparameter", "pkparam", "param", 
                             "parameter"))[1]
      
      warning(paste0("We were looking for a column named `PKparameter` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data in that column for the PK parameter name.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "PKparameter"
      rm(ColToUse)
   }
   
   names(PKparameters)[tolower(names(PKparameters)) == "value"] <- "Value"
   if("Value" %in% names(PKparameters) == FALSE &&
      any(c("geomean", "gm_mean", "gmean", "mean") %in%
          tolower(names(PKparameters)))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "geomean"), 
                    which(tolower(names(PKparameters)) == "gm_mean"), 
                    which(tolower(names(PKparameters)) == "gmean"), 
                    which(tolower(names(PKparameters)) == "mean"))[1]
      
      warning(paste0("We were looking for a column named `Value` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data in that column for any observed values.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "Value"
      rm(ColToUse)
   }
   
   
   
   ### Variability -------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "variability"] <- "Variability"
   if("Variability" %in% names(PKparameters) == FALSE &&
      (any(c("cv", "var", "sd", "range", "min", "minimum") %in%
           tolower(names(PKparameters))) |
       any(str_detect(tolower(names(PKparameters)), "conf")))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "var"), 
                    which(tolower(names(PKparameters)) == "cv"), 
                    which(tolower(names(PKparameters)) == "sd"), 
                    which(tolower(names(PKparameters)) == "range"), 
                    which(tolower(names(PKparameters)) == "minimum"), 
                    which(tolower(names(PKparameters)) == "min"), 
                    which(str_detect(tolower(names(PKparameters)), "conf")))[1]
      
      warning(paste0("We were looking for a column named `Variability` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data in that column for any observed variability.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "Variability"
      rm(ColToUse)
   }
   
   
   ### File -----------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "file"] <- "File"
   if(any(c("simulation", "workspace", "sim") %in% tolower(names(PKparameters)))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "simulation"), 
                    which(tolower(names(PKparameters)) == "workspace"), 
                    which(tolower(names(PKparameters)) == "sim"))[1]
      
      warning(paste0("We were looking for a column named `File` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data that column for the file names.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "File"
      rm(ColToUse)
   }
   
   if("File" %in% names(PKparameters) == FALSE){
      
      # If they've only supplied a single set of PK parameters -- i.e., all
      # values in PKparameters are unique -- then make all PK apply to all
      # files.
      if(any(duplicated(PKparameters$PKparameter)) == FALSE &
         ("File" %in% names(PKparameters) == FALSE ||
          all(is.na(PKparameters$File)))){
         
         PKparameters <- PKparameters %>% ungroup() %>% 
            select(-any_of("File")) %>% 
            left_join(expand.grid(File = sim_data_files, 
                                  PKparameter = PKparameters$PKparameter), 
                      by = "PKparameter")
      } else {
         stop(str_wrap("Something is not correct in how you have specified which simulation files we should use. Please check your input and the help file and try again."),
              call. = FALSE)
      }
   }
   
   # Make sure file extension is xlsx. 
   PKparameters$File <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$|\\.docx$|\\.db$", "", 
                                   PKparameters$File), ".xlsx")
   
   
   
   ### Sheet -------------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "sheet"] <- "Sheet"
   if(any(c("tab", "sheets", "sheet_pkparameter", "sheet_pkparameters") %in% 
          tolower(names(PKparameters)))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "tab"), 
                    which(tolower(names(PKparameters)) == "sheets"), 
                    which(tolower(names(PKparameters)) == "sheet_pkparameter"), 
                    which(tolower(names(PKparameters)) == "sheet_pkparameters"))[1]
      
      warning(paste0("We were looking for a column named `Sheet` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data that column for the sheet names for any user-defined intervals.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "Sheet"
      rm(ColToUse)
   }
   
   if("Sheet" %in% names(PKparameters) == FALSE){
      PKparameters$Sheet <- sheet_PKparameters
   }
   
   
   ### CompoundID --------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "compoundid"] <- "CompoundID"
   if("CompoundID" %in% names(PKparameters) == FALSE &&
      any(c("compound", "compounds", "compoundids", "cmpd", "drug") %in%
          tolower(names(PKparameters)))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "compoundids"), 
                    which(tolower(names(PKparameters)) == "compound"), 
                    which(tolower(names(PKparameters)) == "compounds"), 
                    which(tolower(names(PKparameters)) == "cmpd"), 
                    which(tolower(names(PKparameters)) == "drug"))[1]
      
      warning(paste0("We were looking for a column named `CompoundID` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data in that column for specifying which compound it is.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "CompoundID"
      rm(ColToUse)
   }
   
   if("CompoundID" %in% names(PKparameters) == FALSE){
      PKparameters$CompoundID <- compoundsToExtract
   }
   
   ### Tissue ------------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "tissue"] <- "Tissue"
   if("Tissue" %in% names(PKparameters) == FALSE &&
      any(c("tissues", "matrix") %in%
          tolower(names(PKparameters)))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "tissues"), 
                    which(tolower(names(PKparameters)) == "matrix"))[1]
      
      warning(paste0("We were looking for a column named `Tissue` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data in that column for specifying which tissue it is.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "Tissue"
      rm(ColToUse)
   }
   
   if("Tissue" %in% names(PKparameters) == FALSE){
      PKparameters$Tissue <- tissues
   }
   
   
   ## Checking inputs generally -----------------------------------------------
   
   # Specifying CompoundID and Tissue in ObsPK if possible
   if(("CompoundID" %in% names(PKparameters) == FALSE ||
       all(is.na(PKparameters$CompoundID)))){
      
      # Apply all combos of CompoundID to all rows.
      PKparameters <- PKparameters %>% ungroup() %>% 
         select(-any_of("CompoundID")) %>% 
         left_join(expand.grid(CompoundID = compoundsToExtract, 
                               PKparameter = PKparameters$PKparameter), 
                   by = "PKparameter")
      
   }
   
   # Setting any missing values to the default
   PKparameters$CompoundID[is.na(PKparameters$CompoundID)] <- "substrate"
   
   if(("Tissue" %in% names(PKparameters) == FALSE ||
       all(is.na(PKparameters$Tissue)))){
      
      # Apply all combosof Tissues to all rows.
      PKparameters <- PKparameters %>% ungroup() %>% 
         select(-any_of("Tissue")) %>% 
         left_join(expand.grid(Tissue = tissues, 
                               PKparameter = PKparameters$PKparameter), 
                   by = "PKparameter")
   }
   
   # Setting any missing values to the default
   PKparameters$Tissue[is.na(PKparameters$Tissue)] <- "plasma"
   
   # Sheet is a little different b/c they CANNOT enter more than one sheet for
   # sheet_PKparameters.
   if(("Sheet" %in% names(PKparameters) == FALSE ||
       all(is.na(PKparameters$Sheet))) &
      length(sort(unique(sheet_PKparameters))) == 1){
      PKparameters$Sheet <- sort(unique(Sheet))
   }
   
   # Checking that, when they've supplied a specific sheet, PKparameter does NOT
   # include the dose number. When they have *not* specified a tab, then it
   # should be a PKparameter w/the dose number or it should be something that
   # applies to the entire simulation.
   PKparameters <- PKparameters %>% 
      mutate(PKparameter = case_when(complete.cases(Sheet) ~ 
                                        sub("_dose1|_last", "", PKparameter), 
                                     TRUE ~ PKparameter), 
             PKparameter = harmonize_PK_names(PKparameter), 
             DoseNumProblem = PKparameter %in%
                AllPKParameters$PKparameter_nodosenum & is.na(Sheet))
   
   if(any(PKparameters$DoseNumProblem)){
      warning("You have not specified which interval you want for the following PK:\n", 
              call. = FALSE)
      
      Problem <- PKparameters %>% filter(DoseNumProblem == TRUE) %>% 
         select(File, CompoundID, Tissue, PKparameter)
      Problem <- capture.output(print(Problem, row.names = FALSE))
      
      message(str_c(Problem, collapse = "\n"))
      
      message(paste0(str_wrap("These PK parameters need to either end in `_dose1` or `_last` or something must be filled in for the sheet if it's a user-defined interval. These PK data will be omitted.\n"), 
                     "\n"), 
              call. = FALSE)
      
      PKparameters <- PKparameters %>% 
         filter(DoseNumProblem == FALSE) %>% select(-DoseNumProblem)
   }
   
   
   # At this point, PKparameters, if it exists, should be a data.frame b/c it
   # either was a data.frame at the outset, it has been created by reading an
   # Excel or csv file for observed data, or it came from a report input form.
   # At this point, it should NOT be wide by PKparameter. 
   
   
   ## Checking inputs specific to each sim ------------------------------------
   
   # For checking on existing_exp_details, make sure that the order is 1)
   # harmonize anything that already exists and THEN 2) extract any missing info
   # b/c this will make sure that we get everything we might need.
   if("logical" %in% class(existing_exp_details) == FALSE){ # logical when user has supplied NA
      existing_exp_details <- harmonize_details(existing_exp_details)
   }
   
   # This will get details for any files that weren't already included. 
   existing_exp_details <- extractExpDetails_mult(
      sim_data_files = PKparameters$File, # I think that, if they have supplied files in PKparameters, they will only want those files. 
      exp_details = "Summary and Input", 
      existing_exp_details = existing_exp_details)
   
   # Need to check that the compound they requested was included in the
   # simulation. Also check whether simulation was a DDI sim. 
   
   # First, adding any missing columns to existing_exp_details$MainDetails. 
   MissingCols <- setdiff(paste0("DoseInt", c("_sub", "_inhib", "_inhib2")), 
                          names(existing_exp_details$MainDetails)) 
   if(length(MissingCols) > 0){
      existing_exp_details$MainDetails <- 
         existing_exp_details$MainDetails %>% 
         bind_cols(as.data.frame(matrix(data = NA, 
                                        ncol = length(MissingCols),
                                        dimnames = list(NULL, MissingCols))))
   }
   
   PKparameters <- PKparameters %>% 
      left_join(existing_exp_details$MainDetails %>% 
                   select(File, any_of(AllCompounds$DetailNames), 
                          matches("DoseInt")), 
                by = "File") %>% 
      mutate(GoodCmpd = 
                (CompoundID == "substrate" & complete.cases(Substrate)) |
                (CompoundID == "inhibitor 1" & complete.cases(Inhibitor1)) |
                (CompoundID == "primary metabolite 1" & complete.cases(PrimaryMetabolite1)) |
                (CompoundID == "primary metabolite 2" & complete.cases(PrimaryMetabolite2)) |
                (CompoundID == "secondary metabolite" & complete.cases(SecondaryMetabolite)) |
                (CompoundID == "inhibitor 2" & complete.cases(Inhibitor2)) |
                (CompoundID == "inhibitor 1 metabolite" & complete.cases(Inhibitor1Metabolite)), 
             DDI = complete.cases(Inhibitor1), 
             MD = 
                (CompoundID %in% AllCompounds$CompoundID[
                   AllCompounds$DosedCompoundID == "substrate"] & 
                    complete.cases(DoseInt_sub)) |
                (CompoundID %in% AllCompounds$CompoundID[
                   AllCompounds$DosedCompoundID == "inhibitor 1"] & 
                    complete.cases(DoseInt_inhib)) |
                (CompoundID %in% AllCompounds$CompoundID[
                   AllCompounds$DosedCompoundID == "inhibitor 2"] & 
                    complete.cases(DoseInt_inhib2)))
   
   if(any(PKparameters$GoodCmpd == FALSE)){
      Problem <- PKparameters %>% filter(GoodCmpd == FALSE) %>% 
         select(File, CompoundID)
      Problem <- capture.output(print(Problem, row.names = FALSE))
      
      warning("The following simulation files do not contain the compound you requested PK for:\n",
              call. = FALSE)
      message(str_c(Problem, collapse = "\n"))
      message("They will be omitted.\n")
      
      PKparameters <- PKparameters %>% filter(GoodCmpd == TRUE)
   }
   
   PKparameters <- PKparameters %>% 
      select(File, Sheet, CompoundID, Tissue, PKparameter, 
             any_of(c("Value", "Variability")), 
             DDI, MD)
   
   # If they have left PKparameters as NA, then we need to figure out which PK
   # parameters apply.
   
   if(any(is.na(PKparameters$PKparameter))){
      
      MainPKParams <- AllPKParameters %>%
         # Per Hannah and template: Only include CL/F, t1/2, or tmax
         # if there's a specific reason to.
         filter(str_detect(PKparameter, "AUCinf_[^P]|AUCt|Cmax")) %>%
         filter(!str_detect(PKparameter, "_hepatic|CLpo")) %>% 
         select(PKparameter, AppliesToSingleDose, AppliesOnlyWhenPerpPresent) %>% 
         unique()
      
      MainPKParams <- MainPKParams %>% 
         left_join(expand_grid(File = unique(PKparameters$File), 
                               PKparameter = unique(MainPKParams$PKparameter)), 
                   by = "PKparameter")
      
      PKparameters <- PKparameters %>% 
         left_join(MainPKParams, by = "File")
      
   } else {
      PKparameters <- PKparameters %>% 
         left_join(bind_rows(
            AllPKParameters %>% 
               select(PKparameter, AppliesToSingleDose, 
                      AppliesOnlyWhenPerpPresent) %>% 
               unique(), 
            
            AllPKParameters %>% 
               select(PKparameter_nodosenum, AppliesToSingleDose, 
                      AppliesOnlyWhenPerpPresent) %>% 
               rename(PKparameter = PKparameter_nodosenum) %>% 
               unique()), 
            by = "PKparameter")
   }
   
   # Checking that we're looking for reasonable PK parameters. 
   PKparameters <- PKparameters %>% 
      mutate(HarmoniousDDI = AppliesOnlyWhenPerpPresent == FALSE |
                (AppliesOnlyWhenPerpPresent == TRUE & 
                    DDI == TRUE), 
             HarmoniousRegimen = AppliesToSingleDose == TRUE |
                (AppliesToSingleDose == FALSE & 
                    MD == TRUE), 
             Harmonious = HarmoniousDDI & HarmoniousRegimen) %>% 
      filter(Harmonious == TRUE) %>% 
      select(File, Sheet, CompoundID, Tissue, PKparameter, 
             any_of(c("Value", "Variability"))) %>% 
      unite(col = ID, c(File, Sheet, CompoundID, Tissue, PKparameter), 
            remove = FALSE)
   
   # If they requested AUCinf_dose1, then add AUCt_dose1 to the set of
   # parameters to pull in case of trouble with extrapolation.
   ToAdd <- PKparameters %>% filter(str_detect(PKparameter, "AUCinf")) %>% 
      mutate(PKparameter = sub("AUCinf", "AUCt", PKparameter), 
             Value = NA, 
             Variability = NA) %>% 
      unite(col = ID, c(File, Sheet, CompoundID, Tissue, PKparameter),
            remove = FALSE) %>% 
      filter(ID %in% PKparameters$ID == FALSE)
   
   PKparameters <- bind_rows(PKparameters, 
                             ToAdd) %>% 
      select(File, Sheet, CompoundID, Tissue, PKparameter, Value, Variability)
   
   # Out
   return(PKparameters)
   
}

