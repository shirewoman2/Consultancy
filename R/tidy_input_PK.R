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
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get all the details from the "Input
#'   Sheet" (e.g., when you ran extractExpDetails you said \code{exp_details =
#'   "Summary and Input"} or \code{exp_details = "all"}), you can save some processing
#'   time by supplying that object here, unquoted. If left as NA, this function
#'   will run \code{extractExpDetails} behind the scenes anyway to figure out
#'   some information about your experimental set up.
#' @param sim_data_files a character vector of file names. This should no longer
#'   be NA or "recursive".
#' @param compoundsToExtract the values they supplied for compoundsToExtract in
#'   the parent function.
#' @param tissues the values they supplied for tissues in the parent function
#' @param sheet_user_interval the values they supplied for sheet_user_interval in
#'   the parent function
#' @param sim_data_file_numerator sim file for the numerator. This ONLY comes
#'   from calc_PK_ratios and NOT calc_PK_ratios_mult and thus MUST have length
#'   1.
#' @param sim_data_file_denominator sim file for the denominator. This ONLY
#'   comes from calc_PK_ratios and NOT calc_PK_ratios_mult and thus MUST have
#'   length 1.
#'
#' @return a list of: 1) "PKparameters" -- a tidy data.frame of PKparameters
#'   with standardized column names and contents, 2) "existing_exp_details",
#'   which will only differ from the original in that any missing sim files will
#'   be added -- none will be removed. 3) "FilePairs" -- if this is from
#'   calc_PK_ratios, then a tidy data.frame listing which items should be paired
#'
#' @examples
#' # nope 
tidy_input_PK <- function(PKparameters, 
                          sim_data_files = NA, 
                          sim_data_file_numerator = NA, 
                          sim_data_file_denominator = NA, 
                          existing_exp_details = NA, 
                          compoundsToExtract = "substrate",
                          tissues = "plasma", 
                          sheet_user_interval = NA){
   
   # Reading in any observed data, tidying those data, and harmonizing all the
   # possible places they could have specified which PK parameters they want and
   # which sheets they want those PK data to come from.
   
   # NOTE: Two main options: 1) specify a data.frame or 2) specify individual PK
   # parameters. If they go with option 1, anything for arguments that would be
   # included in that data.frame will be ignored. If they go with option 2,
   # they'll get all possible combinations of files, compounds, tissues, etc.
   
   # Subfuns -----------------------------------------------------------------
   
   get_file_names <- function(sim_data_files){
      # Noting sim_data_files input for later and making sure that we're only
      # extracting each file once
      sim_data_files <- unique(sim_data_files)
      sim_data_files_input <- sim_data_files
      
      # Make sure file extension is xlsx or db.
      sim_data_files <- case_when(
         str_detect(sim_data_files, "xlsx$|db$") ~ sim_data_files, 
         .default = paste0(sub("\\.wksz$|\\.dscw$|\\.docx$", "", sim_data_files), 
                           ".xlsx"))
      
      # Remove artifacts when there's no file specified. 
      sim_data_files <- setdiff(sim_data_files, "NA.xlsx")
      if(length(sim_data_files) == 0){sim_data_files <- NA}
      
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
      
      return(sim_data_files)
   }
   
   
   # Setting up PKparameters data.frame ---------------------------------------
   
   # PKparameters could be a data.frame, character vector, or could be a file to
   # read. Checking.
   if(any(complete.cases(PKparameters)) && 
      "character" %in% class(PKparameters)){
      
      if(length(PKparameters) == 1 && 
         str_detect(PKparameters, "csv|xlsx")){
         
         InputWasDF <- TRUE
         
         PKparameters <- switch(
            str_extract(PKparameters, "csv|xlsx"), 
            "csv" = read.csv(PKparameters, na.strings = c("NA", "", "N/A", "na", "n/a")), 
            "xlsx" = tryCatch(
               openxlsx::read.xlsx(PKparameters, 
                                   sheet = "PKparameters"), 
               error = function(e) openxlsx::read.xlsx(PKparameters, 
                                                       sheet = "observed PK"))) %>% 
            mutate(across(.cols = everything(), 
                          .fns = \(x) case_when(x == " " ~ NA, 
                                                x == "" ~ NA, 
                                                is.null(x) ~ NA, 
                                                .default = x))) %>% 
            purrr::discard(\(x) all(is.na(x))) %>% 
            filter(!if_all(everything(), is.na))
         
         # If there's anything named anything like "File", use that for the
         # "File" column. This is useful to deal with capitalization mismatches
         # and also because, if the user saves the file as certain kinds of csv
         # files, R has trouble importing and will add extra symbols to the 1st
         # column name. Note that this does NOT change the name if it's a
         # calc_PK_ratios set of parameters.
         names(PKparameters)[
            which(str_detect(tolower(names(PKparameters)), "file") &
                     !str_detect(tolower(names(PKparameters)), "num|denom"))][1] <- 
            "File"
         
      } else {
         # This is when they have supplied a character vector of PK parameters. 
         PKparameters <- data.frame(PKparameter = harmonize_PK_names(PKparameters))
         
         InputWasDF <- FALSE
      }
   } else if("data.frame" %in% class(PKparameters)){
      if(nrow(PKparameters) == 0){
         InputWasDF <- FALSE
         PKparameters <- data.frame(PKparameter = NA)
      } else {
         # This is when they have supplied a data.frame
         InputWasDF <- TRUE
      }
      
   } else {
      # This is when PKparameters is NA. 
      PKparameters <- data.frame(PKparameter = harmonize_PK_names(PKparameters))
      
      InputWasDF <- FALSE
   }
   
   # PKparameters should now be a data.frame for all input options. 
   
   if("data.frame" %in% class(PKparameters) == FALSE){
      stop("Tell Laura Shireman that there's a problem with input for PKparameters.")
   }
   
   # Dealing with situation in which user supplied a data.frame that they
   # probably read themselves and includes "" instead of NA
   if(nrow(PKparameters) > 0){
      PKparameters <- PKparameters %>% 
         mutate(across(.cols = everything(), 
                       .fns = \(x) case_when(x == "" ~ NA, 
                                             .default = x)))
   }
   
   # Dealing with possible input from calc_PK_ratios
   FromCalcPKRatios <- 
      any(str_detect(tolower(names(PKparameters)), "numerator")) | 
      any(str_detect(tolower(names(PKparameters)), "denominator")) |
      ("PKparameter" %in% names(PKparameters) && 
          any(str_detect(PKparameters$PKparameter, "/"), na.rm = T)) |
      "NorD" %in% names(PKparameters) |
      any(complete.cases(sim_data_file_numerator)) |
      any(complete.cases(sim_data_file_denominator))
   
   if(FromCalcPKRatios){
      
      # Tidying column names
      
      # File
      names(PKparameters)[
         str_detect(tolower(names(PKparameters)), 
                    "^numerator$|numerator.*file|file.*numerator|numerator.*sim|sim.*numerator")][1] <- "Numerator_File"
      
      names(PKparameters)[
         str_detect(tolower(names(PKparameters)), 
                    "^denominator$|denominator.*file|file.*denominator|denominator.*sim|sim.*denominator")][1] <- "Denominator_File"
      
      if(all(c("Numerator_File", "Denominator_File") %in%
             names(PKparameters) == FALSE) & 
         "File" %in% names(PKparameters)){
         PKparameters <- PKparameters %>% 
            mutate(Numerator_File = File, 
                   Denominator_Tissue = File) %>% 
            select(-File)
      }
      
      if(all(c("Numerator_File", "Denominator_File") %in%
             names(PKparameters) == FALSE)){
         
         # If they did not include Num or Denom files in PKparameters, then it
         # must be b/c this is getting called from calc_PK_ratios (not
         # calc_PK_ratios_mult) and each of sim_data_file_numerator and
         # sim_data_file_denominator will have length 1, so it's safe to add
         # them to the data as a single value.
         
         if(length(sim_data_file_denominator) != 1 |
            length(sim_data_file_numerator) != 1){
            stop(wrapn("We're having trouble figuring out which PK parameters match which simulation data files for the numerator and denominator. Please use a data.frame for the argument 'PKparameters' that includes what the numerator and denominator simulation data files should be rather than specifying them with 'sim_data_file_numerator' and 'sim_data_file_denominator'. If you're uncertain how to set this up, please run 'make_example_PK_input()' for help."), 
                 call. = FALSE)
         }
         
         PKparameters <- PKparameters %>% 
            mutate(Numerator_File = sim_data_file_numerator,
                   Denominator_File = sim_data_file_denominator)
      }
      
      # CompoundID
      names(PKparameters)[
         str_detect(tolower(names(PKparameters)), 
                    "numerator.*compoundid|compoundid.*numerator")][1] <- "Numerator_CompoundID"
      
      names(PKparameters)[
         str_detect(tolower(names(PKparameters)), 
                    "denominator.*compoundid|compoundid.*denominator")][1] <- "Denominator_CompoundID"
      
      if(all(c("Numerator_CompoundID", "Denominator_CompoundID") %in%
             names(PKparameters) == FALSE) & 
         "CompoundID" %in% names(PKparameters)){
         PKparameters <- PKparameters %>% 
            mutate(Numerator_CompoundID = CompoundID, 
                   Denominator_Tissue = CompoundID) %>% 
            select(-CompoundID)
      }
      
      if("Numerator_CompoundID" %in% names(PKparameters) == FALSE){
         PKparameters <- PKparameters %>% 
            left_join(expand_grid(Numerator_File = unique(PKparameters$Numerator_File), 
                                  Numerator_CompoundID = compoundsToExtract), 
                      by = "Numerator_File") %>% 
            mutate(Numerator_CompoundID = case_when(is.na(Numerator_CompoundID) ~ "substrate", 
                                                    .default = Numerator_CompoundID))
      }
      
      if("Denominator_CompoundID" %in% names(PKparameters) == FALSE){
         PKparameters <- PKparameters %>% 
            left_join(expand_grid(Denominator_File = unique(PKparameters$Denominator_File), 
                                  Denominator_CompoundID = compoundsToExtract), 
                      by = "Denominator_File") %>% 
            mutate(Denominator_CompoundID = case_when(is.na(Denominator_CompoundID) ~ "substrate", 
                                                      .default = Denominator_CompoundID))
      }
      
      # Tissue
      names(PKparameters)[
         str_detect(tolower(names(PKparameters)), 
                    "numerator.*tissue|tissue.*numerator")][1] <- "Numerator_Tissue"
      
      names(PKparameters)[
         str_detect(tolower(names(PKparameters)), 
                    "denominator.*tissue|tissue.*denominator")][1] <- "Denominator_Tissue"
      
      if(all(c("Numerator_Tissue", "Denominator_Tissue") %in%
             names(PKparameters) == FALSE) & 
         "Tissue" %in% names(PKparameters)){
         PKparameters <- PKparameters %>% 
            mutate(Numerator_Tissue = Tissue, 
                   Denominator_Tissue = Tissue) %>% 
            select(-Tissue)
      }
      
      if("Numerator_Tissue" %in% names(PKparameters) == FALSE){
         PKparameters <- PKparameters %>% 
            left_join(expand_grid(Numerator_File = unique(PKparameters$Numerator_File), 
                                  Numerator_Tissue = tissues), 
                      by = "Numerator_File") %>% 
            mutate(Numerator_Tissue = case_when(is.na(Numerator_Tissue) ~ "plasma", 
                                                .default = Numerator_Tissue))
      }
      
      if("Denominator_Tissue" %in% names(PKparameters) == FALSE){
         PKparameters <- PKparameters %>% 
            left_join(expand_grid(Denominator_File = unique(PKparameters$Denominator_File), 
                                  Denominator_Tissue = tissues), 
                      by = "Denominator_File") %>% 
            mutate(Denominator_Tissue = case_when(is.na(Denominator_Tissue) ~ "plasma", 
                                                  .default = Denominator_Tissue))
      }
      
      # Sheet
      names(PKparameters)[
         str_detect(tolower(names(PKparameters)), 
                    "numerator.*sheet|sheet.*numerator|numerator.*tab|tab.*numerator")][1] <- "Numerator_Sheet"
      
      names(PKparameters)[
         str_detect(tolower(names(PKparameters)), 
                    "denominator.*sheet|sheet.*denominator|denominator.*tab|tab.*denominator")][1] <- "Denominator_Sheet"
      
      if(all(c("Numerator_Sheet", "Denominator_Sheet") %in%
             names(PKparameters) == FALSE) & 
         "Sheet" %in% names(PKparameters)){
         PKparameters <- PKparameters %>% 
            mutate(Numerator_Sheet = Sheet, 
                   Denominator_Tissue = Sheet) %>% 
            select(-Sheet)
      }
      
      if("Numerator_Sheet" %in% names(PKparameters) == FALSE){
         PKparameters$Numerator_Sheet <- NA
      }
      
      if("Denominator_Sheet" %in% names(PKparameters) == FALSE){
         PKparameters$Denominator_Sheet <- NA
      }
      
      if(all(is.na(PKparameters$Numerator_Sheet)) & 
         all(is.na(PKparameters$Denominator_Sheet)) & 
         complete.cases(sheet_user_interval)){
         PKparameters$Numerator_Sheet <- sheet_user_interval
         PKparameters$Denominator_Sheet <- sheet_user_interval
      }
      
      # PKparameter
      names(PKparameters)[
         str_detect(tolower(names(PKparameters)), 
                    "num(erator)?.*param(eter)?|param(eter)?.*num(erator)?")][1] <- "Numerator_PKparameter"
      
      names(PKparameters)[
         str_detect(tolower(names(PKparameters)), 
                    "denom(inator)?.*param(eter)?|param(eter)?.*denom(inator)?")][1] <- "Denominator_PKparameter"
      
      names(PKparameters)[str_detect(tolower(names(PKparameters)), 
                                     "^pk.*param(eter)$")][1] <- "PKparameter"
      
      if("PKparameter" %in% names(PKparameters) && 
         any(str_detect(PKparameters$PKparameter, "/"), na.rm = T)){
         
         PKparameters <- PKparameters %>% 
            separate_wider_delim(
               cols = PKparameter, 
               delim = " / ", 
               names = c("Numerator_PKparameter", "Denominator_PKparameter"))
         
      }
      
      if(all(c("Numerator_PKparameter", "Denominator_PKparameter") %in%
             names(PKparameters) == FALSE) & 
         "PKparameter" %in% names(PKparameters)){
         PKparameters <- PKparameters %>% 
            mutate(Numerator_PKparameter = PKparameter, 
                   Denominator_PKparameter = PKparameter) %>% 
            select(-PKparameter)
      }
      
      if("Numerator_PKparameter" %in% names(PKparameters) == FALSE){
         PKparameters$Numerator_PKparameter <- NA
      }
      if("Denominator_PKparameter" %in% names(PKparameters) == FALSE){
         PKparameters$Denominator_PKparameter <- NA
      }
      
      # At this point, it should be safe to remove PKparameter and rely on
      # Denominator_PKparameter and Numerator_PKparameter only.
      PKparameters$PKparameter <- NULL 
      
      # Noting original file pairs, adding missing required columns, and
      # reshaping data to work w/rest of function
      PKparameters <- PKparameters %>% 
         mutate(FilePair = paste(Numerator_File, "/", Denominator_File))
      
      FilePairs <- PKparameters
      
      PKparameters <- bind_rows(
         PKparameters %>% 
            select(FilePair, matches("Numerator")) %>% 
            rename_with(.fn = function(.) sub("Numerator_", "", .)) %>% 
            mutate(NorD = "Numerator"), 
         
         PKparameters %>% 
            select(FilePair, matches("Denominator")) %>% 
            rename_with(.fn = function(.) sub("Denominator_", "", .)) %>% 
            mutate(NorD = "Denominator"))
      
      # Setting InputWasDF to TRUE no matter what it was originally b/c we don't
      # want any further data tidying/clearning/harmonizing to happen.
      InputWasDF <- TRUE
      
   } else {
      FilePairs <- NA
   }
   
   # Tidying and harmonizing when input was DF ------------------------------
   
   if(InputWasDF){
      
      ## Harmonizing data format and col names ----------------------------------
      
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
                   ObsValue = `Aggregate value`, 
                   ObsVariability = `Variability value`, 
                   # MeanType = `Summary statistic used for the aggregate value`, # <--- Not using these for now other than for QCing. 
                   # VarType = `Summary statistic used for the variability`, 
                   PKparameter = `PK parameter for R code`, 
                   File = `Simulation file`, 
                   Tab = `Tab to get data from for a user-defined AUC interval`) %>% 
            select(File, Tab, CohortID, Tissue, CompoundID, 
                   PKparameter, ObsValue, ObsVariability)
         
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
      
      # Checking whether data in long or wide format. 
      Wide <- prettify_column_names(names(PKparameters),
                                    return_which_are_PK = TRUE)
      Wide <- any(Wide$IsPKParam)
      
      if(Wide){
         
         if("File" %in% names(PKparameters) == FALSE & 
            nrow(PKparameters) > 1){
            
            # If there is more than one value for each PK parameter, then we
            # don't know what to compare. Give an error message and omit the S/O
            # rows.
            warning(paste0(str_wrap(
               "When you supply a csv file or a data.frame for `PKparameters`, you must either include a column titled 'File' with the PK so that this function knows which simulator output files to use or you must submit only one set of PK parameters and we'll compare them to *all* the simulated files. We don't know what to compare here, so we will use all the PK parameters you listed for all the possible simulations and we will omit any observed data."), 
               "\n"), 
               call. = FALSE)
            
            # We can only use the 1st row here b/c it's not clear what they
            # want.
            PKparameters <- PKparameters[1, ]
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
                         values_to = "ObsValue")
      }
      
      # PKparameters should now be long by PK parameter instead of wide, but the
      # column names might be incorrect. Fixing that next. 
      
      # FIXME - I took most of this section and make a standalone function
      # tidy_PKparameters_names for fixing issues with column names. At some
      # point, go back and replace most of this next section with that function.
      
      
      ### PKparameters ------------------------------------------------------------
      
      names(PKparameters)[tolower(names(PKparameters)) == "pkparameter"] <- "PKparameter"
      if("PKparameter" %in% names(PKparameters) == FALSE &
         any(c("pkparam", "param", "parameter", "pkparameters") %in%
             tolower(names(PKparameters)))){
         
         ColToUse <- which(tolower(names(PKparameters)) %in% 
                              c("pkparameter", "pkparameters", "pkparam", "param", 
                                "parameter"))[1]
         
         warning(wrapn(paste0("We were looking for a column named `PKparameter` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                              names(PKparameters)[ColToUse],
                              "`, which we think is what you might want. We'll use the data in that column for the PK parameter name.")), 
                 call. = FALSE)
         
         names(PKparameters)[ColToUse] <- "PKparameter"
         rm(ColToUse)
      } 
      
      if("PKparameter" %in% names(PKparameters) == FALSE){
         # If they didn't have a column PKparameter, then they want the standard
         # PK parameters, so set this to NA for now.
         PKparameters$PKparameter <- NA
      }
      
      # Noting original value for PKparameters
      PKparameters_orig <- unique(PKparameters$PKparameter)
      
      
      ### File -----------------------------------------------------------------
      
      names(PKparameters)[tolower(names(PKparameters)) == "file"] <- "File"
      if(any(c("simulation", "workspace", "sim") %in% tolower(names(PKparameters)))){
         
         ColToUse <- c(which(tolower(names(PKparameters)) == "simulation"), 
                       which(tolower(names(PKparameters)) == "workspace"), 
                       which(tolower(names(PKparameters)) == "sim"))[1]
         
         warning(wrapn(paste0("We were looking for a column named `File` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                              names(PKparameters)[ColToUse],
                              "`, which we think is what you might want. We'll use the data that column for the file names.")), 
                 call. = FALSE)
         
         names(PKparameters)[ColToUse] <- "File"
         rm(ColToUse)
      }
      
      if("File" %in% names(PKparameters) &&
         (any(complete.cases(PKparameters$File)) &
          any(is.na(PKparameters$File)))){
         stop(str_wrap("You have supplied a set of PK parameters to extract where you sometimes have listed the simulation file name and sometimes have not, so we don't know which simultaion files you want. Please check your input and try again."), 
              call. = FALSE)
      }
      
      # If all values for File are NA, remove this column and then re-add it in
      # the next tidying step.
      if("File" %in% names(PKparameters) && all(is.na(PKparameters$File))){
         PKparameters$File <- NULL
      }
      
      if("File" %in% names(PKparameters) == FALSE){
         
         # If they haven't included "File" column, assume they want all
         # possible combinations of files. 
         suppressMessages(
            PKparameters <- PKparameters %>% 
               left_join(expand_grid(File = get_file_names(sim_data_files), 
                                     PKparameter = unique(PKparameters$PKparameter)))
         )
      }
      
      # FIXME - Deal w/ instances when they have specified variability in the PK
      # parameter name, e.g. AUCinf_dose1__CV
      
      ### ObsValue ------------------------------------------------------------
      
      names(PKparameters)[tolower(names(PKparameters)) == "obsvalue"] <- "ObsValue"
      if("ObsValue" %in% names(PKparameters) == FALSE &&
         any(c("geomean", "gm_mean", "gmean", "mean", "value") %in%
             tolower(names(PKparameters)))){
         
         ColToUse <- c(which(tolower(names(PKparameters)) == "value"), 
                       which(tolower(names(PKparameters)) == "geomean"), 
                       which(tolower(names(PKparameters)) == "gm_mean"), 
                       which(tolower(names(PKparameters)) == "gmean"), 
                       which(tolower(names(PKparameters)) == "mean"))[1]
         
         warning(wrapn(paste0("We were looking for a column named `ObsValue` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                              names(PKparameters)[ColToUse],
                              "`, which we think is what you might want. We'll use the data in that column for any observed values.")), 
                 call. = FALSE)
         
         names(PKparameters)[ColToUse] <- "ObsValue"
         rm(ColToUse)
      } 
      
      if("ObsValue" %in% names(PKparameters) == FALSE){
         PKparameters$ObsValue <- NA
      }
      
      
      ### ObsVariability -------------------------------------------------------------
      
      names(PKparameters)[tolower(names(PKparameters)) == "obsvariability"] <- "ObsVariability"
      if("ObsVariability" %in% names(PKparameters) == FALSE &&
         (any(c("variability", "cv", "var", "sd", "range", "min", "minimum") %in%
              tolower(names(PKparameters))) |
          any(str_detect(tolower(names(PKparameters)), "conf")))){
         
         ColToUse <- c(which(tolower(names(PKparameters)) == "variability"), 
                       which(tolower(names(PKparameters)) == "var"), 
                       which(tolower(names(PKparameters)) == "cv"), 
                       which(tolower(names(PKparameters)) == "sd"), 
                       which(tolower(names(PKparameters)) == "range"), 
                       which(tolower(names(PKparameters)) == "minimum"), 
                       which(tolower(names(PKparameters)) == "min"), 
                       which(str_detect(tolower(names(PKparameters)), "conf")))[1]
         
         warning(wrapn(paste0("We were looking for a column named `ObsVariability` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                              names(PKparameters)[ColToUse],
                              "`, which we think is what you might want. We'll use the data in that column for any observed variability.")), 
                 call. = FALSE)
         
         names(PKparameters)[ColToUse] <- "ObsVariability"
         rm(ColToUse)
      } 
      
      if("ObsVariability" %in% names(PKparameters) == FALSE){
         PKparameters$ObsVariability <- NA
      }
      
      # This needs to be character for possibly combining downstream w/other
      # character variability s/a "1 to 2"
      PKparameters$ObsVariability <- as.character(PKparameters$ObsVariability)
      
      
      ### Sheet -------------------------------------------------------------------
      
      # NB: Sheet needs to be different b/c we do NOT want to use grid.expand on
      # Sheet. Input must be either in the data.frame already OR must be length
      # 1.
      
      names(PKparameters)[tolower(names(PKparameters)) == "sheet"] <- "Sheet"
      
      if("Sheet" %in% names(PKparameters) == FALSE){
         if("useraucsheet" %in% tolower(names(PKparameters))){
            names(PKparameters)[tolower(names(PKparameters)) == "useraucsheet"] <- "Sheet"
         } else if("customaucsheet" %in% tolower(names(PKparameters))){
            names(PKparameters)[tolower(names(PKparameters)) == "customaucsheet"] <- "Sheet"
         }
      }
      
      if("Sheet" %in% names(PKparameters) == FALSE & 
         any(c("tab", "sheets", "sheet_pkparameter", "sheet_user_interval") %in% 
             tolower(names(PKparameters)))){
         
         ColToUse <- c(which(tolower(names(PKparameters)) == "tab"), 
                       which(tolower(names(PKparameters)) == "sheets"), 
                       which(tolower(names(PKparameters)) == "sheet_pkparameter"), 
                       which(tolower(names(PKparameters)) == "sheet_user_interval"))[1]
         
         warning(wrapn(paste0("We were looking for a column named `UserAUCSheet` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                              names(PKparameters)[ColToUse],
                              "`, which we think is what you might want. We'll use that column to determine the sheet names for any user-defined intervals.")), 
                 call. = FALSE)
         
         names(PKparameters)[ColToUse] <- "Sheet"
         rm(ColToUse)
      }
      
      if("Sheet" %in% names(PKparameters) == FALSE){
         # This is ok that it's set to sheet_user_interval b/c we have already
         # checked that sheet_user_interval has length 1. 
         PKparameters$Sheet <- sheet_user_interval
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
         
         warning(wrapn(paste0("We were looking for a column named `CompoundID` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                              names(PKparameters)[ColToUse],
                              "`, which we think is what you might want. We'll use the data in that column for specifying which compound it is.")), 
                 call. = FALSE)
         
         names(PKparameters)[ColToUse] <- "CompoundID"
         rm(ColToUse)
      }
      
      if("CompoundID" %in% names(PKparameters) == FALSE){
         
         # Check for appropriate input for compound ID
         compoundsToExtract <- tolower(compoundsToExtract)
         if(any(compoundsToExtract == "all", na.rm = T)){
            
            Cmpd_all <- 
               existing_exp_details$MainDetails[, c("File", AllRegCompounds$DetailNames)] %>% 
               pivot_longer(cols = -File, 
                            names_to = "DetailNames", 
                            values_to = "ObsValue") %>% 
               left_join(AllRegCompounds %>% select(CompoundID, DetailNames), 
                         by = "DetailNames")
            
            compoundsToExtract <- AllRegCompounds$CompoundID
         }
         
         PKparameters <- PKparameters %>% 
            left_join(expand_grid(File = unique(PKparameters$File), 
                                  CompoundID = compoundsToExtract), 
                      by = "File")
      }
      
      ### Tissue ------------------------------------------------------------------
      
      names(PKparameters)[tolower(names(PKparameters)) == "tissue"] <- "Tissue"
      if("Tissue" %in% names(PKparameters) == FALSE &&
         any(c("tissues", "matrix") %in%
             tolower(names(PKparameters)))){
         
         ColToUse <- c(which(tolower(names(PKparameters)) == "tissues"), 
                       which(tolower(names(PKparameters)) == "matrix"))[1]
         
         warning(wrapn(paste0("We were looking for a column named `Tissue` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                              names(PKparameters)[ColToUse],
                              "`, which we think is what you might want. We'll use the data in that column for specifying which tissue it is.")), 
                 call. = FALSE)
         
         names(PKparameters)[ColToUse] <- "Tissue"
         rm(ColToUse)
      }
      
      if("File" %in% names(PKparameters) & 
         "Tissue" %in% names(PKparameters) == FALSE){
         
         PKparameters <- PKparameters %>% 
            left_join(expand_grid(File = unique(PKparameters$File), 
                                  Tissue = tissues), 
                      by = "File")
      }
   } 
   
   
   # Tidying and harmonizing when input was NOT a DF ---------------------------
   
   if(InputWasDF == FALSE){
      
      # At this point, PKparameters is a single column data.frame where the only
      # column is titled "PKparameter" or it is a data.frame with two columns of
      # PK parameters for the numerator and denominator sims.
      
      # Noting original value for PKparameters
      if(any(str_detect(names(PKparameters), "Numerator|Denominator"))){
         PKparameters_orig <- unique(c(PKparameters$Numerator_PKparameter, 
                                       PKparameters$Denominator_PKparameter))
      } else {
         PKparameters_orig <- PKparameters$PKparameter
      }
      
      ## File -----------------------------------------------------------------
      
      # Getting all possible files
      AllFiles <- get_file_names(sim_data_files)
      PKparameters <- expand_grid(PKparameter = unique(PKparameters$PKparameter), 
                                  File = AllFiles)
      
      ## Checking sheets -----------------------------------------------
      
      # sheet_user_interval should be length 1 and not be named b/c, if they want
      # more than 1, they need to supply it to PKparameters.
      if(length(sheet_user_interval) > 1){
         stop(str_wrap("The value for sheet_user_interval must be only 1 item, and it looks like you have more than that. If you want to specify multiple sheets to use for PK parameters, please specify them by supplying a data.frame to the argument `PKparameters`. You can see examples for how to supply this by running `make_PK_example_input()`."), 
              call. = FALSE)
      }
      # Note to self: this check is also present at the top of pk_table.
      # Probably not needed.
      
      PKparameters$Sheet <- sheet_user_interval
      
      ## Tissue -----------------------------------------------------------
      
      suppressMessages(
         PKparameters <- PKparameters %>% 
            left_join(expand_grid(File = AllFiles, 
                                  Tissue = tissues))
      )
      
      ## ObsValue and variability -------------------------------------------
      
      # Adding these columns so that format will be the same regardless of input
      # scenario.
      PKparameters$ObsValue <- NA
      
      # This needs to be character for possibly combining downstream w/other
      # character varability s/a "1 to 2"
      PKparameters$ObsVariability <- as.character(NA)
      
   } 
   
   # Harmonizing and tidying generally ----------------------------------------
   
   # Format of PKparameters at this point: data.frame with columns File, Sheet,
   # CompoundID, Tissue. Columns may contain NA values and the exact content of
   # the columns may not be correct, but they should exist. 
   
   # Trimming white space
   PKparameters <- PKparameters %>% 
      mutate(across(.cols = where(is.character), 
                    .fns = str_trim))
   
   # Harmonizing content of columns next.
   
   ## File ------------------------------------------------------------------
   
   # Separating file names if there are more than one. 
   PKparameters <- PKparameters %>% 
      mutate(MultFiles = str_detect(File, ","))
   
   FilesToExpand <- PKparameters %>% filter(MultFiles == TRUE)
   if(nrow(FilesToExpand) > 0){
      
      FilesToExpand <- split(FilesToExpand, f = FilesToExpand$File)
      
      for(ff in names(FilesToExpand)){
         FileNames <- data.frame(File = ff, 
                                 NewFile = str_split(string = ff, 
                                                     pattern = ",( )?", 
                                                     simplify = TRUE) %>% 
                                    as.character())
         
         FilesToExpand[[ff]] <- FilesToExpand[[ff]] %>% 
            left_join(FileNames, by = "File", relationship = "many-to-many") %>% 
            select(-File) %>% 
            rename(File = NewFile)
         
         rm(FileNames)
      }
      
      FilesToExpand <- bind_rows(FilesToExpand)
      
      PKparameters <- PKparameters %>% 
         filter(MultFiles == FALSE) %>% 
         bind_rows(FilesToExpand) %>% 
         select(-MultFiles)
   }
   
   # If user specified values for sim_data_files, then remove any others.
   if(all(complete.cases(sim_data_files))){
      TEMP <- PKparameters %>% 
         filter(File %in% sim_data_files)
      
      if(nrow(TEMP) == 0){
         warning(wrapn("None of the simulation files you specified for the argument 'sim_data_files' were present in the column 'File' in the data you supplied for the argument 'PKparameters'. We don't know which simulation files you want, so we will return PK for all the possible simulation files."), 
                 call. = FALSE)
      } else {
         PKparameters <- TEMP
      }
   }
   
   
   ## Sheet ------------------------------------------------------------------
   
   # Checking sheet input. Things that should happen with the sheet: 
   
   # 1. If PKparameter applies to all doses, Sheet should be NA. 
   
   # 2. If PKparameter applies to a specific dose and they have listed _dose1 or
   # _last on the PK parameter, Sheet should be NA.
   
   # 3. If PKparameter applies to a specific user-defined interval and they thus
   # have NOT listed _dose1 or _last on the PK parameter, they SHOULD have
   # supplied a specific sheet.
   
   # 4. If the sheet they supplied includes something indicating that it's a 1st
   # or last dose sheet, then remove the sheet and make sure that the
   # PKparameter has _dose1 or _last on it.
   
   PKparameters <- PKparameters %>% 
      mutate(
         # Not giving a warning when they request AUCinf w/out including _dose1
         # and just fixing this b/c it's clear which dose it's for.
         PKparameter = case_when(
            PKparameter == "AUCinf" ~ "AUCinf_dose1", 
            PKparameter == "AUCinf_withInhib" ~ "AUCinf_dose1_withInhib", 
            PKparameter == "AUCinf_ratio" ~ "AUCinf_ratio_dose1", 
            .default = PKparameter), 
         
         ShouldListSheet = 
            case_when(
               
               PKparameter %in% AllPKParameters$PKparameter[
                  AllPKParameters$AppliesToAllDoses == TRUE] ~ FALSE, 
               
               is.na(PKparameter) ~ FALSE, 
               
               str_detect(PKparameter, "_dose1|_last") ~ FALSE, 
               
               .default = TRUE), 
         
         ShouldListSheetButDidnt = ShouldListSheet == TRUE & is.na(Sheet), 
         
         Sheet4Dose1_CC = str_detect(Sheet, "1st") & 
            complete.cases(Sheet), 
         
         Sheet4Last_CC = str_detect(Sheet, "last") & complete.cases(Sheet), 
         
         PKparam_missing_dose1 = case_when(
            Sheet4Dose1_CC & complete.cases(Sheet) & 
               !str_detect(PKparameter, "_dose1") ~ TRUE, 
            .default = FALSE), 
         
         PKparam_missing_last = Sheet4Last_CC & !str_detect(PKparameter, "_last"), 
         
         # Just fixing things when they have listed 1st or last dose sheet but
         # shouldn't have. 
         PKparameter = case_when(
            PKparam_missing_dose1 ~ paste0(PKparameter, "_dose1"), 
            PKparam_missing_last ~ paste0(PKparameter, "_last"), 
            .default = PKparameter), 
         Sheet = case_when(Sheet4Dose1_CC == TRUE ~ NA, 
                           Sheet4Last_CC == TRUE ~ NA, 
                           .default = Sheet), 
      )
   
   
   if(any(PKparameters$ShouldListSheetButDidnt, na.rm = T)){
      warning(wrapn("It looks like you would like PK data from a user-defined AUC interval because you did not include '_dose1' or '_last' in the PK parameter name for some PK parameters but also did not list which sheet to use for that user-defined interval. We know which sheet to use for the first dose and which to use for the last dose, but we only know which to use for a user-defined AUC interval when you tell us. We will have to ignore any parameters that look like they must be for a custom interval that don't have a sheet included."), 
              call. = FALSE)
      
      PKparameters <- PKparameters %>% filter(!ShouldListSheetButDidnt == TRUE)
      
   }
   
   
   ## PKparameter -----------------------------------------------------------
   
   # Harmonizing
   PKparameters$PKparameter <- harmonize_PK_names(PKparameters$PKparameter)
   
   BadParams <- setdiff(PKparameters$PKparameter, 
                        c(AllPKParameters$PKparameter, 
                          AllPKParameters$PKparameter_nodosenum))
   
   if(length(BadParams) > 0 &&
      any(complete.cases(BadParams))){
      warning(paste0(wrapn("The following PK parameters are not among the possible options and will be ignored:"), 
                     str_c(BadParams, collapse = "\n"), "\n"), 
              call. = FALSE)
      PKparameters <- PKparameters %>% 
         filter(PKparameter %in% BadParams == FALSE)
   }
   
   # If they asked for AUCinf, also give them AUCt_dose1 in case of trouble with
   # extrapolation. Need to note when they originally requested only
   # AUCinf_dose1 b/c removing AUCt_dose1 if there were no issues with
   # extrpolating. Note that this ONLY matters for AUCinf_dose1 and AUCt_dose1!
   # This is imperfect b/c it can still sometimes pick up AUCtau when the person
   # originally called it AUCt, but it's the best I've come up with so far.
   PKparameters <- PKparameters %>% 
      mutate(
         Complete = complete.cases(PKparameter), 
         OriginallyRequested = tolower(PKparameter) %in% 
            tolower(harmonize_PK_names(PKparameters_orig)), 
         OriginallyRequested = case_when(
            str_detect(PKparameter, "AUCt_") ~ OriginallyRequested, 
            .default = Complete), 
         OriginallyRequested = Complete & OriginallyRequested) %>% 
      select(-Complete)
   
   if(any(str_detect(PKparameters$PKparameter, "AUCinf"), na.rm = T)){
      
      # Get all possible AUCinf parameters and their matching AUCt parameter
      ToAdd <- PKparameters %>% filter(str_detect(PKparameter, "AUCinf")) %>% 
         mutate(PKparameter = str_replace(PKparameter, "AUCinf", "AUCt"), 
                # Have to remove obs values or everything gets replicated
                ObsValue = NA, ObsVariability = NA) %>% 
         filter(PKparameter %in% PKparameters$PKparameter == FALSE) %>% 
         mutate(OriginallyRequested = FALSE)
      
      if(nrow(ToAdd) > 0){
         PKparameters <- bind_rows(PKparameters, ToAdd)
      }
      rm(ToAdd)
   }
   
   
   ## Tissue ---------------------------------------------------------------
   
   if("Tissue" %in% names(PKparameters)){
      PKparameters$Tissue <- tolower(PKparameters$Tissue)
   } else {
      PKparameters$Tissue <- NA
   }
   PKparameters$Tissue[is.na(PKparameters$Tissue)] <- "plasma" # NB: This will be fine for parameters that don't actually have a tissue s/a fa b/c it will get ignored.
   
   PossTissues <- c("plasma", "unbound plasma", "blood", "unbound blood", 
                    "peripheral plasma", "peripheral blood")
   if(any(PKparameters$Tissue %in% PossTissues == FALSE)){
      warning("You have not supplied a permissible value for tissue. Options are `plasma`, `unbound plasma`, `blood`, `unbound blood`, `peripheral plasma`, or `peripheral blood`. The PK parameters will be for plasma.\n", 
              call. = FALSE)
      PKparameters$Tissue[PKparameters$Tissue %in% PossTissues == FALSE] <- "plasma"
      PKparameters <- unique(PKparameters)
   }
   
   
   ## CompoundID -------------------------------------------------------------
   
   if("CompoundID" %in% names(PKparameters)){
      PKparameters$CompoundID <- tolower(PKparameters$CompoundID)
      
   } else if(any(complete.cases(compoundsToExtract))){
      PKparameters <- PKparameters %>% 
         left_join(expand_grid(File = unique(PKparameters$File), 
                               CompoundID = compoundsToExtract), 
                   by = "File")
      
   } else {
      PKparameters$CompoundID <- NA
   }
   
   PKparameters$CompoundID[is.na(PKparameters$CompoundID)] <- "substrate"
   
   if(any(complete.cases(compoundsToExtract)) && 
      any(compoundsToExtract[complete.cases(compoundsToExtract)] %in% 
          AllRegCompounds$CompoundID == FALSE, na.rm = T)){
      warning(paste0(str_wrap(paste0(
         "The compound(s) ", 
         str_comma(paste0("`", setdiff(compoundsToExtract, AllRegCompounds$CompoundID), "`")),
         " is/are not among the possible componds to extract and will be ignored. The possible compounds to extract are only exactly these: ",
         str_comma(paste0("`", AllRegCompounds$CompoundID, "`")))), "\n"), 
         call. = FALSE)
      compoundsToExtract <- intersect(compoundsToExtract, AllRegCompounds$CoampoundID)
   }
   
   PKparameters <- PKparameters %>% 
      filter(CompoundID %in% AllRegCompounds$CompoundID)
   
   
   # Checking inputs specific to each sim ------------------------------------
   
   # Noting when parameters only apply to SD or only apply to DDI
   PKparameters <- PKparameters %>% 
      left_join(
         AllPKParameters %>% 
            select(PKparameter,
                   AppliesToSingleDose,
                   AppliesOnlyWhenPerpPresent, 
                   UserInterval) %>% unique(), 
         by = "PKparameter") 
   
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
   if("logical" %in% class(existing_exp_details)){
      # logical when user has supplied NA
      existing_exp_details <- extractExpDetails_mult(
         sim_data_files = sort(unique(c(PKparameters$File, sim_data_files))))
   } else { 
      existing_exp_details <- harmonize_details(existing_exp_details)
   }
   
   # This will get details for any files that weren't already included. 
   if(any(PKparameters$File %in% existing_exp_details$MainDetails$File == FALSE)){
      suppressWarnings(
         existing_exp_details <- extractExpDetails_mult(
            sim_data_files = sort(unique(c(PKparameters$File, sim_data_files))),
            exp_details = "Summary and Input", 
            existing_exp_details = existing_exp_details)
      )
   }
   
   # extractExpDetails will check whether the Excel file provided was, in fact,
   # a Simulator output file and return a list of length 0 if not. Checking for
   # that here.
   if(any(PKparameters$File %in% existing_exp_details$MainDetails$File == FALSE)){
      
      ProbFiles <- setdiff(PKparameters$File, existing_exp_details$MainDetails$File)
      MissingFiles <- ProbFiles[which(ProbFiles %in% list.files() == FALSE)]
      NotSims <- setdiff(ProbFiles, MissingFiles)
      
      if(length(MissingFiles) > 0){
         warning(paste0("The following simulation files are not present and will be ignored:\n", 
                        str_c(paste0("  ", MissingFiles), collapse = "\n")), 
                 call. = FALSE)
      }
      
      if(length(NotSims) > 0){
         warning(paste0("The following files do not appear to be Simcyp Simulator files and will be ignored:\n", 
                        str_c(paste0("  ", NotSims), collapse = "\n")), 
                 call. = FALSE)
      }
      
      PKparameters <- PKparameters %>% filter(!File %in% ProbFiles)
   }
   
   if(any(PKparameters$File %in% existing_exp_details$MainDetails$File[
      existing_exp_details$MainDetails$PopRepSim == "Yes"])){
      
      PopRepSims <- setdiff(PKparameters$File, 
                            existing_exp_details$MainDetails$File[
                               existing_exp_details$MainDetails$PopRepSim == "Yes"])
      
      warning(paste0(wrapn("The following files are population representative simulations and thus have no aggregate PK data. They will be skipped."), 
                     str_c(paste0("  ", PopRepSims), collapse = "\n")), 
              call. = FALSE)
      
      PKparameters <- PKparameters %>% filter(!File %in% PopRepSims)
   }
   
   ## Checking CompoundID -----------------------------------------------------
   
   # Check for appropriate input for compound ID
   compoundsToExtract <- tolower(compoundsToExtract)
   if(any(compoundsToExtract == "all", na.rm = T)){
      
      Cmpd_all <- 
         existing_exp_details$MainDetails[, c("File", AllRegCompounds$DetailNames)] %>% 
         pivot_longer(cols = -File, 
                      names_to = "DetailNames", 
                      values_to = "ObsValue") %>% 
         left_join(AllRegCompounds %>% select(CompoundID, DetailNames), 
                   by = "DetailNames")
      
      compoundsToExtract <- AllRegCompounds$CompoundID
   }
   
   if(any(compoundsToExtract[complete.cases(compoundsToExtract)] %in%
          AllRegCompounds$CompoundID == FALSE)){
      warning(paste0("The compound(s) ", 
                     str_comma(paste0("`", setdiff(compoundsToExtract, AllRegCompounds$CompoundID), "`")),
                     " is/are not among the possible componds to extract and will be ignored. The possible compounds to extract are only exactly these: ",
                     str_comma(paste0("`", AllRegCompounds$CompoundID, "`")), "
                     "), 
              call. = FALSE)
      compoundsToExtract <- intersect(compoundsToExtract, AllRegCompounds$CompoundID)
   }
   
   if(InputWasDF){
      # Getting all possible files
      AllFiles <- get_file_names(PKparameters$File)
   }
   
   suppressMessages(
      PKparameters <- PKparameters %>% 
         left_join(expand_grid(File = AllFiles, 
                               CompoundID = compoundsToExtract))
   )
   
   # Need to check that the compound they requested was included in the
   # simulation. Also check whether simulation was a DDI sim. 
   
   # First, adding any missing columns to existing_exp_details$MainDetails. 
   MissingCols <- setdiff(paste0("Regimen", c("_sub", "_inhib", "_inhib2")), 
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
                   select(File, any_of(AllRegCompounds$DetailNames), 
                          matches("Regimen")), 
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
                (CompoundID %in% AllRegCompounds$CompoundID[
                   AllRegCompounds$DosedCompoundID == "substrate"] & 
                    Regimen_sub == "Multiple Dose") |
                (CompoundID %in% AllRegCompounds$CompoundID[
                   AllRegCompounds$DosedCompoundID == "inhibitor 1"] & 
                    Regimen_inhib == "Multiple Dose") |
                (CompoundID %in% AllRegCompounds$CompoundID[
                   AllRegCompounds$DosedCompoundID == "inhibitor 2"] & 
                    Regimen_inhib2 == "Multiple Dose"))
   
   if(any(PKparameters$GoodCmpd == FALSE)){
      Problem <- PKparameters %>%
         filter(GoodCmpd == FALSE) %>% 
         select(File, CompoundID) %>% 
         as.data.frame()
      
      Problem <- capture.output(print(Problem, row.names = FALSE))
      
      message("Warning:\nThe following simulation files do not contain the compound for which you requested PK:\n")
      message(str_c(Problem, collapse = "\n"))
      message("\nThey will be omitted.\n")
      
      PKparameters <- PKparameters %>% filter(GoodCmpd == TRUE)
   }
   
   PKparameters <- PKparameters %>% 
      select(File, Sheet, CompoundID, Tissue, PKparameter, OriginallyRequested, 
             AppliesToSingleDose, AppliesOnlyWhenPerpPresent, 
             any_of(c("ObsValue", "ObsVariability", "FilePair", "NorD")), 
             DDI, MD)
   
   ## Checking PKparameters again -------------------------------------------
   
   # If they have left PKparameters as NA, then we need to figure out which PK
   # parameters apply.
   
   if(all(is.na(PKparameters$PKparameter))){
      
      MainPKParams <- AllPKParameters %>%
         # Per Hannah and template: Only include CL/F, t1/2, or tmax
         # if there's a specific reason to.
         filter(str_detect(PKparameter, "AUCinf_[^P]|AUCt|Cmax")) %>%
         filter(!str_detect(PKparameter, "_hepatic|CLpo")) %>% 
         select(PKparameter, AppliesToSingleDose, AppliesOnlyWhenPerpPresent) %>% 
         unique()
      
      suppressMessages(
         MainPKParams <- MainPKParams %>% 
            left_join(expand_grid(File = unique(PKparameters$File), 
                                  PKparameter = unique(MainPKParams$PKparameter)))
      )
      
      suppressMessages(
         PKparameters <- PKparameters %>% 
            select(-PKparameter,
                   -AppliesToSingleDose,
                   -AppliesOnlyWhenPerpPresent) %>% 
            left_join(MainPKParams)
      )
      
      # If it's a multiple-dose sim, only show the last-dose PK. Note that this
      # only applies when they have NOT requested specific PK parameters.
      PKparameters <- PKparameters %>% 
         left_join(AllRegCompounds %>% 
                      select(CompoundID, DosedCompoundID, DosedCompoundSuffix),
                   by = "CompoundID") %>% 
         left_join(existing_exp_details$MainDetails %>% 
                      select(File, matches("Regimen")), by = "File") %>% 
         mutate(MyRegimen = case_match(DosedCompoundID, 
                                       "substrate" ~ Regimen_sub, 
                                       "inhibitor 1" ~ Regimen_inhib, 
                                       "inhibitor 2" ~ Regimen_inhib2), 
                Keep = (MyRegimen == "Multiple Dose" &
                           !str_detect(PKparameter, "_dose1")) |
                   (MyRegimen == "Single Dose" & !str_detect(PKparameter, "_last"))) %>% 
         filter(Keep == TRUE)
      
   } 
   
   # Checking that we're looking for reasonable PK parameters. 
   PKparameters <- PKparameters %>% 
      mutate(HarmoniousDDI =  AppliesOnlyWhenPerpPresent == FALSE | 
                (AppliesOnlyWhenPerpPresent == TRUE & 
                    DDI == TRUE),
             HarmoniousDDI = ifelse(CompoundID %in% AllRegCompounds$CompoundID[
                AllRegCompounds$DDIrole == "perpetrator"] &
                   str_detect(PKparameter, "_withInhib|_ratio"), 
                FALSE, HarmoniousDDI), 
             HarmoniousRegimen = AppliesToSingleDose == TRUE |
                (AppliesToSingleDose == FALSE & 
                    MD == TRUE))
   
   if(all(is.na(PKparameters_orig))){
      PKparameters <- PKparameters %>% 
         mutate(HarmoniousRegimen = 
                   (AppliesToSingleDose == FALSE & MD == TRUE) |
                   AppliesToSingleDose == TRUE) %>% 
         filter(!PKparameter %in% AllPKParameters$PKparameter[AllPKParameters$UserInterval])
   }
   
   PKparameters <- PKparameters %>% 
      mutate(Harmonious = HarmoniousDDI & HarmoniousRegimen)
   
   if(any(PKparameters$Harmonious == FALSE, na.rm = T) &
      all(complete.cases(PKparameters_orig)) & FromCalcPKRatios == FALSE){
      
      Problem <- PKparameters %>% filter(Harmonious == FALSE) %>% 
         select(PKparameter, File, Sheet, CompoundID, Tissue) %>% 
         as.data.frame()
      
      Problem <- capture.output(print(Problem, row.names = FALSE))
      
      message("Warning:\nThe following requested PK parameters do not apply to these simulations:\n")
      message(str_c(Problem, collapse = "\n"))
      message("\nThey will be ignored.\n")
   }
   
   PKparameters <- PKparameters %>% 
      filter(Harmonious == TRUE) %>% 
      select(File, Sheet, CompoundID, Tissue, PKparameter, OriginallyRequested, 
             any_of(c("ObsValue", "ObsVariability", "FilePair", "NorD"))) %>% 
      rename(Value = ObsValue, 
             Variability = ObsVariability)
   
   # NB: Need to retain this part in case they have supplied NA for PKparameters
   # but have also supplied user-defined interval sheets. 
   
   # Checking that, when they've supplied a specific sheet, PKparameter does NOT
   # include the dose number. When they have *not* specified a tab, then it
   # should be a PKparameter w/the dose number or it should be something that
   # applies to the entire simulation. We are NOT issuing a warning here b/c I
   # think this will only come up if they supplied nothing for the PK parameters
   # specifically, so no need.
   PKparameters <- PKparameters %>%
      mutate(PKparameter = case_when(complete.cases(Sheet) & Sheet != "" ~
                                        sub("_dose1|_last", "", PKparameter),
                                     TRUE ~ PKparameter),
             PKparameter = harmonize_PK_names(PKparameter))
   
   if(FromCalcPKRatios){
      # Doing some additional final tidying specific to the calc_PK_ratios
      # options. Need to make the tidied PK parameters in PKparameters be the
      # ones that are also present in FilePairs, which has NOT been fully
      # tidied. Also need to break up by sheet and interval here.
      
      # PKparameter should be the only thing that might be all NA in FilePairs
      # but complete in PKparameters. This means we should be able to join
      # FilePairs and PKparameters based on the other columns.
      if(all(is.na(FilePairs$Numerator_PKparameter)) & 
          all(is.na(FilePairs$Denominator_PKparameter))){
         # These are the scenarios when the PK parameter should be the same for
         # the numerator and the denominator.
         
         suppressMessages(
            FilePairs <- FilePairs %>% 
               select(-Numerator_PKparameter, -Denominator_PKparameter) %>% 
               left_join(
                  PKparameters %>% 
                     filter(NorD == "Numerator") %>% 
                     select(-OriginallyRequested, -Value, -Variability, -NorD) %>% 
                     rename_with(.cols = -FilePair, 
                                 .fn = \(x) paste0("Numerator_", x))) %>% 
               # if all parameters were NA, then the numerator and denominator
               # parameters should be the same.
               mutate(Denominator_PKparameter = Numerator_PKparameter)
         )
         
      } else {
         # FIXME: Still trying to figure out how best to determine how to tidy
         # PK parameters when they differed for the numerator and denominator. I
         # think it *might* be ok to skip this, actually, b/c FilePairs will
         # have numerator and denominator PK parameters for all other scenarios.
         # I think.
      }
      
      # Using finalized parameters in FilePairs to determine dataset b/c will
      # need to call on calc_PK_ratios_subfun one dataset at a time. Need
      # those datasets to be broken up by interval, including one interval
      # for 1st dose and one interval for last dose.
      
      FilePairs <- FilePairs %>% 
         mutate(
            IntervalInternalUse_num = 
               str_extract(Numerator_PKparameter, "_dose1|_last"), 
            IntervalInternalUse_denom = 
               str_extract(Denominator_PKparameter, "_dose1|_last"), 
            IntervalInternalUse_num = 
               case_when(
                  is.na(IntervalInternalUse_num) ~ Numerator_Sheet, 
                  .default = IntervalInternalUse_num), 
            IntervalInternalUse_denom = 
               case_when(
                  is.na(IntervalInternalUse_denom) ~ Denominator_Sheet, 
                  .default = IntervalInternalUse_denom))
      
   }
   
   
   # Output -------------------------------------------------------------
   
   return(list(PKparameters = PKparameters, 
               existing_exp_details = existing_exp_details, 
               FilePairs = FilePairs))
   
}


