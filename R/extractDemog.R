#' Extract simulated demographic data from the "Demographic Data" tab of a
#' Simcyp Simulator ouptut Excel file
#'
#' @param sim_data_files a character vector of simulator output files, each in
#'   quotes and encapsulated with \code{c(...)}, NA to extract
#'   concentration-time data for \emph{all} the Excel files in the current
#'   folder, or "recursive" to extract concentration-time data for \emph{all}
#'   the Excel files in the current folder and \emph{all} subfolders. Example of
#'   acceptable input: \code{c("sim1.xlsx", "sim2.xlsx")}. The path should be
#'   included with the file names if they are located somewhere other than your
#'   working directory. If some of your Excel files are not regular simulator
#'   output, e.g. they are sensitivity analyses or a file where you were doing
#'   some calculations, those files will be skipped.
#' @param demog_dataframe an existing data.frame of demographic data
#' @param overwrite TRUE or FALSE for whether to overwrite any existing data
#' @param which_data Which type of demographic data do you want? Options are
#' \describe{\item{"main demographics" (default)}{main demographic data including
#' height, weight, BSA, cardiac output, etc. These are all the items present on
#' the "Demographics" tab in the Simulator output.}
#'
#' \item{"CYPs"}{All of the information from the "Enzymatic Status CYPs" tab}
#'
#' \item{"UGTs}{All of the information from the "Enzymatic Status UGTs" tab}
#'
#' \item{"Drug-Population Parameters"}{All of the information from the
#' "Drug-Population Parameters" tab, such as B/P, fu, Qgut, etc.}
#'
#' \item{"all"}{Demographic data from the tabs "Demographics",
#' "Enzymatic Status CYPs", "Enzymatic Status UGTs", and
#' "Drug-Population Parameters"}}
#'
#' @return a data.frame of demographic data for simulated subjects. Columns
#'   included: File (simulation results file), Trial, Individual, Population,
#'   Sex (M or F), Age (years), Weight_kg (weight in kg), Height_cm (height in
#'   cm), BSA_m2 (body surface area in meters squared), BrainWt_g (brain weight
#'   in g), KidneyWt_g (kidney weight in g), LiverWt_g (liver weight in g),
#'   BMI_kgm2 (body mass index in kg/meter squared), CardiacOut (cardiac output
#'   in L/h), Haematocrit (percent), HSA_gL (human serum albumin in g/L), AGP_gL
#'   (alpha-1-acid glycoprotein in g/L), Other_uM (user-defined value in uM),
#'   Creatinine_umolL (creatinine in umol/L), GFR_mLmin (glomerular filtration
#'   rate in mL/min), GFR_mLminm2 (glomerular filtration rate in mL/min/m
#'   squared of body surface area), RenalFunction (the GFR divided by the
#'   reference GFR, which is 130 mL/min/1.73 m2 for male subjects and 120
#'   mL/min/1.73 m2 for female subjects), AllometricScalar (allometric scalar
#'   used), and Simulated (TRUE for simulated data).
#' @export
#'
#' @examples
#' # none yet
#' 
extractDemog <- function(sim_data_files = NA, 
                         demog_dataframe = NA, 
                         overwrite = FALSE, 
                         which_data = "main demographics"){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
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
   
   # If user has not included "xlsx" in file name, add that.
   sim_data_files[str_detect(sim_data_files, "xlsx$") == FALSE] <-
      paste0(sim_data_files[str_detect(sim_data_files, "xlsx$") == FALSE], 
             ".xlsx")
   
   sim_data_files <- unique(sim_data_files)
   
   # If user supplied an unquoted object, this checks whether that object
   # exists. However, if they supplied NA or NULL, this throws an error. 
   Recode_demog_dataframe <- suppressWarnings(
      try(exists(deparse(substitute(demog_dataframe))) == FALSE, silent = TRUE))
   
   # If they got an error, then the class of Recode_X will be "try-error", and
   # then we want Recode_X to be TRUE.
   if(suppressWarnings("try-error" %in% class(Recode_demog_dataframe))){
      Recode_demog_dataframe <- TRUE
   }
   
   if(Recode_demog_dataframe){
      demog_dataframe <- "none"
   }
   
   # Checking for existing demographic data
   if(exists(deparse(substitute(demog_dataframe))) && 
      "logical" %in% class(demog_dataframe) == FALSE &&
      "data.frame" %in% class(demog_dataframe) && 
      nrow(demog_dataframe) > 0){
      
      if("File" %in% names(demog_dataframe) == FALSE){
         demog_dataframe$File <- "unknown file"
      }
      
      if(overwrite == FALSE){
         
         DataToFetch <- data.frame(File = sim_data_files) %>% 
            filter(!File %in% demog_dataframe$File)
         
         sim_data_files_topull <- unique(as.character(DataToFetch$File))
         
      } else {
         
         DataToFetch <- data.frame(File = sim_data_files)
         
         sim_data_files_topull <- unique(sim_data_files)
         demog_dataframe <- demog_dataframe %>%
            filter(!File %in% DataToFetch$File)
      }
      
   } else {
      
      # This is when there's no existing data, so we're just getting everything. 
      sim_data_files_topull <- sim_data_files
      demog_dataframe <- data.frame()
      
   }
   
   if(length(sim_data_files_topull) == 0){
      message("There are no data to pull that are not already present in your current data.frame. Returning current data.frame.")
      return(demog_dataframe)
   }
   
   # Making sure that all the files exist before attempting to pull data
   if(any(file.exists(sim_data_files_topull) == FALSE)){
      MissingSimFiles <- sim_data_files_topull[
         which(file.exists(sim_data_files_topull) == FALSE)]
      warning(paste0("The file(s) ", 
                     str_comma(paste0("`", MissingSimFiles, "`")), 
                     " is/are not present or have a file path that is too long and thus cannot be extracted.\n"), 
              call. = FALSE)
      sim_data_files_topull <- setdiff(sim_data_files_topull, MissingSimFiles)
   }
   
   which_data <- tolower(which_data)
   which_data[str_detect(which_data, "main|demograph")] <- "Demographic Data"
   which_data[str_detect(which_data, "cyp")] <- "Enzymatic Status CYPs"
   which_data[str_detect(which_data, "ugt")] <- "Enzymatic Status UGTs"
   which_data[str_detect(which_data, "drug|pop")] <- "Drug-Population Parameters"
   which_data <- unique(which_data)
   
   if(any(which_data == "all")){
      which_data <- c("Demographic Data", 
                      "Enzymatic Status CYPs", 
                      "Enzymatic Status UGTs", 
                      "Drug-Population Parameters")
   }
   
   which_data <- which_data[which_data %in% c("Demographic Data", 
                                              "Enzymatic Status CYPs", 
                                              "Enzymatic Status UGTs", 
                                              "Drug-Population Parameters")]
   
   if(length(which_data) == 0){
      stop(wrapn("You have specified something for which data you want that is not among the possible options. Please check the help file and try again."), 
           call. = FALSE)
   }
   
   # Main body of function --------------------------------------------------
   
   Demog <- list()
   Population <- list()
   CYPs <- list()
   UGTs <- list()
   DrugPop <- list()
   
   for(ff in sim_data_files_topull){
      
      ## Population ---------------------------------------------------------
      
      suppressMessages(Summary.xl <- readxl::read_xlsx(ff, sheet = "Summary"))
      
      Population[[ff]] <- tibble(
         File = ff, 
         Population = tidyPop(t(Summary.xl[, 2])[
            which(Summary.xl[, 1] == "Population Name")])$Population)
      
      rm(Summary.xl)
      
      
      ## demog --------------------------------------------------------------
      
      if("Demographic Data" %in% which_data){
         
         Demog.xl <- tryCatch(readxl::read_xlsx(ff, 
                                                skip = 20, # This should be the same every time. 
                                                sheet = "Demographic Data"), 
                              error = function(x) "glitch")
         
         if("character" %in% class(Demog.xl)){
            warning(paste0("The file `", 
                           ff, 
                           "` is not present or does not have a tab titled `Demographic Data`, so we cannot extract any data for it.\n"), 
                    call. = FALSE)
         } else {
            
            ColNames <- c("Index" = "Individual", 
                          "Population" = "PopulationNumber", 
                          "Trial" = "Trial", 
                          "Sex" = "Sex", 
                          "Sex Code" = "SexCode", 
                          "Age (Years)" = "Age", 
                          "Weight (kg)" = "Weight_kg",
                          "Height (cm)" = "Height_cm",
                          "BSA (m²)" = "BSA_m2", 
                          "Brain Weight (g)" = "BrainWt_g", 
                          "Kidney Weight (g)" = "KidneyWt_g",
                          "Liver Weight (g)" = "LiverWt_g", 
                          "BMI (kg/m²)" = "BMI_kgm2", 
                          "Cardiac Output (L/h)" = "CardiacOut", 
                          "Haematocrit (%)" = "Haematocrit", 
                          "HSA (g/L)" = "HSA_gL", 
                          "AGP (g/L)" = "AGP_gL",
                          "Other (µM)" = "Other_uM",
                          "Serum Creatinine (µmol/L)" = "Creatinine_umolL", 
                          "GFR (mL/min/1.73m²)" = "GFR_mLminm2",
                          "GFR (mL/min)" = "GFR_mLmin", 
                          "Renal Function" = "RenalFunction",
                          "Allometric Scalar" = "AllometricScalar", 
                          "Simulation Duration" = "SimDuration")
            
            ColNames <- ColNames[which(names(ColNames) %in% names(Demog.xl))]
            
            MissingColNames <- setdiff(names(Demog.xl), names(ColNames))
            if(length(MissingColNames) > 0){
               warning(paste0("The following column names are present in the demographic data for the file ", 
                              ff, "``, but are not accounted for in this function:\n", 
                              str_c(MissingColNames, collapse =  "\n")), 
                       call. = FALSE)
            }
            
            suppressWarnings(LastRow <- min(which(is.na(Demog.xl$Index))) - 1)
            LastRow <- ifelse(is.infinite(LastRow), nrow(Demog.xl), LastRow)
            
            Demog[[ff]] <- Demog.xl[1:LastRow, names(ColNames)]
            names(Demog[[ff]]) <- ColNames
            
            suppressWarnings(
               Demog[[ff]] <- Demog[[ff]] %>% 
                  select(-SexCode, -SimDuration) %>% 
                  mutate(across(.cols = any_of(setdiff(ColNames, "Sex")), 
                                .fns = as.numeric), 
                         File = ff, 
                         Simulated = TRUE, 
                         Individual = as.character(Individual), 
                         Trial = as.character(Trial))
            )
            
            # Calculating whichever variation of GFR is missing
            if(all(c("GFR_mLmin", "BSA_m2") %in% names(Demog[[ff]])) & 
               "GFR_mLminm2" %in% names(Demog[[ff]]) == FALSE){
               
               Demog[[ff]] <- Demog[[ff]] %>% 
                  mutate(GFR_mLminm2 = GFR_mLmin / (1.73/BSA_m2))
            }
            
            if(all(c("GFR_mLmin2", "BSA_m2") %in% names(Demog[[ff]])) & 
               "GFR_mLmin" %in% names(Demog[[ff]]) == FALSE){
               
               Demog[[ff]] <- Demog[[ff]] %>% 
                  mutate(GFR_mLmin = GFR_mLminm2 * BSA_m2)
            }
            
            rm(Demog.xl, LastRow, ColNames)
            
         }
      }
      
      ## CYPs --------------------------------------------------------------
      
      if("Enzymatic Status CYPs" %in% which_data){
         
         suppressMessages(
            CYPs.xl <- tryCatch(readxl::read_xlsx(ff, 
                                                  skip = 1, # This should be the same every time. 
                                                  sheet = "Enzymatic Status CYPs"), 
                                error = function(x) "glitch")
         )
         
         if("character" %in% class(CYPs.xl)){
            warning(paste0("The file `", 
                           ff, 
                           "` is not present or does not have a tab titled `Enzymatic Status CYPs`, so we cannot extract any data for it.\n"), 
                    call. = FALSE)
         } else {
            
            suppressWarnings(LastRow <- min(which(is.na(CYPs.xl$Index))) - 1)
            LastRow <- ifelse(is.infinite(LastRow), nrow(CYPs.xl), LastRow)
            
            CYPs[[ff]] <- CYPs.xl[20:LastRow, ]
            CYPnames <- t(CYPs.xl[19, ]) %>% as.character()
            CYPnames[is.na(CYPnames)] <- "X"
            CYPnames[which(CYPnames == "PM/EM/IM1/IM2/UM")] <- 
               paste0(CYPnames[which(CYPnames == "PM/EM/IM1/IM2/UM") - 1],
                      "_phenotype")
            CYPnames <- gsub(" ", "_", CYPnames)
            
            TypeNames <- tibble(Type = t(CYPs.xl[18, ])) %>% 
               fill(Type, .direction = "down") %>% 
               mutate(Type = case_match(
                  Type, 
                  "Organ Scalars" ~ "_organ_scalar", 
                  "Enzyme Abundances ( pmol P450 )" ~ "_abundance_pmol", 
                  "Enzyme Abundance (pmol P450/mg mic.protein)" ~ "_abundance_pmol_mg")) %>% 
               pull(Type)
            
            CYPnames <- paste0(CYPnames, TypeNames)
            CYPnames <- sub("phenotype_abundance_pmol", "phenotype", CYPnames)
            CYPnames <- case_when(
               str_detect(CYPnames, "X") ~ "X", 
               CYPnames == "IndexNA" ~ "Individual", 
               str_detect(CYPnames, "NA") ~ sub("NA", "", CYPnames), 
               .default = CYPnames)
            
            CYPs[[ff]] <- CYPs[[ff]][, which(CYPnames != "X")]
            names(CYPs[[ff]]) <- CYPnames[which(CYPnames != "X")]
            
            suppressWarnings(
               CYPs[[ff]] <- CYPs[[ff]] %>% 
                  mutate(
                     across(
                        .cols = CYPnames[
                           str_detect(CYPnames, "abundance|scalar")], 
                        .fns = as.numeric), 
                     File = ff,
                     Simulated = TRUE, 
                     Individual = as.character(Individual), 
                     Trial = as.character(Trial))
            )
            
            rm(CYPs.xl, LastRow, CYPnames, TypeNames)
            
         }
      }
      
      ## UGTs --------------------------------------------------------------
      
      if("Enzymatic Status UGTs" %in% which_data){
         
         suppressMessages(
            UGTs.xl <- tryCatch(readxl::read_xlsx(ff, 
                                                  skip = 1, # This should be the same every time. 
                                                  sheet = "Enzymatic Status UGTs"), 
                                error = function(x) "glitch")
         )
         
         if("character" %in% class(UGTs.xl)){
            warning(paste0("The file `", 
                           ff, 
                           "` is not present or does not have a tab titled `Enzymatic Status UGTs`, so we cannot extract any data for it.\n"), 
                    call. = FALSE)
         } else {
            
            suppressWarnings(LastRow <- min(which(is.na(UGTs.xl$Index))) - 1)
            LastRow <- ifelse(is.infinite(LastRow), nrow(UGTs.xl), LastRow)
            
            UGTs[[ff]] <- UGTs.xl[21:LastRow, ]
            UGTnames <- t(UGTs.xl[20, ]) %>% as.character()
            UGTnames[is.na(UGTnames)] <- "X"
            UGTnames[which(UGTnames == "PM/EM/IM/UM")] <- 
               paste0(UGTnames[which(UGTnames == "PM/EM/IM/UM") - 1],
                      "_phenotype")
            UGTnames <- gsub(" ", "_", UGTnames)
            
            TypeNames <- tibble(Type = t(UGTs.xl[19, ])) %>% 
               fill(Type, .direction = "down") %>% 
               mutate(Type = case_match(
                  Type, 
                  "Absolute UGT Abundance (pmol UGT)" ~ "_abundance_pmol", 
                  "Relative UGT Abundances" ~ "_rel_abundance")) %>% 
               pull(Type)
            
            TissueNames <- tibble(Tissue = t(UGTs.xl[18, ])) %>% 
               fill(Tissue, .direction = "down") %>% 
               pull(Tissue)
            
            UGTnames <- paste0(TissueNames, "_", UGTnames, TypeNames)
            UGTnames <- sub("Kidney_Kidney", "Kidney", UGTnames)
            UGTnames <- sub("Liver_Liver", "Liver", UGTnames)
            UGTnames <- sub("Gut_Gut", "Gut", UGTnames)
            UGTnames <- sub("phenotype_rel_abundance", 
                            "phenotype", UGTnames)
            UGTnames <- case_when(
               str_detect(UGTnames, "X") ~ "X", 
               UGTnames == "NA_IndexNA" ~ "Individual", 
               UGTnames == "NA_TrialNA" ~ "Trial", 
               .default = UGTnames)
            
            UGTs[[ff]] <- UGTs[[ff]][, which(UGTnames != "X")]
            names(UGTs[[ff]]) <- UGTnames[which(UGTnames != "X")]
            
            suppressWarnings(
               UGTs[[ff]] <- UGTs[[ff]] %>% 
                  mutate(
                     across(
                        .cols = UGTnames[
                           str_detect(UGTnames, "abundance")], 
                        .fns = as.numeric), 
                     File = ff,
                     Simulated = TRUE, 
                     Individual = as.character(Individual), 
                     Trial = as.character(Trial))
            )
            
            rm(UGTs.xl, LastRow, UGTnames, TypeNames, TissueNames)
            
         }
      }
      
      ## drug pop --------------------------------------------------------------
      
      if("Drug-Population Parameters" %in% which_data){
         
         suppressMessages(
            DrugPop.xl <- tryCatch(readxl::read_xlsx(ff, 
                                                     skip = 1, # This should be the same every time. 
                                                     sheet = "Drug-Population Parameters"), 
                                   error = function(x) "glitch")
         )
         
         if("character" %in% class(DrugPop.xl)){
            warning(paste0("The file `", 
                           ff, 
                           "` is not present or does not have a tab titled `Enzymatic Status DrugPop`, so we cannot extract any data for it.\n"), 
                    call. = FALSE)
         } else {
            
            suppressWarnings(LastRow <- min(which(is.na(DrugPop.xl$Index))) - 1)
            LastRow <- ifelse(is.infinite(LastRow), nrow(DrugPop.xl), LastRow)
            
            DrugPop[[ff]] <- DrugPop.xl[20:nrow(DrugPop.xl), ]
            DrugPopnames <- tibble(DrugPopnames = t(DrugPop.xl[19, ]) %>% 
                                      as.character()) %>% 
               mutate(
                  DrugPopnames = case_when(
                     is.na(DrugPopnames) ~ "X", 
                     DrugPopnames == "Index" ~ "Individual", 
                     .default = DrugPopnames), 
                  DrugPopnames = str_replace(DrugPopnames, " \\(Sub\\)", "_sub"), 
                  DrugPopnames = str_replace(DrugPopnames, " \\(Inhib\\)", "_inhib"), 
                  DrugPopnames = str_replace(DrugPopnames, " \\(Met1\\)", "_met1"), # FIXME - Not really certain of any of the syntax of these except substrate
                  DrugPopnames = str_replace(DrugPopnames, " \\(Met2\\)", "_met2"), 
                  DrugPopnames = str_replace(DrugPopnames, " \\(SecMet\\)", "_secmet"), 
                  DrugPopnames = str_replace(DrugPopnames, " \\(Inhib2\\)", "_inhib2"), 
                  DrugPopnames = str_replace(DrugPopnames, " \\(Inhib1Met\\)", "_inhib1met"), 
                  DrugPopnames = case_when(
                     str_detect(DrugPopnames, "Dose.*mg") ~ 
                        sub("Dose", "Dose_mg", DrugPopnames), 
                     str_detect(DrugPopnames, "% Bound to LPP") ~ 
                        sub("% Bound to LPP", "PercBoundLPP", DrugPopnames), 
                     .default = DrugPopnames), 
                  DrugPopnames = gsub(" \\(L/h\\)| \\(mg\\)", "", DrugPopnames)) %>% 
               pull(DrugPopnames)
            
            DrugPop[[ff]] <- DrugPop[[ff]][, which(DrugPopnames != "X")]
            names(DrugPop[[ff]]) <- DrugPopnames[which(DrugPopnames != "X")]
            
            suppressWarnings(
               DrugPop[[ff]] <- DrugPop[[ff]] %>% 
                  mutate(
                     across(
                        .cols = DrugPopnames[
                           str_detect(DrugPopnames, "BP|fu|PercBound|Qgut|Dose")], 
                        .fns = as.numeric), 
                     File = ff,
                     Simulated = TRUE, 
                     Individual = as.character(Individual), 
                     Trial = as.character(Trial))
            )
            
            rm(DrugPop.xl, LastRow, DrugPopnames)
            
         }
      }
   }
   
   
   # return -------------------------------------------------------
   
   Demog_list <- list("Demog" = Demog, 
                      "CYPs" = CYPs, 
                      "UGTs" = UGTs, 
                      "DrugPop" = DrugPop)
   
   DataPresent <- names(which(map(Demog_list, length) > 0))
   
   AllDemog <- bind_rows(Demog_list[[DataPresent[1]]])
   
   for(i in setdiff(DataPresent, DataPresent[1])){
      suppressMessages(
         AllDemog <- left_join(AllDemog, 
                               bind_rows(Demog_list[[i]]))
      )
   }
   
   suppressMessages(
      Out <- AllDemog %>% 
         filter(complete.cases(File)) %>% 
         left_join(bind_rows(Population)) %>% 
         bind_rows(demog_dataframe) %>% 
         select(any_of(c("File", "Population", "Trial", "Individual")), 
                everything())
   )
   
   return(Out)
   
}


