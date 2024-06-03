#' Examples for supplying PK parameters to the pk_table function for the
#' argument \code{PKparameters}
#'
#' \code{make_example_PK_input} gives examples of how to request specific PK
#' parameters or supply specific PK data to the functions
#' \code{\link{pk_table}}, \code{\link{pksummary_table}}, or
#' \code{\link{pksummary_mult}}. You can either just get a printout in the
#' console of the names of the parameters you want or you can get an example csv
#' file to use a as a template.
#'
#'
#' @return saves a csv file of example PK parameters for supplying to the
#'   argument \code{PKparameters}
#' @export
#'
#' @examples
#' # None yet. This is an interactive function.
#'
#' 
make_example_PK_input <- function(){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Main body of function ---------------------------------------------------
   
   Opts <- c("1" = "typical dose 1", 
             "2" = "typical last dose",
             "3" = "typical DDI", 
             "4" = "specifying the sheet to use", 
             "5" = "specifying the compounds to use", 
             "6" = "specifying the tissues to use", 
             "7" = "all possible AUCs", 
             "8" = "all possible clearance types", 
             "9" = "comparing multiple simulations to the same observed PK", 
             "10" = "including variability in observed PK")
   
   
   ## Collecting requests ----------------------------------------------------
   message("What kind of PK data do you want in your table? Options are:")
   message(paste0("- ", 1:length(Opts), ") ", Opts, "\n"))
   Q1 <- readline("   ")
   
   if(Q1 %in% as.numeric(1:10) == FALSE){
      message("Please try again. You must enter a number from 1 to 10.\n")
      
      message("What kind of PK data do you want in your table? Options are:   \n  1) typical dose 1\n  2) typical last dose\n  3) typical DDI\n  4) PK from a specific sheet in the output\n  5) PK for specific compounds\n  6) PK for specific tissues\n  7) all possible AUCs\n  8) all possible clearance\n  9) comparing more than one simulation to the same observed PK\n  10) including variability in observed PK")
      
      Q1 <- readline("   ")
      
      if(Q1 %in% as.numeric(1:10) == FALSE){
         stop("Sorry, we don't know what you want. Please start over.", 
              call. = FALSE)
      }
   }
   
   repeat{
      Q2 <- readline("Any others? (Enter a number from 1 to 10 or `n` for no)  ")
      
      if((Q2 %in% as.numeric(1:10) == FALSE) &
         (str_sub(tolower(Q2), 1, 2) != "n")){
         message("Please try again. You must enter a number from 1 to 10 or `n`.\n")
         
         message("What kind of PK data do you want in your table? Options are:   \n  1) typical dose 1\n  2) typical last dose\n  3) typical DDI\n  4) PK from a specific sheet in the output\n  5) PK for specific compounds\n  6) PK for specific tissues\n  7) all possible AUCs\n  8) all possible clearance\n  9) comparing more than one simulation to the same observed PK\n  10) including variability in observed PK\nor `n` for `no more`.")
         
         Q2 <- readline("   ")
         
         if((Q2 %in% as.numeric(1:10) == FALSE) &
            (str_sub(tolower(Q2), 1, 2) != "n")){
            stop("Sorry, we don't know what you want. Please start over.", 
                 call. = FALSE)
         }
      }
      
      if(str_sub(tolower(Q2), 1, 2) == "n"){
         break
      } else {
         Q1 <- c(Q1, Q2)
      }
   }
   
   Q1 <- sort(unique(Q1))
   
   ## Pulling examples ------------------------------------------------------
   
   Examples <- list()
   PKexamples <- PKexamples %>% 
      unite(ID, c(File, Tissue, CompoundID, PKparameter),  remove = FALSE)
   
   if("1" %in% Q1){ # dose 1
      Examples[["A1"]] <- 
         PKexamples %>% filter(str_detect(PKparameter, "_dose1"))
   }
   
   if("2" %in% Q1){ # last dose
      Examples[["A2"]] <- 
         PKexamples %>% filter(str_detect(PKparameter, "_last"))
   }
   
   if("3" %in% Q1){ # DDI
      
      ExistingIDs <- bind_rows(Examples)
      if("ID" %in% names(ExistingIDs)){ExistingIDs <- ExistingIDs$ID}
      
      if("2" %in% Q1){ # last dose
         # If they asked for DDI parameters AND they asked for last dose, just
         # give them last-dose DDI examples. Otherwise, give them only 1st dose
         # DDI examples.
         Examples[["A3"]] <- 
            PKexamples %>% filter(DDI_MD == TRUE) %>% 
            filter(!ID %in% ExistingIDs) %>% 
            mutate(Notes = "")
         
      } else {
         
         Examples[["A3"]] <- 
            PKexamples %>% filter(DDI_SD == TRUE) %>% 
            filter(!ID %in% ExistingIDs) %>% 
            mutate(Notes = "")
      }
      
      if(nrow(Examples[["A3"]]) > 0){
         Examples[["A3"]]$Notes[1] <- 
            "If it's a DDI parameter, add `_withInhib` to the PKparameter name. If it's the DDI ratio, put `_ratio` before the `_dose1` or `_last` bit."
      }
      
      if("4" %in% Q1){ # specific sheet
         ExistingIDs <- bind_rows(Examples)
         if("ID" %in% names(ExistingIDs)){ExistingIDs <- ExistingIDs$ID}
         
         Examples[["A3.4"]] <- 
            PKexamples %>% filter(DDI_sheet == TRUE) %>% 
            filter(!ID %in% ExistingIDs) %>% 
            mutate(Notes = "")
         
         if(nrow(Examples[["A3.4"]]) > 0){
            Examples[["A3.4"]]$Notes[1] <- 
               "If it's a DDI parameter for a user-defined interval, still add `_withInhib` to the end of the PKparameter name, and, if it's the DDI ratio, put `_ratio` at the end."
         }
      }
   }
   
   if("4" %in% Q1){ # specific sheet
      ExistingIDs <- bind_rows(Examples)
      if("ID" %in% names(ExistingIDs)){ExistingIDs <- ExistingIDs$ID}
      
      Examples[["A4"]] <- 
         PKexamples %>% filter(complete.cases(Sheet)) %>% 
         filter(!ID %in% ExistingIDs) %>% 
         mutate(Notes = "")
      
      if(nrow(Examples[["A4"]]) > 0){
         Examples[["A4"]]$Notes[1] <- 
            "Only fill in the `Sheet` column when you have a specific sheet you want and not when it's the standard first or last dose PK. You can specify more than one user-defined tab for the same simulation; put them in separate rows, though."
      }
   }
   
   if("5" %in% Q1){ # specific compounds 
      ExistingIDs <- bind_rows(Examples)
      if("ID" %in% names(ExistingIDs)){ExistingIDs <- ExistingIDs$ID}
      
      Examples[["A5"]] <- 
         PKexamples %>% filter(CompoundID != "substrate") %>% 
         filter(!ID %in% ExistingIDs) %>% 
         mutate(Notes = "")
      
      if(nrow(Examples[["A5"]]) > 0){
         Examples[["A5"]]$Notes[1] <- 
            paste("All the possible compounds:", 
                  str_comma(AllCompounds$CompoundID))
      }
   }
   
   if("6" %in% Q1){ # specific tissues 
      ExistingIDs <- bind_rows(Examples)
      if("ID" %in% names(ExistingIDs)){ExistingIDs <- ExistingIDs$ID}
      
      Examples[["A6"]] <- 
         PKexamples %>% filter(Tissue != "plasma") %>% 
         filter(!ID %in% ExistingIDs) %>% 
         mutate(Notes = "")
      
      if(nrow(Examples[["A6"]]) > 0){
         Examples[["A6"]]$Notes[1] <- 
            "All the possible tissues: plasma (default), unbound plasma, blood, unbound blood, peripheral plasma, or peripheral blood."
      }
   }
   
   if("7" %in% Q1){ # all AUCs
      ExistingIDs <- bind_rows(Examples)
      if("ID" %in% names(ExistingIDs)){ExistingIDs <- ExistingIDs$ID}
      
      Examples[["A7"]] <- 
         PKexamples %>% filter(AUC == TRUE) %>% 
         filter(!ID %in% ExistingIDs) %>% 
         mutate(Notes = "")
      
      if(nrow(Examples[["A7"]]) > 0){
         Examples[["A7"]]$Notes[1] <- 
            "AUCinf_dose1 is only for dose 1. AUCt (whether it's AUCt alone or AUCt_dose1) means there was no extrapolation beyond the end of the specified interval. AUCtau_last is the AUC to time t."
      }
   }
   
   if("8" %in% Q1){ # all CL
      ExistingIDs <- bind_rows(Examples)
      if("ID" %in% names(ExistingIDs)){ExistingIDs <- ExistingIDs$ID}
      
      Examples[["A8"]] <- 
         PKexamples %>% filter(AUC == TRUE) %>% 
         filter(!ID %in% ExistingIDs) %>% 
         mutate(Notes = "")
      
      if(nrow(Examples[["A8"]]) > 0){
         Examples[["A8"]]$Notes[1] <- 
            "When dose 1 clearance was calculated as dose / AUCinf, it is `CLinf_dose1`. If it were calculated as dose / AUC to time t, it will be `CLt_dose1` for dose 1 or `CLt` for a user-defined interval. CLtau_last is for the dose for the AUCtau for the last dose."
      }
   }
   
   if("9" %in% Q1){ # mult sims
      ExistingIDs <- bind_rows(Examples)
      if("ID" %in% names(ExistingIDs)){ExistingIDs <- ExistingIDs$ID}
      
      Examples[["A9"]] <- 
         PKexamples %>% filter(str_detect(File, ", ")) %>% 
         filter(!ID %in% ExistingIDs) %>% 
         mutate(Notes = "")
      
      if(nrow(Examples[["A9"]]) > 0){
         Examples[["A9"]]$Notes[1] <- 
            "To compare the same values for multiple simulation files, you can use a new row for each or just separate the simulation file names by a comma and a space."
      }
   }
   
   if("10" %in% Q1){ # variability in obs PK
      ExistingIDs <- bind_rows(Examples)
      if("ID" %in% names(ExistingIDs)){ExistingIDs <- ExistingIDs$ID}
      
      Examples[["A10"]] <- 
         PKexamples %>% filter(complete.cases(Variability)) %>% 
         filter(!ID %in% ExistingIDs) %>% 
         mutate(Notes = "")
      
      if(nrow(Examples[["A10"]]) > 0){
         Examples[["A10"]]$Notes[1] <- 
            "List variability as a fraction when it's the CV, e.g., 0.5 for `50 percent`, and as the minimum to the maximum when it's a range. CAUTION: Excel will convert things like `1-4` to a date, so list ranges as, e.g., `1 to 4`."
      }
   }
   
   Examples <- bind_rows(Examples) %>% unique() %>% 
      arrange(SortOrder) %>% 
      select(File, CompoundID, Tissue, Sheet, PKparameter, Value, Variability,
             any_of("Notes")) %>% 
      mutate(across(.cols = any_of("Notes"), 
                    .fns = \(x) ifelse(is.na(x), "", x)))
   
   
   ## Returning ------------------------------------------------------------
   
   message(str_wrap("Do you want to just see the R code names for these PK parameters or do you want to make a csv file with examples for how to enter data for the pksummary function argument 'PKparameters'?\n"))
   message("Options are:\n  1) just show me the names to use right here in the console\n  2) make me a csv file example\n")
   Q3 <- readline("    ")
   
   if(Q3 %in% c("1", "2") == FALSE){
      
      message("You entered something other than 1 or 2. Please try again.\n")   
      message(str_wrap("Do you want to just see the R code names for these PK parameters or do you want to make a csv file with examples for how to enter data for the pksummary function argument 'PKparameters'?\n"))
      message("Options are:\n  1) just show me the names to use right here in the console\n  2) make me a csv file example\n")
      Q3 <- readline("    ")
      
      if(Q3 %in% c("1", "2") == FALSE){
         message("You entered something other than 1 or 2 again, so we'll assume you want to just see these names in the console.\n")
         Q3 <- "1"
      }
   }
   
   if(any(c("1", "2", "3") %in% Q1)){
      Q1subset <- Q1[Q1 %in% c("1", "2", "3")]
      Q1 <- setdiff(Q1, Q1subset)
      
      if(length(Q1) == 0){
         OutPK <- paste(str_comma(Opts[Q1subset]), "regimens")
      } else {
         OutPK <- str_comma(c(paste(str_comma(Opts[Q1subset]), "regimens"), 
                              str_comma(Opts[Q1])))
      }
   } else {
      OutPK <- str_comma(Opts[Q1])
   }
   
   
   if(Q3 == "1"){
      message(str_wrap(paste0("Here are examples for the coded PK parameters for ", 
                              OutPK, ".")))
      
      message(Examples %>% pull(PKparameter) %>% unique() %>% 
                 sort() %>% str_c(., collapse = "\n"))
   } else {
      
      MyFile <- "PK examples for SimcypConsultancy package.csv"
      if(file.exists(MyFile)){
         message(str_wrap(paste0("There is already a file with the same name that we're trying to use for saving your examples (`", 
                                 MyFile, 
                                 "`. Please specify what you would like the file to be called instead:\n")))
         MyFile <- readline("    ")
         
         # If they supplied quotes here, that messes things up. Removing. 
         MyFile <- gsub("\"", "", MyFile)
         
         # If they didn't include ".csv" at the end, add that.
         MyFile <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$|\\.docx$|\\.csv$", "", MyFile), ".csv")
      }
      
      write.csv(Examples, file = MyFile, row.names = FALSE, na = "")
      message(str_wrap(paste0(
         "We've made you an example csv file called `", MyFile, 
         "` with PK for ", OutPK, ".")))
      
   }
   
   message("A few miscellaneous notes:\n")
   message(paste0(str_wrap("- If you made a csv file here, we recommend replacing the numbers and the file names with your own data and using that csv file as input to the argument 'PKparameters'.", 
                           indent = 3, exdent = 5), "\n"))
   message(paste0(str_wrap("- If you don't have any observed PK values for comparisons but you want to specify exactly what PK parameters you want from which simulation file or which tab or which tissue, etc., you can specify that information in a csv file, laid out like in this example, and supply that file for the argument `PKparameters`. For any row where the `Value` column is empty, we'll supply only the simulated results.", 
                           indent = 3, exdent = 5), "\n"))
   message(paste0(str_wrap("- Don't worry about whether your source data have been appropriately rounded; the pksummary functions will take care of that for you. We recommend saving the results to a Word file because csv files drop trailing zeroes.", 
                           indent = 3, exdent = 5), "\n"))
   message(paste0(str_wrap("- If you want a standard PK parameter for dose 1 or the last dose, leave the `Sheet` column blank.",
                           indent = 3, exdent = 5), "\n"))
   
}




