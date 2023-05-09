#' Extract pertinent data from Simulator output files for creating forest plots
#'
#' \code{extractForestData} automatically extract the data needed for generating
#' forest plots from Simulator output files and format it for using the forest
#' plot shiny app. This will take some time to run since it needs to open
#' multiple Excel files. NB: This has only been set up to work with geometric
#' means so far.
#'
#' @param sim_data_files a character vector of simulator output files; leaving
#'   this as the default NA will extract forest data from all simulator files in
#'   the current working directory
#' @param PKparameters PK parameters to extract from simulator output files;
#'   default is all possible AUC and Cmax geometric mean ratios for both dose 1
#'   and the last dose simulated. Input must be among "AUCinf_ratio_dose1",
#'   "AUCt_ratio_dose1", "Cmax_ratio_dose1", "AUCtau_ratio_last", or
#'   "Cmax_ratio_last". List them in the order you'd like the columns to appear
#'   in the output.
#' @param sheet optionally specify the name of the sheet where you'd like to
#'   pull the PK data, in quotes; for example, specify the tab where you have a
#'   user-defined AUC integration. \emph{Note:} Unless you want a very specific
#'   Excel sheet that's not what the usual sheet name would be for a first or
#'   last dose, this function will work best if this is left as NA. Also, since
#'   we don't know which dose these data were for, you'll see that the output
#'   parameter names do not include the suffixes "_last" or "_dose1".
#' @param checkDataSource TRUE (default) or FALSE: Include in the output a
#'   data.frame that lists exactly where the data were pulled from the simulator
#'   output file. Useful for QCing.
#' @param save_output optionally save the output by supplying a file name in
#'   quotes here, e.g., "My forest graph data.csv". If you leave off ".csv", it
#'   will still be saved as a csv file.
#'
#' @return a data.frame of data to use for making forest plots
#' @export
#'
#' @examples
#'
#' extractForestData(sim_data_files = NA,
#'                   save_output = "Forest data.csv")
#' 
extractForestData <- function(sim_data_files = NA, 
                              PKparameters = c("AUCinf_ratio_dose1", 
                                               "AUCt_ratio_dose1", 
                                               "Cmax_ratio_dose1", 
                                               "AUCtau_ratio_last", 
                                               "Cmax_ratio_last"), 
                              sheet = NA, 
                              checkDataSource = TRUE, 
                              save_output = NA){
   
   # Error catching -----------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
           call. = FALSE)
   }
   
   # If the user supplied "XXX_ss", change that to "XXX_last".
   PKparameters <- sub("_last", "_last", PKparameters)
   
   # If the user used "_first" instead of "_dose1", change that.
   PKparameters <- sub("_first", "_dose1", PKparameters)
   
   # If the user supplied "XXXtau_dose1", change that to "XXXt_dose1". 
   PKparameters <- sub("tau_dose1", "t_dose1", PKparameters)
   
   # If the user used AUCt_last instead of AUCtau_last, fix that for them.
   PKparameters <- sub("AUCt_last", "AUCtau_last", PKparameters)
   PKparameters <- sub("AUCt_ratio_last", "AUCtau_ratio_last", PKparameters)
   
   if(all(PKparameters %in% c("AUCinf_ratio_dose1", "AUCt_ratio_dose1", 
                              "Cmax_ratio_dose1", "AUCtau_ratio_last", 
                              "Cmax_ratio_last")) == FALSE){
      # The condition above checks whether any parameters supplied are not
      # among the possible options. The condition in this next line checks
      # whether ALL of them are not among the possible options. Just noting
      # that b/c it's easy to miss the specific placement of the parenthesis
      # after "Cmax_ratio_last". -LSh
      if(all(PKparameters %in% c("AUCinf_ratio_dose1", "AUCt_ratio_dose1", 
                                 "Cmax_ratio_dose1", "AUCtau_ratio_last", 
                                 "Cmax_ratio_last") == FALSE)){
         stop("None of the input supplied for PKparameters is among the possible options, which are AUCinf_ratio_dose1, AUCt_ratio_dose1, Cmax_ratio_dose1, AUCtau_ratio_last, and/or Cmax_ratio_last. Please check your input and try again.", 
              call. = FALSE)
      } else {
         warning(paste0("Not all of the input supplied for PKparameters is among the possible options. Specifically, ",
                        str_comma(setdiff(PKparameters, c("AUCinf_ratio_dose1",
                                                          "AUCt_ratio_dose1", 
                                                          "Cmax_ratio_dose1", 
                                                          "AUCtau_ratio_last", 
                                                          "Cmax_ratio_last"))),
                        " are not possible. These will be ignored.",
                        call. = FALSE))
         PKparameters <- PKparameters[PKparameters %in% c("AUCinf_ratio_dose1",
                                                          "AUCt_ratio_dose1", 
                                                          "Cmax_ratio_dose1",
                                                          "AUCtau_ratio_last", 
                                                          "Cmax_ratio_last")]
      }
   }
   
   # Main body of function ------------------------------------------------
   # Getting sim_data_files if not already supplied
   if(length(sim_data_files) == 1 && is.na(sim_data_files)){
      # If left as NA, pull all the files in this folder. 
      sim_data_files <- list.files(pattern = "xlsx")
      sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
   }
   
   Forest_l <- list()
   Deets <- list()
   DataCheck <- list()
   
   for(i in sim_data_files){
      
      message(paste("Extracting data from", i))
      suppressWarnings(
         temp <- extractPK(sim_data_file = i, 
                           PKparameters = PKparameters, 
                           sheet = sheet,
                           includeTrialInfo = FALSE,
                           returnExpDetails = TRUE,
                           returnAggregateOrIndiv = "aggregate", 
                           checkDataSource = checkDataSource)
      )
      
      # If only one parameter was found, then the 1st item in the list will be
      # a vector named as the parameter rather than a data.frame named
      # "aggregate". Adjusting for that.
      if("data.frame" %in% class(temp[[1]]) == FALSE){
         origname <- names(temp)[1]
         names(temp)[1] <- "aggregate"
         temp$aggregate <- as.data.frame(temp$aggregate) %>% 
            mutate(Statistic = names(temp$aggregate))
         names(temp$aggregate)[1] <- origname
      }
      
      Forest_l[[i]] <- temp$aggregate %>% mutate(File = i)
      Deets[[i]] <- as.data.frame(temp$ExpDetails) %>% mutate(File = i)
      if(checkDataSource){
         DataCheck[[i]] <- temp$QC
      }
      
      rm(temp)
   }
   
   if(checkDataSource){
      DataCheck <- bind_rows(DataCheck) %>% 
         select(File, everything()) %>% arrange(File, PKparam)
   }
   
   # Need to check for custom dosing regimens or things get mucked up.
   CustDos_sub <- sapply(Deets, FUN = function(x) class(x$Dose_sub)) == "character"
   CustDos_inhib <- sapply(Deets, FUN = function(x) class(x$Dose_inhib)) == "character"
   
   if(any(CustDos_sub)){
      
      warning(paste("A custom dosing regimen was used for the substrate for the files", 
                    names(Deets)[which(CustDos_sub)], 
                    "so that dose amount and dose interval will be set to NA in the extracted forest-plot data."), 
              call. = FALSE)
      
      for(i in which(CustDos_sub)){
         Deets[[i]]$Dose_sub <- as.numeric(NA)
         Deets[[i]]$DoseInt_sub <- as.numeric(NA)
         Deets[[i]] <- unique(Deets[[i]])
      }
   }
   
   if(any(CustDos_inhib)){
      warning(paste("A custom dosing regimen was used for the effector for the files", 
                    names(Deets)[which(CustDos_inhib)], 
                    "so that dose amount and dose interval will be set to NA in the extracted forest-plot data."), 
              call. = FALSE)
      
      for(i in which(CustDos_inhib)){
         Deets[[i]]$Dose_inhib <- as.numeric(NA)
         Deets[[i]]$DoseInt_inhib <- as.numeric(NA)
         Deets[[i]] <- unique(Deets[[i]])
      }
   }
   
   Deets <- bind_rows(Deets) %>% 
      select(File, everything()) %>% arrange(File)
   
   if(any(is.na(Deets$Inhibitor1))){
      warning(paste0("Forest plots only work for comparing PK parameters with vs. without an effector present, and the files ", 
                     Deets %>% filter(is.na(Inhibitor1)) %>% pull(File) %>% str_comma, 
                     " did not have an effector present. These files will not be included in the output data."))
      
      Deets <- Deets %>% filter(complete.cases(Inhibitor1))
      if(checkDataSource){DataCheck <- DataCheck %>% filter(File %in% Deets$File)}
      Forest_l <- Forest_l[names(Forest_l)[names(Forest_l) %in% Deets$File]]
   }
   
   Forest <- bind_rows(Forest_l)
   
   if(complete.cases(sheet)){
      PKparameters <- sub("_last|_dose1", "", PKparameters)
      names(Forest) <- sub("_last|_dose1", "", names(Forest))
   }
   
   ColNames <- data.frame(PKparam = PKparameters) %>% 
      expand_grid(Stat = c("GMR", "CI90_lo", "CI90_hi")) %>% 
      mutate(Cols = paste(PKparam, Stat, sep = "__")) %>% 
      pull(Cols)
   
   suppressMessages(
      Forest <- Forest %>% 
         mutate(Stat = recode(Statistic, "Geometric Mean" = "GMR",
                              "90% confidence interval around the geometric mean(lower limit)" = "CI90_lo", 
                              "90% confidence interval around the geometric mean(upper limit)" = "CI90_hi")) %>% 
         select(-Statistic) %>% 
         pivot_longer(cols = -c(File, Stat), 
                      names_to = "PKparam", values_to = "Value") %>% 
         filter(Stat %in% c("GMR", "CI90_lo", "CI90_hi")) %>% 
         mutate(ID = paste(PKparam, Stat, sep = "__")) %>% 
         select(-Stat, -PKparam) %>% 
         pivot_wider(names_from = ID, values_from = Value) %>% 
         left_join(Deets %>% select(File, Substrate, Dose_sub, Inhibitor1, 
                                    Dose_inhib)) %>% 
         select(File, Substrate, Dose_sub, Inhibitor1, Dose_inhib, 
                any_of(ColNames)) %>% unique()
   )
   
   if(complete.cases(save_output)){
      if(str_detect(save_output, "\\.")){
         FileName <- sub("\\..*", ".csv", save_output)
      } else {
         FileName <- paste0(save_output, ".csv")
      }
      write.csv(Forest, FileName, row.names = F)
   }
   
   Out <- list("ForestData" = Forest)
   
   if(checkDataSource){
      Out[["QC"]] <- DataCheck
   }
   
   if(length(Out) == 1){
      Out <- Out[[1]]
   }
   
   return(Out)
   
}


