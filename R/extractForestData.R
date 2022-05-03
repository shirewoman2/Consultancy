#' Extract pertinent data from Simulator output files for creating forest plots
#'
#' @param sim_data_files a character vector of simulator output files
#' @param PKparameters PK parameters to extract from simulator output files.
#'   Defaults to all possible AUC and Cmax ratios. List them in the order you'd
#'   like the columns to appear in the output. 
#' @param checkDataSource TRUE or FALSE: Include in the output a data.frame that
#'   lists exactly where the data were pulled from the simulator output file.
#'   Useful for QCing.
#' @param save_output optionally save the output by supplying a file name in
#'   quotes here, e.g., "My experimental details.csv". If you leave off ".csv",
#'   it will still be saved as a csv file.
#'
#' @return
#' @export
#'
#' @examples
#' 
#' # None yet
#' 
extractForestData <- function(sim_data_files, 
                              PKparameters = c("AUCinf_ratio_dose1", 
                                               "AUCt_ratio_dose1", 
                                               "Cmax_ratio_dose1", 
                                               "AUCtau_ratio_ss", 
                                               "Cmax_ratio_ss"), 
                              checkDataSource = TRUE, 
                              save_output = NA){
    Forest_l <- list()
    Deets <- list()
    DataCheck <- list()
    
    for(i in sim_data_files){
        
        suppressWarnings(
            temp <- extractPK(sim_data_file = i, 
                              PKparameters = PKparameters, 
                              includeTrialInfo = FALSE,
                              returnExpDetails = TRUE,
                              returnAggregateOrIndiv = "aggregate", 
                              checkDataSource = checkDataSource)
        )
        
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
    
    ColNames <- data.frame(PKparam = PKparameters) %>% 
        expand_grid(Stat = c("GMR", "CI90_lo", "CI90_hi")) %>% 
        mutate(Cols = paste(PKparam, Stat, sep = "__")) %>% 
        pull(Cols)
    
    suppressMessages(
        Forest <- bind_rows(Forest_l) %>% 
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
                   any_of(ColNames))
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




