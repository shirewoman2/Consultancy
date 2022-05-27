#' Make PK summary tables from multiple simulator output files at once
#'
#' @param observed_PK (optional) If you have a data.frame or an Excel or csv
#'   file with observed PK parameters, supply the data.frame or the full file
#'   name in quotes here, and the simulated-to-observed mean ratios will be
#'   calculated. The supplied data.frame or file \emph{must} include columns for
#'   the simulator output Excel file (title this "File") and each of the PK
#'   parameters you would like to compare, and those column names \emph{must}
#'   match the PK parameter options listed in
#'   \code{data(PKParameterDefinitions)}. If you would like the output table to
#'   include the observed data CV for any of the parameters, add "_CV" to the
#'   end of the parameter name, e.g., "AUCinf_dose1_CV". Note: Whatever you list
#'   for "File" will override anything specified for the argument
#'   \code{sim_data_file}.
#' @param PKparameters (optional) the PK parameters to include as a character
#'   vector. Notes: \itemize{
#'
#'   \item{By default, if you have a single-dose simulation, the parameters will
#'   include AUC and Cmax for dose 1, and, if you have a multiple-dose
#'   simulation, AUC and Cmax for the last dose. Also by default, if you have an
#'   effector present, the parameters will include the AUC and Cmax values with
#'   and without the effector as well as those ratios.}
#'
#'   \item{Alternatively, you can specify a vector of any combination of
#'   specific, individual parameters, e.g., \code{c("Cmax_dose1",
#'   "AUCtau_last").} Be sure to encapsulate the parameters you want with
#'   \code{c(...)}! To see the full set of possible parameters to extract, enter
#'   \code{data(PKParameterDefinitions); view(PKParameterDefinitions)} into the
#'   console. Not case sensitive. If you use "_first" instead of "_dose1", that
#'   will also work.}
#'
#'   \item{If you supply observed data using either the argument
#'   \code{report_input_file} or the argument \code{observed_PK}, the PK
#'   parameters included are only those available for the observed data.}
#'
#'   \item{Parameters that don't make sense for your scenario -- such as asking
#'   for \code{AUCinf_dose1_withInhib} when your simulation did not include an
#'   inhibitor or effector -- will not be included.}
#'
#'   \item{tmax will be listed as median, min, and max rather than mean, lower
#'   and higher X\% confidence interval or X percentiles. Similarly, if you
#'   request trial means, the values for tmax will be the range of medians for
#'   the trials rather than the range of means.}} An example of acceptable input
#'   here: \code{c("AUCtau_last", "AUCtau_last_withInhib", "Cmax_last",
#'   "Cmax_last_withInhib", "AUCtau_ratio_last", "Cmax_ratio_last")}.
#' @param ... other arguments passed to the function
#'   \code{\link{pksummary_table}}
#' @param checkDataSource TRUE (default) or FALSE for whether to include in the
#'   output a data.frame that lists exactly where the data were pulled from the
#'   simulator output file. Useful for QCing.
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.csv". If you leave off ".csv", it will still be saved as a csv file.
#'   If you requested both the table and the QC info, the QC file will have "-
#'   QC" added to the end of the file name.#'
#' @return Returns a data.frame with summary PK parameters from multiple
#'   simulator output files
#' @export
#'
#' @examples
#'
#' # No examples yet
#' 
pksummary_mult <- function(sim_data_files, 
                           observed_PK = NA,
                           PKparameters = NA,
                           ..., 
                           checkDataSource = TRUE, 
                           save_table = NA){
    
    # Read in the observed_PK data if it's not already a data.frame. Note that
    # the class of observed_PK will be logical if left as NA.
    if(class(observed_PK) == "character"){
        observed_PKDF <- switch(str_extract(observed_PK, "csv|xlsx"), 
                                "csv" = read.csv(observed_PK), 
                                "xlsx" = xlsx::read.xlsx(observed_PK, 
                                                         sheetIndex = 1))
    }
    
    if("data.frame" %in% class(observed_PK)){
        observed_PKDF <- observed_PK
    }
    
    if(exists("observed_PKDF", inherits = FALSE) &&
       "File" %in% names(observed_PKDF) == FALSE){
        stop("You must include a column titled 'File' with the observed PK so that this function knows which simulator output file to pull simulated PK parameters from.", 
             call. = FALSE)
    }
    
    # Getting the data
    OutPK <- list()
    OutQC <- list()
    
    if(exists("observed_PKDF", inherits = FALSE)){
        
        for(i in 1:nrow(observed_PKDF)){
            temp <- pksummary_table(observed_PK = observed_PKDF[i, ], 
                                    checkDataSource = checkDataSource,
                                    prettify_effector_name = "effector")
            
            OutPK[[i]] <- temp$Table %>% 
                mutate(File = observed_PK$File[i])
            
            if(checkDataSource){
                OutQC[[i]] <- temp$QC
            }
            
            rm(temp)
        }
        
    } else {
        
        for(i in sim_data_files){
            temp <- pksummary_table(sim_data_file = i, 
                                    PKparameters = PKparameters, 
                                    checkDataSource = checkDataSource,
                                    prettify_effector_name = "effector")
            
            OutPK[[i]] <- temp$Table %>% 
                mutate(File = i)
            
            if(checkDataSource){
                OutQC[[i]] <- temp$QC
            }
            
            rm(temp)
            
        }
    }
    
    OutPK <- bind_rows(OutPK) %>% 
        select(File, everything())
    
    OutQC <- bind_rows(OutQC)
    
    Out <- list(OutPK, OutQC)
    names(Out) <- c("Table", "QC")
    
    if(complete.cases(save_table)){
        
        if(str_detect(save_table, "\\.")){
            FileName <- sub("\\..*", ".csv", save_table)
        } else {
            FileName <- paste0(save_table, ".csv")
        }
        
        write.csv(OutPK, FileName, row.names = F)
        
        if(checkDataSource){
            
            if(str_detect(save_table, "\\.")){
                FileName <- sub("\\..*", " - QC.csv", save_table)
            } else {
                FileName <- paste0(save_table, " - QC.csv")
            }
            
            write.csv(OutQC, FileName, row.names = F)
        }
    }
    
    return(Out)
}


