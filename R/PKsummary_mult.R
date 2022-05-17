#' Make PK summary tables from multiple simulator output files at once
#'
#' @param observed_PK (optional) If you have a data.frame or an Excel or csv
#'   file with observed PK parameters, supply the data.frame or the full file
#'   name in quotes here, and the simulated-to-observed mean ratios will be
#'   calculated. The supplied data.frame or file \emph{must} include columns for
#'   the simulator output Excel file (title this "File") and each of the PK
#'   parameters you would like to compare, and those column names \emph{must}
#'   match the PK parameter options listed in \code{data(AllPKParameters)}. If
#'   you would like the output table to include the observed data CV for any of
#'   the parameters, add "_CV" to the end of the parameter name, e.g.,
#'   "AUCinf_dose1_CV". Note: Whatever you list for "File" will override
#'   anything specified for the argument \code{sim_data_file}.
#' @param ... other arguments passed to the function \code{\link{pksummary_table}}
#'
#' @return Returns a data.frame with summary PK parameters from multiple
#'   simulator output files
#' @export
#'
#' @examples
#' 
#' # No examples yet
#' 
PKsummary_mult <- function(observed_PK, ...){
    
    # Read in the observed_PK data if it's not already a data.frame.
    if(class(observed_PK) == "character"){
        observed_PKDF <- switch(str_extract(observed_PK, "csv|xlsx"), 
                                "csv" = read.csv(observed_PK), 
                                "xlsx" = xlsx::read.xlsx(observed_PK, 
                                                         sheetIndex = 1))
    }
    
    if("File" %in% names(observed_PKDF) == FALSE){
        stop("You must include a column titled 'File' with the observed PK so that this function knows which simulator output file to pull simulated PK parameters from.")
    }
    
    OutPK <- list()
    for(i in 1:nrow(observed_PKDF)){
        OutPK[[i]] <- pksummary_table(observed_PK = observed_PKDF[i, ]) %>% 
            mutate(File = observed_PK$File[i])
    }
    
    OutPK <- bind_rows(OutPK) %>% 
        select(File, everything())
    
    return(OutPK)
}


