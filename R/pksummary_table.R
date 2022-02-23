#' Make summary PK tables for reports
#'
#' \code{pksummary_table} uses Simcyp Simulator output Excel files to create
#' tables of PK parameters for reports and presentations, including reporting
#' means, CVs, and confidence intervals or percentiles. This function
#' automatically finds the correct tab and the correct cells to pull those data.
#' \strong{Note:} If the simulator output Excel file lives on SharePoint, you'll
#' need to close it or this function will just keep running and not generate any
#' output while it waits for access to the file.
#'
#' @param sim_data_file the simulator output file
#' @param PKparameters the PK parameters to include as a character vector. To
#'   see the full set of possible parameters to extract, enter
#'   \code{data(AllPKParameters)} into the console. If left as NA, the PK
#'   parameters will be automatically selected. An example of acceptable input
#'   here: \code{c("AUCtau_ss", "AUCtau_ss_withInhib", "Cmax_ss",
#'   "Cmax_ss_withInhib", "AUCtau_ratio_ss", "Cmax_ratio_ss")}. Parameters that
#'   don't make sense for your scenario -- like asking for
#'   \code{AUCinf_ss_withInhib} when your simulation did not include an
#'   inhibitor or effector -- will not be included.
#' @param mean_type return "arithmetic" or "geometric" (default) means and CVs
#' @param variability_option What type of variability would you like the table
#'   to include? Options are: "90\% CI", "95\% CI", "95th percentiles", or any
#'   combination of those, e.g. \code{variability_option = c("90\% CI", "95th
#'   percentiles"). Note that the confidence intervals are geometric since
#'   that's what the simulator outputs (see an AUC tab and the summary
#'   statistics; these values are the ones for, e.g., "90% confidence interval
#'   around the geometric mean(lower limit)"). The CV will automatically be
#'   included.}
#' @param concatVariability Would you like to have the variability concatenated?
#'   TRUE or FALSE. If "TRUE", the output will be formatted into a single row
#'   and listed as the lower confidence interval or percentile to the upper CI
#'   or percentile. Ex: "2400 to 2700"
#' @param includeHalfLife TRUE or FALSE for whether to include half life as a
#'   parameter in the output table
#' @param includeTrialMeans TRUE or FALSE for whether to include the range of
#'   trial means for a given parameter. Note: This is calculated from individual
#'   values rather than pulled directly from the output.
#' @param includeCV TRUE or FALSE for whether to include rows for CV in the
#'   table
#' @param checkDataSource TRUE or FALSE: Include in the output a data.frame that
#'   lists exactly where the data were pulled from the simulator output file.
#'   Useful for QCing.
#'
#' @return a data.frame of PK summary data or a list of that data.frame plus
#'   information on where the values came from for QCing
#' @export
#' @examples
#' pksummary_table(sim_data_file = "Example simulator output - SD MDZ.xlsx")
#' pksummary_table(sim_data_file = "Example simulator output - SD MDZ.xlsx",
#'                 mean_type = "arithmetic", concatVariability = TRUE,
#'                 checkDataSource = FALSE)
#' pksummary_table(sim_data_file = "Example simulator output - SD MDZ.xlsx",
#'                 mean_type = "arithmetic", includeTrialMeans = TRUE,
#'                 includeCV = FALSE) # Howie's preferred approach. :-) 


pksummary_table <- function(sim_data_file,
                            PKparameters = NA,
                            mean_type = "geometric",
                            variability_option = "90% CI",
                            concatVariability = FALSE,
                            includeHalfLife = FALSE,
                            includeTrialMeans = FALSE,
                            includeCV = TRUE,
                            checkDataSource = TRUE){
    
    FromPKSumTable <- TRUE
    
    Out <- so_table(sim_data_file = sim_data_file, 
                    PKparameters = PKparameters,
                    mean_type = mean_type,
                    variability_option = variability_option,
                    concatVariability = concatVariability,
                    includeHalfLife = includeHalfLife,
                    includeTrialMeans = includeTrialMeans,
                    includeCV = includeCV,
                    checkDataSource = checkDataSource)
    
    return(Out)
    
}

