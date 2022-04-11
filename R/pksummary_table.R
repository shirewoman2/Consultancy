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
#' @param PKparameters the PK parameters to include as a character vector.
#'   Notes: \itemize{ \item{To see the full set of possible parameters to
#'   extract, enter \code{data(AllPKParameters)} into the console.} \item{By
#'   default, if you supply a file for \code{report_input_file}, the PK
#'   parameters included are only those included for the observed data in that
#'   file. Otherwise, the PK parameters will be automatically selected.}
#'   \item{Parameters that don't make sense for your scenario -- like asking for
#'   \code{AUCinf_ss_withInhib} when your simulation did not include an
#'   inhibitor or effector -- will not be included.} \item{tmax will be listed
#'   as median, min, and max rather than mean, lower and higher X\% confidence
#'   interval or X percentiles. Similarly, if you request trial means, the
#'   values for tmax will be the range of medians for the trials rather than the
#'   range of means.}} An example of acceptable input here: \code{c("AUCtau_ss",
#'   "AUCtau_ss_withInhib", "Cmax_ss", "Cmax_ss_withInhib", "AUCtau_ratio_ss",
#'   "Cmax_ratio_ss")}.
#' @param sheet_PKparameters (optional) If you want the PK parameters to be
#'   pulled from a specific tab in the simulator output file, list that tab
#'   here. Most of the time, this should be left as NA.
#' @param mean_type return "arithmetic" or "geometric" (default) means and CVs
#' @param includeCV TRUE or FALSE for whether to include rows for CV in the
#'   table
#' @param includeConfInt TRUE or FALSE for whether to include whatever confidence
#'   intervals were included in the simulator output file. Note that the
#'   confidence intervals are geometric since that's what the simulator outputs
#'   (see an AUC tab and the summary statistics; these values are the ones for,
#'   e.g., "90\% confidence interval around the geometric mean(lower limit)").
#' @param includePerc TRUE or FALSE for whether to include 5th to 95th
#'   percentiles
#' @param concatVariability Would you like to have the variability concatenated?
#'   TRUE or FALSE. If "TRUE", the output will be formatted into a single row
#'   and listed as the lower confidence interval or percentile to the upper CI
#'   or percentile. Ex: "2400 to 2700"
#' @param includeTrialMeans TRUE or FALSE for whether to include the range of
#'   trial means for a given parameter. Note: This is calculated from individual
#'   values rather than pulled directly from the output.
#' @param prettify_columns TRUE or FALSE for whether to make easily
#'   human-readable column names. TRUE makes pretty column names such as "AUC0
#'   to inf (h*ng/mL)" whereas FALSE leaves the column with the R-friendly name
#'   from \code{\link{extractPK}}, e.g., "AUCinf_dose1".
#' @param checkDataSource TRUE or FALSE: Include in the output a data.frame that
#'   lists exactly where the data were pulled from the simulator output file.
#'   Useful for QCing.
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.csv". If you leave off ".csv", it will be saved as a csv file. If you
#'   requested both the table and the QC info, the QC file will have "- QC"
#'   added to the end of the file name.
#'
#' @return a data.frame of PK summary data or a list of that data.frame (named
#'   "Table") plus information on where the values came from for QCing (named
#'   "QC")
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
                            sheet_PKparameters = NA, 
                            mean_type = NA,
                            includeCV = TRUE,
                            includeConfInt = TRUE,
                            includePerc = FALSE,
                            includeTrialMeans = FALSE,
                            concatVariability = FALSE,
                            prettify_columns = TRUE,
                            checkDataSource = TRUE, 
                            save_table = NA){
    
    Out <- so_table(sim_data_file = sim_data_file, 
                    PKparameters = PKparameters,
                    sheet_PKparameters = sheet_PKparameters,
                    mean_type = mean_type,
                    includeCV = includeCV,
                    includeConfInt = includeConfInt,
                    includePerc = includePerc,
                    includeTrialMeans = includeTrialMeans,
                    concatVariability = concatVariability,
                    prettify_columns = prettify_columns,
                    checkDataSource = checkDataSource, 
                    save_table = save_table)
    
    return(Out)
    
}

