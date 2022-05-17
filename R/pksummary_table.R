#' Make summary PK tables for reports
#'
#' \code{pksummary_table} uses Simcyp Simulator output Excel files to create
#' tables of PK parameters for reports and presentations, including reporting
#' means, CVs, and confidence intervals or percentiles. This function
#' automatically finds the correct tab and the correct cells to pull those data.
#' \strong{Notes:} \itemize{\item{Nearly all parameters are for the
#' \emph{substrate}. We're still validating this for extracting PK for an
#' effector. \strong{A request for assistance:} If you extract PK data for an
#' effector by specifying an Excel sheet for that compound, please check the
#' values and tell Laura Shireman how well it works!} \item{Currently, the
#' output column titles list units of ng, mL, and h for AUC and Cmax, and the
#' function doesn't actually check what units are present in the data. If your
#' units are something else, our apologies, but please change the units in the
#' column titles when you use the output table. (The values in the table are
#' fine.) We're working on making this detect what the units were and print
#' those.} \item{ If the simulator output Excel file lives on SharePoint, you'll
#' need to close it or this function will just keep running and not generate any
#' output while it waits for access to the file.}}
#'
#' @param sim_data_file the simulator output file
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
#'   console.}
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
#' @param sheet_PKparameters (optional) If you want the PK parameters to be
#'   pulled from a specific tab in the simulator output file, list that tab
#'   here. Most of the time, this should be left as NA.
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default) or "blood" (possible but not as thoroughly
#'   tested).
#' @param mean_type return "arithmetic" or "geometric" (default) means and CVs
#' @param includeCV TRUE (default) or FALSE for whether to include rows for CV
#'   in the table
#' @param includeConfInt TRUE (default) or FALSE for whether to include whatever
#'   confidence intervals were included in the simulator output file. Note that
#'   the confidence intervals are geometric since that's what the simulator
#'   outputs (see an AUC tab and the summary statistics; these values are the
#'   ones for, e.g., "90\% confidence interval around the geometric mean(lower
#'   limit)").
#' @param includePerc TRUE or FALSE (default) for whether to include 5th to 95th
#'   percentiles
#' @param concatVariability TRUE or FALSE (default) for whether to concatenate
#'   the variability. If "TRUE", the output will be formatted into a single row
#'   and listed as the lower confidence interval or percentile to the upper CI
#'   or percentile, e.g., "2400 to 2700". Please note that the current
#'   SimcypConsultancy template lists one row for each of the upper and lower
#'   values, so this should be set to FALSE for official reports.
#' @param includeTrialMeans TRUE or FALSE (default) for whether to include the
#'   range of trial means for a given parameter. Note: This is calculated from
#'   individual values rather than pulled directly from the output.
#' @param prettify_columns TRUE (default) or FALSE for whether to make easily
#'   human-readable column names. TRUE makes pretty column names such as "AUCinf
#'   (h*ng/mL)" whereas FALSE leaves the column with the R-friendly name from
#'   \code{\link{extractPK}}, e.g., "AUCinf_dose1".
#' @param prettify_effector_name TRUE (default) or FALSE on whether to make
#'   effector name prettier in the prettified column titles. This was designed
#'   for simulations where the effector is one of the standard options for the
#'   simulator, and leaving \code{prettify_effector_name = TRUE} will make the
#'   name of that effector (or effectors if there are any effector metabolites
#'   or other effectors present) be something more human readable. For example,
#'   "SV-Rifampicin-MD" will become "rifampicin", and "Sim-Ketoconazole-200 mg
#'   BID" will become "ketoconazole". Set it to the name you'd prefer to see in
#'   your column titles if you would like something different. For example,
#'   \code{prettify_effector_name = "Drug ABC"}
#' @param checkDataSource TRUE (default) or FALSE for whether to include in the
#'   output a data.frame that lists exactly where the data were pulled from the
#'   simulator output file. Useful for QCing.
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.csv". If you leave off ".csv", it will still be saved as a csv file.
#'   If you requested both the table and the QC info, the QC file will have "-
#'   QC" added to the end of the file name.
#'
#' @return Returns a data.frame of PK summary data or a list of that data.frame
#'   (named "Table") plus information on where the values came from for QCing
#'   (named "QC")
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
                            tissue = "plasma",
                            mean_type = NA,
                            includeCV = TRUE,
                            includeConfInt = TRUE,
                            includePerc = FALSE,
                            includeTrialMeans = FALSE,
                            concatVariability = FALSE,
                            prettify_columns = TRUE,
                            prettify_effector_name = TRUE, 
                            checkDataSource = TRUE, 
                            save_table = NA){
    
    Out <- so_table(sim_data_file = sim_data_file, 
                    PKparameters = PKparameters,
                    sheet_PKparameters = sheet_PKparameters,
                    tissue = tissue,
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

