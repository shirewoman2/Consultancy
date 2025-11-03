#' Extract pertinent data from Simulator output files for creating forest plots
#'
#' \code{extractForestData} automatically extracts data for generating forest
#' plots from Simulator output files and formats them for use with
#' \code{\link{forest_plot}} or the forest plot shiny app. This will take some
#' time to run since it needs to open multiple Excel files. For detailed
#' instructions and examples, please see the SharePoint file "Simcyp PBPKConsult
#' R Files - Simcyp PBPKConsult R Files/SimcypConsultancy function examples and
#' instructions/Forest plots/Examples-for-making-forest-plots.docx". (Sorry, we
#' are unable to include a link to it here.)
#'
#' @param sim_data_files a character vector of simulator output files, each in
#'   quotes and encapsulated with \code{c(...)}, NA to extract forest-plot data
#'   for \emph{all} the Excel files in the current folder, or "recursive" to
#'   extract forest-plot data for \emph{all} the Excel files in the current
#'   folder and \emph{all} subfolders.
#' @param PKparameters PK parameters to extract from simulator output files;
#'   default is all possible AUC and Cmax geometric mean ratios for both dose 1
#'   and the last dose simulated. Input must be among "AUCinf_ratio_dose1",
#'   "AUCt_ratio_dose1", "Cmax_ratio_dose1", "AUCtau_ratio_last", or
#'   "Cmax_ratio_last". List them in the order you'd like the columns to appear
#'   in the output.
#' @param compoundToExtract For which compound do you want to extract PK data?
#'   Options are: \itemize{\item{"substrate" (default),} \item{"primary
#'   metabolite 1",} \item{"primary metabolite 2", or} \item{"secondary
#'   metabolite"}}
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default), "unbound plasma", "blood", or "unbound
#'   blood".
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} in all the files included to get all
#'   the details from the "Input Sheet" (e.g., when you ran extractExpDetails
#'   you said \code{exp_details = "Input Sheet"} or \code{exp_details = "all"}),
#'   you can save some processing time by supplying that object here, unquoted.
#'   If left as NA, this function will run \code{extractExpDetails} behind the
#'   scenes to figure out some information about your experimental set up.
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
                              compoundToExtract = "substrate",
                              tissue = "plasma",
                              existing_exp_details = NA, 
                              sheet = NA, 
                              checkDataSource = TRUE, 
                              save_output = NA){
   
   # Error catching -----------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
   
   # Most error catching done within pk_table function. 
   
   # Main body of function ------------------------------------------------
   
   Out <- pk_table(sim_data_files = sim_data_files, 
                        PKparameters = PKparameters, 
                        compoundsToExtract = compoundToExtract, 
                        tissues = tissue, 
                        sheet_PKparameters = sheet, 
                        existing_exp_details = existing_exp_details, 
                        checkDataSource = checkDataSource, 
                        rounding = "none",
                        save_table = save_output, 
                        extract_forest_data = TRUE)
   
   return(Out$ForestData)
   
}


