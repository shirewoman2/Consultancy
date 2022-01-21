#' Pull concentration-time data from multiple Simcyp Simulator output files
#'
#' \code{extractConcTime_mult} is meant to be used in conjunction with
#' \code{\link{ct_plot_overlay}} to create single graphs with overlaid
#' concentration-time data from multiple Simcyp Simulator output files for easy
#' comparisons. \strong{A couple of notes:}\enumerate{\item{If any of the time
#' or concentration units differ between files, the units from the 1st file
#' listed will be used and all other files' data will be adjusted to match.}
#' \item{We plan to expand this function to include options such as pulling
#' multiple \emph{kinds} of data from a \emph{single} output file so that you
#' could, for example, overlay bound and unbound plasma drug concentrations or
#' blood vs. plasma concentrations, etc., but that is still under construction.
#' -LS}}
#' @param sim_data_files a character vector of the files you'd like to compare,
#'   e.g., \code{c("MyFile1.xlsx", "MyFile2.xlsx")}. The path should be included
#'   with the file names if they are located somewhere other than your working
#'   directory.
#' @param conctime_DF the data.frame that will contain the output. Because we
#'   can see scenarios where you might want to extract some concentration-time
#'   data, play around with those data, and then later decide you want to pull
#'   more concentration-time data for comparisons, this data.frame can already
#'   exist. When that is the case, this function will \emph{add} data to that
#'   data.frame. It will \emph{not} overwrite existing data unless
#'   \code{overwrite} is set to TRUE.
#' @param overwrite TRUE or FALSE on whether to re-extract the
#'   concentration-time data from output files that are already included in
#'   \code{conctime_DF}. Since pulling data from Excel files is slow, by
#'   default, this will \emph{not} overwrite existing data and instead will only
#'   add data from any Excel files that aren't already included. A situation
#'   where you might want to set this to TRUE would be when you have changed
#'   input parameters for simulations and re-run them.
#' @param ... other arguments passed to the function
#'   \code{\link{extractConcTime}}
#'
#' @return a large data.frame with multiple sets of concentration-time data,
#'   formatted the same way as output from the function
#'   \code{\link{extractConcTime}}
#' @export
#'
#' @examples
#' ConcTimeData <-
#'       extractConcTime_mult(
#'             sim_data_files = c("MyFile1.xlsx", "MyFile2.xlsx"),
#'             conctime_DF = "ConcTimeData",
#'             overwrite = FALSE,
#'             tissue = "unbound plasma") # Note that "tissue" is passed to "extractConcTime".
#'

extractConcTime_mult <- function(sim_data_files,
                                 conctime_DF,
                                 overwrite = FALSE,
                                 returnAggregateOrIndiv = "aggregate",
                                 ...){

      if(exists(substitute(conctime_DF))){
            if("File" %in% names(conctime_DF) == FALSE){
                  conctime_DF$File <- "unknown file"
            }

            if(overwrite == FALSE){
                  sim_data_files_topull <- setdiff(sim_data_files,
                                                   conctime_DF$File)
            } else {
                  sim_data_files_topull <- sim_data_files
                  conctime_DF <- conctime_DF %>%
                        filter(!File %in% sim_data_files)
            }
      } else {
            sim_data_files_topull <- sim_data_files
            conctime_DF <- data.frame()
      }

      Data <- list()

      for(i in sim_data_files_topull){

            Data[[i]] <- extractConcTime(
                  sim_data_file = i,
                  returnAggregateOrIndiv = returnAggregateOrIndiv, ...) %>%
                  mutate(File = i)
            if(i != sim_data_files_topull[1]){
                  Data[[i]] <- match_units(DF_to_adjust = Data[[i]],
                                           goodunits = Data[[1]])
            }
      }

      conctime_DF <- bind_rows(conctime_DF,
                               bind_rows(Data))

      return(conctime_DF)

}



