#' Pull concentration-time data from multiple Simcyp Simulator output files
#'
#' \code{extractConcTime_mult} is meant to be used in conjunction with
#' \code{\link{ct_plot_overlay}} to create single graphs with overlaid
#' concentration-time data from multiple Simcyp Simulator output files for easy
#' comparisons. \strong{A couple of notes:}\enumerate{\item{If any of the time
#' or concentration units differ between files, the units from the 1st file
#' listed will be used and all other files' data will be adjusted to match.}
#' \item{If you list multiple files, multiple tissues, and/or multiple compounds
#' to extract (see options below), this will extract \emph{all} possible
#' variations of them. For example, if you ask for "File A" and "File B" and
#' then also ask for "substrate" and "primary metabolite 1", you will get the
#' substrate and primary metabolite 1 data from \emph{both} files.}}
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
#' @param tissue From which tissue(s) should the desired concentrations be
#'   extracted? The default is plasma for typical plasma concentration-time
#'   data. Other options are "blood" or any tissues included in "Sheet Options",
#'   "Tissues" in the simulator. All possible options: "plasma", "blood",
#'   "unbound blood", "unbound plasma", "additional organ", "adipose", "bone",
#'   "brain", "feto-placenta", "GI tissue", "heart", "kidney", "liver", "lung",
#'   "muscle", "pancreas", "peripheral blood", "peripheral plasma", "peripheral
#'   unbound blood", "peripheral unbound plasma", "portal vein blood", "portal
#'   vein plasma", "portal vein unbound blood", "portal vein unbound plasma",
#'   "skin", or "spleen". Not case sensitive. List all tissues desired as a
#'   character vector, e.g., \code{c("plasma", "blood", "liver")}.
#' @param compoundToExtract For which compound(s) do you want to extract
#'   concentration-time data? Options are "substrate" (default), "primary
#'   metabolite 1", "primary metabolite 2", "secondary metabolite", "inhibitor
#'   1" (this can be an inducer, inhibitor, activator, or suppresesor, but it's
#'   labeled as "Inhibitor 1" in the simulator), "inhibitor 2" for the 2nd
#'   inhibitor listed in the simulation, or "inhibitor 1 metabolite" for the
#'   primary metabolite of inhibitor 1. List all desired compounds as a
#'   character vector, e.g., \code{c("substrate", "primary metabolite 1")}.
#'   \emph{Note:} The simulator will report up to one metabolite for the 1st
#'   inhibitor but no other inhibitor metabolites. (Someone please correct me if
#'   that's wrong! -LS)
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
                                 tissue = "plasma",
                                 compoundToExtract = "substrate",
                                 returnAggregateOrIndiv = "aggregate",
                                 ...){

      # Checking on what combinations of data the user has requested and what
      # data are already present in conctime_DF.
      Requested <- expand.grid(Tissue = tissue,
                              Compound = compoundToExtract,
                              File = sim_data_files)
      # NOTE: Since the object "Requested" is going to be created in this
      # function environment *every time* the user calls on extractConcTime_mult
      # but NOT when the user only calls on extractConcTime, I'm using this as a
      # handle to determine whether to give an error within extractConcTime if
      # the user has requested multiple tissues, compounds, or files. We need
      # extractConcTime to ONLY give ONE set of concentration-time data so that
      # it will work as expected with, e.g., ct_plot. -LS

      if(exists(substitute(conctime_DF))){
            if("File" %in% names(conctime_DF) == FALSE){
                  conctime_DF$File <- "unknown file"
            }

            DataToFetch <- conctime_DF %>% select(File, Tissue, Compound) %>%
                  unique() %>% mutate(ExistsAlready = TRUE) %>%
                  right_join(Requested)



            if(overwrite == FALSE){
                  sim_data_files_topull <- setdiff(sim_data_files,
                                                   conctime_DF$File)
            } else {
                  sim_data_files_topull <- sim_data_files
                  conctime_DF <- conctime_DF %>%
                        filter(!File %in% sim_data_files)
            }
      } else {
            DataToFetch <- Requested
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



