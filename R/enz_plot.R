#' Plots of enzyme abundance changes over time to match Consultancy template
#'
#' Using simulated enzyme-abundance data, make publication-quality graphs that
#' match the consultancy template formatting instructions. The options listed in
#' the function \code{\link{ct_plot}}, e.g., specifying the figure type or the
#' axis limits, etc., work here as long as they make sense for plotting
#' enzyme-abundance data. If you would like to plot a specific dose number for
#' "time_range", you must also specify which compound's dose number you want,
#' e.g., "dose 1 substrate", "last dose inhibitor 1", "doses 4 to 6 substrate".
#'
#' @param sim_enz_dataframe the data.frame of enzyme abundance data obtained
#'   from running the function \code{\link{extractEnzAbund}}
#' @param figure_type The type of figure to graph. Default is "percentiles". See
#'   \code{figure_type} options and their explanations for the function
#'   \code{\link{ct_plot}}.
#' @param gut_tissue Which of the two types of gut tissue would you like to
#'   plot? Acceptable options are "colon" or "small intestine". Applicable only
#'   when the tissue extracted with \code{\link{extractEnzAbund}} was gut and
#'   ignored in all other cases.
#' @param ... other arguments passed to the function \code{\link{ct_plot}}
#' @param sim_data_file HISTORICAL, BACK-COMPATIBILITY PURPOSES ONLY: name of
#'   the Excel file containing the simulated enzyme-abundance data
#' @param enzyme HISTORICAL, BACK-COMPATIBILITY PURPOSES ONLY: the enzyme to
#'   plot, e.g., "CYP3A4" (default), "UGT1A1", etc. Spaces or hyphens in enzyme
#'   names will be ignored. Not case sensitive.
#' @param tissue HISTORICAL, BACK-COMPATIBILITY PURPOSES ONLY: the tissue to
#'   plot. Options are "liver" (default), "small intestine", "colon", or
#'   "kidney".
#'
#' @import tidyverse
#' @import readxl
#' @export
#' @examples
#' enz_plot(sim_data_file = "../Example simulator output MD.xlsx",
#'          enzyme = "CYP3A4", tissue = "liver", line_type = "dotted")

enz_plot <- function(sim_enz_dataframe = NA,
                     figure_type = "percentiles",
                     gut_tissue = "colon",
                     linear_or_log = "linear",
                     ...,
                     sim_data_file = NA,
                     enzyme = "CYP3A4",
                     tissue = "liver"){
    
    if("data.frame" %in% class(sim_enz_dataframe)){
        Data <- sim_enz_dataframe
    } else {
        Data <- extractEnzAbund(
            sim_data_file = sim_data_file,
            enzyme = enzyme,
            tissue = ifelse(tissue %in% c("colon", "small intestine"),
                            "gut", tissue),
            returnAggregateOrIndiv = c("aggregate", "individual"))
    }
    
    if(any(unique(Data$Tissue) %in% c("colon", "small intestine"))){
        Data <- Data %>% filter(Tissue == gut_tissue)
    }
    
    ct_plot(sim_obs_dataframe = Data, figure_type = figure_type,
            linear_or_log = linear_or_log, ...)
}



