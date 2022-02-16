#' Plots of enzyme abundance changes over time to match Consultancy template
#'
#' Using simulated enzyme-abundance data, make publication-quality graphs that
#' match the consultancy template formatting instructions. The options listed in
#' the function \code{\link{ct_plot}}, e.g., specifying the figure type or the
#' axis limits, etc., work here as long as they make sense for plotting
#' enzyme-abundance data.
#'
#' @param sim_enz_dataframe the data.frame of enzyme abundance data obtained
#'   from running the function \code{\link{extractEnzAbund}}
#' @param figure_type The type of figure to graph. Default is "percentiles". See
#'   \code{figure_type} options and their explanations for the function
#'   \code{\link{ct_plot}}.
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
                     ...,
                     sim_data_file = NA,
                     enzyme = "CYP3A4",
                     tissue = "liver"){
    
    # Error catching
    if(is.na(sim_data_file) & !("data.frame" %in% class(sim_enz_dataframe))){
        stop("You cannot leave both sim_data_file and sim_enz_dataframe as NA. You must list one of these to specify which data to include in the graph.")
    }
    
    if(tissue %in% c("colon", "small intestine", "liver", "kidney") == FALSE){
        stop("The tissue you entered is not one of the options. Please select one of 'colon', 'small intestine', 'liver', or 'kidney' for the tissue.")
    }
    
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
    
    if(tissue %in% c("colon", "small intestine")){
        Data <- Data %>% filter(Tissue == tissue)
    }
    
    ct_plot(sim_obs_dataframe = Data, figure_type = figure_type, ...)
}



