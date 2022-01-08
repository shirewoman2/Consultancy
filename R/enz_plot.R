#' Plots of enzyme abundance changes over time to match Consultancy template
#'
#' Using simulated enzyme-abundance data, make publication-quality graphs that
#' match the consultancy template formatting instructions. The options in the
#' function \code{\link{ct_plot}} will work here as long as they make sense for
#' plotting enzyme abundance data.
#'
#' @param sim_data_file name of the Excel file containing the simulated
#'   enzyme-abundance data
#' @param sim_obs_dataframe If you have already extracted the concentration-time
#'   data using the function \code{\link{extractEnzAbund}}, you can enter the
#'   name of the output data.frame from that function instead of re-reading the
#'   Excel file, which is the bottleneck step in running this function.
#' @param enzyme the enzyme, e.g., "CYP3A4", "UGT1A1", etc. Spaces or hyphens in
#'   enzyme names will be ignored. Not case sensitive.
#' @param tissue the tissue to plot. Options are "liver" (default), "gut", or
#'   "kidney".
#' @param ... Other arguments passed to the function \code{\link{ct_plot}}
#' 
#' @import tidyverse
#' @import readxl
#' @export
#' @examples
#' extractEnzAbund(sim_data_file = "../Example simulator output MD.xlsx",
#'                 enzyme = "CYP3A4", tissue = "liver")

enz_plot <- function(sim_data_file = NA,
                     sim_enz_dataframe = NA,
                     enzyme = "CYP3A4",
                     tissue = "liver",
                     ...){
    
    # Error catching
    if(is.na(sim_data_file) & is.na(sim_enz_dataframe)){
        stop("You cannot leave both sim_data_file and sim_enz_dataframe as NA. You must list one of these to specify which data to include in the graph.")
    }
    
    if(complete.cases(sim_enz_dataframe)){
        Data <- sim_enz_dataframe
    } else {
        Data <- extractEnzAbund(sim_data_file = sim_data_file, 
                                enzyme = enzyme, 
                                tissue = tissue, 
                                returnAggregateOrIndiv = c("aggregate", "individual"))
    }
    
    ct_plot(sim_obs_dataframe = Data)
}



