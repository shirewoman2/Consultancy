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
#'   from running the function \code{\link{extractEnzAbund}}. Not quoted. 
#' @param figure_type The type of figure to graph. Default is "percentiles". See
#'   \code{figure_type} options and their explanations for the function
#'   \code{\link{ct_plot}}.
#' @param gut_tissue Which of the two types of gut tissue would you like to
#'   plot? Acceptable options are "colon" or "small intestine". Applicable only
#'   when the tissue extracted with \code{\link{extractEnzAbund}} was gut and
#'   ignored in all other cases.
#' @param ... other arguments passed to the function \code{\link{ct_plot}}
#'
#' @import tidyverse
#' @import readxl
#' @export
#' @examples
#' enz_plot(sim_data_file = "../Example simulator output MD.xlsx",
#'          enzyme = "CYP3A4", tissue = "liver", line_type = "dotted")

enz_plot <- function(sim_enz_dataframe,
                     figure_type = "means only",
                     gut_tissue = "colon",
                     linear_or_log = "linear",
                     ...){
    
    # Error catching ----------------------------------------------------------
    
    # Check whether tidyverse is loaded
	if("package:tidyverse" %in% search() == FALSE){
	    stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
	}
    
    # main body of function -----------------------------------------------

	Data <- sim_enz_dataframe
    
    if(any(unique(Data$Tissue) %in% c("colon", "small intestine"))){
        Data <- Data %>% filter(Tissue == gut_tissue)
    }
    
    ct_plot(ct_dataframe = Data, figure_type = figure_type,
            linear_or_log = linear_or_log, ...)
}

