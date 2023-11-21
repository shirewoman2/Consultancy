#' Calculate basic PK parameters for the 1st and last doses of
#' concentration-time data
#'
#' \code{calc_PK} calculates AUCinf_dose1, AUCt_dose1, AUCtau_last, Cmax_dose1,
#' Cmax_last, tmax_dose1, tmax_last, CLinf_dose1, and CLtau_last for the
#' supplied concentration-time data and, when applicable, the same parameters in
#' the presence of a perpetrator and the ratios of those values for perpetrator
#' / baseline. This can accommodate multiple simulations and multiple compounds
#' as long as the dosing regimen was the same. For example, this will do fine
#' with calculating the last-dose PK for two simulations where the last dose of
#' substrate was at t = 168 h but not where one simulation last dose was at t =
#' 168 h and one was at t = 48 h. If you've got a scenario like that, we
#' recommend running this function multiple times, one for each dosing regimen.
#' The input required for \code{ct_dataframe} is pretty specific; please see the
#' help file for that argument.
#'
#' @param ct_dataframe a data.frame of concentration-time data in the same
#'   format as those created by running \code{extractConcTime},
#'   \code{extractConcTime_mult}, \code{extractObsConcTime}, or
#'   \code{extractObsConcTime_mult}. NB: This was only set up to calculate PK
#'   for the 1st and last doses; all other doses will be ignored. If you want PK
#'   for a dose other than those two, we recommend subsetting your data to
#'   include only the 1st dose and whatever dose number you do want and perhaps
#'   running this multiple times in a loop to get all the PK you need. A member
#'   of the R Working Group can help you set this up if you'd like.
#' @param compound_name the name of the compound for which PK are being
#'   calculated, e.g., "midazolam". If you already have a column titled
#'   "Compound" in \code{ct_dataframe}, leaving this as NA will retain that
#'   compound name. If you have more than one compound that you want to specify
#'   -- for example, you're calculating the PK for both the substrate and for
#'   primary metabolite 1 -- you can specify them with a named character vector
#'   like this: \code{compound_name = c("substrate" = "midazolam", "primary
#'   metabolite 1" = "OH-midazolam")}. All possible compound IDs permissible
#'   here: "substrate", "primary metabolite 1", "primary metabolite 2",
#'   "secondary metabolite", "inhibitor 1", "inhibitor 2", or "inhibitor 1
#'   metabolite". This will not affect how any calculations are performed but
#'   will be included in the output data so that you have a record of which
#'   compound the data pertain to.
#' @param perpetrator_name the name of the perpetrator, where applicable, e.g.,
#'   "itraconazole". If you already have a column titled "Inhibitor" in
#'   \code{ct_dataframe}, leaving this as NA will retain that perpetrator name.
#'   This will not affect how any calculations are performed but will be
#'   included in the output data so that you have a record of which compound the
#'   data pertain to.
#' @param first_dose_time the time at which the first dose was administered. If
#'   this is left as NA, the default value, this will be set to 0.
#' @param last_dose_time the time at which the last dose was administered. If
#'   this is left as NA, the default value, we'll assume that the last dose was
#'   administered at the earliest time included in the data for the highest dose
#'   number included in the data. It bears repeating here that this function
#'   only works well if all the data included have the same dosing regimen.
#' @param dose_interval the dosing interval; default is NA which assumes that
#'   all data assigned with a given dose number should be used in calculating PK
#'   values. Cases where this wouldn't necessarily be true: When there's a
#'   washout period included in the data that is longer than the dosing
#'   interval. In that situation, this will assume that only data within the
#'   dosing interval should be used. For example, say that the last QD dose
#'   occurred at 168 h but the washout period lasted until 336 h. For
#'   calculating the last-dose AUCtau, you'd only want the time from 168 to 192
#'   h, so if you set \code{dose_interval = 24}, that will only calculate AUCtau
#'   using data from 168 to 192 h rather than from 168 to 336 h. This is only
#'   set up to accommodate a single dosing interval and not custom dosing
#'   regimens.
#' @param fit_last_x_number_of_points optionally specify that you want to fit
#'   the last X number of points for each dose (replace with whatever number
#'   makes sense for your situation). Default of NA means that we'll fit all the
#'   data after tmax. Keep in mind that this will apply to ALL profiles.
#'   \emph{An important note for fitting simulated data:} If you supply
#'   simulated data with a lot of points, we will only use 100 of those
#'   concentration-time points to describe each dosing interval because
#'   performing, say, 10 trials x 10 subjects = 100 nonlinear regressions with a
#'   thousand points for each dose number requires excessive computing time
#'   needlessly because the regression must minimize the distance between the
#'   fitted curve and every one of those points. Simulated data are pretty
#'   predictable; your fitted parameters will not be less accurate with this
#'   approach. If you supply simulated data, it will probably be less confusing
#'   and yield more predictable results if you specify a value for
#'   \code{fit_points_after_x_time} rather than
#'   \code{fit_last_x_number_of_points}.
#' @param fit_points_after_x_time optionally specify that you want to fit only
#'   points after a certain time after the most-recent dose. Default of NA means
#'   that we'll fit all the data after tmax. Keep in mind that this will apply
#'   to ALL profiles.
#' @param omit_0_concs TRUE (default) or FALSE for whether to omit any points
#'   where the concentration = 0 since A) they were presumably below the LLOQ
#'   and B) they will mess up weighting should you choose to use a "1/y" or
#'   "1/y^2" weighting scheme. Concentrations of 0 at t0 will be retained,
#'   though, to allow for a more accurate calculation of the absorption-phase
#'   contribution to AUCinf and since 0 values at t0 would not be included in
#'   any regression of the elimination phase anyway.
#' @param weights Weighting scheme to use for the regression. User may supply a
#'   numeric vector of weights to use or choose from "1/x", "1/x^2", "1/y" or
#'   "1/y^2". If left as NULL, no weighting scheme will be used. Be careful that
#'   you don't have any infinite values or this will fail!
#' @param returnAggregateOrIndiv return aggregate and/or individual PK
#'   parameters? Options are "aggregate", "individual", or "both" (default).
#' @param return_graphs_of_fits TRUE (default) or FALSE for whether to return a
#'   list of the graphs showing the fitted data any time the dose 1 AUC was
#'   extrapolated to infinity.
#' @param save_graphs_of_fits TRUE or FALSE (default) for whether to save png
#'   files of graphs showing the fitted data for any time the dose 1 AUC was
#'   extrapolated to infinity. This will save one png per set of file,
#'   compoundID, inhibitor, and tissue.
#' @param fig_width figure width in inches for saving graphs of fits (you may
#'   want this to be huge if there are a lot of profiles). Defaults to 8.5.
#' @param fig_height figure height in inches for saving graphs of fits (you may
#'   want this to be huge if there are a lot of profiles). Defaults to 11.
#' @param ncol number of columns to use for graphing the fitted data
#' @param nrow number of rows to use for graphing the fitted data
#'
#' @return returns a list of individual and/or aggregate PK data
#' @export
#'
#' @examples
#' # None yet
#' 
calc_PK <- function(ct_dataframe,
                    compound_name = NA, 
                    perpetrator_name = NA,
                    first_dose_time = NA, 
                    last_dose_time = NA,
                    dose_interval = NA,
                    fit_points_after_x_time = NA,
                    fit_last_x_number_of_points = NA, 
                    omit_0_concs = TRUE,
                    weights = NULL, 
                    returnAggregateOrIndiv = "both", 
                    return_graphs_of_fits = TRUE,
                    save_graphs_of_fits = FALSE, 
                    ncol = NULL, 
                    nrow = NULL, 
                    fig_width = 8.5, 
                    fig_height = 11){
   
   # Error catching and setting up --------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   Out <- recalc_PK(ct_dataframe = ct_dataframe,
                    compound_name = compound_name, 
                    perpetrator_name = perpetrator_name,
                    existing_PK = NULL,
                    compoundID_match = NA,
                    inhibitor_match = NA, 
                    tissue_match = NA, 
                    individual_match = NA, 
                    trial_match = NA,
                    simulated_match = NA, 
                    file_match = NA, 
                    obsfile_match = NA, 
                    dosenum_match = NA, 
                    first_dose_time = first_dose_time, 
                    last_dose_time = last_dose_time,
                    dose_interval = dose_interval,
                    fit_points_after_x_time = fit_points_after_x_time,
                    fit_last_x_number_of_points = fit_last_x_number_of_points, 
                    omit_0_concs = omit_0_concs,
                    weights = weights, 
                    returnAggregateOrIndiv = returnAggregateOrIndiv, 
                    return_graphs_of_fits = return_graphs_of_fits,
                    save_graphs_of_fits = save_graphs_of_fits, 
                    ncol = ncol, 
                    nrow = nrow, 
                    fig_width = fig_width, 
                    fig_height = fig_height)
   
   return(Out)
   
}

