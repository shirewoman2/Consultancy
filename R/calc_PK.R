#' Calculate basic PK parameters for the 1st and last doses of
#' concentration-time data
#'
#' \code{calc_PK} calculates AUCinf_dose1, AUCt_dose1, AUCtau_last, Cmax_dose1,
#' Cmax_last, tmax_dose1, tmax_last, CLinf_dose1, and CLtau_last for the
#' supplied concentration-time data and, when applicable, the same parameters in
#' the presence of a perpetrator and the ratios of those values for perpetrator
#' / baseline. The input required for \code{ct_dataframe} is pretty specific;
#' please see the help file for that argument. A few
#' notes about the output and calculations: \enumerate{\item{Aggregated PK
#' will be recalculated to include the newly calculated individual PK
#' parameters.} \item{AUCs calculated with the trapezoidal rule are calculated with the
#' linear-up/log-down method. If anyone needs that to be linear up and linear
#' down for some reason, please contact Laura Shireman.} \item{Graphs show the
#' time since the start of the dosing interval on the x axis rather than the time since the
#' first dose.}}
#'
#' @param ct_dataframe a data.frame of concentration-time data in the same
#'   format as those created by running \code{extractConcTime},
#'   \code{extractConcTime_mult}, \code{extractObsConcTime}, or
#'   \code{extractObsConcTime_mult}.
#' @param which_dose character vector specifying which dose you want the PK for.
#'   Default is \code{which_dose = c("first", "last")} to get the PK for the
#'   first and last doses. If you only want one of those, just specify the one
#'   you want. If you want a specific interval that may or may not match the
#'   first or last dose, then you can specify it here and everything in that
#'   dosing interval will be integrated. For example, say you want PK for the
#'   first dose, the last dose, and also the interval from 24 to
#'   36 hours. Here's how you would specify that: \code{which_dose = c("first",
#'   "last", "24 to 36")}.
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
#' @param existing_exp_details optionally include the output from
#'   running\code{\link{extractExpDetails_mult}} if these data are simulated. If
#'   you include this, we'll use existing_exp_details to do some data checking,
#'   including checking when each dose starts, what the dosing interval is, and
#'   making sure that, if you ask for the last-dose PK, you only get PK for that
#'   dosing interval and not any washout period that may also have been included
#'   in the data. This is not yet set up to check observed data in this manner.
#' @param first_dose_time the time at which the first dose was administered. If
#'   this is left as NA, the default value, this will be set to the minimum time
#'   included in your data unless you have supplied something for
#'   \code{existing_exp_details} (only applicable with simulated data). When
#'   that's the case and you leave this as NA, we'll get the start time for each
#'   compound from that information.
#' @param last_dose_time the time at which the last dose was administered. If
#'   this is left as NA, the default value, we'll assume that the last dose was
#'   administered at the earliest time included in the data for the highest dose
#'   number included in the data unless you have supplied something for
#'   \code{existing_exp_details} (only applicable with simulated data). When
#'   that's the case and you leave this as NA, we'll calculate the last-dose
#'   time for each compound from that information.
#' @param dosing_interval the dosing interval; default is NA which assumes that
#'   all data assigned with a given dose number should be used in calculating PK
#'   values. Cases where this wouldn't necessarily be true: When there's a
#'   washout period included in the data that is longer than the dosing
#'   interval. In that situation, this will assume that only data within the
#'   dosing interval should be used. For example, say that the last QD dose
#'   occurred at 168 h but the washout period lasted until 336 h. For
#'   calculating the last-dose AUCtau, you'd only want the time from 168 to 192
#'   h, so if you set \code{dosing_interval = 24}, that will only calculate
#'   AUCtau using data from 168 to 192 h rather than from 168 to 336 h. This is
#'   only set up to accommodate a single dosing interval and not custom dosing
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
#' @param effort_to_get_elimination_rate How hard should we try to get the
#'   terminal elimination rate for dose 1? Default, "try really hard", means
#'   that first try a nonlinear regression using the base R function
#'   \code{\link[stats]{nls}}, then, if that fails, we'll try expanding the
#'   boundaries for the regression and use the nls2 package function
#'   \code{\link[nls2]{nls2}} and we'll attempt that twice -- the second time
#'   with some minor variations. That takes a while to run, so, if you
#'   \emph{don't} want us to try that hard, other options are "try hard" (we'll
#'   only try the regular nls fit and then one variation on the nls2 fit), "try"
#'   (we'll only try the regular nls fit and then give up if it doesn't work),
#'   or "don't try" to just \emph{not} get the AUCinf or terminal elimination
#'   rates at all.
#' @param report_progress "yes", "no" (default), or "some" for whether to print
#'   messages saying when each combination of file, observed file, trial,
#'   individual, perpetrator, etc. has been completed. This can fill up your
#'   console but can also be reassuring that things are, in fact, progressing.
#'
#' @return returns a list of individual and/or aggregate PK data
#' @export
#'
#' @examples
#' # None yet
#' 
calc_PK <- function(ct_dataframe,
                    which_dose = c("first", "last"), 
                    compound_name = NA, 
                    perpetrator_name = NA,
                    existing_exp_details = NA, 
                    first_dose_time = NA, 
                    last_dose_time = NA,
                    dosing_interval = NA,
                    fit_points_after_x_time = NA,
                    fit_last_x_number_of_points = NA, 
                    omit_0_concs = TRUE,
                    weights = NULL, 
                    effort_to_get_elimination_rate = "try really hard",
                    report_progress = "no", 
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
                    which_dose = which_dose, 
                    compound_name = compound_name, 
                    perpetrator_name = perpetrator_name,
                    existing_PK = NULL,
                    existing_exp_details = existing_exp_details, 
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
                    dosing_interval = dosing_interval,
                    fit_points_after_x_time = fit_points_after_x_time,
                    fit_last_x_number_of_points = fit_last_x_number_of_points, 
                    omit_0_concs = omit_0_concs,
                    weights = weights, 
                    report_progress = report_progress, 
                    returnAggregateOrIndiv = returnAggregateOrIndiv, 
                    return_graphs_of_fits = return_graphs_of_fits,
                    save_graphs_of_fits = save_graphs_of_fits, 
                    ncol = ncol, 
                    nrow = nrow, 
                    fig_width = fig_width, 
                    fig_height = fig_height)
   
   return(Out)
   
}

