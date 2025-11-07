#' Recalculate the PK for specific concentration-time profiles after running
#' \code{\link{calc_PK}}
#'
#' @description \code{recalc_PK} takes as input the output from
#'   \code{\link{calc_PK}} and recalculates the PK for any concentration-time
#'   profiles specified.
#'
#'   A few notes about the output and calculations:
#'   \enumerate{\item{Aggregated PK
#' will be recalculated to include the newly calculated individual PK
#' parameters.} \item{Graphs show the time since the start of the dosing
#' interval on the x axis rather than the time since the first dose.}
#' \item{The ID you'll see listed on graphs or in the console if you ask for
#' all of the progress to be shown is the compound ID, any perpetrator present,
#' the tissue, the individual, the trial, whether the data were simulated or
#' observed, the file name, the observed file name, and the dose number.}}
#'
#' @param ct_dataframe a data.frame of concentration-time data in the same
#'   format as those created by running \code{extractConcTime},
#'   \code{extractConcTime_mult}, \code{extractObsConcTime}, or
#'   \code{extractObsConcTime_mult}. Here are the columns that are will be
#'   included in your data (some are optional):
#'
#'   \describe{\item{File (optional)}{as in, simulation file; can be a placeholder for all
#'   the concentration-time data the you want to be considered together as a
#'   set. If omitted, we'll assume that all the data should be evaluated as a
#'   single dataset.}
#'   \item{ObsFile (optional)}{Observed-data file name. If omitted, we'll
#'   assume that all the data should be evaluated as a single dataset.}
#'   \item{CompoundID}{the compound ID, e.g., "substrate", "inhibitor 1",
#'   "primary metabolite 1", etc. To see all possible compounds, run
#'   \code{view(AllCompounds)}}
#'   \item{Inhibitor (optional)}{the name of any perpetrator present. For baseline data,
#'   set this to "none". If this is missing, we'll assume you had all baseline data.}
#'   \item{DoseNum}{the dose number. If you don't want to fill this out manually,
#'   try using \code{\link{calc_dosenumber}} to calculate it for you.}
#'   \item{Dose_X}{the dose amount. Replace "X" with "sub" for substrate,
#'   "inhib" for inhibitor 1, and "inhib2" for inhibitor 2.}
#'   \item{Tissue (optional)}{the tissue, e.g., "plasma". To see all possible tissues, run
#'   \code{view(AllTissues)}. If missing, we'll assume you had plasma data.}
#'   \item{Individual}{the individual subject ID}
#'   \item{Simulated}{TRUE or FALSE for whether these data were simulated}
#'   \item{Conc}{the concentration}
#'   \item{Time}{the time}
#'   }
#'
#'   Concentration, time, and dosing units are not considered in these
#'   calculations, so the units for the PK in the results will match whatever
#'   concentration, time, and dosing units were used in your input data.
#'
#' @param which_dose character vector specifying which dose you want the PK for.
#'   Default is \code{which_dose = c("first", "last")} to get the PK for the
#'   first and last doses. If you only want one of those, just specify the one
#'   you want. If you want a specific interval that may or may not match the
#'   first or last dose, then you can specify it here and everything in that
#'   dosing interval will be integrated. For example, say you want PK for the
#'   first dose, the last dose, and also the interval from 24 to
#'   36 hours. Here's how you would specify that: \code{which_dose = c("first",
#'   "last", "24 to 36")}.
#' @param existing_PK the output from \code{\link{calc_PK}}
#' @param existing_exp_details optionally include the output from
#'   running\code{\link{extractExpDetails_mult}} if these data are simulated. If
#'   you include this, we'll use existing_exp_details to do some data checking,
#'   including checking when each dose starts, what the dosing interval is, and
#'   making sure that, if you ask for the last-dose PK, you only get PK for that
#'   dosing interval and not any washout period that may also have been included
#'   in the data. This is not yet set up to check observed data in this manner.
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
#' @param ID_match match specific data set IDs. These are the labels that show
#'   up in graphs of fitted data, e.g., "substrate none plasma 22 10 simulated
#'   abc1a-10mg-xlsx obsdatafile.xlsx NA 1". That ID is, in order, compound ID,
#'   what perpetrator was present, tissue, individual, trial, simulated or
#'   observed data, Simulator file name, observed data file name, and the dose
#'   number. Presumably, you're recalculating PK because you want to use
#'   different points for determining the terminal elimination rate, so the dose
#'   number will probably always be 1 here.
#' @param compoundID_match any values in the CompoundID column to match in the
#'   data; matching PK profiles will be recalculated
#' @param inhibitor_match any values in the Inhibitor column to match in the
#'   data; matching PK profiles will be recalculated
#' @param tissue_match any values in the Tissue column to match in the data;
#'   matching PK profiles will be recalculated
#' @param individual_match any values in the Individual column to match in the
#'   data; matching PK profiles will be recalculated
#' @param trial_match any values in the Trial column to match in the data;
#'   matching PK profiles will be recalculated
#' @param simulated_match any values in the Simulated column to match in the
#'   data; matching PK profiles will be recalculated
#' @param file_match any values in the File column to match in the data;
#'   matching PK profiles will be recalculated
#' @param obsfile_match any values in the ObsFile column to match in the data;
#'   matching PK profiles will be recalculated
#' @param dosenum_match any values in the DoseNum column to match in the data;
#'   matching PK profiles will be recalculated
#' @param first_dose_time the time at which the first dose was administered. If
#'   this is left as NA, the default value, this will be set to the minimum time
#'   included in your data where concentrations were above 0 unless you have
#'   supplied something for \code{existing_exp_details} (only applicable with
#'   simulated data). When that's the case and you leave this as NA, we'll get
#'   the start time for each compound from that information. If you're not
#'   supplying simulated data and something for \code{existing_exp_details}, we
#'   strongly recommend filling in this value rather than letting us estimate
#'   it. You'll get more consistent results when we know for sure when to start
#'   integrating.
#' @param last_dose_time the time at which the last dose was administered. If
#'   this is left as NA, the default value, we'll assume that the last dose was
#'   administered at the earliest time included in the data for the highest dose
#'   number included in the data unless you have supplied something for
#'   \code{existing_exp_details} (only applicable with simulated data). When
#'   that's the case and you leave this as NA, we'll calculate the last-dose
#'   time for each compound from that information. If you're not supplying
#'   simulated data and something for \code{existing_exp_details}, we strongly
#'   recommend filling in this value rather than letting us estimate it. You'll
#'   get more consistent results when we know for sure when to start
#'   integrating.
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
#'   "1/y^2" weighting scheme. \strong{Concentrations of 0 at t0 will be retained,
#'   though, to allow for a more accurate calculation of the absorption-phase
#'   contribution to AUCinf} and since 0 values at t0 would not be included in
#'   any regression of the elimination phase anyway.
#' @param add_t0_point TRUE (default) or FALSE for whether to add a point a t =
#'   0 with a concentration of 0 to the data to be integrated. This ONLY applies
#'   to dose 1 data. If your data did not include a t0 point and you leave this
#'   off, you will miss the initial part of the AUC. If there is already a point
#'   at t = 0, this will be ignored and nothing in your data will change.
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
#'   compoundID, inhibitor, and tissue. Note: This isn't quite working, so you
#'   may want to just look at the graphs rather than saving them until I figure
#'   out the problem. It's not saving all the graphs it should. -LSh
#' @param fig_width figure width in inches for saving graphs of fits (you may
#'   want this to be huge if there are a lot of profiles). Defaults to 8.5.
#' @param fig_height figure height in inches for saving graphs of fits (you may
#'   want this to be huge if there are a lot of profiles). Defaults to 11.
#' @param ncol number of columns to use for graphing the fitted data
#' @param nrow number of rows to use for graphing the fitted data
#' @param effort_to_get_elimination_rate How hard should we try to get the
#'   terminal elimination rate for dose 1? Default, "try really hard", means
#'   that we'll first try a nonlinear regression using the base R function
#'   \code{\link[stats]{nls}}, then, if that fails, we'll try expanding the
#'   boundaries for the regression and use the nls2 package function
#'   \code{\link[nls2]{nls2}} and we'll attempt that twice -- the second time
#'   with some minor variations. That takes a while to run, so, if you
#'   \emph{don't} want us to try that hard, other options are "try hard" (we'll
#'   only try the regular nls fit and then one variation on the nls2 fit), "try"
#'   (we'll only try the regular nls fit and then give up if it doesn't work),
#'   or "don't try" to just \emph{not} get the AUCinf or terminal elimination
#'   rates at all.
#' @param report_progress "yes", "no", or "some" (default) for whether to print
#'   messages saying when each combination of file, observed file, trial,
#'   individual, perpetrator, etc. has been completed. Setting this to "yes" can
#'   fill up your console pretty rapidly but can also be reassuring that things
#'   are, in fact, progressing.
#' @param trapezoidal_method which trapezoidal method should be used for
#'   calculating the AUC? Options are "LULD" (default) for "linear up/log down"
#'   or "linear".
#'
#' @return returns a list of individual and/or aggregate PK data
#' @export
#'
#' @examples
#' # None yet
#'
#' 
recalc_PK <- function(ct_dataframe,
                      which_dose = c("first", "last"), 
                      compound_name = NA, 
                      perpetrator_name = NA,
                      existing_PK,
                      existing_exp_details = NA, 
                      ID_match = NA, 
                      compoundID_match = NA,
                      inhibitor_match = NA, 
                      tissue_match = NA, 
                      individual_match = NA, 
                      trial_match = NA,
                      simulated_match = NA, 
                      file_match = NA, 
                      obsfile_match = NA, 
                      dosenum_match = NA, 
                      first_dose_time = NA, 
                      last_dose_time = NA,
                      dosing_interval = NA,
                      trapezoidal_method = "LULD", 
                      fit_points_after_x_time = NA,
                      fit_last_x_number_of_points = NA, 
                      add_t0_point = TRUE, 
                      omit_0_concs = TRUE,
                      weights = NULL, 
                      effort_to_get_elimination_rate = "try really hard",
                      report_progress = "some", 
                      returnAggregateOrIndiv = "both", 
                      return_graphs_of_fits = TRUE,
                      save_graphs_of_fits = FALSE, 
                      ncol = NULL, 
                      nrow = NULL, 
                      fig_width = 8.5, 
                      fig_height = 11){
   
   # NB: Even though this is called "recalc_PK", it's actually the workhorse
   # function for calculating ALL PK, regardless of whether it's been calculated
   # previously. The calc_PK function calls on this and just doesn't supply
   # anything for existing_PK.
   
   # Error catching and setting up --------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
   
   if("character" %in% class(which_dose) == FALSE){
      warning(wrapn("You requested something for the argument `which_dose` that was not a character vector, which is what we were expecting. We will return PK for the default doses: the first and the last."), 
              call. = FALSE)
      which_dose = c("first", "last")
   }
   
   if(trapezoidal_method %in% c("LULD", "linear") == FALSE){
      warning(wrapn("The only options for the trapezoidal method are 'LULD' for 'linear up/log down' or 'linear', and you have requested something else. We'll use the default of 'LULD'."), 
              .call = FALSE)
      
      trapezoidal_method <- "LULD"
   }
   
   which_dose <- sub("1st", "first", tolower(which_dose))
   SpecialInterval <- which_dose[str_detect(which_dose, "[0-9]")]
   which_dose_main <- which_dose[!str_detect(which_dose, "[0-9]")]
   if(length(SpecialInterval) > 0){
      SpecialInterval <- gsub(" ", "", SpecialInterval)
      SpecialInterval <- lapply(SpecialInterval,
                                FUN = function(x) as.numeric(str_split_1(x, "to")))
      if(any(lapply(SpecialInterval, length) != 2)){
         warning(wrapn("Something is amiss with how you have specified a specific interval to integrate because we're not sure what the start and end times should be. We'll have to ignore the requested specific interval."), 
                 call. = FALSE)
         
         SpecialInterval <- SpecialInterval[which(lapply(SpecialInterval, length) == 2)]
      }
   }
   
   report_progress <- tolower(report_progress)
   if(report_progress %in% c("yes", "no", "some") == FALSE){
      warning(wrapn("The options for report_progress are `yes`, `no`, or `some`, and you've entered something else. We'll set this to the default value of `some`."), 
              call. = FALSE)
   }
   
   if(complete.cases(fit_points_after_x_time) & complete.cases(fit_last_x_number_of_points)){
      warning(wrapn("You requested that we fit both all points after time = X and also the last X number of points. You only get to use one of these options, so we'll use all points after time = X."), 
              call. = FALSE)
   }
   
   if(is.na(first_dose_time) & "first" %in% which_dose & 
      "logical" %in% class(existing_exp_details)){
      warning(wrapn("IMPORTANT: You have not specified the time of the 1st dose. We'll assume it's the minimum time included in your data. Please make SURE that this is correct as incorrect t0 specification can mess up calculations."), 
              call. = FALSE)
   }
   
   if(is.na(last_dose_time) & any(ct_dataframe$DoseNum > 1) & 
      "logical" %in% class(existing_exp_details) & "last" %in% which_dose){
      warning(wrapn("You have not specified the time of the last dose. We'll assume it's the minimum time for the maxmimum dose number in your data."), 
              call. = FALSE)
   }
   
   # Tidying compound names
   if(length(compound_name) == 1 & complete.cases(compound_name) & 
      is.null(names(compound_name))){
      compound_name <- c("substrate" = compound_name)
   }
   
   # Checking for any missing columns that we could just fill in
   if("Compound" %in% names(ct_dataframe) == FALSE || 
      all(is.na(ct_dataframe$Compound))){
      if(any(complete.cases(compound_name)) &&
         any(names(compound_name) %in% AllRegCompounds$CompoundID) == FALSE){
         warning(wrapn("Some of the compound IDs used for naming the values for `compound_name` are not among the permissible compound IDs, so we won't be able to supply a compound name for any of the compound IDs listed. Please check the help file for what values are acceptable."), 
                 call. = FALSE)
         
         compound_name <- rep(NA, each = nrow(AllRegCompounds))
         names(compound_name) <- AllRegCompounds$CompoundID
      } else {
         Missing <- setdiff(AllRegCompounds$CompoundID, names(compound_name))
         ToAdd <- rep(NA, each = length(Missing))
         names(ToAdd) <- Missing
         compound_name <- ToAdd
         rm(Missing, ToAdd)
      }
   }
   
   # NOTHING should be grouped or this will generate a million annoying messages
   # about adding grouping variables!
   ct_dataframe <- ungroup(ct_dataframe)
   
   if("File" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$File <- "unknown file"
   }
   
   ct_dataframe$File[is.na(ct_dataframe$File)] <- "unknown file"
   
   if("ObsFile" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$ObsFile <- "unknown obs file"
   }
   
   ct_dataframe$ObsFile[is.na(ct_dataframe$ObsFile)] <- "unknown obs file"
   
   # If they already had a compound name in the data, this will change it to
   # what they set with compound_name, but if they did not specify
   # compound_name, then this will leave all compound names alone.
   if(any(complete.cases(compound_name))){
      ct_dataframe$Compound <- compound_name[ct_dataframe$CompoundID]
   }
   
   if("Compound" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$Compound <- "unknown compound"
   }
   
   if("Inhibitor" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$Inhibitor <- "none"
   }
   
   if(complete.cases(perpetrator_name)){
      ct_dataframe$Inhibitor[ct_dataframe$Inhibitor != "none"] <- perpetrator_name
   }
   
   if("File" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$File <- NA
   }
   
   if("ObsFile" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$ObsFile <- NA
   }
   
   if("Trial" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$Trial <- "all"
   }
   
   if("Tissue" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$Tissue <- "plasma"
   }
   
   # Checking that they have all the columns necessary
   
   PossDoseCols <- c("substrate" = "Dose_sub", 
                     "inhibitor 1" = "Dose_inhib", 
                     "inhibitor 2" = "Dose_inhib2")
   
   ColsNeeded <- c("CompoundID", "Inhibitor", "Tissue", "Individual", "Trial", 
                   "Simulated", "DoseNum", 
                   PossDoseCols[intersect(unique(ct_dataframe$CompoundID), 
                                          AllCompounds$DosedCompoundID)])
   
   if(all(ColsNeeded %in% names(ct_dataframe)) == FALSE){
      
      MissingCols <- setdiff(ColsNeeded, names(ct_dataframe))
      
      if("Individual" %in% MissingCols){
         warning(wrapn("It looks like you might not have requested individual data when you ran the function `extractConcTime`. We need individual concentration-time profiles here."), 
                 call. = FALSE)
      } 
      stop(wrapn(paste0("The data supplied for ct_dataframe is missing a column or columns called ", 
                        str_comma(paste0("`", MissingCols, "`")), ", so we cannot proceed. Please ensure that your data match the format you'd get from running the functions extractConcTime or extractObsConcTime.")), 
           call. = FALSE)
   }
   
   if("logical" %in% class(existing_exp_details) == FALSE){
      existing_exp_details <- harmonize_details(existing_exp_details)
      
      # Adding any columns that don't exist that we'll need
      MissingCols <- setdiff(paste0(rep(c("StartHr", "DoseInt", "NumDoses"), 
                                        each = 3), 
                                    c("_sub", "_inhib", "_inhib2")), 
                             names(existing_exp_details$MainDetails))
      
      ToAdd <- as.data.frame(matrix(NA, 
                                    nrow = nrow(existing_exp_details$MainDetails),
                                    ncol = length(MissingCols)))
      names(ToAdd) <- MissingCols
      
      existing_exp_details$MainDetails <-
         bind_cols(existing_exp_details$MainDetails, ToAdd)
      
   }
   
   
   # Main body of function -------------------------------------------------
   
   # Adjusting time to be time since most-recent dose. Only keeping 1st and
   # last-dose data for main PK calculations, but also keeping original
   # data.frame so that we can retain any data requested by user for a specific
   # interval.
   ct_dataframe_orig <- ct_dataframe
   
   # Finding the profiles to (re)calculate 
   ct_dataframe <- ct_dataframe %>% 
      mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                        ifelse(Simulated == TRUE, "simulated", "observed"),
                        File, ObsFile, DoseNum))
   
   if(is.null(existing_PK) | "individual" %in% names(existing_PK) == FALSE){
      existing_PK_indiv <- 
         as.data.frame(matrix(data = NA, 
                              ncol = 10, 
                              dimnames = list(NULL, 
                                              c("CompoundID", "Inhibitor", 
                                                "Tissue", "Individual", 
                                                "Trial", "Simulated", "File",
                                                "ObsFile", "DoseNum", 
                                                "PKparameter"))))
   } else {
      existing_PK_indiv <- existing_PK$individual
   } 
   
   existing_PK_indiv <- existing_PK_indiv %>% 
      mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                        ifelse(Simulated == TRUE, "simulated", "observed"),
                        File, ObsFile, DoseNum))
   
   # Making sure it's ok for user to specify "first" or "last" for dosenum_match
   # b/c that's the syntax for the argument which_dose.
   dosenum_match <- as.character(dosenum_match)
   dosenum_match[dosenum_match == "first"] <- "1"
   dosenum_match[dosenum_match == "last"] <- as.character(max(ct_dataframe$DoseNum))
   dosenum_match <- as.numeric(dosenum_match)
   dosenum_match <- dosenum_match[complete.cases(dosenum_match)]
   
   PossMatchCols <- list(
      "ID" = ID_match, 
      "CompoundID" = compoundID_match, 
      "Inhibitor" = inhibitor_match, 
      "Tissue" = tissue_match, 
      "Individual" = individual_match,
      "Trial" = trial_match, 
      "Simulated" = simulated_match, 
      "File" = file_match, 
      "ObsFile" = obsfile_match, 
      "DoseNum" = dosenum_match)
   
   PossMatchCols <- 
      PossMatchCols[sapply(PossMatchCols, 
                           FUN = function(x) length(x[complete.cases(x)]) > 0)]
   
   if(length(PossMatchCols) > 0){
      
      for(j in names(PossMatchCols)){
         Missing <- setdiff(PossMatchCols[[j]], t(ct_dataframe[, j]))
         if(length(Missing) > 0){
            warning(paste0("The following values were not present in the column `", 
                           j, "`:\n", 
                           str_c(Missing, collapse = "\n"), 
                           "\nThey will be ignored."), 
                    call. = FALSE)
            
            PossMatchCols[[j]] <- intersect(PossMatchCols[[j]], t(ct_dataframe[, j]))
            
            if(length(PossMatchCols[[j]]) == 0){
               stop(paste0("None of the values for the column `", 
                           j, "` could be found in your data, so we cannot proceed. Please make sure you're only asking to recalculate PK that already exist in your data. Othewise, please use the function `calc_PK`."), 
                    call. = FALSE)
            }
         }
      }
      
      PossMatches <- expand_grid(as.data.frame(PossMatchCols))
      
      MatchIDs <- list()
      for(i in 1:nrow(PossMatches)){
         MatchIDs[[i]] <- list()
         for(j in names(PossMatchCols)){
            MatchIDs[[i]][[j]] <- which(t(existing_PK_indiv[, j]) %in% PossMatches[i, j])
         }
         MatchIDs[[i]] <- unique(existing_PK_indiv$ID[Reduce(intersect, MatchIDs[[i]])])
      }
      
      MatchIDs <- unlist(MatchIDs)
      CTsubset <- ct_dataframe %>% filter(ID %in% MatchIDs)
      existing_PK_indiv <- existing_PK_indiv %>% filter(!ID %in% MatchIDs)
      
   } else {
      MatchIDs <- unique(ct_dataframe$ID)
      CTsubset <- ct_dataframe
   }
   
   # We only need existing_exp_details info for files we're trying to calculate. 
   if("logical" %in% class(existing_exp_details) == FALSE){
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          unique(CTsubset$File), 
                                          include_or_omit = "include")
   }
   
   
   ## Main PK setup -----------------------------------------------------------
   
   # If simulations had different trial designs or durations, need to calculate
   # last-dose PK separately so that we get the correct time interval. Checking
   # for that.
   MaxDoseNum <- CTsubset %>% 
      group_by(File) %>%
      summarize(MaxDoseNum = max(DoseNum)) %>% 
      ungroup()
   
   CTsubset <- CTsubset %>% 
      left_join(MaxDoseNum, by = "File") %>% 
      mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                        ifelse(Simulated == TRUE, "simulated", "observed"),
                        File, ObsFile, DoseNum)) %>% 
      filter(DoseNum %in% c(1, MaxDoseNum))
   
   # Separating 1st-dose data from everything else
   CT1st <- CTsubset %>% filter(DoseNum == 1)
   CTsubset <- CTsubset %>% filter(DoseNum != 1)
   
   ### Calculating PK parameters for individual datasets ------------------------
   
   ElimFits <- list()
   ElimFitGraphs <- list()
   PKtemp <- list()
   
   #### dose 1 -------------------------------------------------------------
   
   if("first" %in% which_dose){
      
      if(report_progress %in% c("yes", "some")){message("Calculating first-dose PK")}
      
      if("logical" %in% class(existing_exp_details) == FALSE){
         
         # Check whether the compound of interest has a custom-dosing regimen
         # and give a warning if so.
         suppressMessages(
            CustomDoseCheck <- 
               existing_exp_details$MainDetails %>% 
               select(File, StartHr_sub, StartHr_inhib, StartHr_inhib2, 
                      DoseInt_sub, DoseInt_inhib, DoseInt_inhib2) %>% 
               left_join(CT1st %>% select(File, CompoundID) %>% unique()) %>% 
               mutate(Problem = (CompoundID %in% c("substrate", 
                                                   "primary metabolite 1", 
                                                   "primary metabolite 2", 
                                                   "secondary metabolite") & 
                                    DoseInt_sub == "custom dosing") | 
                         (CompoundID %in% c("inhibitor 1", 
                                            "inhibitor 1 metabolite") & 
                             DoseInt_inhib == "custom dosing") |
                         (CompoundID %in% c("inhibitor 2") & 
                             DoseInt_inhib2 == "custom dosing"))
         )
         
         if(any(CustomDoseCheck$Problem == TRUE, na.rm = T)){
            warning(
               paste0(wrapn(paste0(
                  "The ", unique(CustomDoseCheck$CompoundID), 
                  " had a custom dosing regimen for the following files:")), 
                  str_c(CustomDoseCheck$File[which(CustomDoseCheck$Problem == TRUE)], 
                        collapse = "\n"), 
                  "\n", wrapn("We will treat them as if they were a single-dose regimen, which may not be correct. Please check the results carefully.")), 
               call. = FALSE)
         }
         
         suppressMessages(suppressWarnings(
            FirstDoseInfo <- existing_exp_details$MainDetails %>% 
               select(File, StartHr_sub, StartHr_inhib, StartHr_inhib2, 
                      DoseInt_sub, DoseInt_inhib, DoseInt_inhib2) %>% 
               left_join(CT1st %>% select(File, CompoundID) %>% unique()) %>% 
               mutate(t0 = case_when(
                  CompoundID %in% c("substrate", 
                                    "primary metabolite 1", 
                                    "primary metabolite 2", 
                                    "secondary metabolite") ~ StartHr_sub, 
                  CompoundID %in% c("inhibitor 1", 
                                    "inhibitor 1 metabolite") ~ StartHr_inhib, 
                  CompoundID %in% "inhibitor 2" ~ StartHr_inhib2), 
                  
                  MaxTime = case_when(
                     CompoundID %in% c("substrate", 
                                       "primary metabolite 1", 
                                       "primary metabolite 2", 
                                       "secondary metabolite") ~ StartHr_sub + as.numeric(DoseInt_sub), 
                     
                     CompoundID %in% c("inhibitor 1", 
                                       "inhibitor 1 metabolite") ~ StartHr_inhib + as.numeric(DoseInt_inhib), 
                     
                     CompoundID %in% "inhibitor 2" ~ StartHr_inhib2 + as.numeric(DoseInt_inhib2)))
         ))
         
         MaxPossDoseTime <- CT1st %>% ungroup() %>%
            filter(complete.cases(Conc)) %>%
            group_by(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial,
                     Simulated, File, ObsFile, DoseNum, ID) %>%
            summarize(MaxPossTime = max(Time, na.rm = T),
                      # For reasons that utterly baffle and infuriate me,
                      # dplyr will invisibly DROP the last grouping variable
                      # if the result would have only one row. (See help file
                      # for dplyr::summarize.) Why THAT would EVER be what
                      # people really want since the function that groups the
                      # data is NOT summarize and that this occurs even when
                      # they have specified .drop = FALSE in the function
                      # that DOES perform the grouping is *beyond me*.
                      .groups = "keep") %>% 
            ungroup()
         
         suppressMessages(
            FirstDoseInfo <- FirstDoseInfo %>% 
               left_join(MaxPossDoseTime) %>% 
               mutate(MaxTime = ifelse(is.na(MaxTime), 
                                       MaxPossTime, MaxTime)))
         
      } else {
         
         if(add_t0_point){
            if(complete.cases(first_dose_time)){
               # If user specified 1st dose time, then use that for t0.
               T0_toadd <- CT1st %>% 
                  select(-Time, -Conc) %>% unique() %>% 
                  mutate(Conc = 0, 
                         Time = first_dose_time)
            } else {
               # If user did not specify 1st dose time, then use minimum time
               # in data.
               T0 <- CT1st %>% 
                  group_by(across(
                     .cols = c(ID, CompoundID, Inhibitor, Tissue, Individual, Trial, 
                               Simulated, File, ObsFile, DoseNum, 
                               any_of(c("Dose_sub", "Dose_inhib", "Dose_inhib2",
                                        "Compound", "Conc_units", "Time_units", 
                                        "MaxDoseNum"))))) %>% 
                  summarize(T0 = min(Time)) %>% 
                  ungroup() 
               
               T0_toadd <- CT1st %>% 
                  select(-Time, -Conc) %>% unique() %>% 
                  mutate(Conc = 0, 
                         Time = first_dose_time)
            }
            
            CT1st <- CT1st %>% bind_rows(T0_toadd)
            
         }
         
         suppressMessages(suppressWarnings(
            FirstDoseInfo <- CT1st %>% ungroup() %>%
               filter(complete.cases(Conc)) %>%
               group_by(across(.cols = any_of(c("Compound", "CompoundID",
                                                "Inhibitor", "Tissue",
                                                "Individual", "Trial",
                                                "Simulated", "File",
                                                "ObsFile", "DoseNum", "ID")))) %>%
               summarize(t0 = min(Time, na.rm = T),
                         MaxTime = max(Time, na.rm = T),
                         # For reasons that utterly baffle and infuriate me,
                         # dplyr will invisibly DROP the last grouping
                         # variable if the result would have only one row.
                         # (See help file for dplyr::summarize.) Why THAT
                         # would EVER be what people really want since the
                         # function that groups the data is NOT summarize and
                         # that this occurs even when they have specified
                         # .drop = FALSE in the function that DOES perform
                         # the grouping is *beyond me*.
                         .groups = "keep") %>%
               mutate(MaxTime =
                         switch(as.character(complete.cases(dosing_interval)),
                                "TRUE" = t0 + dosing_interval,
                                "FALSE" = MaxTime)) %>%
               filter(complete.cases(File) | complete.cases(ObsFile)) %>% 
               ungroup()
         ))
         
         if(complete.cases(first_dose_time)){
            FirstDoseInfo$t0 <- first_dose_time
         }
         
         if(complete.cases(dosing_interval)){
            FirstDoseInfo$MaxTime <- FirstDoseInfo$t0 + dosing_interval
         }
      }
      
      CT1st <- split(CT1st, CT1st$ID)
      
      for(j in names(CT1st)){
         
         if(nrow(CT1st[[j]]) == 0){next}
         
         if(report_progress == "yes"){
            message(paste0("   Calculating PK for dataset ", j))
         }
         
         MyDose <- switch(unique(CT1st[[j]]$CompoundID), 
                          "substrate" = unique(CT1st[[j]]$Dose_sub), 
                          "primary metabolite 1" = NA, 
                          "primary metabolite 2" = NA, 
                          "secondary metabolite" = NA,
                          "inhibitor 1" = unique(CT1st[[j]]$Dose_inhib), 
                          "inhibitor 2" = unique(CT1st[[j]]$Dose_inhib2),
                          "inhibitor 1 metabolite" = NA) %>% 
            as.numeric()
         
         suppressMessages(
            CT_temp <- CT1st[[j]] %>% 
               left_join(FirstDoseInfo) %>% 
               filter(Time >= t0 & Time <= MaxTime) %>% 
               # NB: Need to subtract t0 here so that fitting starts at the
               # right time. Also, there may be 0 concs *after* t0 depending
               # on the absorption, so I'm specifically using the minimum
               # value so that all subjects in the same simulation have the
               # same t0 for dose 1.
               mutate(Time = Time - t0)
         )
         
         if(omit_0_concs){
            # NB: It's appropriate to set this to 0 rather than t0 b/c we
            # have already subtracted t0. t0 = 0 now.
            CT_temp <- CT_temp %>%
               filter((Time != 0 & Conc > 0) | Time == 0)
         }
         
         # Subsetting the data if there are lots of points b/c fitting takes
         # a lot longer. When there are thousands of points, there are
         # thousands of values to minimize the residuals for. Instead of
         # using all of the data, using only a subset of the time points when
         # there are more than 100 observeations.
         if(nrow(CT_temp) > 100){
            CT_temp <- as.data.frame( # For some reason, you can't do this w/tibbles. 
               CT_temp)[seq(from = 1, to = nrow(CT_temp), length.out = 100), ]
            
         }
         
         if(is.na(fit_last_x_number_of_points)){
            Omit <- NA
         } else {
            Omit <- 1:(nrow(CT_temp) - fit_last_x_number_of_points)
         }
         
         if(nrow(CT_temp) < 2){
            warning(paste0("There are not enough data to calculate PK for this ID:\n", 
                           j, "\n"), 
                    call. = FALSE)
            next
         }
         
         if(effort_to_get_elimination_rate != "don't try"){
            
            TEMP <- elimFit(DF = CT_temp, 
                            concentration = Conc, 
                            time = Time, 
                            tmax = fit_points_after_x_time, 
                            omit = Omit,
                            weights = weights,
                            useNLS_outnames = FALSE,
                            modelType = "monoexponential",
                            graph = TRUE, 
                            returnRSS = TRUE, 
                            returnAIC = TRUE)
            
            if(any(TEMP == "Insufficient data to create model")){
               
               ElimFits[[j]] <- NULL
               ExtrapProbs <- TRUE
               AUCextrap_temp <- NA
               
            }
         } else {
            
            ElimFits[[j]] <- NULL
            ExtrapProbs <- NA
            AUCextrap_temp <- NA
            TEMP <- NULL
         }
         
         # Checking for whether any estimates were negative, which would
         # be nonsensical. If they were, try again if that's what the user
         # wants.
         if(("Estimates" %in% names(TEMP) &&
             (any(is.na(TEMP$Estimates$Beta)) |
              any(TEMP$Estimates$Estimate < 0))) & 
            effort_to_get_elimination_rate %in% c("try really hard", 
                                                  "try hard")){
            
            cc <- capture.output(
               type="message",
               TEMP <- try(elimFit(DF = CT_temp, 
                                   concentration = Conc, 
                                   time = Time, 
                                   useNLS_outnames = FALSE,
                                   modelType = "biexponential",
                                   weights = weights,
                                   startValues = data.frame(A = c(100, 1000), 
                                                            alpha = c(0.1, 1), 
                                                            B = c(1, 100), 
                                                            beta = c(0.01, 0.1)),
                                   graph = TRUE, 
                                   returnRSS = TRUE, 
                                   returnAIC = TRUE), 
                           silent = TRUE)
            )
         }
         
         # Check whether that attempt worked and try again if not if that's
         # what the user wants.
         if((inherits(TEMP, "try-error") |
             (inherits(TEMP, "try-error") == FALSE && 
              "Estimates" %in% names(TEMP) &&
              (any(is.na(TEMP$Estimates$Beta)) |
               any(TEMP$Estimates$Estimate < 0)))) &
            effort_to_get_elimination_rate  == "try really hard"){
            TEMP <- elimFit(DF = CT_temp, 
                            concentration = Conc, 
                            time = Time, 
                            weights = weights,
                            useNLS_outnames = FALSE,
                            modelType = "monoexponential",
                            graph = TRUE, 
                            returnRSS = TRUE, 
                            returnAIC = TRUE)
         }
         
         # Check whether things worked. If they did, make the graph showing
         # that. If they did not, make a graph that says there was a problem.
         if("Estimates" %in% names(TEMP) &&
            (any(is.na(TEMP$Estimates$Beta)) |
             any(TEMP$Estimates$Estimate < 0)) == FALSE){
            ElimFitGraphs[[j]] <- TEMP$Graph +
               ggtitle(sub("observed ", "observed\n", 
                           sub("simulated ", "simulated\n", j))) +
               theme(title = element_text(size = 6))
         } else {
            ElimFitGraphs[[j]] <- ggplot(CT_temp, aes(x = Time, y = Conc)) +
               geom_point() +
               annotate(geom = "text", x = mean(CT_temp$Time, na.rm = T),
                        y = mean(CT_temp$Conc, na.rm = T), size = 8,
                        color = "red", 
                        label = "There was a problem with extrapolation\nto infinity for this graph.") +
               ggtitle(sub("observed ", "observed\n", 
                           sub("simulated ", "simulated\n", j)))
         }
         
         if("Estimates" %in% names(TEMP) && 
            TEMP$Estimates$Estimate[TEMP$Estimates$Beta == "k"] > 0){
            ElimFits[[j]] <- TEMP$Estimates
            ExtrapProbs <- FALSE
         } else {
            # This is when fitting still failed, so TEMP == "Insufficient
            # data to create model".
            ElimFits[[j]] <- NULL
            ExtrapProbs <- TRUE
            AUCextrap_temp <- NA
         }
         
         Extrap <- ifelse(ExtrapProbs == FALSE, 
                          TRUE, FALSE)
         
         if(is.null(ElimFits[[j]]) == FALSE & Extrap == TRUE){
            AUCextrap_temp <- noncompAUC(DF = CT1st[[j]], 
                                         concentration = Conc, 
                                         time = Time, 
                                         type = trapezoidal_method, 
                                         extrap_inf = TRUE, 
                                         extrap_inf_coefs = ElimFits[[j]], 
                                         reportFractExtrap = TRUE)
         }
         
         AUCt_temp <- noncompAUC(DF = CT1st[[j]], 
                                 concentration = Conc, 
                                 time = Time, 
                                 type = trapezoidal_method, 
                                 extrap_inf = FALSE)
         
         suppressMessages(
            CmaxTmax_temp <- CT1st[[j]] %>% 
               # NB: Need to subtract t0 here so that fitting starts at the
               # right time. Also, there may be 0 concs *after* t0 depending
               # on the absorption, so I'm specifically using the minimum
               # value so that all subjects in the same simulation have the
               # same t0 for dose 1.
               left_join(FirstDoseInfo) %>% 
               mutate(Time = Time - t0) %>% 
               summarize(Cmax = max(Conc, na.rm = T), 
                         tmax = Time[which.max(Conc)], 
                         Cmin = min(Conc, na.rm = T), 
                         Clast = Conc[which.max(Time)])
         )
         
         PKtemp[[j]] <- 
            data.frame(
               ID = j,
               WhichDose = "dose1", 
               Compound = unique(CT1st[[j]]$Compound), 
               CompoundID = unique(CT1st[[j]]$CompoundID), 
               Inhibitor = unique(CT1st[[j]]$Inhibitor),
               Tissue = unique(CT1st[[j]]$Tissue),
               Individual = unique(CT1st[[j]]$Individual), 
               Trial = unique(CT1st[[j]]$Trial), 
               Simulated = unique(CT1st[[j]]$Simulated), 
               File = unique(CT1st[[j]]$File), 
               ObsFile = unique(CT1st[[j]]$ObsFile), 
               DoseNum = unique(CT1st[[j]]$DoseNum), 
               AUCinf = ifelse(Extrap, AUCextrap_temp$AUC, NA),
               AUCinf_fraction_extrapolated = ifelse(Extrap, 
                                                     AUCextrap_temp$`Fraction extrapolated to infinity`, 
                                                     NA),
               ExtrapProbs = ExtrapProbs,
               k = ifelse(is.null(ElimFits[[j]]),
                          NA, ElimFits[[j]]$Estimate[ElimFits[[j]]$Beta %in% c("k", "beta")])) %>% 
            mutate(HalfLife = log(2) / k,
                   AUCt = AUCt_temp, 
                   Cmax = CmaxTmax_temp$Cmax, 
                   tmax = CmaxTmax_temp$tmax, 
                   Cmin = CmaxTmax_temp$Cmin, 
                   Clast = CmaxTmax_temp$Clast,
                   CL = ifelse(is.na(AUCinf), 
                               NA, MyDose / AUCinf * 1000), 
                   Dose = MyDose, 
                   RSS = unique(TEMP$Estimates$RSS), 
                   AIC = unique(TEMP$Estimates$AIC))
         
         suppressWarnings(
            rm(AUCextrap_temp, AUCt_temp, CmaxTmax_temp, ExtrapProbs, Extrap, 
               TEMP, CT_temp))
      }
   }
   
   
   #### last dose -----------------------------------------------------------
   
   if("last" %in% which_dose & length(CTsubset) > 0){
      
      if(nrow(CTsubset) == 0){
         warning(wrapn("You requested PK for the last dose in your data, but we cannot find any last-dose concentration-time data, so we will not be able to return any PK data."), 
                 call. = FALSE)
      } else {
         
         if(report_progress %in% c("yes", "some")){message("Calculating last-dose PK")}
         
         # NB: There may be more than one maximum dose number b/c CTsubset could
         # include multiple sets of data, and that should be fine. 
         
         if("logical" %in% class(existing_exp_details) == FALSE){
            suppressMessages(suppressWarnings(
               LastDoseTime <- existing_exp_details$MainDetails %>% 
                  select(File, SimDuration, StartHr_sub, StartHr_inhib, StartHr_inhib2,
                         DoseInt_sub, DoseInt_inhib, DoseInt_inhib2, 
                         NumDoses_sub, NumDoses_inhib, NumDoses_inhib2) %>% 
                  left_join(ct_dataframe %>% select(File, CompoundID) %>% unique()) %>% 
                  mutate(
                     across(.cols = c(StartHr_sub, DoseInt_sub, NumDoses_sub, 
                                      StartHr_inhib, DoseInt_inhib, NumDoses_inhib, 
                                      StartHr_inhib2, DoseInt_inhib2, NumDoses_inhib2, 
                                      SimDuration), 
                            .fns = as.numeric), 
                     t0 = 
                        case_when(CompoundID %in% c("substrate", 
                                                    "primary metabolite 1", 
                                                    "primary metabolite 2", 
                                                    "secondary metabolite") ~ 
                                     StartHr_sub + DoseInt_sub * NumDoses_sub, 
                                  
                                  CompoundID %in% c("inhibitor 1", 
                                                    "inhibitor 1 metabolite") ~ 
                                     StartHr_inhib + DoseInt_inhib * NumDoses_inhib, 
                                  
                                  CompoundID %in% "inhibitor 2" ~ 
                                     StartHr_inhib2 + DoseInt_inhib2 * NumDoses_inhib2), 
                     t0 = 
                        case_when(t0 == SimDuration & 
                                     CompoundID %in% c("substrate", 
                                                       "primary metabolite 1", 
                                                       "primary metabolite 2", 
                                                       "secondary metabolite") ~ 
                                     StartHr_sub + DoseInt_sub * NumDoses_sub - DoseInt_sub, 
                                  
                                  t0 == SimDuration & 
                                     CompoundID %in% c("inhibitor 1", 
                                                       "inhibitor 1 metabolite") ~ 
                                     StartHr_inhib + DoseInt_inhib * NumDoses_inhib - DoseInt_inhib, 
                                  
                                  t0 == SimDuration & 
                                     CompoundID %in% "inhibitor 2" ~ 
                                     StartHr_inhib2 + DoseInt_inhib2 * NumDoses_inhib2 - DoseInt_inhib2, 
                                  
                                  TRUE ~ t0)
                  ))
            )
            
            # NB: Will get MaxTime from the step where we check dose intervals.
            
         } else {
            
            suppressMessages(suppressWarnings(
               LastDoseTime <- CTsubset %>% 
                  ungroup() %>%
                  filter(complete.cases(Conc)) %>%
                  group_by(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial,
                           Simulated, File, ObsFile, DoseNum, ID) %>%
                  summarize(t0 = min(Time, na.rm = T),
                            MaxTime = max(Time, na.rm = T),
                            # For reasons that utterly baffle and infuriate me,
                            # dplyr will invisibly DROP the last grouping
                            # variable if the result would have only one row.
                            # (See help file for dplyr::summarize.) Why THAT
                            # would EVER be what people really want since the
                            # function that groups the data is NOT summarize and
                            # that this occurs even when they have specified
                            # .drop = FALSE in the function that DOES perform
                            # the grouping is *beyond me*.
                            .groups = "keep") %>%
                  mutate(MaxTime =
                            switch(as.character(complete.cases(dosing_interval)),
                                   "TRUE" = t0 + dosing_interval,
                                   "FALSE" = MaxTime)) %>% 
                  filter(complete.cases(File)) %>% 
                  ungroup()
            ))
            
            if(complete.cases(last_dose_time)){
               LastDoseTime$t0 <- last_dose_time
            }
            
            if(complete.cases(dosing_interval)){
               LastDoseTime$MaxTime <- LastDoseTime$t0 + dosing_interval
            }
         }
         
         DoseIntChecks <- list()
         DoseIntWarnings <- list()
         
         # Checking for any mismatches w/dosing interval. 
         if("logical" %in% class(existing_exp_details)){
            # probably observed data, so no existing_exp_details b/c not
            # Simulator output
            DoseIntChecks <- LastDoseTime %>% 
               mutate(IntEndMatch = TRUE, 
                      EndHr = MaxTime, 
                      Interval = MaxTime - t0, 
                      StartHr = t0, 
                      DoseInt = Interval)
            
         } else {
            # simulated data here or else something where user had provided
            # required dosing interval data
            for(file in unique(CTsubset$File)){
               
               DoseIntChecks[[file]] <- list()
               DoseIntWarnings[[file]] <- list()
               
               for(cmpd in unique(CTsubset$CompoundID[CTsubset$File == file])){
                  
                  TEMP <- check_doseint(sim_data_file = file, 
                                        existing_exp_details = existing_exp_details, 
                                        compoundID = cmpd, 
                                        stop_or_warn_missing_file = "warn")
                  
                  DoseIntWarnings[[file]][[cmpd]] <- TEMP$message
                  DoseIntChecks[[file]][[cmpd]] <- TEMP$interval
               }
               
               DoseIntChecks[[file]] <- bind_rows(DoseIntChecks[[file]])
            }
            
            DoseIntChecks <- bind_rows(DoseIntChecks)
            
         }
         
         if("data.frame" %in% class(DoseIntChecks)){
            
            suppressMessages(
               LastDoseTime <- LastDoseTime %>% 
                  left_join(DoseIntChecks %>% 
                               mutate(MaxTime = case_when(IntEndMatch == TRUE ~ EndHr,
                                                          IntEndMatch == FALSE & Interval != "dose1" ~ StartHr + DoseInt, 
                                                          .default = NA)) %>% 
                               select(File, CompoundID, MaxTime))
            )
            
         } else {
            # custom dosing intervals, etc. 
            suppressMessages(
               LastDoseTime <- LastDoseTime %>% 
                  left_join(existing_exp_details$MainDetails %>% 
                               select(File, SimDuration)) %>% 
                  rename(MaxTime = SimDuration)
            )
         }
         
         CTsubset <- split(CTsubset, list(CTsubset$MaxDoseNum))
         
         for(i in names(CTsubset)){
            
            CTsubset[[i]] <- split(CTsubset[[i]], 
                                   CTsubset[[i]]$ID)
            
            for(j in names(CTsubset[[i]])){
               
               MyDose <- switch(unique(CTsubset[[i]][[j]]$CompoundID), 
                                "substrate" = unique(CTsubset[[i]][[j]]$Dose_sub), 
                                "primary metabolite 1" = NA, 
                                "primary metabolite 2" = NA, 
                                "secondary metabolite" = NA,
                                "inhibitor 1" = unique(CTsubset[[i]][[j]]$Dose_inhib), 
                                "inhibitor 2" = unique(CTsubset[[i]][[j]]$Dose_inhib2),
                                "inhibitor 1 metabolite" = NA)
               
               suppressMessages(
                  CTsubset[[i]][[j]] <- CTsubset[[i]][[j]] %>% 
                     select(-any_of(c("t0", "MaxTime"))) %>% 
                     left_join(LastDoseTime) %>% 
                     filter(Time >= t0 & Time <= MaxTime) %>% 
                     # Setting t0 to 0 for ease of remaining PK calculations
                     mutate(Time = Time - t0)
               )
               
               # There could be 0 rows if there are different maximum dose
               # numbers for different simulations, so check, skip if there are
               # no rows here b/c it's probably fine, and then give message
               # about what dataset is being checked.
               if(nrow(CTsubset[[i]][[j]]) == 0){next}
               
               if(report_progress == "yes"){message(paste0("   Calculating PK for dataset ", j))}
               
               if(omit_0_concs){
                  # NB: Here, since this will NOT include any true t0 data (t =
                  # 0 for dose 1 b/c  CTsubset[[i]][[j]] does not include dose 1
                  # data), all concentrations should be above 0 if that's what
                  # the user requested.
                  CTsubset[[i]][[j]] <- CTsubset[[i]][[j]] %>%
                     filter(Conc > 0)
                  
                  # Check again for sufficient data
                  if(nrow(CTsubset[[i]][[j]]) == 0){
                     warning(wrapn(paste0("There are no non-zero concentrations for the last dose for dataset '", 
                                          j, "'. This dataset will be skipped.")), 
                             call. = FALSE)
                     
                     next
                  }
                  
               }
               
               # Make a graph of the last-dose data so that user can check that
               # they're integrating what they *think* they're integrating.
               ElimFitGraphs[[j]] <- 
                  ggplot(CTsubset[[i]][[j]], aes(x = Time, y = Conc)) + 
                  geom_line() + 
                  ggtitle(sub("observed ", "observed\n", 
                              sub("simulated ", "simulated\n", j))) +
                  theme_consultancy() +
                  theme(text = element_text(size = 6), 
                        plot.title = element_text(size = 6))
               
               AUCt_temp <- noncompAUC(DF = CTsubset[[i]][[j]], 
                                       concentration = Conc, 
                                       time = Time, 
                                       type = trapezoidal_method, 
                                       extrap_inf = FALSE)
               
               CmaxTmax_temp <- CTsubset[[i]][[j]] %>% 
                  summarize(Cmax = max(Conc, na.rm = T), 
                            tmax = Time[which.max(Conc)], 
                            Cmin = min(Conc, na.rm = T), 
                            Clast = Conc[which.max(Time)])
               
               PKtemp[[paste("last", j)]] <- 
                  data.frame(
                     ID = j,
                     WhichDose = "last", 
                     Compound = unique(CTsubset[[i]][[j]]$Compound), 
                     CompoundID = unique(CTsubset[[i]][[j]]$CompoundID), 
                     Inhibitor = unique(CTsubset[[i]][[j]]$Inhibitor),
                     Tissue = unique(CTsubset[[i]][[j]]$Tissue),
                     Individual = unique(CTsubset[[i]][[j]]$Individual), 
                     Trial = unique(CTsubset[[i]][[j]]$Trial), 
                     Simulated = unique(CTsubset[[i]][[j]]$Simulated), 
                     File = unique(CTsubset[[i]][[j]]$File), 
                     ObsFile = unique(CTsubset[[i]][[j]]$ObsFile), 
                     DoseNum = unique(CTsubset[[i]][[j]]$DoseNum), 
                     AUCinf = NA,
                     AUCinf_fraction_extrapolated = NA, 
                     ExtrapProbs = FALSE,
                     k = NA) %>% 
                  mutate(HalfLife = NA,
                         AUCt = AUCt_temp, 
                         Cmax = CmaxTmax_temp$Cmax, 
                         tmax = CmaxTmax_temp$tmax, 
                         Cmin = CmaxTmax_temp$Cmin, 
                         Clast = CmaxTmax_temp$Clast,
                         CL = MyDose / AUCt * 1000, 
                         Dose = MyDose)
               
               suppressWarnings(rm(AUCt_temp, CmaxTmax_temp))
            }
         }
      }
   }
   
   #### special interval ------------------------------------------------------
   
   if(length(SpecialInterval) > 0){
      
      if(report_progress %in% c("yes", "some")){message("Calculating PK for user-defined interval")}
      
      # NB: There may be more than one user-specified interval and that should be
      # fine.
      for(i in 1:length(SpecialInterval)){
         
         if("logical" %in% class(existing_exp_details) == FALSE){
            suppressMessages(
               FirstDoseInfo <- existing_exp_details$MainDetails %>% 
                  select(File, StartHr_sub, StartHr_inhib, StartHr_inhib2, 
                         DoseInt_sub, DoseInt_inhib, DoseInt_inhib2) %>% 
                  left_join(CTsubset[["1"]] %>% select(File, CompoundID) %>% unique()) %>% 
                  mutate(t0 = case_when(
                     CompoundID %in% c("substrate", 
                                       "primary metabolite 1", 
                                       "primary metabolite 2", 
                                       "secondary metabolite") ~ StartHr_sub, 
                     CompoundID %in% c("inhibitor 1", 
                                       "inhibitor 1 metabolite") ~ StartHr_inhib, 
                     CompoundID %in% "inhibitor 2" ~ StartHr_inhib2), 
                     
                     MaxTime = case_when(
                        CompoundID %in% c("substrate", 
                                          "primary metabolite 1", 
                                          "primary metabolite 2", 
                                          "secondary metabolite") ~ StartHr_sub + DoseInt_sub, 
                        
                        CompoundID %in% c("inhibitor 1", 
                                          "inhibitor 1 metabolite") ~ StartHr_inhib + DoseInt_inhib, 
                        
                        CompoundID %in% "inhibitor 2" ~ StartHr_inhib2 + DoseInt_inhib2))
            )
         } else {
            suppressMessages(suppressWarnings(
               FirstDoseInfo <- CTsubset[["1"]] %>% ungroup() %>%
                  filter(complete.cases(Conc)) %>%
                  group_by(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial,
                           Simulated, File, ObsFile, DoseNum, ID) %>%
                  summarize(t0 = min(Time, na.rm = T),
                            MaxTime = max(Time, na.rm = T),
                            # For reasons that utterly baffle and infuriate me,
                            # dplyr will invisibly DROP the last grouping
                            # variable if the result would have only one row.
                            # (See help file for dplyr::summarize.) Why THAT
                            # would EVER be what people really want since the
                            # function that groups the data is NOT summarize and
                            # that this occurs even when they have specified
                            # .drop = FALSE in the function that DOES perform
                            # the grouping is *beyond me*.
                            .groups = "keep") %>%
                  mutate(MaxTime =
                            switch(as.character(complete.cases(dosing_interval)),
                                   "TRUE" = t0 + dosing_interval,
                                   "FALSE" = MaxTime)) %>% 
                  filter(complete.cases(File) | complete.cases(ObsFile)) %>% 
                  ungroup()
            ))
            
            if(complete.cases(first_dose_time)){
               FirstDoseInfo$t0 <- first_dose_time
            }
         }
         
         if(omit_0_concs){
            CTsubset[[i]][[j]] <- CTsubset[[i]][[j]] %>%
               filter((Time > FirstDoseInfo$t0 & Conc > 0) |
                         (Time == FirstDoseInfo$t0))
         }
         
         CTtemp <- ct_dataframe_orig %>% 
            filter(Time >= SpecialInterval[[i]][1] & 
                      Time <= SpecialInterval[[i]][2]) %>% 
            mutate(Time = Time - SpecialInterval[[i]][1], 
                   DoseNum = str_c(paste0(SpecialInterval[[i]], "h"),
                                   collapse = "to"), 
                   ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                              ifelse(Simulated == TRUE, "simulated", "observed"),
                              File, ObsFile, DoseNum))
         
         CTtemp <- split(CTtemp, CTtemp$ID)
         
         for(j in names(CTtemp)){
            
            if(nrow(CTtemp[[j]]) == 0){next}
            
            if(report_progress == "yes"){message(paste0("   Calculating PK for dataset ", j))}
            
            MyDose <- switch(unique(CTtemp[[j]]$CompoundID), 
                             "substrate" = unique(CTtemp[[j]]$Dose_sub), 
                             "primary metabolite 1" = NA, 
                             "primary metabolite 2" = NA, 
                             "secondary metabolite" = NA,
                             "inhibitor 1" = unique(CTtemp[[j]]$Dose_inhib), 
                             "inhibitor 2" = unique(CTtemp[[j]]$Dose_inhib2),
                             "inhibitor 1 metabolite" = NA)
            
            # Make a graph of the data so that user can check that they're
            # integrating what they *think* they're integrating.
            ElimFitGraphs[[j]] <- 
               ggplot(CTtemp[[j]], aes(x = Time, y = Conc)) + 
               geom_line() + 
               ggtitle(sub("observed ", "observed\n", 
                           sub("simulated ", "simulated\n", j))) +
               theme_consultancy() +
               theme(text = element_text(size = 6), 
                     plot.title = element_text(size = 6))
            
            AUCt_temp <- noncompAUC(DF = CTtemp[[j]], 
                                    concentration = Conc, 
                                    time = Time, 
                                    type = trapezoidal_method, 
                                    extrap_inf = FALSE)
            
            CmaxTmax_temp <- CTtemp[[j]] %>% 
               summarize(Cmax = max(Conc, na.rm = T), 
                         tmax = Time[which.max(Conc)], 
                         Cmin = min(Conc, na.rm = T), 
                         Clast = Conc[which.max(Time)])
            
            PKtemp[[paste(str_c(SpecialInterval[[i]], collapse = "to"), j)]] <- 
               data.frame(
                  ID = j,
                  WhichDose = str_c(paste(SpecialInterval[[i]], "h"),
                                    collapse = " to "), 
                  Compound = unique(CTtemp[[j]]$Compound), 
                  CompoundID = unique(CTtemp[[j]]$CompoundID), 
                  Inhibitor = unique(CTtemp[[j]]$Inhibitor),
                  Tissue = unique(CTtemp[[j]]$Tissue),
                  Individual = unique(CTtemp[[j]]$Individual), 
                  Trial = unique(CTtemp[[j]]$Trial), 
                  Simulated = unique(CTtemp[[j]]$Simulated), 
                  File = unique(CTtemp[[j]]$File), 
                  ObsFile = unique(CTtemp[[j]]$ObsFile), 
                  DoseNum = NA, 
                  AUCinf = NA,
                  AUCinf_fraction_extrapolated = NA, 
                  ExtrapProbs = FALSE,
                  k = NA) %>% 
               mutate(HalfLife = NA,
                      AUCt = AUCt_temp, 
                      Cmax = CmaxTmax_temp$Cmax, 
                      tmax = CmaxTmax_temp$tmax, 
                      Cmin = CmaxTmax_temp$Cmin, 
                      Clast = CmaxTmax_temp$Clast,
                      CL = MyDose / AUCt * 1000, 
                      Dose = MyDose)
            
            suppressWarnings(rm(AUCt_temp, CmaxTmax_temp))
         }
      }
   }
   
   #### Collecting results from all intervals -------------------
   
   PKtemp <- bind_rows(PKtemp) 
   
   if("AIC" %in% names(PKtemp) == FALSE){PKtemp$AIC <- as.numeric(NA)}
   if("RSS" %in% names(PKtemp) == FALSE){PKtemp$RSS <- as.numeric(NA)}
   
   PKtemp <- PKtemp %>% 
      pivot_longer(cols = c(AUCinf, AUCinf_fraction_extrapolated,
                            k, HalfLife, AUCt, Cmax, Cmin, Clast, tmax, CL),
                   names_to = "PKparameter",
                   values_to = "Value") %>%
      mutate(PKparameter = paste0(PKparameter, "_", WhichDose),
             PKparameter = 
                case_when(PKparameter == "AUCt_last" ~ "AUCtau_last",
                          PKparameter == "CL_last" ~ "CLtau_last", 
                          PKparameter == "CL_dose1" ~ "CLinf_dose1",
                          TRUE ~ PKparameter), 
             AIC = case_when(
                str_detect(PKparameter, "AUCinf|CLinf|k_|HalfLife") ~ AIC, 
                .default = NA), 
             RSS = case_when(
                str_detect(PKparameter, "AUCinf|CLinf|k_|HalfLife") ~ RSS, 
                .default = NA)) %>% 
      # Removing PK parameters that would not be accurate or informative for
      # that dose
      filter(!PKparameter %in% c("Cmin_dose1", "AUCinf_last", 
                                 "HalfLife_last", "k_last", 
                                 "AUCinf_fraction_extrapolated_last")) 
   
   if(any(PKtemp$ExtrapProbs, na.rm = TRUE) & 
      effort_to_get_elimination_rate != "don't try"){
      warning(paste0(
         wrapn("There were problems extrapolating to infinity for some data. The sets of data with problems are listed here by CompoundID, Inhibitor, Tissue, Individual, Trial, whether the data were simulated or observed, File, ObsFile, and DoseNum:"), 
         str_c(sort(unique(PKtemp$ID[PKtemp$ExtrapProbs])), collapse = "\n")), 
         call. = FALSE)
   }
   
   PKtemp <- PKtemp %>% 
      mutate(PKparameter = ifelse(Inhibitor != "none", 
                                  paste0(PKparameter, "_withInhib"), 
                                  PKparameter))
   
   # Replacing original PK and renaming this as PKtemp to match syntax in
   # original function
   PKtemp <- bind_rows(existing_PK_indiv, PKtemp) %>% 
      filter(complete.cases(PKparameter))
   
   
   ## Checking for possible DDI parameters to calculate -----------------------
   
   if(any(PKtemp$Inhibitor != "none")){
      
      # Removing any ratios b/c we'll be recalculating them anyway. 
      PKtemp <- PKtemp %>% filter(!str_detect(PKparameter, "ratio"))
      
      suppressMessages(
         PKDDI <- PKtemp %>% select(-ID, -ExtrapProbs, -WhichDose) %>% 
            # Removing any ratios b/c we're recalculating all of them
            filter(!str_detect(PKparameter, "_ratio")) %>% unique() %>% 
            # Recoding inhibitor to be consistent. Only using "none" and "inhibitorX".
            mutate(Inhibitor = ifelse(Inhibitor == "none", Inhibitor, "inhibitorX"), 
                   PKparameter = sub("_withInhib", "", PKparameter)) %>% 
            pivot_wider(names_from = Inhibitor, 
                        values_from = Value) %>% 
            mutate(Value = inhibitorX / none, 
                   PKparameter = sub("_", "_ratio_", PKparameter)) %>% 
            select(Compound, CompoundID, Tissue, Individual, Trial,
                   Simulated, File, ObsFile, DoseNum, PKparameter, Value) %>% 
            # Remove any instances where there weren't matching data
            filter(complete.cases(Value)) %>% 
            # Get the inhibitor name back
            left_join(PKtemp %>% filter(Inhibitor != "none") %>% 
                         select(Compound, CompoundID, Tissue, Individual, 
                                Trial, Simulated, File, Dose, 
                                ObsFile, DoseNum, Inhibitor) %>% unique()) %>% 
            mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                              ifelse(Simulated == TRUE, "simulated", "observed"),
                              File, ObsFile, DoseNum)) %>% 
            # Removing some artifacts that are nonsensical PK parameters
            filter(!str_detect(PKparameter, "fraction_extrapolated_ratio"))
      )
      
      PKtemp <- bind_rows(PKtemp, PKDDI) %>% 
         # Also fixing PK parameter names here since they should be different in
         # the presence of a perpetrator.
         mutate(PKparameter = case_when(
            Inhibitor != "none" & !str_detect(PKparameter, "ratio") ~ 
               paste0(sub("_withInhib", "", PKparameter), "_withInhib"), 
            TRUE ~ PKparameter)) %>% unique()
      
   }
   
   if(effort_to_get_elimination_rate == "don't try"){
      PKtemp <- PKtemp %>% 
         filter(!str_detect(PKparameter, "AUCinf|CLinf"))
   }
   
   ## Calculating aggregate stats --------------------------------------------
   
   if(returnAggregateOrIndiv %in% c("aggregate", "both")){
      
      suppressWarnings(suppressMessages(
         PK_agg_temp <- PKtemp %>% 
            filter(PKparameter != "AUCinf_dose1" |
                      (PKparameter == "AUCinf_dose1" & ExtrapProbs == FALSE)) %>% 
            select(-ExtrapProbs) %>% 
            group_by(Compound, CompoundID, Inhibitor, Tissue, 
                     Simulated, File, ObsFile, Dose, 
                     DoseNum, PKparameter) %>% 
            summarize(
               N = n(), 
               Mean = mean(Value, na.rm = T),
               SD = sd(Value, na.rm = T), 
               Geomean = gm_mean(Value, na.rm = F), # need this to fail when there were problems, e.g., problems extrapolating to inf
               GeoCV = gm_CV(Value, na.rm = F), 
               CI90_lower = gm_conf(Value, CI = 0.90)[1], 
               CI90_upper = gm_conf(Value, CI = 0.90)[2],
               Median = median(Value, na.rm = T), 
               Minimum = min(Value, na.rm = T), 
               Maximum = max(Value, na.rm = T)) %>% 
            ungroup()
      ))
      
   }
   
   if(effort_to_get_elimination_rate == "don't try"){
      PK_agg_temp <- PK_agg_temp %>% 
         filter(!str_detect(PKparameter, "AUCinf|CLinf"))
   }
   
   
   # Collecting results to return --------------------------------------------
   
   if(save_graphs_of_fits){
      
      for(i in names(ElimFitGraphs)){
         if(all(is.null(nrow) & is.null(ncol))){
            Nrow <- NULL
            Ncol <- NULL
         } else {
            Nrow <- ifelse(is.null(nrow), 
                           round_up_unit(length(names(ElimFitGraphs)) / ncol, 1), 
                           nrow)
            Ncol <- ifelse(is.null(ncol), 
                           round_up_unit(length(names(ElimFitGraphs)) / nrow, 1), 
                           ncol)
         }
         
         ggpubr::ggarrange(
            plotlist = ElimFitGraphs, 
            ncol = Ncol, 
            nrow = Nrow)
         ggsave(paste0(gsub("/", "-", gsub(".xlsx", "", i)), ".png"), 
                height = fig_height, width = fig_width, dpi = 300)
      }
      
   }
   
   Out <- list()
   
   if(returnAggregateOrIndiv %in% c("individual", "both")){
      Out[["individual"]] <- PKtemp %>% 
         mutate(Inhibitor = ifelse(Inhibitor == "inhibitor",
                                   {{perpetrator_name}}, Inhibitor)) %>% 
         select(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial, 
                Simulated, File, ObsFile, Dose, 
                DoseNum, PKparameter, Value, ExtrapProbs, AIC, RSS)
   }
   
   if(returnAggregateOrIndiv %in% c("aggregate", "both")){
      Out[["aggregate"]] <- PK_agg_temp %>% 
         mutate(Inhibitor = ifelse(Inhibitor == "inhibitor",
                                   {{perpetrator_name}}, Inhibitor)) %>% 
         select(Compound, CompoundID, everything())
   }
   
   if(return_graphs_of_fits){
      # Replacing original graphs
      if("graphs" %in% names(existing_PK)){
         existing_PK$graphs <- 
            existing_PK$graphs[
               setdiff(names(existing_PK$graphs), 
                       MatchIDs)]
         
         existing_PK$graphs <- c(existing_PK$graphs, 
                                 ElimFitGraphs)
         
         Out[["graphs"]] <- existing_PK[["graphs"]]
      } else {
         Out[["graphs"]] <- ElimFitGraphs
      }
      
      SortIDs <- ct_dataframe_orig %>% 
         select(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                Simulated, File, ObsFile, DoseNum) %>% 
         unique() %>% 
         arrange(DoseNum, CompoundID, Inhibitor, Tissue, File, ObsFile, 
                 Trial, Individual) %>% 
         mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                           ifelse(Simulated == TRUE, "simulated", "observed"),
                           File, ObsFile, DoseNum)) %>% 
         pull(ID)
      
      # Main graphs and then special interval graphs
      Out[["graphs"]] <- Out[["graphs"]][c(intersect(SortIDs, names(Out[["graphs"]])), 
                                           setdiff(names(ElimFitGraphs), SortIDs))]
      
   }
   
   if(exists("DoseIntChecks", inherits = FALSE)){
      
      DoseIntWarnings <- unlist(DoseIntWarnings)
      DoseIntWarnings <- DoseIntWarnings[str_detect(DoseIntWarnings, "mismatch")]
      
      if(length(DoseIntWarnings) > 0){
         warning(str_c(DoseIntWarnings, collapse = "\n"), 
                 call. = FALSE)
      }
   }
   
   return(Out)
   
}

