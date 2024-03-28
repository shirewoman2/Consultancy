#' Recalculate the PK for specific concentration-time profiles after running
#' \code{\link{calc_PK}}
#'
#' \code{recalc_PK} takes as input the output from \code{\link{calc_PK}} and
#' recalculates the PK for any concentration-time profiles specified. A few
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
                      fit_points_after_x_time = NA,
                      fit_last_x_number_of_points = NA, 
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
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if("character" %in% class(which_dose) == FALSE){
      warning("You requested something for the argument `which_dose` that was not a character vector, which is what we were expecting. We will return PK for the default doses: the first and the last.\n", 
              call. = FALSE)
      which_dose = c("first", "last")
   }
   
   which_dose <- sub("1st", "first", tolower(which_dose))
   SpecialInterval <- which_dose[str_detect(which_dose, "[0-9]")]
   which_dose_main <- which_dose[!str_detect(which_dose, "[0-9]")]
   if(length(SpecialInterval) > 0){
      SpecialInterval <- gsub(" ", "", SpecialInterval)
      SpecialInterval <- lapply(SpecialInterval,
                                FUN = function(x) as.numeric(str_split_1(x, "to")))
      if(any(lapply(SpecialInterval, length) != 2)){
         warning("Something is amiss with how you have specified a specific interval to integrate because we're not sure what the start and end times should be. We'll have to ignore the requested specific interval.\n", 
                 call. = FALSE)
         
         SpecialInterval <- SpecialInterval[which(lapply(SpecialInterval, length) == 2)]
      }
   }
   
   report_progress <- tolower(report_progress)
   if(report_progress %in% c("yes", "no", "some") == FALSE){
      warning("The options for report_progress are `yes`, `no`, or `some`, and you've entered something else. We'll set this to the default value of `some`.\n", 
              call. = FALSE)
   }
   
   if(complete.cases(fit_points_after_x_time) & complete.cases(fit_last_x_number_of_points)){
      warning("You requested that we fit both all points after time = X and also the last X number of points. You only get to use one of these options, so we'll use all points after time = X.\n", 
              call. = FALSE)
   }
   
   if(is.na(first_dose_time) & "logical" %in% class(existing_exp_details)){
      warning("IMPORTANT: You have not specified the time of the 1st dose. We'll assume it's the minimum time included in your data. Please make SURE that this is correct as incorrect t0 specification can mess up calculations.\n", 
              call. = FALSE)
   }
   
   if(is.na(last_dose_time) & any(ct_dataframe$DoseNum > 1) & 
      "logical" %in% class(existing_exp_details)){
      warning("You have not specified the time of the last dose. We'll assume it's the minimum time for the maxmimum dose number in your data.\n", 
              call. = FALSE)
   }
   
   # Tidying compound names
   if(length(compound_name) == 1 & complete.cases(compound_name) & 
      is.null(names(compound_name))){
      compound_name <- c("substrate" = compound_name)
   }
   
   if("Compound" %in% names(ct_dataframe) == FALSE || 
      all(is.na(ct_dataframe$Compound))){
      if(any(complete.cases(compound_name)) &&
         any(names(compound_name) %in% AllCompounds$CompoundID) == FALSE){
         warning("Some of the compound IDs used for naming the values for `compound_name` are not among the permissible compound IDs, so we won't be able to supply a compound name for any of the compound IDs listed. Please check the help file for what values are acceptable.\n", 
                 call. = FALSE)
         
         compound_name <- rep(NA, each = nrow(AllCompounds))
         names(compound_name) <- AllCompounds$CompoundID
      } else {
         Missing <- setdiff(AllCompounds$CompoundID, names(compound_name))
         ToAdd <- rep(NA, each = length(Missing))
         names(ToAdd) <- Missing
         compound_name <- c(compound_name, ToAdd)
         rm(Missing, ToAdd)
      }
   }
   
   # If they already had a compound name in the data, this will change it to
   # what they set with compound_name, but if they did not specify
   # compound_name, then this will leave all compound names alone.
   if(any(complete.cases(compound_name))){
      ct_dataframe$Compound <- compound_name[ct_dataframe$CompoundID]
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
   
   # Checking that they have all the columns necessary
   if(all(c("CompoundID", "Inhibitor", "Tissue", "Individual", "Trial", 
            "Simulated", "DoseNum") %in% names(ct_dataframe)) == FALSE){
      
      MissingCols <- setdiff(c("CompoundID", "Inhibitor", "Tissue", "Individual", "Trial", 
                               "Simulated", "DoseNum"), 
                             names(ct_dataframe))
      
      if("Individual" %in% MissingCols){
         warning("It looks like you might not have requested individual data when you ran the function `extractConcTime`. We need individual concentration-time profiles here.\n", 
                 call. = FALSE)
      } 
      stop(paste0("The data supplied for ct_dataframe is missing a column or columns called ", 
                  str_comma(paste0("`", MissingCols, "`")), ", so we cannot proceed. Please ensure that your data match the format you'd get from running the functions extractConcTime or extractObsConcTime."), 
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
      
      PossMatches <- expand.grid(PossMatchCols)
      
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
      summarize(MaxDoseNum = max(DoseNum))
   
   CTsubset <- CTsubset %>% 
      left_join(MaxDoseNum, by = "File") %>% 
      mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                        ifelse(Simulated == TRUE, "simulated", "observed"),
                        File, ObsFile, DoseNum)) %>% 
      filter(DoseNum %in% c(1, MaxDoseNum))
   
   
   ### Calculating PK parameters for individual datasets ------------------------
   
   ElimFits <- list()
   ElimFitGraphs <- list()
   PKtemp <- list()
   
   if(any(c("first", "last") %in% which_dose)){
      
      CTsubset <- split(CTsubset, CTsubset$DoseNum)
      
      #### dose 1 -------------------------------------------------------------
      
      if("first" %in% which_dose){
         
         if(report_progress %in% c("yes", "some")){message("Calculating 1st-dose PK")}
         
         if("logical" %in% class(existing_exp_details) == FALSE){
            suppressMessages(
               FirstDoseTime <- existing_exp_details$MainDetails %>% 
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
            
            MaxPossDoseTime <- CTsubset[["1"]] %>% ungroup() %>%
               filter(complete.cases(Conc)) %>%
               group_by(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial,
                        Simulated, File, ObsFile, DoseNum, ID) %>%
               summarize(MaxPossTime = max(Time, na.rm = T),
                         # For reasons that utterly baffle and infuriate me, dplyr
                         # will invisibly DROP the last grouping variable if the
                         # result would have only one row. (See help file for
                         # dplyr::summarize.) Why THAT would EVER be what people
                         # really want since the function that groups the data is NOT
                         # summarize and that this occurs even when they have
                         # specified .drop = FALSE in the function that DOES perform
                         # the grouping is *beyond me*.
                         .groups = "keep") 
            
            suppressMessages(
               FirstDoseTime <- FirstDoseTime %>% 
                  left_join(MaxPossDoseTime) %>% 
                  mutate(MaxTime = ifelse(is.na(MaxTime), 
                                          MaxPossTime, MaxTime)))
            
         } else {
            
            suppressMessages(suppressWarnings(
               FirstDoseTime <- CTsubset[["1"]] %>% ungroup() %>%
                  filter(complete.cases(Conc)) %>%
                  group_by(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial,
                           Simulated, File, ObsFile, DoseNum, ID) %>%
                  summarize(t0 = min(Time, na.rm = T),
                            MaxTime = max(Time, na.rm = T),
                            # For reasons that utterly baffle and infuriate me, dplyr
                            # will invisibly DROP the last grouping variable if the
                            # result would have only one row. (See help file for
                            # dplyr::summarize.) Why THAT would EVER be what people
                            # really want since the function that groups the data is NOT
                            # summarize and that this occurs even when they have
                            # specified .drop = FALSE in the function that DOES perform
                            # the grouping is *beyond me*.
                            .groups = "keep") %>%
                  mutate(MaxTime =
                            switch(as.character(complete.cases(dosing_interval)),
                                   "TRUE" = t0 + dosing_interval,
                                   "FALSE" = MaxTime)) %>% 
                  filter(complete.cases(File) | complete.cases(ObsFile))
            ))
            
            if(complete.cases(first_dose_time)){
               FirstDoseTime$t0 <- first_dose_time
            }
            
            if(complete.cases(dosing_interval)){
               FirstDoseTime$MaxTime <- FirstDoseTime$t0 + dosing_interval
            }
         }
         
         CTsubset[["1"]] <- split(CTsubset[["1"]], 
                                  CTsubset[["1"]]$ID)
         
         for(j in names(CTsubset[["1"]])){
            
            if(report_progress == "yes"){message(paste0("   Calculating PK for dataset ", j))}
            
            MyDose <- switch(unique(CTsubset[["1"]][[j]]$CompoundID), 
                             "substrate" = unique(CTsubset[["1"]][[j]]$Dose_sub), 
                             "primary metabolite 1" = NA, 
                             "primary metabolite 2" = NA, 
                             "secondary metabolite" = NA,
                             "inhibitor 1" = unique(CTsubset[["1"]][[j]]$Dose_inhib), 
                             "inhibitor 2" = unique(CTsubset[["1"]][[j]]$Dose_inhib2),
                             "inhibitor 1 metabolite" = NA)
            
            suppressMessages(
               CT_temp <- CTsubset[["1"]][[j]] %>% 
                  left_join(FirstDoseTime) %>% 
                  filter(Time >= t0 & Time <= MaxTime) %>% 
                  # NB: Need to subtract t0 here so that fitting
                  # starts at the right time. Also, there may be 0
                  # concs *after* t0 depending on the absorption,
                  # so I'm specifically using the minimum value so
                  # that all subjects in the same simulation have
                  # the same t0 for dose 1.
                  mutate(Time = Time - t0)
            )
            
            if(omit_0_concs){
               # NB: It's appropriate to set this to 0 rather t0 b/c we have
               # already subtracted t0. t0 = 0 now.
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
            
            if(effort_to_get_elimination_rate != "don't try"){
               
               TEMP <- elimFit(DF = CT_temp, 
                               concentration = Conc, 
                               time = Time, 
                               tmax = fit_points_after_x_time, 
                               omit = Omit,
                               weights = weights,
                               useNLS_outnames = FALSE,
                               modelType = "monoexponential",
                               graph = TRUE)
               
               if(any(TEMP == "Insufficient data to create model")){
                  
                  ElimFits[[j]] <- NULL
                  ExtrapProbs <- TRUE
                  AUCextrap_temp <- NA
                  
               }
            } else {
               
               ElimFits[[j]] <- NULL
               ExtrapProbs <- NA
               AUCextrap_temp <- NA
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
                                      graph = TRUE), 
                              silent = TRUE)
               )
            }
            
            # Check whether that attempt worked and try again if not if that's what the user wants.
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
                               graph = TRUE)
            }
            
            # Check whether things worked. If they did, make the graph.
            if("Estimates" %in% names(TEMP) &&
               (any(is.na(TEMP$Estimates$Beta)) |
                any(TEMP$Estimates$Estimate < 0)) == FALSE){
               ElimFitGraphs[[j]] <- TEMP$Graph +
                  ggtitle(sub("observed ", "observed\n", 
                              sub("simulated ", "simulated\n", j))) +
                  theme(title = element_text(size = 6))
            } 
            
            if("Estimates" %in% names(TEMP)){
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
               AUCextrap_temp <- noncompAUC(DF = CTsubset[["1"]][[j]], 
                                            concentration = Conc, 
                                            time = Time, 
                                            type = "LULD", 
                                            extrap_inf = TRUE, 
                                            extrap_inf_coefs = ElimFits[[j]], 
                                            reportFractExtrap = TRUE)
            }
            
            AUCt_temp <- noncompAUC(DF = CTsubset[["1"]][[j]], 
                                    concentration = Conc, 
                                    time = Time, 
                                    type = "LULD", 
                                    extrap_inf = FALSE)
            
            suppressMessages(
               CmaxTmax_temp <- CTsubset[["1"]][[j]] %>% 
                  # NB: Need to subtract t0 here so that fitting starts at the
                  # right time. Also, there may be 0 concs *after* t0 depending on
                  # the absorption, so I'm specifically using the minimum value so
                  # that all subjects in the same simulation have the same t0 for
                  # dose 1.
                  left_join(FirstDoseTime) %>% 
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
                  Compound = unique(CTsubset[["1"]][[j]]$Compound), 
                  CompoundID = unique(CTsubset[["1"]][[j]]$CompoundID), 
                  Inhibitor = unique(CTsubset[["1"]][[j]]$Inhibitor),
                  Tissue = unique(CTsubset[["1"]][[j]]$Tissue),
                  Individual = unique(CTsubset[["1"]][[j]]$Individual), 
                  Trial = unique(CTsubset[["1"]][[j]]$Trial), 
                  Simulated = unique(CTsubset[["1"]][[j]]$Simulated), 
                  File = unique(CTsubset[["1"]][[j]]$File), 
                  ObsFile = unique(CTsubset[["1"]][[j]]$ObsFile), 
                  DoseNum = unique(CTsubset[["1"]][[j]]$DoseNum), 
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
                      Dose = MyDose)
            
            suppressWarnings(
               rm(AUCextrap_temp, AUCt_temp, CmaxTmax_temp, ExtrapProbs, Extrap, 
                  TEMP, CT_temp))
         }
      }
      
      #### last dose -----------------------------------------------------------
      
      if("last" %in% which_dose & 
         # if only single dose, nothing will have a name other than 1
         length(CTsubset[setdiff(names(CTsubset), "1")]) > 0){
         
         if(report_progress %in% c("yes", "some")){message("Calculating last-dose PK")}
         
         # NB: There may be more than one maximum dose number b/c CTsubset could
         # include multiple sets of data, and that should be fine. 
         
         if("logical" %in% class(existing_exp_details) == FALSE){
            suppressMessages(
               LastDoseTime <- existing_exp_details$MainDetails %>% 
                  select(File, SimDuration, StartHr_sub, StartHr_inhib, StartHr_inhib2,
                         DoseInt_sub, DoseInt_inhib, DoseInt_inhib2, 
                         NumDoses_sub, NumDoses_inhib, NumDoses_inhib2) %>% 
                  left_join(ct_dataframe %>% select(File, CompoundID) %>% unique()) %>% 
                  mutate(t0 = 
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
                                      
                                      TRUE ~ t0))
            )
            
            # NB: Will get MaxTime from the step where we check dose intervals.
            
         } else {
            
            suppressMessages(suppressWarnings(
               LastDoseTime <- bind_rows(CTsubset[setdiff(names(CTsubset), "1")]) %>% 
                  ungroup() %>%
                  filter(complete.cases(Conc)) %>%
                  group_by(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial,
                           Simulated, File, ObsFile, DoseNum, ID) %>%
                  summarize(t0 = min(Time, na.rm = T),
                            MaxTime = max(Time, na.rm = T),
                            # For reasons that utterly baffle and infuriate me, dplyr
                            # will invisibly DROP the last grouping variable if the
                            # result would have only one row. (See help file for
                            # dplyr::summarize.) Why THAT would EVER be what people
                            # really want since the function that groups the data is NOT
                            # summarize and that this occurs even when they have
                            # specified .drop = FALSE in the function that DOES perform
                            # the grouping is *beyond me*.
                            .groups = "keep") %>%
                  mutate(MaxTime =
                            switch(as.character(complete.cases(dosing_interval)),
                                   "TRUE" = t0 + dosing_interval,
                                   "FALSE" = MaxTime)) %>% 
                  filter(complete.cases(File))
            ))
            
            if(complete.cases(last_dose_time)){
               LastDoseTime$t0 <- last_dose_time
            }
            
            if(complete.cases(dosing_interval)){
               LastDoseTime$MaxTime <- LastDoseTime$t0 + dosing_interval
            }
         }
         
         NotDose1 <- setdiff(names(CTsubset), "1")
         
         DoseIntChecks <- list()
         DoseIntWarnings <- list()
         
         for(i in NotDose1){
            
            # Checking for any mismatches w/dosing interval. 
            if("logical" %in% class(existing_exp_details) == FALSE){
               for(file in unique(CTsubset[[i]]$File)){
                  
                  DoseIntChecks[[file]] <- list()
                  DoseIntWarnings[[file]] <- list()
                  
                  for(cmpd in unique(CTsubset[[i]]$CompoundID[
                     CTsubset[[i]]$File == file])){
                     
                     TEMP <- check_doseint(sim_data_file = file, 
                                           existing_exp_details = existing_exp_details, 
                                           compoundID = cmpd, stop_or_warn = "warn")
                     
                     DoseIntWarnings[[file]][[cmpd]] <- TEMP$message
                     DoseIntChecks[[file]][[cmpd]] <- TEMP$interval
                  }
                  
                  DoseIntChecks[[file]] <- bind_rows(DoseIntChecks[[file]])
               }
               
               DoseIntChecks <- bind_rows(DoseIntChecks)
               
               if("data.frame" %in% class(DoseIntChecks)){
                  
                  suppressMessages(
                     LastDoseTime <- LastDoseTime %>% 
                        left_join(DoseIntChecks %>% 
                                     mutate(MaxTime = case_when(
                                        IntervalRemaining == 0 ~ SimDuration,
                                        TRUE ~ LastDoseTime + DoseInt_X)) %>% 
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
            }
            
            CTsubset[[i]] <- split(CTsubset[[i]], 
                                   CTsubset[[i]]$ID)
            
            for(j in names(CTsubset[[i]])){
               
               if(report_progress == "yes"){message(paste0("   Calculating PK for dataset", j))}
               
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
               
               if(omit_0_concs){
                  # NB: Here, since this will NOT include any true t0 data (t =
                  # 0 for dose 1 b/c  CTsubset[[i]][[j]] does not include dose 1
                  # data), all concentrations should be above 0 if that's what
                  # the user requested.
                  CTsubset[[i]][[j]] <- CTsubset[[i]][[j]] %>%
                     filter(Conc > 0)
               }
               
               # Make a graph of the last-dose data so that user can check that they're
               # integrating what they *think* they're integrating.
               ElimFitGraphs[[j]] <- 
                  ct_plot(CTsubset[[i]][[j]] %>% 
                             mutate(Trial = "mean"), 
                          figure_type = "means only", 
                          linear_or_log = "linear", 
                          graph_title = sub("observed ", "observed\n", 
                                            sub("simulated ", "simulated\n", j)), 
                          graph_title_size = 6) +
                  theme(text = element_text(size = 6))
               
               AUCt_temp <- noncompAUC(DF = CTsubset[[i]][[j]], 
                                       concentration = Conc, 
                                       time = Time, 
                                       type = "LULD", 
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
               FirstDoseTime <- existing_exp_details$MainDetails %>% 
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
               FirstDoseTime <- CTsubset[["1"]] %>% ungroup() %>%
                  filter(complete.cases(Conc)) %>%
                  group_by(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial,
                           Simulated, File, ObsFile, DoseNum, ID) %>%
                  summarize(t0 = min(Time, na.rm = T),
                            MaxTime = max(Time, na.rm = T),
                            # For reasons that utterly baffle and infuriate me, dplyr
                            # will invisibly DROP the last grouping variable if the
                            # result would have only one row. (See help file for
                            # dplyr::summarize.) Why THAT would EVER be what people
                            # really want since the function that groups the data is NOT
                            # summarize and that this occurs even when they have
                            # specified .drop = FALSE in the function that DOES perform
                            # the grouping is *beyond me*.
                            .groups = "keep") %>%
                  mutate(MaxTime =
                            switch(as.character(complete.cases(dosing_interval)),
                                   "TRUE" = t0 + dosing_interval,
                                   "FALSE" = MaxTime)) %>% 
                  filter(complete.cases(File) | complete.cases(ObsFile))
            ))
            
            if(complete.cases(first_dose_time)){
               FirstDoseTime$t0 <- first_dose_time
            }
         }
         
         if(omit_0_concs){
            CTsubset[[i]][[j]] <- CTsubset[[i]][[j]] %>%
               filter((Time > FirstDoseTime$t0 & Conc > 0) |
                         (Time == FirstDoseTime$t0))
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
               ct_plot(CTtemp[[j]] %>% 
                          mutate(Trial = "mean"), 
                       linear_or_log = "linear", 
                       figure_type = "means only",
                       graph_title = sub("observed ", "observed\n", 
                                         sub("simulated ", "simulated\n", j)), 
                       graph_title_size = 6) +
               theme(text = element_text(size = 6))
            
            AUCt_temp <- noncompAUC(DF = CTtemp[[j]], 
                                    concentration = Conc, 
                                    time = Time, 
                                    type = "LULD", 
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
   
   PKtemp <- bind_rows(PKtemp) %>% 
      pivot_longer(cols = c(AUCinf, AUCinf_fraction_extrapolated,
                            k, HalfLife,
                            AUCt, 
                            Cmax, Cmin, Clast, tmax, CL), 
                   names_to = "PKparameter", 
                   values_to = "Value") %>% 
      mutate(PKparameter = paste0(PKparameter, "_", WhichDose),
             PKparameter = 
                case_when(PKparameter == "AUCt_last" ~ "AUCtau_last",
                          PKparameter == "CL_last" ~ "CLtau_last", 
                          PKparameter == "CL_dose1" ~ "CLinf_dose1",
                          TRUE ~ PKparameter)) %>% 
      # Removing PK parameters that would not be accurate or informative for
      # that dose
      filter(!PKparameter %in% c("Cmin_dose1", "AUCinf_last", 
                                 "AUCinf_fraction_extrapolated_last")) 
   
   if(any(PKtemp$ExtrapProbs, na.rm = TRUE)){
      warning(paste0(
         "There were problems extrapolating to infinity for some data. The sets of data with problems are listed here by CompoundID, Inhibitor, Tissue, Individual, Trial, whether the data were simulated or observed, File, ObsFile, and DoseNum:\n", 
         str_c(sort(unique(PKtemp$ID[PKtemp$ExtrapProbs])), collapse = "\n")), 
         call. = FALSE)
   }
   
   # Replacing original PK and renaming this as PKtemp to match syntax in
   # original function
   PKtemp <- bind_rows(PKtemp) %>% 
      mutate(PKparameter = ifelse(Inhibitor != "none", 
                                  paste0(PKparameter, "_withInhib"), 
                                  PKparameter))
   
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
               Maximum = max(Value, na.rm = T)) 
      ))
      
   }
   
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
                DoseNum, ExtrapProbs, 
                PKparameter, Value)
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
         existing_PK[["graphs"]][MatchIDs] <- 
            ElimFitGraphs[MatchIDs]
         
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
   
   if(exists("DoseIntChecks", inherits = FALSE) &&
      any(unlist(map(DoseIntChecks, c(1, 1))) == "mismatch")){
      Problems <- unlist(map(DoseIntChecks, c(1, 1)))
      Problems <- Problems[Problems == "mismatch"]
      
      warning(paste0("For the following files, the dosing interval does not match the AUC interval for the last dose:\n", 
                     str_c(Problems, collapse = "\n")), 
              call. = FALSE)
   }
   
   return(Out)
   
}

