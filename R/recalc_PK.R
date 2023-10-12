#' Recalculate the PK for specific concentration-time profiles after running
#' \code{\link{calc_PK}}
#'
#' \code{recalc_PK} takes as input the output from \code{\link{calc_PK}} and
#' recalculates the PK for any concentration-time profiles specified. Please
#' note that the aggregated PK will be recalculated to include the newly
#' calculated individual PK parameters.
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
#' @param existing_PK the output from \code{\link{calc_PK}}
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
#'   and B) they will mess up weighting, should you choose to use a "1/y" or
#'   "1/y^2" weighting scheme.
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
#'
#' @return returns a list of individual and/or aggregate PK data
#' @export
#'
#' @examples
#' # None yet
#'
#' 
recalc_PK <- function(ct_dataframe,
                      existing_PK,
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
   
   # Main body of function -------------------------------------------------
   
   if("File" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$File <- NA
   }
   
   if("ObsFile" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$ObsFile <- NA
   }
   
   # Finding the profiles to recalculate 
   ct_dataframe <- ct_dataframe %>% 
      mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                        ifelse(Simulated == TRUE, "simulated", "observed"),
                        File, ObsFile, DoseNum))
   
   if("individual" %in% names(existing_PK)){
      existing_PK_indiv <- existing_PK$individual
   } else {
      existing_PK_indiv <- 
         as.data.frame(matrix(data = NA, 
                              ncol = 10, 
                              dimnames = list(NULL, 
                                              c("CompoundID", "Inhibitor", 
                                                "Tissue", "Individual", 
                                                "Trial", "Simulated", "File",
                                                "ObsFile", "DoseNum", 
                                                "PKparameter"))))
   }
   
   existing_PK_indiv <- existing_PK_indiv %>% 
      mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                        ifelse(Simulated == TRUE, "simulated", "observed"),
                        File, ObsFile, DoseNum))
   
   PossMatchCols <- list("CompoundID" = compoundID_match, 
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
   
   Keys <- ct_dataframe %>% 
      group_by(CompoundID, Inhibitor, Tissue, Individual, Trial, 
               Simulated, File, ObsFile, DoseNum) %>% 
      mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                        ifelse(Simulated == TRUE, "simulated", "observed"),
                        File, ObsFile, DoseNum))
   
   NewPK <- calc_PK(ct_dataframe = CTsubset, 
                    first_dose_time = first_dose_time, 
                    last_dose_time = last_dose_time, 
                    dose_interval = dose_interval, 
                    fit_points_after_x_time = fit_points_after_x_time, 
                    fit_last_x_number_of_points = fit_last_x_number_of_points, 
                    omit_0_concs = omit_0_concs, 
                    weights = weights, 
                    returnAggregateOrIndiv = "individual", 
                    return_graphs_of_fits = return_graphs_of_fits, 
                    save_graphs_of_fits = FALSE)
   
   # Replacing original PK and renaming this as PKtemp to match syntax in
   # original function
   PKtemp <- existing_PK_indiv %>% 
      bind_rows(NewPK$individual)
   
   # Replacing original graphs
   if("graphs" %in% names(existing_PK)){
      existing_PK[["graphs"]][MatchIDs] <- 
         NewPK[["graphs"]][MatchIDs]
   } else {
      existing_PK[["graphs"]] <- NewPK[["graphs"]]
   }
   
   if(returnAggregateOrIndiv %in% c("aggregate", "both")){
      
      suppressMessages(
         PK_agg_temp <- PKtemp %>% 
            filter(PKparameter != "AUCinf_dose1" |
                      (PKparameter == "AUCinf_dose1" & ExtrapProbs == FALSE)) %>% 
            select(-ExtrapProbs) %>% 
            group_by(CompoundID, Inhibitor, Tissue, 
                     Simulated, File, ObsFile, DoseNum, PKparameter) %>% 
            summarize(
               Mean = mean(Value, na.rm = T),
               SD = sd(Value, na.rm = T), 
               Geomean = gm_mean(Value), 
               GeoCV = gm_CV(Value), 
               CI90_lower = gm_conf(Value, CI = 0.90)[1], 
               CI90_upper = gm_conf(Value, CI = 0.90)[2],
               Median = median(Value, na.rm = T), 
               Minimum = min(Value, na.rm = T), 
               Maximum = max(Value, na.rm = T)) 
      )
      
   }
   
   if(save_graphs_of_fits){
      # Grouping by File, CompoundID, Inhibitor, Tissue for saving
      Keys <- Keys %>% 
         mutate(ID2 = paste(File, CompoundID, Inhibitor, Tissue))
      
      for(i in unique(Keys$ID2)){
         if(all(is.null(nrow) & is.null(ncol))){
            Nrow <- NULL
            Ncol <- NULL
         } else {
            Nrow <- ifelse(is.null(nrow), 
                           round_up_unit(length(unique(Keys$ID[Keys$ID2 == i])) / ncol, 1), 
                           nrow)
            Ncol <- ifelse(is.null(ncol), 
                           round_up_unit(length(unique(Keys$ID[Keys$ID2 == i])) / nrow, 1), 
                           ncol)
         }
         
         ggpubr::ggarrange(
            plotlist = existing_PK[["graphs"]], #[Keys$ID[Keys$ID2 == i]], 
            ncol = Ncol, 
            nrow = Nrow)
         ggsave(paste0(gsub("/", "-", gsub(".xlsx", "", i)), ".png"), 
                height = fig_height, width = fig_width, dpi = 300)
      }
      
   }
   
   Out <- list()
   if(returnAggregateOrIndiv %in% c("individual", "both")){
      Out[["individual"]] <- PKtemp %>% 
         select(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                Simulated, File, ObsFile, DoseNum, ExtrapProbs, 
                PKparameter, Value)
   }
   
   if(returnAggregateOrIndiv %in% c("aggregate", "both")){
      Out[["aggregate"]] <- PK_agg_temp 
   }
   
   if(return_graphs_of_fits){
      Out[["graphs"]] <- existing_PK[["graphs"]]
   }
   
   return(Out)
   
}

