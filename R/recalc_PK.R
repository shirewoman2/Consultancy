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
#' @param first_dose_time the time at which the first dose was administered.
#'   Leaving this as the default (NA) will set this to 0.
#' @param last_dose_time NA the time at which the last dose was administered.
#'   Leaving this as the default (NA) means that we'll assume that the last dose
#'   was administered at the earliest time included in the data for the highest
#'   dose number.
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
#' @param fit_points_after_x_time optionally specify that you want to fit only
#'   points after a certain time after the most-recent dose. Default of NA means
#'   that we'll fit all the data after tmax. Keep in mind that this will apply
#'   to ALL profiles.
#' @param returnAggregateOrIndiv return aggregate (default) and/or individual PK
#'   parameters? Options are "aggregate", "individual", or "both". For aggregate
#'   data, values are pulled from simulator output -- not calculated -- and the
#'   output will be a data.frame with the PK parameters in columns and the
#'   statistics reported exactly as in the simulator output file.
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
   
   MatchIDs <- list()
   for(i in names(PossMatchCols)){
      MatchIDs[[i]] <- which(t(existing_PK_indiv[, i]) %in% PossMatchCols[[i]])
   }
   MatchIDs <- unique(existing_PK_indiv$ID[Reduce(intersect, MatchIDs)])
   
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

