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
#'
#' @return returns a list of individual and/or aggregate PK data
#' @export
#'
#' @examples
#' # None yet
#'
#' 
recalc_PK <- function(ct_dataframe,
                      compound_name = NA, 
                      perpetrator_name = NA,
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
   
   # NB: Even though this is called "recalc_PK", it's actually the workhorse
   # function for calculating ALL PK, regardless of whether it's been calculated
   # previously. The calc_PK function calls on this and just doesn't supply
   # anything for existing_PK.
   
   # Error catching and setting up --------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if(complete.cases(fit_points_after_x_time) & complete.cases(fit_last_x_number_of_points)){
      warning("You requested that we fit both all points after time = X and also the last X number of points. You only get to use one of these options, so we'll use all points after time = X.\n", 
              call. = FALSE)
   }
   
   if(is.na(first_dose_time)){
      warning("You have not specified the time of the 1st dose. We'll assume it's the minimum time included in your data.\n", 
              call. = FALSE)
   }
   
   if(is.na(last_dose_time) & any(ct_dataframe$DoseNum > 1)){
      warning("You have not specified the time of the last dose. We'll assume it's the minimum time for the maxmimum dose number in your data.\n", 
              call. = FALSE)
   }
   
   # Tidying compound names
   if(length(compound_name) == 1 & complete.cases(compound_name) & 
      is.null(names(compound_name))){
      compound_name <- c("substrate" = compound_name)
   }
   
   if("Compound" %in% names(ct_dataframe) == FALSE){
      if(any(names(compound_name) %in% AllCompounds$CompoundID) == FALSE){
         warning("Some of the compound IDs used for naming the values for `compound_name` are not among the permissible compound IDs, so we won't be able to supply a compound name for any of the compound IDs listed. Please check the help file for what values are acceptable.\n", 
                 call. = FALSE)
         
         compound_name <- rep(NA, each = nrow(AllCompounds))
         names(compound_name) <- AllCompounds$CompoundID
      } else {
         Missing <- setdiff(AllCompounds$CompoundID, names(compound_name))
         ToAdd <- rep(NA, each = length(Missing))
         names(ToAdd) <- Missing
         compound_name <- c(compound_name, Missing)
         rm(Missing, ToAdd)
      }
      
      ct_dataframe$Compound <- compound_name[ct_dataframe$CompoundID]
      
   }
   
   if("Inhibitor" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$Inhibitor <- "none"
   }
   
   if(complete.cases(perpetrator_name)){
      ct_dataframe$Inhibitor[ct_dataframe$Inhibitor != "none"] <- perpetrator_name
   }
   
   # Main body of function -------------------------------------------------
   
   if("File" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$File <- NA
   }
   
   if("ObsFile" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$ObsFile <- NA
   }
   
   if(omit_0_concs){
      ct_dataframe <- ct_dataframe %>%
         filter((Time != 0 & Conc > 0) | Time == 0)
   }
   
   if(is.na(first_dose_time)){
      first_dose_time <- min(ct_dataframe$Time[ct_dataframe$DoseNum == 1], na.rm = T)
   }
   
   MaxDoseNum <- max(ct_dataframe$DoseNum, na.rm = T)
   
   if(is.na(last_dose_time)){
      last_dose_time <- min(ct_dataframe$Time[ct_dataframe$DoseNum == MaxDoseNum], na.rm = T)
   }
   
   # Adjusting time to be time since most-recent dose. Only keeping 1st and
   # last-dose data.
   ct_dataframe <- ct_dataframe %>% 
      filter(DoseNum %in% c(1, MaxDoseNum)) %>% 
      mutate(TimeOrig = Time, 
             DoseTime = ifelse(DoseNum == 1, first_dose_time, last_dose_time), 
             Time = TimeOrig - DoseTime)
   
   # Finding the profiles to recalculate 
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
      CTsubset <- ct_dataframe
   }
   
   CTsubset <- CTsubset %>% 
      group_by(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial, 
               Simulated, File, ObsFile, DoseNum)
   
   suppressMessages(
      t0s <- CTsubset %>% filter(complete.cases(Conc)) %>% 
         group_by(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial, 
                  Simulated, File, ObsFile, DoseNum, # why the @#$% won't DoseNum be included in group_var() sometimes after this?!?!?!?!?!
                  .drop = FALSE) %>% 
         summarize(t0 = min(Time), 
                   MaxTime = max(Time)) %>% 
         mutate(MaxTime = 
                   switch(as.character(complete.cases(dose_interval)), 
                          "TRUE" = t0 + dose_interval, 
                          "FALSE" = MaxTime))
   )
   # Not sure this will be best way to set this up
   
   ## Calculating PK parameters for individual datasets ------------------------
   
   Keys_CT <- CTsubset %>%
      select(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial, 
             Simulated, File, ObsFile, DoseNum) %>% unique() %>% 
      mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                        ifelse(Simulated == TRUE, "simulated", "observed"),
                        File, ObsFile, DoseNum))
   
   # Keys for CT and t0 *should* be the same, but setting this up separately to
   # get names for t0s means that this will check that they contain the same
   # info.
   Keys_t0 <- t0s %>%
      select(Compound, CompoundID, Inhibitor, Tissue, Individual, Trial, 
             Simulated, File, ObsFile, DoseNum) %>% unique() %>% 
      mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                        ifelse(Simulated == TRUE, "simulated", "observed"),
                        File, ObsFile, DoseNum))
   
   CTsubset <- group_split(CTsubset)
   names(CTsubset) <- Keys_CT$ID
   t0s <- group_split(t0s)
   names(t0s) <- Keys_t0$ID
   
   ElimFits <- list()
   ElimFitGraphs <- list()
   PKtemp <- list()
   
   for(j in names(CTsubset)){
      
      ThisIsDose1 <- unique(CTsubset[[j]]$DoseNum) == 1
      MyDose <- switch(unique(CTsubset[[j]]$CompoundID), 
                       "substrate" = unique(CTsubset[[j]]$Dose_sub), 
                       "primary metabolite 1" = NA, 
                       "primary metabolite 2" = NA, 
                       "secondary metabolite" = NA,
                       "inhibitor 1" = unique(CTsubset[[j]]$Dose_inhib), 
                       "inhibitor 2" = unique(CTsubset[[j]]$Dose_inhib2),
                       "inhibitor 1 metabolite" = NA)
      
      LastDoseNum <- max(CTsubset[[j]]$DoseNum)
      SDorMD <- ifelse(LastDoseNum == 1, "SD", "MD")
      
      if(SDorMD == "MD"){
         CTsubset[[j]] <- CTsubset[[j]] %>% 
            filter(DoseNum %in% c(1, LastDoseNum))
      }
      
      CTsubset[[j]] <- CTsubset[[j]] %>% 
         filter(Time >= t0s[[j]]$t0 & Time <= t0s[[j]]$MaxTime)
      
      if(ThisIsDose1){
         
         # Subsetting the data if there are lots of it b/c fitting takes a lot
         # longer. When there are thousands of points, there are thousands of
         # values to minimize the residuals for. Instead of using all of the
         # data, using only a subset of the time points when there are more than
         # 100 observeations.
         if(nrow(CTsubset[[j]]) > 100){
            CTsubset[[j]] <- as.data.frame( # For some reason, you can't do this w/tibbles. 
               CTsubset[[j]])[seq(from = 1, 
                                  to = nrow(CTsubset[[j]]), 
                                  length.out = 100), ]
            
         }
         
         if(is.na(fit_last_x_number_of_points)){
            Omit <- NA
         } else {
            Omit <- 1:(nrow(CTsubset[[j]]) - fit_last_x_number_of_points)
         }
         
         TEMP <- elimFit(DF = CTsubset[[j]], 
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
         } else {
            
            if(any(is.na(TEMP$Estimates$Beta)) |
               any(TEMP$Estimates$Estimate < 0)){
               
               cc <- capture.output(
                  type="message",
                  TEMP <- try(elimFit(DF = CTsubset[[j]], 
                                      concentration = Conc, 
                                      time = Time, 
                                      useNLS_outnames = FALSE,
                                      modelType = "biexponential",
                                      startValues = data.frame(A = c(100, 1000), 
                                                               alpha = c(0.1, 1), 
                                                               B = c(1, 100), 
                                                               beta = c(0.01, 0.1)),
                                      graph = TRUE), 
                              silent = TRUE)
               )
            }
            
            if(inherits(TEMP, "try-error") |
               (inherits(TEMP, "try-error") == FALSE && 
                (any(is.na(TEMP$Estimates$Beta)) |
                 any(TEMP$Estimates$Estimate < 0)))){
               TEMP <- elimFit(DF = CTsubset[[j]], 
                               concentration = Conc, 
                               time = Time, 
                               useNLS_outnames = FALSE,
                               modelType = "monoexponential",
                               graph = TRUE)
            }
            
            if((any(is.na(TEMP$Estimates$Beta)) |
                any(TEMP$Estimates$Estimate < 0)) == FALSE){
               ElimFitGraphs[[j]] <- TEMP$Graph +
                  ggtitle(sub("observed ", "observed\n", 
                              sub("simulated ", "simulated\n", j))) +
                  theme(title = element_text(size = 6))
            } 
            
            ElimFits[[j]] <- TEMP$Estimates
            
            ExtrapProbs <- any(is.na(ElimFits[[j]]$Beta))
            
         }
         
         rm(TEMP)
         
         Extrap <- ifelse(ExtrapProbs == FALSE, 
                          TRUE, FALSE)
         
      } else {
         Extrap <- FALSE
         ExtrapProbs <- NA
         ElimFits[[j]] <- NULL
      }
      
      if(ThisIsDose1 & is.null(ElimFits[[j]]) == FALSE & 
         Extrap == TRUE){
         AUCextrap_temp <- noncompAUC(DF = CTsubset[[j]], 
                                      concentration = Conc, 
                                      time = Time, 
                                      type = "LULD", 
                                      extrap_inf = TRUE, 
                                      extrap_inf_coefs = ElimFits[[j]], 
                                      reportFractExtrap = TRUE)
      }
      
      AUCt_temp <- noncompAUC(DF = CTsubset[[j]], 
                              concentration = Conc, 
                              time = Time, 
                              type = "LULD", 
                              extrap_inf = FALSE)
      
      CmaxTmax_temp <- CTsubset[[j]] %>% 
         summarize(Cmax = max(Conc, na.rm = T), 
                   tmax = Time[which.max(Conc)], 
                   Cmin = min(Conc, na.rm = T), 
                   Clast = Conc[which.max(Time)])
      
      PKtemp[[j]] <- 
         data.frame(
            ID = j,
            WhichDose = ifelse({ThisIsDose1}, "dose1", "last"),
            Compound = unique(CTsubset[[j]]$Compound), 
            CompoundID = unique(CTsubset[[j]]$CompoundID), 
            Inhibitor = unique(CTsubset[[j]]$Inhibitor),
            Tissue = unique(CTsubset[[j]]$Tissue),
            Individual = unique(CTsubset[[j]]$Individual), 
            Trial = unique(CTsubset[[j]]$Trial), 
            Simulated = unique(CTsubset[[j]]$Simulated), 
            File = unique(CTsubset[[j]]$File), 
            ObsFile = unique(CTsubset[[j]]$ObsFile), 
            DoseNum = unique(CTsubset[[j]]$DoseNum), 
            AUCinf = ifelse({ThisIsDose1 & Extrap}, AUCextrap_temp$AUC, NA),
            AUCinf_fraction_extrapolated = ifelse({ThisIsDose1 & Extrap}, 
                                                  AUCextrap_temp$`Fraction extrapolated to infinity`, 
                                                  NA),
            ExtrapProbs = ExtrapProbs,
            k = ElimFits[[j]]$Estimate[ElimFits[[j]]$Beta %in% c("k", "beta")]) %>% 
         mutate(HalfLife = log(2) / k,
                AUCt = AUCt_temp, 
                Cmax = CmaxTmax_temp$Cmax, 
                tmax = CmaxTmax_temp$tmax, 
                Cmin = CmaxTmax_temp$Cmin, 
                Clast = CmaxTmax_temp$Clast,
                CL = MyDose / ifelse({ThisIsDose1}, AUCinf, AUCt) * 1000, 
                Dose = MyDose)
      
      suppressWarnings(
         rm(AUCextrap_temp, AUCt_temp, CmaxTmax_temp, ExtrapProbs, Extrap)
      )
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
         "The following combinations of data had problems with extrapolation to infinity. Data are listed by CompoundID, Inhibitor, Tissue, Individual, Trial, whether the data were simulated or observed, File, ObsFile, and DoseNum:\n", 
         str_c(unique(PKtemp$ID[PKtemp$ExtrapProbs], collapse = "\n"))), 
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
      
      # Removing any ratios b/c we'll be recalcualting them anyway. 
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
                   PKparameter = sub("_dose1", "_ratio_dose1", PKparameter), 
                   PKparameter = sub("_last", "_ratio_last", PKparameter)) %>% 
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
            filter(PKparameter != "AUCinf_fraction_extrapolated_ratio_dose1")
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
               Mean = mean(Value, na.rm = T),
               SD = sd(Value, na.rm = T), 
               Geomean = gm_mean(Value), 
               GeoCV = gm_CV(Value), 
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
      
      SortIDs <- ct_dataframe %>% 
         select(any_of(group_vars(Keys_CT))) %>% 
         unique() %>% 
         arrange(across(group_vars(Keys_CT))) %>% 
         mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                           ifelse(Simulated == TRUE, "simulated", "observed"),
                           File, ObsFile, DoseNum)) %>% 
         pull(ID)
      
      Out[["graphs"]] <- Out[["graphs"]][intersect(SortIDs, names(Out[["graphs"]]))]
      
   }
   
   return(Out)
   
}

