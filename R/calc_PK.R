#' Calculate basic PK parameters for the 1st and last doses of
#' concentration-time data
#'
#' \code{calc_PK} calculates AUCinf_dose1, AUCt_dose1, AUCtau_last, Cmax_dose1,
#' Cmax_last, tmax_dose1, tmax_last, CLinf_dose1, and CLtau_last for the
#' supplied concentration-time data and, when applicable, the same parameters in
#' the presence of an effector and the ratios of those values for effector /
#' baseline. This can accommodate multiple simulations and multiple compounds as
#' long as the dosing regimen was the same. For example, this will do fine with
#' calculating the last-dose PK for two simulations where the last dose of
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
   
   # Main body of function -------------------------------------------------
   
   if("File" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$File <- NA
   }
   
   if("ObsFile" %in% names(ct_dataframe) == FALSE){
      ct_dataframe$ObsFile <- NA
   }
   
   if(is.na(first_dose_time)){
      first_dose_time <- min(ct_dataframe$Time[ct_dataframe$DoseNum == 1], na.rm = T)
   }
   
   if(is.na(last_dose_time)){
      MaxDoseNum <- max(ct_dataframe$DoseNum, na.rm = T)
      last_dose_time <- min(ct_dataframe$Time[ct_dataframe$DoseNum == MaxDoseNum], na.rm = T)
   }
   
   # Adjusting time to be time since most-recent dose
   ct_dataframe <- ct_dataframe %>% 
      mutate(TimeOrig = Time, 
             DoseTime = ifelse(DoseNum == 1, first_dose_time, last_dose_time), 
             Time = TimeOrig - DoseTime)
   
   ct_dataframe <- ct_dataframe %>% 
      group_by(CompoundID, Inhibitor, Tissue, Individual, Trial, 
               Simulated, File, ObsFile, DoseNum)
   
   suppressMessages(
      t0s <- ct_dataframe %>% filter(complete.cases(Conc)) %>% 
         summarize(t0 = min(Time), 
                   MaxTime = max(Time)) %>% 
         mutate(MaxTime = 
                   switch(as.character(complete.cases(dose_interval)), 
                          "TRUE" = t0 + dose_interval, 
                          "FALSE" = MaxTime))
   )
   # Not sure this will be best way to set this up
   
   Keys <- group_keys(ct_dataframe) %>% 
      mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                        ifelse(Simulated == TRUE, "simulated", "observed"),
                        File, ObsFile, DoseNum))
   
   ct_dataframe <- group_split(ct_dataframe)
   names(ct_dataframe) <- Keys$ID
   t0s <- group_split(t0s)
   names(t0s) <- Keys$ID
   
   ElimFits <- list()
   ElimFitGraphs <- list()
   PKtemp <- list()
   
   for(j in names(ct_dataframe)){
      
      ThisIsDose1 <- unique(ct_dataframe[[j]]$DoseNum) == 1
      MyDose <- switch(unique(ct_dataframe[[j]]$CompoundID), 
                       "substrate" = unique(ct_dataframe[[j]]$Dose_sub), 
                       "primary metabolite 1" = NA, 
                       "primary metabolite 2" = NA, 
                       "secondary metabolite" = NA,
                       "inhibitor 1" = unique(ct_dataframe[[j]]$Dose_inhib), 
                       "inhibitor 2" = unique(ct_dataframe[[j]]$Dose_inhib2),
                       "inhibitor 1 metabolite" = NA)
      
      LastDoseNum <- max(ct_dataframe[[j]]$DoseNum)
      SDorMD <- ifelse(LastDoseNum == 1, "SD", "MD")
      
      if(SDorMD == "MD"){
         ct_dataframe[[j]] <- ct_dataframe[[j]] %>% 
            filter(DoseNum %in% c(1, LastDoseNum))
      }
      
      ct_dataframe[[j]] <- ct_dataframe[[j]] %>% 
         filter(Time >= t0s[[j]]$t0 & Time <= t0s[[j]]$MaxTime)
      
      if(ThisIsDose1){
         
         # Subsetting the data if there are lots of it b/c fitting takes a lot
         # longer. When there are thousands of points, there are thousands of
         # values to minimize the residuals for. Instead of using all of the
         # data, using only a subset of the time points when there are more than
         # 100 observeations.
         if(nrow(ct_dataframe[[j]]) > 100){
            ct_dataframe[[j]] <- ct_dataframe[[j]][seq(from = 1, 
                                                       to = nrow(ct_dataframe[[j]]), 
                                                       length.out = 100), ]
            
         }
         
         if(is.na(fit_last_x_number_of_points)){
            Omit <- NA
         } else {
            Omit <- 1:(nrow(ct_dataframe[[j]]) - fit_last_x_number_of_points)
         }
         TEMP <- elimFit(DF = ct_dataframe[[j]], 
                         concentration = Conc, 
                         time = Time, 
                         tmax = fit_points_after_x_time, 
                         omit = Omit,
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
                  TEMP <- try(elimFit(DF = ct_dataframe[[j]], 
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
               TEMP <- elimFit(DF = ct_dataframe[[j]], 
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
         AUCextrap_temp <- noncompAUC(DF = ct_dataframe[[j]], 
                                      concentration = Conc, 
                                      time = Time, 
                                      type = "LULD", 
                                      extrap_inf = TRUE, 
                                      extrap_inf_coefs = ElimFits[[j]], 
                                      reportFractExtrap = TRUE)
      }
      
      AUCt_temp <- noncompAUC(DF = ct_dataframe[[j]], 
                              concentration = Conc, 
                              time = Time, 
                              type = "LULD", 
                              extrap_inf = FALSE)
      
      CmaxTmax_temp <- ct_dataframe[[j]] %>% 
         summarize(Cmax = max(Conc, na.rm = T), 
                   tmax = Time[which.max(Conc)])
      
      PKtemp[[j]] <- 
         data.frame(
            ID = j,
            WhichDose = ifelse({ThisIsDose1}, "dose1", "last"),
            CompoundID = unique(ct_dataframe[[j]]$CompoundID), 
            Inhibitor = unique(ct_dataframe[[j]]$Inhibitor),
            Tissue = unique(ct_dataframe[[j]]$Tissue),
            Individual = unique(ct_dataframe[[j]]$Individual), 
            Trial = unique(ct_dataframe[[j]]$Trial), 
            Simulated = unique(ct_dataframe[[j]]$Simulated), 
            File = unique(ct_dataframe[[j]]$File), 
            ObsFile = unique(ct_dataframe[[j]]$ObsFile), 
            DoseNum = unique(ct_dataframe[[j]]$DoseNum), 
            AUCinf = ifelse({ThisIsDose1 & Extrap}, AUCextrap_temp$AUC, NA),
            AUCinf_fraction_extrapolated = ifelse({ThisIsDose1 & Extrap}, 
                                                  AUCextrap_temp$`Fraction extrapolated to infinity`, 
                                                  NA),
            ExtrapProbs = ExtrapProbs,
            AUCt = AUCt_temp, 
            Cmax = CmaxTmax_temp$Cmax, 
            tmax = CmaxTmax_temp$tmax) %>% 
         mutate(CL = MyDose / ifelse({ThisIsDose1}, AUCinf, AUCt) * 1000)
      
      rm(AUCextrap_temp, AUCt_temp, CmaxTmax_temp, ExtrapProbs, Extrap)
   }
   
   PKtemp <- bind_rows(PKtemp) %>% 
      pivot_longer(cols = c(AUCinf, AUCinf_fraction_extrapolated, AUCt, Cmax, tmax, CL), 
                   names_to = "PKparameter", 
                   values_to = "Value") %>% 
      mutate(PKparameter = paste0(PKparameter, "_", WhichDose),
             PKparameter = 
                case_when(PKparameter == "AUCt_last" ~ "AUCtau_last",
                          PKparameter == "CL_last" ~ "CLtau_last", 
                          PKparameter == "CL_dose1" ~ "CLinf_dose1",
                          TRUE ~ PKparameter))
   
   if(any(PKtemp$ExtrapProbs)){
      warning(paste0(
         "The following combinations of data had problems with extrapolation to infinity. Data are listed by CompoundID, Inhibitor, Tissue, Individual, Trial, whether the data were simulated or observed, File, ObsFile, and DoseNum:\n", 
         str_c(unique(PKtemp$ID[PKtemp$ExtrapProbs], collapse = "\n"))), 
         call. = FALSE)
   }
   
   ## Checking for possible DDI parameters to calculate -----------------------
   
   if(any(PKtemp$Inhibitor != "none")){
      suppressMessages(
         PKDDI <- PKtemp %>% select(-ID) %>% 
            # Recoding inhibitor to be consistent. Only using "none" and "inhibitorX".
            mutate(Inhibitor = ifelse(Inhibitor == "none", Inhibitor, "inhibitorX")) %>% 
            pivot_wider(names_from = Inhibitor, 
                        values_from = Value) %>% 
            mutate(Value = inhibitorX / none, 
                   PKparameter = sub("_dose1", "_ratio_dose1", PKparameter), 
                   PKparameter = sub("_last", "_ratio_last", PKparameter)) %>% 
            select(CompoundID, Tissue, Individual, Trial,
                   Simulated, File, ObsFile, DoseNum, PKparameter, Value) %>% 
            # Remove any instances where there weren't matching data
            filter(complete.cases(Value)) %>% 
            # Get the inhibitor name back
            left_join(PKtemp %>% 
                         select(CompoundID, Tissue, Individual, 
                                Trial, Simulated, File, 
                                ObsFile, DoseNum, Inhibitor)) %>% 
            mutate(ID = paste(CompoundID, Inhibitor, Tissue, Individual, Trial, 
                              ifelse(Simulated == TRUE, "simulated", "observed"),
                              File, ObsFile, DoseNum))
      )
      
      PKtemp <- bind_rows(PKtemp, PKDDI) %>% 
         # Also fixing PK parameter names here since they should be different in
         # the presence of an effector.
         mutate(PKparameter = case_when(Inhibitor != "none" &
                                           !str_detect(PKparameter, "ratio") ~ 
                                           paste0(PKparameter, "_withInhib"), 
                                        TRUE ~ PKparameter))
      
   }
   
   ## Calculating aggregate stats --------------------------------------------
   
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
                           round_up_unit(length(Keys$ID[Keys$ID2 == i]) / ncol, 1), 
                           nrow)
            Ncol <- ifelse(is.null(ncol), 
                           round_up_unit(length(Keys$ID[Keys$ID2 == i]) / nrow, 1), 
                           ncol)
         }
         
         ggpubr::ggarrange(
            plotlist = ElimFitGraphs[Keys$ID[Keys$ID2 == i]], 
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
      Out[["graphs"]] <- ElimFitGraphs
   }
   
   return(Out)
   
}

