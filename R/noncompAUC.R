#' Calculate the AUC using the trapezoidal rule
#'
#' Given a data.frame of concentration-time data, \code{noncompAUC} calculates
#' the area under the concentration-time curve for the times included in the
#' input data.frame using either the linear or the linear up/log down
#' trapezoidal rule. Optionally extrapolate to infinity or back extrapolate to
#' t0.
#'
#' @param DF Input data.frame with concentration-time data.
#' @param concentration The name of the column containing drug concentrations
#' @param time The name of the column containing time data
#' @param type The type of trapezoidal rule to use. Options are "LULD" (default)
#'   for "linear up, log down" or "linear".
#' @param extrap_inf Extrapolate the curve to infinity? TRUE or FALSE.
#' @param extrap_inf_coefs If you've already performed a nonlinear regression to
#'   a monoexponential decay equation, \code{noncompAUC} will use that for
#'   extrapolation to infinity. Provide a data.frame of the coefficients from a
#'   \code{nls} regression. Format must be the standard output from
#'   \code{summary(nls(...))[["coefficients"]]} or the coefficients from
#'   \code{\link{elimFit}}. It doesn't matter what you name the coefficients,
#'   but this function assumes that they will be listed with the y-intercept
#'   \emph{A0} in the first row and the rate constant \emph{k} in the next row.
#'   Note that this is for a \emph{mono}exponential decay. If you've fit a bi-
#'   or triexponential decay model to your data, only supply the rate constant
#'   and y intercept for the \emph{terminal} portion of the data.
#' @param extrap_inf_times If \code{extrap_inf} is TRUE but you haven't supplied
#'   the coefficients from fitting a monoexponential decay to your data, specify
#'   the time range to use for fitting as \code{c(minTime, maxTime)}. If this is
#'   left as \code{c(NA, NA)}, the time range from tmax to tlast will be used.
#' @param reportTermFit  TRUE or FALSE for whether to report the fit for the
#'   terminal elimination calculation. If TRUE, this changes the output from a
#'   single number to a named list that includes the AUC and the terminal
#'   elimination fit (will be named "Terminal elimination fit") from
#'   \code{\link[stats]{nls}}.
#' @param reportFractExtrap TRUE or FALSE for whether to report the fraction of
#'   the AUC extrapolated to infinity. If TRUE, this changes the output from a
#'   single number to a named list that includes the AUC and the fraction
#'   extrapolated to infinity (will be named "Fraction extrapolated to
#'   infinity").
#'
#' @param extrap_t0 TRUE or FALSE for whether to back extrapolate the curve to
#'   0. This is useful when the dose was administered IV and you know you're
#'   missing a significant portion of the curve from t0 to the first sampling
#'   time.
#' @param extrap_t0_coefs If you've already performed a nonlinear regression to
#'   a monoexponential, biexponential, or triexponential decay equation,
#'   \code{noncompAUC} will use those for back extrapolation to t0. Provide a
#'   named list of: \describe{\item{\code{coefs}}{the coefficients from a
#'   \code{nls} regression. Format must be the standard output from
#'   \code{summary(nls(...))[["coefficients"]]} or the coefficients from
#'   \code{\link{elimFit}}. It doesn't matter what you name the coefficients,
#'   but this function assumes that they will be listed with the y-intercept for
#'   the first term in the first row, the rate constant for the first term in
#'   the next row, the y intercept for the second term in the next row, the rate
#'   constant for the second term in the next row after that, etc.}
#'   \item{\code{tmax}}{The time to start fitting the elimination curve. This
#'   will be used to determine how far back in time t0 is.} }
#' @param extrap_t0_model "monoexponential" or "biexponential". Only used when
#'   \code{extrap_t0} is TRUE and you haven't supplied your own fitted
#'   coefficients. This sets which exponential decay model to fit to your data
#'   and defaults to "monoexponential".
#' @param extrap_t0_times If \code{extrap_t0} is TRUE, specify the time range to
#'   use for fitting the exponential decay as \code{c(minTime, maxTime)}. If
#'   this is left as \code{c(NA, NA)}, the full time range from tmax to tlast
#'   will be used.
#' @param reportC0 TRUE or FALSE for whether to report the back-extrapolated
#'   concentration at t0. If TRUE, the output becomes a named list that includes
#'   the AUC and the extrapolated C0 value (will be named "C0"). This is useful
#'   as a sanity check because the maximum concentration at t0 in, e.g., plasma
#'   should be no larger than approximately the dose / total plasma volume,
#'   which is ~3 L in a healthy, 70-kg adult.
#' @param reportBackExtrapFit  TRUE or FALSE for whether to report the fit for
#'   the back-extrapolation to t0 calculation. If TRUE, this changes the output
#'   from a single number to a named list that includes the AUC and the back
#'   extrapolated fit (will be named "Back-extrapolation to t0 fit") from
#'   \code{\link[stats]{nls}}.
#'
#' @details \strong{A few notes:}\itemize{
#'
#'   \item If there are two consecutive time points with the same measured
#'   concentration, that results in an undefined value for the log trapezoidal
#'   rule. To deal with this, anytime the user has opted for the linear up/log
#'   down trapezoidal rule but there are also consecutive time points with the
#'   same concentration, those individual trapezoids will be calculated linearly
#'   rather than using a log function and all AUCs will be added together at the
#'   end.
#'
#'   \item I intentionally omitted the option of using cubic splines for
#'   calculating the AUC because they can become unstable with noisy
#'   concentration-time data and are thus less desireable than the trapezoidal
#'   rule. For more details, please see
#'   \url{https://www.certara.com/2011/04/02/calculating-auc-linear-and-log-linear/}.}
#'
#'
#'
#' @return Returns the calculated AUC as a number or, depending on the options
#'   selected, a named list of the AUC (\code{AUC[["AUC"]]}), the fraction of
#'   the curve extrapolated to infinity (\code{AUC[["Fraction extrapolated to
#'   infinity"]]}), and the back extrapolated C0 (\code{AUC[["C0"]]}).
#'
#' @examples
#' data(ConcTime)
#' IV1 <- ConcTime %>% dplyr::filter(SubjectID == 101 & Drug == "A" &
#'                                         DoseRoute == "IV")
#' noncompAUC(IV1, time = TimeHr)
#' noncompAUC(IV1, time = TimeHr, type = "LULD")
#' noncompAUC(IV1, time = TimeHr, type = "linear")
#'
#' # Extrapolating to infinity
#' noncompAUC(IV1, time = TimeHr, extrap_inf = TRUE,
#'            extrap_inf_times = c(0.5, NA))
#'
#' # Extrapolating to infinity and reporting the fraction extrapolated
#' noncompAUC(IV1, time = TimeHr, extrap_inf = TRUE,
#'            extrap_inf_times = c(0.5, NA),
#'            reportFractExtrap = TRUE)
#'
#' # Back extrapolating to t0
#' noncompAUC(IV1, time = TimeHr, extrap_t0 = TRUE,
#'            extrap_t0_model = "monoexponential",
#'            extrap_t0_times = c(0.5, NA))
#'
#' # Back extrapolating to t0 and reporting extrapolated C0
#' noncompAUC(IV1, time = TimeHr, extrap_t0 = TRUE,
#'            extrap_t0_model = "monoexponential",
#'            extrap_t0_times = c(0.5, NA),
#'            reportC0 = TRUE)
#'
#' # Extrapolating to infinity, reporting the fraction extrapolated to infinity,
#' # back extrapolating to t0, and reporting extrapolated C0
#' noncompAUC(IV1, time = TimeHr,
#'            extrap_inf = TRUE,
#'            extrap_inf_times = c(0.5, NA),
#'            reportFractExtrap = TRUE,
#'            extrap_t0 = TRUE,
#'            extrap_t0_model = "monoexponential",
#'            extrap_t0_times = c(0.5, NA),
#'            reportC0 = TRUE)
#'
#'
#' # For supplying your own coefficients to back extrapolate with:
#' tmax <- 0.5
#' # The elimination phase begins at t = 0.5 hrs here, so don't start fitting at
#' # t0; you'll get bad estimates.
#' Fit <- nls(Concentration ~ A * exp(-k * (TimeHr - tmax)),
#'            data = IV1 %>%  dplyr::filter(TimeHr >= tmax),
#'            start = list(A = max(IV1$Concentration), k = 0.01))
#' MyCoefs <- summary(Fit)[["coefficients"]]
#'
#' # Extrapolating to infinity with supplied coefficients
#' noncompAUC(IV1, time = TimeHr, extrap_inf = TRUE,
#'            extrap_inf_times = c(0.5, NA),
#'            extrap_inf_coefs = MyCoefs)
#'
#' # Back extapolating to t0 with supplied coefficients
#' noncompAUC(IV1, time = TimeHr, extrap_t0 = TRUE,
#'            extrap_t0_coefs = list(coefs = MyCoefs,
#'                                   tmax = tmax))
#'
#' @export
#'

noncompAUC <- function(DF, concentration = Concentration,
                       time = Time,
                       type = "LULD",
                       extrap_inf = FALSE,
                       extrap_inf_coefs = NULL,
                       extrap_inf_times = c(NA, NA),
                       reportTermFit = FALSE,
                       reportFractExtrap = FALSE,

                       extrap_t0 = FALSE,
                       extrap_t0_coefs = NULL,
                       extrap_t0_model = "monoexponential",
                       extrap_t0_times = c(NA, NA),
                       reportBackExtrapFit = FALSE,
                       reportC0 = FALSE) {


      # Defining pipe operator and bang bang
      `%>%` <- magrittr::`%>%`
      `!!` <- rlang::`!!`


      if(type %in% c("linear", "LULD") == FALSE){
            stop("The only options for type of AUC calculation are 'LULD' for 'linear up, log down' or 'linear'.")
      }

      if(extrap_t0_model %in% c("monoexponential", "biexponential") == FALSE){
            stop("The only options for models to automatically extrapolate back to t0 are 'monoexponential' or 'biexponential'. Please select one of those.")
      }

      if(is.null(extrap_t0_coefs) == FALSE &
         all(c("coefs", "tmax") %in% names(extrap_t0_coefs)) == FALSE){
            stop("If you supply coefficients and tmax in a list for back-extrapolating to t0, the names of those items must be 'coefs' and 'tmax'. Please check the names of the items in your supplied list.")
      }

      concentration <- rlang::enquo(concentration)
      time <- rlang::enquo(time)

      DF <- DF %>% dplyr::select(!! time, !! concentration) %>%
            dplyr::rename(TIME = !! time,
                          CONC = !! concentration)

      DF <- DF %>%  dplyr::filter(complete.cases(TIME))

      DFmean <- DF %>%
            dplyr::filter(complete.cases(CONC)) %>%
            dplyr::group_by(TIME) %>%
            dplyr::summarize(CONC = mean(CONC), .groups = "drop_last") %>%
            dplyr::arrange(TIME)

      if(extrap_inf){

            Tmax <- extrap_inf_times[1]

            if(is.na(extrap_inf_times[1])){
                  extrap_inf_times[1] <- min(DFmean$TIME, na.rm = TRUE)
            }

            if(is.na(extrap_inf_times[2])){
                  extrap_inf_times[2] <- max(DFmean$TIME, na.rm = TRUE)
            }

            if(is.null(extrap_inf_coefs)){
                  StartTime <- DFmean %>%  dplyr::filter(TIME >= extrap_inf_times[1]) %>%
                        dplyr::pull(TIME) %>% min
                  EndTime <- DFmean %>%  dplyr::filter(TIME <= extrap_inf_times[2]) %>%
                        dplyr::pull(TIME) %>% max

                  Fit_inf <- elimFit(DFmean %>%  dplyr::filter(TIME >= StartTime &
                                                                     TIME <= EndTime),
                                     concentration = CONC,
                                     time = TIME,
                                     tmax = Tmax,
                                     modelType = "monoexponential")

                  if(Fit_inf[[1]][1] == "Cannot fit to model"){
                        stop("Attempts to fit a monoexponential decay equation to the data failed to converge. Try a different time range.")
                  }

                  rm(StartTime, EndTime)

            } else {

                  Fit_inf <- as.data.frame(extrap_inf_coefs)
                  Fit_inf$Beta <- c("A", "k")

            }

            Clast <- DFmean %>%  dplyr::filter(complete.cases(CONC)) %>%
                  dplyr::summarize(Clast = CONC[which.max(TIME)],
                                   .groups = "drop_last") %>%  dplyr::pull(Clast)

            AUClast_inf <- Clast / Fit_inf$Estimate[Fit_inf$Beta == "k"]

            rm(Tmax)

      } else {
            AUClast_inf <- 0
      }

      if(extrap_t0){

            Tmax <- extrap_t0_times[1]

            if(is.na(extrap_t0_times[1])){
                  extrap_t0_times[1] <- min(DFmean$TIME, na.rm = TRUE)
            }

            if(is.na(extrap_t0_times[2])){
                  extrap_t0_times[2] <- max(DFmean$TIME, na.rm = TRUE)
            }

            if(is.null(extrap_t0_coefs)){
                  StartTime <- DFmean %>%  dplyr::filter(TIME >= extrap_t0_times[1]) %>%
                        dplyr::pull(TIME) %>% min
                  EndTime <- DFmean %>%  dplyr::filter(TIME <= extrap_t0_times[2]) %>%
                        dplyr::pull(TIME) %>% max

                  Fit_t0 <- elimFit(DFmean %>%  dplyr::filter(TIME >= StartTime &
                                                                    TIME <= EndTime),
                                    concentration = CONC,
                                    time = TIME,
                                    tmax = Tmax,
                                    modelType = extrap_t0_model)

                  Tmax <- ifelse(complete.cases(Tmax), Tmax,
                                 DFmean$TIME[which.max(DFmean$CONC)])

                  if(Fit_t0[[1]][1] == "Cannot fit to model"){
                        stop(paste("Attempts to fit a",
                                   extrap_t0_model,
                                   "decay equation to the data failed to converge. Try a different model."))
                  }

                  if(extrap_t0_model == "monoexponential"){
                        C0 <- Fit_t0$Estimate[Fit_t0$Beta == "A"] *
                              exp(-Fit_t0$Estimate[Fit_t0$Beta == "k"] * -Tmax)
                  } else {
                        C0 <- Fit_t0$Estimate[Fit_t0$Beta == "A"] *
                              exp(-Fit_t0$Estimate[Fit_t0$Beta == "alpha"] * -Tmax) +
                              Fit_t0$Estimate[Fit_t0$Beta == "B"] *
                              exp(-Fit_t0$Estimate[Fit_t0$Beta == "beta"] * -Tmax)
                  }

                  rm(StartTime, EndTime)

            } else {

                  MyCoefs <- as.data.frame(extrap_t0_coefs[["coefs"]])
                  tmax <- extrap_t0_coefs[["tmax"]]

                  if(nrow(MyCoefs) == 2){
                        C0 <- MyCoefs$Estimate[1] *
                              exp(-MyCoefs$Estimate[2] * -tmax)
                  }

                  if(nrow(MyCoefs) == 4){
                        C0 <- MyCoefs$Estimate[1] *
                              exp(-MyCoefs$Estimate[2] * -tmax) +
                              MyCoefs$Estimate[3] *
                              exp(-MyCoefs$Estimate[4] * -tmax)
                  }

                  if(nrow(MyCoefs) == 6){
                        C0 <- MyCoefs$Estimate[1] *
                              exp(-MyCoefs$Estimate[2] * -tmax) +
                              MyCoefs$Estimate[3] *
                              exp(-MyCoefs$Estimate[4] * -tmax) +
                              MyCoefs$Estimate[5] *
                              exp(-MyCoefs$Estimate[6] * -tmax)
                  }

            }

            if(any(DFmean$TIME == 0)){
                  DFmean$CONC[DFmean$TIME == 0] <- C0
            } else {
                  DFmean <- dplyr::bind_rows(DFmean,
                                             data.frame(CONC = C0, TIME = 0)) %>%
                        dplyr::arrange(TIME) %>% unique()
            }

            rm(Tmax)
      }

      # function for linear trapezoidal rule
      lintrap <- function(DFmean){
            sum(0.5*((DFmean$TIME[2:length(DFmean$TIME)] -
                            DFmean$TIME[1:(length(DFmean$TIME)-1)]) *
                           (DFmean$CONC[2:length(DFmean$CONC)] +
                                  DFmean$CONC[1:(length(DFmean$CONC)-1)])))
      }

      if(type == "linear"){
            AUClast <- lintrap(DFmean)

      } else {
            TMAX <- DFmean$TIME[which.max(DFmean$CONC)]

            DFup <- DFmean %>% dplyr::filter(TIME <= TMAX)
            DFdown <- DFmean %>% dplyr::filter(TIME >= TMAX)

            AUCup <- lintrap(DFup)

            # function for log trapezoidal rule
            logtrap <- function(DFdown){
                  sum(# C1 - C2
                        ((DFdown$CONC[1:(length(DFdown$CONC)-1)] -
                                DFdown$CONC[2:length(DFdown$CONC)]) /
                               # ln(C1) - ln(C2)
                               (log(DFdown$CONC[1:(length(DFdown$CONC)-1)]) -
                                      log(DFdown$CONC[2:length(DFdown$CONC)])) ) *
                              # t2 - t1
                              (DFdown$TIME[2:length(DFdown$TIME)] -
                                     DFdown$TIME[1:(length(DFdown$TIME)-1)]) )
            }

            # If any values for concentration are the same for two time points,
            # which WILL happen randomly sometimes due to inherent limitations
            # in measurements, use the linear trapezoidal rule to add that
            # trapezoid to the total AUC. To do that, I'll need to break those
            # up into multiple DFs.
            if(any(DFdown$CONC[1:(length(DFdown$CONC)-1)] ==
                   DFdown$CONC[2:length(DFdown$CONC)])){

                  # Noting which are problematic.
                  ProbPoints <- which(DFdown$CONC[1:(length(DFdown$CONC)-1)] ==
                                            DFdown$CONC[2:length(DFdown$CONC)])
                  AUCsToAdd <- c()
                  RowsToUse <- sort(unique(c(1, ProbPoints, ProbPoints + 1,
                                             nrow(DFdown))))
                  for(k in 1:(length(RowsToUse) - 1)){

                        if(RowsToUse[k] %in% ProbPoints){
                              tempDF <- DFdown[RowsToUse[k]:(RowsToUse[k] + 1), ]
                              AUCsToAdd[k] <- lintrap(tempDF)
                        } else {
                              tempDF <- DFdown[RowsToUse[k]:RowsToUse[k + 1], ]
                              AUCsToAdd[k] <- logtrap(tempDF)
                        }
                        rm(tempDF)
                  }

                  AUCdown <- sum(AUCsToAdd)

            } else {
                  AUCdown <- logtrap(DFdown)
            }

            # Adding up and down portions of the curve

            # If AUCup is NA, e.g., when it's an IV bolus so there's no
            # point where the concentration is increasing, then AUCup will
            # be NA. Need to account for that with na.rm = T.
            AUClast <- sum(AUCup, AUCdown, na.rm = TRUE)
      }

      # Total AUC will be AUClast + AUClast_inf. Note that if extrap_inf ==
      # FALSE, then AUClast_inf is 0.
      AUC <- AUClast + AUClast_inf

      if(reportFractExtrap | reportC0 | reportTermFit | reportBackExtrapFit){
            AUC <- list(AUC = AUC)

            if(reportC0 & extrap_t0){
                  AUC[["C0"]] <- C0
            }

            if(reportBackExtrapFit & extrap_t0){
                  AUC[["Back-extrapolation to t0 fit"]] <- Fit_t0
            }

            if(reportFractExtrap & extrap_inf){
                  AUC[["Fraction extrapolated to infinity"]] <-
                        AUClast_inf / AUC[["AUC"]]
            }

            if(reportTermFit & extrap_inf){
                  AUC[["Terminal elimination fit"]] <- Fit_inf
            }

      }

      return(AUC)

}



