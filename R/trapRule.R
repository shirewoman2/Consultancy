#' Calculate the AUC using the trapezoidal rule
#'
#' Given a data.frame of concentration-time data, \code{trapRule} calculates the
#' area under the concentration-time curve for the times included in the input
#' data.frame using either the linear or the linear up/log down trapezoidal
#' rule. This does NOT extrapolate to infinity or back extrapolate to t0; it
#' \emph{only} calculates the area under the curve for the time points included.
#' If you supply a data.frame with multiple individuals, this will assume you
#' want to calculate the mean AUC and will still only return a single number. If
#' you want one AUC per subject, please see the examples at the bottom of the
#' help file.
#'
#' @param ct_dataframe input data.frame with concentration-time data.
#' @param concentration the name of the column containing drug concentrations
#'   (unquoted)
#' @param time the name of the column containing time data (unquoted)
#' @param type the type of trapezoidal rule to use. Options are "LULD" (default)
#'   for "linear up, log down" or "linear".
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
#'   \item The option of using cubic splines for calculating the AUC was
#'   intentionally omitted because they can become unstable with noisy
#'   concentration-time data and are thus less desirable than the trapezoidal
#'   rule. For more details, please see
#'   \url{https://www.certara.com/2011/04/02/calculating-auc-linear-and-log-linear/}.}
#'
#'
#'
#'
#' @return Returns the calculated AUC
#'
#' @examples
#' MDZmean <- MDZct %>% filter(Trial == "mean" & File == "mdz-5mg-sd-fa1.xlsx")
#' trapRule(MDZmean, concentration = Conc, time = Time)
#' trapRule(MDZmean, concentration = Conc, time = Time, type = "linear")
#'
#' # Get the AUC for each combination of individual and trial
#' MDZct %>% filter(File == "mdz-5mg-sd-fa1.xlsx" &
#'                      DoseNum == 1) %>%
#'     group_by(Individual, Trial) %>%
#'     group_modify(~ .x %>% summarize(
#'         AUC = trapRule(.x,
#'                        concentration = Conc,
#'                        time = Time,
#'                        type = "LULD")))
#'
#'
#' @export
#' 

trapRule <- function(ct_dataframe, 
                     concentration = Concentration,
                     time = Time,
                     type = "LULD") {
    
    # Error catching ---------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
             call. = FALSE)
    }
    
    if(nrow(ct_dataframe) == 0){
        warning("Please check your input. The data.frame you supplied for ct_dataframe doesn't have any rows.", 
                call. = FALSE)
        return(NA)
    }
    
    if(type %in% c("linear", "LULD") == FALSE){
        warning("The only options for type of AUC calculation are 'LULD' for 'linear up, log down' or 'linear', and your input doesn't match either of those. We'll use the default `LULD`.")
        type <- "LULD"
    }
    
    concentration <- rlang::enquo(concentration)
    time <- rlang::enquo(time)
    
    ct_dataframe <- ct_dataframe %>% dplyr::select(!! time, !! concentration) %>%
        dplyr::rename(TIME = !! time,
                      CONC = !! concentration)
    
    ct_dataframe <- ct_dataframe %>%  dplyr::filter(complete.cases(TIME))
    
    DFmean <- ct_dataframe %>%
        dplyr::filter(complete.cases(CONC)) %>%
        dplyr::group_by(TIME) %>%
        dplyr::summarize(CONC = mean(CONC), .groups = "drop_last") %>%
        dplyr::arrange(TIME)
    
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
        # This is for when the type is linear-up-log-down.
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
        # up into multiple ct_dataframes.
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
                    tempct_dataframe <- DFdown[RowsToUse[k]:(RowsToUse[k] + 1), ]
                    AUCsToAdd[k] <- lintrap(tempct_dataframe)
                } else {
                    tempct_dataframe <- DFdown[RowsToUse[k]:RowsToUse[k + 1], ]
                    AUCsToAdd[k] <- logtrap(tempct_dataframe)
                }
                rm(tempct_dataframe)
            }
            
            AUCdown <- sum(AUCsToAdd)
            
        } else {
            AUCdown <- logtrap(DFdown)
        }
        
        # Adding up and down portions of the curve
        AUClast <- sum(AUCup, AUCdown, na.rm = TRUE)
    }
    
    return(AUClast)
    
}





