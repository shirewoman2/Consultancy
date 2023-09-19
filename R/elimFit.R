#' Calculate the elimination rate(s) for a concentration-time curve
#'
#' \code{elimFit} fits an exponential decay equation to concentration-time data.
#' The equation, which is of the form \eqn{f(t) = concentration = A * exp(-kt)}
#' where A is ~Cmax, k is the elimination rate constant, and t is time. The
#' equation can be monoexponential, biexponential, or triexponential.
#'
#' @param DF The data.frame with the concentration-time data
#' @param startValues The starting values to be used in the fit. Options:
#'   \describe{\item{NA}{If left as \code{NA}, the starting values will be
#'   determined automatically.} \item{a list}{A list of starting values for the
#'   fit. Use the same values that you'd supply for \code{\link[stats]{nls}}.
#'   For a monoexponential fit, this will be a list of A and k. For a
#'   biexponential fit: A, alpha, B, beta. And for a triexponential fit: A,
#'   alpha, B, beta, G, gamma.} \item{a data.frame}{Sometimes, especially with
#'   sparsely sampled time points,  the \code{nls} algorithm can fail to
#'   converge if you don't have perfect estimates for the starting values. When
#'   that is the case, set \code{startValues} to a two row data.frame with
#'   columns for each coefficient. The first row should contain the minimum of
#'   the range to search, and the second row should contain the maximum of the
#'   range to search for that coefficient. This option uses
#'   \code{\link[nls2]{nls2}}, which does a more rigorous search for starting
#'   values than \code{\link[stats]{nls}}, to perform the regression. (For more
#'   information, see the documentation on \code{\link[nls2]{nls2}} and the
#'   algorithm "\code{random-search}".) \strong{A warning:} Because \code{nls2}
#'   searches more possible starting values, it can be appreciably slower than
#'   \code{nls}.} \strong{A piece of advice:} Regardless of whether you choose
#'   to use \code{nls} (supply no starting values or supply a list) or
#'   \code{nls2} (supply a data.frame), you truly will benefit by supplying
#'   reasonable start values. Even if you're supplying a data.frame for
#'   \code{nls2} to search, those values should still be reasonable or you just
#'   won't randomly select enough decent starting places to come up with
#'   regressions that will converge.}
#'
#' @param concentration The name of the column that contains concentration data
#' @param time The name of the column that contains time data
#' @param tmax The putative time at which the maximum concentration is observed,
#'   the time at which drug elimination becomes the dominant process. If left as
#'   NA, this will be whatever time is tmax. If you only want to fit a subset of
#'   times that don't necessarily include the actual tmax, set this to the time
#'   you want to start fitting. Time points before tmax will be omitted from the
#'   fit.
#' @param omit An index of which, if any, samples to omit from the regression.
#'   These samples will be depicted as red open circles in the graph, if you
#'   choose to make one, but will not be included in the regression. Note that
#'   only points after tmax are used in the regression anyway, so there's no
#'   need to omit, e.g., t0.
#' @param modelType The mathematical model to use for fitting the data; options
#'   are "monoexponential", "biexponential", or "triexponential".
#' @param returnDataUsed Should the data used be returned? I wrote this script
#'   initially for bootstrapping, where it can be useful to see what data were
#'   used as input. For that reason, I'm including the option of returning the
#'   data that were used.
#' @param weights Weighting scheme to use for the regression. User may supply a
#'   numeric vector of weights to use or choose from "1/x", "1/x^2", "1/y" or
#'   "1/y^2". If left as NULL, no weighting scheme will be used. Be careful that
#'   you don't have any infinite values or this will fail!
#' @param returnRSS TRUE or FALSE for whether to resturn the residual sum of
#'   squares. If set to TRUE, this will be the last column of the output
#'   data.frame where all rows = the residual sum of squares. (I wanted the
#'   output to still be a data.frame, so that's the place I could think of to
#'   put it.)
#' @param useNLS_outnames TRUE or FALSE for whether to use the standard output
#'   coeffecient names that come with the nls or nls2 functions, e.g.,
#'   "Estimate", "Std. Error", "t value", and "Pr(>|t|)". These names are
#'   annoying to work with for output data.frames b/c they don't follow standard
#'   column-naming practices (they contain spaces and symbols). If set to FALSE,
#'   the names of the output coefficient data.frame will be "Estimate", "SE",
#'   "tvalue" and "pvalue".
#' @param maxiter Maximum number of iterations of start values to use; default
#'   is 50, just like \code{\link[stats]{nls}}. (See also
#'   \code{\link[stats]{nls.control}}.) Using more iterations means more random
#'   sampling of starting values and thus a higher likelihood of the fit
#'   converging. However, it also means more processing time.
#' @param graph TRUE or FALSE for whether to create a graph of the data
#'
#' @return Returns a data.frame of the coefficients or returns a list containing
#'   whatever combination the user has specified of: \describe{\item{DataUsed}{A
#'   data.frame of the input data} \item{Estimates}{A data.frame of the
#'   estimated coefficients} \item{Graph}{A ggplot2 graph of the input data with
#'   a line showing the fit to the terminal phase data}}
#'
#' @examples
#' # Example data to work with:
#' data(ConcTime)
#' IV1 <- dplyr::filter(ConcTime, SubjectID == 101 & DoseRoute == "IV" &
#'                                      Drug == "A")
#' IV1 <- dplyr::select(IV1, SubjectID, TimeHr, Concentration)
#'
#' # Automatically select the start values for the regression and start
#' # fitting at whatever time is tmax
#' elimFit(IV1, concentration = Concentration, time = TimeHr,
#'             modelType = "monoexponential")
#'
#' # Set the start values yourself using a list (algorithm uses nls
#' # to fit the data).
#' elimFit(IV1, concentration = Concentration, time = TimeHr,
#'             startValues = list(A = 30, k = 0.01),
#'             modelType = "monoexponential")
#'
#' # Set the start values yourself but use the more robust nls2
#' # function to do the regression. Provide a range of values to search.
#' elimFit(IV1, concentration = Concentration, time = TimeHr,
#'             startValues = data.frame(A = c(5, 50), k = c(0.0001, 0.05)),
#'             modelType = "monoexponential")
#'
#' # Omit a point. In this case, omit the point at t = 8.
#' elimFit(IV1, concentration = Concentration, time = TimeHr,
#'             modelType = "monoexponential",
#'             omit = which(IV1$TimeHr == 8))
#'
#' # Don't start fitting until a later time than tmax, e.g., t = 4.
#' elimFit(IV1, concentration = Concentration, time = TimeHr,
#'             modelType = "monoexponential", tmax = 4))
#'
#' # Weight by 1/y.
#' elimFit(IV1, concentration = Concentration, time = TimeHr,
#'             weight = 1/IV1$Concentration,
#'             modelType = "monoexponential")
#'
#' # Another way to weight by 1/y
#' elimFit(IV1, concentration = Concentration, time = TimeHr,
#'             weight = "1/y",
#'             modelType = "monoexponential")
#'
#' # Get the residual sum of squares
#' elimFit(IV1, concentration = Concentration, time = TimeHr,
#'             modelType = "monoexponential", returnRSS = TRUE)
#'
#' # Use better names for the columns in the output
#' elimFit(IV1, concentration = Concentration, time = TimeHr,
#'             modelType = "monoexponential", useNLS_outnames = FALSE)
#'
#' # Graph the data; good for visually inspecting the fit.
#' elimFit(IV1, concentration = Concentration, time = TimeHr,
#'             modelType = "monoexponential", tmax = 4, graph = TRUE)
#'
#' # Some data for using the triexponential fit
#' TriExpDF <- data.frame(Time_min = c(0, 18, 33, 48, 63, 95, 123, 153, 183,
#'                                     213, 243, 303, 483),
#'                        Concentration = c(0, 420, 228, 143, 90, 48, 28,
#'                                          18, 13, 10, 7, 4, 2))
#'
#' # Some starting values to use for the triexponential fit
#' Start_tri <- data.frame(A = c(100, 500),
#'                         alpha = c(0.01, 0.5),
#'                         B = c(50, 200),
#'                         beta = c(0.001, 0.05),
#'                         G = c(1, 100),
#'                         gamma = c(1e-04, 0.01))
#'
#' elimFit(TriExpDF, startValues = Start_tri, concentration = Concentration,
#'         time = Time_min, tmax = 33, modelType = "triexponential",
#'         graph = TRUE)
#'
#'
#' @export
#'

elimFit <- function(DF, concentration = Concentration,
                    time = Time,
                    startValues = NA,
                    omit = NA,
                    tmax = NA,
                    modelType = "monoexponential",
                    returnDataUsed = FALSE,
                    weights = NULL,
                    returnRSS = FALSE,
                    useNLS_outnames = TRUE,
                    maxiter = 50,
                    graph = FALSE){

      # Defining pipe operator and bang bang
      `%>%` <- magrittr::`%>%`
      `!!` <- rlang::`!!`

      # Catching inappropriate model input
      if(modelType %in% c("monoexponential", "biexponential",
                          "triexponential") == FALSE){
            return("Acceptable model types are 'monoexponential', 'biexponential', or 'triexponential'.")
      }

      # Setting up the input data.frame
      DFinit <- DF
      concentration <- rlang::enquo(concentration)
      time <- rlang::enquo(time)

      # If the user supplied a value for "omit" but that value doesn't fall
      # within DF, issue a warning but keep going.
      if(any(complete.cases(omit)) & any(omit %in% 1:nrow(DF) == FALSE) |
         length(omit) == 0){
            message("One or more of the values supplied for 'omit' do not fall within the range of the data.frame supplied. All of the points supplied were included in the regression.")
      }

      DF <- DF %>% dplyr::select(!! concentration, !! time) %>%
            dplyr::rename(TIME = !! time,
                          CONC = !! concentration)

      # DFinit2 is for the purposes of graphing only.
      DFinit2 <- DFinit %>% dplyr::select(!! concentration, !! time) %>%
            dplyr::rename(TIME = !! time,
                          CONC = !! concentration)

      # Adding user-supplied weights. I need to add it here and then remove it
      # b/c I need to filter out the points that are before tmax and also
      # anything they want to omit.
      if(class(weights) == "numeric"){
            DF$WEIGHT <- weights
      }

      # Removing any rows the user requests
      if(any(complete.cases(omit)) & any(omit %in% 1:nrow(DF))){
            DFomit <- DF %>% dplyr::slice(omit)
            DF <- DF %>% dplyr::slice(-omit)
            DFinit2 <- DFinit2 %>% dplyr::slice(-omit)
      }

      DF <- DF[complete.cases(DF$CONC) & complete.cases(DF$TIME), ]
      DF <- dplyr::arrange(DF, TIME)

      # Setting tmax if it wasn't already.
      if(is.na(tmax)){
            tmax <- DF$TIME[which.max(DF$CONC)]
      }
      DF <- DF[DF$TIME >= tmax, ]

      # Re-establishing weights now that points that are to be omitted have been.
      if(class(weights) == "numeric"){
            weights <- DF$WEIGHT
      }

      # Accounting for the offset in time since tmax is often not at t0.
      DF$Time.offset <- DF$TIME - tmax

      # Catching inappropriate input with < 2 rows
      if(nrow(DF) < 2 | length(unique(DF$TIME)) <= 2){
            if(returnDataUsed){
                  Result <- list(DataUsed = DFinit,
                                 Estimates = "Insufficient data to create model")
                  return(Result)
            }
            else{
                  return("Insufficient data to create model")
            }
      }

      # Determining good starting values for the fit if the user didn't
      # already supply them
      if(is.na(startValues[[1]][1])){
            startValues.A <- max(DF$CONC)
            tfirstlast <- DF[c(1, nrow(DF)), ]
            startValues.k <- -1*((log(tfirstlast$CONC[2]) - log(tfirstlast$CONC[1]))/
                                       (tfirstlast$Time.offset[2] -
                                              tfirstlast$Time.offset[1]))

            if(startValues.k == Inf | is.nan(startValues.k)){
                  startValues.k <- 0.01
            }

            if(modelType == "monoexponential"){
                  startValues <- list(A = startValues.A,
                                      k = startValues.k)
            }

            if(modelType == "biexponential"){
                  startValues <- list(A = startValues.A,
                                      alpha = startValues.k,
                                      B = startValues.A/2,
                                      beta = 0.1) # Just guessing that this value might work
            }

            if(modelType == "triexponential"){
                  startValues <- list(A = startValues.A,
                                      alpha = startValues.k,
                                      B = startValues.A/2,
                                      beta = 0.1,
                                      G = startValues.A/3,
                                      gamma = 0.01) # Just guessing that this value might work
            }

      }


      # Setting up the weights to use
      if(class(weights) == "character"){

            WeightOptions <- DF %>%
                  dplyr::select(CONC, TIME) %>%
                  dplyr::mutate(One_x = 1/TIME,
                                One_x2 = 1/TIME^2,
                                One_y = 1/CONC,
                                One_y2 = 1/CONC^2)

            weights <- tolower(weights)

            MyWeights <- c("1/x" = "One_x", "1/x^2" = "One_x2",
                           "1/y" = "One_y", "1/y^2" = "One_y2")

            weights <- WeightOptions %>%  dplyr::pull(MyWeights[weights])
      }

      if(any(is.infinite(weights))){
            stop("The weights used for the regression cannot include infinite numbers. Please change the weighting scheme to avoid this.")
      }

      # Fitting
      Fit <- NULL
      if(modelType == "monoexponential"){
            # Determining whether to use nls or nls2 and then fitting
            if(class(startValues) == "list"){
                  Fit <- tryCatch(nls(CONC ~ A * exp(-k * Time.offset),
                                      data = DF,
                                      start = startValues,
                                      weights = weights,
                                      nls.control(maxiter = maxiter)),
                                  error = function(x) return("Cannot fit to model"))
            } else{
                  if(nrow(startValues) != 2 |
                     all(c("A", "k") %in%
                         names(startValues)) == FALSE){
                        stop("If you submit a data.frame with possible starting values for a monoexponential model, there must be two rows for each coefficient -- A and k -- to be fit.")
                  }

                  Fit <- tryCatch(nls2::nls2(CONC ~ A * exp(-k * Time.offset),
                                             data = DF,
                                             start = startValues,
                                             weights = weights,
                                             nls.control(maxiter = maxiter)),
                                  error = function(x) return("Cannot fit to model"))
            }
      }

      if(modelType == "biexponential"){
            # Determining whether to use nls or nls2 and then fitting
            if(class(startValues) == "list"){
                  Fit <- tryCatch(nls(
                        CONC ~ A * exp(-alpha * Time.offset) +
                              B * exp(-beta * Time.offset),
                        data = DF,
                        start = startValues,
                        weights = weights,
                        nls.control(maxiter = maxiter)),
                        error = function(x) return("Cannot fit to model"))
            } else{
                  if(nrow(startValues) != 2 |
                     all(c("A", "alpha", "B", "beta") %in%
                         names(startValues)) == FALSE){
                        stop("If you submit a data.frame with possible starting values for a biexponential model, there must be two rows for each coefficient -- A, alpha, B, and beta -- to be fit.")
                  }

                  Fit <- tryCatch(nls2::nls2(
                        CONC ~ A * exp(-alpha * Time.offset) +
                              B * exp(-beta * Time.offset),
                        data = DF,
                        start = startValues,
                        nls.control(maxiter = maxiter),
                        weights = weights),
                        error = function(x) return("Cannot fit to model"))
            }
      }

      if(modelType == "triexponential"){
            # Determining whether to use nls or nls2
            if(class(startValues) == "list"){
                  Fit <- tryCatch(nls(
                        CONC ~ A * exp(-alpha * Time.offset) +
                              B * exp(-beta * Time.offset) +
                              G * exp(-gamma * Time.offset),
                        data = DF,
                        start = startValues,
                        weights = weights,
                        nls.control(maxiter = maxiter)),
                        error = function(x) return("Cannot fit to model"))
            } else{
                  if(nrow(startValues) != 2 |
                     all(c("A", "alpha", "B", "beta", "G", "gamma") %in%
                         names(startValues)) == FALSE){
                        stop("If you submit a data.frame with possible starting values for a triexponential model, there must be two rows for each coefficient -- A, alpha, B, beta, G, and gamma -- to be fit.")
                  }

                  Fit <- tryCatch(nls2::nls2(
                        CONC ~ A * exp(-alpha * Time.offset) +
                              B * exp(-beta * Time.offset) +
                              G * exp(-gamma * Time.offset),
                        data = DF,
                        start = startValues,
                        weights = weights,
                        nls.control(maxiter = maxiter)),
                        error = function(x) return("Cannot fit to model"))
            }
      }

      # Setting up the data to graph
      CurveData <- data.frame(Time.offset = seq(min(DF$Time.offset, na.rm = T),
                                                max(DF$Time.offset, na.rm = T),
                                                length.out = 100))

      # Checking whether the fit worked at all
      if(class(Fit) == "character"){
            # This is for when the fit did NOT work. Returning a data.frame of
            # NA values.
            DataUsed <- DFinit
            Estimates <- data.frame(Beta = NA, Estimate = NA, SE = NA,
                                    tvalue = NA, pvalue = NA)

            if(useNLS_outnames){
                  names(Estimates) <- c("Beta", "Estimate", "Std. Error", "t value",
                                        "Pr(>|t|)")
            }

            Estimates$Message <- "Cannot fit to model"

            Graph <- ggplot2::ggplot(DFinit2, ggplot2::aes(x = TIME, y = CONC)) +
                  ggplot2::geom_point() +
                  ggplot2::scale_y_log10() +
                  ggplot2::xlab(rlang::as_label(time)) + ggplot2::ylab(rlang::as_label(concentration))

            if(any(complete.cases(omit)) & any(omit %in% 1:nrow(DFinit))){
                  Graph <- Graph +
                        ggplot2::geom_point(data = DFomit, color = "red", shape = "O", size = 2)

            }

            Result <- list(DataUsed = DataUsed, Estimates = Estimates)

      } else { # This is for when the fit did work fine.

            Estimates <- as.data.frame(summary(Fit)[["coefficients"]])

            if(modelType == "monoexponential"){
                  CurveData <- CurveData %>%
                        dplyr::mutate(A = Estimates[row.names(Estimates) == "A", "Estimate"],
                                      k = Estimates[row.names(Estimates) == "k", "Estimate"],
                                      CONC = A * exp(-k * Time.offset))

                  Estimates <- Estimates %>%
                        dplyr::mutate(Beta = row.names(Estimates)) %>%
                        dplyr::select(Beta, everything())

            }

            if(modelType == "biexponential"){
                  CurveData <- CurveData %>%
                        dplyr::mutate(A = Estimates[row.names(Estimates) == "A", "Estimate"],
                                      alpha = Estimates[row.names(Estimates) == "alpha", "Estimate"],
                                      B = Estimates[row.names(Estimates) == "B", "Estimate"],
                                      beta = Estimates[row.names(Estimates) == "beta", "Estimate"],
                                      CONC = A * exp(-alpha * Time.offset) +
                                            B * exp(-beta * Time.offset))

                  # Figuring out which beta is which and making it consistent
                  Betas <- Estimates %>%
                        dplyr::mutate(ParamSet = c("Set1", "Set1", "Set2", "Set2"),
                                      ParamType = rep(c("A0", "rate"), 2)) %>%
                        dplyr::select(Estimate, ParamSet, ParamType) %>%
                        tidyr::spread(key = ParamType, value = Estimate) %>%
                        dplyr::arrange(-A0) %>%
                        dplyr::mutate(ParamSet = c("A", "B"))

                  A <- Betas$A0[Betas$ParamSet == "A"]
                  alpha <- Betas$rate[Betas$ParamSet == "A"]
                  B <- Betas$A0[Betas$ParamSet == "B"]
                  beta <- Betas$rate[Betas$ParamSet == "B"]

                  Betas <- data.frame(Beta = c("A", "alpha", "B", "beta"),
                                      Estimate = c(A, alpha, B, beta)) %>%
                        dplyr::mutate(Beta = factor(Beta, levels = Beta))

                  Estimates <- Estimates %>% dplyr::left_join(Betas, by = "Estimate") %>%
                        dplyr::arrange(Beta) %>%
                        dplyr::select(Beta, everything())
            }

            if(modelType == "triexponential"){
                  CurveData <- CurveData %>%
                        dplyr::mutate(A = Estimates[row.names(Estimates) == "A", "Estimate"],
                                      alpha = Estimates[row.names(Estimates) == "alpha", "Estimate"],
                                      B = Estimates[row.names(Estimates) == "B", "Estimate"],
                                      beta = Estimates[row.names(Estimates) == "beta", "Estimate"],
                                      G = Estimates[row.names(Estimates) == "G", "Estimate"],
                                      gamma = Estimates[row.names(Estimates) == "gamma", "Estimate"],
                                      CONC = A * exp(-alpha * Time.offset) +
                                            B * exp(-beta * Time.offset) +
                                            G * exp(-gamma * Time.offset))

                  # Figuring out which beta is which and making it consistent
                  Betas <- Estimates %>%
                        dplyr::mutate(ParamSet = c("Set1", "Set1", "Set2", "Set2",
                                                   "Set3", "Set3"),
                                      ParamType = rep(c("A0", "rate"), 3)) %>%
                        dplyr::select(Estimate, ParamSet, ParamType) %>%
                        tidyr::spread(key = ParamType, value = Estimate) %>%
                        dplyr::arrange(-A0) %>%
                        dplyr::mutate(ParamSet = c("A", "B", "G"))

                  A <- Betas$A0[Betas$ParamSet == "A"]
                  alpha <- Betas$rate[Betas$ParamSet == "A"]
                  B <- Betas$A0[Betas$ParamSet == "B"]
                  beta <- Betas$rate[Betas$ParamSet == "B"]
                  G <- Betas$A0[Betas$ParamSet == "G"]
                  gamma <- Betas$rate[Betas$ParamSet == "G"]

                  Betas <- data.frame(Beta = c("A", "alpha", "B", "beta",
                                               "G", "gamma"),
                                      Estimate = c(A, alpha, B, beta, G, gamma)) %>%
                        dplyr::mutate(Beta = factor(Beta, levels = Beta))

                  Estimates <- Estimates %>% dplyr::left_join(Betas, by = "Estimate") %>%
                        dplyr::arrange(Beta) %>%
                        dplyr::select(Beta, everything())
            }

            CurveData <- CurveData %>%
                  dplyr::mutate(TIME = Time.offset + tmax)

            Graph <- ggplot2::ggplot(DFinit2, ggplot2::aes(x = TIME, y = CONC)) +
                  ggplot2::geom_point() +
                  ggplot2::geom_line(data = CurveData) +
                  ggplot2::scale_y_log10() +
                  ggplot2::xlab(rlang::as_label(time)) + ggplot2::ylab(rlang::as_label(concentration))

            if(any(complete.cases(omit)) & any(omit %in% 1:nrow(DFinit))){
                  Graph <- Graph +
                        ggplot2::geom_point(data = DFomit, color = "red", shape = "O", size = 2)

            }
      }

      Result <- list(DataUsed = DFinit,
                     Estimates = Estimates,
                     Graph = Graph)

      # Here's what to do if the fit didn't work at all. I can't remember what
      # caused this to happen, but I know I needed to catch this.
      if(is.null(Fit)){
            Estimates <- data.frame(Beta = NA, Estimate = NA, SE = NA,
                                    tvalue = NA, pvalue = NA)
            names(Estimates) <- c("Beta", "Estimate", "Std. Error", "t value",
                                  "Pr(>|t|)")
            Estimates$Message <- "Cannot fit to model"

            Result <- list(DataUsed = DFinit,
                           Estimates = Estimates,
                           Graph)
      }

      # If the user wants to use better names for the output data.frame, setting
      # those here.
      if(useNLS_outnames == FALSE & class(Fit) == "nls"){
            names(Result[["Estimates"]]) <-
                  c("Beta", "Estimate", "SE", "tvalue", "pvalue")
      }

      # If the user wanted to have an estimate of the residual sum of squares,
      # adding that to the output data.frame.
      if(returnRSS & !is.null(Fit) & class(Fit) == "nls"){
            Result[["Estimates"]]$RSS <- as.numeric(Fit$m$deviance())
      }

      # Adjusting the final output to only contain the results requested.
      if(returnDataUsed){
            if(graph){
                  Result <- Result[c("Estimates", "DataUsed", "Graph")]
            } else{
                  Result <- Result[c("Estimates", "DataUsed")]
            }
      } else{
            if(graph){
                  Result <- Result[c("Estimates", "Graph")]
            } else{
                  Result <- Result[["Estimates"]]
            }
      }

      return(Result)

}


