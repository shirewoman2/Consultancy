
#' inductFit
#'
#' This function is an adaptation of Howie's script
#' "Induction_fit_script_ver2.r" and fits induction data -- either activity or
#' mRNA -- to one or all of four models for calculating Ind_max, Ind50, and,
#' when appropriate, a slope. Like the original script, weighting is by 1/y^2.
#' One way in which this differs from the original script is that no upper or
#' lower bounds for parameter estimates are in use. This means that, if
#' sufficient data to describe the curve do not exist, the function will fail to
#' deliver any fitted parameters, which is meant to be a benefit but could be an
#' annoyance depending on your perspective.
#'
#'
#' @param DF the data.frame containing induction data
#' @param concentration the name of the column within DF that contains
#'   concentration data. This should be unquoted.
#' @param fold_induct the name of the column within DF that contains fold-change
#'   data, e.g., mRNA measurements or activity.
#' @param model which model(s) you would like to use. The four model options
#'   are: \describe{
#'
#'   \item{Indmax}{the Indmax model. This assumes a hyperbolic shape to the
#'   interaction when inducer concentration is plotted on the x axis and fold
#'   induction is plotted on the y. \deqn{Ind = (Indmax * I)/(IndC50 + I)} where
#'   Ind = the y axis observed fold induction, I is the inducer concentration,
#'   and Indmax (the maximum induction), and IndC50 (the inducer concentration
#'   at 1/2 Indmax) are the fitted parameters.}
#'
#'   \item{IndmaxSlope}{the Indmax model with the addition of a slope n
#'   \deqn{fold induction = 1 + Indmax * I^n / (IndC50^n + I^n)}}
#'
#'   \item{Slope}{slope only: \deqn{fold induction = I * n}}
#'
#'   \item{Sig3Param}{Sigmoidal 3-parameter model (often used by Xenotech):
#'   \deqn{fold induction = Emax / (1 + exp( -(I - IndC50)/n ))}}
#'
#'   \item{all}{All 4 models will be fitted to the data.} }
#'
#' @param measurement the type of measurement used. Options are "mRNA" or
#'   "activity". This only affects the y axis labels on the output graph(s).
#' @param donor the name of the column within DF that contains the donor IDs
#' @param fitByDonor TRUE or FALSE: Do you want to fit the data by individual
#'   donor (TRUE) or in aggregate (FALSE)?
#'
#'
#' @return Returns a list of \describe{ \item{Fit}{the fitted parameters}
#'   \item{Fit_means}{the mean fitted parameters for all donors} \item{Graph}{a
#'   graph or set of graphs of the data with the fitted parameters depicted as
#'   lines} \item{Curve}{a data.frame of numeric data used to graph the fitted
#'   parameters in case you'd like to plot the fitted parameters in some other
#'   way}}
#' @export
#'
#' @examples
#' data(IndData)
#'
#' inductFit(IndData, model = "Indmax")$Graph
#' inductFit(IndData %>% rename(Conc_uM = Concentration_uM),
#'           concentration = Conc_uM, model = "IndmaxSlope")$Graph
#' inductFit(IndData, model = "Slope")$Graph
#' inductFit(IndData, model = "Sig3Param")
#'
#' inductFit(IndData, model = "IndmaxSlope", measurement = "activity")
#'
#' MyFit <- inductFit(IndData, model = "all")
#' MyFit$Graph; MyFit$Fit
#'
#' MyFit <- inductFit(IndData, model = "Sig3Param", fitByDonor = TRUE, donor = DonorID)
#' MyFit$Fit; MyFit$Fit_means; MyFit$Graph
#'
#' MyFit <- inductFit(IndData, model = "all", fitByDonor = TRUE, donor = DonorID)
#' MyFit$Fit; MyFit$Fit_means; MyFit$Graph
#'
#'
#'


inductFit <- function(DF,
                      concentration = Concentration_uM,
                      fold_induct = FoldInduction,
                      model = "IndmaxSlope",
                      measurement = "mRNA",
                      donor = DONOR,
                      fitByDonor = FALSE){

   # Defining pipe operator and bang bang
   `%>%` <- magrittr::`%>%`
   `!!` <- rlang::`!!`

   concentration <- rlang::enquo(concentration)
   fold_induct <- rlang::enquo(fold_induct)
   donor <- rlang::enquo(donor)

   # Options for model: IndmaxSlope, Indmax, Slope, Sig3Param, all
   if(model[1] %in% c("Indmax", "IndmaxSlope", "Slope", "Sig3Param", "all") == FALSE){
      stop("Model options are 'Indmax', 'IndmaxSlope', 'Slope', 'Sig3Param' or 'all'. Please enter a valid model.")
   }

   if(length(model) > 1){
      stop("Please select only one option for the model. Model options are 'Indmax', 'IndmaxSlope', 'Slope', 'Sig3Param' or 'all'.")
   }

   # Options for measurement
   if(measurement[1] %in% c("mRNA", "activity") == FALSE){
      stop("Measurement options are 'mRNA' or 'activity'.")
   }

   if(length(measurement) > 1){
      stop("Please select only one option for the measurement. Options are 'mRNA' or 'activity'.")
   }

   if(rlang::as_label(concentration) %in% names(DF) == FALSE){
      stop("The column you have listed for the concentration data is not present in your data.frame. Please enter a valid column for concentration data.")
   }

   if(rlang::as_label(fold_induct) %in% names(DF) == FALSE){
      stop("The column you have listed for the fold-change data is not present in your data.frame. Please enter a valid column for fold-change data.")
   }

   if(rlang::as_label(donor) %in% names(DF) == FALSE & fitByDonor == TRUE){
      stop("The column you have listed for the donor is not present in your data.frame. Please enter a valid column for the donor.")
   }

   # Need a donor column for joining purposes later. Adding a placeholder
   # here.
   if(rlang::as_label(donor) %in% names(DF) == FALSE){
      DF <- DF %>% dplyr::select(any_of(c(rlang::as_label(concentration),
                                          rlang::as_label(fold_induct)))) %>%
         dplyr::rename(FoldInduction = !! fold_induct,
                       Concentration_uM = !! concentration)
      DF$DONOR <- "A"
   } else {
      DF <- DF %>% dplyr::select(any_of(c(rlang::as_label(concentration),
                                          rlang::as_label(fold_induct),
                                          rlang::as_label(donor)))) %>%
         dplyr::rename(FoldInduction = !! fold_induct,
                       Concentration_uM = !! concentration,
                       DONOR = !! donor)
   }


   # Need to add a column for the model chosen for graphing purposes.
   DF$model <- model

   # Making prettier facet labels
   ModelFacet <- c(Indmax = "Indmax model",
                   IndmaxSlope = "Indmax slope model",
                   Slope = "Slope model",
                   Sig3Param = "Sigmoidal 3-parameter model (Xenotech)")

   # Setting better colors for graphs than the weird default
   scale_colour_discrete <- function(...) scale_colour_brewer(..., palette="Set1")
   scale_fill_discrete <- function(...) scale_fill_brewer(... , palette="Set1")

   # Changing the y axis label to fit activity or mRNA
   Ylab <- switch(measurement,
                  activity = "Fold change\n(activity with drug / activity with vehicle control)",
                  mRNA = "Fold change\n(mRNA with drug / mRNA with vehicle control)")

   # starting parameters for each model
   StartVals <- switch(model,
                       IndmaxSlope = list(Indmax = 4, # max(DF$Concentration_uM, na.rm = T), <-- not sure why this doesn't work, but it doesn't. Fit won't converge.
                                          IndC50 = 5, # max(DF$Concentration_uM, na.rm = T)*2/3,
                                          slope = 1),
                       Indmax = list(Indmax = 4,
                                     IndC50 = 5),
                       Slope = list(slope = 1),
                       Sig3Param = list(Indmax = 4,
                                        IndC50 = 5,
                                        slope = 1),
                       all = list(Indmax = 4, IndC50 = 5, slope = 1))

   inductFit_prelim <- function(DF, model){
      if(model != "all"){

         Weights <- 1/(DF$FoldInduction^2)

         IndFit <- broom::tidy(
            switch(model,
                   Indmax = nls(FoldInduction ~ 1+(Indmax*Concentration_uM)/(IndC50+Concentration_uM),
                                data = DF, start = StartVals, weights = Weights),
                   IndmaxSlope = nls(FoldInduction ~ 1+(Indmax*Concentration_uM^slope) /
                                        (IndC50^slope+Concentration_uM^slope),
                                     data = DF, start = StartVals, weights = Weights),
                   Slope =  nls(FoldInduction ~ 1+(Concentration_uM*slope),
                                data = DF, start = StartVals, weights = Weights),
                   Sig3Param = nls(FoldInduction ~ Indmax/(1+exp(-(Concentration_uM-IndC50)/slope)),
                                   data = DF, start = StartVals, weights = Weights) ))
         IndFit$model <- model

         Indmax <- IndFit$estimate[IndFit$term == "Indmax"]
         IndC50 <- IndFit$estimate[IndFit$term == "IndC50"]
         slope <- IndFit$estimate[IndFit$term == "slope"]

         # Making data.frame to hold the predicted values for the graph
         Curve <- data.frame(Concentration_uM = seq(min(DF$Concentration_uM, na.rm = T),
                                        1.2*max(DF$Concentration_uM, na.rm = T),
                                        length.out = 300),
                             model = model)

         Curve$FoldInduction <- switch(model,
                                     Indmax = 1+(Indmax*Curve$Concentration_uM)/(IndC50+Curve$Concentration_uM),
                                     IndmaxSlope = 1+(Indmax*Curve$Concentration_uM^slope) /
                                        (IndC50^slope+Curve$Concentration_uM^slope),
                                     Slope = 1 + Curve$Concentration_uM * slope,
                                     Sig3Param = Indmax/(1+exp(-(Curve$Concentration_uM-IndC50)/slope)))

         ModelTitle <- switch(model,
                              Indmax = "Indmax model",
                              IndmaxSlope = "Indmax slope model",
                              Slope = "Slope model",
                              Sig3Param = "Sigmoidal 3-parameter model (Xenotech)")

      } else {

         # Model is "all"
         suppressMessages(
            DF <- DF %>% dplyr::select(-model) %>%
               dplyr::full_join(
                  expand.grid(DONOR = unique(DF$DONOR),
                              model = c("Indmax", "IndmaxSlope",
                                        "Slope", "Sig3Param"))) %>%
               dplyr::mutate(Model_ch = ModelFacet[model])
         )

         IndFit <- list(
            Indmax = broom::tidy(
               nls(FoldInduction ~ 1+(Indmax*Concentration_uM)/(IndC50+Concentration_uM),
                   data = DF, start = StartVals[c("Indmax", "IndC50")], weights = Weights)) %>%
               dplyr::mutate(model = "Indmax"),
            IndmaxSlope = broom::tidy(
               nls(FoldInduction ~ 1+(Indmax*Concentration_uM^slope) /
                      (IndC50^slope+Concentration_uM^slope),
                   data = DF, start = StartVals, weights = Weights)) %>%
               dplyr::mutate(model = "IndmaxSlope"),
            Slope =  broom::tidy(
               nls(FoldInduction ~ 1+(Concentration_uM*slope),
                   data = DF, start = StartVals["slope"], weights = Weights)) %>%
               dplyr::mutate(model = "Slope"),
            Sig3Param = broom::tidy(
               nls(FoldInduction ~ Indmax/(1+exp(-(Concentration_uM-IndC50)/slope)),
                   data = DF, start = StartVals, weights = Weights)) %>%
               dplyr::mutate(model = "Sig3Param")  )

         # Making data.frame to hold the predicted values for the graph
         Curve <- data.frame(Concentration_uM = rep(seq(min(DF$Concentration_uM, na.rm = T),
                                            1.2*max(DF$Concentration_uM, na.rm = T),
                                            length.out = 300), 4),
                             model = rep(c("Indmax", "IndmaxSlope", "Slope", "Sig3Param"),
                                         each = 300))  %>%
            dplyr::mutate(Model_ch = ModelFacet[model])

         Curve <- Curve %>%
            dplyr::mutate(
               # Indmax model
               FoldInduction = 1 + (IndFit[["Indmax"]]$estimate[IndFit[["Indmax"]]$term == "Indmax"] *
                                     Concentration_uM) /
                  (IndFit[["Indmax"]]$estimate[IndFit[["Indmax"]]$term == "IndC50"] +
                      Concentration_uM),
               # IndmaxSlope model
               FoldInduction = ifelse(model == "IndmaxSlope",
                                    1+(IndFit[["IndmaxSlope"]]$estimate[IndFit[["IndmaxSlope"]]$term == "Indmax"] *
                                          Concentration_uM^IndFit[["IndmaxSlope"]]$estimate[IndFit[["IndmaxSlope"]]$term == "slope"]) /
                                       (IndFit[["IndmaxSlope"]]$estimate[IndFit[["IndmaxSlope"]]$term == "IndC50"] ^
                                           IndFit[["IndmaxSlope"]]$estimate[IndFit[["IndmaxSlope"]]$term == "slope"] +
                                           Concentration_uM^IndFit[["IndmaxSlope"]]$estimate[IndFit[["IndmaxSlope"]]$term == "slope"]),
                                    FoldInduction),
               # Slope model
               FoldInduction = ifelse(model == "Slope",
                                    1 + Concentration_uM * IndFit[["Slope"]]$estimate[IndFit[["Slope"]]$term == "slope"],
                                    FoldInduction),
               # Sig3Param model
               FoldInduction = ifelse(model == "Sig3Param",
                                    IndFit[["Sig3Param"]]$estimate[IndFit[["Sig3Param"]]$term == "Indmax"] /
                                       (1+exp(-(Concentration_uM-IndFit[["Sig3Param"]]$estimate[IndFit[["Sig3Param"]]$term == "IndC50"]) /
                                                 IndFit[["Sig3Param"]]$estimate[IndFit[["Sig3Param"]]$term == "slope"])),
                                    FoldInduction))
         ModelTitle <- NULL
      }

      G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction)) +
         geom_point() +
         geom_line(data = Curve) +
         annotation_logticks(sides = "b",
                             short = unit(1.5,"mm"),
                             mid = unit(1.5,"mm"),
                             long = unit(3,"mm")) +
         scale_x_log10() +
         ggtitle(ModelTitle) +
         xlab(expression(Concentration~(mu*M))) +
         ylab(Ylab) +
         theme(panel.background = element_rect(fill="white", color=NA),
               panel.grid.minor.y = element_line(color = NA),
               panel.grid.minor.x = element_line(color = NA),
               panel.grid.major = element_line(colour = NA),
               plot.background = element_rect(fill="white", colour=NA),
               panel.border = element_rect(color="black", fill=NA),
               strip.background = element_rect(color=NA, fill="white"),
               legend.background = element_rect(color=NA, fill=NA),
               legend.key = element_rect(color=NA, fill=NA)
         )

      if(model == "all"){
         G <- G + facet_wrap(~ Model_ch)
      }

      Out <- list("Fit" = IndFit,
                  "Graph" = G,
                  "Curve" = Curve)

      return(Out)
   }

   if(fitByDonor == FALSE){
      Out <- inductFit_prelim(DF, model)
   } else {

      # fit by donor

      # defining geometric mean function for reporting means
      gm_mean <- function(x) exp(mean(log(x)))

      CurveData <- list()
      MyFits <- list()

      if(model != "all"){

         for(i in unique(DF$DONOR)){
            temp <- DF %>% dplyr::filter(DONOR == i)
            temp_fit <- inductFit_prelim(temp, model = model)
            rm(temp)

            CurveData[[i]] <- temp_fit$Curve %>%
               dplyr::mutate(DONOR = i)
            MyFits[[i]] <- temp_fit$Fit %>% dplyr::mutate(DONOR = i)
            rm(temp_fit)
         }

         CurveData <- do.call(dplyr::bind_rows, CurveData)

         ModelTitle <- switch(model,
                              Indmax = "Indmax model",
                              IndmaxSlope = "Indmax slope model",
                              Slope = "Slope model",
                              Sig3Param = "Sigmoidal 3-parameter model (Xenotech)")

         G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction,
                             color = DONOR)) +
            geom_point() +
            geom_line(data = CurveData) +
            annotation_logticks(sides = "b",
                                short = unit(1.5,"mm"),
                                mid = unit(1.5,"mm"),
                                long = unit(3,"mm")) +
            scale_x_log10() +
            ggtitle(ModelTitle) +
            xlab(expression(Concentration~(mu*M))) +
            ylab(Ylab) +
            theme(panel.background = element_rect(fill="white", color=NA),
                  panel.grid.minor.y = element_line(color = NA),
                  panel.grid.minor.x = element_line(color = NA),
                  panel.grid.major = element_line(colour = NA),
                  plot.background = element_rect(fill="white", colour=NA),
                  panel.border = element_rect(color="black", fill=NA),
                  strip.background = element_rect(color=NA, fill="white"),
                  legend.background = element_rect(color=NA, fill=NA),
                  legend.key = element_rect(color=NA, fill=NA)
            )

         IndFit <- do.call(dplyr::bind_rows, MyFits)
         suppressMessages(
            IndFit_means <- IndFit %>% dplyr::group_by(term) %>%
               dplyr::summarize(GMean = gm_mean(estimate),
                                Mean = mean(estimate),
                                SD = sd(estimate))
         )

         IndFit <- IndFit %>%
            dplyr::select(term, estimate, DONOR) %>%
            tidyr::pivot_wider(values_from = estimate,
                               names_from = term)

      } else {

         # fit all models by donor
         for(i in unique(DF$DONOR)){
            temp <- DF %>% dplyr::filter(DONOR == i)
            temp_fit <- inductFit_prelim(temp, model = model)
            rm(temp)

            CurveData[[i]] <- temp_fit$Curve %>%
               dplyr::mutate(DONOR = i)
            MyFits[[i]] <- do.call(dplyr::bind_rows, temp_fit$Fit) %>%
               dplyr::mutate(DONOR = i)
            rm(temp_fit)
         }

         suppressMessages(
            DF <- DF %>% dplyr::select(-model) %>%
               dplyr::full_join(
                  expand.grid(DONOR = unique(DF$DONOR),
                              model = c("Indmax", "IndmaxSlope",
                                        "Slope", "Sig3Param"))) %>%
               dplyr::mutate(Model_ch = ModelFacet[model])
         )

         CurveData <- do.call(dplyr::bind_rows, CurveData) %>%
            dplyr::mutate(Model_ch = ModelFacet[model])

         G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction,
                             color = DONOR)) +
            geom_point() +
            geom_line(data = CurveData) +
            annotation_logticks(sides = "b",
                                short = unit(1.5,"mm"),
                                mid = unit(1.5,"mm"),
                                long = unit(3,"mm")) +
            scale_x_log10() +
            xlab(expression(Concentration~(mu*M))) +
            ylab(Ylab) +
            facet_wrap(~ Model_ch) +
            theme(
               panel.background = element_rect(fill="white", color=NA),
               panel.grid.minor.y = element_line(color = NA),
               panel.grid.minor.x = element_line(color = NA),
               panel.grid.major = element_line(colour = NA),
               plot.background = element_rect(fill="white", colour=NA),
               panel.border = element_rect(color="black", fill=NA),
               strip.background = element_rect(color=NA, fill="white"),
               legend.background = element_rect(color=NA, fill=NA),
               legend.key = element_rect(color=NA, fill=NA)
            )

         IndFit <- do.call(dplyr::bind_rows, MyFits)

         suppressMessages(
            IndFit_means <- IndFit %>% dplyr::group_by(model, term) %>%
               dplyr::summarize(GMean = gm_mean(estimate),
                                Mean = mean(estimate),
                                SD = sd(estimate))
         )

         IndFit <- IndFit %>%
            dplyr::select(term, estimate, model, DONOR) %>%
            tidyr::pivot_wider(values_from = estimate,
                               names_from = term) %>%
            dplyr::arrange(model)
      }

      Out <- list("Fit" = IndFit,
                  "Fit_means" = IndFit_means,
                  "Graph" = G,
                  "Curve" = CurveData)

   }

   return(Out)

}


