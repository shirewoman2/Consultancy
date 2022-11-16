#' Fit induction data to calculate IndC50, Indmax, and/or slope
#'
#' \code{inductFit} fits induction data -- either activity or mRNA expression --
#' to one or all of four models for calculating Ind_max, Ind50, and, when
#' appropriate, a slope. Like Howie's R script for fitting induction data
#' ("Induction_fit_script_ver2.r"), default weighting is by 1/y^2, although you
#' can change that with the "weights" argument. IMPORTANT: One way in which this
#' function differs from Howie's script is that no upper or lower bounds for
#' parameter estimates are in use. This means that, if sufficient data to
#' describe the curve do not exist, the function will fail to deliver any fitted
#' parameters, which is meant to be a benefit but could be an annoyance
#' depending on your perspective. Because this does not include those
#' boundaries, you may get different results between this function and Howie's
#' script.
#'
#' @param DF the data.frame containing induction data with a column for the drug
#'   concentration, a column for the fold induction observed, and, if you want
#'   to fit the data by individual, a column for the donor ID. For an example,
#'   please see \code{data(IndData)}. This should not be in quotes.
#' @param conc_column the name of the column within DF that contains
#'   concentration data. This should be unquoted.
#' @param fold_change_column the name of the column within DF that contains
#'   fold-change data, e.g., mRNA measurements or activity. This should be
#'   unquoted.
#' @param donor_column the name of the column within DF that contains the donor
#'   IDs, unquoted
#' @param model which model(s) you would like to use. The four model options
#'   are: \describe{
#'
#'   \item{"Indmax"}{the Indmax model. This assumes a hyperbolic shape to the
#'   interaction when inducer concentration is plotted on the x axis and fold
#'   induction is plotted on the y. \deqn{Ind = (Indmax * I)/(IndC50 + I)} where
#'   Ind = the y axis observed fold induction, I is the inducer concentration,
#'   and Indmax (the maximum induction), and IndC50 (the inducer concentration
#'   at 1/2 Indmax) are the fitted parameters.}
#'
#'   \item{"IndmaxSlope"}{the Indmax model with the addition of a slope n
#'   \deqn{fold induction = 1 + Indmax * I^n / (IndC50^n + I^n)}}
#'
#'   \item{"slope"}{slope only: \deqn{fold induction = I * n}}
#'
#'   \item{"Sig3Param"}{Sigmoidal 3-parameter model (often used by Xenotech):
#'   \deqn{fold induction = Indmax / (1 + exp( -(I - IndC50)/n ))}}
#'
#'   \item{"all"}{All 4 models will be fitted to the data.} }
#'
#' @param measurement the type of measurement used. Options are "mRNA" or
#'   "activity". This only affects the y axis labels on the output graph(s).
#' @param fitByDonor TRUE (default) or FALSE for whether to fit the data by
#'   individual donor
#' @param weights weighting scheme to use for the regression. User may supply a
#'   numeric vector of weights to use or choose from "none", "1/x", "1/x^2",
#'   "1/y" or "1/y^2" (default). Be careful that you don't have any infinite
#'   values or this will fail!
#' @param color_set the set of colors to use. Options: \describe{
#'
#'   \item{"default"}{a set of colors from Cynthia Brewer et al. from Penn State
#'   that are friendly to those with red-green colorblindness. The first three
#'   colors are green, orange, and purple. This can also be referred to as
#'   "Brewer set 2".}
#'
#'   \item{"Brewer set 1"}{colors selected from the Brewer palette "set 1". The
#'   first three colors are red, blue, and green.}
#'
#'   \item{"ggplot2 default"}{the default set of colors used in ggplot2 graphs
#'   (ggplot2 is an R package for graphing.)}
#'
#'   \item{"rainbow"}{colors selected from a rainbow palette. The default
#'   palette is limited to something like 6 colors, so if you have more than
#'   that, that's when this palette is most useful. It's \emph{not} very useful
#'   when you only need a couple of colors.}
#'
#'   \item{"blue-green"}{a set of blues fading into greens. This palette can be
#'   especially useful if you are comparing a systematic change in some
#'   continuous variable -- for example, increasing dose or predicting how a
#'   change in intrinsic solubility will affect concentration-time profiles --
#'   because the direction of the trend will be clear.}
#'
#'   \item{"blues"}{a set of blues fading light blue to dark blue. Like
#'   "blue-green", this palette can be especially useful if you are comparing a
#'   systematic change in some continuous variable.}
#'
#'   \item{"Tableau"}{uses the standard Tableau palette; requires the "ggthemes"
#'   package}
#'
#'   \item{"viridis"}{from the eponymous package by Simon Garnier and ranges
#'   colors from purple to blue to green to yellow in a manner that is
#'   "printer-friendly, perceptually uniform and easy to read by those with
#'   colorblindness", according to the package author}}
#'
#' @param graph_title optionally specify a title that will be centered across
#'   your graph or set of graphs
#' @param graph_title_size the font size for the graph title if it's included;
#'   default is 14. This also determines the font size of the graph labels. 
#' @param y_axis_limits optionally set the Y axis limits, e.g., \code{c(1, 5)}.
#'   If left as NA, the Y axis limits will be automatically selected. (Reminder:
#'   Numeric data should not be in quotes.)
#' @param hline_foldinduct1 TRUE or FALSE (default) on whether to include a
#'   dotted red line where the fold induction = 1.
#' @param num_sigfig optionally specify the number of significant figures you
#'   would like any output rounded to. If left as NA, no rounding will be
#'   performed.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My induction graph.png". If you leave off ".png", it
#'   will be saved as a png file, but if you specify a different file extension,
#'   it will be saved as that file format. Acceptable extensions are "eps",
#'   "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg". Leaving this as NA
#'   means the file will not be automatically saved to disk.
#' @param fig_height figure height in inches; default is 5
#' @param fig_width figure width in inches; default is 5.5
#' @param save_output optionally save the output by supplying a file name in
#'   quotes here, e.g., "My fitted induction parameters.csv". Saving as a csv
#'   file will save only the fitted parameters. However, if you would like a
#'   Word file with a) the equations used, b) the fitted parameters, and c) the
#'   graphs, end this file name with ".docx" instead. 
#'
#'
#' @return Returns a list of \describe{ \item{Fit}{the fitted parameters}
#'   \item{Fit_means}{the mean fitted parameters for all donors} \item{Graph}{a
#'   graph or set of graphs of the data with the fitted parameters depicted as
#'   lines} \item{Curve}{a data.frame of numeric data used to graph the fitted
#'   parameters in case you'd like to plot the fitted parameters in some other
#'   way}}
#' @import tidyverse
#' @import rlang
#' @export
#' @examples
#' data(IndData)
#'
#' inductFit(IndData, model = "Indmax")$Graph
#' inductFit(IndData %>% rename(Conc_uM = Concentration_uM),
#'           conc_column = Conc_uM, model = "IndmaxSlope")$Graph
#' inductFit(IndData, model = "slope")$Graph
#' inductFit(IndData, model = "Sig3Param")
#'
#' inductFit(IndData, model = "IndmaxSlope", measurement = "activity")
#'
#' MyFit <- inductFit(IndData, model = "all")
#' MyFit$Graph; MyFit$Fit
#'
#' MyFit <- inductFit(IndData, model = "Sig3Param", fitByDonor = TRUE, donor_column = DonorID)
#' MyFit$Fit; MyFit$Fit_means; MyFit$Graph
#'
#' MyFit <- inductFit(IndData, model = "all", fitByDonor = TRUE, donor_column = DonorID)
#' MyFit$Fit; MyFit$Fit_means; MyFit$Graph
#'
#'
#' 


inductFit <- function(DF,
                      conc_column = Concentration_uM,
                      fold_change_column = FoldInduction,
                      model = "IndmaxSlope",
                      measurement = "mRNA",
                      donor_column = DonorID,
                      fitByDonor = TRUE, 
                      weights = "1/y^2", 
                      color_set = "default",
                      y_axis_limits = NA,
                      hline_foldinduct1 = FALSE,
                      num_sigfig = NA, 
                      graph_title = NA,
                      graph_title_size = 14, 
                      save_graph = NA,
                      fig_height = 5,
                      fig_width = 5.5, 
                      save_output = NA){
    
    # Error catching ----------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    # Setting things up for nonstandard evaluation -------------------------
    conc_column <- rlang::enquo(conc_column)
    fold_change_column <- rlang::enquo(fold_change_column)
    donor_column <- rlang::enquo(donor_column)
    
    if(rlang::as_label(conc_column) %in% names(DF) == FALSE){
        stop("The column you have listed for the concentration data is not present in your data.frame. Please enter a valid column for concentration data.",
             call. = FALSE)
    }
    
    if(rlang::as_label(fold_change_column) %in% names(DF) == FALSE){
        stop("The column you have listed for the fold-change data is not present in your data.frame. Please enter a valid column for fold-change data.",
             call. = FALSE)
    }
    
    if(rlang::as_label(donor_column) %in% names(DF) == FALSE & fitByDonor == TRUE){
        stop("The column you have listed for the donor is not present in your data.frame. Please enter a valid column for the donor.",
             call. = FALSE)
    }
    
    # Need a donor column for joining purposes later. Adding a placeholder
    # here.
    if(rlang::as_label(donor_column) %in% names(DF) == FALSE){
        DF <- DF %>% dplyr::select(any_of(c(rlang::as_label(conc_column),
                                            rlang::as_label(fold_change_column)))) %>%
            dplyr::rename(FoldInduction = !! fold_change_column,
                          Concentration_uM = !! conc_column)
        DF$DonorID <- "A"
    } else {
        DF <- DF %>% dplyr::select(any_of(c(rlang::as_label(conc_column),
                                            rlang::as_label(fold_change_column),
                                            rlang::as_label(donor_column)))) %>%
            dplyr::rename(FoldInduction = !! fold_change_column,
                          Concentration_uM = !! conc_column,
                          DonorID = !! donor_column)
    }
    
    LegendTitle <- rlang::as_label(donor_column)
    
    # initial error catching ------------------------------------------------
    # Options for model: IndmaxSlope, Indmax, slope, Sig3Param, all
    if(tolower(model[1]) %in% tolower(c("Indmax", "IndmaxSlope", "slope", "Sig3Param", "all")) == FALSE){
        stop("Model options are 'Indmax', 'IndmaxSlope', 'slope', 'Sig3Param' or 'all'. Please enter a valid model.",
             call. = FALSE)
    }
    
    model <- switch(tolower(model), 
                    "indmax" = "Indmax",
                    "indmaxslope" = "IndmaxSlope", 
                    "slope" = "slope", 
                    "sig3param" = "Sig3Param", 
                    "all" = "all")
    
    if(length(model) > 1){
        stop("Please select only one option for the model. Model options are 'Indmax', 'IndmaxSlope', 'slope', 'Sig3Param' or 'all'.",
             call. = FALSE)
    }
    
    # Options for measurement
    if(measurement[1] %in% c("mRNA", "activity") == FALSE){
        stop("Measurement options are 'mRNA' or 'activity'.",
             call. = FALSE)
    }
    
    if(length(measurement) > 1){
        stop("Please select only one option for the measurement. Options are 'mRNA' or 'activity'.",
             call. = FALSE)
    }
    
    # General data setup ---------------------------------------------------
    # Need to add a column for the model chosen for graphing purposes.
    DF$model <- model
    
    # Making prettier facet labels
    ModelFacet <- c(Indmax = "Indmax model",
                    IndmaxSlope = "Indmax slope model",
                    Slope = "slope model",
                    Sig3Param = "sigmoidal 3-parameter model")
    
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
                        slope = list(slope = 1),
                        Sig3Param = list(Indmax = 4,
                                         IndC50 = 5,
                                         slope = 1),
                        all = list(Indmax = 4, IndC50 = 5, slope = 1))
    
    
    # Setting up the weights to use
    if(class(weights) == "character"){
        
        weights <- tolower(weights)
        
        DF <- DF %>%
            dplyr::mutate(Weights = switch(weights, 
                                           "1/x" = 1/Concentration_uM, 
                                           "1/x^2" = 1/Concentration_uM^2,
                                           "1/y" = 1/FoldInduction,
                                           "1/y^2" = 1/FoldInduction^2, 
                                           "none" = 1)) 
        
        
    }
    
    # defining subfunction for fitting to induction models -----------------
    inductFit_prelim <- function(DF, model){
        
        if(model != "all"){
            
            IndFit <- 
                switch(model,
                       Indmax = tryCatch(
                           nls(FoldInduction ~ 1+(Indmax*Concentration_uM)/(IndC50+Concentration_uM),
                               data = DF, start = StartVals, weights = DF$Weights),
                           error = function(x){data.frame(term = c("Indmax", "IndC50"),
                                                          estimate = NA)}),
                       
                       IndmaxSlope = tryCatch(
                           nls(FoldInduction ~ 1+(Indmax*Concentration_uM^slope) /
                                   (IndC50^slope+Concentration_uM^slope),
                               data = DF, start = StartVals, weights = DF$Weights),
                           error = function(x){data.frame(term = c("Indmax", "IndC50", "slope"),
                                                          estimate = NA)}),
                       
                       slope =  tryCatch(
                           nls(FoldInduction ~ 1+(Concentration_uM*slope),
                               data = DF, start = StartVals, weights = DF$Weights),
                           error = function(x){data.frame(term = c("slope"),
                                                          estimate = NA)}),
                       
                       Sig3Param = tryCatch(
                           nls(FoldInduction ~ Indmax/(1+exp(-(Concentration_uM-IndC50)/slope)),
                               data = DF, start = StartVals, weights = DF$Weights) ),
                       error = function(x){data.frame(term = c("Indmax", "IndC50", "slope"),
                                                      estimate = NA)})
            if(class(IndFit) != "data.frame"){
                IndFit <- broom::tidy(IndFit)
            }
            
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
                                          slope = 1 + Curve$Concentration_uM * slope,
                                          Sig3Param = Indmax/(1+exp(-(Curve$Concentration_uM-IndC50)/slope)))
            
            ModelTitle <- switch(model,
                                 Indmax = "Indmax model",
                                 IndmaxSlope = "Indmax slope model",
                                 slope = "slope model",
                                 Sig3Param = "sigmoidal 3-parameter model")
            
        } else {
            
            # Model is "all"
            suppressMessages(
                DF <- DF %>% dplyr::select(-model) %>%
                    dplyr::full_join(
                        expand.grid(DonorID = unique(DF$DonorID),
                                    model = c("Indmax", "IndmaxSlope",
                                              "Slope", "Sig3Param"))) %>%
                    dplyr::mutate(Model_ch = ModelFacet[model])
            )
            
            IndFit <- list(
                Indmax = tryCatch(
                    broom::tidy(
                        nls(FoldInduction ~ 1+(Indmax*Concentration_uM)/(IndC50+Concentration_uM),
                            data = DF, start = StartVals[c("Indmax", "IndC50")],
                            weights = DF$Weights)) %>%
                        dplyr::mutate(model = "Indmax"),
                    error = function(x){data.frame(term = c("Indmax", "IndC50"),
                                                   estimate = NA, 
                                                   model = "IndMax")}),
                
                IndmaxSlope = tryCatch(
                    broom::tidy(
                        nls(FoldInduction ~ 1+(Indmax*Concentration_uM^slope) /
                                (IndC50^slope+Concentration_uM^slope),
                            data = DF, start = StartVals, weights = DF$Weights)) %>%
                        dplyr::mutate(model = "IndmaxSlope"),
                    error = function(x){data.frame(term = c("Indmax", "IndC50", "slope"),
                                                   estimate = NA, 
                                                   model = "IndMaxSlope")}),
                
                slope =  tryCatch(
                    broom::tidy(
                        nls(FoldInduction ~ 1+(Concentration_uM*slope),
                            data = DF, start = StartVals["slope"], weights = DF$Weights)) %>%
                        dplyr::mutate(model = "slope"),
                    error = function(x){data.frame(term = c("slope"),
                                                   estimate = NA, 
                                                   model = "Slope")}),
                
                Sig3Param = tryCatch(
                    broom::tidy(
                        nls(FoldInduction ~ Indmax/(1+exp(-(Concentration_uM-IndC50)/slope)),
                            data = DF, start = StartVals, weights = DF$Weights)) %>%
                        dplyr::mutate(model = "Sig3Param"),
                    error = function(x){data.frame(term = c("Indmax", "IndC50", "slope"),
                                                   estimate = NA, 
                                                   model = "Sig3Param")})  )
            
            # Making data.frame to hold the predicted values for the graph
            Curve <- data.frame(Concentration_uM = seq(min(DF$Concentration_uM, na.rm = T),
                                                       1.2*max(DF$Concentration_uM, na.rm = T),
                                                       length.out = 300))
            
            Curve_Indmax <- Curve %>%
                mutate(Model = "Indmax",
                       FoldInduction = 
                           1 + (IndFit[["Indmax"]]$estimate[IndFit[["Indmax"]]$term == "Indmax"] *
                                    Concentration_uM) /
                           (IndFit[["Indmax"]]$estimate[IndFit[["Indmax"]]$term == "IndC50"] +
                                Concentration_uM))
            
            Curve_IndmaxSlope <- Curve %>% 
                mutate(Model = "IndmaxSlope",
                       FoldInduction = 
                           1 + (IndFit[["IndmaxSlope"]]$estimate[IndFit[["IndmaxSlope"]]$term == "Indmax"] *
                                    Concentration_uM^IndFit[["IndmaxSlope"]]$estimate[IndFit[["IndmaxSlope"]]$term == "slope"]) /
                           (IndFit[["IndmaxSlope"]]$estimate[IndFit[["IndmaxSlope"]]$term == "IndC50"] ^
                                IndFit[["IndmaxSlope"]]$estimate[IndFit[["IndmaxSlope"]]$term == "slope"] +
                                Concentration_uM^IndFit[["IndmaxSlope"]]$estimate[IndFit[["IndmaxSlope"]]$term == "slope"]))
            Curve_slope <- Curve %>% 
                mutate(Model = "Slope", 
                       FoldInduction = 
                           1 + Concentration_uM * IndFit[["slope"]]$estimate[IndFit[["slope"]]$term == "slope"])
            
            Curve_Sig3Param <- Curve %>% 
                mutate(Model = "Sig3Param", 
                       FoldInduction = 
                           IndFit[["Sig3Param"]]$estimate[IndFit[["Sig3Param"]]$term == "Indmax"] /
                           (1+exp(-(Concentration_uM-IndFit[["Sig3Param"]]$estimate[IndFit[["Sig3Param"]]$term == "IndC50"]) /
                                      IndFit[["Sig3Param"]]$estimate[IndFit[["Sig3Param"]]$term == "slope"])))
            
            Curve <- bind_rows(Curve_Indmax, Curve_IndmaxSlope, Curve_slope,
                               Curve_Sig3Param)  %>%
                mutate(Model_ch = ModelFacet[Model])
            ModelTitle <- NULL
        }
        
        if(hline_foldinduct1){
            G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction)) +
                geom_hline(yintercept = 1, color = "red", linetype = "dotted") +
                geom_point() +
                geom_line(data = Curve %>% 
                              filter(complete.cases(FoldInduction)))
        } else {
            G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction)) +
                geom_point() +
                geom_line(data = Curve %>% 
                              filter(complete.cases(FoldInduction)))
        }
        if(model == "all"){
            G <- G + facet_wrap(~ Model_ch, scales = "free")
        }
        
        Out <- list("Fit" = IndFit,
                    "Graph" = G,
                    "Curve" = Curve)
        
        return(Out)
    }
    
    
    # Fitting models to data -----------------------------------------------
    if(fitByDonor){
        
        # fit by donor
        CurveData <- list()
        MyFits <- list()
        
        if(model != "all"){
            
            for(i in unique(DF$DonorID)){
                temp <- DF %>% dplyr::filter(DonorID == i)
                temp_fit <- inductFit_prelim(temp, model = model)
                rm(temp)
                
                CurveData[[i]] <- temp_fit$Curve %>%
                    dplyr::mutate(DonorID = i)
                MyFits[[i]] <- temp_fit$Fit %>% dplyr::mutate(DonorID = i)
                rm(temp_fit)
            }
            
            FitFail <- names(MyFits)[
                sapply(MyFits, function(x) "p.value" %in% names(x)) == FALSE]
            
            if(length(FitFail) > 0){
                warning(paste0("For donor ", str_comma(FitFail),
                               ", the ", model, " model failed to fit the data. No fitted line will be shown on the graph, and no fitted parameters will be returned."),
                        call. = FALSE)
            }
            
            CurveData <- do.call(dplyr::bind_rows, CurveData)
            
            if(hline_foldinduct1){
                G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction,
                                    color = DonorID)) +
                    geom_hline(yintercept = 1, color = "red", linetype = "dotted") +
                    geom_point() +
                    geom_line(data = CurveData %>% 
                                  filter(complete.cases(FoldInduction)))
                
            } else {
                
                G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction,
                                    color = DonorID)) +
                    geom_point() +
                    geom_line(data = CurveData %>% 
                                  filter(complete.cases(FoldInduction)))
            }
            
            
            IndFit <- do.call(dplyr::bind_rows, MyFits)
            suppressMessages(
                IndFit_means <- IndFit %>% dplyr::group_by(term) %>%
                    dplyr::summarize(Geomean = gm_mean(estimate),
                                     Mean = mean(estimate, na.rm = T),
                                     SD = sd(estimate, na.rm = T))
            )
            
            IndFit <- IndFit %>%
                dplyr::select(term, estimate, DonorID) %>%
                tidyr::pivot_wider(values_from = estimate,
                                   names_from = term)
            
        } else {
            
            # Keeping track of failed fits
            FitFail <- list()
            
            # fit all models by donor
            for(i in unique(DF$DonorID)){
                temp <- DF %>% dplyr::filter(DonorID == i)
                temp_fit <- inductFit_prelim(temp, model = model)
                
                FitFail[[i]] <- names(temp_fit$Fit)[
                    sapply(temp_fit$Fit, function(x) "p.value" %in% names(x)) == FALSE]
                
                rm(temp)
                
                CurveData[[i]] <- temp_fit$Curve %>%
                    dplyr::mutate(DonorID = i)
                
                MyFits[[i]] <- 
                    do.call(
                        dplyr::bind_rows, 
                        # only keeping fits that converged
                        temp_fit$Fit[names(temp_fit$Fit) %in% FitFail[[i]] == FALSE]) %>%
                    dplyr::mutate(DonorID = i)
                rm(temp_fit)
            }
            
            suppressMessages(
                DF <- DF %>% dplyr::select(-model) %>%
                    dplyr::full_join(
                        expand.grid(DonorID = unique(DF$DonorID),
                                    model = c("Indmax", "IndmaxSlope",
                                              "Slope", "Sig3Param"))) %>%
                    dplyr::mutate(Model_ch = ModelFacet[model])
            )
            
            CurveData <- do.call(dplyr::bind_rows, CurveData) %>%
                dplyr::mutate(Model_ch = ModelFacet[Model])
            
            if(hline_foldinduct1){
                G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction,
                                    color = DonorID)) +
                    geom_hline(yintercept = 1, color = "red", linetype = "dotted") +
                    geom_point() +
                    labs(color = LegendTitle) +
                    geom_line(data = CurveData %>% 
                                  filter(complete.cases(FoldInduction))) +
                    facet_wrap(~ Model_ch)
                
            } else {
                G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction,
                                    color = DonorID)) +
                    geom_point() +
                    labs(color = LegendTitle) +
                    geom_line(data = CurveData %>% 
                                  filter(complete.cases(FoldInduction))) +
                    facet_wrap(~ Model_ch)
            }
            
            IndFit <- do.call(dplyr::bind_rows, MyFits)
            
            suppressMessages(
                IndFit_means <- IndFit %>% dplyr::group_by(model, term) %>%
                    dplyr::summarize(Geomean = gm_mean(estimate),
                                     Mean = mean(estimate, na.rm = T),
                                     SD = sd(estimate, na.rm = T))
            )
            
            IndFit <- IndFit %>%
                dplyr::select(term, estimate, model, DonorID) %>%
                tidyr::pivot_wider(values_from = estimate,
                                   names_from = term) %>%
                dplyr::arrange(model)
            
            # Checking for failed fits and printing warning message
            if(any(sapply(FitFail, length) > 0)){
                FitFail <- FitFail[sapply(FitFail, FUN = function(x) length(x) > 0)]
                for(i in 1:length(FitFail)){
                    warning(paste0("For donor ", names(FitFail)[i],
                                   ", the model failed to fit the data for the ",
                                   str_comma(FitFail[[i]]),
                                   " model. No fitted line will be shown on the graph, and no fitted parameters will be returned."),
                            call. = FALSE)
                }
            }
        }
        
        Out <- list("Fit" = IndFit,
                    "Fit_means" = IndFit_means,
                    "Graph" = G,
                    "Curve" = CurveData)
        
    } else {
        
        Out <- inductFit_prelim(DF, model)
        
        # Checking for failed fits and printing warning message
        if(length(model) == 1 & model != "all"){
            FitFail <- "p.value" %in% names(Out$Fit) == FALSE
            if(FitFail){
                warning(paste0("The model failed to fit the data for the ", 
                               model, " model. No fitted line will be shown on the graph, and no fitted parameters will be returned."),
                        call. = FALSE)
            }
        } else {
            FitFail <- names(Out$Fit)[sapply(Out$Fit, function(x) "p.value" %in% names(x)) == FALSE]
            if(length(FitFail) > 0){
                warning(paste0("The model failed to fit the data for the ",
                               str_comma(FitFail), " model. No fitted line will be shown on the graph, and no fitted parameters will be returned."),
                        call. = FALSE)
            }
        }
        
        Out$Fit <- bind_rows(Out$Fit) %>%
            dplyr::select(term, estimate, model) %>%
            tidyr::pivot_wider(values_from = estimate,
                               names_from = term)
        
    }
    
    # Making the final graph look nice --------------------------------------
    
    ModelTitle <- switch(model,
                         Indmax = "Indmax model",
                         IndmaxSlope = "Indmax slope model",
                         Slope = "Slope model",
                         Sig3Param = "Sigmoidal 3-parameter model", 
                         all = NULL)
    
    suppressWarnings(
        Out$Graph <- Out$Graph +
            annotation_logticks(sides = "b",
                                short = unit(1.5,"mm"),
                                mid = unit(1.5,"mm"),
                                long = unit(3,"mm")) +
            scale_x_log10() +
            ggtitle(ModelTitle) +
            xlab("Concentration (μM)") +
            ylab(Ylab) +
            theme_consultancy() +
            theme(panel.grid.minor.y = element_line(color = NA),
                  panel.grid.minor.x = element_line(color = NA),
                  panel.grid.major = element_line(colour = NA),
                  panel.border = element_rect(color="black", fill=NA)
            )
    )
    
    # Setting y axis limits ------------------------------------------------
    if(any(complete.cases(y_axis_limits))){
        Ylim <- y_axis_limits[1:2]
    } else {
        Ylim <- range(DF$FoldInduction, na.rm = TRUE)
    }
    
    PossYBreaks <- data.frame(Ymax = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50,
                                       100, 200, 500, 1000, 2000, 5000,
                                       10000, 20000, 50000, 100000,
                                       200000, 500000, Inf),
                              YBreaksToUse = c(0.02, 0.05, 0.1, 0.2, 0.5,
                                               1, 2, 5, 10, 20, 50, 100, 200,
                                               500, 1000, 2000, 5000, 10000,
                                               20000, 50000, 100000, 200000))
    
    YBreaksToUse <- PossYBreaks %>% filter(Ymax >= (Ylim[2] - Ylim[1])) %>%
        slice(which.min(Ymax)) %>% pull(YBreaksToUse)
    
    YInterval    <- YBreaksToUse
    YmaxRnd      <- ifelse(is.na(y_axis_limits[2]), 
                           round_up_unit(Ylim[2], YInterval),
                           y_axis_limits[2])
    YBreaks      <- seq(ifelse(is.na(y_axis_limits[1]), 
                               0, y_axis_limits[1]),
                        YmaxRnd, YInterval)                    # create labels at major points (not minor like in ct_plot b/c too busy for small plots like this, imo)
    YLabels      <- format(YBreaks, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
    
    # If the user specified a y axis interval, make sure there's an axis label
    # for the top of the y axis
    if(any(complete.cases(y_axis_limits))){
        YBreaks <- unique(c(YBreaks, y_axis_limits[2]))
        if(length(YLabels) == length(YBreaks)){
            YLabels[length(YLabels)] <- 
                format(YBreaks[length(YBreaks)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
        } else {
            YLabels <- c(YLabels, 
                         format(YBreaks[length(YBreaks)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE))
        }
    }
    
    suppressWarnings(
        Out$Graph <- Out$Graph + 
            scale_y_continuous(limits = c(ifelse(is.na(y_axis_limits[1]), 
                                                 0, y_axis_limits[1]),
                                          YmaxRnd), 
                               breaks = YBreaks,
                               labels = YLabels)
    )
    
    if(complete.cases(graph_title)){
        Out$Graph <- Out$Graph + ggtitle(graph_title) +
            theme(plot.title = element_text(hjust = 0.5, size = graph_title_size))
    }
    
    # Adding options for colors -----------------------------------------------
    NumColors <- length(unique(DF$DonorID))
    
    if(color_set == "default"){
        # Using "Dark2" b/c "Set2" is just really, really light. 
        Out$Graph <- Out$Graph + 
            scale_color_brewer(palette = "Dark2") +
            scale_fill_brewer(palette="Dark2")
    }
    
    if(color_set == "blue-green"){
        Out$Graph <- Out$Graph + 
            scale_color_manual(values = blueGreens(NumColors)) +
            scale_fill_manual(values = blueGreens(NumColors))
    }
    
    if(color_set == "blues"){
        Out$Graph <- Out$Graph + 
            scale_color_manual(values = blues(NumColors)) +
            scale_fill_manual(values = blues(NumColors))
    }
    
    if(color_set == "rainbow"){
        Out$Graph <- Out$Graph + 
            scale_color_manual(values = rainbow(NumColors)) +
            scale_fill_manual(values = rainbow(NumColors))
    }
    
    if(str_detect(tolower(color_set), "brewer.*2|set.*2")){
        # Using "Dark2" b/c "Set2" is just really, really light. 
        Out$Graph <- Out$Graph + 
            scale_fill_brewer(palette = "Dark2") +
            scale_color_brewer(palette = "Dark2")
    }
    
    if(str_detect(tolower(color_set), "brewer.*1|set.*1")){
        Out$Graph <- Out$Graph + 
            scale_fill_brewer(palette = "Set1") +
            scale_color_brewer(palette = "Set1")
    }
    
    if(color_set == "Tableau"){
        Out$Graph <- Out$Graph + 
            ggthemes::scale_color_tableau() +
            ggthemes::scale_fill_tableau()
    }
    
    if(color_set == "viridis"){
        Out$Graph <- Out$Graph + 
            viridis::scale_color_viridis(discrete = TRUE) +
            viridis::scale_fill_viridis(discrete = TRUE)
    }
    
    # saving and formatting output ---------------------------------------------
    
    if(complete.cases(num_sigfig)){
        Out$Fit <- Out$Fit %>% 
            mutate(across(.cols = any_of(c("Indmax", "IndC50", "slope")), 
                          .fns = signif, digits = num_sigfig))
    }
    
    if(complete.cases(save_graph)){
        FileName <- save_graph
        if(str_detect(FileName, "\\.")){
            # Making sure they've got a good extension
            Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
            FileName <- sub(paste0(".", Ext), "", FileName)
            Ext <- ifelse(Ext %in% c("eps", "ps", "jpeg", "tiff",
                                     "png", "bmp", "svg", "jpg"), 
                          Ext, "png")
            FileName <- paste0(FileName, ".", Ext)
        } else {
            FileName <- paste0(FileName, ".png")
        }
        
        ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
               plot = Out$Graph)
        
    }
    
    if(complete.cases(save_output)){
        if(str_detect(save_output, "docx")){ 
            # This is when they want a Word file as output
            
            # May need to change the working directory temporarily, so
            # determining what it is now
            CurrDir <- getwd()
            
            OutPath <- dirname(save_output)
            if(OutPath == "."){
                OutPath <- getwd()
            }
            
            # Check for whether they're trying to save on SharePoint, which DOES
            # NOT WORK. If they're trying to save to SharePoint, instead, save
            # to their Documents folder.
            
            # Side regex note: The myriad \ in the "sub" call are necessary b/c
            # \ is an escape character, and often the SharePoint and Large File
            # Store directory paths start with \\\\.
            if(str_detect(sub("\\\\\\\\", "//", OutPath), SimcypDir$SharePtDir)){
                
                OutPath <- paste0("C:/Users/", Sys.info()[["user"]], 
                                  "/Documents")
                warning(paste0("You have attempted to use this function to save a Word file to SharePoint, and Microsoft permissions do not allow this. We will attempt to save the ouptut to your Documents folder, which we think should be ", 
                               OutPath,
                               ". Please copy the output to the folder you originally requested or try saving locally or on the Large File Store."), 
                        call. = FALSE)
            }
            
            LFSPath <- str_detect(sub("\\\\\\\\", "//", OutPath), SimcypDir$LgFileDir)
            
            if(LFSPath){
                # Create a temporary directory in the user's AppData/Local/Temp
                # folder.
                TempDir <- tempdir()
                
                # Upon exiting this function, delete that temporary directory.
                on.exit(unlink(TempDir))
                
            }
            
            FileName <- basename(save_output)
            
            rmarkdown::render(system.file("rmarkdown/templates/inductfit/skeleton/skeleton.Rmd",
                                          package="SimcypConsultancy"), 
                              output_dir = switch(as.character(LFSPath), 
                                                  "TRUE" = TempDir,
                                                  "FALSE" = OutPath),
                              output_file = FileName, 
                              quiet = TRUE)
            # Note: The "system.file" part of the call means "go to where the
            # package is installed, search for the file listed, and return its
            # full path.
            
            if(LFSPath){
                file.copy(file.path(TempDir, FileName), OutPath, overwrite = TRUE)
            }
            
        } else {
            # This is when they want a .csv file as output. 
            
            if(str_detect(save_output, "\\.")){
                FileName <- sub("\\..*", ".csv", save_output)
            } else {
                FileName <- paste0(save_output, ".csv")
            }
            write.csv(Out$Fit, FileName, row.names = F)
        }
    }
    
    return(Out)
    
}


