#' Fit induction data to calculate EC50, Emax, and/or slope
#'
#' \code{inductFit} fits induction data -- either activity or mRNA expression --
#' to one or all of four models for calculating Indmax or Emax, EC50, and, when
#' appropriate, a slope. Like Howie's R script for fitting induction data
#' ("Induction_fit_script_ver2.r"), by default, data are weighted by 1/y^2, but
#' you can change that with the "weights" argument. \strong{Two important
#' notes:} \itemize{\item{With the exception of the sigmoidal 3-parameter model,
#' the fitted parameter describing maximal induction is Emax and \emph{not}
#' Indmax, the parameter used in the Simcyp Simulator. Note that \strong{Indmax
#' = Emax + 1}.} \item{One way in which this function differs from Howie's
#' script is that no upper or lower bounds for parameter estimates are in use.
#' This means that, if sufficient data to describe the curve do not exist, the
#' function will fail to deliver any fitted parameters, which is meant to be a
#' benefit but could be an annoyance depending on your perspective. Because this
#' does not include those boundaries, you may get different results between this
#' function and Howie's script.}}
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
#'   \item{"Emax"}{the Emax model. This assumes a hyperbolic shape to the
#'   interaction when inducer concentration is plotted on the x axis and fold
#'   induction is plotted on the y. \deqn{fold induction = 1 + (Emax * I)/(EC50
#'   + I)} where I is the inducer concentration, and Emax (the maximum
#'   induction), and EC50 (the inducer concentration at 1/2 Emax) are the fitted
#'   parameters.}
#'
#'   \item{"EmaxSlope"}{the Emax model with the addition of a slope n \deqn{fold
#'   induction = 1 + Emax * I^n / (EC50^n + I^n)}}
#'
#'   \item{"slope"}{slope only: \deqn{fold induction = 1 + I * n}}
#'
#'   \item{"Sig3Param"}{Sigmoidal 3-parameter model (often used by Xenotech):
#'   \deqn{fold induction = Indmax / (1 + exp( -(I - IndC50)/n ))} SPECIAL NOTE:
#'   The sigmoidal 3-parameter model is the \emph{only} model that determines
#'   Indmax instead of Emax!}
#'
#'   \item{"all" (default)}{All 4 models will be fitted to the data.} }
#'
#' @param measurement the type of measurement used. Options are "mRNA" or
#'   "activity". This only affects the y axis labels on the output graph(s).
#' @param enzyme the enzyme involved. Default is "CYP3A4" but can be set to NA
#'   to leave as unspecified. This only affects the figure and table headings
#'   and captions in the Word document output.
#' @param drug the drug used in the incubations. Default is NA to leave this
#'   unspecified, but supplying a value will include the drug name in the x-axis
#'   label on the graphs as well as the figure and table headings and captions
#'   in the Word document output.
#' @param fitByDonor TRUE (default) or FALSE for whether to fit the data by
#'   individual donor
#' @param weights weighting scheme to use for the regression. User may supply a
#'   numeric vector of weights to use or choose from "none", "1/x", "1/x^2",
#'   "1/y" or "1/y^2" (default). Be careful that you don't have any infinite
#'   values or this will fail!
#' @param omit An index of which, if any, samples to omit from regression(s).
#'   These samples will be depicted as open circles in the graph but will not be
#'   included in the regression. An example of acceptable input where, say, you
#'   want to omit samples where the concentration was greater than 100 uM but
#'   still want to see those points on the graph(s): \code{omit =
#'   which(DF$Concentration > 100)}
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
#'   graphs, end this file name with ".docx" instead. \strong{WARNING:} SAVING
#'   TO WORD DOES NOT WORK ON SHAREPOINT. This is a Microsoft permissions issue,
#'   not an R issue. If you try to save on SharePoint, you will get a warning
#'   that R will save your file instead to your local (not OneDrive) Documents
#'   folder.
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
#' inductFit(IndData, model = "Emax")$Graph
#' inductFit(IndData %>% rename(Conc_uM = Concentration_uM),
#'           conc_column = Conc_uM, model = "EmaxSlope")$Graph
#' inductFit(IndData, model = "slope")$Graph
#' inductFit(IndData, model = "Sig3Param")
#'
#' inductFit(IndData, model = "EmaxSlope", measurement = "activity")
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
                      model = "EmaxSlope",
                      measurement = "mRNA",
                      enzyme = "CYP3A4", 
                      drug = NA,
                      donor_column = DonorID,
                      fitByDonor = TRUE, 
                      weights = "1/y^2", 
                      omit = NA,
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
    
    # Making the model be case insensitive
    model <- switch(tolower(model), 
                    "emax" = "Emax",
                    "emaxslope" = "EmaxSlope", 
                    "slope" = "slope", 
                    "sig3param" = "Sig3Param", 
                    "all" = "all")
    
    # Options for model: EmaxSlope, Emax, slope, Sig3Param, all
    if(tolower(model[1]) %in% tolower(c("Emax", "EmaxSlope", "slope", "Sig3Param", "all")) == FALSE){
        stop("Model options are 'Emax', 'EmaxSlope', 'slope', 'Sig3Param' or 'all'. Please enter a valid model.",
             call. = FALSE)
    }
    
    if(length(model) > 1){
        stop("Please select only one option for the model. Model options are 'Emax', 'EmaxSlope', 'slope', 'Sig3Param' or 'all'.",
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
    
    
    # General data setup ---------------------------------------------------
    # Need to add a column for the model chosen for graphing purposes.
    DF$Model <- model
    DF$Model_ch <- model # This may change depending on model selected but needs to be set to something. 
    
    # Noting which points to omit
    DF$Omit <- FALSE
    DF$Omit[omit] <- TRUE
    
    # Making prettier facet labels
    ModelFacet <- c(Emax = "Emax model",
                    EmaxSlope = "Emax slope model",
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
                        EmaxSlope = list(Emax = 4, # max(DF$Concentration_uM, na.rm = T), <-- not sure why this doesn't work, but it doesn't. Fit won't converge.
                                         EC50 = 5, # max(DF$Concentration_uM, na.rm = T)*2/3,
                                         slope = 1),
                        Emax = list(Emax = 4,
                                    EC50 = 5),
                        slope = list(slope = 1),
                        Sig3Param = list(Emax = 4,
                                         EC50 = 5,
                                         slope = 1),
                        all = list(Emax = 4, EC50 = 5, slope = 1))
    
    
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
                       Emax = tryCatch(
                           nls(FoldInduction ~ 1+(Emax*Concentration_uM)/(EC50+Concentration_uM),
                               data = DF %>% filter(Omit == FALSE),
                               start = StartVals,
                               weights = DF$Weights[DF$Omit == FALSE]),
                           error = function(x){data.frame(term = c("Emax", "EC50"),
                                                          estimate = NA)}),
                       
                       EmaxSlope = tryCatch(
                           nls(FoldInduction ~ 1+(Emax*Concentration_uM^slope) /
                                   (EC50^slope+Concentration_uM^slope),
                               data = DF %>% filter(Omit == FALSE),
                               start = StartVals, 
                               weights = DF$Weights[DF$Omit == FALSE]),
                           error = function(x){data.frame(term = c("Emax", "EC50", "slope"),
                                                          estimate = NA)}),
                       
                       slope =  tryCatch(
                           nls(FoldInduction ~ 1+(Concentration_uM*slope),
                               data = DF %>% filter(Omit == FALSE),
                               start = StartVals,
                               weights = DF$Weights[DF$Omit == FALSE]),
                           error = function(x){data.frame(term = c("slope"),
                                                          estimate = NA)}),
                       
                       Sig3Param = tryCatch(
                           nls(FoldInduction ~ Emax/(1+exp(-(Concentration_uM-EC50)/slope)),
                               data = DF %>% filter(Omit == FALSE),
                               start = StartVals, 
                               weights = DF$Weights[DF$Omit == FALSE])),
                       error = function(x){data.frame(term = c("Emax", "EC50", "slope"),
                                                      estimate = NA)})
            if(class(IndFit) != "data.frame"){
                IndFit <- broom::tidy(IndFit)
            }
            
            IndFit$Model <- model
            IndFit$Model_ch <- model
            
            Emax <- IndFit$estimate[IndFit$term == "Emax"]
            EC50 <- IndFit$estimate[IndFit$term == "EC50"]
            slope <- IndFit$estimate[IndFit$term == "slope"]
            
            # Making data.frame to hold the predicted values for the graph
            Curve <- data.frame(Concentration_uM = seq(min(DF$Concentration_uM, na.rm = T),
                                                       1.2*max(DF$Concentration_uM, na.rm = T),
                                                       length.out = 300),
                                Model = model, 
                                Model_ch = model)
            
            Curve$FoldInduction <- switch(model,
                                          Emax = 1+(Emax*Curve$Concentration_uM)/(EC50+Curve$Concentration_uM),
                                          EmaxSlope = 1+(Emax*Curve$Concentration_uM^slope) /
                                              (EC50^slope+Curve$Concentration_uM^slope),
                                          slope = 1 + Curve$Concentration_uM * slope,
                                          Sig3Param = Emax/(1+exp(-(Curve$Concentration_uM-EC50)/slope)))
            Curve$Omit <- FALSE
            
        } else {
            
            # Model is "all"
            suppressMessages(
                DF <- DF %>% dplyr::select(-Model) %>%
                    dplyr::full_join(
                        expand.grid(DonorID = unique(DF$DonorID),
                                    Model = c("Emax", "EmaxSlope",
                                              "Slope", "Sig3Param"))) %>%
                    dplyr::mutate(Model_ch = ModelFacet[Model])
            )
            
            IndFit <- list(
                Emax = tryCatch(
                    broom::tidy(
                        nls(FoldInduction ~ 1+(Emax*Concentration_uM)/(EC50+Concentration_uM),
                            data = DF %>% filter(Omit == FALSE),
                            start = StartVals[c("Emax", "EC50")],
                            weights = DF$Weights[DF$Omit == FALSE])) %>%
                        dplyr::mutate(Model = "Emax"),
                    error = function(x){data.frame(term = c("Emax", "EC50"),
                                                   estimate = NA, 
                                                   Model = "Emax")}),
                
                EmaxSlope = tryCatch(
                    broom::tidy(
                        nls(FoldInduction ~ 1+(Emax*Concentration_uM^slope) /
                                (EC50^slope+Concentration_uM^slope),
                            data = DF %>% filter(Omit == FALSE),
                            start = StartVals, 
                            weights = DF$Weights[DF$Omit == FALSE])) %>%
                        dplyr::mutate(Model = "EmaxSlope"),
                    error = function(x){data.frame(term = c("Emax", "EC50", "slope"),
                                                   estimate = NA, 
                                                   Model = "EmaxSlope")}),
                
                slope =  tryCatch(
                    broom::tidy(
                        nls(FoldInduction ~ 1+(Concentration_uM*slope),
                            data = DF %>% filter(Omit == FALSE),
                            start = StartVals["slope"], 
                            weights = DF$Weights[DF$Omit == FALSE])) %>%
                        dplyr::mutate(Model = "slope"),
                    error = function(x){data.frame(term = c("slope"),
                                                   estimate = NA, 
                                                   Model = "Slope")}),
                
                Sig3Param = tryCatch(
                    broom::tidy(
                        nls(FoldInduction ~ Emax/(1+exp(-(Concentration_uM-EC50)/slope)),
                            data = DF %>% filter(Omit == FALSE),
                            start = StartVals, 
                            weights = DF$Weights[DF$Omit == FALSE])) %>%
                        dplyr::mutate(Model = "Sig3Param"),
                    error = function(x){data.frame(term = c("Emax", "EC50", "slope"),
                                                   estimate = NA, 
                                                   Model = "Sig3Param")})  )
            
            # Making data.frame to hold the predicted values for the graph
            Curve <- data.frame(Concentration_uM = seq(min(DF$Concentration_uM, na.rm = T),
                                                       1.2*max(DF$Concentration_uM, na.rm = T),
                                                       length.out = 300))
            
            Curve_Emax <- Curve %>%
                mutate(Model = "Emax",
                       FoldInduction = 
                           1 + (IndFit[["Emax"]]$estimate[IndFit[["Emax"]]$term == "Emax"] *
                                    Concentration_uM) /
                           (IndFit[["Emax"]]$estimate[IndFit[["Emax"]]$term == "EC50"] +
                                Concentration_uM))
            
            Curve_EmaxSlope <- Curve %>% 
                mutate(Model = "EmaxSlope",
                       FoldInduction = 
                           1 + (IndFit[["EmaxSlope"]]$estimate[IndFit[["EmaxSlope"]]$term == "Emax"] *
                                    Concentration_uM^IndFit[["EmaxSlope"]]$estimate[IndFit[["EmaxSlope"]]$term == "slope"]) /
                           (IndFit[["EmaxSlope"]]$estimate[IndFit[["EmaxSlope"]]$term == "EC50"] ^
                                IndFit[["EmaxSlope"]]$estimate[IndFit[["EmaxSlope"]]$term == "slope"] +
                                Concentration_uM^IndFit[["EmaxSlope"]]$estimate[IndFit[["EmaxSlope"]]$term == "slope"]))
            Curve_slope <- Curve %>% 
                mutate(Model = "Slope", 
                       FoldInduction = 
                           1 + Concentration_uM * IndFit[["slope"]]$estimate[IndFit[["slope"]]$term == "slope"])
            
            Curve_Sig3Param <- Curve %>% 
                mutate(Model = "Sig3Param", 
                       FoldInduction = 
                           IndFit[["Sig3Param"]]$estimate[IndFit[["Sig3Param"]]$term == "Emax"] /
                           (1+exp(-(Concentration_uM-IndFit[["Sig3Param"]]$estimate[IndFit[["Sig3Param"]]$term == "EC50"]) /
                                      IndFit[["Sig3Param"]]$estimate[IndFit[["Sig3Param"]]$term == "slope"])))
            
            Curve <- bind_rows(Curve_Emax, Curve_EmaxSlope, Curve_slope,
                               Curve_Sig3Param)  %>%
                mutate(Model_ch = ModelFacet[Model], 
                       Omit = FALSE)
        }
        
        # Setting up facet labels for graphs
        DF$Model_ch <- factor(DF$Model_ch,
                              levels = c("Emax model", "Emax slope model", 
                                         "sigmoidal 3-parameter model", 
                                         "slope model"))
        levels(DF$Model_ch) <- c(
            expression(E[max]~model),
            expression(E[max]~slope~model),
            expression(sigmoidal~"3-parameter"~model),
            expression(slope~model))
        
        Curve$Model_ch <- factor(Curve$Model_ch,
                                 levels = c("Emax model", "Emax slope model", 
                                            "sigmoidal 3-parameter model", 
                                            "slope model"))
        levels(Curve$Model_ch) <- c(
            expression(E[max]~model),
            expression(E[max]~slope~model),
            expression(sigmoidal~"3-parameter"~model),
            expression(slope~model))
        
        G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction, 
                            shape = Omit)) +
            scale_shape_manual(values = c(19, 1)) +
            guides(shape = "none")
        
        if(hline_foldinduct1){
            G <- G +
                geom_hline(yintercept = 1, color = "red", linetype = "dotted")
        } 
        
        G <- G +
            geom_point() +
            geom_line(data = Curve %>% 
                          filter(complete.cases(FoldInduction)))
        
        if(model == "all"){
            G <- G + facet_wrap(~ Model_ch, scales = "free", 
                                labeller = label_parsed)
        }
        
        Out <- list("Fit" = IndFit,
                    "Graph" = G,
                    "Curve" = Curve)
        
        return(Out)
    }
    
    # Fitting models to data -----------------------------------------------
    if(fitByDonor){
        # fitting by donor here
        
        Curve <- list()
        MyFits <- list()
        
        if(model != "all"){
            # Fitting one specific model here
            
            for(i in unique(DF$DonorID)){
                temp <- DF %>% dplyr::filter(DonorID == i)
                temp_fit <- inductFit_prelim(temp, model = model)
                rm(temp)
                
                Curve[[i]] <- temp_fit$Curve %>%
                    dplyr::mutate(DonorID = i, Omit = FALSE)
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
            
            Curve <- do.call(dplyr::bind_rows, Curve)
            
            G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction,
                                color = DonorID, shape = Omit))
            
            if(hline_foldinduct1){
                G <- G +
                    geom_hline(yintercept = 1, color = "red", linetype = "dotted")
            }   
            
            G <- G + 
                geom_point() +
                geom_line(data = Curve %>% 
                              filter(complete.cases(FoldInduction))) +
                scale_shape_manual(values = c(19, 1)) +
                guides(shape = "none")
            
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
            # Fitting all models, by donor, here
            
            # Keeping track of failed fits
            FitFail <- list()
            
            # fit all models by donor
            for(i in unique(DF$DonorID)){
                temp <- DF %>% dplyr::filter(DonorID == i)
                temp_fit <- inductFit_prelim(temp, model = model)
                
                FitFail[[i]] <- names(temp_fit$Fit)[
                    sapply(temp_fit$Fit, function(x) "p.value" %in% names(x)) == FALSE]
                
                rm(temp)
                
                Curve[[i]] <- temp_fit$Curve %>%
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
                DF <- DF %>% dplyr::select(-Model) %>%
                    dplyr::full_join(
                        expand.grid(DonorID = unique(DF$DonorID),
                                    Model = c("Emax", "EmaxSlope",
                                              "Slope", "Sig3Param"))) %>%
                    dplyr::mutate(Model_ch = ModelFacet[Model])
            )
            
            Curve <- do.call(dplyr::bind_rows, Curve) %>%
                dplyr::mutate(Model_ch = ModelFacet[Model])
            
            # Setting up facet labels for graphs
            DF$Model_ch <- factor(DF$Model_ch,
                                  levels = c("Emax model", "Emax slope model", 
                                             "sigmoidal 3-parameter model", 
                                             "slope model"))
            levels(DF$Model_ch) <- c(
                expression(E[max]~model),
                expression(E[max]~slope~model),
                expression(sigmoidal~"3-parameter"~model),
                expression(slope~model))
            
            Curve$Model_ch <- factor(Curve$Model_ch,
                                     levels = c("Emax model", "Emax slope model", 
                                                "sigmoidal 3-parameter model", 
                                                "slope model"))
            levels(Curve$Model_ch) <- c(
                expression(E[max]~model),
                expression(E[max]~slope~model),
                expression(sigmoidal~"3-parameter"~model),
                expression(slope~model))
            
            G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction,
                                color = DonorID, shape = Omit)) +
                scale_shape_manual(values = c(19, 1)) +
                guides(shape = "none")
            
            if(hline_foldinduct1){
                G <- G +
                    geom_hline(yintercept = 1, color = "red", linetype = "dotted") 
                
            }
            
            G <- G +
                geom_point() +
                labs(color = LegendTitle) +
                geom_line(data = Curve %>% 
                              filter(complete.cases(FoldInduction))) +
                facet_wrap(~ Model_ch, labeller = label_parsed)
            
            IndFit <- do.call(dplyr::bind_rows, MyFits) %>% 
                mutate(term = ifelse(term == "Emax" & Model == "Sig3Param", 
                                     "Indmax", term), 
                       term = ifelse(term == "EC50" & Model == "Sig3Param", 
                                     "IndC50", term))
            
            suppressMessages(
                IndFit_means <- IndFit %>% dplyr::group_by(Model, term) %>%
                    dplyr::summarize(Geomean = gm_mean(estimate),
                                     Mean = mean(estimate, na.rm = T),
                                     SD = sd(estimate, na.rm = T))
            )
            
            IndFit <- IndFit %>%
                dplyr::select(term, estimate, Model, DonorID) %>%
                tidyr::pivot_wider(values_from = estimate,
                                   names_from = term) %>%
                arrange(Model, DonorID)
            
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
                    "Curve" = Curve)
        
    } else {
        # Fitting mean data here
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
            dplyr::select(term, estimate, Model) %>%
            tidyr::pivot_wider(values_from = estimate,
                               names_from = term) %>% 
            arrange(Model)
        
    }
    
    # Making the final graph look nice --------------------------------------
    
    suppressWarnings(
        Out$Graph <- Out$Graph +
            annotation_logticks(sides = "b",
                                short = unit(1.5,"mm"),
                                mid = unit(1.5,"mm"),
                                long = unit(3,"mm")) +
            scale_x_log10() +
            xlab(ifelse(complete.cases(drug), 
                        paste(drug, "concentration (μM)"), "Concentration (μM)")) +
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
            
            FileName <- basename(save_output)
            
            rmarkdown::render(system.file("rmarkdown/templates/inductfit/skeleton/skeleton.Rmd",
                                          package="SimcypConsultancy"), 
                              output_dir = OutPath, 
                              output_file = FileName, 
                              quiet = TRUE)
            # Note: The "system.file" part of the call means "go to where the
            # package is installed, search for the file listed, and return its
            # full path.
            
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
    
    if(complete.cases(num_sigfig)){
        Out$Fit <- Out$Fit %>% 
            mutate(across(.cols = any_of(c("Emax", "EC50", "slope", "IndC50", "Indmax")), 
                          .fns = signif, digits = num_sigfig))
    }
    
    return(Out)
    
}


