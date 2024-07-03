#' Fit induction data to calculate EC50, Emax, and/or slope
#'
#' \code{inductFit} fits induction data -- either activity or mRNA expression --
#' to one or all of four models for calculating Indmax or Emax, EC50, and, when
#' appropriate, a slope. \strong{Two important notes:} \itemize{\item{With the
#' exception of the sigmoidal 3-parameter model, the fitted parameter describing
#' maximal induction is Emax and \emph{not} Indmax, the parameter used in the
#' Simcyp Simulator. Note that \strong{Indmax = Emax + 1}.} \item{If you're
#' familiar with Howie's R script for fitting induction data
#' ("Induction_fit_script_ver2.r"), one way in which this function differs is
#' that the default setting includes no upper or lower bounds for parameter
#' estimates. This means that, if sufficient data to describe the curve do not
#' exist, the function will fail to deliver any fitted parameters, which is
#' meant to be a benefit but could be an annoyance depending on your
#' perspective. If you would like this function to behave more like Howie's
#' script, please look at the help file for the arguments \code{bounds_Emax},
#' \code{bounds_EC50}, and \code{bounds_slope}.}} For detailed
#' instructions and examples, please see the SharePoint file "Simcyp PBPKConsult
#' R Files - Simcyp PBPKConsult R Files/SimcypConsultancy function examples and
#' instructions/Fitting induction data/Fitting-induction-data.docx". (Sorry, we
#' are unable to include a link to it here.)
#'
#' @param DF the data.frame containing induction data with a column for the drug
#'   concentration, a column for the fold induction observed, and, if you want
#'   to fit the data by individual, a column for the donor ID. For an example,
#'   please run \code{view(IndData)}. 
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
#' @param graph_mean_of_fits TRUE or FALSE (default) for whether to graph a line
#'   representing the mean of all donors' fitted parameters. Please note that
#'   this is NOT line showing the model fitted to the mean data; instead, this
#'   is the mean of all the donors individually fitted parameters. This only
#'   applies when \code{fitByDonor = TRUE}. If \code{graph_mean_of_fits =
#'   FALSE}, graph will show a line for each donor's fitted parameters.
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
#' @param bounds_Emax lower and upper boundaries for fitting Indmax or Emax,
#'   specified as a vector of numbers, e.g., \code{c(1, 20)}; default is no
#'   boundaries. If you only want to specify an upper limit, set the first
#'   number to \code{-Inf} and the second number to that value. If you only want
#'   to specify a lower limit, set the first number to that value and the second
#'   number to \code{Inf}.
#' @param bounds_EC50 lower and upper boundaries for fitting IndC50 or EC50,
#'   specified as a vector of numbers, e.g., \code{c(1, 20)}; default is no
#'   boundaries. If you only want to specify an upper limit, set the first
#'   number to \code{-Inf} and the second number to that value. If you only want
#'   to specify a lower limit, set the first number to that value and the second
#'   number to \code{Inf}.
#' @param bounds_slope lower and upper boundaries for fitting the slope,
#'   specified as a vector of numbers, e.g., \code{c(1, 20)}; default is no
#'   boundaries. If you only want to specify an upper limit, set the first
#'   number to \code{-Inf} and the second number to that value. If you only want
#'   to specify a lower limit, set the first number to that value and the second
#'   number to \code{Inf}.
#' @param bounds_gamma lower and upper boundaries for fitting the Hill
#'   coefficient gamma, specified as a vector of numbers, e.g., \code{c(1, 20)};
#'   default is no boundaries. If you only want to specify an upper limit, set
#'   the first number to \code{-Inf} and the second number to that value. If you
#'   only want to specify a lower limit, set the first number to that value and
#'   the second number to \code{Inf}.
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
#' @param vert_line optionally include a vertical dotted red line at some
#'   concentration. Use a numeric value for the concentration you want or leave
#'   as NA (default) for no vertical line.
#' @param Imaxu_line optionally specify a value for 30*Imax,u (see p. 5 USFDA
#'   Guidance (2020) "In Vitro Drug Interaction Studies - Cytochrome P450 Enzyme
#'   and Transporter Mediated Drug Interactions") and this will create red
#'   dotted line that goes down from the top of the graph at that concentration
#'   and then across to the y axis at the 2-fold change level. Points within
#'   that upper left corner are a potential concern for induction. Use a numeric
#'   value for the concentration you want or leave as NA (default) for no line.
#' @param rounding option for what rounding to perform, if any. Options are:
#'   \describe{
#'
#'   \item{NA or "Consultancy"}{All output will be rounded according
#'   to Simcyp Consultancy Team standards: to three significant figures when the
#'   value is < 100 or to the ones place if the value is >= 100. Please see the
#'   function \code{\link{round_consultancy}}, which does the rounding here.}
#'
#'
#'   \item{"none"}{No rounding will be performed.}
#'
#'   \item{"significant X" where "X" is a number}{Output will be rounded to X
#'   significant figures. \code{rounding = "signif X"} also works fine.}
#'
#'   \item{"round X" where "X" is a number}{Output will be rounded to X digits}
#'
#'   \item{"Consultancy"}{round according to the Simcyp Consultancy report 
#'   template: 3 significant figures if less than 100 and otherwise, round to 
#'   the nearest integer}
#'
#'   \item{"Word only"}{Output saved to Word file will be rounded using the
#'   function \code{\link{round_consultancy}}, but nothing will be rounded in
#'   the csv file if you supply a file name for \code{save_table} or in
#'   the output R object. This can be useful when you want to have nicely
#'   rounded and formatted output in a Word file while
#'   keeping all the decimal places elsewhere}}
#' @param include_fit_stats TRUE (default) or FALSE for whether to include
#'   statistics describing the goodness of fit: the standard error (SE), the p
#'   value, and the Akaike information criterion (AIC). These values come from
#'   the nonlinear regression of the various models to the data.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My induction graph.png". If you leave off ".png", it
#'   will be saved as a png file, but if you specify a different file extension,
#'   it will be saved as that file format. Acceptable extensions are "eps",
#'   "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg". Do not include any
#'   slashes, dollar signs, or periods in the file name. If you only want to
#'   save the table of fitted parameters, please use the argument
#'   \code{save_table}. If you want to save a Word document with all the
#'   results, please use the argument \code{save_output}.
#' @param fig_height figure height in inches; default is 5
#' @param fig_width figure width in inches; default is 5.5
#' @param save_table optionally save the table of fitted parameters by supplying
#'   a file name in quotes here, e.g., "My fitted induction parameters.csv".
#'   Saving as an xlsx file is also supported. Do not include any slashes,
#'   dollar signs, or periods in the file name. If you only want to save the
#'   graphs, please use the argument \code{save_graph}. If you want to save a
#'   Word document with all the results, please use the argument
#'   \code{save_output}.
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" (default) or "landscape" 
#' @param save_output optionally save a) the equations used for fitting, b) the
#'   fitted parameters, and c) the graphs by supplying a file name in quotes
#'   here, e.g., "My induction results.docx". Do not include any slashes, dollar
#'   signs, or periods in the file name. If you only want to save the graphs,
#'   please use the argument \code{save_graph}. If you only want to save the
#'   table of fitted parameters, please use the argument \code{save_table}.
#'
#' @return Returns a list of \describe{ \item{Fit}{the fitted parameters}
#'   \item{Fit_means}{the mean fitted parameters for all donors} \item{Graph}{a
#'   graph or set of graphs of the data with the fitted parameters depicted as
#'   lines} \item{Curve}{a data.frame of numeric data used to graph the fitted
#'   parameters in case you'd like to plot the fitted parameters in some other
#'   way}}
#' @export
#' @examples
#' # IndData is a data.frame of example induction data included with the package.
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
                      donor_column = DonorID,
                      fitByDonor = TRUE, 
                      model = "all",
                      graph_mean_of_fits = FALSE,
                      measurement = "mRNA",
                      enzyme = "CYP3A4", 
                      drug = NA,
                      weights = "1/y^2", 
                      bounds_Emax = NA,
                      bounds_EC50 = NA,
                      bounds_slope = NA,
                      bounds_gamma = NA,
                      omit = NA,
                      color_set = "default",
                      y_axis_limits = NA,
                      hline_foldinduct1 = FALSE,
                      vert_line = NA,
                      Imaxu_line = NA, 
                      rounding = NA, 
                      include_fit_stats = TRUE, 
                      graph_title = NA,
                      graph_title_size = 14, 
                      save_graph = NA,
                      save_table = NA,
                      page_orientation = "portrait", 
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
                   "indmax" = "Emax",
                   "emaxslope" = "EmaxSlope", 
                   "indmaxslope" = "EmaxSlope",
                   "slope" = "Slope", 
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
      stop("Measurement options are 'mRNA' or 'activity'. Please select one of those and try again.",
           call. = FALSE)
   }
   
   if(length(measurement) > 1){
      stop("Please select only one option for the measurement. Options are 'mRNA' or 'activity'.",
           call. = FALSE)
   }
   
   if(any(complete.cases(bounds_Emax))){
      if(length(bounds_Emax) == 1){
         warning("You have specified only one value for the boundaries for fitting Emax, and it requires two. We will not place any boundaries on the fit for Emax.")
         bounds_Emax <- NA
      }
   }
   
   if(any(complete.cases(bounds_EC50))){
      if(length(bounds_EC50) == 1){
         warning("You have specified only one value for the boundaries for fitting EC50, and it requires two. We will not place any boundaries on the fit for EC50.")
         bounds_EC50 <- NA
      }
   }
   
   if(any(complete.cases(bounds_slope))){
      if(length(bounds_slope) == 1){
         warning("You have specified only one value for the boundaries for fitting slope, and it requires two. We will not place any boundaries on the fit for slope.")
         bounds_slope <- NA
      }
   }
   
   if(any(complete.cases(bounds_gamma))){
      if(length(bounds_gamma) == 1){
         warning("You have specified only one value for the boundaries for fitting the Hill coefficient gamma, and it requires two. We will not place any boundaries on the fit for gamma.")
         bounds_gamma <- NA
      }
   }
   
   if(any(complete.cases(c(bounds_Emax, bounds_EC50, bounds_slope, bounds_gamma)))){
      Bounds <- TRUE
      if(any(is.na(bounds_Emax))){
         bounds_Emax <- c(-Inf, Inf)
      }
      
      if(any(is.na(bounds_EC50))){
         bounds_EC50 <- c(-Inf, Inf)
      }
      
      if(any(is.na(bounds_slope))){
         bounds_slope <- c(-Inf, Inf)
      }
      
      if(any(is.na(bounds_gamma))){
         bounds_gamma <- c(-Inf, Inf)
      }
      
   } else {
      Bounds <- FALSE
   }
   
   page_orientation <- tolower(page_orientation)[1]
   if(page_orientation %in% c("portrait", "landscape") == FALSE){
      warning("You must specify `portrait` or `landscape` for the argument page_orientation, and you've specified something else. We'll use the default of `portrait`.\n", 
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
      DF <- DF %>% select(any_of(c(rlang::as_label(conc_column),
                                   rlang::as_label(fold_change_column)))) %>%
         rename(FoldInduction = !! fold_change_column,
                Concentration_uM = !! conc_column)
      DF$DonorID <- "A"
   } else {
      DF <- DF %>% select(any_of(c(rlang::as_label(conc_column),
                                   rlang::as_label(fold_change_column),
                                   rlang::as_label(donor_column)))) %>%
         rename(FoldInduction = !! fold_change_column,
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
   
   # Changing the y axis label to fit activity or mRNA
   Ylab <- switch(measurement,
                  activity = "Fold change\n(activity with drug / activity with vehicle control)",
                  mRNA = "Fold change\n(mRNA with drug / mRNA with vehicle control)")
   
   # starting parameters for each model
   StartVals <- switch(model,
                       EmaxSlope = list(Emax = 4, # max(DF$Concentration_uM, na.rm = T), <-- not sure why this doesn't work, but it doesn't. Fit won't converge.
                                        EC50 = 5, # max(DF$Concentration_uM, na.rm = T)*2/3,
                                        Gamma = 1),
                       Emax = list(Emax = 4,
                                   EC50 = 5),
                       Slope = list(slope = 1),
                       Sig3Param = list(Indmax = 4,
                                        IndC50 = 5,
                                        Gamma = 1),
                       all = list(Emax = 4, EC50 = 5, slope = 1, Gamma = 1,
                                  Indmax = 4, IndC50 = 5))
   
   
   # Setting up the weights to use
   if(class(weights) == "character"){
      
      weights <- tolower(weights)
      
      DF <- DF %>%
         mutate(Weights = switch(weights, 
                                 "1/x" = 1/Concentration_uM, 
                                 "1/x^2" = 1/Concentration_uM^2,
                                 "1/y" = 1/FoldInduction,
                                 "1/y^2" = 1/FoldInduction^2, 
                                 "none" = 1)) 
      
      
   }
   
   # defining subfunction for making curves ----------------------------------
   
   fittedcurve <- function(indfit, model){
      
      Curve <- data.frame(Concentration_uM = seq(min(DF$Concentration_uM, na.rm = T),
                                                 1.2*max(DF$Concentration_uM, na.rm = T),
                                                 length.out = 300))
      MyCurves <- list()
      
      if(any(c("all", "Emax") %in% model) & 
         any(c("all", "Emax") %in% names(indfit)) &&
         all(complete.cases(indfit$Emax$estimate))){
         MyCurves[["Emax"]] <- Curve %>%
            mutate(Model = "Emax",
                   FoldInduction = 
                      1 + (indfit[["Emax"]]$estimate[indfit[["Emax"]]$term == "Emax"] *
                              Concentration_uM) /
                      (indfit[["Emax"]]$estimate[indfit[["Emax"]]$term == "EC50"] +
                          Concentration_uM))
      }
      
      if(any(c("all", "EmaxSlope") %in% model) & 
         any(c("all", "EmaxSlope") %in% names(indfit)) &&
         all(complete.cases(indfit$EmaxSlope$estimate))){
         MyCurves[["EmaxSlope"]] <- Curve %>%
            mutate(Model = "EmaxSlope",
                   FoldInduction = 
                      1 + (indfit[["EmaxSlope"]]$estimate[indfit[["EmaxSlope"]]$term == "Emax"] *
                              Concentration_uM^indfit[["EmaxSlope"]]$estimate[indfit[["EmaxSlope"]]$term == "Gamma"]) /
                      (indfit[["EmaxSlope"]]$estimate[indfit[["EmaxSlope"]]$term == "EC50"] ^
                          indfit[["EmaxSlope"]]$estimate[indfit[["EmaxSlope"]]$term == "Gamma"] +
                          Concentration_uM^indfit[["EmaxSlope"]]$estimate[indfit[["EmaxSlope"]]$term == "Gamma"]))
      }
      
      if(any(c("all", "Slope") %in% model) & 
         any(c("all", "Slope") %in% names(indfit)) &&
         all(complete.cases(indfit$Slope$estimate))){
         MyCurves[["Slope"]] <- Curve %>%
            mutate(Model = "Slope", 
                   FoldInduction = 
                      1 + Concentration_uM * indfit[["Slope"]]$estimate[indfit[["Slope"]]$term == "slope"])
      }
      
      if(any(c("all", "Sig3Param") %in% model) & 
         any(c("all", "Sig3Param") %in% names(indfit)) &&
         all(complete.cases(indfit$Sig3Param$estimate))){
         MyCurves[["Sig3Param"]] <- Curve %>%
            mutate(Model = "Sig3Param", 
                   FoldInduction = 
                      indfit[["Sig3Param"]]$estimate[indfit[["Sig3Param"]]$term == "Indmax"] /
                      (1+exp(-(Concentration_uM-indfit[["Sig3Param"]]$estimate[indfit[["Sig3Param"]]$term == "IndC50"]) /
                                indfit[["Sig3Param"]]$estimate[indfit[["Sig3Param"]]$term == "Gamma"])))
      }
      
      MyCurves <- bind_rows(MyCurves)  %>%
         mutate(Model_ch = ModelFacet[Model], # NB: Capital M here b/c referring to the column and not the argument
                Omit = FALSE)
      
      return(MyCurves)
      
   }
   
   # defining individual subfunctions for fitting induction models -------------
   
   Emax_fun <- function(indDF){
      tryCatch(
         nls(FoldInduction ~ 1+(Emax*Concentration_uM)/(EC50+Concentration_uM),
             data = indDF %>% filter(Omit == FALSE),
             start = StartVals[c("Emax", "EC50")], 
             algorithm = ifelse(Bounds, "port", "default"),
             lower = switch(as.character(Bounds),
                            "TRUE" = c(bounds_Emax[1], bounds_EC50[1]),
                            "FALSE" = -Inf),
             upper = switch(as.character(Bounds),
                            "TRUE" = c(bounds_Emax[2], bounds_EC50[2]),
                            "FALSE" = Inf),
             weights = indDF$Weights[indDF$Omit == FALSE]),
         error = function(x){data.frame(term = c("Emax", "EC50"),
                                        estimate = NA)})
   }
   
   EmaxSlope_fun <- function(indDF){
      tryCatch(
         nls(FoldInduction ~ 1+(Emax*Concentration_uM^Gamma) /
                (EC50^Gamma+Concentration_uM^Gamma),
             data = indDF %>% filter(Omit == FALSE),
             start = StartVals[c("Emax", "EC50", "Gamma")],
             algorithm = ifelse(Bounds, "port", "default"), 
             lower = switch(as.character(Bounds),
                            "TRUE" = c(bounds_Emax[1], bounds_EC50[1], bounds_gamma[1]),
                            "FALSE" = -Inf),
             upper = switch(as.character(Bounds),
                            "TRUE" = c(bounds_Emax[2], bounds_EC50[2], bounds_gamma[2]),
                            "FALSE" = Inf),
             weights = indDF$Weights[indDF$Omit == FALSE]),
         error = function(x){data.frame(term = c("Emax", "EC50", "Gamma"),
                                        estimate = NA)})
   }
   
   Slope_fun <- function(indDF){
      tryCatch(
         nls(FoldInduction ~ 1+(Concentration_uM*slope),
             data = indDF %>% filter(Omit == FALSE),
             start = StartVals["slope"], 
             algorithm = ifelse(Bounds, "port", "default"),
             lower = switch(as.character(Bounds),
                            "TRUE" = c(bounds_slope[1]),
                            "FALSE" = -Inf),
             upper = switch(as.character(Bounds),
                            "TRUE" = c(bounds_slope[2]),
                            "FALSE" = Inf),
             weights = indDF$Weights[indDF$Omit == FALSE]),
         error = function(x){data.frame(term = c("slope"),
                                        estimate = NA)})
   }
   
   Sig3Param_fun <- function(indDF){
      tryCatch(
         nls(FoldInduction ~ Indmax/(1+exp(-(Concentration_uM-IndC50)/Gamma)),
             data = indDF %>% filter(Omit == FALSE),
             start = StartVals[c("Indmax", "IndC50", "Gamma")], 
             algorithm = ifelse(Bounds, "port", "default"), 
             lower = switch(as.character(Bounds),
                            "TRUE" = c(bounds_Emax[1], bounds_EC50[1], bounds_gamma[1]),
                            "FALSE" = -Inf),
             upper = switch(as.character(Bounds),
                            "TRUE" = c(bounds_Emax[2], bounds_EC50[2], bounds_gamma[2]),
                            "FALSE" = Inf),
             weights = indDF$Weights[indDF$Omit == FALSE]),
         error = function(x){data.frame(term = c("Indmax", "IndC50", "slope"),
                                        estimate = NA)})
   }
   
   
   # defining general subfunction for fitting induction models -----------------
   inductFit_prelim <- function(indDF, model){
      
      indfit <- list()
      
      if(model != "all"){
         
         indfit[[model]] <- 
            switch(model,
                   "Emax" = Emax_fun(indDF),
                   "EmaxSlope" = EmaxSlope_fun(indDF),
                   "Slope" =  Slope_fun(indDF),
                   "Sig3Param" = Sig3Param_fun(indDF))
         
         if(class(indfit[[model]]) != "data.frame"){
            indfit[[model]] <- broom::tidy(indfit[[model]]) %>% 
               mutate(AIC = AIC(indfit[[model]]))
         }
         
         Emax <- indfit[[model]]$estimate[indfit[[model]]$term == "Emax"]
         EC50 <- indfit[[model]]$estimate[indfit[[model]]$term == "EC50"]
         Indmax <- indfit[[model]]$estimate[indfit[[model]]$term == "Indmax"]
         IndC50 <- indfit[[model]]$estimate[indfit[[model]]$term == "IndC50"]
         Slope <- indfit[[model]]$estimate[indfit[[model]]$term == "slope"]
         
         # Making data.frame to hold the predicted values for the graph
         Curve <- fittedcurve(indfit = indfit, 
                              model = model)
         
      } else {
         
         # Model is "all"
         suppressMessages(
            indDF <- indDF %>% select(-Model) %>% unique() %>% 
               full_join(
                  expand.grid(DonorID = unique(indDF$DonorID),
                              Model = c("Emax", "EmaxSlope",
                                        "Slope", "Sig3Param")))
         )
         
         indfit <- list(
            Emax = Emax_fun(indDF),
            EmaxSlope = EmaxSlope_fun(indDF),
            Slope =  Slope_fun(indDF),
            Sig3Param = Sig3Param_fun(indDF))
         
         GoodFits <- which(unlist(lapply(indfit, FUN = function(x) is.data.frame(x))) == FALSE)
         indfit[GoodFits] <- lapply(indfit[GoodFits], FUN = function(x){
            broom::tidy(x) %>% mutate(AIC = AIC(x)) })
         
         # Making data.frame to hold the predicted values for the graph
         Curve <- fittedcurve(indfit = indfit, 
                              model = model)
      }
      
      Out <- list("Fit" = indfit,
                  "Curve" = Curve)
      
      return(Out)
      
   }
   
   # Fitting models to data -----------------------------------------------
   
   # RETURN TO THIS. This needs better organization to avoid duplicative code,
   # especially since the Indmaxu_line option is only set up to work for some
   # scenarios (I was too busy w/other things to get this working perfectly.)
   
   if(fitByDonor){
      # fitting by donor here
      
      Curve <- list()
      MyFits <- list()
      
      if(model != "all"){
         # Fitting one specific model here
         
         for(i in unique(DF$DonorID)){
            temp <- DF %>% filter(DonorID == i)
            temp_fit <- inductFit_prelim(indDF = temp, model = model)
            rm(temp)
            
            Curve[[i]] <- temp_fit$Curve %>%
               mutate(DonorID = i, Omit = FALSE)
            MyFits[[i]] <- temp_fit$Fit[[model]] %>% mutate(DonorID = i)
            rm(temp_fit)
         }
         
         FitFail <- names(MyFits)[
            sapply(MyFits, function(x) "p.value" %in% names(x)) == FALSE]
         
         if(length(FitFail) > 0){
            warning(paste0("For donor ", str_comma(FitFail),
                           ", the ", model, " model failed to fit the data. No fitted line will be shown on the graph, and no fitted parameters will be returned."),
                    call. = FALSE)
         }
         
         Curve <- do.call(bind_rows, Curve)
         
         IndFit <- do.call(bind_rows, MyFits)
         suppressMessages(
            IndFit_means <- IndFit %>% group_by(term) %>%
               summarize(Geomean = gm_mean(estimate),
                         Mean = mean(estimate, na.rm = T),
                         SD = sd(estimate, na.rm = T))
         )
         
         IndFit <- IndFit %>%
            select(-statistic) %>% 
            pivot_longer(cols = -c(AIC, DonorID, term),
                         names_to = "Param",
                         values_to = "Value") %>% 
            mutate(Param = case_when(Param == "std.error" ~ paste0(term, "_SE"), 
                                     Param == "p.value" ~ paste0(term, "_pvalue"), 
                                     Param == "estimate" ~ term)) %>% 
            select(-term) %>% 
            pivot_wider(values_from = Value,
                        names_from = Param) %>% 
            mutate(Model = model) %>% 
            select(Model, DonorID,
                   any_of(paste0(rep(c("Emax", "EC50", "Gamma", "slope", "Indmax", "IndC50"), 
                                     each = 3), 
                                 c("", "_SE", "_pvalue"))), 
                   AIC)
         
         if(graph_mean_of_fits){
            
            IndFit_means_forcurves <- list(IndFit_means %>% rename(estimate = Mean))
            names(IndFit_means_forcurves) <- model
            
            Curve <- fittedcurve(indfit = IndFit_means_forcurves,
                                 model = model) %>% 
               left_join(expand.grid(Model = model, 
                                     DonorID = unique(DF$DonorID)), 
                         by = join_by(Model))
         }
         
         G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction,
                             color = DonorID, shape = Omit))
         
         GoodXLim <- ggplot_build(G + geom_point() + scale_x_log10())$layout$panel_params[[1]]$x.range
         GoodYLim <- ggplot_build(G + geom_point() + scale_x_log10())$layout$panel_params[[1]]$y.range
         
         if(hline_foldinduct1){
            G <- G +
               geom_hline(yintercept = 1, color = "red", linetype = "dotted")
            GoodYLim <- range(c(GoodYLim, 1))
         }
         
         if(complete.cases(vert_line)){
            G <- G +
               geom_vline(xintercept = vert_line, color = "red", linetype = "dotted")
            GoodXLim <- range(c(GoodXLim, log10(vert_line))) # needs to be log transformed since that's what it is in ggplot_build
         }
         
         if(complete.cases(Imaxu_line)){
            G <- G +
               geom_path(data = data.frame(
                  Concentration_uM = c(Imaxu_line, Imaxu_line,
                                       10^GoodXLim[1]*0.9),
                  FoldInduction = c(GoodYLim[2]*1.1, 2, 2),
                  Omit = NA),
                  color = "red", linetype = "dotted")
            
            GoodXLim <- range(c(GoodXLim, log10(Imaxu_line))) # needs to be log transformed since that's what it is in ggplot_build
            GoodYLim <- range(c(GoodYLim, 2))
         }
         
         G <- G +
            geom_point() +
            scale_shape_manual(values = c(19, 1)) +
            guides(shape = "none")
         
         if(graph_mean_of_fits){
            G <- G +
               geom_line(data = Curve %>%
                            filter(complete.cases(FoldInduction)), 
                         color = "black")
         } else {
            G <- G +
               geom_line(data = Curve %>%
                            filter(complete.cases(FoldInduction)))
         }
         
         
      } else {
         # Fitting all models, by donor, here
         
         # Keeping track of failed fits
         FitFail <- list()
         
         # fit all models by donor
         for(i in unique(DF$DonorID)){
            temp <- DF %>% filter(DonorID == i)
            temp_fit <- inductFit_prelim(indDF = temp, model = model)
            
            FitFail[[i]] <- names(temp_fit$Fit)[
               sapply(temp_fit$Fit, function(x) "p.value" %in% names(x)) == FALSE]
            
            rm(temp)
            
            Curve[[i]] <- temp_fit$Curve %>%
               mutate(DonorID = i)
            
            MyFits[[i]] <- 
               bind_rows(temp_fit$Fit[names(temp_fit$Fit) %in% 
                                         FitFail[[i]] == FALSE], .id = "Model") %>%
               mutate(DonorID = i)
            rm(temp_fit)
         }
         
         IndFit <- do.call(bind_rows, MyFits) %>%
            mutate(term = ifelse(term == "Emax" & Model == "Sig3Param",
                                 "Indmax", term),
                   term = ifelse(term == "EC50" & Model == "Sig3Param",
                                 "IndC50", term))
         
         suppressMessages(
            IndFit_means <- IndFit %>% group_by(Model, term) %>%
               summarize(Geomean = gm_mean(estimate),
                         Mean = mean(estimate, na.rm = T),
                         SD = sd(estimate, na.rm = T))
         )
         
         IndFit <- IndFit %>%
            select(-statistic) %>% 
            pivot_longer(cols = -c(AIC, DonorID, term, Model),
                         names_to = "Param",
                         values_to = "Value") %>% 
            mutate(Param = case_when(Param == "std.error" ~ paste0(term, "_SE"), 
                                     Param == "p.value" ~ paste0(term, "_pvalue"), 
                                     Param == "estimate" ~ term)) %>% 
            select(-term) %>% 
            pivot_wider(values_from = Value,
                        names_from = Param) %>% 
            select(Model, DonorID,
                   any_of(paste0(rep(c("Emax", "EC50", "Gamma", "slope", "Indmax", "IndC50"), 
                                     each = 3), 
                                 c("", "_SE", "_pvalue"))), 
                   AIC)
         
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
         
         if(graph_mean_of_fits){
            
            IndFit_means_forcurves <- IndFit_means %>% rename(estimate = Mean)
            IndFit_means_forcurves <- split(IndFit_means_forcurves, IndFit_means_forcurves$Model)
            
            Curve <- fittedcurve(indfit = IndFit_means_forcurves,
                                 model = model) %>% 
               left_join(expand.grid(Model = model, 
                                     DonorID = unique(DF$DonorID)), 
                         by = join_by(Model))
         }
         
         suppressMessages(
            DF <- DF %>% select(-Model) %>%
               full_join(
                  expand.grid(DonorID = unique(DF$DonorID),
                              Model = c("Emax", "EmaxSlope",
                                        "Slope", "Sig3Param"))) %>%
               mutate(Model_ch = ModelFacet[Model]) # NB: Capital M here b/c referring to the column and not the argument
         )
         
         # Setting up graphs -------------------------------------------------
         
         Curve <- do.call(bind_rows, Curve) %>%
            mutate(Model_ch = ModelFacet[Model]) # NB: Capital M here b/c referring to the column and not the argument
         
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
         
         GoodYLim <- ggplot_build(G + geom_point() + scale_x_log10())$layout$panel_params[[1]]$y.range
         GoodXLim <- ggplot_build(G + geom_point() + scale_x_log10())$layout$panel_params[[1]]$x.range
         
         if(hline_foldinduct1){
            G <- G +
               geom_hline(yintercept = 1, color = "red", linetype = "dotted")
            GoodYLim <- range(c(GoodYLim, 1))
            
         }
         
         if(complete.cases(vert_line)){
            G <- G +
               geom_vline(xintercept = vert_line, color = "red", linetype = "dotted")
            GoodXLim <- range(c(GoodXLim, log10(vert_line))) # needs to be log transformed since that's what it is in ggplot_build
         }
         
         if(complete.cases(Imaxu_line)){
            G <- G +
               geom_path(data = data.frame(
                  Concentration_uM = c(Imaxu_line, Imaxu_line,
                                       10^GoodXLim[1]*0.9),
                  FoldInduction = c(GoodYLim[2]*1.1, 2, 2),
                  Omit = NA),
                  color = "red", linetype = "dotted")
            
            GoodXLim <- range(c(GoodXLim, log10(Imaxu_line))) # needs to be log transformed since that's what it is in ggplot_build
            GoodYLim <- range(c(GoodYLim, 2))
         }
         
         G <- G +
            geom_point() +
            labs(color = LegendTitle) +
            facet_wrap(~ Model_ch, labeller = label_parsed)
         
         if(graph_mean_of_fits){
            G <- G +
               geom_line(data = Curve %>%
                            filter(complete.cases(FoldInduction)), 
                         color = "black")
         } else {
            G <- G +
               geom_line(data = Curve %>%
                            filter(complete.cases(FoldInduction)))
         }
      }
      
      Out <- list("Fit" = IndFit,
                  "Fit_means" = IndFit_means,
                  "Graph" = G,
                  "Curve" = Curve)
      
   } else {
      # Fitting mean data here
      Out <- inductFit_prelim(indDF = DF, model)
      if(model != "all"){
         Out$Fit[[model]]$Model <- model
      }
      
      # Checking for failed fits and printing warning message
      if(length(model) == 1 & model != "all"){
         FitFail <- "p.value" %in% names(Out$Fit[[model]]) == FALSE
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
      
      Out$Fit <- bind_rows(Out$Fit, .id = "Model") %>%
         select(term, estimate, Model) %>%
         tidyr::pivot_wider(values_from = estimate,
                            names_from = term) %>%
         arrange(Model)
      
      # Setting up graphs ------------------------------------------------------
      
      # Setting up facet labels for graphs
      DF <- DF %>% select(-c(Model, Model_ch)) %>% 
         left_join(expand.grid(DonorID = unique(DF$DonorID), 
                               Model = Out$Fit$Model), 
                   by = "DonorID") %>% 
         mutate(Model_ch = ModelFacet[Model]) # NB: Capital M here b/c referring to the column and not the argument
      
      DF$Model_ch <- factor(DF$Model_ch,
                            levels = c("Emax model", "Emax slope model",
                                       "sigmoidal 3-parameter model",
                                       "slope model"))
      levels(DF$Model_ch) <- c(
         expression(E[max]~model),
         expression(E[max]~slope~model),
         expression(sigmoidal~"3-parameter"~model),
         expression(slope~model))
      
      Out$Curve$Model_ch <- factor(Out$Curve$Model_ch,
                                   levels = c("Emax model", "Emax slope model",
                                              "sigmoidal 3-parameter model",
                                              "slope model"))
      levels(Out$Curve$Model_ch) <- c(
         expression(E[max]~model),
         expression(E[max]~slope~model),
         expression(sigmoidal~"3-parameter"~model),
         expression(slope~model))
      
      G <- ggplot(DF, aes(x = Concentration_uM, y = FoldInduction,
                          shape = Omit)) +
         scale_shape_manual(values = c(19, 1)) +
         guides(shape = "none") +
         geom_point() +
         geom_line(data = Out$Curve %>%
                      filter(complete.cases(FoldInduction)))
      
      GoodXLim <- ggplot_build(G + geom_point() + scale_x_log10())$layout$panel_params[[1]]$x.range
      GoodYLim <- ggplot_build(G + geom_point() + scale_x_log10())$layout$panel_params[[1]]$y.range
      
      
      if(model == "all"){
         G <- G + facet_wrap(~ Model_ch, scales = "free",
                             labeller = label_parsed)
      }
      
      Out[["Graph"]] <- G
   }
   
   ## Setting y axis limits ------------------------------------------------
   
   if(all(complete.cases(y_axis_limits))){
      if(length(y_axis_limits) < 2){
         warning("If you want to specify y axis limits, you must provide 2 numbers for the argument `y_axis_limits`, and you've only supplied one. We don't know what to use for y axis limits.\n", 
                 call. = FALSE)
         Ylim <- GoodYLim
      } else {
         Ylim <- y_axis_limits[1:2]
         GoodYLim <- Ylim
      }
   } else {Ylim <- GoodYLim}
   
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
   
   ## !!! RETURN TO THIS. Need to set up things to work both for when user want
   ## to set y axis limits AND when we're using GoodYLim to specify where the
   ## red dotted line should go up to.
   
   # suppressWarnings(
   #     Out$Graph <- Out$Graph +
   #         scale_y_continuous(limits = c(ifelse(is.na(y_axis_limits[1]),
   #                                              0, y_axis_limits[1]),
   #                                       YmaxRnd),
   #                            breaks = YBreaks,
   #                            labels = YLabels)
   # )
   
   if(complete.cases(graph_title)){
      Out$Graph <- Out$Graph + ggtitle(graph_title) +
         theme(plot.title = element_text(hjust = 0.5, size = graph_title_size))
   }
   
   ## Adding options for colors -----------------------------------------------
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
   
   ## Making the final graph look nice --------------------------------------
   
   suppressWarnings(
      Out$Graph <- Out$Graph +
         annotation_logticks(sides = "b",
                             short = unit(1.5,"mm"),
                             mid = unit(1.5,"mm"),
                             long = unit(3,"mm")) +
         scale_x_log10() +
         coord_cartesian(xlim = 10^GoodXLim, ylim = GoodYLim) +
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
   
   # saving and formatting output ---------------------------------------------
   
   if(include_fit_stats == FALSE){
      Out$Fit <- Out$Fit %>% select(-any_of(matches("_SE|_pvalue|AIC")))
   }
   
   if(complete.cases(save_graph)){
      FileName <- save_graph
      if(str_detect(FileName, "\\.")){
         # Making sure they've got a good extension
         Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
         FileName <- sub(paste0(".", Ext), "", FileName)
         if(Ext %in% c("eps", "ps", "jpeg", "tiff",
                       "png", "bmp", "svg", "jpg") == FALSE){
            warning(paste0("You have requested the graph's file extension be `",
                           Ext, "`, but we haven't set up that option. We'll save your graph as a `png` file instead.\n"),
                    call. = FALSE)
         }
         Ext <- ifelse(Ext %in% c("eps", "ps", "jpeg", "tiff",
                                  "png", "bmp", "svg", "jpg"),
                       Ext, "png")
         FileName <- paste0(FileName, ".", Ext)
      } else {
         FileName <- paste0(FileName, ".png")
      }
      
      ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
             plot = Out$Graph)
      
      rm(FileName)
      
   }
   
   if(complete.cases(save_table)){
      FileName <- save_table
      if(str_detect(FileName, "\\.")){
         # Making sure they've got a good extension
         Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
         FileName <- sub(paste0(".", Ext), "", FileName)
         if(Ext %in% c("csv", "xlsx") == FALSE){
            warning(paste0("You have requested the table's file extension be `",
                           Ext, "`, but we haven't set up that option. We'll save your table as a `csv` file instead.\n"),
                    call. = FALSE)
         }
         Ext <- ifelse(Ext %in% c("csv", "xlsx"),
                       Ext, "csv")
         FileName <- paste0(FileName, ".", Ext)
      } else {
         FileName <- paste0(FileName, ".csv")
      }
      
      # Rounding as requested
      Out$Fit <- Out$Fit %>%
         mutate(across(.cols = any_of(matches("Emax|EC50|Gamma|slope|IndC50|Indmax|AIC")),
                       .fns = round_opt, round_fun = rounding, is_this_for_Word = FALSE))
      
      write.csv(Out$Fit, FileName, row.names = F)
      
      rm(FileName)
      
   }
   
   if(complete.cases(save_output)){
      
      # Prettifying column names
      names(Out$Fit) <- sub("_SE", " SE", names(Out$Fit))
      names(Out$Fit) <- sub("_pvalue", " p value", names(Out$Fit))
      
      # Setting file extension to be docx
      FileName <- sub("\\..*$", ".docx", save_output)
      
      # May need to change the working directory temporarily, so determining
      # what it is now
      CurrDir <- getwd()
      
      OutPath <- dirname(FileName)
      if(OutPath == "."){
         OutPath <- getwd()
      }
      
      FileName <- basename(FileName)
      TemplatePath <- switch(page_orientation, 
                             "landscape" = system.file("Word/landscape_report_template.dotx",
                                                       package="SimcypConsultancy"), 
                             "portrait" = system.file("Word/report_template.dotx",
                                                      package="SimcypConsultancy"))
      
      # NB: NO ROUNDING before here! Rmd file optionally calculates mean of
      # fits, so it can't be character data yet!
      
      rmarkdown::render(system.file("rmarkdown/templates/inductfit/skeleton/skeleton.Rmd",
                                    package="SimcypConsultancy"),
                        output_format = rmarkdown::word_document(reference_docx = TemplatePath), 
                        output_dir = OutPath,
                        output_file = FileName,
                        quiet = TRUE)
      # Note: The "system.file" part of the call means "go to where the
      # package is installed, search for the file listed, and return its
      # full path.
      
   }
   
   # Rounding as requested
   Out$Fit <- Out$Fit %>%
      mutate(across(.cols = any_of(matches("Emax|EC50|Gamma|slope|IndC50|Indmax|AIC")),
                    .fns = round_opt, 
                    round_fun = rounding,
                    is_this_for_Word = FALSE))
   
   return(Out)
   
}


