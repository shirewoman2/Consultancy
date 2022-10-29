#' Create a forest plot
#'
#' \code{forest_plot} creates forest plots of AUC and Cmax ratios. Please use
#' the function \code{\link{extractForestData}} to generate the input data for
#' \code{forest_dataframe}. The data will be broken up by the file name on the y
#' axis and, optionally, whatever you specify for \code{facet_column_x} in the
#' horizontal direction. WARNING: This is a relatively new function and should
#' be rigorously QC'ed.
#'
#' @param forest_dataframe a data.frame with extracted forest-plot data,
#'   generated from running \code{\link{extractForestData}} on Simulator output
#'   files or a csv file with the same data. If this is a forest plot of
#'   perpetrator DDIs, the column "Substrate" will be what is used to label the
#'   y axis. If it is a forest plot of victim DDIs, it will be the column
#'   "Inhibitor1". If, instead of the compound or inhibitor 1 names, you would
#'   like some other label to appear on the y axis, please see the argument
#'   \code{y_axis_labels}.
#' @param perp_or_victim specify whether the drug of interest is a "victim" or
#'   "perpetrator". This will determine whether the graphs will be labeled on
#'   the y axis by the substrate name (for perpetrator forest plots) or by the
#'   effector name (for victim forest plots).
#' @param PKparameters optionally specify which PK parameters included in
#'   \code{forest_dataframe} to use as input. If left as NA, all the PK
#'   parameters you extracted with \code{\link{extractForestData}} will be
#'   included. If you try to include a parameter that's not already present in
#'   forest_dataframe, it will be ignored. Enclose the parameters with
#'   \code{c(...)}.
#' @param y_axis_column the column by which you would like to break up the y
#'   axis, e.g., \code{y_axis_column = File} (default). You must specify
#'   something here; it cannot be left blank. If you use File as the column to
#'   break up the data, this will assume that what you want to show on the y
#'   axis is the substrate if the drug of interest is a perpetrator or the
#'   inhibitor if it's a victim.
#' @param y_order optionally supply a character vector to specify the order of
#'   the items on the y axis. If you'd like to change the name of the item as on
#'   the y axis -- for example, change your file names to something more
#'   readable than things like "abc-25mg-qd.xlsx" -- you can do that here with a
#'   named character vector like this: \code{y_order = c("abc1a-mdz.xlsx" =
#'   "Midazolam with in vitro induction", "abc1a-mdz-10x.xlsx" = "Midazolam with
#'   10-fold higher Indmax")} If \code{y_order} is left as NA, the y axis will
#'   be sorted according to the geometric mean AUC ratio with inhibitors on top
#'   and inducers on the bottom. Please see the example section at the bottom of
#'   this help file for more examples.
#' @param y_axis_column_secondary optionally break up the graphs along the y
#'   axis by an additional column. For example, say your drug of interest is a
#'   perpetrator and you've administered each of the substrates on different
#'   days. Here's how you could approach that: \enumerate{\item{Make a column
#'   "DoseDay" in your data.frame that specifies which day the substrate was
#'   administered. Let's say you have values in that column of "Day 1", "Day 5",
#'   and "Day 14".} \item{Set \code{y_axis_column = DoseDay}} \item{Set the
#'   order of the dose days: \code{y_order = c("Day 1", "Day 5", "Day 14")}.
#'   Please note that these must perfectly match the actual values in that
#'   DoseDay column.} \item{Set \code{y_axis_column_secondary = Substrate}}
#'   \item{Last, set \code{y_order_secondary = c("buprenorphine", "repaglinide",
#'   "midazolam")}, where those are the substrates simulated in the order you
#'   want.}} This will break up the graphs first by the dose day and next by the
#'   substrate and do so in the order you specified.
#' @param y_order_secondary 
#' @param x_axis_limits the x axis limits to use; default is 0.06 to 12.
#' @param x_axis_label optionally supply a character vector or an expression to
#'   use for the x axis label
#' @param facet_column_x optionally break up the graph horizontally into small
#'   multiples. The designated column name should be unquoted, e.g.,
#'   \code{facet_column_x = Dose_sub}
#' @param dose_units the units used in dosing. If you set \code{facet_column_x},
#'   \code{y_axis_column}, or \code{y_axis_column_secondary} to Dose_sub or
#'   Dose_inhib, setting the dose units here will automatically add those units
#'   to the appropriate graph labels. This way, the graph label will be, e.g.,
#'   "50 mg" and "100 mg" instead of just "50" and "100". This just helps make
#'   it clearer what the numbers represent. If you specify anything other than
#'   Dose_sub or Dose_inhib for \code{facet_column_x}, \code{y_axis_column}, or
#'   \code{y_axis_column_secondary}, this will be ignored.
#' @param prettify_compound_names TRUE (default) or FALSE on whether to make
#'   compound names prettier. This was designed for simulations where the
#'   substrates or effectors are among the standard options for the simulator,
#'   and leaving \code{prettify_compound_names = TRUE} will make the name of
#'   those compounds something more human readable. For example,
#'   "SV-Rifampicin-MD" will become "rifampicin", and "Sim-Midazolam" will
#'   become "midazolam".
#' @param legend_position specify where you want the legend to be. Options are
#'   "left", "right", "bottom", "top", or "none" (default) if you don't want one
#'   at all. \emph{Note:} We're still working on the legend position when
#'   there's something specified for \code{y_axis_column_secondary}; choosing
#'   that option requires us to lay out the graphs differently, and when we do
#'   that, the legend position doesn't work well for anything other than "none"
#'   or "right".
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png" or "My conc time
#'   graph.docx". If you leave off ".png" or ".docx" from the file name, it will
#'   be saved as a png file, but if you specify a different graphical file
#'   extension, it will be saved as that file format. Acceptable graphical file
#'   extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg".
#'   Leaving this as NA means the file will not be saved to disk.
#'   \strong{WARNING:} SAVING TO WORD DOES NOT WORK ON SHAREPOINT. This is a
#'   Microsoft permissions issue, not an R issue. If you try to save on
#'   SharePoint, you will get a warning that R will save your file instead to
#'   your Documents folder.
#' @param fig_height figure height in inches; default is 6
#' @param fig_width figure width in inches; default is 5
#'
#' @return Output is a graph.
#' @import tidyverse
#' @export
#'
#' @examples
#'
#' # We'll use some example forest-plot data for the substrate bufuralol
#' # with various effectors. To start, we'll just look at one dose level.
#' Buf_lowdose <- ForestData %>% filter(Dose_sub == 20)
#' forest_plot(forest_dataframe = Buf_lowdose,
#'             perp_or_victim = "victim",
#'             x_axis_limits = c(0.9, 5))
#'
#' # If there were multiple dosing levels of your drug, though, it might be
#' # nice to break up the graph by the substrate dose like this:
#' forest_plot(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             facet_column_x = Dose_sub,
#'             x_axis_limits = c(0.9, 5))
#'
#' # Maybe you want just one longer graph  with all the low-dose simulations
#' # on the top and the high-dose simulations on the bottom. Here's one
#' # way to do that: 

forest_plot <- function(forest_dataframe, 
                        perp_or_victim, 
                        PKparameters = NA, 
                        y_axis_column = File, 
                        y_order = NA, 
                        y_axis_column_secondary,
                        y_order_secondary = NA,
                        x_axis_limits = NA, 
                        x_axis_label = NA,
                        facet_column_x, 
                        dose_units = "mg",
                        prettify_compound_names = TRUE, 
                        legend_position = "none", 
                        save_graph = NA,
                        fig_height = 6,
                        fig_width = 5){
    
    # Error catching -----------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
             call. = FALSE)
    }
    
    if(class(forest_dataframe) == "character" && 
       str_detect(forest_dataframe, "csv")){
        forest_dataframe <- read.csv(forest_dataframe)
    }
    
    if(nrow(forest_dataframe) == 0){
        stop("Please check your input. The data.frame you supplied for `forest_dataframe` doesn't have any rows.", 
             call. = FALSE)
    }
    
    if(all(c("File", "Substrate", "Inhibitor1") %in%
           names(forest_dataframe)) == FALSE){
        stop("Please check your input for `forest_dataframe` because it does not appear to match the output from running the function `extractForestData`, and it must match that for this function to work.", 
             call. = FALSE)
    }
    
    # Cleaning up inputs
    perp_or_victim <- ifelse(str_detect(perp_or_victim, "perp"), 
                             "perpetrator", "victim")
    
    # y_axis_column must not be empty.
    if(as_label(rlang::enquo(y_axis_column)) == "<empty>"){
        stop("Something must be specified for `y_axis_column` for the forest_plot function to work.", 
             call. = FALSE)
    }
    
    # Either y_axis_column or y_axis_column_secondary must be File. Checking
    # for that.
    if(as_label(rlang::enquo(y_axis_column_secondary)) != "File" &
       as_label(rlang::enquo(y_axis_column)) != "File"){
        stop("Either `y_axis_column` or `y_axis_column_secondary` must be set to file for the forest_plot function to work.", 
             call. = FALSE)
    }
    
    # Checking for when there might be multiple files for the same substrate or
    # inhibitor names
    Check <- forest_dataframe %>%
        mutate(Compound = switch(perp_or_victim,
                                 "perpetrator" = Substrate,
                                 "victim" = Inhibitor1))
    
    FacetSec <- paste(as_label(rlang::enquo(facet_column_x)) == "<empty>", 
                      as_label(rlang::enquo(y_axis_column_secondary)) == "<empty>")
    
    if(FacetSec == "TRUE TRUE"){
        # user is not faceting and no sec y
        Check <- Check %>%
            group_by(Compound)
    } else if(FacetSec == "FALSE TRUE"){
        # user *is* faceting, no sec y
        Check <- Check %>% group_by(Compound, !!rlang::enquo(facet_column_x))
    } else if(FacetSec == "FALSE FALSE"){
        # user is faceting, there is a sec y
        Check <- Check %>% group_by(Compound, !!rlang::enquo(facet_column_x),
                                    !!rlang::enquo(y_axis_column_secondary))
    } else {
        # user is not faceting, but there is a sec y: "TRUE FALSE"
        Check <- Check %>% group_by(Compound, 
                                    !!rlang::enquo(y_axis_column_secondary))
    }
    Check <- Check %>% summarize(N = n())
    
    if(is.na(y_order[1]) & any(Check$N > 1)){
        stop(paste0("You have more than one file per " ,
                    switch(perp_or_victim, 
                           "perpetrator" = "substrate", 
                           "victim" = "effector"),
                    " for `",
                    str_comma(Check$Compound[which(Check$N > 1)]),
                    "`. Did you specify `perp_or_victim` correctly? You *can* have more than one file per ",
                    switch(perp_or_victim, 
                           "perpetrator" = "substrate", 
                           "victim" = "effector"),
                    ", but only if you either facet your graphs with `facet_column_x` so that there's only one file per ",
                    switch(perp_or_victim, 
                           "perpetrator" = "substrate", 
                           "victim" = "effector"),
                    " or if you specify something for `y_order` so that it's clear what y axis labels you want to use for those files."),
             call. = FALSE)
    }
    
    if(complete.cases(y_order[1])){
        # RETURN TO THIS. Will need to uncomment this once I've written the extra facetting bit.    
        #     if(anyDuplicated(unique(names(y_order))) > 0 |
        #        (is.null(names(y_order)) == FALSE &&
        #        length(unique(names(y_order))) != length(unique(y_order)))){
        #         stop("One of your files or file labels is duplicated in `y_order`. Please list each file and each label only once and try again.",
        #              call. = FALSE)
        #     }
        
        if(is.null(names(y_order)) == FALSE && 
           any(forest_dataframe$File %in% names(y_order) == FALSE)){
            warning(paste0("The file(s) ", 
                           str_comma(setdiff(forest_dataframe$File, 
                                             names(y_order))), 
                           " are not included in `y_order`, so they will be labeled as NA on the y axis unless that's fixed."), 
                    call. = FALSE)
        }
    }
    
    if(class(prettify_compound_names) != "logical"){
        warning("You appear to have supplied something to the argument `prettify_compound_names` other than TRUE or FALSE. Unfortunately, those are the only permissible values. We'll set this to TRUE.", 
                call. = FALSE)
        prettify_compound_names <- TRUE
    }
    
    # Making most character arguments lower case to avoid case sensitivity
    legend_position <- tolower(legend_position)
    
    if(legend_position %in% c("none", "bottom", "left", "right", "top") == FALSE){
        warning(paste0("You listed `", legend_position, 
                       "` for the legend position, which is not among the permissible options. The default of no legend (legend_position = `none`) will be used."),
                call. = FALSE)
        legend_position <- "none"
    }
    
    # Main body of function -------------------------------------------------
    # Setting things up for nonstandard evaluation 
    facet_column_x <- rlang::enquo(facet_column_x)
    y_axis_column <- rlang::enquo(y_axis_column)
    y_axis_column_secondary <- rlang::enquo(y_axis_column_secondary)
    
    # Setting up a column in the DF for the compound of interest and prettifying
    # as desired by user.
    forest_dataframe <- forest_dataframe %>% 
        mutate(MyCompound_orig = switch(perp_or_victim,
                                        "perpetrator" = Substrate,
                                        "victim" = Inhibitor1),
               MyCompound = ifelse(prettify_compound_names, 
                                   prettify_compound_name(MyCompound_orig), 
                                   MyCompound_orig))
    
    # Reshaping the data to make the data.frame long format instead of wide.
    forest_dataframe <- forest_dataframe %>% 
        pivot_longer(cols = matches("^AUC.*ratio|Cmax.*ratio"), 
                     names_to = "PKParam", values_to = "Value") %>% 
        separate(PKParam, into = c("PKParam", "Statistic"), sep = "__") %>% 
        pivot_wider(names_from = "Statistic", values_from = "Value")
    
    if(complete.cases(PKparameters[1]) &&
       any(PKparameters %in% forest_dataframe$PKParam) == FALSE){
        stop("None of the PK parameters requested are present in the data.frame supplied for `forest_dataframe`. No graph can be made.", 
             call. = FALSE)
    }
    
    if(complete.cases(PKparameters[1])){
        forest_dataframe <- forest_dataframe %>% 
            filter(PKParam %in% {{PKparameters}})
    }
    
    forest_dataframe <- forest_dataframe %>% 
        # Graphing this is easiest if the levels start with the item we want on
        # the bottom of the y axis and work upwards.
        mutate(PKParam = factor(PKParam, levels = c("Cmax_ratio_last", 
                                                    "AUCtau_ratio_last", 
                                                    "Cmax_ratio_dose1", 
                                                    "AUCt_ratio_dose1", 
                                                    "AUCinf_ratio_dose1")))
    
    # Only use PK parameters where there are all complete cases. 
    ParamToUse <- forest_dataframe %>% select(PKParam, GMR) %>% 
        group_by(PKParam) %>% 
        summarize(GMR = all(complete.cases(GMR))) %>% 
        filter(GMR == TRUE) %>% pull(PKParam) %>% as.character()
    
    if(complete.cases(PKparameters) && 
       all(PKparameters %in% ParamToUse == FALSE)){
        warning(paste0("Not all of your supplied PK parameters had complete values, and only parameters with all complete values can be included here. The PK parameters with missing values, which will not be included in the graph, were: ", 
                       str_comma(setdiff(PKparameters, ParamToUse))), 
                call. = FALSE)
    }
    
    if(is.na(y_order[1]) & 
       (as_label(rlang::enquo(y_axis_column)) == "File" |
        as_label(rlang::enquo(y_axis_column_secondary)) == "File")){
        # Determining order of files/compounds if unspecified
        
        if(str_detect(perp_or_victim, "perp")){
            # perpetrator plots where y_order is unspecified
            
            if(length(ParamToUse) == 0){
                YCmpdOrder <- sort(unique(forest_dataframe$Substrate))
            } else {
                YCmpdOrder <- forest_dataframe %>%
                    select(Substrate, PKParam, GMR) %>% 
                    filter(PKParam %in% ParamToUse) %>% 
                    group_by(Substrate, PKParam) %>% 
                    summarize(GMR = max(GMR, na.rm = T)) %>% 
                    pivot_wider(names_from = PKParam, values_from = GMR) %>% 
                    arrange(across(any_of(c("AUCinf_ratio_dose1", "AUCt_ratio_dose1", 
                                            "AUCtau_ratio_last", "Cmax_ratio_dose1", 
                                            "Cmax_ratio_last")))) %>% 
                    pull(Substrate) %>% rev()
            }
            
            forest_dataframe <- forest_dataframe %>% 
                mutate(YCmpd = factor(Substrate, levels = YCmpdOrder), 
                       YCol = {{y_axis_column}})
            
            if(as_label(y_axis_column) %in% c("Dose_sub", "Dose_inhib")){
                forest_dataframe <- forest_dataframe %>% 
                    mutate(YCol = paste(YCol, {{dose_units}}), 
                           YCol = forcats::fct_reorder(.f = YCol, 
                                                       .x = !!y_axis_column,
                                                       .fun = min))
            }
            
        } else {
            # victim plots where order y_order is unspecified
            
            if(length(ParamToUse) == 0){
                YCmpdOrder <- sort(unique(forest_dataframe$Inhibitor1))
            } else {
                YCmpdOrder <- forest_dataframe %>%
                    select(Inhibitor1, PKParam, GMR) %>% 
                    filter(PKParam %in% ParamToUse) %>% 
                    group_by(Inhibitor1, PKParam) %>% 
                    summarize(GMR = max(GMR, na.rm = T)) %>% 
                    pivot_wider(names_from = PKParam, values_from = GMR) %>% 
                    arrange(across(any_of(c("AUCinf_ratio_dose1", "AUCt_ratio_dose1", 
                                            "AUCtau_ratio_last", "Cmax_ratio_dose1", 
                                            "Cmax_ratio_last")))) %>% 
                    pull(Inhibitor1) %>% rev()
            }
            
            forest_dataframe <- forest_dataframe %>% 
                mutate(YCmpd = factor(Inhibitor1, levels = YCmpdOrder), 
                       YCol = {{y_axis_column}}) %>% 
                arrange(YCmpd) %>% 
                mutate(File = factor(File, levels = unique(File)))
            
            if(as_label(y_axis_column) %in% c("Dose_sub", "Dose_inhib")){
                forest_dataframe <- forest_dataframe %>% 
                    mutate(YCol = paste(YCol, {{dose_units}}), 
                           YCol = forcats::fct_reorder(.f = YCol, 
                                                       .x = !!y_axis_column,
                                                       .fun = min))
            } else {
                forest_dataframe <- forest_dataframe %>% 
                    mutate(YCol = as.factor(YCol))
            }
        }
    } else {
        # this is when the user *has* specified y order
        
        # Checking on *how* user specified order
        if(as_label(y_axis_column) == "File"){
            # This is when they specified by file names
            
            # They may have specified file names or specified the compounds that
            # each file represents. Furthermore, they may have specified files
            # in the names of a character vector. Dealing with each of those
            # three scenarios.
            if(any(str_detect(names(y_order), "xlsx"))){
                # When the file labels are specified
                Yorder <- names(y_order)
                forest_dataframe <- forest_dataframe %>% 
                    mutate(YCol = y_order[File],
                           File = factor(File, levels = Yorder)) %>% 
                    arrange(File) %>% 
                    mutate(YCol = factor(YCol, levels = unique(YCol)))
                
            } else if(any(str_detect(y_order, "xlsx"))){
                # When the file labels are not specified but the files
                # themselves are
                Yorder <- y_order
                
                forest_dataframe <- forest_dataframe %>% 
                    mutate(YCol = factor(File, levels = Yorder))
            } else {
                # This is when they have gone straight to the compounds that
                # each file represents and just listed them.
                
                # Check whether the items they listed are among the compounds or
                # the prettified compounds.
                
                
            }
            
        } else {
            Yorder <- y_order
            
            if(str_detect(perp_or_victim, "perp")){
                
                forest_dataframe <- forest_dataframe %>% 
                    mutate(YCol = factor(Substrate, levels = y_order))
                
            } else {
                
                forest_dataframe <- forest_dataframe %>% 
                    mutate(YCol = factor(Inhibitor1, levels = y_order))
                
            }
        }
    }
    
    if(as_label(facet_column_x) != "<empty>"){
        forest_dataframe <- forest_dataframe %>%
            mutate(FCX = !!facet_column_x)
        if(as_label(facet_column_x) %in% c("Dose_sub", "Dose_inhib")){
            forest_dataframe <- forest_dataframe %>% 
                mutate(FCX = paste(FCX, {{dose_units}}), 
                       FCX = forcats::fct_reorder(.f = FCX, 
                                                  .x = !!facet_column_x,
                                                  .fun = min))
        }
    }
    
    Rect <- data.frame(Xmin = c(1.25, 2, 5, 0.5, 0.2, 0, 0.8), 
                       Xmax = c(2, 5, Inf, 0.8, 0.5, 0.2, 1.25), 
                       Ymin = -Inf, Ymax = Inf, 
                       IntLevel = c("weak", "moderate", "strong", 
                                    "weak", "moderate", "strong", "insignificant")) %>% 
        mutate(IntLevel = factor(IntLevel, 
                                 levels = c("insignificant", "weak", "moderate", 
                                            "strong")))
    
    FillColor <- c("insignificant" = "white", "weak" = "gray95", 
                   "moderate" = "gray90", "strong" = "gray75")
    
    if(is.na(x_axis_limits[1])){
        x_axis_limits <- c(
            round_down(x = min(forest_dataframe$CI90_lo)), 
            round_up(x = max(forest_dataframe$CI90_hi)))
        
    }
    
    XBreaks <- c(0.001, 0.01, 0.05, 0.1, 0.2, 0.5, 0.8, 1.25, 2, 5, 10, 
                 50, 100, 500, 1000)
    XBreaks <- XBreaks[XBreaks >= x_axis_limits[1] & 
                           XBreaks <= x_axis_limits[2]]
    
    if(x_axis_limits[1] %in% XBreaks == FALSE){
        XBreaks <- c(XBreaks, x_axis_limits[1])
    }
    
    if(x_axis_limits[2] %in% XBreaks == FALSE){
        XBreaks <- c(XBreaks, x_axis_limits[2])
    }
    
    ## Making graph w/y_order_secondary column specified ---------------------
    if(as_label(y_axis_column_secondary) != "<empty>"){
        # This is when the user wants to have an additional y axis grouping s/a
        # day of dosing or prandial state or something like that.
        
        forest_dataframe <- forest_dataframe %>%
            mutate(Y2Col = {{y_axis_column_secondary}})
        
        if(as_label(y_axis_column) == "File"){
            forest_dataframe <- forest_dataframe %>% 
                mutate(YCol = YCmpd)
        }
        
        if(as_label(y_axis_column_secondary) == "File"){
            forest_dataframe <- forest_dataframe %>% 
                mutate(Y2Col = YCmpd)
        }
        
        if(as_label(y_axis_column_secondary) %in% c("Dose_sub", "Dose_inhib")){
            forest_dataframe <- forest_dataframe %>% 
                mutate(Y2Col = paste(Y2Col, {{dose_units}}), 
                       Y2Col = forcats::fct_reorder(.f = Y2Col, 
                                                    .x = !!y_axis_column_secondary,
                                                    .fun = min))
        }
        
        G <- list()
        
        for(g in unique(forest_dataframe$YCol)){
            
            Forest_subset <- forest_dataframe %>% filter(YCol == g)
            
            if(as_label(y_axis_column_secondary) == "File"){
                Forest_subset$Y2Col <- factor(Forest_subset$YCmpd, 
                                              levels = YCmpdOrder)
            }
            
            G[[g]] <- suppressWarnings(
                ggplot(Forest_subset, aes(x = GMR, y = PKParam, 
                                          xmin = CI90_lo, xmax = CI90_hi)) +
                    geom_rect(data = Rect, aes(xmin = Xmin, xmax = Xmax, ymin = Ymin, 
                                               ymax = Ymax, fill = IntLevel), 
                              inherit.aes = FALSE) +
                    scale_fill_manual(values = FillColor) +
                    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
                    geom_point(shape = 21, size = 2.5, fill = "white") +
                    geom_errorbar(width = 0.3) +
                    scale_y_discrete(breaks = levels(forest_dataframe$PKParam),
                                     labels = c(expression(C[max]~ratio), 
                                                expression(AUC[tau]~ratio), 
                                                expression(C[max]~ratio), 
                                                expression(AUC[t]~ratio), 
                                                expression(AUC[infinity]~ratio))) +
                    labs(fill = "Interaction level") +
                    scale_x_log10(breaks = XBreaks) + 
                    coord_cartesian(xlim = x_axis_limits) +
                    xlab(ifelse(is.na(x_axis_label), 
                                "Geometric Mean Ratio (90% confidence interval)", 
                                x_axis_label)) + 
                    ylab(unique(Forest_subset$YCol)) +
                    theme_consultancy() +
                    theme(plot.margin = margin(0, 1, 1, 1),
                          legend.position = legend_position,
                          panel.background = element_rect(fill = "white", color = NA),
                          plot.background = element_rect(fill = "white", color = NA),
                          strip.background = element_rect(color=NA, fill="white"),
                          # strip.text = element_text(face = "bold"),
                          strip.text.y.left = element_text(angle = 0),
                          strip.placement = "outside",
                          panel.border = element_rect(colour = "grey70", fill = NA),
                          panel.spacing.y = unit(0, "cm"),
                          panel.spacing.x = unit(0.5, "cm"))
            )
            
            if(g != unique(forest_dataframe$YCol)[
                length(unique(forest_dataframe$YCol))]){
                
                G[[g]] <- G[[g]] +
                    theme(axis.text.x = element_blank(), 
                          axis.title.x = element_blank(),
                          axis.ticks.x = element_blank())
            }
            
            if(as_label(facet_column_x) != "<empty>"){
                G[[g]] <- G[[g]] + facet_grid(Y2Col ~ FCX, switch = "y") 
            } else {
                G[[g]] <- G[[g]] + facet_grid(Y2Col ~ ., switch = "y") 
            }
            
            rm(Forest_subset)
        }
        
        G <- patchwork::wrap_plots(G, ncol = 1, guides = "collect")
        
        
    } else {
        
        ## Making graph w/out y secondary column ------------------------------
        if(as_label(y_axis_column) == "File"){
            forest_dataframe <- forest_dataframe %>% 
                mutate(YCol = YCmpd)
        }
        
        G <- ggplot(forest_dataframe, aes(x = GMR, y = PKParam, 
                                          xmin = CI90_lo, xmax = CI90_hi)) +
            geom_rect(data = Rect, aes(xmin = Xmin, xmax = Xmax, 
                                       ymin = Ymin, ymax = Ymax, fill = IntLevel), 
                      inherit.aes = FALSE) +
            scale_fill_manual(values = FillColor) +
            geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
            geom_point(shape = 21, size = 2.5, fill = "white") +
            geom_errorbar(width = 0.3) +
            scale_y_discrete(breaks = levels(forest_dataframe$PKParam),
                             labels = c(expression(C[max]~ratio), 
                                        expression(AUC[tau]~ratio), 
                                        expression(C[max]~ratio), 
                                        expression(AUC[t]~ratio), 
                                        expression(AUC[infinity]~ratio))) +
            labs(fill = "Interaction level")
        
        if(as_label(facet_column_x) != "<empty>"){
            G <- G + facet_grid(YCol ~ FCX, switch = "y") 
        } else {
            G <- G + facet_grid(YCol ~ ., switch = "y") 
        }
        
        G <- suppressWarnings(
            G +
                scale_x_log10(breaks = XBreaks) + 
                coord_cartesian(xlim = x_axis_limits) +
                xlab(ifelse(is.na(x_axis_label), 
                            "Geometric Mean Ratio (90% confidence interval)", 
                            x_axis_label)) + 
                ylab(NULL) +
                theme_consultancy() +
                theme(
                    axis.line.x.bottom = element_blank(), 
                    axis.line.y.left = element_blank(),
                    legend.position = legend_position, ### KEEP THIS
                    strip.text = element_text(face = "bold"),
                    strip.text.y.left = element_text(angle = 0),
                    strip.placement = "outside",
                    panel.border = element_rect(colour = "grey70", fill = NA),
                    panel.spacing.y = unit(0, "cm"),
                    panel.spacing.x = unit(0.5, "cm")))
        
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
            Ext <- "png"
        }
        
        suppressWarnings(
            ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
                   plot = G)
        )
    }
    
    return(G)
}


