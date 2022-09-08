#' Create a forest plot
#'
#' \code{forest_plot} creates forest plots of AUC and Cmax ratios. Please use
#' the function \code{\link{extractForestData}} to generate the input data for
#' \code{forest_dataframe}. The data will be broken up by the file name on the y
#' axis and, optionally, whatever you specify for \code{facet_column} in the
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
#'   "perpetrator". This will determine the graphs will be labeled on the y axis
#'   by the substrate name (for perpetrator forest plots) or by the effector
#'   name (for victim forest plots).
#' @param PKparameters optionally specify which PK parameters included in
#'   \code{forest_dataframe} to use as input. If left as NA, all the PK
#'   parameters you extracted with \code{\link{extractForestData}} will be
#'   included. If you try to include a parameter that's not already present in
#'   forest_dataframe, it will be ignored. Enclose the parameters with
#'   \code{c(...)}.
#' @param y_axis_order optionally supply a character vector to specify the order
#'   of the files or compounds on the y axis. If your character vector contains
#'   "xlsx" in the any of the text, we'll assume you want to sort by the file
#'   names; otherwise, we'll assume you want to sort by the compound names. If
#'   \code{y_axis_order} is unspecified, the y axis will be sorted according to
#'   the geometric mean AUC ratio with inhibitors on top and inducers on the
#'   bottom. If there's more than one row for a substrate or inhibitor (say
#'   you're including scenarios where you've reduced the Ki or the IndC50, for
#'   example), this may not give you the order you want and you might be better
#'   off specifying manually. If you supply a named character vector of file
#'   names, in addition to using that for setting the y axis order, we'll use
#'   those values for the y axis labels instead of the compound names. This can
#'   be useful when you want to add notes about changes in parameters such as
#'   when you're including sensitivity analyses. An example of the syntax to
#'   use: \code{y_axis_labels = c("abc1a-mdz.xlsx" = "Midazolam with in vitro
#'   induction", "abc-1a-mdz-10x.xlsx" = "Midazolam with 10-fold higher
#'   Indmax")} Please see the example section at the bottom of this help file
#'   for more examples.
#' @param x_axis_limits the x axis limits to use; default is 0.06 to 12.
#' @param facet_column optionally break up the graph into small multiples. The
#'   designated column name should be unquoted, e.g., \code{facet_column =
#'   Dose_sub}
#' @param prettify_compound_names TRUE (default) or FALSE on whether to make
#'   compound names prettier. This was designed for simulations where the
#'   substrates or effectors are among the standard options for the simulator,
#'   and leaving \code{prettify_compound_names = TRUE} will make the name of
#'   those compounds something more human readable. For example,
#'   "SV-Rifampicin-MD" will become "rifampicin", and "Sim-Midazolam" will
#'   become "midazolam".
#' @param legend_position specify where you want the legend to be. Options are
#'   "left", "right", "bottom", "top", or "none" (default) if you don't want one
#'   at all.
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
#' # with various effectors.
#' forest_plot(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             x_axis_limits = c(0.9, 5))
#'
#' # If there were multiple dosing levels of bufuralol, it might be
#' # nice to break up the graph by the substrate dose like this:
#' forest_plot(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             facet_column = Dose_sub,
#'             x_axis_limits = c(0.9, 5))
#'
#'
#' # If you want to maybe add some units in the graph to the substrate dose
#' # or maybe also say "Dose = X mg MyCompound", you can modify the column
#' # Dose_sub into a new column, Dose_sub_pretty, and use it like this:
#' ForestData <- ForestData %>%
#'                   mutate(Dose_sub_pretty = paste("Dose =", Dose_sub,
#'                                                  "mg bufuralol"))
#'
#' forest_plot(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             facet_column = Dose_sub_pretty,
#'             x_axis_limits = c(0.9, 5))
#' 

forest_plot <- function(forest_dataframe, 
                        perp_or_victim, 
                        PKparameters = NA, 
                        y_axis_order = NA, 
                        x_axis_limits = c(0.05, 15), 
                        facet_column, 
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
    
    if(all(c("File", "Substrate", "Dose_sub", "Dose_inhib", "Inhibitor1") %in%
           names(forest_dataframe)) == FALSE){
        stop("Please check your input for `forest_dataframe` because it does not appear to match the output from running the function `extractForestData`, and it must match that for this function to work.", 
             call. = FALSE)
    }
    
    # Checking for when there might be multiple files for the same substrate or
    # inhibitor names
    Check <- forest_dataframe %>% 
        mutate(Compound = switch(perp_or_victim, 
                                 "perpetrator" = Substrate, 
                                 "victim" = Inhibitor1)) %>% 
        group_by(Compound) %>% 
        summarize(N = n())
    if(is.na(y_axis_order[1]) & any(Check$N > 1)){
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
                    " but only if you specify something for `y_axis_order` so that it's clear what y axis labels you want to use for those files."),
             call. = FALSE)
    }
    
    if(complete.cases(y_axis_order[1])){
        if(anyDuplicated(unique(names(y_axis_order))) > 0 |
           length(unique(names(y_axis_order))) != length(unique(y_axis_order))){
            stop("One of your files is duplicated in `y_axis_order`. Please list each file only once and try again.",
                 call. = FALSE)
        }
        
        if(any(forest_dataframe$File %in% names(y_axis_order) == FALSE)){
            warning(paste0("The file(s) ", 
                           str_comma(setdiff(forest_dataframe$File, 
                                             names(y_axis_order))), 
                           " are not included in `y_axis_order`, so they will be labeled as NA on the y axis unless that's fixed."), 
                    call. = FALSE)
        }
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
    # Prettifying compound names before doing anything else 
    if(class(prettify_compound_names) == "logical" && # NB: "prettify_compound_names" is the argument value
       prettify_compound_names){ # NB: Note that we're only allowing T or F for prettify_compound_names here.
        forest_dataframe <- forest_dataframe %>%
            mutate(Substrate = prettify_compound_name(Substrate), 
                   Inhibitor1 = prettify_compound_name(Inhibitor1))
    }
    
    # Reshaping the data to make the data.frame long format instead of wide.
    forest_dataframe <- forest_dataframe %>% 
        pivot_longer(cols = matches("^AUC|Cmax"), 
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
    
    # Determining order of compounds if unspecified
    if(is.na(y_axis_order[1])){
        
        if(str_detect(perp_or_victim, "perp")){
            # perpetrator plots where y_axis_order is unspecified
            
            if(length(ParamToUse) == 0){
                YOrder <- sort(unique(forest_dataframe$Substrate))
            } else {
                YOrder <- forest_dataframe %>% select(Substrate, PKParam, GMR) %>% 
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
                mutate(YCol = factor(Substrate, levels = YOrder))
            
        } else {
            # victim plots where order y_axis_order is unspecified
            
            if(length(ParamToUse) == 0){
                YOrder <- sort(unique(forest_dataframe$Inhibitor1))
            } else {
                YOrder <- forest_dataframe %>% select(Inhibitor1, PKParam, GMR) %>% 
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
                mutate(YCol = factor(Inhibitor1, levels = YOrder))
        }
    } else {
        
        # Checking on *how* user specified order
        if(any(str_detect(y_axis_order, "xlsx")) |
           any(str_detect(names(y_axis_order), "xlsx"))){
            # This is when they specified by file names
            
            if(any(str_detect(names(y_axis_order), "xlsx"))){
                # When the file labels are specified
                Yorder <- names(y_axis_order)
                forest_dataframe <- forest_dataframe %>% 
                    mutate(YCol = y_axis_order[File],
                           File = factor(File, levels = Yorder)) %>% 
                    arrange(File) %>% 
                    mutate(YCol = factor(YCol, levels = unique(YCol)))
                
            } else {
                # When the file labels are not specified
                Yorder <- y_axis_order
                
                forest_dataframe <- forest_dataframe %>% 
                    mutate(YCol = factor(File, levels = Yorder))
            }
            
        } else {
            Yorder <- y_axis_order
            
            if(str_detect(perp_or_victim, "perp")){
                
                forest_dataframe <- forest_dataframe %>% 
                    mutate(YCol = factor(Substrate, levels = y_axis_order))
                
            } else {
                
                forest_dataframe <- forest_dataframe %>% 
                    mutate(YCol = factor(Inhibitor1, levels = y_axis_order))
                
            }
        }
    }
    
    # Setting things up for nonstandard evaluation 
    facet_column <- rlang::enquo(facet_column)
    
    if(as_label(facet_column) != "<empty>"){
        forest_dataframe <- forest_dataframe %>%
            mutate(FC = {{facet_column}})
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
    
    G <- ggplot(forest_dataframe, aes(x = GMR, y = PKParam, xmin = CI90_lo, xmax = CI90_hi)) +
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
        labs(fill = "Interaction level")
    
    if(as_label(facet_column) != "<empty>"){
        G <- G + facet_grid(YCol ~ FC, switch = "y") 
    } else {
        G <- G + facet_grid(YCol ~ ., switch = "y") 
    }
    
    XBreaks <- c(0.05, 0.1, 0.2, 0.5, 0.8, 1.25, 2, 5, 10)
    XBreaks <- XBreaks[XBreaks >= x_axis_limits[1] & 
                           XBreaks <= x_axis_limits[2]]
    
    if(x_axis_limits[1] %in% XBreaks == FALSE){
        XBreaks <- c(XBreaks, x_axis_limits[1])
    }
    
    if(x_axis_limits[2] %in% XBreaks == FALSE){
        XBreaks <- c(XBreaks, x_axis_limits[2])
    }

    G <- G +
        scale_x_log10(breaks = XBreaks) + 
        coord_cartesian(xlim = x_axis_limits) +
        xlab("Geometric Mean Ratio (90% confidence interval)") + ylab(NULL) +
        theme(
            legend.position = legend_position,
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            strip.background = element_rect(color=NA, fill="white"),
            strip.text.y.left = element_text(angle = 0),
            strip.placement = "outside",
            panel.border = element_rect(colour = "grey70", fill = NA),
            panel.spacing.y = unit(0, "cm"),
            panel.spacing.x = unit(0.5, "cm"))
    
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
        
        ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
               plot = G)
    }
    
    return(G)
}


