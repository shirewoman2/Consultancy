#' Automatically scale a ggplot graph y axis for concentrations
#'
#' @param conc_range user input for y axis limits for a linear plot
#' @param linear_or_log default "linear"
#' @param pad_y_axis user-specified value for pad_y_axis
#'
#' @return
#' @export
#'
#' @examples
#' 
#' # Working on it
#' 
#' 
scale_y_conc <- function(conc_range = NA, 
                         is_enz_plot = FALSE,
                         linear_or_log = "linear",
                         pad_y_axis = TRUE){
    
    conc_range_input <- conc_range
    conc_range <- sort(conc_range)
    
    # Error catching --------------------------------------------------------
    
    # Checking that input for conc_range is either NA or has length 2. 
    
    if((length(conc_range) == 1 && is.na(conc_range)) |
       (any(complete.cases(conc_range)) & length(conc_range) == 2) == FALSE){
        warning("Acceptable input for conc_range is either `NA` or two numeric values, and that's not what you've supplied. Please check the help file, and, for now, we'll use the full concentration range.",
                call. = FALSE)
        conc_range <- NA
    }
    
    linear_or_log <- ifelse(str_detect(tolower(linear_or_log), "log"),
                            "log", "linear")
    
    # Main body of function -------------------------------------------------
    
    ## Getting limits of the data ------------------------------------------
    GraphData <- last_plot()$data
    names(GraphData)[which(names(GraphData) == as_label(last_plot()$mapping$y))] <- "Conc"
    names(GraphData)[which(names(GraphData) == as_label(last_plot()$mapping$x))] <- "Time"
    
    if(all(is.na(conc_range))){
        conc_range <- range(GraphData$Conc, na.rm = TRUE)
        
        if(linear_or_log == "log"){
            # If conc_range[1] is 0, which can happen when the concs are really low, that
            # is undefined for log transformations. Setting it to be max value / 100
            # when that happens.
            conc_range[1] <- ifelse(conc_range[1] == 0, 
                                    conc_range[2]/100, conc_range[1])
            conc_range[1] <- round_down(conc_range[1])
            conc_range[2] <- round_up(conc_range[2])
            
            # If user set conc_range[1] to 0, set it to 1/100th the higher value and
            # tell them we can't use 0.
            if(conc_range[1] <= 0){
                conc_range[1] <- conc_range[2]/100
                warning("You requested a lower y axis limit that is undefined for log-transformed data. The lower y axis limit will be set to 1/100th the upper y axis limit instead.",
                        call. = FALSE)
            }
        } 
    }
    
    if(is_enz_plot & all(is.na(conc_range_input))){
        # If it's an enzyme abundance plot, user will generally want the lower
        # limit for the y axis to be 0 rather than 100 in the case of induction.
        # Setting that here, but user can override w/conc_range argument.
        conc_range[1] <- 0 
        conc_range[2] <- max(GraphData$Conc, na.rm = TRUE)
    }
    
    # Some users are sometimes getting Inf for possible upper limit of data,
    # although I haven't been able to reproduce this error. Trying to catch
    # that nonetheless.
    if(is.infinite(conc_range[2]) | is.na(conc_range[2])){
        conc_range[2] <- max(conc_range, na.rm = T)
    }
    
    ## Setting breaks ------------------------------------------------------
    if(linear_or_log == "linear"){
        
        PossYBreaks <- data.frame(Ymax = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50,
                                           100, 200, 500, 1000, 2000, 5000,
                                           10000, 20000, 50000, 100000,
                                           200000, 500000, Inf),
                                  YBreaksToUse = c(0.02, 0.05, 0.1, 0.2, 0.5,
                                                   1, 2, 5, 10, 20, 50, 100, 200,
                                                   500, 1000, 2000, 5000, 10000,
                                                   20000, 50000, 100000, 200000))
        
        YBreaksToUse <- PossYBreaks %>% filter(Ymax >= (conc_range[2] - conc_range[1])) %>%
            slice(which.min(Ymax)) %>% pull(YBreaksToUse)
        
        YInterval    <- YBreaksToUse
        conc_range[2]      <- ifelse(all(is.na(conc_range_input[2])), 
                                     round_up_unit(conc_range[2], YInterval),
                                     conc_range[2])
        YBreaks      <- seq(ifelse(is.na(conc_range[1]), 
                                   0, conc_range[1]),
                            conc_range[2], YInterval/2)                    # create labels at major and minor points
        YLabels      <- format(YBreaks, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
        YLabels[seq(2,length(YLabels),2)] <- ""                         # add blank labels at every other point i.e. for just minor tick marks at every other point
        
        # If the user specified a y axis range, make sure there's an axis label for
        # the top of the y axis
        if(any(complete.cases(conc_range_input))){
            YBreaks <- sort(unique(c(YBreaks, conc_range[2])))
            if(length(YLabels) == length(YBreaks)){
                YLabels[length(YLabels)] <- 
                    format(YBreaks[length(YBreaks)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
            } else {
                YLabels <- c(YLabels, 
                             format(YBreaks[length(YBreaks)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE))
            }
        }
    } else {
        # This is when it's a semi-log plot.
        
        YLogBreaks <- as.vector(outer(1:9, 10^(log10(conc_range[1]):log10(conc_range[2]))))
        YLogBreaks <- YLogBreaks[YLogBreaks >= conc_range[1] & YLogBreaks <= conc_range[2]]
        YLogLabels   <- rep("",length(YLogBreaks))
        
        
        if(all(is.na(conc_range_input)) |
           # checking whether conc_range values are a factor of 10 b/c, if they are,
           # then just use the conc_range that we would have come up with using the
           # other method b/c that makes prettier breaks
           all(log10(conc_range) == round(log10(conc_range)))){
            
            # add labels at order of magnitude
            YLogLabels[seq(1,length(YLogLabels),9)] <- 
                format(YLogBreaks[seq(1,length(YLogLabels),9)],
                       scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
            
        } else {
            
            # add labels for the 1st and last YLogBreaks and also 3 in between. The
            # odd fractions are b/c I want to have them spaced out somewhat
            # regularly, and that requires nonlinear intervals since it's log
            # transformed.
            YLogLabels[1] <- 
                format(YLogBreaks[1], scientific = FALSE, trim = TRUE, 
                       drop0trailing = TRUE)
            Nbreaks <- length(YLogBreaks)
            YLogLabels[Nbreaks] <- 
                format(YLogBreaks[Nbreaks], scientific = FALSE, trim = TRUE,
                       drop0trailing = TRUE)
            YLogLabels[round(Nbreaks/4)] <- 
                format(YLogBreaks[round(Nbreaks/4)], scientific = FALSE,
                       trim = TRUE, drop0trailing = TRUE)
            YLogLabels[round(2*Nbreaks/3)] <- 
                format(YLogBreaks[round(2*Nbreaks/3)], scientific = FALSE,
                       trim = TRUE, drop0trailing = TRUE)
            YLogLabels[round(5*Nbreaks/6)] <- 
                format(YLogBreaks[round(5*Nbreaks/6)], scientific = FALSE, 
                       trim = TRUE, drop0trailing = TRUE)
            
        } 
    }
    
    ## Getting other params for scale_y_XXX ----------------------------------
    
    # Noting whether geom_ribbon included.
    AnyRibbon <- any(sapply(last_plot()$layers, FUN = function(x) any(class(x$geom) == "GeomRibbon")))
    
    # Adding padding if user requests it
    if(class(pad_y_axis) == "logical"){ # class is logical if pad_y_axis unspecified
        if(is_enz_plot){
            if(pad_y_axis){
                pad_y_num <-  c(0.02, 0.1)
            } else {
                pad_y_num <- c(0, 0)
            }
            
        } else {
            if(pad_y_axis){
                pad_y_num <-  c(0.02, 0)
            } else {
                pad_y_num <- c(0, 0)
            }
        }
    } else {
        pad_y_num <- pad_y_axis
        if(length(pad_y_axis) == 1){
            pad_y_num <- c(pad_y_num, 0)
        } else {
            pad_y_num <- pad_y_axis[1:2]
        }
    }
    
    my_scale_fun <- function(){
        
    }
    
    
    return(switch(paste(linear_or_log, AnyRibbon), 
                  "linear TRUE" = scale_y_continuous(
                      limits = conc_range, 
                      labels = scales::percent,
                      expand = expansion(mult = pad_y_num)),
                  
                  "linear FALSE" = scale_y_continuous(
                      limits = conc_range,
                      labels = YLabels,
                      expand = expansion(mult = pad_y_num)) + 
                      coord_cartesian(xlim = range(GraphData$Time), 
                                      ylim = conc_range),
                  
                  "log TRUE" = scale_y_log10(
                      limits = conc_range, 
                      labels = scales::percent,
                      expand = expansion(mult = pad_y_num)),
                  
                  "log FALSE" = scale_y_log10(
                      limits = conc_range, 
                      breaks = YLogBreaks,
                      labels = YLogLabels,
                      expand = expansion(mult = pad_y_num))))
    
}

