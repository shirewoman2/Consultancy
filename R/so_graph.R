#' Graph of simulated vs. observed PK
#'
#' \code{so_graph} makes a graph of simulated vs. observed PK, including
#' indicating where the predicted parameters fell within 1.5- or 2-fold of the
#' observed. When the PK parameter is a geometric mean ratio of the parameter in
#' the presence of an effector / the parameter in the absence of the effector,
#' the 1.5-fold line will be replaced with a curve as described in Guest Galetin
#' 2011 Drug Metab Dispos.
#'
#' @param PKtable a table in the same format as output from the function
#'   \code{\link{pksummary_mult}}
#' @param PKparameters any of the PK parameters included in the output from
#'   \code{\link{pksummary_mult}}; if left as NA, this will make graphs for each
#'   parameter included in \code{PKtable}. To see the full set of possible
#'   parameters, enter \code{view(PKParameterDefinitions)} into the console.
#' @param color_set set of colors to use for indicating the 1.5-fold and 2-fold
#'   boundaries of the simulated / observed ratio. The default is "red black",
#'   which results in a black line at the 1.5-fold boundary and a red one at the
#'   2-fold boundary. Other options are "red green", "muted red green" (a
#'   lighter, somewhat more muted red and green that work well for indicating
#'   the boundary when you're using shading instead of lines), and "black",
#'   which will result in only black lines or shading. You also can set this to
#'   any two colors you'd like, e.g., \code{color_set = c("yellow", "blue")}
#' @param boundary_indicator how to indicate the boundaries of 1.5-fold and
#'   2-fold comparisons of simulated / observed. Options are "lines" (default)
#'   "fill" to get a shaded area, or "none" to remove any indicators of those
#'   boundaries. \strong{NOTE: There is a known bug within RStudio that causes
#'   filled semi-transparent areas like you get with the "fill" option to NOT
#'   get graphed for certain versions of RStudio.} To get around this, within
#'   RStudio, go to Tools --> Global Options --> General --> Graphics --> And
#'   then set "Graphics device: backend" to "AGG". Honestly, this is a better
#'   option for higher-quality graphics anyway!
#' @param line_width line width; default is 0.7
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png"
#' @param fig_height figure height in inches; default is 8
#' @param fig_width figure width in inches; default is 6
#'
#' @return a set of arranged ggplot2 graphs 
#' @export
#'
#' @examples
#' # Assuming you have run pksummary_mult on a few files with observed data 
#' # to generate an object called MyPKOutput, then:
#' so_graph(MyPKOutput)
#' so_graph(MyPKOutput, boundary_indicator = "fill")
#' 

so_graph <- function(PKtable, 
                     PKparameters = NA, 
                     color_set = "red black", 
                     boundary_indicator = "lines",
                     line_width = 0.7, 
                     save_graph = NA, 
                     fig_width = 8, 
                     fig_height = 6){
    
    # Error catching ----------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    if("list" %in% class(PKtable)){
        PKtable <- PKtable$Table
    }
    
    if("Observed" %in% PKtable$Statistic == FALSE){
        stop("We can't find the observed data in the table provided for `PKtable`, so we can't make a simulated-versus-observed graph.", 
             call. = FALSE)
    }
    
    boundary_indicator <- ifelse(str_detect(tolower(boundary_indicator), "fil"), 
                                 "fill", "lines")
    
    if(boundary_indicator == "fill" & length(color_set) == 1 & 
       !str_detect(color_set, " ")){
        warning("You have requested only one color for the fill, which means you'll only see a shaded rectangle for the 2-fold boundary. If you would also like to see the 1.5-fold or Guest-curve boundary, please add a second color.", 
                call. = FALSE)
    }
    
    if(any(is.na(PKparameters))){
        PKparameters <- names(PKtable)
        
        # Need to get the un-prettified names here
        PKparameters <- data.frame(PrettifiedNames = PKparameters) %>% 
            left_join(AllPKParameters %>% select(PrettifiedNames, PKparameter) %>% 
                          unique(), 
                      by = join_by(PrettifiedNames)) %>% 
            filter(complete.cases(PKparameter)) %>% 
            pull(PKparameter)
    }
    
    # Main body of function --------------------------------------------------
    suppressWarnings(
        SO <- PKtable %>% 
            filter(Statistic %in% c("Simulated", "Observed")) %>%
            unique() %>% 
            pivot_longer(names_to = "PrettifiedNames", 
                         values_to = "Value", 
                         cols = c(-File, -Statistic)) %>% 
            mutate(Value = as.numeric(Value),
                   PrettifiedNames = sub("with .* ", "with effector ", PrettifiedNames)) %>% 
            filter(complete.cases(Value))
    )
    
    # Checking whether PK parameters are, in fact, prettified
    if(any(str_detect(SO$PrettifiedNames, " "))){
        suppressMessages(
            SO <- SO %>% 
                left_join(AllPKParameters %>% select(PrettifiedNames, PKparameter) %>% 
                              unique())
        )
    } else {
        SO <- SO %>% mutate(PKparameter = PrettifiedNames)
    }
    
    SO <- SO %>% 
        pivot_wider(names_from = Statistic, values_from = Value) %>% 
        filter(complete.cases(Observed) & 
                   PKparameter %in% {{PKparameters}})
    
    Fold1.5_upper <- data.frame(Observed = 10^seq(-3, 6, length.out = 100)) %>% 
        mutate(LimitName = "upper", 
               Simulated = Observed * 1.5)
    Fold1.5_lower <- Fold1.5_upper %>% 
        mutate(LimitName = "lower", 
               Simulated = Observed / 1.5)
    
    Fold2_upper <- data.frame(Observed = 10^seq(-4, 6, length.out = 100)) %>% 
        mutate(LimitName = "upper", 
               Simulated = Observed * 2)
    Fold2_lower <- Fold2_upper %>% 
        mutate(LimitName = "lower", 
               Simulated = Observed / 2)
    Unity <- data.frame(Observed = 10^seq(-4, 6, length.out = 100)) %>% 
        mutate(Simulated = Observed)
    
    # Guest curves
    GuestCurve_upper <- data.frame(Observed = 10^seq(-4, 6, length.out = 1000)) %>% 
        mutate(Limit = ifelse(Observed >= 1, 
                              (1 + 2*(Observed - 1))/Observed,
                              (1 + 2*((1/Observed) - 1))/(1/Observed)),
               LimitName = "upper",
               Simulated = Observed * Limit)
    
    GuestCurve_lower <- GuestCurve_upper %>% 
        mutate(LimitName = "lower", 
               Simulated = Observed / Limit)
    
    
    # Setting up polygons
    Poly2x <- Fold2_upper %>% arrange(Observed) %>% 
        bind_rows(Fold1.5_upper %>% arrange(desc(Observed))) %>% 
        bind_rows(Fold1.5_lower %>% arrange(Observed)) %>% 
        bind_rows(Fold2_lower %>% arrange(desc(Observed)))
    
    Poly1.5x <- Fold1.5_upper %>% arrange(Observed) %>% 
        bind_rows(Fold1.5_lower %>% arrange(desc(Observed)))
    
    Poly2xGuest <- Fold2_upper %>% arrange(Observed) %>% 
        bind_rows(GuestCurve_upper %>% arrange(desc(Observed))) %>% 
        bind_rows(GuestCurve_lower %>% arrange(Observed)) %>% 
        bind_rows(Fold2_lower %>% arrange(desc(Observed)))
    
    Poly1.5xGuest <- GuestCurve_upper %>% arrange(Observed) %>% 
        bind_rows(GuestCurve_lower %>% arrange(desc(Observed)))
    
    
    if(length(color_set) == 1){
        BoundColors <- switch(color_set, 
                              "red green" = c("red", "#17A142"), 
                              "muted red green" = c("#E6A2A2", "#A4E4AF"),
                              "red black" = c("red", "black"),
                              "black" = c("black", "black"))
    } else {
        BoundColors <- color_set[1:2]
    }
    
    # It's possible to have both CLt_dose1 and CLinf_dose1 and they're labeled
    # the same way in PKexpressions. Adjusting for that scenario.
    if(all(c("CLinf_dose1", "CLt_dose1") %in% PKparameters)){
        PKexpressions[["CLinf_dose1"]] <- 
            expression(atop(Dose ~ 1 ~ CL ~ "(L/h)", 
                            paste("CL as dose/AUCinf")))
        PKexpressions[["CLt_dose1"]] <- 
            expression(atop(Dose ~ 1 ~ CL ~ "(L/h)", 
                            paste("CL as dose/AUCt")))
    }
    
    G <- list()
    SO <- split(SO, f = SO$PKparameter)
    
    for(i in names(SO)){
        
        Limits <- c(
            round_down(min(c(SO[[i]]$Observed, SO[[i]]$Simulated), na.rm = T)),
            
            round_up(max(c(SO[[i]]$Observed, SO[[i]]$Simulated), na.rm = T)))
        
        G[[i]] <- ggplot(SO[[i]], aes(x = Observed, y = Simulated)) +
            geom_line(data = Unity, linetype = "dashed", linewidth = line_width)
        
        if(str_detect(boundary_indicator, "line")){
            G[[i]] <- G[[i]] + 
                geom_line(data = Fold2_upper, color = BoundColors[1], linewidth = line_width) +
                geom_line(data = Fold2_lower, color = BoundColors[1], linewidth = line_width) +
                geom_line(data = switch(as.character(str_detect(i, "ratio")), 
                                        "TRUE" = GuestCurve_upper,
                                        "FALSE" = Fold1.5_upper), 
                          color = BoundColors[2], linewidth = line_width) +
                geom_line(data = switch(as.character(str_detect(i, "ratio")), 
                                        "TRUE" = GuestCurve_lower, 
                                        "FALSE" = Fold1.5_lower),
                          color = BoundColors[2], linewidth = line_width) 
        }
        
        if(str_detect(boundary_indicator, "fill")){
            G[[i]] <- G[[i]] +
                geom_polygon(data = switch(as.character(str_detect(i, "ratio")), 
                                           "TRUE" = Poly2xGuest, 
                                           "FALSE" = Poly2x), 
                             fill = BoundColors[1], alpha = 0.2) +
                geom_polygon(data = switch(as.character(str_detect(i, "ratio")),
                                           "TRUE" = Poly1.5xGuest,
                                           "FALSE" = Poly1.5x), 
                             fill = BoundColors[2], alpha = 0.2)
        }
        
        MaxMinRatio <- range(c(SO[[i]]$Observed, SO[[i]]$Simulated))
        MaxMinRatio <- MaxMinRatio[2] / MaxMinRatio[1]
        
        if(MaxMinRatio > 100){
            MajBreaks <- 10^(-3:6)
            MinBreaks <- rep(1:9)*rep(10^(-3:6), each = 9)
        } else {
            MajBreaks <- c(10^(-3:6),
                           3*10^(-3:6),
                           5*10^(-3:6))
            MinBreaks <- rep(1:9)*rep(10^(-3:6), each = 9)
        }
        
        G[[i]] <- G[[i]] + geom_point(size = 2) +
            scale_y_log10(breaks = MajBreaks, minor_breaks = MinBreaks) +
            scale_x_log10(breaks = MajBreaks, minor_breaks = MinBreaks) +
            coord_cartesian(xlim = Limits, ylim = Limits) + # this causes the shading to disappear for Guest curves. no idea why, but I think it's a bug w/coord_cartesian.
            ggtitle(PKexpressions[[i]]) +
            theme_bw() +
            theme(aspect.ratio = 1, 
                  plot.title = element_text(hjust = 0.5),
                  axis.title = element_text(color = "black", face = "bold"),
                  axis.title.x = element_text(margin = margin(2.75, 0, 0, 0)),
                  axis.title.x.top = element_text(margin = margin(0, 0, 2.75, 0)),
                  axis.title.y = element_text(margin = margin(0, 2.75, 0, 0)),
                  axis.title.y.right = element_text(margin = margin(0, 0, 0, 2.75)))
    }
    
    if(length(G) == 1){
        
        G <- G[[1]]
        
    } else {
        
        G <-  ggpubr::ggarrange(plotlist = G, align = "hv")
        
    }
    
    
    # Saving --------------------------------------------------------------
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
        
        # This is when they want any kind of graphical file format.
        ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
               plot = G)
        
    }
    
    return(G)
    
}


