#' Graph of simulated vs. observed PK
#'
#' \code{so_graph} makes a graph of simulated vs. observed PK, including
#' indicating where the predicted parameters fell within 1.5- or 2-fold of the
#' observed.
#'
#' @param PKtable a table in the same format as output from the function
#'   \code{\link{pksummary_mult}}
#' @param PKparameter any of the PK parameters included in the output from
#'   \code{\link{pksummary_mult}}. To see the full set of possible parameters,
#'   enter \code{view(PKParameterDefinitions)} into the console.
#' @param color_set set of colors to use for indicating the 1.5-fold and 2-fold
#'   boundaries of the simulated / observed ratio. The default is "red green",
#'   "muted red green" will result in lighter, somewhat more muted red and green
#'   that works better for indicating the boundary with shading instead of
#'   lines, and "black" will result in only black lines or shading. You also can
#'   set this to any two colors you'd like, e.g., \code{color_set = c("yellow",
#'   "blue")}
#' @param boundary_indicator how to indicate the boundaries of 1.5-fold and
#'   2-fold comparisons of simulated / observed. Options are "lines" (default)
#'   "fill" to get a shaded area, or "none" to remove any indicators of those
#'   boundaries.
#' @param line_width line width; default is 0.8
#'
#' @return
#' @export
#'
#' @examples
#' 

so_graph <- function(PKtable, 
                     PKparameter, 
                     color_set = "red green", 
                     boundary_indicator = "line",
                     line_width = 0.8){
    
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
    
    if(any(str_detect(PKparameter, "ratio"))){
        warning("You've requested a simulated vs. observed graph for a geometric mean ratio, which means that you really should make a graph like that in Guest Galetin 2011 Drug Metab Dispos paper, but we haven't that up yet.", 
                call. = FALSE)
    }
    
    # Main body of function --------------------------------------------------
    SO <- PKtable %>% 
        filter(Statistic %in% c("Simulated", "Observed")) %>%
        unique() %>% 
        pivot_longer(names_to = "PrettifiedNames", 
                     values_to = "Value", 
                     cols = c(-File, -Statistic)) %>% 
        mutate(Value = as.numeric(Value),
               PrettifiedNames = sub("with .* ", "with effector ", PrettifiedNames))
    
    # Checking whether PK parameters are, in fact, prettified
    if(any(str_detect(SO$PrettifiedNames, " "))){
        SO <- SO %>% 
            left_join(AllPKParameters %>% select(PrettifiedNames, PKparameter) %>% 
                          unique())
    } else {
        SO <- SO %>% mutate(PKparameter = PrettifiedNames)
    }
    
    SO <- SO %>% 
        select(-PrettifiedNames) %>% 
        pivot_wider(names_from = Statistic, values_from = Value) %>% 
        filter(complete.cases(Observed) & 
                   PKparameter %in% {{PKparameter}})
    
    Fold1.5_upper <- data.frame(Observed = 10^seq(-3, 6, length.out = 100)) %>% 
        mutate(LimitName = "upper", 
               Simulated = Observed * 1.5)
    Fold1.5_lower <- Fold1.5_upper %>% 
        mutate(LimitName = "lower", 
               Simulated = Observed / 1.5)
    
    Fold2_upper <- data.frame(Observed = 10^seq(-3, 6, length.out = 100)) %>% 
        mutate(LimitName = "upper", 
               Simulated = Observed * 2)
    Fold2_lower <- Fold2_upper %>% 
        mutate(LimitName = "lower", 
               Simulated = Observed / 2)
    Unity <- data.frame(Observed = 10^seq(-3, 6, length.out = 100)) %>% 
        mutate(Simulated = Observed)
    
    Limits <- c(
        round_down(min(c(SO$Observed, SO$Simulated), na.rm = T)),
        
        round_up(max(c(SO$Observed, SO$Simulated), na.rm = T)))
    
    if(length(color_set) == 1){
        BoundColors <- switch(color_set, 
                              "red green" = c("red", "#17A142"), 
                              "muted red green" = c("#E6A2A2", "#A4E4AF"),
                              "black" = c("black", "black"))
    } else {
        BoundColors <- color_set[1:2]
    }
    
    # Setting up polygongs
    Poly2x <- Fold2_upper %>% arrange(Observed) %>% 
        bind_rows(Fold2_lower %>% arrange(desc(Observed)))
    
    Poly1.5x <- Fold1.5_upper %>% arrange(Observed) %>% 
        bind_rows(Fold1.5_lower %>% arrange(desc(Observed)))
    
    
    G <- ggplot(SO, aes(x = Observed, y = Simulated)) +
        geom_line(data = Unity, linetype = "dashed", linewidth = line_width)
    
    if(str_detect(boundary_indicator, "line")){
        G <- G + 
            geom_line(data = Fold2_upper, color = BoundColors[1], linewidth = line_width) +
            geom_line(data = Fold2_lower, color = BoundColors[1], linewidth = line_width) +
            geom_line(data = Fold1.5_upper, color = BoundColors[2], linewidth = line_width) +
            geom_line(data = Fold1.5_lower, color = BoundColors[2], linewidth = line_width) 
    }
    
    if(str_detect(boundary_indicator, "fill")){
        G <- G +
            geom_polygon(data = Poly2x, fill = BoundColors[1]) +
            geom_polygon(data = Poly1.5x, fill = BoundColors[2])
    }
    
    G <- G + geom_point(size = 2) +
        scale_y_log10(
            breaks = c(10^(-1:4),
                       3*10^(-1:4),
                       5*10^(-1:4)),
            minor_breaks = rep(1:9)*rep(10^(-1:4), each = 9)) +
        scale_x_log10(
            breaks = c(10^(-1:4),
                       3*10^(-1:4),
                       5*10^(-1:4)),
            minor_breaks = rep(1:9)*rep(10^(-1:4), each = 9)) +
        coord_cartesian(xlim = Limits, ylim = Limits) +
        ggtitle(PKexpressions[[PKparameter]]) +
        theme_bw() +
        theme(aspect.ratio = 1, 
              plot.title = element_text(hjust = 0.5),
              axis.title = element_text(color = "black", face = "bold"),
              axis.title.x = element_text(margin = margin(2.75, 0, 0, 0)),
              axis.title.x.top = element_text(margin = margin(0, 0, 2.75, 0)),
              axis.title.y = element_text(margin = margin(0, 2.75, 0, 0)),
              axis.title.y.right = element_text(margin = margin(0, 0, 0, 2.75)))
    
    return(G)
}


