#' Plot the dosing regimens used in simulations
#'
#' \code{dosing_regimen_plot} creates a graph of the dosing regimen used in the
#' simulations provided. UNDER CONSTRUCTION.
#'
#' @param existing_exp_details output from \code{\link{extractExpDetails}} or
#'   \code{\link{extractExpDetails_mult}}.
#' @param sims_to_include optionally specify which simulation files you'd like
#'   to include in the annotated output. Acceptable input:
#'
#'   \describe{\item{NA (default)}{get all the simulations included in
#'   \code{existing_exp_details}}
#'
#'   \item{a character vector of the file names you want}{The items in the character
#'   vector must \emph{exactly} match file names in the column "File" of the
#'   "MainDetails" item in \code{existing_exp_details}, including the ".xlsx" or ".db"
#'   file extension}}
#' @param colorBy_column (optional) the column in
#'   \code{existing_exp_details$Dosing} that should be used for determining
#'   which color lines will be. This should be unquoted, e.g.,
#'   \code{colorBy_column = Compound}.
#' @param color_set the set of colors to use. Options: \describe{
#'
#'   \item{"default"}{a set of colors from Cynthia Brewer et al. from Penn State
#'   that are friendly to those with red-green colorblindness. The first three
#'   colors are green, orange, and purple. This can also be referred to as
#'   "Brewer set 2". If there are only two unique values in the colorBy_column,
#'   then Brewer set 1 will be used since red and blue are still easily
#'   distinguishable but also more aesthetically pleasing than green and
#'   orange.}
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
#'   \item{"blues"}{a set of blues fading from sky to navy. Like
#'   "blue-green", this palette can be especially useful if you are comparing a
#'   systematic change in some continuous variable.}
#'
#'   \item{"greens"}{a set of greens fading from chartreuse to forest. Great for showing
#'   systematic changes in a continuous variable.}
#'
#'   \item{"purples"}{a set of purples fading from lavender to aubergine. Great for showing
#'   systematic changes in a continuous variable.}
#'
#'   \item{"reds"}{a set of reds from pink to brick. Great for showing
#'   systematic changes in a continuous variable.}
#'
#'   \item{"Tableau"}{uses the standard Tableau palette; requires the "ggthemes"
#'   package}
#'
#'   \item{"viridis"}{from the eponymous package by Simon Garnier and ranges
#'   colors from purple to blue to green to yellow in a manner that is
#'   "printer-friendly, perceptually uniform and easy to read by those with
#'   colorblindness", according to the package author}
#'
#'   \item{a character vector of colors}{If you'd prefer to set all the colors
#'   yourself to \emph{exactly} the colors you want, you can specify those
#'   colors here. An example of how the syntax should look: \code{color_set =
#'   c("dodgerblue3", "purple", "#D8212D")} or, if you want to specify exactly
#'   which item in \code{colorBy_column} gets which color, you can supply a
#'   named vector. For example, if you're coloring the lines by the compound ID,
#'   you could do this: \code{color_set = c("substrate" = "dodgerblue3",
#'   "inhibitor 1" = "purple", "primary metabolite 1" = "#D8212D")}. If you'd
#'   like help creating a specific gradation of colors, please talk to a member
#'   of the R Working Group about how to do that using
#'   \link{colorRampPalette}.}}
#'
#' @param facet1_column optionally break up the graph into small multiples.
#'   Graphs will already be broken up horizontally by the simulation file name;
#'   this specifies an optional second column to break up the data by
#'   vertically. The designated column name should be unquoted, e.g.,
#'   \code{facet1_column = CompoundID}.
#' @param bar_width width of the bars in hours; we'll try to make a reasonable
#'   size bar here, but you may want to adjust this. Two notes here: \itemize{
#'   
#'   \item{If more than one compound is dosed at the same time, the bar
#' width for each will be \code{bar_width} divided by the number of compounds
#' dosed at that time.}
#'
#' \item{If there's only 1 dose and it's a t = 0, the bar might look funky if
#' the width is very large. If it's, say, 4, the x axis will span -2 h to +2 h,
#' which might not be what you want, so please try adjusting the bar width in
#' that scenario.}}
#'
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
#' dosing_regimen_plot(existing_exp_details = MDZdetails)
#' 
#' dosing_regimen_plot(existing_exp_details = MDZdetails,
#'                     bar_width = 20)
#' 
#' dosing_regimen_plot(existing_exp_details = MDZdetails,
#'                     colorBy_column = CompoundID)
#' 
dosing_regimen_plot <- function(existing_exp_details, 
                                sims_to_include = NA, 
                                colorBy_column, 
                                color_set = NA, 
                                facet1_column, 
                                bar_width = NA){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
           call. = FALSE)
   }
   
   existing_exp_details <- harmonize_details(existing_exp_details)
   
   
   # Main body of function -------------------------------------------------
   
   if(any(complete.cases(sims_to_include))){
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          which_sims = sims_to_include, 
                                          include_or_omit = "include")
   }
   
   if(nrow(existing_exp_details$Dosing) == 0){
      stop(wrapn("We cannot find any dosing information for the simulation you requested, so we cannot make your graph."), 
           call. = FALSE)
   }
   
   Dosing <- existing_exp_details$Dosing
   
   TimeRange <- max(Dosing$Time, na.rm = T) - min(Dosing$Time, na.rm = T)
   
   if(is.na(bar_width)){
      bar_width <- TimeRange / 80
   } 
   
   Xexpand <- (TimeRange + bar_width / 2)/TimeRange - 1
   
   ## Setting things up for nonstandard evaluation ----------------------------
   
   facet1_column <- rlang::enquo(facet1_column)
   colorBy_column <- rlang::enquo(colorBy_column)
   
   if(as_label(facet1_column) != "<empty>"){
      Dosing <- Dosing %>% 
         mutate(FC1 = {{facet1_column}})
   }
   
   if(as_label(colorBy_column) != "<empty>"){
      
      Dosing <- Dosing %>%
         mutate(colorBy_column = {{colorBy_column}})
      
      if(class(Dosing$colorBy_column) == "numeric"){
         Levels <- sort(unique(Dosing$colorBy_column))
         Dosing <- Dosing %>% 
            mutate(colorBy_column = factor(colorBy_column, levels = Levels))
      }
      
      if(all(is.na(color_set)) & 
         as_label(colorBy_column) == "DoseRoute"){
         
         color_set <- c("Oral" = "dodgerblue4", 
                        "i.v. bolus" = "#E41A1C", 
                        "iv. infusion" = "#91429D", 
                        "Dermal" = "seagreen", 
                        "Inhaled" = "#5ECCF3", 
                        "Long-Acting-Injectable" = "orange", 
                        "IntraVaginal" = "#08E6D1", 
                        "Rectal" = "#6F4C29", 
                        "Synovial Joint" = "#E0E006", 
                        "Other site" = "gray20", 
                        "Subcutaneous" = "#F51B7E", 
                        "Custom" = "black")
         
         legend_label_color <- "Dose route"
         
      } else {
         legend_label_color <- as_label(colorBy_column)
      }
      
      NumColorsNeeded <- Dosing %>% 
         pull(colorBy_column) %>% unique() %>% length() 
      
      if(all(is.na(color_set))){
         color_set <- "default"
      }
      
      color_set <- make_color_set(color_set = color_set, 
                                  num_colors = NumColorsNeeded)
      
      G <- ggplot(Dosing, aes(x = Time, xend = Time,
                              y = Dose, 
                              fill = colorBy_column)) +
         geom_bar(stat = "identity", position = "dodge", 
                  width = bar_width) +
         scale_fill_manual(values = color_set) +
         labs(fill = legend_label_color)
      
   } else {
      G <- ggplot(Dosing, aes(x = Time, xend = Time,
                              y = Dose)) +
         geom_bar(stat = "identity", position = "dodge", 
                  width = bar_width)
   }
   
   if(as_label(facet1_column) != "<empty>"){
      G <- G +
         ggh4x::facet_grid2(FC1 ~ File, scales = "free", 
                            axes = "all", switch = "y")
   } else {
      G <- G + 
         facet_grid(. ~ File, scales = "free")
   }
   
   Xlim <- c(min(Dosing$Time, na.rm = T) - bar_width/2, 
             max(Dosing$Time, na.rm = T) + bar_width/2)
   
   
   suppressMessages(
      G <- G + 
         # scale_y_continuous(limits = c(0, max(Dosing$Dose)), 
         #                    expand = expansion(mult = c(0, 0.05))) +
         scale_x_continuous(limits = Xlim) +
         xlab("Time (h)") +
         ylab("Dose (mg)") +
         ggtitle("Dosing regimens") +
         scale_x_time(
            time_range = Xlim, 
            x_breaks = list(from = min(Dosing$Time, na.rm = T), 
                            to = max(Dosing$Time, na.rm = T), 
                            by = "default")) +
         theme_consultancy(border = TRUE) +
         theme(legend.position = "bottom", 
               legend.justification = c(0, 0), 
               strip.placement = "outside")
   )
   
   # FIXME!!!!!
   
   return(G)
   
}



