#' Make plots for comparing populations across simulations
#'
#' @description \code{demog_plot_sim} will make a series of graphs comparing
#'   parameters across simulated populations. All the parameters available on
#'   the "Demographic Data" tab of a Simcyp Simulator output Excel file are
#'   available for making comparisons, and you must obtain the input data for
#'   making these graphs by running \code{\link{extractDemog}}. If you are
#'   looking at the distributions of one parameter across populations, you can
#'   display that with either boxplots or kernel density plots (like a smoothed
#'   histogram). If you want to compare across simulations the relationship
#'   between pairs of parameters, scatter plots will be used. (Only certain
#'   pairs of parameters are available; please see notes for the argument
#'   'demog_parameters'.)
#'
#' @param demog_dataframe the output from running \code{\link{extractDemog}}.
#' @param sims_to_include optionally specify which simulations to include. These
#'   must be included in \code{demog_dataframe} in the column "File".
#' @param graph_title title to use on the plots
#' @param demog_parameters demographic parameters to include. We're starting
#'   with a limited set: \itemize{\item{Individual parameters, which will be
#'   displayed as either a kernel density plot or a boxplot depending on your
#'   choice for \code{variability_display}: \itemize{
#'
#'   \item{"Age" (age in years)}
#'
#'   \item{"AGP_gL" (alpha-1-acid glycoprotein in g/L; "AGP" is fine)}
#'
#'   \item{"BMI_kgm2" ("BMI" is fine)}
#'
#'   \item{"BrainWt_g" (brain weight; "Brain" is fine)}
#'
#'   \item{"BSA_m2" (body surface area in m2; "BSA" is fine)}
#'
#'   \item{"CardiacOut" (cardiac output in L/h; "Cardiac" is fine)}
#'
#'   \item{"Creatinine_umolL" (creatinine in umol/L; "Creatinine" is fine)}
#'
#'   \item{"GFR_mLminm2" (glomerular filtration rate in mL/min/m2; "GFR" is fine)}
#'
#'   \item{"Haematocrit" (haematocrit)}
#'
#'   \item{"Height_cm" (height in cm; "Height" is fine)}
#'
#'   \item{"HSA_gL" (human serum albumin in g/L; "HSA" is fine)}
#'
#'   \item{"KidneyWt_g" (kidney weight; "Kidney" is fine)}
#'
#'   \item{"LiverWt_g" (liver weight; "Liver" is fine)}
#'
#'   \item{"Sex" (graph shows the percent female by population)}
#'
#'   \item{"Weight_kg" (weight in kg; "Weight" is fine)}
#'
#'   \item{"RenalFunction" (renal function as calculated by the GFR in
#'   mL/min/m squared body surface area divided by the reference GFR for that
#'   sex: 120 for female subjects and 130 for male subjects as of V23 of the
#'   Simcyp Simulator)}}}
#'
#'   \itemize{Comparisons of two parameters, which will create a scatter
#'   plot: \itemize{
#'
#'   \item{"Weight vs Height"}
#'
#'   \item{"Height vs Age"}
#'
#'   \item{"Weight vs Age"}
#'
#'   \item{"Sex vs Age"}}}}
#'
#'   If you want only a subset
#'   of those, list them in a character vector, e.g., \code{demog_parameters = c("Age",
#'   "Height_cm", "Weight_kg")}. Plots will be in the order you list.
#' @param variability_display How should the variability be shown? Options are
#'   "kernel density" (default, a type of smoothed histogram) or "boxplot". Any
#'   demographic parameters requested in the form of "X vs Y", e.g., "weight vs
#'   height", will always be shown as scatter plots.
#' @param alpha how transparent to make the points, with 0 being completely
#'   transparent and invisible so I don't know why you'd want that but, hey, you
#'   do you, to 1, which is fully opaque.
#' @param ncol optionally specify the number of columns. If left as NULL, a
#'   reasonable guess will be used.
#' @param nrow optionally specify the number of rows. If left as NULL, a
#'   reasonable guess will be used.
#' @param facet_by_sex TRUE or FALSE (default) for whether to break up the
#'   graphs into facets based on the sex of the subjects
#' @param facet_column_additional optionally specify an additional column to
#'   facet the graphs by horizontally. If \code{facet_by_sex} is set to TRUE,
#'   the graphs will be broken up vertically by sex.
#' @param border_facets TRUE (default) or FALSE for whether to include a border
#'   around the facets if the graphs are broken up by the sex of the subjects
#' @param graph_labels TRUE or FALSE for whether to include labels (A, B, C,
#'   etc.) for each of the small graphs.
#' @param colorBy_column the column in \code{demog_dataframe} that should be
#'   used for determining which color lines and/or points will be. This should
#'   be unquoted, e.g., \code{colorBy_column = File}. If left blank, we will
#'   color by the simulation file name.
#' @param color_labels optionally specify a character vector for how you'd like
#'   the labels for whatever you choose for \code{colorBy_column} to show up in
#'   the legend. For example, use \code{color_labels = c("file 1.xlsx" =
#'   "healthy subjects", "file 2.xlsx" = "renally impaired subjects")} to
#'   indicate which simulations represent what. The order in the legend will
#'   match the order designated here.
#' @param legend_label_color optionally indicate on the legend something
#'   explanatory about what the colors represent. For example, if
#'   \code{colorBy_column = File} and \code{legend_label_color = "Population"},
#'   that will make the label above the file names in the legend more
#'   explanatory than just "File". The default is to use whatever the column
#'   name is for \code{colorBy_column}. If you don't want a label for this
#'   legend item, set this to "none".
#' @param legend_position specify where you want the legend to be. Options are
#'   "left", "right" (default), "bottom", "top", or "none" if you don't want one
#'   at all. Note: If you include labels on your graphs (graph_labels = TRUE),
#'   we recommend NOT putting the legend on the left or the top because the
#'   labels wind up on the outside compared to the legend, and it just looks
#'   dorky. 
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
#'   \item{"greens"}{a set of greens fading from chartreuse to forest. Like
#'   "blue-green", this palette can be especially useful if you are comparing a
#'   systematic change in some continuous variable.}
#'
#'   \item{"purples"}{a set of purples fading from lavender to aubergine. Like
#'   "blue-green", this palette can be especially useful if you are comparing a
#'   systematic change in some continuous variable.}
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
#' @param return_indiv_graphs TRUE or FALSE (default) for whether to return a
#'   list of each of the individual graphs
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "Demographics comparisons.png". Acceptable graphical
#'   file extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or
#'   "svg". Do not include any slashes, dollar signs, or periods in the file
#'   name. Leaving this as NA means the file will not be automatically saved to
#'   disk.
#' @param fig_height figure height in inches; default is 8
#' @param fig_width figure width in inches; default is 6
#'
#'
#' @return a set of graphs. This does not yet save the graphs for you, so you'll
#'   need to run ggsave(...) to do that.
#' @export
#'
#' @examples
#' # none yet
demog_plot_sim <- function(demog_dataframe, 
                           sims_to_include = "all", 
                           demog_parameters = NA, 
                           variability_display = "kernel density", 
                           colorBy_column, 
                           color_set = "default", 
                           color_labels = NA, 
                           legend_label_color = NA,
                           legend_position = "right", 
                           graph_title = "Demographics", 
                           alpha = 0.8, 
                           ncol = NULL, 
                           nrow = NULL, 
                           facet_by_sex = TRUE, 
                           facet_column_additional, 
                           border_facets = TRUE, 
                           graph_labels = TRUE, 
                           return_indiv_graphs = FALSE, 
                           save_graph = NA,
                           fig_height = 8,
                           fig_width = 6){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   if(facet_by_sex & length(unique(demog_dataframe$Sex)) == 1){
      warning("You requested that we facet the graphs by sex, but there's only one sex in your data. We will not be able to do this.\n", 
              call. = FALSE)
      facet_by_sex = FALSE
   }
   
   legend_position <- tolower(legend_position)[1]
   if(complete.cases(legend_position) && 
      legend_position %in% c("left", "right", "bottom", "top", "none") == FALSE){
      warning(wrapn("You have specified something for the legend position that is not among the possible options. We'll set it to 'bottom', the default."), 
              call. = FALSE)
      legend_position <- "bottom"
   }
   
   # Keeping only requested sims ----------------------------------------------
   
   if(all(sims_to_include == "all") == FALSE){
      demog_dataframe <- filter_sims(demog_dataframe, 
                                     which_sims = sims_to_include, 
                                     include_or_omit = "include")
   }
   
   # Setting things up for nonstandard evaluation ----------------------------
   
   colorBy_column <- rlang::enquo(colorBy_column)
   facet_column_additional <- rlang::enquo(facet_column_additional)
   
   if(as_label(colorBy_column) == "<empty>"){
      if("File" %in% names(demog_dataframe) == FALSE){
         warning("You have not specified which column in your data you'd like to use for coloring things, and you also don't have a column in your data titled `File`, which is what we use for the default, so there will be only one color in your graphs.\n", 
                 call. = FALSE)
         
         demog_dataframe$File <- "missing file name"
      }
      
      demog_dataframe$colorBy_column <- demog_dataframe$File
      
   } else {
      if(class(demog_dataframe %>% pull(!!colorBy_column)) == "numeric"){
         
         Levels <- sort(unique(demog_dataframe %>% pull(!!colorBy_column)))
         demog_dataframe <- demog_dataframe %>% 
            mutate(colorBy_column = factor({{colorBy_column}}, levels = Levels))
         
      } else if(class(demog_dataframe %>% pull(!!colorBy_column)) == "factor"){
         
         Levels <- levels(demog_dataframe %>% pull(!!colorBy_column))
         demog_dataframe <- demog_dataframe %>% 
            mutate(colorBy_column = factor({{colorBy_column}}, levels = Levels))
      } else {
         demog_dataframe <- demog_dataframe %>% 
            mutate(colorBy_column = {{colorBy_column}})
      }
   }
   
   # If user filled in color_labels but not colorBy_column, give a warning.
   if(as_label(colorBy_column) == "<empty>" & any(complete.cases(color_labels))){
      if("colorBy_column" %in% names(demog_dataframe)){
         warning(wrapn("You have specified something for the argument 'color_labels' but nothing for the argument 'colorBy_column'. We think you want to color things by the simulation file name, so that's what we're using to assign colors. This might not work out great if that's a bad assumption, in which case, please assign a column in your data to the argument 'colorBy_column'."), 
                 call. = FALSE) 
      } else {
         warning(wrapn("You have specified something for the argument 'color_labels' but nothing for the argument 'colorBy_column'. Since we don't know which column contains the data to use for your color labels, the labels will be ignored."), 
                 call. = FALSE) 
         
      }
   }
   
   # If there are any replicate names for color_labels, give a warning.
   if(any(duplicated(names(color_labels)))){
      warning(paste0("You have listed this file more than once for the argument `color_labels`:\n", names(color_labels[duplicated(names(color_labels))]), 
                     "\nand we can only work with unique values here. We won't be able to use anything for `color_labels`. Please check your input."), 
              call. = FALSE)
      color_labels <- NA
   }
   
   # If the color labels don't match the items in the colorBy_column, give a
   # warning.
   if(as_label(colorBy_column) != "<empty>" && 
      any(complete.cases(color_labels)) && 
      all(names(color_labels) %in% sort(t(unique(
         demog_dataframe[, as_label(colorBy_column)])))) == FALSE){
      
      BadLabs <- setdiff(names(color_labels), 
                         sort(t(unique(demog_dataframe[, as_label(colorBy_column)]))))
      
      warning(paste0("The labels you supplied for `color_labels` are not all present in the column ", 
                     as_label(colorBy_column), 
                     ". This will mess up the colors and the legend labels on your graph unless that's fixed. Specifically, the following values are not present in the column ",
                     as_label(colorBy_column), ":\n     ", 
                     str_comma(BadLabs)), 
              call. = FALSE)
      
      WarningLabel <- paste0("WARNING: There's a mismatch between\nthe label given and the items included in\nthe column used for setting the color.", 
                             gsub(" - problem no. 1", "", 
                                  paste(" - problem no.", 
                                        1:length(unique(demog_dataframe[, as_label(colorBy_column)])))))
      
      color_labels[which(names(color_labels) %in% BadLabs)] <-
         WarningLabel[1:length(BadLabs)]
      
      NewNames <- setdiff(sort(t(unique(demog_dataframe[, as_label(colorBy_column)]))),
                          names(color_labels))
      
      if(length(NewNames) == 0){
         # This happens when the file they named just is not present.
         color_labels <- color_labels[names(color_labels) %in%
                                         sort(unique(demog_dataframe[, as_label(colorBy_column)]))]
      } else {
         # This happens when the file they named was probably misspelled.
         NewNames <- NewNames[complete.cases(NewNames)]
         names(color_labels)[which(names(color_labels) %in% BadLabs)] <- NewNames
      }
      rm(NewNames, BadLabs, WarningLabel)
   }
   
   # Setting factors for color_labels. 
   if(any(complete.cases(color_labels))){
      demogcheck <- demog_dataframe %>% 
         filter(colorBy_column %in% names(color_labels)) %>% 
         select(colorBy_column) %>% unique() %>% pull()
      
      if(length(sort(unique(demogcheck))) > 
         length(color_labels[names(color_labels) %in% demog_dataframe$colorBy_column])){
         warning(paste0("You have not included enough labels for the colors in the legend. The values in '",
                        as_label(colorBy_column), 
                        "' will be used as labels instead.\n"),
                 call. = FALSE)
         color_labels <- NA
      } else {
         if(length(color_labels[names(color_labels) %in% demog_dataframe$colorBy_column]) == 0 |
            length(sort(unique(demogcheck))) == 0){
            warning(paste0("There is some kind of mismatch between the color labels provided and the values actually present in ",
                           as_label(colorBy_column), ". The specified labels cannot be used.\n"),
                    call. = FALSE)  
         } else {
            
            MissingLabels <- setdiff(unique(demog_dataframe$colorBy_column), 
                                     names(color_labels))
            if(length(MissingLabels) > 0){
               names(MissingLabels) <- MissingLabels
               color_labels <- c(color_labels, MissingLabels)
            }
            
            demog_dataframe <- demog_dataframe %>% 
               mutate(colorBy_column = color_labels[colorBy_column], 
                      colorBy_column = factor(colorBy_column, levels = color_labels))
            
         }
      }
   }
   
   # Faceting
   if(as_label(facet_column_additional) != "<empty>"){
      demog_dataframe <- demog_dataframe %>% 
         mutate(FC = !!facet_column_additional)
   }
   
   
   # Possible demographic parameters -----------------------------------------
   
   names(demog_dataframe) <- tolower(names(demog_dataframe))
   # Renaming colorBy_column for ease of coding since I'm lifting some of the
   # code from other functions. This is just fixing the case since I just made
   # everything lower.
   demog_dataframe <- demog_dataframe %>% 
      rename(colorBy_column = colorby_column)
   
   Harmonized <- harmonize_demog(demog_dataframe = demog_dataframe, 
                                 demog_parameters = demog_parameters)
   DemogParams <- Harmonized$DemogParams
   PossDemogParams <- Harmonized$PossDemogParams
   
   DemogLabs <- PossDemogParams$Label
   names(DemogLabs) <- tolower(PossDemogParams$Parameter)
   
   
   # Setting up colors -------------------------------------------------------
   NumColorsNeeded <- demog_dataframe %>% 
      pull(colorBy_column) %>% unique() %>% length()
   
   MyColors <- make_color_set(color_set = color_set, 
                              num_colors = NumColorsNeeded)
   
   # NOTE: For no reason I can discern, if the user has observed
   # data that should be all one color but then uses scale_color_X
   # where x is anything except "manual", the observed points
   # DISAPPEAR. That's why, below, whenever it's scale_color_x, I'm
   # setting the colors needed and then using scale_color_manual
   # instead of scale_color_x. -LSh
   
   # If there are only single variables being plotted (only boxplots or density
   # plots), then the legend is fine. If there is a mix of single variables and
   # then also scatter plots comparing 2 variables, then the legend just WILL
   # NOT work: It duplicates the fill legend no matter what I do. To work around
   # this, check for whether there are ANY scatter plots and, if there are, omit
   # the legend for the single variable graphs.
   AnyScatter <- any(str_detect(DemogParams$Parameter, "vs"))
   
   
   # Graphing -----------------------------------------------------------------
   
   # # Setting up function for making the glyph be a filled rectangle no matter
   # # what. Credit for this:
   # # https://stackoverflow.com/questions/76872072/change-legend-geom-with-override-aes-or-key-glyph
   # 
   # "%||%" <- function(a, b) {
   #    if (!is.null(a)) a else b
   # }
   # 
   # draw_key_cust <- function(data, params, size) {
   #    # make fill inherit color if NA
   #    if (is.na(data$fill)) data$fill <- data$col
   #    
   #    grid::rectGrob(
   #       gp = grid::gpar(
   #          col = NA, fill = alpha(data$fill %||%
   #                                    data$colour %||% "grey20", data$alpha),
   #          lty = data$linetype %||%
   #             1
   #       )
   #    )
   # }
   
   subfun_density <- function(Var){
      names(demog_dataframe)[names(demog_dataframe) == Var] <- "MyVar"
      
      suppressWarnings(
         G <- ggplot(demog_dataframe, aes(x = MyVar, fill = colorBy_column)) +
            geom_density(alpha = 0.5) +
            guides(fill = guide_legend(override.aes = 
                                          list(shape = 15,
                                               size = 6, 
                                               color = NA,
                                               alpha = 1)), 
                   color = "none") + 
            ylab("Distribution") +
            xlab(DemogLabs[Var])
      )
      
      if(complete.cases(legend_label_color)){
         G <- G + labs(fill = legend_label_color)
      } else {
         G <- G + labs(fill = NULL)
      }
      
      G <- G +
         scale_fill_manual(values = MyColors) +
         theme_consultancy(border = border_facets) + 
         theme(strip.placement = "outside", 
               legend.position = ifelse(AnyScatter, "none", legend_position))
      
      return(G)
   }
   
   
   subfun_boxplots <- function(Var){
      names(demog_dataframe)[names(demog_dataframe) == Var] <- "MyVar"
      
      suppressWarnings(
         G <- ggplot(demog_dataframe, aes(y = MyVar, 
                                          x = colorBy_column,  fill = colorBy_column)) +
            geom_boxplot(alpha = 0.8) +
            guides(fill = guide_legend(override.aes = 
                                          list(shape = 15,
                                               size = 6, 
                                               color = NA,
                                               alpha = 1)), 
                   color = "none") + 
            ylab(DemogLabs[Var]) +
            xlab(NULL)
      )
      
      if(complete.cases(legend_label_color)){
         G <- G + labs(fill = legend_label_color)
      } else {
         G <- G + labs(fill = NULL)
      }
      
      G <- G +
         scale_fill_manual(values = MyColors) +
         theme_consultancy(border = border_facets) + 
         theme(strip.placement = "outside", 
               legend.position = ifelse(AnyScatter, "none", legend_position))
      
      return(G)
   }
   
   
   MyGraphs <- list()
   MissingParams <- as.character(c())
   
   for(yy in DemogParams$Parameter){
      
      if(yy %in% names(demog_dataframe) == FALSE){
         MissingParams <- c(MissingParams, yy)
         next
      }
      
      if(yy %in% c(names(demog_dataframe), 
                   tolower(PossDemogParams$Parameter)) == FALSE & 
         all(is.na(DemogParams$Orig)) == FALSE){
         MissingParams <- c(MissingParams, 
                            DemogParams$Orig[DemogParams$Parameter == yy])
         next
      }
      
      if(all(is.na(demog_dataframe[, yy]))){
         next
      }
      
      if(all(is.na(DemogParams$Orig)) & 
         yy == "allometricscalar" & 
         "allometricscalar" %in% names(demog_dataframe) &&
         all(demog_dataframe$allometricscalar == 1, na.rm = T)){
         next
      }
      
      if(yy == "sex vs age"){
         MyGraphs[[yy]] <- 
            ggplot(demog_dataframe, 
                   aes(x = age, y = colorBy_column, fill = colorBy_column)) +
            facet_grid(sex ~ ., switch = "y") +
            geom_violin(alpha = 0.7) +
            scale_fill_manual(values = MyColors) +
            guides(fill = guide_legend(override.aes = 
                                          list(shape = 15,
                                               size = 6, 
                                               color = NA,
                                               alpha = 1)), 
                   color = "none") + 
            ylab("Sex") +
            xlab("Age (years)") +
            theme_consultancy(border = border_facets) + 
            theme(strip.placement = "outside", 
                  legend.position = ifelse(AnyScatter, "none", legend_position))
         
         if(complete.cases(legend_label_color)){
            MyGraphs[[yy]] <- MyGraphs[[yy]] + labs(fill = legend_label_color)
         } else {
            MyGraphs[[yy]] <- MyGraphs[[yy]] + labs(fill = NULL)
         }
         
         # MyGraphs[[yy]] <- patchwork::free(MyGraphs[[yy]])
         
         
      } else if(yy == "sex"){
         
         PercFemale <- demog_dataframe %>% 
            group_by(across(.cols = any_of(c("colorBy_column", "fc")))) %>% 
            summarize(NumF = length(which(sex == "F")), 
                      NumTot = n()) %>% 
            ungroup() %>% 
            mutate(PercFemale = NumF / NumTot)
         
         MyGraphs[[yy]] <-
            ggplot(PercFemale, aes(x = colorBy_column, fill = colorBy_column,
                                   y = PercFemale)) +
            geom_bar(stat = "identity", alpha = 0.7) +
            guides(fill = guide_legend(override.aes = 
                                          list(shape = 15, 
                                               size = 6, 
                                               color = NA, 
                                               alpha = 1)), 
                   color = "none") + 
            scale_fill_manual(values = MyColors) +
            scale_y_continuous(labels = scales::percent, 
                               limits = c(0, 1)) +
            ylab("Percent female") +
            xlab(NULL) +
            labs(fill = NULL) +
            theme_consultancy(border = border_facets) + 
            theme(strip.placement = "outside", 
                  legend.position = ifelse(AnyScatter, "none", legend_position))
         
         if(complete.cases(legend_label_color)){
            MyGraphs[[yy]] <- MyGraphs[[yy]] + labs(fill = legend_label_color)
         } else {
            MyGraphs[[yy]] <- MyGraphs[[yy]] + labs(fill = NULL)
         }
         
      } else if(yy %in% tolower(c("Weight vs Height", 
                                  "Height vs Age", 
                                  "Weight vs Age"))){
         MyGraphs[[yy]] <- 
            ggplot(
               demog_dataframe, 
               switch(yy, 
                      "weight vs height" = aes(y = weight_kg, x = height_cm,
                                               shape = sex, 
                                               color = colorBy_column), 
                      "height vs age" = aes(y = height_cm, x = age, shape = sex, 
                                            color = colorBy_column), 
                      "weight vs age" = aes(y = weight_kg, x = age, shape = sex, 
                                            color = colorBy_column))) +
            geom_point(alpha = alpha) + 
            scale_color_manual(values = MyColors) +
            labs(color = NULL) +
            guides(color = guide_legend(override.aes = 
                                           list(shape = 15, 
                                                size = 6, 
                                                fill = NA, 
                                                alpha = 1)), 
                   shape = guide_legend(override.aes = list())) + 
            ylab(case_match(yy, 
                            "weight vs height" ~ "Weight (kg)", 
                            "height vs age" ~ "Height (cm)", 
                            "weight vs age" ~ "Weight (kg)")) +
            xlab(case_match(yy, 
                            "weight vs height" ~ "Height (cm)", 
                            "height vs age" ~ "Age (years)", 
                            "weight vs age" ~ "Age (years)")) +
            theme_consultancy(border = border_facets)
         
         if(length(unique(demog_dataframe$sex)) == 1){
            MyGraphs[[yy]] <- MyGraphs[[yy]] + guides(shape = "none") 
         }
         
      } else {
         
         if(str_detect(tolower(variability_display), "density")){
            MyGraphs[[yy]] <- subfun_density(yy)
         } else {
            MyGraphs[[yy]] <- subfun_boxplots(yy)
         }
         
         if(length(unique(demog_dataframe$sex)) == 1){
            MyGraphs[[yy]] <- MyGraphs[[yy]] + guides(shape = "none") 
         }
      }
      
      if(facet_by_sex & yy != "sex"){
         if(as_label(facet_column_additional) != "<empty>"){
            MyGraphs[[yy]] <- MyGraphs[[yy]] + 
               facet_grid(sex ~ fc, switch = "y") +
               theme(strip.placement = "outside")
            
         } else {
            MyGraphs[[yy]] <- MyGraphs[[yy]] + 
               facet_grid(sex ~ ., switch = "y") +
               theme(strip.placement = "outside")
            
         }
      } else {
         if(as_label(facet_column_additional) != "<empty>"){
            MyGraphs[[yy]] <- MyGraphs[[yy]] + 
               facet_grid(. ~ fc, switch = "y") +
               theme(strip.placement = "outside")
         }  
      }
   }
   
   if(length(MissingParams) > 0 & all(complete.cases(DemogParams$Orig))){
      warning(wrapn(paste0(
         "The variable(s) ", 
         str_comma(paste0("'", MissingParams, "'")), 
         " is/are not included in what you have supplied for demog_dataframe, so we will not be able to make that graph.")),
         call. = FALSE)
      
   }
   
   
   # Printing collected graphs -----------------------------------------------
   
   # NB: theme(legend.position... must be inside plot_annotation or ALL of the
   # possible legends will show up.
   
   G <- patchwork::wrap_plots(MyGraphs) +
      patchwork::plot_layout(
         guides = "collect",
         ncol = ncol,
         nrow = nrow) +
      patchwork::plot_annotation(
         title = graph_title,
         tag_levels = switch(as.character(graph_labels),
                             "TRUE" = "A",
                             "FALSE" = NULL), 
         theme = theme(legend.position = legend_position)) &
      theme(plot.title = element_text(size = 12,
                                      hjust = 0.5,
                                      face = "bold"),
            legend.box = case_when(
               legend_position %in% c("left", "right") ~ "vertical", 
               legend_position %in% c("top", "bottom") ~ "horizontal"), 
            legend.justification = c(0.5, 0.5))
   
   
   # Saving ----------------------------------------------------------------
   
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
         Ext <- "png"
      }
      
      # This is when they want any kind of graphical file format.
      ggsave(FileName, height = fig_height, width = fig_width, dpi = 600, 
             plot = G)
      
   }
   
   if(return_indiv_graphs){
      return(MyGraphs)
   } else {
      return(G)
   }
   
}





