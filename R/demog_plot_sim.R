#' Make plots for comparing populations across simulations
#'
#' UNDER CONSTRUCTION.
#'
#' @param demog_dataframe the output from running \code{\link{extractDemog}}.
#'   Optionally (and we recommend) with added observed demographic data, perhaps
#'   from observed overlay XML files.
#' @param graph_title title to use on the plots
#' @param demog_parameters demographic parameters to include. We're starting
#'   with a limited set: "Age", "Weight_kg" ("Weight" is also fine), "Height_cm"
#'   ("Height" is fine), "Weight vs Height", "Height vs Age", "Weight vs Age",
#'   "HSA_gL" ("HSA" is fine), "AGP_gL" ("AGP" is fine), "Sex", "Sex vs Age",
#'   "BMI_kgm2" ("BMI" is fine), and "RenalFunction". If you want only a subset
#'   of those, list them in a character vector, e.g., \code{demog_parameters = c("Age",
#'   "Height_cm", "Weight_kg")}. Plots will be in the order listed.
#' @param variability_display How should the variability be shown? Options are
#'   "kernal density" (default, a type of smoothed histogram) or "boxplot". Any
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
#' @param border_facets TRUE (default) or FALSE for whether to include a border
#'   around the facets if the graphs are broken up by the sex of the subjects
#' @param graph_labels TRUE or FALSE for whether to include labels (A, B, C,
#'   etc.) for each of the small graphs.
#' @param colorBy_column the column in \code{demog_dataframe} that should be
#'   used for determining which color lines and/or points will be. This should
#'   be unquoted, e.g., \code{colorBy_column = File}. If left blank, we will
#'   color by the simulation file name.
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
#'
#' @return a set of graphs. This does not yet save the graphs for you, so you'll
#'   need to run ggsave(...) to do that.
#' @export
#'
#' @examples
#' # none yet
demog_plot_sim <- function(demog_dataframe, 
                           variability_display = "kernal density", 
                           colorBy_column, 
                           color_labels = NA, 
                           legend_label_color = NA,
                           color_set = "default", 
                           graph_title = "Demographics", 
                           demog_parameters = NA, 
                           alpha = 0.8, 
                           ncol = NULL, 
                           nrow = NULL, 
                           facet_by_sex = TRUE, 
                           border_facets = TRUE, 
                           graph_labels = TRUE){
   
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
   
   
   # Setting things up for nonstandard evaluation ----------------------------
   
   colorBy_column <- rlang::enquo(colorBy_column)
   
   if(as_label(colorBy_column) == "<empty>"){
      if("File" %in% names(demog_dataframe) == FALSE){
         warning("You have not specified which column in your data you'd like to use for coloring things, and you also don't have a column in your data titled `File`, which is what we use for the default, so there will be only one color in your graphs.\n", 
                 call. = FALSE)
         
         demog_dataframe$File <- "missing file name"
      }
      
      demog_dataframe$colorby_column <- demog_dataframe$File
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
      warning("You have specified something for `color_labels` but nothing for `colorBy_column`. Since R doesn't know which column contains the data to use for your color labels, they will be ignored.\n", 
              call. = FALSE)
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
   
   
   # Returning to error catching ---------------------------------------------
   
   # Addressing any issues w/case and periods for "vs"
   demog_parameters <- tolower(gsub("\\.", "", as.character(demog_parameters)))
   names(demog_dataframe) <- tolower(names(demog_dataframe))
   # Renaming colorBy_column for ease of coding since I'm lifting some of the
   # code from other functions
   demog_dataframe <- demog_dataframe %>% rename(colorBy_column = colorby_column)
   
   demog_parameters <- case_match(demog_parameters, 
                                  "height vs weight" ~ "weight vs height", 
                                  "age vs height" ~ "height vs age", 
                                  "age vs weight" ~ "weight vs age", 
                                  "age vs sex" ~ "sex vs age", 
                                  "weight" ~ "weight_kg",
                                  "height" ~ "height_cm",
                                  "hsa" ~ "hsa_gl",
                                  "agp" ~ "agp_gl",
                                  "bmi" ~ "bmi_kgm2",
                                  .default = demog_parameters)
   
   BadVar <- setdiff(demog_parameters, 
                     tolower(c("Age", 
                               "AGP_gL", 
                               "BMI_kgm2", 
                               "Height_cm", 
                               "Height vs Age", 
                               "HSA_gL",
                               "Weight_kg",
                               "Weight vs Age", 
                               "Weight vs Height",
                               "Sex", 
                               "Sex vs Age", 
                               "RenalFunction")))
   
   if(length(BadVar) > 0 && any(complete.cases(BadVar))){
      warning(paste0("The demographic parameters ", 
                     str_comma(paste0("`", BadVar, "`")), 
                     " are not among the possible options for demog_parameters, so they won't be included. Please check the help file for options.\n"), 
              call. = FALSE)
      
      demog_parameters <- setdiff(demog_parameters, BadVar)
   }
   
   if("sex" %in% names(demog_dataframe) == FALSE){
      demog_dataframe$Sex <- "unknown"
   }
   
   DemogLabs <- c("age" = "Age (years)", 
                  "weight_kg" = "Weight (kg)", 
                  "height_cm" = "Height (cm)", 
                  "hsa_gl" = "HSA (g/L)", 
                  "agp_gl" = "AGP (g/L)", 
                  "sex" = "Sex", 
                  "bmi_kgm2" = "BMI (kg/m2)", 
                  "renalfunction" = "Renal function")
   
   if(all(is.na(demog_parameters))){
      Graphs <- tolower(c("Age", "Weight_kg", "Height_cm", "Weight vs Height",
                          "Height vs Age", "Weight vs Age", "HSA_gL",
                          "AGP_gL", "Sex", "Sex vs Age", "BMI_kgm2", "RenalFunction"))
   } else {
      Graphs <- demog_parameters
   }
   
   
   # Setting up colors -------------------------------------------------------
   NumColorsNeeded <- demog_dataframe %>% 
      pull(colorBy_column) %>% unique() %>% length()
   
   # This is when the user wants specific user-specified colors rather
   # that one of the pre-made sets.
   if(length(color_set) > 1){
      
      # If they supply a named character vector whose values are not
      # present in the data, convert it to an unnamed character vector.
      if(is.null(names(color_set)) == FALSE && 
         all(unique(demog_dataframe$colorBy_column) %in% names(color_set) == FALSE)){
         warning(paste0("You have provided a named character vector of colors, but some or all of the items in the column ", 
                        as_label(colorBy_column),
                        " are not included in the names of the vector. We will not be able to map those colors to their names and will instead assign colors in the alphabetical order of the unique values in ",
                        as_label(colorBy_column), ".\n"), 
                 call. = FALSE)
         
         MyColors <- as.character(color_set)
      } else if(length(color_set) < NumColorsNeeded){
         warning(paste("There are", NumColorsNeeded,
                       "unique values in the column you have specified for the colors, but you have only specified", 
                       length(color_set), 
                       "colors to use. We will recycle the colors to get enough to display your data, but you probably will want to supply more colors and re-graph.\n"), 
                 call. = FALSE)
         
         MyColors <- rep(color_set, 100)[1:NumColorsNeeded]
      } else {
         MyColors <- color_set
      }
      
   } else {
      
      # If there are only 2 groups for the colorBy_column and color_set was set to
      # "default", use Brewer set 1 instead of Brewer set 2 b/c it's more
      # aesthetically pleasing.
      if(NumColorsNeeded <= 2 &
         color_set[1] == "default"){
         color_set <- "Brewer set 1"
      }
      
      # NOTE: For no reason I can discern, if the user has observed
      # data that should be all one color but then uses scale_color_X
      # where x is anything except "manual", the observed points
      # DISAPPEAR. That's why, below, whenever it's scale_color_x, I'm
      # setting the colors needed and then using scale_color_manual
      # instead of scale_color_x. -LSh
      
      color_set <- ifelse(str_detect(tolower(color_set), 
                                     "default|brewer.*2|set.*2"), 
                          "set2", color_set)
      color_set <- ifelse(str_detect(tolower(color_set),
                                     "brewer.*1|set.*1"), 
                          "set1", color_set)
      
      suppressWarnings(
         MyColors <- 
            switch(
               color_set,
               # Using "Dark2" b/c "Set2" is just really,
               # really light.
               "set2" = RColorBrewer::brewer.pal(NumColorsNeeded, "Dark2")[
                  1:NumColorsNeeded], 
               "blue-green" = blueGreens(NumColorsNeeded),
               "blues" = blues(NumColorsNeeded),
               "greens" = chartreuse(NumColorsNeeded, shade = "darker"), 
               "purples" = purples(NumColorsNeeded, shade = "darker"), 
               "rainbow" = rainbow(NumColorsNeeded),
               "set1" = RColorBrewer::brewer.pal(NumColorsNeeded, "Set1")[
                  1:NumColorsNeeded],
               "Tableau" = ggthemes::tableau_color_pal(
                  palette = "Tableau 10")(NumColorsNeeded),
               "viridis" = viridis::viridis_pal()(NumColorsNeeded))
      )
      # NB: For the RColorBrewer palettes, the minimum number of
      # colors you can get is 3. Since sometimes we might only want 1
      # or 2 colors, though, we have to add the [1:NumColorsNeeded]
      # bit.
      
      if(any(is.na(MyColors))){
         warning("The color set you requested does not have enough values for the number of colors required. We're switching the color set to `rainbow` for now.\n", 
                 call. = FALSE)
         
         MyColors <- rainbow(NumColorsNeeded)
      }
      
      # FIXME - Add a check here that the colors supplied are all actually colors.
      
   }
   
   # Graphing -----------------------------------------------------------------
   
   subfun_density <- function(Var){
      names(demog_dataframe)[names(demog_dataframe) == Var] <- "MyVar"
      
      suppressWarnings(
         G <- ggplot(demog_dataframe, aes(x = MyVar, fill = colorBy_column)) +
            geom_density(alpha = 0.5, show.legend = FALSE) +
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
         theme(legend.position = "none", 
               strip.placement = "outside")
      
      return(G)
   }
   
   
   subfun_boxplots <- function(Var){
      names(demog_dataframe)[names(demog_dataframe) == Var] <- "MyVar"
      
      suppressWarnings(
         G <- ggplot(demog_dataframe, aes(y = MyVar, 
                                          x = colorBy_column,  fill = colorBy_column)) +
            geom_boxplot(alpha = 0.5) +
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
         theme(legend.position = "none", 
               strip.placement = "outside")
      
      return(G)
   }
   
   
   MyGraphs <- list()
   
   for(yy in Graphs){
      if(yy == "sex vs age"){
         MyGraphs[[yy]] <- 
            ggplot(demog_dataframe, 
                   aes(x = age, y = colorBy_column, fill = colorBy_column)) +
            facet_grid(sex ~ ., switch = "y") +
            geom_violin(alpha = 0.7, show.legend = FALSE) +
            scale_fill_manual(values = MyColors) +
            ylab("Sex") +
            xlab("Age (years)") +
            theme_consultancy(border = border_facets) + 
            theme(legend.position = "none", 
                  strip.placement = "outside")
         
         if(complete.cases(legend_label_color)){
            MyGraphs[[yy]] <- MyGraphs[[yy]] + labs(fill = legend_label_color)
         } else {
            MyGraphs[[yy]] <- MyGraphs[[yy]] + labs(fill = NULL)
         }
         
         MyGraphs[[yy]] <- patchwork::free(MyGraphs[[yy]])
         
         
      } else if(yy == "sex"){
         
         PercFemale <- demog_dataframe %>% 
            group_by(colorBy_column) %>% 
            summarize(NumF = length(which(sex == "F")), 
                      NumTot = n()) %>% 
            ungroup() %>% 
            mutate(PercFemale = NumF / NumTot)
         
         MyGraphs[[yy]] <-
            ggplot(PercFemale, aes(x = colorBy_column, fill = colorBy_column,
                                   y = PercFemale)) +
            geom_bar(stat = "identity", alpha = 0.7) +
            scale_fill_manual(values = MyColors) +
            scale_y_continuous(labels = scales::percent, 
                               limits = c(0, 1)) +
            ylab("Percent female") +
            xlab(NULL) +
            labs(fill = NULL) +
            theme_consultancy(border = border_facets) + 
            theme(legend.position = "none", 
                  strip.placement = "outside")
         
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
         MyGraphs[[yy]] <- MyGraphs[[yy]] + 
            facet_grid(sex ~ ., switch = "y") +
            theme(strip.placement = "outside")
      }
   }
   
   patchwork::wrap_plots(MyGraphs) +
      patchwork::plot_layout(guides = "collect", 
                             ncol = ncol, 
                             nrow = nrow) + 
      patchwork::plot_annotation(title = graph_title, 
                                 tag_levels = switch(as.character(graph_labels), 
                                                     "TRUE" = "A", 
                                                     "FALSE" = NULL)) & 
      theme(plot.title = element_text(size = 12, 
                                      hjust = 0.5, 
                                      face = "bold"), 
            legend.position = "bottom")
   
}





