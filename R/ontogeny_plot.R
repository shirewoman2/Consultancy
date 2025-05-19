#' Graph the ontogenies of drug-metabolizing enzymes and transporters
#'
#' @description \code{ontogeny_plot} uses the betas from the Simcyp Simulator to
#'   make graphs of the ontogenies of all the enzymes and transporters included
#'   in the Simulator.
#'
#' @param enzyme drug-metabolizing enzymes and transporters to plot, which
#'   should be supplied as either a single string, in quotes, that we'll try to
#'   match (e.g., "CYP3A4" will get you both the Upreti and Salem CYP3A4
#'   ontogeny profiles as well as "CYP3A4 in vitro" and "Intestinal CYP3A4") or
#'   as a character vector of the enzymes or transporters you want. Options are
#'   listed in the data.frame OntogenyEquations. Whatever you supply, we'll look
#'   for it in either the column "Enzyme", which is just the actual isoform
#'   name, or in the column EnzymeDescription, which specifies exactly which
#'   ontogeny profile it is. To see the object OntogenyEquations and get a
#'   better idea of what we mean, type this into the console:
#'   \code{view(OntogenyEquations)}
#' @param enzyme_type type of enzyme or transporter. Instead of graphing only a
#'   specific enzyme or enzymes, you can request any enzymes or transporters of
#'   a specific type. Please see the options in "OntogenyEquations" in the
#'   column "EnzymeType". This should be supplied as a character vector, e.g.,
#'   \code{enzyme_type = c("transporters", "UGTs")}
#' @param ontogeny_equations_to_use By default, the ontogeny equations are from
#'   the object OntogenyEquations, which is from the Simcyp Simulator help file.
#'   This can be useful for more-advanced R users if, for example, you want to
#'   filter OntogenyEquations more flexibly than just listing a string or
#'   character vector of enzymes to match for the arguments \code{enzyme} and
#'   \code{enzyme_type} or if you want to supply your own set of equations to
#'   describe the profiles as long as the data.frame is set up like
#'   OntogenyEquations. If you supply something here, the arguments
#'   \code{enzyme} and \code{enzyme_type} will be ignored.
#' @param simulator_version Simcyp Simulator version to display. Options are 21,
#'   22, 23, or 24 (default).
#' @param compare_to_no_ontogeny TRUE or FALSE (default) for whether to show a
#'   line on the graph for no ontogeny, which would be a horizontal line at 1.
#' @param age_range age range in years as a numeric vector. Default is
#'   \code{age_range = c(0, 18)}, which will include ages 0 to 18 years in the
#'   graph.
#' @param x_axis_interval optionally specify the x-axis interval to use. Default
#'   is an interval of 3 years.
#' @param facet1_column optionally break up the graph into small multiples; this
#'   specifies the first of up to two columns to break up the data by, and the
#'   designated column name should be unquoted, e.g., \code{facet1_column =
#'   Tissue}. If \code{floating_facet_scale} is FALSE and you haven't specified
#'   \code{facet_ncol} or  \code{facet_nrow}, then \code{facet1_column} will
#'   designate the rows of the output graphs.
#' @param facet1_title optionally specify a title to describe facet 1. This is
#'   ignored if \code{floating_facet_scale} is TRUE or if you have specified
#'   \code{facet_ncol} or \code{facet_nrow}.
#' @param facet2_title optionally specify a title to describe facet 2. This is
#'   ignored if \code{floating_facet_scale} is TRUE or if you have specified
#'   \code{facet_ncol} or \code{facet_nrow}.
#' @param facet2_column optionally break up the graph into small multiples; this
#'   specifies the second of up to two columns to break up the data by, and the
#'   designated column name should be unquoted, e.g., \code{facet2_column =
#'   CompoundID}. If \code{floating_facet_scale} is FALSE and you haven't
#'   specified \code{facet_ncol} or  \code{facet_nrow}, then
#'   \code{facet2_column} will designate the columns of the output graphs.
#' @param colorBy_column (optional) the column in \code{OntogenyEquations} that
#'   should be used for determining which color lines should be. This should be
#'   unquoted, e.g., \code{colorBy_column = Tissue}.
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
#'   named vector. For example, if you're coloring the lines by the enzyme,
#'   you could do this: \code{color_set = c("CYP3A4" = "dodgerblue3",
#'   "CYP3A5" = "purple", "CYP3A7" = "#D8212D")}. If you'd
#'   like help creating a specific gradation of colors, please talk to a member
#'   of the R Working Group about how to do that using
#'   \link{colorRampPalette}.}}
#'
#' @param graph_title optionally specify a title that will be centered across
#'   your graph or set of graphs
#' @param graph_title_size the font size for the graph title if it's included;
#'   default is 14. This also determines the font size of the graph labels.
#' @param legend_position Specify where you want the legend to be. Options are
#'   "left", "right" (default in most scenarios), "bottom", "top", or "none" if
#'   you don't want one at all.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My ontogeny graph.png"If you leave off ".png", the
#'   graph will be saved as a png file, but if you specify a different graphical
#'   file extension, it will be saved as that file format. Acceptable graphical
#'   file extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or
#'   "svg". Do not include any slashes, dollar signs, or periods in the file
#'   name. Leaving this as NA means the file will not be saved to disk.
#' @param fig_height figure height in inches
#' @param fig_width figure width in inches
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
#' # If there's only one possible profile, using
#' the ontogeny_plot function is straightforward.
#' ontogeny_plot(enzyme = "UGT1A4")
#' 
#' # If there are multiple profiles, you'll be asked
#' # which you would like interactively. 
#' ontogeny_plot(enzyme = "CYP3A4")
#' 
#' # If you don't want to be asked, you can turn this off.
#' ontogeny_plot(enzyme = "CYP3A4",
#'               ask_if_multiple_enzymes = F)
#' # ...but you might want to color the lines by the 
#' # enzyme description because otherwise, you won't
#' # know which is which. 
#' ontogeny_plot(enzyme = "CYP3A4",
#'               ask_if_multiple_enzymes = F, 
#'               colorBy_column = EnzymeDescription)
#' 
#' # You can change the colors
#' ontogeny_plot(enzyme = "CYP3A4",
#'               ask_if_multiple_enzymes = F, 
#'               colorBy_column = EnzymeDescription, 
#'               color_set = "viridis")
#' 
#' # You can use regular expressions to get multiple 
#' # possible enzymes. 
#' ontogeny_plot(enzyme = "CYP|UGT", 
#'               ask_if_multiple_enzymes = F, 
#'               colorBy_column = EnzymeDescription, 
#'               facet1_column = EnzymeType)
#' 
#' # Any ontogeny profile included in the Simcyp Simulator
#' # may be plotted. 
#' ontogeny_plot(enzyme = "P-gp", 
#'               colorBy_column = EnzymeDescription)
#' 
#' # To see what the possibilities are and the equations 
#' # being used:
#' view(OntogenyEquations)
#'
#' 
ontogeny_plot <- function(enzyme = NA, 
                          enzyme_type = NA, 
                          ask_if_multiple_enzymes = TRUE, 
                          ontogeny_equations_to_use = NA, 
                          simulator_version = 24, 
                          compare_to_no_ontogeny = FALSE, 
                          age_range = c(0, 18), 
                          x_axis_interval = 3, 
                          facet1_column,
                          facet1_title = NA, 
                          facet2_column,
                          facet2_title = NA, 
                          colorBy_column, 
                          color_set = "default", 
                          legend_position = "right",
                          graph_title = NA,
                          graph_title_size = 14, 
                          save_graph = NA,
                          fig_height = NA,
                          fig_width = NA){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
           call. = FALSE)
   }
   
   if(length(enzyme) > 1){
      enzyme <- str_c(enzyme, collapse = "|")
   }
   # Remove parentheses
   enzyme <- gsub("\\(|\\)", ".", enzyme)
   
   
   if(length(enzyme_type) > 1){
      enzyme_type <- str_c(enzyme_type, collapse = "|")
   }
   
   # Remove parentheses
   enzyme_type <- gsub("\\(|\\)", ".", enzyme_type)
   
   
   # Setting up data ----------------------------------------------------------
   
   if("data.frame" %in% class(ontogeny_equations_to_use)){
      Ontogenies <- ontogeny_equations_to_use
   } else {
      
      Ontogenies <- OntogenyEquations %>% 
         filter(switch(as.character(simulator_version), 
                       "21" = V21 == TRUE, 
                       "22" = V22 == TRUE, 
                       "23" = V23 == TRUE, 
                       "24" = V24 == TRUE)) 
      
      if(any(complete.cases(enzyme))){
         Ontogenies <- Ontogenies %>% 
            filter(str_detect(tolower(EnzymeDescription), tolower({{enzyme}})) |
                      str_detect(tolower(Enzyme), tolower({{enzyme}})))
      }
      
      if(any(complete.cases(enzyme_type))){
         Ontogenies <- Ontogenies %>% 
            filter(str_detect(tolower(EnzymeType), tolower({{enzyme_type}})))
      }
   }
   
   # This object is easiest to grab here, but there will be a message about this
   # and possibly an interactive input lower down in function.
   EnzymesIncluded <- Ontogenies %>% select(EnzymeType, Enzyme, EnzymeDescription)
   
   Ontogenies <- Ontogenies %>% 
      left_join(expand_grid(Age = seq(0, 25, length.out = 1000), 
                            EnzymeDescription = OntogenyEquations$EnzymeDescription),
                by = "EnzymeDescription") %>% 
      mutate(AgeGroup = case_when(Age <= Age_cap1 ~ 1, 
                                  Age > Age_cap1 & Age <= Age_cap2 ~ 2, 
                                  Age > Age_cap2 ~ 3),
             
             Fraction = case_when(
                
                # no ontogeny
                is.na(Age50) & is.na(C0) ~ 1, 
                
                # sigmoidal below Age_cap1
                complete.cases(Age_cap1) & 
                   Age <= Age_cap1 & 
                   complete.cases(Fmax) ~ 
                   Fbirth + ( (Fmax - Fbirth) * Age^n) / (Age50^n + Age^n),
                
                # exponential between Age_cap1 & 2
                (Age > Age_cap1 | is.na(Age_cap1)) & 
                   Age <= Age_cap2 & complete.cases(C2) ~
                   C0 + C1*exp(C2*(Age - Age_cap1 - C3)), 
                
                # linear between Age_cap1 & 2
                (Age > Age_cap1 | is.na(Age_cap1)) &
                   Age <= Age_cap2 & is.na(C2) ~
                   C0 + C1 * Age, 
                
                # same as adult above Age_cap1
                Age > Age_cap1 & is.na(Age_cap2) ~ 1,
                
                # same as adult above Age_cap2
                Age > Age_cap2 ~ 1), 
             
             Fraction = case_when(str_detect(EnzymeDescription, "Upreti") &
                                     !str_detect(EnzymeDescription, "modified") &
                                     Age > 10 ~ NA,
                                  .default = Fraction),
             
             Age_mo = Age * 12)  
   
   if(compare_to_no_ontogeny){
      Ontogenies <- Ontogenies %>% 
         bind_rows(data.frame(Age = c(0, 18), 
                              Enzyme = "No ontogeny", 
                              EnzymeDescription = "No ontogeny", 
                              Fraction = 1))
   }
   
   
   # Setting up for NSE ------------------------------------------------------
   
   facet1_column <- rlang::enquo(facet1_column)
   facet2_column <- rlang::enquo(facet2_column)
   colorBy_column <- rlang::enquo(colorBy_column)
   
   if(as_label(colorBy_column) != "<empty>"){
      Ontogenies <- Ontogenies %>%
         mutate(colorBy_column = {{colorBy_column}})
      
      if(class(Ontogenies$colorBy_column) == "numeric"){
         Levels <- sort(unique(Ontogenies$colorBy_column))
         Ontogenies <- Ontogenies %>%
            mutate(colorBy_column = factor(colorBy_column, levels = Levels))
      }
   }
   
   if(as_label(facet1_column) != "<empty>"){
      
      Ontogenies <- Ontogenies %>%
         mutate(FC1 = {{facet1_column}})
      
      if(length(unique(Ontogenies$FC1)) == 1){
         warning(paste0("You requested the column `",
                        as_label(facet1_column),
                        "` for facet1_column, but that column contains only 1 unique value. Are you sure that's what you want?"),
                 call. = FALSE)
      }
   }
   
   if(as_label(facet2_column) != "<empty>"){
      Ontogenies <- Ontogenies %>%
         mutate(FC2 = {{facet2_column}})
      
      if(length(unique(Ontogenies$FC2)) == 1){
         warning(paste0("You requested the column `",
                        as_label(facet2_column),
                        "` for facet2_column, but that column contains only 1 unique value. Are you sure that's what you want?"),
                 call. = FALSE)
      }
   }
   
   # If the user wants to title their facets, check whether ggh4x is installed
   # and ask user if they want to install it if not.
   if(any(c(complete.cases(facet1_title), complete.cases(facet2_title))) & 
      length(find.package("ggh4x", quiet = TRUE)) == 0){
      message(paste0("\n", wrapn("You requested a title for facet1 or facet 2. Adding a title to facets requires the package ggh4x.")))
      Install <- readline(prompt = "Is it ok to install ggh4x for you? (y or n)   ")
      
      if(tolower(str_sub(Install, 1, 1)) == "y"){
         install.packages("ggh4x")
      } else {
         message("Ok, we will not install ggh4x for you, but we also won't be able to add facet titles to your graph.\n")
         facet1_title <- NA
         facet2_title <- NA
      }
   }
   
   # Adjusting input data.frame for facet titles
   if(complete.cases(facet1_title)){
      Ontogenies$Facet1Title <- facet1_title
   }
   
   if(complete.cases(facet2_title)){
      Ontogenies$Facet2Title <- facet2_title
   }
   
   if(as_label(colorBy_column) != "<empty>"){
      Ontogenies <- Ontogenies %>% 
         mutate(colorBy_column = !!colorBy_column) # wait... should this be !! or {}?
   }
   
   Ontogenies <- Ontogenies %>% 
      unite(col = Group, Enzyme, EnzymeDescription, EnzymeType, Tissue, remove = FALSE)
   
   # Checking that we're including what the user expects
   message("Enzymes to be included in the graph: ")
   message(paste(paste(capture.output(as.data.frame(EnzymesIncluded)), collapse = "\n"), 
                 "\n"))
   
   if(nrow(EnzymesIncluded) > 1 & ask_if_multiple_enzymes == TRUE){
      
      message(str_wrap("\nWhich rows in the data.frame do you want? Please use a numeric vector such as 'c(1, 3, 4:5)' to specify or say 'all' for all rows."))
      WhichRows <- readline("   ")
      message("\n")
      
      if(tolower(WhichRows[1]) != "all"){
         WhichRows <- eval(str2expression(WhichRows))
         if(any(as.numeric(WhichRows) == FALSE)){
            warning(wrapn("You provided input that was not 'all' nor was it numeric, so we don't know which rows of enzymes you want. We'll give you all of them."), 
                    call. = FALSE)
         } else {
            EnzymesIncluded <- EnzymesIncluded %>% slice(WhichRows)
            
            Ontogenies <- Ontogenies %>% 
               filter(Enzyme %in% EnzymesIncluded$Enzyme & 
                         EnzymeType %in% EnzymesIncluded$EnzymeType & 
                         EnzymeDescription %in% EnzymesIncluded$EnzymeDescription)
         }
      }
   }
   
   
   # Graphing -----------------------------------------------------------------
   
   A <- ggplot(Ontogenies, 
               switch(as.character(as_label(colorBy_column) != "<empty>"), 
                      "TRUE" = aes(x = Age, y = Fraction, 
                                   color = colorBy_column, group = Group), 
                      "FALSE" = aes(x = Age, y = Fraction, group = Group))) +
      geom_line(linewidth = 1) +
      labs(color = NULL) +
      xlab("Age (years)") +
      ylab("Fraction of adult expression") +
      # scale_y_continuous(limits = c(0, 2), 
      #                    breaks = seq(0, 2, 0.5), 
      #                    expand = c(0, 0)) +
      scale_x_continuous(limits = age_range, 
                         breaks = seq(age_range[1], age_range[2], 
                                      x_axis_interval))
   
   ## Dealing with possible facets ------------------------------------------
   
   # Here are the options for faceting: 
   FacetOpts <- paste(ifelse(as_label(facet1_column) == "<empty>", 
                             "NoFC1", 
                             ifelse(complete.cases(facet1_title), 
                                    "FC1PlusTitle", "FC1")),
                      ifelse(as_label(facet2_column) == "<empty>", 
                             "NoFC2", 
                             ifelse(complete.cases(facet2_title), 
                                    "FC2PlusTitle", "FC2")))
   # If there are no facet columns or if there are just no titles for those
   # columns, those scenarios all work the same.
   FacetOpts <- ifelse(FacetOpts %in% c("NoFC1 NoFC2", "FC1 FC2", 
                                        "NoFC1 FC2", "FC1 NoFC2"), 
                       "ggplot2 facets", FacetOpts)
   
   
   # Setting up theme for facet titles
   FacetTitleTheme_XY <- ggh4x::strip_nested(
      text_x = ggh4x::elem_list_text(face = c("bold", "plain"), 
                                     size = c(1.25 * calc_element("strip.text.x", theme_consultancy())$size,
                                              calc_element("strip.text.x", theme_consultancy())$size)), 
      by_layer_x = TRUE, 
      
      text_y = ggh4x::elem_list_text(face = c("bold", "plain"), 
                                     size = c(1.25 * calc_element("strip.text.x", theme_consultancy())$size,
                                              calc_element("strip.text.x", theme_consultancy())$size)), 
      by_layer_y = TRUE)
   
   FacetTitleTheme_Y <- ggh4x::strip_nested(
      text_x = ggh4x::elem_list_text(face = "plain", 
                                     size = calc_element("strip.text.x", theme_consultancy())$size), 
      by_layer_x = TRUE, 
      
      text_y = ggh4x::elem_list_text(face = c("bold", "plain"), 
                                     size = c(1.25 * calc_element("strip.text.x", theme_consultancy())$size,
                                              calc_element("strip.text.x", theme_consultancy())$size)), 
      by_layer_y = TRUE)
   
   FacetTitleTheme_X <- ggh4x::strip_nested(
      text_x =             ggh4x::elem_list_text(face = c("bold", "plain"), 
                                                 size = c(1.25 * calc_element("strip.text.x", theme_consultancy())$size,
                                                          calc_element("strip.text.x", theme_consultancy())$size)), 
      by_layer_x = TRUE, 
      
      text_y = ggh4x::elem_list_text(face = "plain", 
                                     size = calc_element("strip.text.x", theme_consultancy())$size), 
      by_layer_y = TRUE)
   
   
   A <- A + 
      switch(FacetOpts, 
             "ggplot2 facets" = facet_grid(rows = vars(!!facet1_column), cols = vars(!!facet2_column)), 
             "FC1PlusTitle FC2" = ggh4x::facet_nested(Facet1Title + FC1 ~ FC2, 
                                                      strip = FacetTitleTheme_Y), 
             "FC1PlusTitle NoFC2" = ggh4x::facet_nested(Facet1Title + FC1 ~ ., 
                                                        strip = FacetTitleTheme_Y), 
             "FC1 FC2PlusTitle" = ggh4x::facet_nested(FC1 ~ Facet2Title + FC2, 
                                                      strip = FacetTitleTheme_X), 
             "FC1PlusTitle FC2PlusTitle" = ggh4x::facet_nested(Facet1Title + FC1 ~ Facet2Title + FC2, 
                                                               strip = FacetTitleTheme_XY), 
             "NoFC1 FC2PlusTitle" = ggh4x::facet_nested(. ~ Facet2Title + FC2, 
                                                        strip = FacetTitleTheme_X))
   
   ## Setting colors ---------------------------------------------------------
   
   if(as_label(colorBy_column) != "<empty>"){
      
      if(is.null(names(color_set)) == FALSE){
         # Checking that all names of colors are present in the data. 
         Missing <- setdiff(names(color_set), 
                            unique(Ontogenies$colorBy_column))
         
         if(length(Missing) > 0){
            warning(wrapn(paste0("You have supplied a named character vector for the colors to use in your graph, but the following value(s) is/are not present in your data: ", 
                                 str_comma(paste0("'", Missing, "'")), ". These data will be omitted from your graph.")), 
                    call. = FALSE)
            
            color_set <- color_set[setdiff(names(color_set), Missing)]
         }
      }
      
      NumColorsNeeded <- Ontogenies %>% 
         pull(colorBy_column) %>% unique() %>% sort() %>% length()
      
      MyColors <- make_color_set(color_set = color_set, 
                                 num_colors = NumColorsNeeded)
      
      suppressWarnings(
         A <-  A + scale_color_manual(values = MyColors, drop = FALSE) +
            scale_fill_manual(values = MyColors, drop = FALSE)
      )
   }
   
   A <- A +
      theme_consultancy(border = any(c(as_label(facet1_column) != "<empty>", 
                                       as_label(facet2_column) != "<empty>"))) +
      theme(legend.position = legend_position)
   
   if(any(complete.cases(graph_title))){
      A <- A + ggtitle(graph_title) +
         theme(plot.title = element_text(hjust = 0.5, 
                                         size = graph_title_size), 
               plot.title.position = "panel")
   }
   
   # Saving -----------------------------------------------------------------
   
   if(complete.cases(save_graph)){
      
      # Checking for NA for fig_height and width
      if(is.na(fig_height)){fig_height <- 5}
      
      if(is.na(fig_width)){fig_width <- 8}
      
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
             plot = A)
   }
   
   return(A)
}

# ontogeny_plot(enzyme = "CYP3A4")
# 
# ontogeny_plot(enzyme = "CYP|UGT") +
#    facet_wrap(~ EnzymeType)
# ontogeny_plot(enzyme = "UGT1A4")
# ontogeny_plot(enzyme = "P-gp")
# ontogeny_plot(enzyme = "CYP2C9") + scale_color_brewer(palette = "Set1")
# ontogeny_plot(enzyme_type = c("CYPs", "UGTs")) +
#    facet_wrap(~ EnzymeDescription, scales = "free")
# 
# unique(OntogenyEquations$EnzymeType)
# 
# Plots <- list()
# for(i in unique(OntogenyEquations$EnzymeType)){
#    Plots[[i]] <- ontogeny_plot(enzyme_type = i) +
#       facet_wrap(~ EnzymeDescription, scales = "free", 
#                  ncol = 3) +
#       ggtitle(i) +
#       theme(legend.position = "none")
#    
# }
# 
# glist <- lapply(Plots, ggplotGrob)
# ggsave("Ontogenies in V22.pdf", 
#        width = 8.5, height = 11, 
#        gridExtra::marrangeGrob(glist, nrow = 1, ncol = 1))
# 
# 
