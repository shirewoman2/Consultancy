#' Graph the ontogenies of drug-metabolizing enzymes and transporters
#'
#' \code{ontogeny_plot} uses the betas from the Simcyp Simulator to make graphs
#' of the ontogenies of all the enzymes and transporters included in the
#' Simulator. \strong{Note:} Only the beta version of the package includes the
#' object OntogenyEquations, which is required for this function to work.
#'
#' @param Enzyme drug-metabolizing enzymes and transporters to plot, which
#'   should be supplied as either a single string, in quotes, that we'll try to
#'   match (e.g., "CYP3A4" will get you profiles 1 and 2 as well as "CYP3A4 in
#'   vitro" and "Intestinal CYP3A4") or as a character vector of the enzymes or
#'   transporters you want. Options are listed in the column "Enzyme" of the
#'   data.frame OntogenyEquations, which is saved with this package. To see it,
#'   type into the console \code{view(OntogenyEquations)} There's also a
#'   slightly different version of the column "Enzyme" called
#'   "Enzyme_alternative", which tries to make a few things a little clearer for
#'   graphs. For example, instead of "CYP3A4 Profile 2", it lists "CYP3A4
#'   (Upreti)". We'll look for whatever you supply for the argument "Enzyme" in
#'   \emph{both} the column "Enzyme" and the column "Enzyme_alternative" and
#'   return anything that matches in either place.
#' @param Enzyme_type type of enzyme or transporter. Instead of graphing only a
#'   specific enzyme or enzymes, you can request any enzymes or transporters of
#'   a specific type. Please see the options in "OntogenyEquations" in the
#'   column "Enzyme_type". This should be supplied as a character vector, e.g.,
#'   \code{Enzyme_type = c("transporters", "UGTs")}
#' @param simulator_version Simcyp Simulator version to display. Options are 21,
#'   22 (default), or 23.
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
#' @param legend_position Specify where you want the legend to be. Options are
#'   "left", "right" (default in most scenarios), "bottom", "top", or "none" if
#'   you don't want one at all.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My ontogeny graph.png"If you leave off ".png"
#'   or ".docx", the graph will be saved as a png file, but if you specify a
#'   different graphical file extension, it will be saved as that file format.
#'   Acceptable graphical file extensions are "eps", "ps", "jpeg", "jpg",
#'   "tiff", "png", "bmp", or "svg". Do not include any slashes, dollar signs,
#'   or periods in the file name. Leaving this as NA means the file will not be
#'   saved to disk.
#' @param fig_height figure height in inches
#' @param fig_width figure width in inches
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
#' # none yet
#' 
ontogeny_plot <- function(Enzyme = NA, 
                          Enzyme_type = NA, 
                          simulator_version = 22, 
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
                          save_graph = NA,
                          fig_height = NA,
                          fig_width = NA){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
           call. = FALSE)
   }
   
   warning("The ontogeny equation shown on screen for Simcyp Simulator versions before V24 was INCORRECT. Amita is checking on the corrected equations to use with the Science Team, and these graphs should be considered as PRELIMINARY unless you have confirmed with Amita that the equations are correct. Please ask Laura Sh. about this if you want to use these graphs in a report!\n", 
           call. = FALSE)
   # Note to self: I need to check back w/Amita after her 3/19/22 email to see
   # what the status is on this. -LSh
   
   if(length(Enzyme) > 1){
      Enzyme <- str_c(Enzyme, collapse = "|")
   }
   # Remove parentheses
   Enzyme <- gsub("\\(|\\)", ".", Enzyme)
   
   
   if(length(Enzyme_type) > 1){
      Enzyme_type <- str_c(Enzyme_type, collapse = "|")
   }
   
   # Remove parentheses
   Enzyme_type <- gsub("\\(|\\)", ".", Enzyme_type)
   
   
   # Setting up data ----------------------------------------------------------
   
   Ontogenies <- OntogenyEquations
   
   if(any(complete.cases(Enzyme))){
      Ontogenies <- Ontogenies %>% 
         filter(str_detect(tolower(Enzyme_alternative), tolower({{Enzyme}})) |
                   str_detect(tolower(Enzyme), tolower({{Enzyme}})))
   }
   
   if(any(complete.cases(Enzyme_type))){
      Ontogenies <- Ontogenies %>% 
         filter(str_detect(tolower(Enzyme_type), tolower({{Enzyme_type}})))
   }
   
   Ontogenies <- Ontogenies %>% 
      filter(switch(as.character(simulator_version), 
                    "21" = V21 == TRUE, 
                    "22" = V22 == TRUE, 
                    "23" = V23 == TRUE)) %>% 
      left_join(expand.grid(Age = seq(0, 25, length.out = 1000), 
                            Enzyme = OntogenyEquations$Enzyme), by = "Enzyme") %>% 
      mutate(AgeGroup = case_when(Age <= Age_cap1 ~ 1, 
                                  Age > Age_cap1 & Age <= Age_cap2 ~ 2, 
                                  Age > Age_cap2 ~ 3),
             
             Fraction = case_when(
                # sigmoidal below Age_cap1
                complete.cases(Age_cap1) & 
                   Age <= Age_cap1 & 
                   complete.cases(Fmax) ~ 
                   Fbirth + ( (Fmax - Fbirth) * Age^n) / (Age50^n + Age^n),
                
                # exponential between Age_cap1 & 2. Will address Upreti, which
                # is different, at the bottom.
                (Age > Age_cap1 | is.na(Age_cap1)) & 
                   Age <= Age_cap2 & complete.cases(C2) ~
                   C0 + C1*exp(C2*(Age - Age_cap2 - C3)), 
                
                # linear between Age_cap1 & 2
                (Age > Age_cap1 | is.na(Age_cap1)) &
                   Age <= Age_cap2 & is.na(C2) ~
                   C0 + C1 * Age, 
                
                # same as adult above Age_cap1
                Age > Age_cap1 & is.na(Age_cap2) ~ 1,
                
                # same as adult above Age_cap2
                Age > Age_cap2 ~ 1), 
             
             # Upreti used a different exponential. Adjusting for that.
             Fraction = case_when(str_detect(Enzyme_alternative, "Upreti") & 
                                     (Age > Age_cap1 | is.na(Age_cap1)) & 
                                     Age <= Age_cap2 & complete.cases(C2) ~ 
                                     C0 + C1 * exp(C2 * (Age - C3)), 
                                  TRUE ~ Fraction), 
             
             Age_mo = Age * 12)  
   
   
   if(compare_to_no_ontogeny){
      Ontogenies <- Ontogenies %>% 
         bind_rows(data.frame(Age = c(0, 18), 
                              Enzyme = "No ontogeny", 
                              Enzyme_alternative = "No ontogeny", 
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
      message("\nYou requested a title for facet1 or facet 2. Adding a title to facets requires the package ggh4x,\nwhich the R Working Group will ask IT to install next time VDIs are rebuilt but which we didn't\nthink of this go 'round.")
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
      unite(col = Group, Enzyme, Enzyme_type, Tissue, remove = FALSE)
   
   
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
      
      NumColorsNeeded <- Ontogenies %>% 
         pull(colorBy_column) %>% unique() %>% sort() %>% length()
      
      # If there's only one unique value in the colorBy_column, then make that
      # item black.
      if(NumColorsNeeded == 1){
         A <- A + scale_color_manual(values = "black") +
            scale_fill_manual(values = "black")
         
      } else {
         
         # This is when the user wants specific user-specified colors rather
         # that one of the pre-made sets.
         if(length(color_set) > 1){
            
            # If they supply a named character vector whose values are not
            # present in the data, convert it to an unnamed character vector.
            if(is.null(names(color_set)) == FALSE && 
               all(unique(Ontogenies$colorBy_column) %in% names(color_set) == FALSE)){
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
            
            # NOTE: For no reason I can discern, if the user has observed
            # data that should be all one color but then uses scale_color_X
            # where x is anything except "manual", the observed points
            # DISAPPEAR. That's why, below, whenever it's scale_color_x, I'm
            # setting the colors needed and then using scale_color_manual
            # instead of scale_color_x. -LSh
            
            color_set <- ifelse(color_set == "default" & 
                                   NumColorsNeeded == 2, 
                                "set1", "set2")
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
         }
         
         suppressWarnings(
            A <-  A + scale_color_manual(values = MyColors, drop = FALSE) +
               scale_fill_manual(values = MyColors, drop = FALSE)
         )
         
      }
   }
   
   A <- A +
      theme_consultancy() +
      theme(legend.position = legend_position)
   
   
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
                                  "png", "bmp", "svg", "jpg", "docx"), 
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

# ontogeny_plot(Enzyme = "CYP3A4")
# 
# ontogeny_plot(Enzyme = "CYP|UGT") +
#    facet_wrap(~ Enzyme_type)
# ontogeny_plot(Enzyme = "UGT1A4")
# ontogeny_plot(Enzyme = "P-gp")
# ontogeny_plot(Enzyme = "CYP2C9") + scale_color_brewer(palette = "Set1")
# ontogeny_plot(Enzyme_type = c("CYPs", "UGTs")) +
#    facet_wrap(~ Enzyme_alternative, scales = "free")
# 
# unique(OntogenyEquations$Enzyme_type)
# 
# Plots <- list()
# for(i in unique(OntogenyEquations$Enzyme_type)){
#    Plots[[i]] <- ontogeny_plot(Enzyme_type = i) +
#       facet_wrap(~ Enzyme_alternative, scales = "free", 
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
