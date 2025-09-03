#' Create a set of colors for use in graphs
#'
#' @param color_set the set of colors to use. Options: \describe{
#'
#'   \item{"default" or "Brewer dark 2" or "dark 2"}{a set of colors from Cynthia 
#'   Brewer et al. from Penn State
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
#'   \item{"blues"}{a set of blues fading from baby to navy. Like
#'   "blue-green", this palette can be especially useful if you are comparing a
#'   systematic change in some continuous variable.}
#'
#'   \item{"greens"}{a set of greens from chartreuse to forest. Great for showing
#'   systematic changes in a continuous variable.}
#'
#'   \item{"purples"}{a set of purples from lavender to aubergine. Great for showing
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
#' @param num_colors the number of colors needed
#' @param shade for the color_sets "blues", "blue-green", "greens", "purples",
#'   "reds", and "rainbow", the shade of colors to use, which can be "regular"
#'   "darker", or "lighter". This will be ignored for other supplied values for
#'   color_set.
#'
#' @return a character vector of color names
#' @export
#'
#' @examples
#' make_color_set("blues", 10)
#' make_color_set("Tableau", 3)
#'
#' # You can see the colors with show_col from the scales package.
#' make_color_set("viridis", 5) %>% scales::show_col()
#' make_color_set("Brewer set 1", 4) %>% scales::show_col()
#'
#' 
make_color_set <- function(color_set, 
                           num_colors, 
                           shade = NA){
   
   # error catching -------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   
   # Main body of function ----------------------------------------------------
   
   # Checking whether the single-length string they provided is already a
   # color. None of the color sets actually are.
   ColorCheck <- try(expr = col2rgb(color_set), silent = TRUE)
   
   # This is when the user wants specific user-specified colors rather
   # that one of the pre-made sets.
   if(length(color_set) > 1 | is.matrix(ColorCheck)){
      
      if(length(color_set) < num_colors){
         warning(paste("There are", num_colors,
                       "unique values in the column you have specified for the colors, but you have only specified", 
                       length(color_set), 
                       "colors to use. We will recycle the colors to get enough to display your data, but you probably will want to supply more colors and re-graph.\n"), 
                 call. = FALSE)
         
         MyColors <- rep(color_set, 100)[1:num_colors]
      } else {
         MyColors <- color_set
      }
      
      # Checking whether all colors supplied are legitimate colors in R
      ColorCheck <- try(expr = col2rgb(MyColors), silent = TRUE)
      
      if(is.matrix(ColorCheck) == FALSE){
         warning(wrapn("Some of the colors supplied are not legitimate colors in R, so we can't set up the colors you wanted."), 
                 call. = FALSE)
         
         return(rep("gray", num_colors))
      }
      
   } else {
      
      color_set <- tolower(color_set)
      
      if(color_set == "default" & num_colors == 1){
         return("black")
      }
      
      if(color_set == "default" & num_colors == 2){
         color_set <- "set1"
      }
      
      # NOTE: For no reason I can discern, if the user has observed data that
      # should be all one color but then uses scale_color_X where x is anything
      # except "manual", the observed points DISAPPEAR. That's why, below,
      # whenever it's scale_color_x, I'm setting the colors needed and then
      # using scale_color_manual instead of scale_color_x. -LSh
      
      color_set <- ifelse(str_detect(tolower(color_set), 
                                     "default|brewer.*2|set.*2|dark.*2"), 
                          "set2", color_set)
      color_set <- ifelse(str_detect(tolower(color_set),
                                     "brewer.*1|set.*1"), 
                          "set1", color_set)
      
      shade <- case_when(is.na(shade) & 
                            color_set %in% c("greens", "purples") ~ "darker", 
                         
                         is.na(shade) & 
                            color_set %in% c("blues", "blue-green", "reds", 
                                             "rainbow") ~ "regular", 
                         
                         is.na(shade) ~ "regular", 
                         
                         .default = shade)
      
      suppressWarnings(
         MyColors <- 
            case_when(
               # Using "Dark2" b/c "Set2" is just really,
               # really light.
               color_set == "set2" ~ RColorBrewer::brewer.pal(num_colors, "Dark2")[
                  1:num_colors], 
               
               color_set == "dark2" ~ RColorBrewer::brewer.pal(num_colors, "Dark2")[
                  1:num_colors], 
               
               color_set == "blue-green" ~ blueGreens(num_colors, shade = shade),
               
               color_set == "blues" ~ blues(num_colors, shade = shade),
               
               color_set == "greens" ~ greens(num_colors, shade = shade), 
               
               color_set == "purples" ~ purples(num_colors, shade = shade), 
               
               color_set == "reds" ~ reds(num_colors, shade = shade), 
               
               color_set == "rainbow" ~ rainbow(num_colors, shade = shade),
               
               color_set == "elle" & num_colors <= 4 ~
                  c("#800074", "#298c8c", "dodgerblue4", "black")[1:num_colors], 
               
               color_set == "elle" & num_colors > 4 ~ 
                  rainbow(num_colors, shade = shade), 
               
               color_set == "set1" ~ RColorBrewer::brewer.pal(num_colors, "Set1")[
                  1:num_colors],
               
               color_set == "tableau" ~ ggthemes::tableau_color_pal(
                  palette = "Tableau 10")(num_colors),
               
               color_set == "viridis" ~ viridis::viridis_pal()(num_colors))
      )
      # NB: For the RColorBrewer palettes, the minimum number of colors you can
      # get is 3. Since sometimes we might only want 1 or 2 colors, though, we
      # have to add the [1:num_colors] bit.
      
      if(any(is.na(MyColors))){
         warning("The color set you requested does not have enough values for the number of colors required. We're switching the color set to `rainbow` for now.\n", 
                 call. = FALSE)
         
         MyColors <- rainbow(num_colors)
      }
   }
   
   # Checking whether the single-length string they provided is already a
   # color. None of the color sets actually are.
   ColorCheck <- try(expr = col2rgb(MyColors), silent = TRUE)
   
   if(is.null(MyColors) & is.matrix(ColorCheck)){
      MyColors <- color_set
   }
   
   if(length(MyColors) < num_colors){
      MyColors <- rep(MyColors, num_colors)
   }
   
   return(MyColors[1:num_colors])
   
}

