#' INTERNAL: Create a set of boundary colors based on the color set and the
#' boundaries the user needs ---- CONSIDERING WHETHER IT'S WORTH IT TO MAKE
#' THIS. It doesn't actually save me much copying and pasting since I only use
#' it once in formatTable_Simcyp, where it has one set of color_set options and
#' boundaries and number of breaks, and only twice in so_graph, where it has a
#' different set of color_set options and boundaries and number of breaks.
#'
#' @param color_set either a single value, e.g., "red green" or a character
#'   vector of colors. Acceptable single-value color set names:\itemize{
#'   \item{"yellow to red" - for highlighting in PK tables}
#'
#'   \item{"traffic" - for highlighting in PK tables}
#'
#'   \item{"red black lines" or "red black fill" - for S/O boundary lines or fill}
#'
#'   \item{"red green lines" or "red green fill" - for S/O boundary lines or fill}
#'
#'   \item{"muted red green lines" or "muted red green fill" - for S/O boundary lines or fill}}
#'
#' @param boundaries numeric vector of boundaries
#' @param max_num_breaks maximum number of breaks. For so_graph, this should be
#'   3 and for formatTable_Simcyp, this should be 4.
#'
#' @return a character vector of colors, one for each numeric boundary
#'
#' @examples
#' # None. Internal use only.
#' 
set_boundary_colors <- function(color_set,
                                boundaries, 
                                break_type = "GMR"){
   
   # Error catching --------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Tidying inputs
   if(any(boundaries < 1)){
      warning("At least one of the numbers you specified for boundaries was < 1. We will automatically use both the original number you specified and its inverse for highlighting, so we'll ignore any values < 1 here.", 
              call. = FALSE)
   }
   
   boundaries <- sort(unique(c(boundaries, 1/boundaries)))
   boundaries <- boundaries[which(boundaries >= 1)]
   
   color_set <- tolower(color_set)
   
   GoodColors <- c("red black", "yellow to red", "green to red", 
                   "muted red green", "lisa", "traffic")
   
   if(length(boundaries) != length(color_set) &
      tolower(color_set[1]) %in% GoodColors == FALSE){
      warning("You have specified one number of colors for highlighting values and a different number of cutoff values, so we don't know what colors you want. We'll use the default colors for highlighting.", 
              call. = FALSE)
      color_set <- "yellow to red"
   }
   
   if(color_set[1] %in% GoodColors == FALSE && 
      tryCatch(is.matrix(col2rgb(color_set)),
               error = function(x) FALSE) == FALSE){
      warning("The values you used for highlighting are not all valid colors in R. We'll used the default colors instead.", 
              call. = FALSE)
      color_set <- "yellow to red"
   } 
   
   # A little Easter egg 
   if(all(tolower(color_set) == "lisa")){
      color_set <- "traffic"
   }
   
   # "green to red" is the same as "muted red green"
   if(all(tolower(color_set) == "green to red")){
      color_set <- "muted red green"
   }
   
   # Upper number of breaks not including 1
   MaxBound <- length(boundaries)
   
   # Tidying inputs
   if(1 %in% boundaries & 
      any(color_set %in% GoodColors)){
      color_set <- paste(color_set, "1")
   }
   
   # Boundaries are only set up with *specific* colors when there are <= 3
   # boundaries, not including the middle. If there are more boundaries than
   # that, then we'll use colorRampPalette to pick the colors. Because of how
   # this is set up in the boundary_color_set switch, we want the breaks to be
   # no larger than 4.
   ColorChoices <- paste(
      color_set, 
      cut(length(boundaries), breaks = c(0:min(c(4, MaxBound)), Inf)))
   
   boundary_color_set <- 
      switch(ColorChoices, 
             # red black -----------------------------------------------------
             
             # 1 boundary, e.g., it's only unity
             "red black (0,1]" = "black", 
             
             # 2 boundaries
             "red black (1,2]" = c("black", "black"), 
             
             # 3 boundaries
             "red black (2,3]" = c("black", "black", "red"), 
             
             # 4 boundaries
             "red black (3,4]" = c("black",
                                     colorRampPalette(c("black", "#FFC000", "red"))(
                                        length(boundaries) - 1)), 
             
             # > 4 boundaries (this is same as above intentionally)
             "red black (4,Inf]" = c("black",
                                     colorRampPalette(c("black", "#FFC000", "red"))(
                                        length(boundaries) - 1)), 
             
             # 1 boundary, e.g., it's only unity
             "red black 1 (0,1]" = "black", 
             
             # 2 boundaries
             "red black 1 (1,2]" = c("black", "black"), 
             
             # 3 boundaries
             "red black 1 (2,3]" = c("black", "black", "red"), 
             
             # 4 boundaries
             "red black 1 (3,4]" = c("black",
                                       colorRampPalette(c("black", "#FFC000", "red"))(
                                          length(boundaries) - 1)), 
             
             # > 4 boundaries (this is same as above intentionally)
             "red black 1 (4,Inf]" = c("black",
                                       colorRampPalette(c("black", "#FFC000", "red"))(
                                          length(boundaries) - 1)), 
             
             # red green ------------------------------------------------------
             
             # 1 boundary, e.g., it's only unity
             "red green (0,1]" = "#17A142", 
             
             # 2 boundaries
             "red green (1,2]" = c("#17A142", "#17A142"), 
             
             # 3 boundaries
             "red green (2,3]" = c("#17A142", "#17A142", "red"), 
             
             # 4 boundaries
             "red green (3,4]" = c("#17A142", 
                                     colorRampPalette(c("#17A142", "red"))(
                                        length(boundaries) - 1)), 
             
             # >4 boundaries (this is same as above intentionally)
             "red green (4,Inf]" = c("#17A142", 
                                     colorRampPalette(c("#17A142", "red"))(
                                        length(boundaries) - 1)), 
             
             
             # 1 boundary, e.g., it's only unity
             "red green 1 (0,1]" = "#17A142", 
             
             # 2 boundaries
             "red green 1 (1,2]" = c("#17A142", "red"), 
             
             # 3 boundaries
             "red green 1 (2,3]" = c("#17A142", "orange", "red"), 
             
             # 4 boundaries
             "red green 1 (3,4]" = c("#17A142", 
                                       colorRampPalette(c("#17A142", "red"))(
                                          length(boundaries) - 1)), 
             
             # >4 boundaries (this is same as above intentionally)
             "red green 1 (4,Inf]" = c("#17A142", 
                                       colorRampPalette(c("#17A142", "red"))(
                                          length(boundaries) - 1)), 
             
             
             # muted red green ------------------------------------------------
             
             # 1 boundary, e.g., it's only unity
             "muted red green (0,1]" = "#A4E4AF", 
             
             # 2 boundaries
             "muted red green (1,2]" = c("#A4E4AF", "#A4E4AF"), 
             
             # 3 boundaries
             "muted red green (2,3]" = c("#A4E4AF", "#A4E4AF", "#E6A2A2"), 
             
             # 4 boundaries
             "muted red green (3,4]" = c("#A4E4AF", 
                                           colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                                              length(boundaries)-1)), 
             
             # >4 boundaries (this is same as above intentionally)
             "muted red green (4,Inf]" = c("#A4E4AF", 
                                           colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                                              length(boundaries)-1)), 
             
             # 1 boundary, e.g., it's only unity
             "muted red green 1 (0,1]" = "#A4E4AF", 
             
             # 2 boundaries
             "muted red green 1 (1,2]" = c("#A4E4AF", "#A4E4AF"), 
             
             # 3 boundaries
             "muted red green 1 (2,3]" = c("#A4E4AF", "#A4E4AF", "#E6A2A2"), 
             
             # 4 boundaries
             "muted red green 1 (3,4]" = c("#A4E4AF", 
                                             colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                                                length(boundaries)-1)), 
             
             # >4 boundaries (this is same as above intentionally)
             "muted red green 1 (4,Inf]" = c("#A4E4AF", 
                                             colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                                                length(boundaries)-1)), 
             
             
             # yellow to red ---------------------------------------------------
             
             # no middle, 1 boundary
             "yellow to red (0,1]" = "#FF9595",
             
             # no middle, 2 boundaries
             "yellow to red (1,2]" = c("#FFFF95", "#FF9595"),
             
             # no middle, 3 boundaries
             "yellow to red (2,3]" = c("#FFFF95", "#FFDA95", "#FF9595"),
             
             # no middle, >3 boundaries
             "yellow to red (3,4]" = colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                length(boundaries)),
             
             # This is the same as the above on purpose.
             "yellow to red (4,Inf]" = colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                length(boundaries)),
             
             # Just highlight everything white This would be weird and
             # probably not what the user wants, but is among the possible
             # choices for inputs.
             "yellow to red 1 (0,1]" = c("white"),
             
             # highlight middle, 1 boundary other than middle
             "yellow to red 1 (1,2]" = c("white", "#FF9595"),
             
             # highlight middle, 2 boundaries other than middle
             "yellow to red 1 (2,3]" = c("white", "#FFFF95", "#FF9595"),
             
             # highlight middle, 3 boundaries other than middle
             "yellow to red 1 (3,4]" = c("white", "#FFFF95", "#FFDA95", "#FF9595"),
             
             # highlight middle, >3 boundaries other than middle
             "yellow to red 1 (4,Inf]" =
                c("white",
                  colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                     length(boundaries))),
             
             
             # traffic --------------------------------------------------------
             
             # no middle, 1 boundary
             "traffic (0,1]" = "#FF0000",
             
             # no middle, 2 boundaries
             "traffic (1,2]" = c("#FFC000", "#FF0000"),
             
             # no middle, 3 boundaries
             "traffic (2,3]" = colorRampPalette(c("#FFC000", "#FF0000"))(
                length(boundaries)),
             
             # no middle, >3 boundaries
             "traffic (3,4]" = colorRampPalette(c("#FFC000", "#FF0000"))(
                length(boundaries)),
             # This is the same as the above on purpose.
             
             "traffic (4,Inf]" = colorRampPalette(c("#FFC000", "#FF0000"))(
                length(boundaries)),
             # This is the same as the above on purpose.
             
             # Just highlight everything green. This would be weird and
             # probably not what the user wants, but is among the possible
             # choices for inputs.
             "traffic 1 (0,1]" = c("#00B050"),
             
             # highlight middle, 1 boundary other than middle
             "traffic 1 (1,2]" = c("#00B050", "#FF0000"),
             
             # highlight middle, 2 boundaries other than middle
             "traffic 1 (2,3]" = c("#00B050", "#92D050", "#FF0000"),
             
             # highlight middle, 3 boundaries other than middle
             "traffic 1 (3,4]" = c("#00B050", "#92D050", "#FFC000", "#FF0000"),
             
             # highlight middle, >3 boundaries other than middle
             "traffic 1 (4,Inf]" =
                c("#00B050", "#92D050", 
                  colorRampPalette(c("#FFC000", "#FF0000"))(
                     length(boundaries)-1))
      ) 
   
   if(break_type == "GMR" & color_set %in% c("yellow to red 1", 
                                             "green to red 1", 
                                             "traffic 1")){
      boundary_color_set <- 
         switch(ColorChoices, 
                
                # yellow to red for GMRs -----------------------------------------
                
                "yellow to red 1 (3,4]" = c("white", "#FFFF95", "#FFDA95", "#FF9595"), 
                
                "green to red 1 (3,4]" = c("#C7FEAC", "#FFFF95", "#FFDA95", "#FF9595"), 
                
                "traffic 1 (3,4]" = c("#00B050", "#92D050", "#FFC000", "#FF0000")
         )
   }
   
   if(break_type == "GMR"){
      names(boundary_color_set) <- c("negligible", 
                                     "weak", 
                                     "moderate", 
                                     "strong")
   }
   
   return(boundary_color_set)
   
}

#' Make a key for highlighted geometric mean ratios for DDIs in a table 
#'
#' @param highlight_gmr_colors a set of colors for highlighting geometric mean
#'   ratios for DDIs. Options are "yellow to red", "green to red", "traffic" (a
#'   more vivid version of "green to red"), or a vector of 4 colors of your
#'   choosing. 
#'
#' @return a flextable for use as a key for which colors mean what GMR cutoff
#' @export
#'
#' @examples
#' make_gmr_highlight_key("yellow to red")
#' 
make_gmr_highlight_key <- function(highlight_gmr_colors){
   
   highlight_gmr_colors <- set_boundary_colors(color_set = highlight_gmr_colors, 
                                               boundaries = c(1, 1.25, 2, 5), 
                                               break_type = "GMR")
   
   tibble(`Interaction level` = names(highlight_gmr_colors)) %>% 
      flextable::flextable() %>% 
      flextable::bold(part = "header") %>% 
      flextable::bg(i = 1, 
                    bg = highlight_gmr_colors["negligible"]) %>% 
      flextable::bg(i = 2, 
                    bg = highlight_gmr_colors["weak"]) %>% 
      flextable::bg(i = 3, 
                    bg = highlight_gmr_colors["moderate"]) %>% 
      flextable::bg(i = 4, 
                    bg = highlight_gmr_colors["strong"]) %>% 
      flextable::width(width = 1.5) %>% 
      flextable::align(align = "center", part = "all")
   
}


#' Make a key for highlighted S/O values in a table or graph
#'
#' @param highlight_so_cutoffs cutoffs for highlighting simulated-to-observed
#'   ratios. Anything that is above those values or below the inverse of those
#'   values will be highlighted. Acceptable input for, say, highlighting values
#'   that are > 125\% or < 80\% of the observed and also, with a second color,
#'   values that are > 150\% or < 66\% would be: \code{highlight_so_cutoffs =
#'   c(1.25, 1.5)}. If you would like the middle range of values to be
#'   highlighted, include 1 in your cutoffs. For example, say you would like
#'   everything that's < 80\% or > 125\% to be highlighted red but you'd like
#'   the "good" values from 80\% to 125\% to be green, you can get that by
#'   specifying
#'   \code{highlight_so_cutoffs = c(1, 1.25)} and \code{highlight_so_colors =
#'   c("green", "red")}
#' @param highlight_so_colors optionally specify a set of colors to use for
#'   highlighting S/O values outside the limits you specified with
#'   \code{highlight_so_cutoffs}. Options: \describe{
#'
#'   \item{"yellow to red" (default)}{A range of light yellow to light orange to
#'   light red. If you have included 1 in your cutoffs and you leave
#'   \code{highlight_so_colors} with the default setting, values in the middle,
#'   "good" range of S/O values will be highlighted a light green.}
#'
#'   \item{"traffic"}{light green, yellow, and red designed to display values
#'   outside 1.25, 1.5, and 2 fold of unity, respectively. If you include 1 in
#'   \code{highlight_so_cutoffs}, you'll get a darker green for "good" S/O
#'   values. This color scheme was borrowed from Lisa, so if you've seen her
#'   slides, these will look familiar.}
#'
#'   \item{a character vector of specific colors}{Any R-acceptable colors, will
#'   work here, e.g., \code{highlight_so_colors = c("yellow", "orange", "red")}}.
#'   If you do specify your own bespoke colors, you'll need to make sure that
#'   you supply one color for every value in \code{highlight_so_cutoffs}.}
#'
#' @return a flextable for use as a key for which colors mean what S/O cutoff
#' @export
#'
#' @examples
#' make_so_highlight_key(highlight_so_colors = "green to red",
#'                       highlight_so_cutoffs = c(1, 1.25, 1.5, 2))
#' 
make_so_highlight_key <- function(highlight_so_cutoffs, 
                                  highlight_so_colors){
   
   highlight_so_colors <- set_boundary_colors(color_set = highlight_so_colors, 
                                              boundaries = highlight_so_cutoffs, 
                                              break_type = "SO")
   
   SOkey <- data.frame(UpperA = highlight_so_cutoffs[1:(length(highlight_so_cutoffs)-1)],
                       UpperB = highlight_so_cutoffs[2:(length(highlight_so_cutoffs))],
                       LowerA = 1/highlight_so_cutoffs[2:(length(highlight_so_cutoffs))],
                       LowerB = 1/highlight_so_cutoffs[1:(length(highlight_so_cutoffs)-1)]) %>%
      bind_rows(data.frame(UpperA = NA,
                           UpperB = highlight_so_cutoffs[length(highlight_so_cutoffs)],
                           LowerA = NA,
                           LowerB = 1/highlight_so_cutoffs[length(highlight_so_cutoffs)])) %>%
      mutate(across(.cols = everything(), .fns = round_consultancy),
             Text = paste(LowerA, "to", LowerB, "fold or", UpperA, "to", UpperB, "fold"))
   
   SOkey$Text[1] <- paste(SOkey$LowerA[1], "to", SOkey$UpperB[1], "fold")
   SOkey$Text[nrow(SOkey)] <- paste("<", SOkey$LowerB[nrow(SOkey)],
                                    "fold or >", SOkey$UpperB[nrow(SOkey)], "fold")
   
   SOkey <- SOkey %>% select(Text) %>%
      rename(`S/O cutoff` = Text) %>%
      flextable::flextable() %>%
      flextable::bold(part = "header") %>%
      flextable::width(width = 3.5) %>%
      flextable::align(align = "center", part = "all")
   
   for(i in 1:length(highlight_so_cutoffs)){
      SOkey <- SOkey %>%
         flextable::bg(i = i,
                       bg = highlight_so_colors[i])
   }
   
   SOkey
   
}



