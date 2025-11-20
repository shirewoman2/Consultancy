#' Show color sets included in the SimcypConsultancy package
#'
#' @param color_set the color set or color sets you would like to see. Options
#'   are: "all" to see all possibilities or "reds", "blues", "purples",
#'   "blue-greens", "greens", "rainbow", and/or the name of a specific
#'   consultant who has supplied their favorite colors to the R Working Group to
#'   see specific sets of colors. To see which consultants have included their
#'   favorite colors here, please run \code{names(OurFavoriteColors)} in the
#'   console.
#' @param num_colors optionally specify the number of colors you wish to see for
#'   each color set. Setting this to "original", the default, will show you the
#'   number of colors that were originally specified for that color set. Set
#'   this to some other number and, if there were not originally that number of
#'   colors, we'll make a gradient between them until you have the number you
#'   want.
#' @param shade optionally specify whether you want the shade to be "regular",
#'   "darker", or "lighter". This only applies to "reds", "blues", "purples",
#'   "blue-greens", "greens", "reds", and "rainbow" color sets.
#'
#' @returns a graph
#' @export
#'
#' @examples
#' show_colors(color_set = c("Elle", "Laura Sh", "reds", "blues"),
#'             num_colors = "original")
#'
#' show_colors(color_set = "traffic", num_colors = "original")

show_colors <- function(color_set, 
                        num_colors = "original", 
                        shade = NA){
   
   # error catching -------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
   
   color_set <- tolower(color_set)
   
   # Fixing possible misspecification of color set names
   color_set <- case_when(
      color_set == "default" & num_colors == 2 ~ "set1", 
      
      str_detect(color_set, "green") & str_detect(color_set, "blue") ~ "blue-green", 
      
      str_detect(color_set, "green") & str_detect(color_set, "red") ~ "green to red", 
      
      str_detect(color_set, "yellow") & str_detect(color_set, "red") ~ "yellow to red", 
      
      str_detect(color_set, "default|brewer.*2|set.*2|dark.*2") ~ "set2", 
      
      str_detect(color_set, "brewer.*1|set.*1") ~ "set1", 
      
      str_detect(color_set, "laura sa") ~ "laurasa", 
      
      str_detect(color_set, "laura sh") ~ "laurash", 
      
      .default = color_set)
   
   AllColors <- c("rainbow", 
                  "reds",
                  "blues", 
                  "blue-greens",
                  "greens", 
                  "purples",
                  "traffic", 
                  "green to red", 
                  "yellow to red", 
                  "set1", 
                  "set2", 
                  "dark1", 
                  "dark2", 
                  tolower(names(OurFavoriteColors)))
   
   if(any(color_set == "all")){
      color_set <- AllColors
   }
   
   BadColors <- setdiff(tolower(color_set), 
                        AllColors)
   
   if(length(BadColors) > 0){
      warning(paste0(wrapn("The following color sets are not among the available options and will be skipped:"), 
                     str_c(paste0("   ", BadColors), collapse = "\n")), 
              call. = FALSE)
      
      color_set <- intersect(color_set, AllColors)
      
   }
   
   shade <- tolower(shade)[1]
   shade <- ifelse(is.na(shade), "regular", shade)
   if(shade %in% c("darker", "lighter", "regular") == FALSE){
      warning(wrapn("You have specified something other than 'regular', 'lighter', or 'darker' for the shade, and those are the only possible options. We'll set the shade to 'regular'."), 
              .call = FALSE)
      shade <- "regular"
   }
   
   # subfuns --------------------------------------------------------------
   
   show_col_gg <- function(colors, ncol = NULL) {
      n <- length(colors)
      if (is.null(ncol)) {
         ncol <- ceiling(sqrt(n))  # auto square layout
      }
      
      DF <- data.frame(
         Color = colors,
         x = rep(seq_len(ncol), length.out = n),
         y = rep(seq_len(ceiling(n / ncol)), each = ncol)[1:n]
      )
      
      DF$HCL <- farver::decode_colour(
         colour = DF$Color,
         to = "hcl")[, "l"]
      
      DF <- DF %>% 
         mutate(
            LabelColor = case_when(HCL > 50 ~ "black", 
                                   .default = "white"))
      
      G <- 
         ggplot(DF, 
                aes(x, y, fill = Color, label = Color, color = LabelColor)) +
         geom_tile(color = "white") +
         scale_fill_identity() +
         scale_color_identity() +
         geom_text() +
         coord_equal() +
         theme_void() +
         theme(legend.position = "none") +
         scale_y_reverse()  # match top-to-bottom order
      
      return(G)
   }
   
   show_colors_subfun <- function(color_set, 
                                  num_colors, 
                                  shade){
      
      color_set <- tolower(color_set)
      
      if(color_set == "default" & num_colors == 1){
         color_set <- "black"
      }
      
      if(color_set %in% tolower(names(OurFavoriteColors))){
         names(OurFavoriteColors) <- tolower(names(OurFavoriteColors))
         
         NumColSpecified <- length(OurFavoriteColors[[color_set]])
      } else {
         NumColSpecified <- bind_rows(
            RColorBrewer::brewer.pal.info %>% 
               mutate(Set = tolower(rownames(.))) %>% 
               filter(!Set %in% c("greens", "purples", "reds", "blues")),
            
            tribble(
               ~Set,         ~maxcolors, 
               "greens",      case_match(shade, 
                                         "regular" ~ 9, 
                                         "lighter" ~ 6, 
                                         "darker" ~ 7),   
               "purples",     case_match(shade, 
                                         "regular" ~ 9, 
                                         "lighter" ~ 6, 
                                         "darker" ~ 7), 
               "blues",       case_match(shade, 
                                         "regular" ~ 6, 
                                         "lighter" ~ 3, 
                                         "darker" ~ 2), 
               "blue-green",  case_match(shade, 
                                         "regular" ~ 5, 
                                         "lighter" ~ 3, 
                                         "darker" ~ 4), 
               "reds",        case_match(shade, 
                                         "regular" ~ 3, 
                                         "lighter" ~ 2, 
                                         "darker" ~ 2), 
               "rainbow",    case_match(shade, 
                                        "regular" ~ 7, 
                                        "lighter" ~ 6, 
                                        "darker" ~ 7), 
               
               "green to red",   4,
               "traffic",        4, 
               "yellow to red",  4)) %>% 
            filter(Set == color_set) %>% 
            pull(maxcolors)
      }
      
      if(length(NumColSpecified) == 0){
         # arbitrarily setting this
         NumColSpecified <- 4
      }
      
      if(tolower(num_colors) == "original"){
         num_colors <- NumColSpecified
      } 
      
      G <- show_col_gg(
         colors = make_color_set(color_set = color_set, 
                                 num_colors = num_colors, 
                                 shade = shade)) +
         ggtitle(case_match(color_set, 
                            "laurasa" ~ "LauraSa", 
                            "laurash" ~ "LauraSh", 
                            .default = str_to_title(color_set))) + 
         theme(plot.title = element_text(hjust = 0.5, 
                                         face = "bold", 
                                         size = 14))
      
      return(G)
   }

   
   # Making graphs --------------------------------------------------------
   
   G <- list()
   
   for(cc in color_set){
      G[[cc]] <- show_colors_subfun(color_set = cc, 
                                    num_colors = num_colors, 
                                    shade = shade)
   }
     
   patchwork::wrap_plots(G) 
   
}




