#' Create a vector of a rainbow of colors
#'
#' \code{rainbow} is useful for getting a set of colors for graphs when you need
#' a LOT of colors; there are prettier and more easily distinguishable options
#' if you only have a couple of comparisons.
#'
#' @param ncolors number of colors desired
#'
#' @return a character vector of colors
#' @export
#'
#' @examples
#' # Create a rainbow of 10 colors.
#' rainbow(10)
#' 
#' # Try using scales::show_col() to visualize the colors, ex:
#' scales::show_col(rainbow(10))
#' 
rainbow <- function(ncolors){
   
   if(ncolors <= 7){
      colRainbow <- colorRampPalette(c("#B91E02FF", "#F66D19FF", "#B8F735FF",
                                       "#22C4E3FF", "#434DB9FF"))
      
   } else {
      colRainbow <- colorRampPalette(c("gray20", "antiquewhite4", "firebrick3",
                                       "darkorange", "green3", "seagreen3",
                                       "cadetblue", "dodgerblue3", "royalblue4",
                                       "darkorchid4"))
      
   }
   
   return(colRainbow(ncolors))
}



#' Create a vector of blues fading into greens
#'
#' \code{blueGreens} is useful for getting a set of pretty blues that fade into
#' greens for graphs.
#'
#' @param ncolors number of colors desired
#'
#' @return a character vector of colors
#' @export
#'
#' @examples
#' # Create a set of 10 blues to greens.
#' blueGreens(10)
#'
#' # Try using scales::show_col() to visualize the colors, ex:
#' scales::show_col(blueGreens(10))
#' 
blueGreens <- function(ncolors){
   
   colblueGreens <- colorRampPalette(c("royalblue4", "dodgerblue3",
                                       "cadetblue", "seagreen3", "green3"))
   return(colblueGreens(ncolors))
}


#' Create a vector of blues
#'
#' \code{blues} is useful for getting a set of pretty blues for graphs. These
#' blues come from the 4th through 9th blues from grDevices::blues9, just to
#' give credit where it's due, plus a couple of darker blues at the end. 
#'
#' @param ncolors number of colors desired
#'
#' @return a character vector of colors
#' @export
#'
#' @examples
#' # Create a set of 10 blues.
#' blues(10)
#'
#' # Try using scales::show_col() to visualize the colors, ex:
#' scales::show_col(blues(10))
#' 
blues <- function(ncolors){
   
   colBlues <- colorRampPalette(c("#9ECAE1", "#6BAED6", "#4292C6", "#2171B5",
                                  "#08519C", "#08306B", "#051D41", "#020B18"))
   
   return(colBlues(ncolors))
   
}


#' Create a vector of yellow-greens
#'
#' \code{chartreuse} is useful for getting a set of yellows to greens for
#' graphs. These are from the RColorBrewer package, color set "YlGn", just to
#' give credit where it's due, but they can be any number in length rather than
#' only up to 9 colors.
#'
#' @param ncolors number of colors desired
#' @param shade "full range" for all chartreuses, from very light to quite dark,
#'   "darker" to get only the darker shades
#'
#' @return a character vector of colors
#' @export
#'
#' @examples
#' # Create a set of 10 yellows to chartreuses to greens.
#' chartreuse(10)
#'
#' # Try using scales::show_col() to visualize the colors, ex:
#' scales::show_col(chartreuse(10))
#' 
chartreuse <- function(ncolors, 
                       shade = "full range"){
   
   if(shade %in% c("full range", "darker") == FALSE){
      warning("The options for the argument shade are `full range` or `darker`, and you've supplied something else. We'll use the default of `full range`.\n", 
              call. = FALSE)
      shade = "full range"
   }
   
   colChartreuse <- colorRampPalette(
      switch(shade, 
             "full range" = c("#FFFFE5", "#F7FCB9", "#D9F0A3", 
                              "#ADDD8E", "#78C679", "#41AB5D", 
                              "#238443", "#006837", "#004529"), 
             "darker" = c("#ADDD8E", "#78C679", "#41AB5D", 
                          "#238443", "#006837", "#004529", "#001E12")))
   
   return(colChartreuse(ncolors))
   
}


#' Create a vector of purples
#'
#' \code{purples} is useful for getting a set of yellows to greens for
#' graphs. These are from the RColorBrewer package, color set "YlGn", just to
#' give credit where it's due, but they can be any number in length rather than
#' only up to 9 colors.
#'
#' @param ncolors number of colors desired
#'
#' @return a character vector of colors
#' @export
#'
#' @examples
#' # Create a set of 10 purples.
#' purples(10)
#'
#' # Try using scales::show_col() to visualize the colors, ex:
#' scales::show_col(purples(10))
#' 
purples <- function(ncolors, 
                    shade = "full range"){
   
   if(shade %in% c("full range", "darker") == FALSE){
      warning("The options for the argument shade are `full range` or `darker`, and you've supplied something else. We'll use the default of `full range`.\n", 
              call. = FALSE)
      shade = "full range"
   }
   
   colPurple <- colorRampPalette(
      switch(shade, 
             "full range" = c("#FCFBFD", "#EFEDF5", "#DADAEB",
                              "#BCBDDC", "#9E9AC8", "#807DBA", 
                              "#6A51A3", "#54278F", "#3F007D"), 
             "darker" = c("#BCBDDC", "#9E9AC8", "#807DBA", 
                          "#6A51A3", "#54278F", "#3F007D", "#26004C", "#17002E")))
   
   return(colPurple(ncolors))
   
}



