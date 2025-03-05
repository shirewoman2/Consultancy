#' Create a vector of a rainbow of colors
#'
#' \code{rainbow} is useful for getting a set of colors for graphs when you need
#' a LOT of colors; there are prettier and more easily distinguishable options
#' if you only have a couple of comparisons.
#'
#' @param ncolors number of colors desired
#' @param shade the shade of colors to use, which can be "regular" (default),
#'   "darker", or "lighter"
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
rainbow <- function(ncolors, 
                    shade = "regular"){
   
   if(ncolors <= 7){
      colRainbow <- colorRampPalette(c("#B91E02FF", "#F66D19FF", "#B8F735FF",
                                       "#22C4E3FF", "#434DB9FF"))
      
   } else {
      colRainbow <- colorRampPalette(c("gray20", "antiquewhite4", "firebrick3",
                                       "darkorange", "green3", "seagreen3",
                                       "cadetblue", "dodgerblue3", "royalblue4",
                                       "darkorchid4"))
      
   }
   
   MyColors <- colRainbow(ncolors)
   
   if(shade == "darker"){
      MyColors <- scales::muted(MyColors, l = 30, c = 70)
   } else if(shade == "lighter"){
      MyColors <- scales::muted(MyColors, l = 90, c = 100)
   }
   
   return(MyColors)
}



#' Create a vector of blues fading into greens
#'
#' \code{blueGreens} is useful for getting a set of pretty blues that fade into
#' greens for graphs.
#'
#' @param ncolors number of colors desired
#' @param shade the shade of colors to use, which can be "regular" (default),
#'   "darker", or "lighter"
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
blueGreens <- function(ncolors, 
                       shade = "regular"){
   
   if(shade == "darker"){
      colblueGreens <- colorRampPalette(c("#0D3053", "#2171B5", "#024E4E", "#083E22"))
      
   } else if(shade == "lighter"){
      colblueGreens <- colorRampPalette(c("#6ED1FE", "#D5F2FF", "#78FE78"))
      
   } else {
      # regular
      colblueGreens <- colorRampPalette(c("royalblue4", "dodgerblue3",
                                          "cadetblue", "seagreen3", "green3"))
   }
   
   MyColors <- colblueGreens(ncolors)
   
   return(MyColors)
   
}


#' Create a vector of blues
#'
#' \code{blues} is useful for getting a set of pretty blues for graphs. These
#' blues come from the 4th through 9th blues from grDevices::blues9, just to
#' give credit where it's due. 
#'
#' @param ncolors number of colors desired
#' @param shade the shade of colors to use, which can be "regular" (default),
#'   "darker", or "lighter"
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
blues <- function(ncolors, 
                  shade = "regular"){
   
   if(shade == "darker"){
      colBlues <- colorRampPalette(c("#2171B5", "#031023"))
   } else if(shade == "lighter"){
      colBlues <- colorRampPalette(c("#D5F2FF", "#6ED1FE", "#0B86F5"))
   } else {
      # regular
      colBlues <- colorRampPalette(c("#9ECAE1", "#6BAED6", "#4292C6", "#2171B5",
                                     "#08519C", "#08306B"))
      
   }
   
   MyColors <- colBlues(ncolors)
   
   return(MyColors)
   
}


#' Create a vector of greens
#'
#' \code{greens} is useful for getting a set of colors from chartreuse to
#' forest green for graphs. These are from the RColorBrewer package, color set
#' "YlGn", just to give credit where it's due, but they can be any number in
#' length rather than only up to 9 colors.
#'
#' @param ncolors number of colors desired
#' @param shade the shade of colors to use, which can be "regular" (default),
#'   "darker", or "lighter"
#'
#' @return a character vector of colors
#' @export
#'
#' @examples
#' # Create a set of 10 colors that are deepening shades of green.
#' greens(10)
#'
#' # Try using scales::show_col() to visualize the colors, ex:
#' scales::show_col(greens(ncolors = 10))
#' scales::show_col(greens(ncolors = 10, shade = "darker"))
#' 
greens <- function(ncolors, 
                   shade = "regular"){
   
   if(shade == "darker"){
      colGreen <- colorRampPalette(c("#78C679", "#41AB5D", 
                                     "#238443", "#006837", "#004529", 
                                     "#002617", "black"))
   } else if(shade == "lighter"){
      colGreen <- colorRampPalette(c("#FFFFE5", "#F7FCB9", "#D9F0A3", 
                                     "#ADDD8E", "#78C679", "#41AB5D"))
   } else {
      # Anything else, give them "regular". 
      colGreen <- colorRampPalette(c("#FFFFE5", "#F7FCB9", "#D9F0A3", 
                                     "#ADDD8E", "#78C679", "#41AB5D", 
                                     "#238443", "#006837", "#004529"))
   }
   
   return(colGreen(ncolors))
   
}


#' Create a vector of yellow-greens
#'
#' \code{chartreuse} is useful for getting a set of colors from chartreuse to
#' forest green for graphs. These are from the RColorBrewer package, color set
#' "YlGn", just to give credit where it's due, but they can be any number in
#' length rather than only up to 9 colors.
#'
#' @param ncolors number of colors desired
#' @param shade the shade of colors to use, which can be "regular" (default),
#'   "darker", or "lighter"
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
                       shade = "regular"){
   greens(ncolors = ncolors, shade = shade)
}


#' Create a vector of purples
#'
#' \code{purples} is useful for getting a set of purples from lavender to
#' aubergine for graphs.
#'
#' @param ncolors number of colors desired
#' @param shade the shade of colors to use, which can be "regular" (default),
#'   "darker", or "lighter"
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
                    shade = "regular"){
   
   if(shade == "darker"){
      colPurple <- colorRampPalette(c("#9E9AC8", "#807DBA", 
                                      "#6A51A3", "#54278F", "#3F007D", 
                                      "#37005C", "#23003A"))
   } else if(shade == "lighter"){
      colPurple <- colorRampPalette(c("#D8DBF4", "#BCBDDC", "#9E9AC8", "#807DBA", 
                                      "#6A51A3", "#54278F"))
   } else {
      # Anything else, give them "regular". 
      colPurple <- colorRampPalette(c("#FCFBFD", "#EFEDF5", "#DADAEB",
                                      "#BCBDDC", "#9E9AC8", "#807DBA", 
                                      "#6A51A3", "#54278F", "#3F007D"))
   }
   
   return(colPurple(ncolors))
   
}


#' Create a vector of reds
#'
#' \code{reds} is useful for getting a set of reds from pink to
#' brick for graphs.
#'
#' @param ncolors number of colors desired
#' @param shade the shade of colors to use, which can be "regular" (default),
#'   "darker", or "lighter"
#'
#' @return a character vector of colors
#' @export
#'
#' @examples
#' # Create a set of 10 reds. 
#' reds(10)
#'
#' # Try using scales::show_col() to visualize the colors, ex:
#' scales::show_col(reds(10))
#' scales::show_col(reds(10, shade = "lighter"))
#' 
reds <- function(ncolors, 
                 shade = "regular"){
   
   if(shade == "darker"){
      colRed <- colorRampPalette(c("#E41A1C", "#480808"))
   } else if(shade == "lighter"){
      colRed <- colorRampPalette(c("#FEE8E8", "#E41A1C"))
   } else {
      # Anything else, give them "regular". 
      colRed <- colorRampPalette(c("#FDC3C3", "#E41A1C", "#480808"))
   }
   
   return(colRed(ncolors))
   
}



#' Make a vector of colors that match the "green to red" option for highlighting
#' GMR values
#'
#' @return a named character vector
#' @export
#'
green_to_red <- function(){
   c("negligible" = "#C7FEAC", # light green
     "weak" = "#FFFF95", # light yellow
     "moderate" = "#FFDA95", # light orange
     "strong" = "#FF9595")   # light red
}


#' Make a vector of colors that match the "yellow to red" option for
#' highlighting GMR values
#'
#' @return a named character vector
#' @export
#'
yellow_to_red <- function(){
   c("negligible" = "white",
     "weak" = "#FFFF95", # light yellow
     "moderate" = "#FFDA95", # light orange
     "strong" = "#FF9595") # light red
   
}


