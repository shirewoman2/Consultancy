#' Round a number or vector of numbers up or down to nice intervals
#'
#' Round numbers to nice intervals with the following options: \describe{
#' \item{round_up}{round up to the nearest power of 10} \item{round_down}{round
#' down to the nearest power of 10} \item{round_up_nice}{round up to the nearest
#' "nice" value. From
#' \url{http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x)}
#' } \item{round_up_unit}{round up x to the nearest b}
#' \item{round_down_unit}{round x down to the nearest b} \item{round_unit}{round
#' x up or down to the nearest b; direction depends on how close x is to b}
#' \item{log10_breaks}{creates a vector evenly spaced on a log 10 scale: 10
#' points across each order of magnitude} }
#'
#' @param x the number or vector of numbers to be rounded
#' @param b the unit to round the number to (applies to
#'   \code{\link{round_up_unit}} and \code{\link{round_down_unit}} only)
#'
#' @return a rounded number
#' @export
#'
#' @examples
#' round_up(c(4, 1, 101, 99, 1002))
#' round_down(c(4, 1, 101, 99, 1002))
#' round_up_nice(0.24562)
#' round_up_nice(2936.24562)
#' round_up_nice(c(0.2356, 1.52, 102549.2))
#' round_up_unit(33.5, 2)
#' round_up_unit(33.5, 5)
#' round_up_unit(x = c(4, 1, 101, 99, 1002), b = 5)
#' round_down_unit(1.23, 0.1)
#' round_down_unit(x = c(4, 1, 101, 99, 1002), b = 25)
#'
#' 
round_up      <- function(x) {
    # function to round up to the nearest power of 10
    
    ## NOTE TO DEVELOPERS: If you change any of the text for the description,
    ## please copy and paste that text above all the other functions included in
    ## this script. That way, when people search for any of the functions here,
    ## they'll see the same help file.
    
    10^ceiling(log10(x))
}

#' Round a number or vector of numbers up or down to nice intervals
#'
#' Round numbers to nice intervals with the following options: \describe{
#' \item{round_up}{round up to the nearest power of 10} \item{round_down}{round
#' down to the nearest power of 10} \item{round_up_nice}{round up to the nearest
#' "nice" value. From
#' \url{http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x)}
#' } \item{round_up_unit}{round up x to the nearest b}
#' \item{round_down_unit}{round x down to the nearest b} \item{round_unit}{round
#' x up or down to the nearest b; direction depends on how close x is to b}
#' \item{log10_breaks}{creates a vector evenly spaced on a log 10 scale: 10
#' points across each order of magnitude} }
#'
#' @param x the number or vector of numbers to be rounded
#' @param b the unit to round the number to (applies to
#'   \code{\link{round_up_unit}} and \code{\link{round_down_unit}} only)
#'
#' @return a rounded number
#' @export
#'
#' @examples
#' round_up(c(4, 1, 101, 99, 1002))
#' round_down(c(4, 1, 101, 99, 1002))
#' round_up_nice(0.24562)
#' round_up_nice(2936.24562)
#' round_up_nice(c(0.2356, 1.52, 102549.2))
#' round_up_unit(33.5, 2)
#' round_up_unit(33.5, 5)
#' round_up_unit(x = c(4, 1, 101, 99, 1002), b = 5)
#' round_down_unit(1.23, 0.1)
#' round_down_unit(x = c(4, 1, 101, 99, 1002), b = 25)
#'
#'
round_down    <- function(x) 10^floor(log10(x))



#' Round a number or vector of numbers up or down to nice intervals
#'
#' Round numbers to nice intervals with the following options: \describe{
#' \item{round_up}{round up to the nearest power of 10} \item{round_down}{round
#' down to the nearest power of 10} \item{round_up_nice}{round up to the nearest
#' "nice" value. From
#' \url{http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x)}
#' } \item{round_up_unit}{round up x to the nearest b}
#' \item{round_down_unit}{round x down to the nearest b} \item{round_unit}{round
#' x up or down to the nearest b; direction depends on how close x is to b}
#' \item{log10_breaks}{creates a vector evenly spaced on a log 10 scale: 10
#' points across each order of magnitude} }
#'
#' @param x the number or vector of numbers to be rounded
#' @param b the unit to round the number to (applies to
#'   \code{\link{round_up_unit}} and \code{\link{round_down_unit}} only)
#'
#' @return a rounded number
#' @export
#'
#' @examples
#' round_up(c(4, 1, 101, 99, 1002))
#' round_down(c(4, 1, 101, 99, 1002))
#' round_up_nice(0.24562)
#' round_up_nice(2936.24562)
#' round_up_nice(c(0.2356, 1.52, 102549.2))
#' round_up_unit(33.5, 2)
#' round_up_unit(33.5, 5)
#' round_up_unit(x = c(4, 1, 101, 99, 1002), b = 5)
#' round_down_unit(1.23, 0.1)
#' round_down_unit(x = c(4, 1, 101, 99, 1002), b = 25)
#'
#'
round_up_nice <- function(x, nice=seq(1, 10, 0.1)) {    # function to round up to the nearest "nice" value (obtained
    if(length(x) == 1){
        10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
    } else {
        sapply(x,
               FUN = function(x){10^floor(log10(x)) *
                       nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]})
    }
}




#' Round a number or vector of numbers up or down to nice intervals
#'
#' Round numbers to nice intervals with the following options: \describe{
#' \item{round_up}{round up to the nearest power of 10} \item{round_down}{round
#' down to the nearest power of 10} \item{round_up_nice}{round up to the nearest
#' "nice" value. From
#' \url{http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x)}
#' } \item{round_up_unit}{round up x to the nearest b}
#' \item{round_down_unit}{round x down to the nearest b} \item{round_unit}{round
#' x up or down to the nearest b; direction depends on how close x is to b}
#' \item{log10_breaks}{creates a vector evenly spaced on a log 10 scale: 10
#' points across each order of magnitude} }
#'
#' @param x the number or vector of numbers to be rounded
#' @param b the unit to round the number to (applies to
#'   \code{\link{round_up_unit}} and \code{\link{round_down_unit}} only)
#'
#' @return a rounded number
#' @export
#'
#' @examples
#' round_up(c(4, 1, 101, 99, 1002))
#' round_down(c(4, 1, 101, 99, 1002))
#' round_up_nice(0.24562)
#' round_up_nice(2936.24562)
#' round_up_nice(c(0.2356, 1.52, 102549.2))
#' round_up_unit(33.5, 2)
#' round_up_unit(33.5, 5)
#' round_up_unit(x = c(4, 1, 101, 99, 1002), b = 5)
#' round_down_unit(1.23, 0.1)
#' round_down_unit(x = c(4, 1, 101, 99, 1002), b = 25)
#'
#'
round_up_unit <- function(x, b) { # round x up to nearest b
    
    subfun <- function(x){
        rounded <- round(x/b)*b
        if (rounded < x) {rounded <- rounded + b}
        return(rounded)
    }
    
    if(length(x) == 1){
        return(subfun(x))
    } else {
        return(sapply(x, FUN = subfun))
    }
}





#' Round a number or vector of numbers up or down to nice intervals
#'
#' Round numbers to nice intervals with the following options: \describe{
#' \item{round_up}{round up to the nearest power of 10} \item{round_down}{round
#' down to the nearest power of 10} \item{round_up_nice}{round up to the nearest
#' "nice" value. From
#' \url{http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x)}
#' } \item{round_up_unit}{round up x to the nearest b}
#' \item{round_down_unit}{round x down to the nearest b} \item{round_unit}{round
#' x up or down to the nearest b; direction depends on how close x is to b}
#' \item{log10_breaks}{creates a vector evenly spaced on a log 10 scale: 10
#' points across each order of magnitude} }
#'
#' @param x the number or vector of numbers to be rounded
#' @param b the unit to round the number to (applies to
#'   \code{\link{round_up_unit}} and \code{\link{round_down_unit}} only)
#'
#' @return a rounded number
#' @export
#'
#' @examples
#' round_up(c(4, 1, 101, 99, 1002))
#' round_down(c(4, 1, 101, 99, 1002))
#' round_up_nice(0.24562)
#' round_up_nice(2936.24562)
#' round_up_nice(c(0.2356, 1.52, 102549.2))
#' round_up_unit(33.5, 2)
#' round_up_unit(33.5, 5)
#' round_up_unit(x = c(4, 1, 101, 99, 1002), b = 5)
#' round_down_unit(1.23, 0.1)
#' round_down_unit(x = c(4, 1, 101, 99, 1002), b = 25)
#'
#'
round_down_unit <- function(x, b) { # round x down to nearest b
    subfun <- function(x){
        rounded <- round(x/b)*b
        if(rounded %% b == 0 & rounded < x){
            return(rounded)
        } else {
            if (rounded > x) {rounded <- rounded - b}
            return(rounded)
        }
    }
    
    if(length(x) == 1){
        return(subfun(x))
    } else {
        return(sapply(x, FUN = subfun))
    }
}





#' Round a number or vector of numbers up or down to nice intervals
#'
#' Round numbers to nice intervals with the following options: \describe{
#' \item{round_up}{round up to the nearest power of 10} \item{round_down}{round
#' down to the nearest power of 10} \item{round_up_nice}{round up to the nearest
#' "nice" value. From
#' \url{http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x)}
#' } \item{round_up_unit}{round up x to the nearest b}
#' \item{round_down_unit}{round x down to the nearest b} \item{round_unit}{round
#' x up or down to the nearest b; direction depends on how close x is to b}
#' \item{log10_breaks}{creates a vector evenly spaced on a log 10 scale: 10
#' points across each order of magnitude} }
#'
#' @param x the number or vector of numbers to be rounded
#' @param b the unit to round the number to (applies to
#'   \code{\link{round_up_unit}} and \code{\link{round_down_unit}} only)
#'
#' @return a rounded number
#' @export
#'
#' @examples
#' round_up(c(4, 1, 101, 99, 1002))
#' round_down_unit(1.23, 0.1)
#' round_down_unit(x = c(4, 1, 101, 99, 1002), b = 25)
#'
#'
round_unit <- function(x, b){
    ifelse((x %% b) <= b/2, 
           round_down_unit(x, b), 
           round_up_unit(x, b))
}





#' Rounding to match the SimcypConsultancy report template requirements
#'
#' If the number is > 100, round to the ones place. If the number is less, round
#' to 3 significant figures. Output is character data.
#'
#' @param x
#'
#' @return Returns character data with the appropriate number of digits
#' @export
#'
#' @examples
#' round_consultancy(1239.236)
#' round_consultancy(0.23513)
#' round_consultancy(c(1239.236, 0.23513))
#' 
round_consultancy <- function(x){
    if_else(x > 100, 
            as.character(round(x, 0)), 
            # This next convoluted bit will retain trailing zeroes since
            # "signif" alone will not
            formatC(signif(x, digits=3), digits=3, format="fg", flag="#"))
}



