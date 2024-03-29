#' Convert times from numbers to POSIXct
#'
#' Since data imported from Excel often changes date and time data to numeric,
#' \code{timeConv} converts dates from decimal numbers to Date with a time zone
#' of "UTC" unless otherwise specified.
#'
#' @param x A numeric string
#' @param dataSource source of the data. Either "Excel" (default), R, or "Unix"
#'   depending on where the data are coming from. Excel data have an origin of
#'   Dec. 30, 1899 whereas Unix or R data have an origin of Jan. 1, 1970.
#' @return Returns a POSIXct object
#'
#' @examples
#' timeConv(43252.5)
#' # "2018-06-01 12:00:00 UTC"
#'
#' timeConv(18526.2356, dataSource = "R")
#' # "2020-09-21 05:39:15 UTC"
#'
#' @export
#'

timeConv <- function(x, tz = "UTC", dataSource = "Excel") {
    
	# Check whether tidyverse is loaded
	if("package:tidyverse" %in% search() == FALSE){
		stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
	}

    if(dataSource %in% c("Excel", "Unix", "R") == FALSE){
        stop("Invalid selection for dataSource. Valid selections are 'Excel', 'Unix', or 'R'.",
             call. = FALSE)
    }
    
    output <-
        lubridate::ymd_hms(
            format(
                as.POSIXct(
                    as.Date(
                        x,
                        origin =
                            ifelse(dataSource == "Excel",
                                   "1899-12-30", "1970-01-01")),
                    tzone = tz), tz = tz))
    
    return(output)
}


