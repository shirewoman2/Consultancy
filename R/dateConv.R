#' Convert dates from numbers to Date
#'
#' Since data imported from Excel or simply manipulated within R often changes
#' date and time data to numeric, \code{dateConv} converts dates from decimal
#' numbers to Date.
#'
#' @param x A numeric string
#' @param dataSource Either "Excel" (default), "Unix" or "R" depending on
#'   where the data are coming from. Excel data have an origin of Dec. 30, 1899
#'   whereas Unix and R data have an origin of Jan. 1, 1970.
#' @examples
#' dateConv(43252)
#' # "2018-06-01"
#'
#' dateConv(18526, dataSource = "R")
#' # "2020-09-21"
#'
#' @return Returns a date object
#' @export
#'

dateConv <- function(x, dataSource = "Excel") {

      if(dataSource %in% c("Excel", "Unix", "R") == FALSE){
            stop("Invalid selection for dataSource. Valid selections are 'Excel', 'Unix', or 'R'.")
      }

      output <- as.Date(as.numeric(x),
                        origin = ifelse(dataSource == "Excel",
                                        "1899-12-30", "1970-01-01"))
      return(output)
}



