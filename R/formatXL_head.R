#' Save an Excel file with standard formatting for the header row
#'
#' This function saves a data.frame to an Excel file with the header row in bold
#' face, centered both horizontally and vertically, and with wrapped text. It
#' also sets the column widths to a reasonable guess based on the data you're
#' writing. For more details, please see \code{\link{formatXL}}.
#'
#' @param DF input data.frame
#' @param file file name (character)
#' @param sheet sheet name (character). Defaults to the name of the supplied
#'   data.frame if no other name is supplied.
#'
#' @return This does not return any R objects; instead, it saves an Excel file.
#' @examples
#' data(iris)
#'
#' formatXL_head(DF = iris, file = "test.xlsx", sheet = "iris1")
#'
#'
#' @return saves a formatted Excel file 
#' @export
#'
#'
formatXL_head <- function(DF, file, sheet = NA){
    
    # Error catching ---------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    # All the columns must be named for this to work well. Checking that.
    if(any(is.na(names(DF)))){
        stop("All the columns in your data.frame must be named.",
             call. = FALSE)
    }
    
    # Main body of function ---------------------------------------------------
    
    GoodWidths <- guess_col_widths(DF)
    
    ## Setting column widths, applying styles, and saving --------------------
    formatXL(DF %>% ungroup(), file = file, sheet = sheet,
             colWidth = list(colNum = NULL, width = GoodWidths),
             styles = list(list(rows = 0,
                                font = list(bold = TRUE),
                                textposition = list(alignment = "middle",
                                                    wrapping = TRUE))))
    
}




