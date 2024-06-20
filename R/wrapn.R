#' INTERNAL PACKAGE USE. Wrap warning text and add a carriage return at the end.
#'
#' @param x warning text to wrap
#'
#' @return a character string 
#'
#' @examples
#' warning(wrapn("This is a long warning that should be wrapped nicely with pretty line breaks and end with a carriage return."))
wrapn <- function(x){
   paste0(str_wrap(x), "\n")
}

