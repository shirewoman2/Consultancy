#' INTERNAL: Rounding function 
#'
#' @param x vector to be rounded
#' @param round_fun which function to use to round
#'
#' @return rounded numbers that might, after running this, be character data
#'
#' @examples
#' # None.
#' 

round_opt <- function(x, round_fun){
    
    round_fun <- ifelse(is.na(round_fun), "consultancy", tolower(round_fun))
    round_fun <- ifelse(str_detect(tolower(round_fun), "word"), "none", round_fun)
    
    suppressWarnings(
        NumDig <- as.numeric(str_trim(sub("signif(icant)?|round", "", round_fun)))
    )
    
    if(str_detect(round_fun, "signif|round") & 
       !str_detect(round_fun, "[0-9]{1,}")){
        warning("You appear to want some rounding, but we're not sure how many digits. We'll use 3 for now, but please check the help file for appropriate input for the argument `rounding`.", 
                call. = FALSE)
        NumDig <- 3
    }
    
    round_fun <- str_trim(sub("[0-9]{1,}", "", round_fun))
    round_fun <- ifelse(str_detect(round_fun, "signif"), "signif", round_fun)
    
    Out <- switch(round_fun, 
                  "round" = round(x, digits = NumDig),
                  "signif" = signif(x, digits = NumDig), 
                  "consultancy" = round_consultancy(x), 
                  "none" = x)
    
    return(Out)
}
