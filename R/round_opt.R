#' INTERNAL: Rounding function
#'
#' @param x vector to be rounded
#' @param round_fun which function to use to round. Options: "round", "signif",
#'   "consultancy", "none".
#' @param is_this_for_Word TRUE or FALSE for whether this is being applied to a
#'   Word file in this situation
#' @param out_class data class of the ouput; defaults to "numeric" but
#'   "character" is acceptable and will often work better if this is for a Word
#'   file or is being supplied to \code{formatTable_Simcyp}. NB: If you chose a
#'   rounding option of "Consultancy", this will return character data no matter
#'   what.
#'
#' @return rounded numbers that might, after running this, be character data
#' 

round_opt <- function(x, 
                      round_fun, 
                      is_this_for_Word = FALSE, 
                      out_class = "numeric"){
   
   round_fun <- ifelse(is.na(round_fun), "consultancy", tolower(round_fun))
   round_fun <- ifelse(str_detect(tolower(round_fun), "word") & 
                          is_this_for_Word, 
                       "consultancy", round_fun)
   
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
   if(out_class == "character"){
      Out <- as.character(Out)
   }
   
   return(Out)
}
