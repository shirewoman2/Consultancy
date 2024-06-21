#' Format common PBPK model parameters to have appropriate subscripts and
#' superscripts. UNDER CONSTRUCTION.
#'
#' @param DF a data.frame with a column containing, e.g., "ka (h^-1)", "fu,p",
#'   or other PBPK model parameters with syntax that matches what's in the
#'   column "Notes" in the object AllExpDetails, which is included with this
#'   package and is used extensively for figuring out how simulations were set
#'   up.
#' @param FT optionally supply a flextable if you've already got one started.
#'   This MUST be the flextable version of DF because DF is what we'll use to
#'   figure out which cells should be changed to what values and NOT the
#'   flextable itself. If you don't supply a flextable for FT, we'll make DF
#'   into a generic flextable and use that, and that -- NOT supplying anything
#'   for FT -- is actually the preferable option. Honestly, we're tempted to
#'   just not let you supply a separate flextable and ONLY use DF to make the
#'   output, but we're trying to be flexible here. So please play nice and ONLY
#'   supply a value for FT that has EVERYTHING in the same position as it is in
#'   DF.
#' @param parameter_column the name of the column in DF that contains your PK
#'   parameters, unquoted. For example, supply \code{parameter_column = Notes}
#'
#' @return a flextable
#' @export
#'
#' @examples
#' DF <- data.frame(Parameter = c("MW", "ka (h^-1)",
#'                                "fu,p", "fu,gut", "fu,mic CYP3A4"),
#'                   Value = c(350, 0.5, 0.01, 0.8, 1))
#'
#' format_scripts(DF = DF)
#' 
format_scripts <- function(DF, 
                           FT = NA,
                           parameter_column){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   
   # Main body of function ----------------------------------------------------
   
   if(class(FT) %in% "logical"){
      # logical when NA
      FT <- flextable::flextable(DF)
   }
   
   # Setting up for nonstandard evaluation
   parameter_column <- rlang::enquo(parameter_column)
   
   if("Parameter" %in% names(DF) & 
      as_label(parameter_column) != "Parameter"){
      DF <- DF %>% rename(Parameter_orig = Parameter)
   }
   
   DF <- DF %>%
      mutate(Parameter = {{parameter_column}})
   
   FT <- FT %>% 
      compose(i = which(DF$Parameter == "ka (h^-1)"), 
              j = which(names(DF) == "Parameter"), 
              part = "body", 
              value = as_paragraph("k", as_sub("a"), " (h", as_sup("-1"), ")")) %>% 
      
      compose(i = which(DF$Parameter == "fu,p"), 
              j = which(names(DF) == "Parameter"), 
              part = "body", 
              value = as_paragraph("fu", as_sub("p"))) %>% 
      
      compose(i = which(DF$Parameter == "fu,gut"), 
              j = which(names(DF) == "Parameter"), 
              part = "body", 
              value = as_paragraph("fu", as_sub("gut"))) %>% 
      
      compose(i = which(DF$Parameter == "tlag (h)"), 
              j = which(names(DF) == "Parameter"), 
              part = "body", 
              value = as_paragraph("t", as_sub("lag"), " (h)")) %>% 
      
      compose(i = which(str_detect(DF$Parameter, "^fu,mic")), 
              j = which(names(DF) == "Parameter"), 
              part = "body", 
              value = as_paragraph("fu", as_sub("mic"), 
                                   sub("fu,mic", "", 
                                       DF[which(str_detect(DF$Parameter, "fu,mic")), 
                                          which(names(DF) == "Parameter")]))) %>% 
      
      compose(i = which(str_detect(DF$Parameter, "^Ki")), 
              j = which(names(DF) == "Parameter"), 
              part = "body", 
              value = as_paragraph("K", as_sub("i"), 
                                   sub("^Ki", "", 
                                       DF[which(str_detect(DF$Parameter, "fu,mic")), 
                                          which(names(DF) == "Parameter")])))
   
   return(FT)
}


