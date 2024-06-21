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
   
   if(as_label(parameter_column) %in% names(DF) == FALSE){
      warning(wrapn(paste0("You specified the parameter column name to be `", 
                           as_label(parameter_column), 
                           "`, but that column is not present in DF. We cannot format the subscripts, superscripts, or special characters in your data.frame and will only be able to return a generic flextable object.")), 
              call. = FALSE)
      
      return(FT)
   }
   
   DF <- DF %>%
      mutate(Parameter = {{parameter_column}})
   
   FT <- FT %>% 
      
      # First, all the places where we replace the ENTIRE cell contents. These
      # don't need to be run in a loop b/c we replace EVERYTHING with the same
      # value, so, even if there were more than one place with, e.g., "ka
      # (h^-1)", ALL of those places would be replaced with the same thing.
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
      
      compose(i = which(DF$Parameter == "CLint,biliary"), 
              j = which(names(DF) == "Parameter"), 
              part = "body", 
              value = as_paragraph("CLint", as_sub("biliary"))) %>% 
      
      compose(i = which(DF$Parameter == "CLint,additional HLM"), 
              j = which(names(DF) == "Parameter"), 
              part = "body", 
              value = as_paragraph("CLint", as_sub("additional HLM"))) %>% 
      
      compose(i = which(DF$Parameter == "tlag (h)"), 
              j = which(names(DF) == "Parameter"), 
              part = "body", 
              value = as_paragraph("t", as_sub("lag"), " (h)")) %>% 
      
      # Special characters
      
      compose(i = which(str_detect(DF$Parameter, "uL")),
              j = which(names(DF) == "Parameter"), 
              part = "body", 
              value = as_paragraph(sub("uL", "\u03BCL", 
                                       DF[which(str_detect(DF$Parameter, "uL")), 
                                          which(names(DF) == "Parameter")])))
   
   # Next, all the places where there might be multiple matches b/c we're only
   # matching, e.g., the 1st part of the string and then need to fill in the
   # rest of the string with variable text.
   MultPieceVars <- c("^fu,mic", "^fu,mic.*\\(Ki\\)$", "^Ki")
   
   for(mpv in MultPieceVars){
      
      rows <- which(str_detect(DF$Parameter, mpv))
      
      for(r in rows){
         FT <- FT %>% 
            compose(i = r, 
                    j = which(names(DF) == "Parameter"), 
                    part = "body", 
                    value = switch(
                       mpv, 
                       "^fu,mic" = as_paragraph("fu", as_sub("mic"), 
                                                sub(mpv, "", 
                                                    DF[r, which(names(DF) == "Parameter")])), 
                       
                       "^fu,mic.*\\(Ki\\)$" = as_paragraph("fu", as_sub("mic"), 
                                                           gsub("^fu,mic|\\(Ki\\)", "", 
                                                                DF[r, which(names(DF) == "Parameter")]), 
                                                           "(K", as_sub("i"), ")"), 
                       
                       "^Ki" = as_paragraph("K", as_sub("i"), 
                                            sub(mpv, "", 
                                                DF[r, which(names(DF) == "Parameter")])))
            )
      }
   }
   
   return(FT)
}


