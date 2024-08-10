#' Format a table to look nice in a Word file or pdf - UNDER CONSTRUCTION
#'
#' \code{format_table_simple} is meant to for use with writing compound summary
#' pdfs. It includes minimal error catching at this point. 
#'
#' @param DF a data.frame or a flextable
#' @param shading_column If you would like to alternate the shading of the rows
#'   in the output table, supply here the unquoted name of the column to check
#'   for when to change the shading; every time that column's value changes, the
#'   shading will alternate between white and light gray. For example, if you
#'   have a table with PK values for multiple files and you have more than one
#'   row per file (an example of this would be the output from the function
#'   \code{\link{pksummary_mult}}), setting \code{shading_column = File} will
#'   cause the shading of the rows to alternate between white and light gray
#'   whenever the file changes. Please see the examples at the bottom of this
#'   help file.
#' @param merge_shaded_cells TRUE (default) or FALSE for whether to merge the
#'   cells that have the same shade. This only applies when one of the columns
#'   in the input data.frame is used for deciding when to alternate shading,
#'   that is, \code{shading_column} has a value.
#'
#' @return a formatted table
#' @export
#'
#' @examples
#' # None yet 
#' 
format_table_simple <- function(DF, 
                        shading_column, 
                        merge_shaded_cells = TRUE){
   
   # flextable function w/the specs I want. Note: I had a lot of trouble when I
   # tried to use a modified version of formatTable_Simcyp here. I kept getting
   # latex errors about extra carriage returns. Could not figure out the problem!
   
   
   # Setting things up for nonstandard evaluation ----------------------------
   shading_column <- rlang::enquo(shading_column)
   
   # Main body of function ---------------------------------------------------
   if("Parameter" %in% names(DF)){
      FT <- format_scripts(DF, parameter_column = Parameter)
   } else {
      FT <- format_scripts(DF)
   }
   
   # Setting up shading ----------------------------------------------------
   
   if(as_label(shading_column) != "<empty>"){
      
      ShadeCol <- DF %>% pull(!!shading_column)
      
      ShadeChange <- which(ShadeCol[1:(length(ShadeCol) - 1)] != 
                              ShadeCol[2:nrow(DF)]) + 1
      if(length(ShadeChange) == 0){
         DF$Shade <- FALSE
      } else {
         ShadeRows <- ShadeChange[seq(1, length(ShadeChange), by = 2)]
         if(length(ShadeChange) > 1){
            NoShadeRows <- ShadeChange[seq(2, length(ShadeChange), by = 2)]
         } else {
            NoShadeRows <- 1
         }
         DF$Shade <- as.logical(NA)
         DF$Shade[ShadeRows] <- TRUE
         DF$Shade[NoShadeRows] <- FALSE
         DF <- DF %>% fill(Shade, .direction = "down") %>% 
            mutate(Shade = ifelse(is.na(Shade), FALSE, Shade))
         
         ShadeRows <- which(DF$Shade)
         FT <- FT %>% 
            flextable::bg(i = ShadeRows, bg = "#F2F2F2") %>% 
            flextable::bg(i = NoShadeRows, bg = "white") %>% 
            flextable::bg(part = "header", bg = "white")
         
         if(merge_shaded_cells){
            FT <- FT %>% 
               flextable::merge_v(j = which(names(DF) == as_label(shading_column)))
         }
      }
      
   }
   
   FT <- FT %>% 
      font(part = "all",
           # fontname = "fourier") %>% # doesn't work. I think the font has to be something available in Word b/c I think the folder loation is not the same as for latex.
           fontname = "Palatino Linotype") %>%
      bold(part = "header") %>% 
      width(width = (7 / ncol(DF))) %>%
      fix_border_issues()
   
   return(FT)
}
