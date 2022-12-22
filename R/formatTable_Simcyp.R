#' Format tables according to Simcyp Consultancy Team specfications
#'
#' \code{formatTable_Simcyp} was designed to work with output from
#' \code{\link{pksummary_table}} and \code{\link{pksummary_mult}}, and it
#' formats tables so that the column headings and the first column are bold, and
#' the second through the last columns are centered. Column headings with, e.g.,
#' "AUCinf" or "Cmax" will have the "inf" or the "max" subscripted, and the
#' table will automatically expand to fit the contents.
#'
#' @param DF a data.frame, usually output from \code{\link{pksummary_table}} or
#'   \code{\link{pksummary_mult}}
#' @param fontsize the numeric font size for the output. Default is 11 point.
#' @param shading_column If you would like to alternate the shading of the rows
#'   in the output data.frame, supply here the unquoted name of the column to
#'   check for when to change the shading; everytime that column's value
#'   changes, the shading will alternate between white and light gray. For
#'   example, if you have a table with PK values for multiple files and you have
#'   more than one row per file (an example of this would be the output from the
#'   function \code{\link{pksummary_mult}}), if you set \code{shading_column =
#'   File}, the shading of the rows will alternate between white and light gray
#'   whenever the file changes.
#'
#' @return
#' @export
#'
#' @examples
#' MyData <- data.frame(ColA = rep(LETTERS[1:3], each = 2),
#'                      ColB = 1:6)
#' formatTable_Simcyp(MyData)
#' 
formatTable_Simcyp <- function(DF, 
                               fontsize = 11, 
                               shading_column){
    
    # Error catching ---------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
             call. = FALSE)
    }
    
    if(nrow(DF) == 0){
        stop("Please check your input. The data.frame you supplied doesn't have any rows.", 
             call. = FALSE)
    }
    
    # Setting things up for nonstandard evaluation ----------------------------
    shading_column <- rlang::enquo(shading_column)
    
    
    # Main body of function -------------------------------------------------
    FT <- DF %>% 
        flextable::flextable() %>% 
        
        # Make the header bold
        flextable::bold(part = "header") %>%
        
        # Make the 1st column bold
        flextable::bold(j = 1, part = "body") %>% 
        
        # center the header row
        flextable::align(align = "center", part = "header") %>% 
        
        # center the columns with numbers, i.e., the 2nd column through the
        # penultimate column
        flextable::align(align = "center", j = 2:ncol(DF)) %>%
        
        # Set the font size
        flextable::fontsize(part = "all", size = fontsize) %>% 
        
        # setting up which borderlines to show
        flextable::border_remove() %>% 
        flextable::border_inner_v(part = "all", 
                                  border = officer::fp_border(width = 0.5)) %>% 
        flextable::border_outer(border = officer::fp_border(width = 0.5)) %>% 
        
        # making the width autofitted to contents
        flextable::set_table_properties(width = 1, layout = "autofit")
    
    # Optionally including shading whenever the shading column changes
    if(as_label(shading_column) != "<empty>"){
        
        ShadeCol <- DF %>% pull(!!shading_column)
        
        ShadeChange <- which(ShadeCol[1:(length(ShadeCol) - 1)] != 
                                 ShadeCol[2:nrow(DF)]) + 1
<<<<<<< HEAD
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
        }
        
=======
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
>>>>>>> master
        ShadeRows <- which(DF$Shade)
        FT <- FT %>% 
            flextable::bg(i = ShadeRows, bg = "#F2F2F2")
    }
    
    # Dealing with subscripts
    ColNames <- names(DF)
    ColNames <- sub("AUCt ", "AUC~t~ ", ColNames)
    ColNames <- sub("AUCt$", "AUC~t~", ColNames)
    ColNames <- sub("AUCtau", "AUC~tau~", ColNames)
    ColNames <- sub("Cmax", "C~max~", ColNames)
    ColNames <- sub("Cmin", "C~min~", ColNames)
    ColNames <- sub("tmax", "t~max~", ColNames)
    ColNames <- sub("tlag", "t~lag~", ColNames)
    ColNames <- sub(" ka ", " k~a~ ", ColNames)
    ColNames <- sub("^ka ", "k~a~ ", ColNames)
    ColNames <- sub(" fa ", " f~a~ ", ColNames)
    ColNames <- sub("^fa ", "f~a~ ", ColNames)
    ColNames <- sub(" fh ", " f~h~ ", ColNames)
    ColNames <- sub("^fh ", "f~h~ ", ColNames)
    ColNames <- sub(" fg ", " f~g~ ", ColNames)
    ColNames <- sub("^fg ", "f~g~ ", ColNames)
    ColNames <- sub("Indmax", "Ind~max~", ColNames)
    ColNames <- sub("Emax", "E~max~", ColNames)
    ColNames <- sub("IndC50", "IndC~50~", ColNames)
    ColNames <- sub("EC50", "EC~50~", ColNames)
    
    ColNames <- str_split(ColNames, pattern = "~", simplify = T)
    
    if(ncol(ColNames) == 3){
        for(i in which(ColNames[,2] != "")){
            FT <- FT %>% 
                flextable::compose(part = "header",
                                   j = i,
                                   value = flextable::as_paragraph(
                                       ColNames[i, 1],
                                       flextable::as_sub(ColNames[i, 2]), 
                                       ColNames[i, 3]))
        }
    }
    
    return(FT)
    
}


