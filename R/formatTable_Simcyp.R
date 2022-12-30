#' Format tables according to Simcyp Consultancy Team specifications
#'
#' \code{formatTable_Simcyp} makes a nicely formatted table from a data.frame or
#' tibble. It was primarily designed to work with output from
#' \code{\link{pksummary_table}} and \code{\link{pksummary_mult}}, so, by
#' default, it formats tables so that the column headings and the first column
#' are bold, and the second through the last columns are centered. Column
#' headings with, e.g., "AUCinf" or "Cmax" will have the "inf" or the "max"
#' subscripted, and the table will automatically expand to fit the contents. You
#' can save the output to a Word file with the argument \code{save_table}.
#'
#' @param DF a data.frame, usually output from \code{\link{pksummary_table}} or
#'   \code{\link{pksummary_mult}}
#' @param fontsize the numeric font size for the output table. Default is 11
#'   point.
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
#' @param bold_1st_column TRUE (default) or FALSE for whether to make the first
#'   column bold
#' @param center_1st_column TRUE or FALSE (default) for whether to make the
#'   alignment of the first column centered
#' @param highlight_cells optionally specify cells in the table to be
#'   highlighted with a numeric vector where the 1st number is the row number
#'   and the 2nd number is the column number (just like regular row and column
#'   specifications in R). For example, \code{highlight_cells = c(1, 2)} will
#'   make the cell in row 1 and column 2 highlighted. Use "0" for the row number
#'   if you want to highlight something in the header row, and use NA in place
#'   of a row or column number to mean that you want to highlight everything in
#'   that row or column. If you want to specify multiple places to highlight,
#'   use a list of numeric vectors. Please see the examples at the bottom of the
#'   help file.
#' @param highlight_color color to use for highlighting; default is yellow.
#'   Color can be specified using any R-friendly color name or hex code, e.g.,
#'   "red" or "#D8212D".
#' @param save_table optionally save the output table by supplying a file name
#'   in quotes here, e.g., "My nicely formatted table.docx". If you leave off
#'   the file extension, we'll assume you want it to be ".docx". If there is a
#'   column titled "File" in your table, we'll add a caption listing which files
#'   were included. \strong{WARNING:} SAVING TO WORD DOES NOT WORK ON
#'   SHAREPOINT. This is a Microsoft permissions issue, not an R issue. If you
#'   try to save on SharePoint, you will get a warning that R will save your
#'   file instead to your local (not OneDrive) Documents folder.
#'
#' @return
#' @export
#'
#' @examples
#' MyData <- data.frame(ColA = rep(LETTERS[1:3], each = 2),
#'                      ColB = 1:6)
#' formatTable_Simcyp(MyData)
#' formatTable_Simcyp(MyData, bold_1st_column = FALSE)
#' formatTable_Simcyp(MyData, center_1st_column = TRUE)
#' formatTable_Simcyp(MyData, fontsize = 18)
#' formatTable_Simcyp(MyData, shading_column = ColA)
#'
#' # Highlighting examples
#' ## Highlight row 1, column 2
#' formatTable_Simcyp(MyData, highlight_cells = c(1, 2))
#' 
#' ## Highlight all of column 2
#' formatTable_Simcyp(MyData, highlight_cells = c(NA, 2))
#' 
#' ## Highlight all of row 1
#' formatTable_Simcyp(MyData, highlight_cells = c(1, NA))
#' 
#' ## Highlight the 2nd column in the header
#' formatTable_Simcyp(MyData, highlight_cells = c(0, 2))
#' 
#' ## Set the highlight color to light blue instead of yellow
#' formatTable_Simcyp(MyData, highlight_cells = c(1, NA), 
#'                    highlight_color = "lightblue")
#'                    
#' ## Highlighting multiple cells
#' formatTable_Simcyp(MyData, highlight_cells = list(c(1, 2), c(3,1), c(5, 2)),
#'                    highlight_color = "lightblue")
#' 
#' # Saving
#' ## Adding a column called "File" so that there will be a caption in the Word
#' ## document listing which files were included in the table.
#' MyData$File <- "abc-1a.xlsx"
#' formatTable_Simcyp(MyData, save_table = "My data.docx")
#' 
formatTable_Simcyp <- function(DF, 
                               fontsize = 11, 
                               shading_column, 
                               bold_1st_column = TRUE,
                               center_1st_column = FALSE,
                               highlight_cells = NA, 
                               highlight_color = "yellow",
                               save_table = NA){
    
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
    
    if(any(complete.cases(highlight_cells))){
        if(class(highlight_cells) == "numeric"){
            highlight_cells <- list(highlight_cells[1:2])
        }
        
        if(any(sapply(highlight_cells, length) < 2)){
            warning("For highlighting cells, you must specify a row and a column for everything you want to be highlighted, and you have only specified one number for at least one of the items you asked to be highlighted. We don't know which rows or columns to highlight without that second number, so nothing will be highlighted.", 
                    call. = FALSE)
            highlight_cells <- NA
        }
    }
    
    # Setting things up for nonstandard evaluation ----------------------------
    shading_column <- rlang::enquo(shading_column)
    
    
    # Main body of function -------------------------------------------------
    FT <- DF %>% 
        flextable::flextable() %>% 
        
        # Make the header bold
        flextable::bold(part = "header")
    
    # Optionally make the 1st column bold
    if(bold_1st_column){
        FT <- FT %>% 
            flextable::bold(j = 1, part = "body")
    }
    
    FT <- FT %>% 
        
        # center the header row
        flextable::align(align = "center", part = "header") %>% 
        
        # center the columns with numbers, i.e., the 2nd column through the
        # penultimate column
        flextable::align(align = "center", 
                         j = ifelse(center_1st_column, 1, 2):ncol(DF)) %>%
        
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
        
        ShadeRows <- which(DF$Shade)
        FT <- FT %>% 
            flextable::bg(i = ShadeRows, bg = "#F2F2F2")
    }
    
    # Optionally highlight specific cells
    if(any(complete.cases(highlight_cells))){
        for(i in 1:length(highlight_cells)){
            
            FT <- FT %>% 
                flextable::bg(i = switch(paste(is.na(highlight_cells[[i]][1]), 
                                               highlight_cells[[i]][1] == 0), 
                                         "TRUE NA" = NULL, 
                                         "FALSE TRUE" = 1, # this is when the row should be the only row in the header
                                         "FALSE FALSE" = highlight_cells[[i]][1]), 
                              j = switch(as.character(is.na(highlight_cells[[i]][2])), 
                                         "TRUE" = NULL, 
                                         "FALSE" = highlight_cells[[i]][2]), 
                              bg = highlight_color, 
                              part = ifelse(complete.cases(highlight_cells[[i]][1]) & 
                                                highlight_cells[[i]][1] == 0, 
                                            "header", "body"))   
        }
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
    
    # Saving --------------------------------------------------------------
    if(complete.cases(save_table)){
        
        # Format the file name appropriately, including making the extension be
        # docx, even if they specified something else.
        save_table <- ifelse(str_detect(save_table, "\\..*$"), 
                             sub("\\..*", ".docx", save_table), 
                             paste0(save_table, ".docx"))
        
        # Now that the file should have an appropriate extension, check what
        # the path and basename should be.
        OutPath <- dirname(save_table)
        save_table <- basename(save_table)
        
        # May need to change the working directory temporarily, so
        # determining what it is now
        CurrDir <- getwd()
        
        OutPath <- dirname(save_table)
        if(OutPath == "."){
            OutPath <- getwd()
        }
        
        # Check for whether they're trying to save on SharePoint, which DOES
        # NOT WORK. If they're trying to save to SharePoint, instead, save
        # to their Documents folder.
        
        # Side regex note: The myriad \ in the "sub" call are necessary b/c
        # \ is an escape character, and often the SharePoint and Large File
        # Store directory paths start with \\\\.
        if(str_detect(sub("\\\\\\\\", "//", OutPath), SimcypDir$SharePtDir)){
            
            OutPath <- paste0("C:/Users/", Sys.info()[["user"]], 
                              "/Documents")
            warning(paste0("You have attempted to use this function to save a Word file to SharePoint, and Microsoft permissions do not allow this. We will attempt to save the ouptut to your Documents folder, which we think should be ", 
                           OutPath,
                           ". Please copy the output to the folder you originally requested or try saving locally or on the Large File Store."), 
                    call. = FALSE)
        }
        
        LFSPath <- str_detect(sub("\\\\\\\\", "//", OutPath), SimcypDir$LgFileDir)
        
        if(LFSPath){
            # Create a temporary directory in the user's AppData/Local/Temp
            # folder.
            TempDir <- tempdir()
            
            # Upon exiting this function, delete that temporary directory.
            on.exit(unlink(TempDir))
            
        }
        
        FileName <- basename(save_table)
        
        rmarkdown::render(system.file("rmarkdown/templates/savetablesimcyp/skeleton/skeleton.Rmd",
                                      package="SimcypConsultancy"), 
                          output_dir = switch(as.character(LFSPath), 
                                              "TRUE" = TempDir,
                                              "FALSE" = OutPath),
                          output_file = FileName, 
                          quiet = TRUE)
        # Note: The "system.file" part of the call means "go to where the
        # package is installed, search for the file listed, and return its
        # full path.
        
        if(LFSPath){
            file.copy(file.path(TempDir, FileName), OutPath, overwrite = TRUE)
        }
    }
    
    return(FT)
    
}


