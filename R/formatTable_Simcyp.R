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
#' @param merge_shaded_cells TRUE (default) or FALSE for whether to merge the
#'   cells that have the same shade. This only applies when one of the columns
#'   in the input data.frame is used for deciding when to alternate shading,
#'   that is, \code{shading_column} has a value.
#' @param bold_cells optionally specify cells in the table to be in bold-face
#'   text with a numeric vector where the 1st number is the row number and the
#'   2nd number is the column number (just like regular row and column
#'   specifications in R). For example, \code{bold_cells = c(1, 2)} will make
#'   the cell in row 1 and column 2 bold face. Use "0" for the row number if you
#'   want to use bold face for something in the header row, and use NA in place
#'   of a row or column number to make everything in that row or column bold
#'   face. If you want to specify multiple places to use bold face, use a list
#'   of numeric vectors. By default, the header row and the 1st column will be
#'   bold. Set \code{bold_cells = NA} to make \emph{nothing} bold. Please see
#'   the examples at the bottom of the help file.
#' @param center_1st_column TRUE or FALSE (default) for whether to make the
#'   alignment of the first column centered
#' @param highlight_so_cutoffs optionally specify cutoffs for highlighting any
#'   simulated-to-observed ratios. Anything that is above those values or below
#'   the inverse of those values will be highlighted. To figure out what cells
#'   to highlight, this looks for a column titled "Statistic" or "Stat", then
#'   looks for what row contains "S/O" or "simulated (something something)
#'   observed" (as in, we'll use some wildcards to try to match your specific
#'   text). Next, it looks for any values in that same row that are above those
#'   cutoffs. This overrides anything else you specified for highlighting. The
#'   default is NA, for \emph{not} highlighting based on S/O value. Acceptable
#'   input for, say, highlighting values that are > 125\% or < 80\% of the
#'   observed and also, with a second color, values that are > 150\% or < 66\%
#'   would be: \code{highlight_so_cutoffs = c(1.25, 1.5)}. If you would like the
#'   middle range of values to be highlighted, include 1 in your cutoffs. For
#'   example, say you would like everything that's < 80\% or > 125\% to be
#'   highlighted red but you'd like the "good" values from 80\% to 125\% to be
#'   green, you can get that by specifying
#'   \code{highlight_so_cutoffs = c(1, 1.25)} and \code{highlight_so_colors =
#'   c("green", "red")}
#' @param highlight_so_colors optionally specify a set of colors to use for
#'   highlighting S/O values outside the limits you specified with
#'   \code{highlight_so_cutoffs}. Options: \describe{
#'
#'   \item{"yellow to red" (default)}{A range of light yellow to light orange to
#'   light red. If you have included 1 in your cutoffs and you leave
#'   \code{highlight_so_colors} with the default setting, values in the middle,
#'   "good" range of S/O values will be highlighted a light green.}
#'
#'   \item{"Lisa"}{Lisa's preferred set of colors: light green, yellow, and red
#'   designed to display values outside 1.25, 1.5, and 2 fold of unity, 
#'   respectively. If you include 1 in \code{highlight_so_cutoffs} like Lisa 
#'   often does, you'll get a darker green for "good" S/O values.}
#'
#'   \item{a character vector of specific colors}{Any R-acceptable colors, will
#'   work here, e.g., \code{highlight_so_colors = c("yellow", "orange", "red")}}.
#'   If you do specify your own bespoke colors, you'll need to make sure that 
#'   you supply one color for every value in \code{highlight_so_cutoffs}.} 
#' @param highlight_cells optionally specify cells in the table to be
#'   highlighted with a numeric vector where the 1st number is the row number
#'   and the 2nd number is the column number (just like regular row and column
#'   specifications in R). For example, \code{highlight_cells = c(1, 2)} will
#'   make the cell in row 1 and column 2 highlighted. Use "0" for the row number
#'   if you want to highlight something in the header row, and use NA in place
#'   of a row or column number to highlight everything in that row or column. If
#'   you want to specify multiple places to highlight, use a list of numeric
#'   vectors. Please see the examples at the bottom of the help file.
#' @param highlight_color color to use for highlighting; default is yellow.
#'   Color can be specified using any R-friendly color name or hex code, e.g.,
#'   "red" or "#D8212D".
#' @param save_table optionally save the output table by supplying a file name
#'   in quotes here, e.g., "My nicely formatted table.docx". If you leave off
#'   the file extension, we'll assume you want it to be ".docx". If there is a
#'   column titled "File" in your table, we'll add a caption listing which files
#'   were included.
#' @param title_document optionally specify a title for the Word document
#'   output. If you don't save the table, this will be ignored.
#'
#' @return a formatted table
#' @export
#'
#' @examples
#' MyPKTable <- tibble(Statistic = c("Simulated", "CV%", "Observed", "S/O"),
#'                         AUCinf = c(2756, 32.5, 1801, 1.53), 
#'                         Cmax = c(852, 45.8, 775, 1.1), 
#'                         `Half life` = c(7.75, 5.7, 6.05, 1.28))
#' formatTable_Simcyp(MyPKTable)
#' formatTable_Simcyp(MyPKTable, center_1st_column = TRUE)
#' formatTable_Simcyp(MyPKTable, fontsize = 18)
#' formatTable_Simcyp(MyPKTable, shading_column = Statistic)
#' 
#' # Highlighting examples
#' ## Highlighting S/O values outside bioequivalence of 125%.
#' formatTable_Simcyp(MyPKTable,
#'                    highlight_so_cutoffs = 1.25)
#' 
#' ## Highlighting S/O values with a few more colors based on the S/O.
#' formatTable_Simcyp(MyPKTable,
#'                    highlight_so_cutoffs = c(1.25, 1.5))
#' 
#' ## Highlighting S/O values and shading the "good" values green.
#' formatTable_Simcyp(MyPKTable,
#'                    highlight_so_cutoffs = c(1, 1.25, 1.5))
#' 
#' ## Highlight exactly the cells you want, e.g., row 1, column 2
#' formatTable_Simcyp(MyPKTable, highlight_cells = c(1, 2))
#' 
#' ## Highlight all of column 2
#' formatTable_Simcyp(MyPKTable, highlight_cells = c(NA, 2))
#' 
#' ## Highlight all of row 1
#' formatTable_Simcyp(MyPKTable, highlight_cells = c(1, NA))
#' 
#' ## Highlight the 2nd column in the header
#' formatTable_Simcyp(MyPKTable, highlight_cells = c(0, 2))
#' 
#' ## Set the highlight color to light blue instead of yellow
#' formatTable_Simcyp(MyPKTable, highlight_cells = c(1, NA),
#'                    highlight_color = "lightblue")
#' 
#' ## Highlighting multiple cells
# 'formatTable_Simcyp(MyPKTable, highlight_cells = list(c(1, 2), c(3,3), c(4, 2)),
#'                    highlight_color = "lightblue")
#' 
#' # Bold-face examples
#' ## Make only the cell in row 4 and column 2 be bold face. This will
#' ## override the default of having the header row and the 1st column in bold.
#' formatTable_Simcyp(MyPKTable, bold_cells = c(4, 2))
#' 
#' ## Make the cell in row 4 and column 2 be bold face AND include the original
#' ## defaults of having the header row and the 1st column be in bold.
#' formatTable_Simcyp(MyPKTable, bold_cells = list(c(0, NA), c(NA, 1), c(4, 2)))
#' 
#' # Saving
#' ## Adding a column called "File" so that there will be a caption in the Word
#' ## document listing which files were included in the table. Also setting 
#' ## the document title.
#' MyPKTable$File <- "abc-1a.xlsx"
#' formatTable_Simcyp(MyPKTable, 
#'                    highlight_so_cutoffs = c(1, 1.25, 1.5),
#'                    save_table = "My data.docx", 
#'                    title_document = "PK data")
#' 
#' 

formatTable_Simcyp <- function(DF, 
                               fontsize = 11, 
                               shading_column, 
                               merge_shaded_cells = TRUE,
                               bold_cells = list(c(0, NA), c(NA, 1)),
                               center_1st_column = FALSE,
                               highlight_so_cutoffs = NA, 
                               highlight_so_colors = "yellow to red",
                               highlight_cells = NA, 
                               highlight_color = "yellow",
                               save_table = NA, 
                               title_document = NA){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
           call. = FALSE)
   }
   
   if("data.frame" %in% class(DF) == FALSE){
      stop("Please check your input. The `formatTable_Simcyp` function only works with data.frames, and it looks like you have supplied some other type of data.", 
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
   
   if(any(complete.cases(bold_cells))){
      if(class(bold_cells) == "numeric"){
         bold_cells <- list(bold_cells[1:2])
      }
      
      if(any(sapply(bold_cells, length) < 2)){
         warning("For making cells bold, you must specify a row and a column for everything you want to have bold-face text, and you have only specified one number for at least one of the items you asked to be bold face. We don't know which rows or columns to make bold face without that second number, so we'll use the default settings and make the 1st column and the header row bold.", 
                 call. = FALSE)
         bold_cells <- list(c(0, NA), c(NA, 1))
      }
   }
   
   # Setting things up for nonstandard evaluation ----------------------------
   shading_column <- rlang::enquo(shading_column)
   
   
   # Main body of function -------------------------------------------------
   FT <- flextable::flextable(DF)
   
   # Optionally making things bold face
   if(any(sapply(bold_cells, complete.cases))){
      for(i in 1:length(bold_cells)){
         
         FT <- FT %>% 
            flextable::bold(i = switch(paste(is.na(bold_cells[[i]][1]), 
                                             bold_cells[[i]][1] == 0), 
                                       "TRUE NA" = NULL, 
                                       "FALSE TRUE" = 1, # this is when the row should be the only row in the header
                                       "FALSE FALSE" = bold_cells[[i]][1]), 
                            j = switch(as.character(is.na(bold_cells[[i]][2])), 
                                       "TRUE" = NULL, 
                                       "FALSE" = bold_cells[[i]][2]), 
                            part = ifelse(complete.cases(bold_cells[[i]][1]) & 
                                             bold_cells[[i]][1] == 0, 
                                          "header", "body"))   
      }
   }
   
   FT <- FT %>% 
      # center the header row
      flextable::align(align = "center", part = "header") %>% 
      
      # make everything have a white background (we'll fill in other shading later)
      flextable::bg(part = "all", bg = "white")
   
   # center the columns that contain numbers, i.e., the 2nd column through the
   # penultimate column and optionally center the 1st column
   if(center_1st_column == FALSE & ncol(DF) == 1){
      FT <- FT %>% flextable::align(align = "left")
   } else {
      FT <- FT %>% 
         flextable::align(align = "center", 
                          j = switch(paste(center_1st_column, ncol(DF) > 1),
                                     "TRUE TRUE" = 1:ncol(DF),
                                     "TRUE FALSE" = 1:ncol(DF), 
                                     "FALSE TRUE" = 2:ncol(DF)))
   }
   
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
         flextable::bg(i = ShadeRows, bg = "#F2F2F2") %>% 
         flextable::bg(i = NoShadeRows, bg = "white") %>% 
         flextable::bg(part = "header", bg = "white")
      
      if(merge_shaded_cells){
         FT <- FT %>% 
            flextable::merge_v(j = which(names(DF) == as_label(shading_column)))
      }
   } 
   
   # Optionally highlight specific cells
   if(any(sapply(highlight_cells, complete.cases))){
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
   
   # Optionally highlighting poor fidelity S/O values
   if(any(complete.cases(highlight_so_cutoffs))){
      
      # Tidying inputs
      HighlightMiddle <- 1 %in% highlight_so_cutoffs
      
      if(any(highlight_so_cutoffs < 1)){
         warning("At least one of the numbers you specified for highlight_so_cutoffs was < 1. We will automatically use both the original number you specified and its inverse for highlighting, so we'll ignore any values < 1 here.", 
                 call. = FALSE)
         highlight_so_cutoffs <- highlight_so_cutoffs[which(highlight_so_cutoffs >= 1)]
      }
      
      if(length(highlight_so_cutoffs) != length(highlight_so_colors) &
         highlight_so_colors[1] %in% c("yellow to red", "Lisa") == FALSE){
         warning("You have specified one number of colors for highlighting S/O values and a different number of cutoff values, so we don't know what colors you want. We'll use the default colors for highlighting.", 
                 call. = FALSE)
         highlight_so_colors <- "yellow to red"
      }
      
      highlight_so_colors <- tolower(highlight_so_colors)
      highlight_so_cutoffs <- sort(unique(highlight_so_cutoffs))
      
      if(highlight_so_colors[1] %in% c("yellow to red", "lisa") == FALSE && 
         is.matrix(col2rgb(highlight_so_colors)) == FALSE){
         warning("The values you used for highlighting problematic S/O ratios are not all valid colors in R. We'll used the default colors instead.", 
                 call. = FALSE)
         highlight_so_colors <- "yellow to red"
      } 
      
      if(highlight_so_colors[1] %in% c("yellow to red", "lisa")){
         
         ColorChoices <- paste(
            highlight_so_colors,
            HighlightMiddle, 
            cut(length(highlight_so_cutoffs), breaks = c(0:4, Inf)))
         
         highlight_so_colors <- 
            switch(ColorChoices, 
                   ## yellow to red
                   
                   # no middle, 1 cutoff
                   "yellow to red FALSE (0,1]" = "#FF9595", 
                   
                   # no middle, 2 cutoffs
                   "yellow to red FALSE (1,2]" = c("#FFFF95", "#FF9595"), 
                   
                   # no middle, 3 cutoffs
                   "yellow to red FALSE (2,3]" = c("#FFFF95", "#FFDA95", "#FF9595"), 
                   
                   # no middle, >3 cutoffs
                   "yellow to red FALSE (3,4]" = colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                      length(highlight_so_cutoffs)), 
                   # This is the same as the above on purpose.
                   "FALSE (4,Inf]" = colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                      length(highlight_so_cutoffs)), 
                   
                   # Just highlight everything green. This would be weird and
                   # probably not what the user wants, but is among the possible
                   # choices for inputs.
                   "yellow to red TRUE (0,1]" = c("#C7FEAC"),
                   
                   # highlight middle, 1 cutoff other than middle
                   "yellow to red TRUE (1,2]" = c("#C7FEAC", "#FF9595"), 
                   
                   # highlight middle, 2 cutoffs other than middle
                   "yellow to red TRUE (2,3]" = c("#C7FEAC", "#FFFF95", "#FF9595"), 
                   
                   # highlight middle, 3 cutoffs other than middle
                   "yellow to red TRUE (3,4]" = c("#C7FEAC", "#FFFF95", "#FFDA95", "#FF9595"), 
                   
                   # highlight middle, >3 cutoffs other than middle
                   "yellow to red TRUE (4,Inf]" = 
                      c("#C7FEAC", 
                        colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                           length(highlight_so_cutoffs))), 
                   
                   ## Lisa
                   
                   # no middle, 1 cutoff
                   "lisa FALSE (0,1]" = "#FF0000", 
                   
                   # no middle, 2 cutoffs
                   "lisa FALSE (1,2]" = c("#92D050", "#FF0000"), 
                   
                   # no middle, 3 cutoffs
                   "lisa FALSE (2,3]" = c("#92D050", "#FFC000", "#FF0000"), 
                   
                   # no middle, >3 cutoffs
                   "lisa FALSE (3,4]" = colorRampPalette(c("#92D050", "#FFC000", "#FF0000"))(
                      length(highlight_so_cutoffs)), 
                   # This is the same as the above on purpose.
                   "FALSE (4,Inf]" = colorRampPalette(c("#92D050", "#FFC000", "#FF0000"))(
                      length(highlight_so_cutoffs)), 
                   
                   # Just highlight everything green. This would be weird and
                   # probably not what the user wants, but is among the possible
                   # choices for inputs.
                   "lisa TRUE (0,1]" = c("#00B050"),
                   
                   # highlight middle, 1 cutoff other than middle
                   "lisa TRUE (1,2]" = c("#00B050", "#FF0000"), 
                   
                   # highlight middle, 2 cutoffs other than middle
                   "lisa TRUE (2,3]" = c("#00B050", "#92D050", "#FF0000"), 
                   
                   # highlight middle, 3 cutoffs other than middle
                   "lisa TRUE (3,4]" = c("#00B050", "#92D050", "#FFC000", "#FF0000"), 
                   
                   # highlight middle, >3 cutoffs other than middle
                   "lisa TRUE (4,Inf]" = 
                      c("#00B050", 
                        colorRampPalette(c("#FFC000", "#FF0000"))(
                           length(highlight_so_cutoffs)))
            )
      }
      
      StatCol <- which(str_detect(names(DF), "[Ss]tat$|[Ss]tatistic"))
      SOrows <- which(str_detect(DF[, StatCol][[1]],
                                 "S/O|[Ss]simulated.*[Oo]bserved"))
      for(i in SOrows){
         for(j in 1:length(highlight_so_cutoffs)){
            suppressWarnings(
               SO_col <- which(
                  as.numeric(t(DF[i, ])) >= highlight_so_cutoffs[j] | 
                     as.numeric(t(DF[i, ])) <= 1/highlight_so_cutoffs[j])
            )
            
            if(length(SO_col) > 0){
               FT <- FT %>% 
                  flextable::bg(i = i, 
                                j = SO_col, 
                                bg = highlight_so_colors[j])
            }
            
            rm(SO_col)
         }
      }
   }
   
   FT <- FT %>% 
      
      # Set the font size
      flextable::fontsize(part = "all", size = fontsize) %>% 
      
      # setting up which borderlines to show
      flextable::border_remove() %>% 
      flextable::border_inner_v(part = "all", 
                                border = officer::fp_border(width = 0.5)) %>% 
      flextable::border_outer(border = officer::fp_border(width = 0.5)) %>% 
      
      # making the width autofitted to contents
      flextable::set_table_properties(width = 1, layout = "autofit")
   
   # Dealing with subscripts
   ColNames <- names(DF)
   ColNames <- sub("AUCt( |$)", "AUC~t~ ", ColNames)
   ColNames <- sub("AUCinf( |$)", "AUC~inf~ ", ColNames)
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
      
      FileName <- basename(save_table)
      
      rmarkdown::render(system.file("rmarkdown/templates/savetablesimcyp/skeleton/skeleton.Rmd",
                                    package="SimcypConsultancy"), 
                        output_dir = OutPath, 
                        output_file = FileName, 
                        quiet = TRUE)
      # Note: The "system.file" part of the call means "go to where the
      # package is installed, search for the file listed, and return its
      # full path.
      
   }
   
   return(FT)
   
}


