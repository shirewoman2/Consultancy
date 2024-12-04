#' Save a data.frame or tibble to Excel with some standard formatting
#'
#' Please be aware that this \emph{does} overwrite existing data.
#'
#' @param table the table (the data.frame or tibble, really) that you want to
#'   save
#' @param save_table file name for saving your table. \emph{This does not work
#'   if you have included a "." anywhere in your file name except in front of
#'   the file extension.} Good: \code{save_table = "my file 1.xlsx"} Bad:
#'   \code{save_table = "my.file.1.xlsx"}
#' @param output_tab_name name of the tab to save your table to; defaults to
#'   "Sheet1".
#' @param freeze_top_row TRUE (default) or FALSE for whether to freeze the view
#'   in Excel so that the top row will always be visible
#' @param center_top_row TRUE (default) or FALSE for whether to center the text
#'   in the top row
#' @param wrap_text TRUE (default) or FALSE for whether to wrap text
#' @param highlight_cells a named list of cells to highlight where the names are
#'   the colors to use for highlighting and the values are the cells to
#'   highlight. Default is NA for no highlighting. There are two possible ways
#'   to specify which cells to highlight: \enumerate{\item{Use Excel-style cell
#'   names, e.g., \code{highlight_cells = list("yellow" = "A2",
#'   "pink" = "B5:C10")}. Please note that the 1st row in the output is
#'   \emph{not} the 1st row in your data.frame but the \emph{header} of your
#'   data.frame. Keep that in mind when specifying which row should be
#'   highlighted.}
#'   \item{Specify the cells in your data.frame as items in a list. Admittedly,
#'   this can be a little confusing because it requires nested lists. The first
#'   item will be a numeric vector of the rows and the second item will be a
#'   numeric vector of the columns for everything that is contiguously
#'   highlighted. If you want multiple, noncontiguous cells to be highlighted
#'   the same color, use multiple lists. For example, this will highlight two
#'   patches of cells yellow:
#'   \code{highlight_cells = list("yellow" = list(list("rows" = c(1:2), "columns" = c(3:4)),
#'   list("rows" = c(18:20), "columns" = 6)))}. Try it out and see what we mean. 
#'   Since this is focused on the \emph{input} to the function, we have set 
#'   this up so that row 1 here is
#'   row 1 in your input data.frame, which is different from how we set it up
#'   if you use Excel cell names to specify which thing should be highlighted.
#'   Please note that distinction. If you want to add highlighting to something
#'   in the header, refer to that as row 0. If you want to highlight all the
#'   rows or all the columns, set the rows or columns to NA, e.g.,
#'   \code{highlight_cells = list("yellow" = list(list("rows" = 1, "columns" = NA))} will
#'   make everything in row 1 highlighted yellow.}}
#'
#' @return does not return a new object; only saves an existing object to Excel
#' @export
#'
#' @examples
#' save_table_to_Excel(table = starwars,
#'                     save_table = "starwars characters.xlsx")
#'                     
save_table_to_Excel <- function(table, 
                                save_table, 
                                output_tab_name = "Sheet1", 
                                freeze_top_row = TRUE, 
                                center_top_row = TRUE, 
                                wrap_text = TRUE, 
                                highlight_cells = NA){
   
   # Error catching --------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Checking input for highlighting
   if(class(highlight_cells) != "logical" & class(highlight_cells) != "list"){
      warning(wrapn("We need the input for the argument 'highlight_cells' to be a named list, where the names are the highlight colors you want and the items in the list are the cells you want to highlight that color, but you have supplied something else. We cannot highlight anything for you."), 
              call. = FALSE)
      highlight_cells <- NA
   }
   
   if(class(highlight_cells) == "list"){
      ColCheck <- all(is.matrix(col2rgb(names(highlight_cells))))
      if(ColCheck == FALSE){
         warning(wrapn("We need the input for the argument 'highlight_cells' to be a named list, where the names are the highlight colors you want and the items in the list are the cells you want to highlight that color. The names of your list are not all legitimate colors in R, so we cannot highlight anything for you."), 
                 call. = FALSE)
         highlight_cells <- NA
      }
   }
   
   
   # Main body of function -------------------------------------------------
   
   ## Setting up the Excel file  -------------------------------------------
   
   FileName <- save_table
   
   # Making sure they've got a good extension by just removing whatever they
   # have and then replacing it with xlsx
   if(str_detect(FileName, "\\.")){
      FileName <- sub("\\..*$", "", FileName)
   } 
   FileName <- paste0(FileName, ".xlsx")
   
   # Using openxlsx to format things. 
   if(file.exists(FileName)){
      WB <- openxlsx::loadWorkbook(file = FileName)
   } else {
      WB <- openxlsx::createWorkbook()
   }
   
   if(file.exists(FileName) &&
      output_tab_name %in% readxl::excel_sheets(path = FileName)){
      openxlsx::removeWorksheet(wb = WB, 
                                sheet = output_tab_name)
   }
   
   openxlsx::addWorksheet(wb = WB, 
                          sheetName = output_tab_name)
   
   
   ## Setting some aesthetics -------------------------------------------------
   
   # Setting up header style and writing the values and that style to the sheet 
   HeaderStyle <- openxlsx::createStyle(textDecoration = "bold",
                                        halign = ifelse(center_top_row, 
                                                        "center", "left"), 
                                        valign = "center", 
                                        wrapText = wrap_text)
   
   openxlsx::writeData(wb = WB, 
                       sheet = output_tab_name,
                       x = as.data.frame(table), 
                       headerStyle = HeaderStyle)
   
   BodyStyle <- openxlsx::createStyle(wrapText = wrap_text)
   
   openxlsx::addStyle(wb = WB, 
                      sheet = output_tab_name, 
                      style = BodyStyle, 
                      rows = 2:(nrow(table) + 1), 
                      cols = 1:ncol(table), 
                      gridExpand = TRUE)
   
   # Freezing view 
   openxlsx::freezePane(wb = WB,
                        sheet = output_tab_name,
                        firstRow =  freeze_top_row)
   
   # Setting column widths
   ColWidths <- guess_col_widths(DF = table, 
                                 wrap = wrap_text)
   
   openxlsx::setColWidths(wb = WB, 
                          sheet = output_tab_name, 
                          cols = 1:ncol(table), 
                          widths = ColWidths)
   
   # Highlighting
   if(class(highlight_cells) == "list" &&
      any(lapply(highlight_cells, FUN = length) > 0)){
      
      for(cc in names(highlight_cells)){
         
         HighlightTemp <- openxlsx::createStyle(wrapText = TRUE, 
                                                fgFill = cc)
         
         # Check for whether they have supplied Excel cell names for where to
         # add highlighting or R rows and columns
         ExcelFormat <- any(str_detect(unlist(highlight_cells[[cc]][[1]]),
                                       "[A-Z|a-z]"))
         
         if(ExcelFormat){
            # This is when the cells to highlight are specified using
            # Excel-style syntax s/a "A2:B4"
            for(i in 1:length(highlight_cells[[cc]])){
               
               MyCells <- str_split_1(highlight_cells[[cc]][i], ":")
               TempRows <- as.numeric(str_extract(MyCells, "[0-9]{1,}"))
               TempCols <- toupper(str_extract(MyCells, "[A-Z|a-z]{1,}"))
               ExcelCols <- c(LETTERS, 
                              paste0(rep(LETTERS, each = 26), 
                                     LETTERS))
               TempCols <- as.numeric(sapply(TempCols, 
                                             FUN = \(x) which(ExcelCols == x)))
               
               if(length(MyCells) == 2){
                  TempRows <- TempRows[1]:TempRows[2]
                  TempCols <- TempCols[1]:TempCols[2]
               }
               
               openxlsx::addStyle(wb = WB, 
                                  sheet = output_tab_name, 
                                  style = HighlightTemp, 
                                  rows = TempRows, 
                                  cols = TempCols, 
                                  gridExpand = TRUE)
               
               rm(TempRows, TempCols, MyCells)
            }
            
         } else {
            # This is when the syntax is based on the R rows and columns
            
            # Checking for nested list, which is what we need
            if(class(highlight_cells[[cc]][[1]]) == "numeric"){
               # This might happen if they only want to highlight one contiguous
               # set of cells and don't notice that the input should be a nested
               # list. Make it one in that case.
               highlight_cells[[cc]][[1]] <- list(highlight_cells[[cc]][[1]])
            }
            
            for(i in 1:length(highlight_cells[[cc]])){
               # Checking that there are 2 items in the list: rows and columns.
               if(length(highlight_cells[[cc]][[i]]) != 2){
                  warning(wrapn(paste0(
                     "The ", scales::ordinal(i), 
                     " item for highlighting cells with the color ", 
                     cc, " is not listed correctly. We need a list of two numeric vectors: the 1st for rows and the 2nd for columns. We will not highlight these cells in the Excel file.")), 
                     call. = FALSE)
                  
                  next
               }
               
               if(all(is.na(highlight_cells[[cc]][[i]][[1]]))){
                  highlight_cells[[cc]][[i]][[1]] <- 1:now(table)
               }
               
               if(all(is.na(highlight_cells[[cc]][[i]][[2]]))){
                  highlight_cells[[cc]][[i]][[2]] <- 1:ncol(table)
               }
               
               openxlsx::addStyle(wb = WB, 
                                  sheet = output_tab_name, 
                                  style = HighlightTemp, 
                                  # IMPORTANT: The + 1 is to account for the header row.
                                  rows = highlight_cells[[cc]][[i]][[1]] + 1, 
                                  cols = highlight_cells[[cc]][[i]][[2]], 
                                  gridExpand = TRUE)
            }
         }
         
         rm(HighlightTemp)
         
      }
   }
   
   
   ## Saving -----------------------------------------------------------------
   
   openxlsx::saveWorkbook(wb = WB, 
                          file = FileName, 
                          overwrite = TRUE)
   
   
}


