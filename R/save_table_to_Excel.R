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
#' @param freeze_first_col TRUE (default) or FALSE for whether to freeze the
#'   view in Excel so that the first column will always be visible
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
#' @param bold_cells a list of cells to make bold. Default is NA for nothing but
#'   the first row being bold (which it will be automatically). There are two
#'   possible ways
#'   to specify which cells to make bold: \enumerate{\item{Use Excel-style cell
#'   names, e.g., \code{bold_cells = list("A2", "B5:C10")}. Please note that the 1st row in the output is
#'   \emph{not} the 1st row in your data.frame but the \emph{header} of your
#'   data.frame. Keep that in mind when specifying which row should be
#'   bold. }
#'   \item{Specify the cells in your data.frame as items in a list. Admittedly,
#'   this can be a little confusing because it requires nested lists. The first
#'   item will be a numeric vector of the rows and the second item will be a
#'   numeric vector of the columns for everything that is contiguously
#'   highlighted. If you want multiple, noncontiguous cells to be highlighted
#'   the same color, use multiple lists. For example, this will make two
#'   patches of cells bold:
#'   \code{bold_cells = list(list("rows" = c(1:2), "columns" = c(3:4)),
#'   list("rows" = c(18:20), "columns" = 6))}. Try it out and see what we mean.
#'   Since this is focused on the \emph{input} to the function, we have set
#'   this up so that row 1 here is
#'   row 1 in your input data.frame, which is different from how we set it up
#'   if you use Excel cell names to specify which thing should be highlighted.
#'   Please note that distinction. The header is row 0. If you want to make bold all the
#'   rows or all the columns, set the rows or columns to NA, e.g.,
#'   \code{bold_cells = list(list("rows" = 1, "columns" = NA))} will
#'   make everything in row 1 highlighted yellow.}}
#' @param column_widths optionally specify a numeric vector for the column
#'   widths. If left as NA, we will guess at reasonable column widths. If you
#'   specify anything, though, you'll need to specify ALL of the column widths.
#'   We'll repeat whatever number or numbers you use until we have enough for
#'   all the columns in your data, so if you want all your columns to be, e.g.,
#'   20 units wide in whatever units it is that Excel uses, just specify
#'   \code{column_widths = 20}.
#' @param overwrite Should we overwrite if your Excel file already exists and
#'   has a tab of the same name that you're trying to save? Options are "yes"
#'   (default) to always overwrite, "no" to never overwrite, or "ask", which
#'   means that we will ask you whether to overwrite and give you a chance to
#'   supply a different file name.
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
                                overwrite = "yes", 
                                center_top_row = TRUE, 
                                freeze_top_row = TRUE, 
                                freeze_first_col = TRUE, 
                                wrap_text = TRUE, 
                                column_widths = NA, 
                                highlight_cells = NA, 
                                bold_cells = NA){
   
   # Error catching --------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
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
   
   overwrite <- tolower(overwrite)[1]
   overwrite <- case_when("logical" %in% class(overwrite) & 
                             overwrite == TRUE ~ "y", 
                          "logical" %in% class(overwrite) & 
                             overwrite == FALSE ~ "n", 
                          str_detect(overwrite, "y") ~ "y", 
                          str_detect(overwrite, "n") ~ "n", 
                          overwrite %in% c("ask", "y", "n") ~ overwrite, 
                          .default = overwrite)
   
   if(is.na(overwrite) || overwrite %in% c("ask", "y", "n") == FALSE){
      warning(wrapn("For the argument 'overwrite', you have supplied something other than 'ask', 'yes', or 'no', which are the only possible options. We'll use the default of asking before we overwrite an exsiting Excel file and tab."), 
              call. = FALSE)
      
      overwrite <- "ask"
   }
   
   freeze_top_row <- freeze_top_row[1]
   if("logical" %in% class(freeze_top_row) == FALSE){
      warning(wrapn("You have specified something other than TRUE or FALSE for whether to freeze the top row, so we'll use the default of TRUE."), 
              call. = FALSE)
      freeze_top_row <- TRUE
   }

   freeze_first_col <- freeze_first_col[1]
   if("logical" %in% class(freeze_first_col) == FALSE){
      warning(wrapn("You have specified something other than TRUE or FALSE for whether to freeze the first column, so we'll use the default of TRUE."), 
              call. = FALSE)
      freeze_first_col <- TRUE
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
      
      if(overwrite == "ask"){
         
         message("An Excel file with the tab you provided already exists. Is it ok to overwrite it? (y or n)")
         overwrite <- readline("   ")
         overwrite <- str_extract(tolower(overwrite), "y|n")[1]
         
         if(overwrite %in% c("y", "n") == FALSE){
            stop(wrapn("You did not enter 'y' or 'n' for whether to overwrite the existing tab in your Excel file. We cannot save anything for you."), 
                 call. = FALSE)
         }
         
         if(overwrite == "n"){
            message(wrapn("What would you like the file name to be instead? Be warned: We will not check a second time for whether that file and tab name already exist."))
            FileName <- readline("    ")
            FileName <- gsub("\"", "", FileName)
            overwrite <- "y"
            
            openxlsx::removeWorksheet(wb = WB, 
                                      sheet = output_tab_name)
            
            openxlsx::addWorksheet(wb = WB, 
                                   sheetName = output_tab_name)
         }
      }
      
      # At this point, we have asked whether to overwrite if that were
      # necessary, and overwrite is now ONLY going to be either y or n.
      
      if(overwrite == "n"){
         stop(wrapn("You asked that we not overwrite existing data when saving your Excel file, so we need either a different file name or a different output tab name to save. For now, we will not save your data to an Excel file."))
      } else if(overwrite == "y"){
         openxlsx::removeWorksheet(wb = WB, 
                                   sheet = output_tab_name)
         
         openxlsx::addWorksheet(wb = WB, 
                                sheetName = output_tab_name)
      }
      
   } else {
      openxlsx::addWorksheet(wb = WB, 
                             sheetName = output_tab_name)
   }
   
   
   ## Setting some aesthetics -------------------------------------------------
   
   ### Main ----------------------------------------------------------------
   
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
                        firstRow =  freeze_top_row, 
                        firstCol = freeze_first_col)
   
   # Setting column widths
   if(all(is.na(column_widths))){
      ColWidths <- guess_col_widths(DF = table, 
                                    wrap = wrap_text)
   } else {
      ColWidths <- rep(column_widths, length = ncol(table))[1:ncol(table)]
   }
   
   openxlsx::setColWidths(wb = WB, 
                          sheet = output_tab_name, 
                          cols = 1:ncol(table), 
                          widths = ColWidths)
   
   ### Highlighting ---------------------------------------------------------------- 
   if(class(highlight_cells) == "list" &&
      any(lapply(highlight_cells, FUN = length) > 0)){
      
      for(cc in names(highlight_cells)){
         
         HighlightTemp <- openxlsx::createStyle(wrapText = wrap_text, 
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
                                  gridExpand = TRUE, 
                                  stack = TRUE)
               
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
                                  gridExpand = TRUE, 
                                  stack = TRUE)
            }
         }
         
         rm(HighlightTemp)
         
      }
   }
   
   
   ### Bold ---------------------------------------------------------------- 
   
   # NB: Note that lists supplied for bold_cells should have one fewer level
   # than lists supplied for highlight_cells. This is b/c highlight_cells needs
   # to have a level specifying the color wherease bold_cells does not.
   if(class(bold_cells) == "list" &&
      any(lapply(bold_cells, FUN = length) > 0)){
      
      BoldTemp <- openxlsx::createStyle(wrapText = wrap_text, 
                                        textDecoration = "bold")
      
      for(cc in 1:length(bold_cells)){
         
         # Check for whether they have supplied Excel cell names for where to
         # add highlighting or R rows and columns
         ExcelFormat <- any(str_detect(unlist(bold_cells[[cc]][[1]]),
                                       "[A-Z|a-z]"))
         
         if(ExcelFormat){
            # This is when the cells to highlight are specified using
            # Excel-style syntax s/a "A2:B4"
            
            if("list" %in% class(bold_cells[[cc]])){
               bold_cells[[cc]] <- unlist(bold_cells[[cc]])
            }
            
            MyCells <- str_split_1(bold_cells[[cc]], ":")
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
                               style = BoldTemp, 
                               rows = TempRows, 
                               cols = TempCols, 
                               gridExpand = TRUE, 
                               stack = TRUE)
            
            rm(TempRows, TempCols, MyCells)
            
         } else {
            # This is when the syntax is based on the R rows and columns
            
            # Checking that there are 2 items in the list: rows and columns.
            if(length(bold_cells[[cc]]) != 2){
               warning(wrapn(paste0(
                  "The ", scales::ordinal(cc), 
                  " item for making cells bold is not listed correctly. We need a list of two numeric vectors: the 1st for rows and the 2nd for columns. We will not make these cells bold in the Excel file.")), 
                  call. = FALSE)
               
               next
            }
            
            if(all(is.na(bold_cells[[cc]][[1]]))){
               bold_cells[[cc]][[1]] <- 1:now(table)
            }
            
            if(all(is.na(bold_cells[[cc]][[2]]))){
               bold_cells[[cc]][[2]] <- 1:ncol(table)
            }
            
            openxlsx::addStyle(wb = WB, 
                               sheet = output_tab_name, 
                               style = BoldTemp, 
                               # IMPORTANT: The + 1 is to account for the header row.
                               rows = bold_cells[[cc]][[1]] + 1, 
                               cols = bold_cells[[cc]][[2]], 
                               gridExpand = TRUE, 
                               stack = TRUE)
         }
      }
      
      rm(BoldTemp)
   }
   
   
   ## Saving -----------------------------------------------------------------
   
   openxlsx::saveWorkbook(wb = WB, 
                          file = FileName, 
                          overwrite = TRUE)
   
   
}


