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
#' @param wrap TRUE (default) or FALSE for whether to wrap text 
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
                                wrap = TRUE){
   
   # Error catching --------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
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
                                        wrapText = wrap)
   
   openxlsx::writeData(wb = WB, 
                       sheet = output_tab_name,
                       x = as.data.frame(table), 
                       headerStyle = HeaderStyle)
   
   BodyStyle <- openxlsx::createStyle(wrapText = wrap)
   
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
                                 wrap = wrap)
   
   openxlsx::setColWidths(wb = WB, 
                          sheet = output_tab_name, 
                          cols = 1:ncol(table), 
                          widths = ColWidths)
   
   
   ## Saving -----------------------------------------------------------------
   
   openxlsx::saveWorkbook(wb = WB, 
                          file = FileName, 
                          overwrite = TRUE)
   
   
}


