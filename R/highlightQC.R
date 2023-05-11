#' Highlight specific cells in Simcyp Simulator output Excel files for QCing
#' tables of PK data
#'
#' \code{highlightQC} uses a data.frame of specific files, tabs, PK parameters,
#' and cell locations to determine where to apply yellow highlighting for the
#' purposes of QCing. This does NOT change any of the values in the Excel file;
#' it \emph{only} adds yellow highlighting.
#'
#' @param qc_dataframe a data.frame listing each PK parameters to be QCed as a
#'   row and including columns for "File", "PKparam", "Tab", and then the
#'   specific summary statistics used. The QC output from
#'   \code{\link{pksummary_table}} or \code{\link{pksummary_mult}} where you
#'   have set \code{checkDataSource = TRUE} is the\emph{perfect} format for this
#'   data.frame and is what this function was designed to use.
#' @param stats a character vector of the summary statistics to highlight.
#'   Options are "geomean", "mean", "median", "CI90_low", "CI90_high", "per5",
#'   "per95", "min", "max", "GCV", and/or "CV". An example of acceptable input
#'   would be \code{stats = c("geomean", "CI90_low", "CI90_high")} to highlight
#'   the cells containing the geometric mean and the upper and lower values for
#'   the geometric 90\% confidence interval. Regardless of what stats are
#'   requested, \code{highlightQC} will highlight only the median, minimum, and
#'   maximum values for tmax to match the Simcyp Consultancy report templates.
#'
#' @return Does not return an R object; saves highlighting in Excel files
#' @export
#'
#' @examples
#' # None yet
#' 
highlightQC <- function(qc_dataframe, stats){
   
   # Need to convert letter name of column back to a number
   XLCols <- c(LETTERS, paste0("A", LETTERS), paste0("B", LETTERS))
   
   qc_dataframe <- split(qc_dataframe, f = qc_dataframe$File)
   
   for(k in names(qc_dataframe)){
      
      # Loading the workbook so that we can highlight things
      wb <- xlsx::loadWorkbook(k)
      
      # Setting up the cell style to use
      QCStyle <- xlsx::CellStyle(
         wb, 
         dataFormat = xlsx::DataFormat("0.00"),
         fill = xlsx::Fill(foregroundColor = "yellow"), 
         alignment = xlsx::Alignment(horizontal = "ALIGN_CENTER", 
                                     vertical = "VERTICAL_CENTER"),
         border = xlsx::Border(position = c("TOP", "BOTTOM", "LEFT", "RIGHT"), 
                               pen = "BORDER_DOTTED"),
         font = xlsx::Font(wb, heightInPoints = 8))
      
      qc_dataframe[[k]] <- split(qc_dataframe[[k]], f = qc_dataframe[[k]]$Tab)
      
      for(i in names(qc_dataframe[[k]])){
         
         ToHighlight <- qc_dataframe[[k]][[i]] %>% ungroup() %>% 
            select(File, PKparam, Tab, any_of(unique(c(stats, "min", "max", "median")))) %>% 
            pivot_longer(cols = -c("File", "Tab", "PKparam"), 
                         names_to = "Stat", values_to = "Cell") %>% 
            mutate(Column = str_extract(Cell, "[A-Z]{1,2}"), 
                   Row = as.numeric(gsub("[A-Z]{1,2}", "", Cell))) %>% 
            filter((str_detect(PKparam, "tmax") & Stat %in% c("min", "max", "median")) |
                      (!str_detect(PKparam, "tmax") & Stat %in% stats)) %>% 
            arrange(Row)
         
         # Need to subtract 1 from the column number b/c the 1st column is blank
         # and xlsx package skips blank cells
         ToHighlight$Column <- as.numeric(sapply(
            ToHighlight$Column, FUN = function(x) which(XLCols == x) - 1))
         
         MySheet <- xlsx::getSheets(wb)[[i]]
         
         # Applying the highlighting to the workbook
         for(j in unique(ToHighlight$Row)){
            
            MyRow <- xlsx::getCells(row = xlsx::getRows(MySheet, rowIndex = j))
            
            lapply(MyRow[ToHighlight %>% filter(Row == j) %>% pull(Column)], 
                   FUN = function(x) xlsx::setCellStyle(cell = x, cellStyle = QCStyle))
            
            rm(MyRow)
         }
         
         rm(ToHighlight, MySheet)
      }
      
      xlsx::saveWorkbook(wb, file = k)
      
      rm(wb)
      
      # From https://stackoverflow.com/questions/12476044/r-how-to-clear-memory-used-by-rjava
      suppressMessages(gc(verbose = FALSE)) # Can't seem to suppress the output, though. 
      J("java.lang.Runtime")$getRuntime()$gc()
      
   }
}

