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
#'   have set \code{checkDataSource = TRUE} is the \emph{perfect} format for
#'   this data.frame and is what this function was designed to use.
#' @param stats a character vector of the summary statistics to highlight.
#'   Options are "geomean", "mean", "median", "CI90_low", "CI90_high", "per5",
#'   "per95", "min", "max", "GCV", and/or "CV". An example of acceptable input
#'   would be \code{stats = c("geomean", "CI90_low", "CI90_high")} to highlight
#'   the cells containing the geometric mean and the upper and lower values for
#'   the geometric 90\% confidence interval. Regardless of what stats are
#'   requested, \code{highlightQC} will highlight only the median, minimum, and
#'   maximum values for tmax to match the Simcyp Consultancy report templates.
#' @param java_fail_option Option you want to have happen if Java fails because
#'   it ran out of memory. By default, behind the scenes, it's Java -- not R --
#'   that highlights the appropriate cells in the Simulator output Excel files,
#'   but Java requires \emph{so much memory} that it fails for large files.
#'   There are two options
#'   here: \describe{\item{"fail" (default)}{We'll \emph{try} to have Java highlight
#'   things, but if it fails because it ran out of memory, nothing happens.}
#'
#'   \item{"highlight anyway"}{There \emph{is} a way to get the highlighting
#'   you want without Java, but it just doesn't work as well. If we don't use
#'   Java, you'll get the appropriate yellow highlighting, but the watermark and blue
#'   shading that are present on tabs such as the "Summary" tab, the "Input Sheet",
#'   and the tab with the population parameters will disappear. Those tabs will
#'   still be protected, but they \emph{will look different.}}
#'
#'   \item{"highlight a copy"}{We won't use Java to highlight, so you'll lose
#'   the watermark and blue background on protected tabs, but we'll do that
#'   on a copy of the original Simulator Excel file. It will be named the same
#'   but will have "QC" appended to the end of the file name.}}
#'
#' @return Does not return an R object; saves highlighting in Excel files
#' @export
#'
#' @examples
#' # None yet
#' 
highlightQC <- function(qc_dataframe, 
                        stats, 
                        java_fail_option = "fail"){
   
   # Error catching 
   java_fail_option <- tolower(java_fail_option)[1]
   if(java_fail_option %in% c("fail", "highlight anyway", "highlight a copy") == FALSE){
      warning(paste0("The options for `java_fail_option` are one of `fail` (default), `highlight anyway`, or `highlight a copy`, and you specified `", 
                     java_fail_option, "`. Since that's not one of the options, we'll go with the default."), 
              call. = FALSE)
      java_fail_option <- "fail"
   }
   
   # Need to convert letter name of column back to a number
   XLCols <- c(LETTERS, paste0("A", LETTERS), paste0("B", LETTERS))
   
   qc_dataframe <- split(qc_dataframe, f = qc_dataframe$File)
   
   for(k in names(qc_dataframe)){
      
      qc_dataframe[[k]] <- split(qc_dataframe[[k]], f = qc_dataframe[[k]]$Tab)
      
      # Loading the workbook so that we can highlight things
      wb <- tryCatch(xlsx::loadWorkbook(k), error = function(x) "Out of memory")
      
      if(class(wb) != "character"){
         # If class(wb) isn't character, then Java had enough memory and we
         # don't even need to consider the alternatives for java_fail_option.
         
         # Setting up the cell style to use
         QCStyle <- xlsx::CellStyle(
            wb, 
            dataFormat = xlsx::DataFormat("0.00"),
            fill = xlsx::Fill(foregroundColor = "yellow"), 
            alignment = xlsx::Alignment(horizontal = "ALIGN_CENTER", 
                                        vertical = "VERTICAL_CENTER"),
            border = xlsx::Border(position = c("TOP", "BOTTOM", "LEFT", "RIGHT"), 
                                  pen = "BORDER_DOTTED"),
            font = xlsx::Font(wb, heightInPoints = 8), 
         )
         
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
         suppressMessages(gc(verbose = FALSE))
         # Can't seem to suppress the output, though, but it's not showing up when
         # running pksummary_x.
         
         rJava::J("java.lang.Runtime")$getRuntime()$gc()
         
      } else {
         
         if(java_fail_option == "fail"){
            warning("This Excel file is just too large for Java to load, so we won't be able to highlight anything.", 
                    call. = FALSE)
            next
            
         } else {
            
            # This is when Java didn't have enough memory. The first steps for both
            # java_fail_options are the same.
            
            # Setting up the cell style to use
            ToQC <- openxlsx::createStyle(fontSize = 8, fgFill = "yellow", 
                                          numFmt = "0.00", 
                                          halign = "center", valign = "center", 
                                          border = "TopBottomLeftRight",
                                          borderStyle = "hair")
            
            # Loading the workbook so that we can highlight things
            wb <- openxlsx::loadWorkbook(k)
            
            for(i in names(qc_dataframe[[k]])){
               
               ToHighlight <- qc_dataframe[[k]][[i]] %>% ungroup() %>% 
                  select(File, PKparam, Tab, 
                         any_of(unique(c(stats, "min", "max", "median")))) %>% 
                  pivot_longer(cols = -c("File", "Tab", "PKparam"), 
                               names_to = "Stat", values_to = "Cell") %>% 
                  mutate(Column = str_extract(Cell, "[A-Z]{1,2}"), 
                         Row = as.numeric(gsub("[A-Z]{1,2}", "", Cell))) %>% 
                  filter((str_detect(PKparam, "tmax") & Stat %in% c("min", "max", "median")) |
                            (!str_detect(PKparam, "tmax") & Stat %in% stats)) %>% 
                  arrange(Row)
               
               
               # ToHighlight <- qc_dataframe[[k]][[i]] %>% ungroup() %>% 
               #    select(File, Tab, any_of(StatsToHighlight)) %>% 
               #    pivot_longer(cols = -c("File", "Tab"), 
               #                 names_to = "Stat", values_to = "Cell") %>% 
               #    mutate(Column = str_extract(Cell, "[A-Z]{1,2}"), 
               #           Row = as.numeric(gsub("[A-Z]{1,2}", "", Cell)))
               ToHighlight$Column <- as.numeric(sapply(
                  ToHighlight$Column, FUN = function(x) which(XLCols == x)))
               
               # Applying the highlighting to the workbook
               for(j in 1:nrow(ToHighlight)){
                  openxlsx::addStyle(wb, sheet = i, style = ToQC,
                                     rows = ToHighlight$Row[j], 
                                     cols = ToHighlight$Column[j])
               }
               
               rm(ToHighlight)
            }
            
            openxlsx::saveWorkbook(wb, 
                         file = switch(
                            java_fail_option, 
                            "highlight anyway" = k,
                            "highlight a copy" = sub("//.xlsx", " - QC.xlsx", k)), 
                         overwrite = TRUE)
         }
      }
   }
}

