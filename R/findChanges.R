
#' Find differences between two Excel files
#'
#' This function reads in all sheets in 2 Excel files and finds any differences
#' between them. It is NOT smart, so if the only difference on a given sheet is
#' that everything has moved down a row, it will find LOTS of changes. It is
#' also SLOW b/c it has to read everything from Excel.
#'
#'
#' @param File1 An Excel file you want to compare
#' @param File2 The other Excel file you want to compare
#' @param outputAllSheets TRUE or FALSE: Do you want to have the output include
#'   a list of all the loaded Excel sheets?
#'
#' @return If \code{ouputAllSheets} is FALSE, output is a list of which rows and
#'   columns differ between File1 and File2. If \code{ouputAllSheets} is TRUE,
#'   output is both the list and also a list of all the sheet contents from both
#'   files.
#' @import tidyverse
#' @import readxl
#' @export
#' @examples
#' # No examples available atm.
#'
findXLChanges <- function(File1, File2, outputAllSheets = TRUE){

      File1_wb <- openxlsx::loadWorkbook(File1)
      Sheets1 <- names(File1_wb)

      File2_wb <- openxlsx::loadWorkbook(File2)
      Sheets2 <- names(File2_wb)

      if(all(Sheets1 %in% Sheets2) == FALSE | all(Sheets2 %in% Sheets1) == FALSE){
            MissingFrom1 <- setdiff(Sheets1, Sheets2)
            Msg <- paste0("The sheet(s) ", stringr::str_comma(MissingFrom1),
                          " is/are present in file 1 but not in file 2; it/they will be ignored.")

            MissingFrom2 <- setdiff(Sheets2, Sheets1)
            Msg <- paste0(Msg, "The sheet(s) ", stringr::str_comma(MissingFrom1),
                          " is/are present in file 2 but not in file 1; it/they will be ignored.")

            warning(Msg)
      }

      AllSheets <- intersect(Sheets1, Sheets2)

      F1_sheets <- list()
      F2_sheets <- list()

      AllSame <- list()

      for(i in AllSheets){
            # Reading & comparing
            F1_i <- openxlsx::read.xlsx(File1_wb, sheet = i, colNames = FALSE,
                              skipEmptyCols = FALSE, skipEmptyRows = FALSE)
            F2_i <- openxlsx::read.xlsx(File2_wb, sheet = i, colNames = FALSE,
                              skipEmptyCols = FALSE, skipEmptyRows = FALSE)

            AllSame[[i]] <- all(F1_i == F2_i, na.rm = TRUE)

            F1_sheets[[i]] <- F1_i
            F2_sheets[[i]] <- F2_i

            rm(F1_i, F2_i)
      }

      DiffFound <- names(AllSame[AllSame == FALSE])
      DiffCell <- list()

      # Names of Excel columns
      XLCols <- c(LETTERS, paste0("A", LETTERS), paste0("B", LETTERS),
                  paste0("C", LETTERS), paste0("D", LETTERS), paste0("E", LETTERS))

      for(i in DiffFound){

            DiffCell[[i]] <- list()

            for(j in 1:ncol(F1_sheets[[i]])){
                  AnyDiffCol <- F1_sheets[[i]][, j] == F2_sheets[[i]][, j]
                  DiffRow <- which(AnyDiffCol == FALSE)
                  if(length(DiffRow) > 0){
                        DiffCell[[i]] <- paste0(XLCols[j], DiffRow)
                  }

                  rm(AnyDiffCol, DifRow)
            }


      }

      if(outputAllSheets){
            Out <- list(DiffCell, F1_sheets, F2_sheets)
      } else {
            Out <- DiffCell
      }

      return(Out)


}

# findXLChanges(File1, File2, outputAllSheets = FALSE)


