#' Generate a form for entering data for a report
#'
#' Creates an Excel file with forms to fill out about observed data. Those forms
#' can be fed into \code{\link{pksummary_table}} to automate some of the
#' calculations for writing a report.
#'
#' The tabs in the Excel file this function creates are: \describe{
#'
#' \item{how to use this file}{Instructions for how to use the Excel template
#' form.}
#'
#' \item{study info - no DDI}{a form for entering observed PK data about a
#' clinical study that was \emph{not} a drug-drug interaction study. Make as
#' many copies of this tab as there are clinical data sets that you want to
#' compare and name the tabs according to which data sets they describe.}
#'
#' \item{study info - DDI}{a form for entering observed PK data about a clinical
#' study that \emph{was} a drug-drug interaction study. Make as many copies of
#' this tab as there are clinical data sets you want to compare and name the
#' tabs according to which data sets they describe.} }
#'
#' @param filename the Excel file name that you'd like for the form you're
#'   creating, ending in ".xlsx"
#'
#' @return This does not return an R object; it saves an Excel file that serves
#'   as a form for recording observed data and certain general information for a
#'   report.
#'
#' @export
#'
#' @examples
#' generateReportInputForm("Ultraconazole report input.xlsx")
#'
#' 

generateReportInputForm <- function(filename){
    
    # Error catching ----------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    # Check for "\" b/c people will probably paste the path from Windows
    filename <- gsub("\\\\", "/", filename)
    
    # If they didn't include ".xlsx" at the end, add that.
    filename <- ifelse(str_detect(filename, "xlsx$"), 
                       filename, paste0(filename, ".xlsx"))
    
    # Check whether file exists and stop if it does so that no one
    # accidentally overwrites something they've spent time filling out
    # already.
    if(file.exists(filename)){
        stop(paste0("The file '", filename, "' already exists. Please use a different file name or delete the existing version and try again."),
             call. = FALSE)
    }
    
    # Main body of function ------------------------------------------------
    
    Path <- dirname(filename)
    File <- basename(filename)
    CurDir <- getwd()
    
    # For some reason, when you only include a relative path for the file with
    # the formatXL function, it will overwrite rather than adding sheets, but
    # it DOESN'T do that when you just set the path first and then write.
    setwd(Path)
    
    HowTo <- ReportInputForm[["how to use this file"]]
    StudyNoDDI <- ReportInputForm[["study info - no DDI"]]
    StudyDDI <- ReportInputForm[["study info - DDI"]]
    
    HowTo <- HowTo[2:nrow(HowTo),]
    formatXL(as.data.frame(HowTo) %>% rename("How to use this Excel file" = HowTo),
             file = filename,
             sheet = "how to use this file",
             colWidth = list(colNum = 1,
                             width = 250),
             styles = list(
                 list(rows = c(0, 4, 11, 17), font = list(bold = TRUE, size = 16)),
                 list(rows = 19, font = list(color = "red", bold = TRUE))
             ))
    
    ## Not including the "Overall" tab for now.
    
    # Overall <- Overall[2:nrow(Overall),]
    # formatXL(Overall %>% rename("Overall report information" = X1,
    #                             "ignore" = X2, "_" = X3),
    #          file = filename,
    #          sheet = "overall report info",
    #          colWidth = list(colNum = 1:3,
    #                          width = c(75, 0, 30)),
    #          styles = list(
    #              list(columns = 1, textposition = list(wrapping = TRUE)),
    #              list(rows = 0, font = list(bold = TRUE, size = 16)),
    #              list(rows = 0, columns = 3, font = list(color = "#FCFEFE")), # <- Closest I can get to white since, for some reason, "white" doesn't work and neither does the hex specification.
    #              list(rows = 1, font = list(italics = TRUE),
    #                   textposition = list(wrapping = TRUE)),
    #              list(rows = 3, font = list(bold = TRUE),
    #                   textposition = list(alignment = "middle"))
    #          ))
    
    
    StudyNoDDI <- StudyNoDDI[2:nrow(StudyNoDDI), ]
    formatXL(StudyNoDDI %>% rename("Simulated data (no DDI)" = X1,
                                   "ignore" = X2, "_" = X3),
             file = filename,
             sheet = "study info - no DDI",
             colWidth = list(colNum = 1:3,
                             width = c(75, 0, 30)),
             styles = list(
                 list(columns = 1, textposition = list(wrapping = TRUE)),
                 
                 # simulated data section
                 list(rows = 0, font = list(bold = TRUE, size = 18)), 
                 list(rows = c(1, 8), font = list(bold = TRUE, size = 14), # This is actually both simulated and observed headings for "Item" and "Value"
                      textposition = list(alignment = "middle")), 
                 list(rows = 2, columns = 3, textposition = list(wrapping = TRUE)),
                 
                 # observed data section
                 list(rows = 0, columns = 3, font = list(color = "#FCFEFE")), # <- Closest I can get to white since, for some reason, "white" doesn't work and neither does the hex specification.
                 list(rows = 5, font = list(bold = TRUE, size = 18)), 
                 list(rows = 6, font = list(italics = TRUE), # Notes for observed data section
                      textposition = list(wrapping = TRUE)),
                 
                 # Dose 1 data
                 list(rows = 13, columns = 1:3, 
                      font = list(bold = TRUE, size = 14), fill = "#DDEBF7"),
                 list(rows = 14:28, columns = 1:3, fill = "#DDEBF7"),
                 
                 # Multiple-dose data
                 list(rows = 30, columns = 1:3, 
                      font = list(bold = TRUE, size = 14), fill = "#E2EFDA"),
                 list(rows = 31:41, columns = 1:3, fill = "#E2EFDA")
             ))
    
    StudyDDI <- StudyDDI[2:nrow(StudyDDI), ]
    formatXL(StudyDDI %>% rename("Simulated data with DDI" = X1,
                                 "ignore" = X2, "_" = X3, "__" = X4,
                                 "ignore2" = X5, "___" = X6),
             file = filename,
             sheet = "study info - DDI",
             colWidth = list(colNum = 1:6,
                             width = c(80, 0, 30,
                                       75, 0, 30)),
             styles = list(
                 list(columns = c(1, 4), textposition = list(wrapping = TRUE)),
                 
                 # simulated data section
                 list(rows = 0, font = list(bold = TRUE, size = 18)), 
                 list(rows = c(1, 8), font = list(bold = TRUE, size = 14), # This is actually both simulated and observed headings for "Item" and "Value"
                      textposition = list(alignment = "middle")),
                 list(rows = 2, columns = 3, textposition = list(wrapping = TRUE)),
                 
                 # observed data section
                 list(rows = 0, columns = 2:6, font = list(color = "#FCFEFE")), # <- Closest I can get to white since, for some reason, "white" doesn't work and neither does the hex specification.
                 list(rows = 5, font = list(bold = TRUE, size = 18)), 
                 list(rows = 6, font = list(italics = TRUE), # Notes for observed data section
                      textposition = list(wrapping = TRUE)),
                 
                 # Dose 1 data
                 list(rows = 14, columns = 1:6, 
                      font = list(bold = TRUE, size = 14), fill = "#DDEBF7"),
                 list(rows = c(15, 32), columns = 1:6, 
                      font = list(bold = TRUE), fill = "#DDEBF7"),
                 list(rows = c(16:31, 33:42), columns = 1:6, fill = "#DDEBF7"),
                 
                 # Multiple-dose data
                 list(rows = 45, columns = 1:6, 
                      font = list(bold = TRUE, size = 14), fill = "#E2EFDA"),
                 list(rows = c(46, 59), columns = 1:6, 
                      font = list(bold = TRUE), fill = "#E2EFDA"),
                 list(rows = c(47:58, 60:67), columns = 1:6, fill = "#E2EFDA")
             ))
    
    setwd(CurDir)
}


