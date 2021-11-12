#' Generate a form for entering data for a report
#'
#' Creates an Excel file with three forms to fill out. Those forms can be fed
#' into \code{\link{getSectionInfo}} and \code{\link{so_table}} to automate some
#' of the calculations and graph creation for writing a report.
#'
#' The three tabs in the Excel file this function creates are: \describe{
#'
#' \item{Overall report form}{a form for entering overall data about the project
#' -- information that will be consistent throughout the whole report such as
#' the drug name and the complaint it is meant to treat. There should be only
#' ONE copy of this tab when you're finished.}
#'
#' \item{Observed data form}{a form for entering data about a clinical study.
#' Make as many copies of this tab as there are clinical studies you want to
#' compare and name the tabs according to which study they describe.}
#'
#' \item{Section input form}{a form for entering information about a given
#' section such as which simulation file you'd like to graph and compare to
#' clinical data. As with the tab "Observed data form", make as many copies of
#' this tab as you'd like -- one for each standard results subsection in the
#' report.}}
#'
#' @param filename the Excel file name that you'd like for the form you're
#'   creating, ending in ".xlsx". Note: This WILL overwrite any existing file
#'   with the same name, so BE CAREFUL.
#'
#' @return This does not return an R object; it saves an Excel file that serves
#'   as a form for report information.
#'
#' @export
#'
#' @examples
#' generateReportInputForm("//certara.com/data/sites/SHF/Consult/abc-1a/Ultraconazole report input.xlsx")
#' generateReportInputForm(paste0(SimcypDir$LgFileDir, "abc-1a/Ultraconazole report input.xlsx"))
#' generateReportInputForm(paste0(SimcypDir$SharePtDir, "def-2b/Superstatin report input.xlsx"))
#'
#' 

generateReportInputForm <- function(filename){
      
      # Check for "\" b/c people will probably paste the path from Windows
      filename <- gsub("\\\\", "/", filename)
      
      # If people *did* copy and paste the full path and it includes the "https"
      # part of the share point drive, that doesn't work well. Switch that.
      filename <- sub("https:..s08sharepoint.certara.com.sites.consult.", 
                      SimcypDir$SharePtDir, filename)
      
      # Loading the forms
      data("ReportInputForm")
      
      Path <- dirname(filename)
      File <- basename(filename)
      CurDir <- getwd()
      
      # For some reason, when you only include a relative path for the file with
      # the formatXL function, it will overwrite rather than adding sheets, but
      # it DOESN'T do that when you just set the path first and then write.
      setwd(Path)
      
      formatXL(ReportInputForm[["Overall report form"]], 
               file = filename, 
               sheet = "overall report info",
               styles = list(
                     list(columns = 1, textposition = list(wrapping = TRUE)),
                     list(rows = 0, font = list(bold = TRUE, size = 12), 
                          textposition = list(alignment = "middle"))
               ))
      
      formatXL(ReportInputForm[["Observed data form"]], 
               file = filename, 
               sheet = "observed data", 
               colWidth = list(colNum = 1:3, 
                               width = c(75, 0, 30)),
               styles = list(
                     list(columns = 1, textposition = list(wrapping = TRUE)),
                     list(rows = 0, font = list(bold = TRUE, size = 12), 
                          textposition = list(alignment = "middle")), 
                     list(rows = 22, font = list(bold = TRUE), 
                          textposition = list(wrapping = FALSE))
               ))
      
      
      formatXL(ReportInputForm[["Section input form"]], 
               file = filename,
               sheet = "section input",
               colWidth = list(colNum = 1:3,
                               width = c(75, 0, 30)),
               styles = list(
                     list(columns = 1, textposition = list(wrapping = TRUE)),
                     list(rows = 0, font = list(bold = TRUE, size = 12), 
                          textposition = list(alignment = "middle"))
               ))
      
      setwd(CurDir)
}


