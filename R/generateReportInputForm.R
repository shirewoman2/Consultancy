#' Generate a form for entering data for a report
#'
#' Creates an Excel file with three forms to fill out. Those forms can be fed
#' into \code{\link{getSectionInfo}} and \code{\link{so_table}} to automate some
#' of the calculations and graph creation for writing a report. \strong{A note:}
#' This generates a warning that we're just not able to get rid of that says
#' "Workbook has no sheets!" Please disregard that. \strong{Important note:}
#' This does not work on the SharePoint drive because it doesn't have permission
#' to write there. Please set your working directory to a different location. It
#' \emph{will} work on the Large File Store drive, but it will be quite slow to
#' do so from your local machine. Really, everything runs markedly faster when
#' you run it from your VM instead.
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
#'   creating, ending in ".xlsx". IMPORTANT: If you specify the full file path,
#'   this does not work, and we haven't figured out why. Instead, please
#'   navigate to where you want the file to be and only list the file name here
#'   rather than the full path. 
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
    
    # If they didn't include ".xlsx" at the end, add that.
    filename <- ifelse(str_detect(filename, "xlsx$"), 
                       filename, paste0(filename, ".xlsx"))
    
    # Check whether file exists and stop if it does so that no one
    # accidentally overwrites something they've spent time filling out
    # already.
    if(file.exists(filename)){
        stop(paste0("The file '", filename, "' already exists. Please use a different file name or delete the existing version and try again."))
    }
    
    # Loading the forms
    data("ReportInputForm")
    
    Path <- dirname(filename)
    File <- basename(filename)
    CurDir <- getwd()
    
    # For some reason, when you only include a relative path for the file with
    # the formatXL function, it will overwrite rather than adding sheets, but
    # it DOESN'T do that when you just set the path first and then write.
    setwd(Path)
    
    # data(ReportInputForm)
    HowTo <- ReportInputForm[["how to use this file"]]
    Overall <- ReportInputForm[["overall report info"]]
    ObsNoDDI <- ReportInputForm[["observed data - no DDI"]]
    ObsDDI <- ReportInputForm[["observed data - DDI"]]
    TabGraph <- ReportInputForm[["table and graph input"]]
    
    HowTo <- HowTo[2:nrow(HowTo),]
    formatXL(as.data.frame(HowTo) %>% rename("How to use this Excel file" = HowTo),
             file = filename,
             sheet = "how to use this file",
             colWidth = list(colNum = 1,
                             width = 250),
             styles = list(
                 list(rows = 0, font = list(bold = TRUE, size = 16)),
                 list(rows = 18, font = list(bold = TRUE)),
                 list(rows = 20, font = list(color = "red"))
             ))
    
    Overall <- Overall[2:nrow(Overall),]
    formatXL(Overall %>% rename("Overall report information" = X1,
                                "ignore" = X2, "_" = X3),
             file = filename,
             sheet = "overall report info",
             colWidth = list(colNum = 1:3,
                             width = c(75, 0, 30)),
             styles = list(
                 list(columns = 1, textposition = list(wrapping = TRUE)),
                 list(rows = 0, font = list(bold = TRUE, size = 16)),
                 list(rows = 0, columns = 3, font = list(color = "#FCFEFE")), # <- Closest I can get to white since, for some reason, "white" doesn't work and neither does the hex specification.
                 list(rows = 1, font = list(italics = TRUE),
                      textposition = list(wrapping = TRUE)),
                 list(rows = 3, font = list(bold = TRUE),
                      textposition = list(alignment = "middle"))
             ))
    
    
    ObsNoDDI <- ObsNoDDI[2:nrow(ObsNoDDI), ]
    formatXL(ObsNoDDI %>% rename("Observed data (no DDI involved)" = X1,
                                 "ignore" = X2, "_" = X3),
             file = filename,
             sheet = "observed data - no DDI",
             colWidth = list(colNum = 1:3,
                             width = c(75, 0, 30)),
             styles = list(
                 list(columns = 1, textposition = list(wrapping = TRUE)),
                 list(rows = 0, font = list(bold = TRUE, size = 16)),
                 list(rows = 0, columns = 3, font = list(color = "#FCFEFE")), # <- Closest I can get to white since, for some reason, "white" doesn't work and neither does the hex specification.
                 list(rows = 1, font = list(italics = TRUE),
                      textposition = list(wrapping = TRUE)),
                 list(rows = 3, font = list(bold = TRUE),
                      textposition = list(alignment = "middle")),
                 list(rows = c(8, 21), font = list(bold = TRUE))
             ))
    
    ObsDDI <- ObsDDI[2:nrow(ObsDDI), ]
    formatXL(ObsDDI %>% rename("Observed data with DDI" = X1,
                               "ignore" = X2, "_" = X3, "__" = X4,
                               "ignore2" = X5, "___" = X6),
             file = filename,
             sheet = "observed data - DDI",
             colWidth = list(colNum = 1:6,
                             width = c(75, 0, 30,
                                       75, 0, 30)),
             styles = list(
                 list(columns = c(1, 4), textposition = list(wrapping = TRUE)),
                 list(rows = 0, font = list(bold = TRUE, size = 16)),
                 list(rows = 0, columns = 3:6, font = list(color = "#FCFEFE")), # <- Closest I can get to white since, for some reason, "white" doesn't work and neither does the hex specification.
                 list(rows = 1, font = list(italics = TRUE),
                      textposition = list(wrapping = TRUE)),
                 list(rows = 3, font = list(bold = TRUE),
                      textposition = list(alignment = "middle")),
                 list(rows = c(15, 28), font = list(bold = TRUE))
             ))
    
    TabGraph <- TabGraph[2:nrow(TabGraph), ]
    formatXL(TabGraph %>% rename("Table and graph input" = X1,
                                 "ignore" = X2, "_" = X3),
             file = filename,
             sheet = "table and graph input",
             colWidth = list(colNum = 1:3,
                             width = c(75, 0, 30)),
             styles = list(
                 list(columns = 1, textposition = list(wrapping = TRUE)),
                 list(rows = 0, font = list(bold = TRUE, size = 16)),
                 list(rows = 0, columns = 3, font = list(color = "#FCFEFE")), # <- Closest I can get to white since, for some reason, "white" doesn't work and neither does the hex specification.
                 list(rows = 1, font = list(italics = TRUE),
                      textposition = list(wrapping = TRUE)),
                 list(rows = 3, font = list(bold = TRUE),
                      textposition = list(alignment = "middle"))
             ))
    
    
    setwd(CurDir)
}


