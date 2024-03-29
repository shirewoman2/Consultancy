% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSectionInfo.R
\name{getSectionInfo}
\alias{getSectionInfo}
\title{Get information from an Excel report-input file about a section for a report}
\usage{
getSectionInfo(report_input_file = NA, sheet_report = "study info - no DDI")
}
\arguments{
\item{report_input_file}{the name of the Excel file created by running
\code{\link{generateReportInputForm}}, including the path if it's in any
other directory than the current one, which you have now filled out}

\item{sheet_report}{the sheet in the Excel report template file that contains
information about this section of the report. In the original template,
this was the tab titled "table and graph input".}
}
\value{
a list object
}
\description{
\code{getSectionInfo} pulls information from an Excel file generated by
\code{\link{generateReportInputForm}} to feed into the function
\code{\link{pksummary_table}}. \strong{Most users probably do not want this function
alone since it's mainly for use inside other functions.} Please see
\code{\link{pksummary_table}} for details.
}
\examples{
# getSectionInfo(report_input_file = "//certara.com/data/sites/SHF/Consult/abc-1a/Report input.xlsx")

}
