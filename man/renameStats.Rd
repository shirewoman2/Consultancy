% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renameStats.R
\name{renameStats}
\alias{renameStats}
\title{INTERNAL: Rename the statistics listed in Simulator output to R-friendly
versions}
\usage{
renameStats(OrigStat, use = "Simulator to R")
}
\arguments{
\item{OrigStat}{the original statistic name}

\item{use}{options are "Simulator to R" for internal use when renaming stats
from Simulator-output names into R-friendly column names, "report" for
renaming R columns into the values that show up in the Simcyp Consultancy
report template tables of PK parameters, or "report to R" when reversing
from report-friendly names to R-friendly names.}
}
\value{
an R-friendly version of that statistic name
}
\description{
\code{renameStats} takes Simulator statistic names such as "90% confidence
interval around the geometric mean(lower limit)" and converts them to names
that would work for R column names, e.g., "CI90_lower", or converts them to
names that comply with Simcyp Consultancy Team report templates. This is
mainly meant for internal use in the SimcypConsultancy package to help make
sure we're naming things consistently.
}
\examples{
renameStats(c("Geometric Mean", "Min Val", "5th centile"))

}
