% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_units.R
\name{convert_time_units}
\alias{convert_time_units}
\title{Convert the time units in a data.frame of concentration-time data}
\usage{
convert_time_units(
  DF_to_convert,
  time_units = "hours",
  DF_with_good_units = NA
)
}
\arguments{
\item{DF_to_convert}{the data.frame of concentration-time data with units that
may need to be converted. If you're converting concentration units, this must
include the columns "Conc" and "Conc_units", and, if you're converting time
units, it must include the columns "Time", and "Time_units". (Outputs from
\code{\link{extractConcTime}}, \code{\link{extractConcTime_mult}}, and
\code{\link{extractObsConcTime}} work here.) If you want to convert between
mass per volume and molar concentrations and there are multiple compounds
present, this must also include the column "CompoundID".}

\item{time_units}{time units to use, e.g., "hours". Options for time units
are "hours", "days", and "minutes".}

\item{DF_with_good_units}{if you would like to just match another data.frame,
supply here a data.frame that has the desired concentration and time units.
Options for concentration units are the same as the ones in the Excel form
for PE data entry, and options for time units are "hours", "days", and
"minutes".}
}
\value{
a data.frame with converted time units
}
\description{
Convert the time units in a data.frame of concentration-time data
}
\examples{
# none yet


}
