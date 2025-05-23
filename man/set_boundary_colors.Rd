% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_boundary_colors.R
\name{set_boundary_colors}
\alias{set_boundary_colors}
\title{INTERNAL: Create a set of boundary colors based on the color set and the
boundaries the user needs.}
\usage{
set_boundary_colors(color_set, boundaries, break_type = "SO highlight")
}
\arguments{
\item{color_set}{either a single value, e.g., "red green" or a character
  vector of colors. Acceptable single-value color set names:\describe{
  \item{"yellow to red"}{for highlighting in PK tables}

  \item{"green to red"}{for highlighting in PK tables}

  \item{"traffic"}{for highlighting in PK tables}

  \item{"red black"}{for S/O boundary lines or fill}

  \item{"red green"}{for S/O boundary lines or fill}

  \item{"muted red green"}{for S/O boundary lines or fill}}}

\item{break_type}{What type of breaks are these? Options are "GMR" (there
will only be 4 and the output will be a named character vector for the
intensity of the DDI), "SO highlight" (default) for S/O values in tables,
and "SO line" for coloring the lines in S/O graphs. This distinction
between "SO highlight" and "SO line" arises because the colors for the
lines in the S/O graphs generally should have the 1st color repeated
whereas the highlighting for tables should \emph{not} have any repeats. At
present, "yellow to red", "traffic", and "green to red" are only available
for break_types of "GMR" or "SO highlight". "red black", "red green", and
"muted red green" are only available for a break_type of "SO line".}
}
\value{
a character vector of colors, one for each numeric boundary
}
\description{
INTERNAL: Create a set of boundary colors based on the color set and the
boundaries the user needs.
}
\examples{
# None. Internal use only.

}
