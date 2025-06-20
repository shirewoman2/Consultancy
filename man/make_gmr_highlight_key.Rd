% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_boundary_colors.R
\name{make_gmr_highlight_key}
\alias{make_gmr_highlight_key}
\title{Make a key for highlighted geometric mean ratios for DDIs in a table}
\usage{
make_gmr_highlight_key(highlight_gmr_colors, orientation = "long")
}
\arguments{
\item{highlight_gmr_colors}{a set of colors for highlighting geometric mean
ratios for DDIs. Options are "yellow to red", "green to red", "traffic" (a
more vivid version of "green to red"), or a vector of 4 colors of your
choosing.}

\item{orientation}{orientation of the key; options are "long" (default) or
"wide"}
}
\value{
a flextable for use as a key for which colors mean what GMR cutoff
}
\description{
Make a key for highlighted geometric mean ratios for DDIs in a table
}
\examples{
make_gmr_highlight_key("yellow to red")

}
