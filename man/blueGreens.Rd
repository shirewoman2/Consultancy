% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorVectors.R
\name{blueGreens}
\alias{blueGreens}
\title{Create a vector of blues fading into greens}
\usage{
blueGreens(ncolors)
}
\arguments{
\item{ncolors}{number of colors desired}
}
\value{
a character vector of colors
}
\description{
\code{blueGreens} is useful for getting a set of pretty blues that fade into
greens for graphs.
}
\examples{
# Create a set of 10 blues to greens.
blueGreens(10)

# Try using scales::show_col() to visualize the colors, ex:
scales::show_col(blueGreens(10))

}
