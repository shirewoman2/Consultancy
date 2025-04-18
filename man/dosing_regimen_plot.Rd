% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dosing_regimen_plot.R
\name{dosing_regimen_plot}
\alias{dosing_regimen_plot}
\title{Plot the dosing regimens used in simulations}
\usage{
dosing_regimen_plot(
  existing_exp_details,
  sims_to_include = NA,
  break_up_by = "Compound"
)
}
\arguments{
\item{existing_exp_details}{output from \code{\link{extractExpDetails}} or
\code{\link{extractExpDetails_mult}}.}

\item{sims_to_include}{optionally specify which simulation files you'd like
  to include in the annotated output. Acceptable input:

  \describe{\item{NA (default)}{get all the simulations included in
  \code{existing_exp_details}}

  \item{a character vector of the file names you want}{The items in the character
  vector must \emph{exactly} match file names in the column "File" of the
  "MainDetails" item in \code{existing_exp_details}, including the ".xlsx" or ".db"
  file extension}}}

\item{break_up_by}{The output graphs will all be broken up by the simulation
file name and also by either the compound name (specify \code{break_up_by =
"Compound"}, the default) or by the compound ID (specify
\code{break_up_by = "CompoundID"} to break up the graphs by whether the
compound was the substrate, the inhibitor 1, etc. in the simulation).}
}
\value{
a ggplot2 graph
}
\description{
\code{dosing_regimen_plot} creates a graph of the dosing regimen used in the
simulations provided. UNDER CONSTRUCTION.
}
\examples{
# UNDER CONSTRUCTION 

}
