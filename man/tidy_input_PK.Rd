% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_input_PK.R
\name{tidy_input_PK}
\alias{tidy_input_PK}
\title{Tidy input PK data as needed}
\usage{
tidy_input_PK(
  PKparameters,
  sim_data_files = NA,
  existing_exp_details = NA,
  compoundsToExtract = "substrate",
  tissues = "plasma",
  sheet_PKparameters = NA
)
}
\arguments{
\item{PKparameters}{whatever they have supplied for PKparameters for
pksummary_table or pksummary_mult. This can be a csv file, a character
vector, or NA.}

\item{sim_data_files}{a character vector of file names. This should no longer
be NA or "recursive".}

\item{existing_exp_details}{If you have already run
\code{\link{extractExpDetails_mult}} to get all the details from the "Input
Sheet" (e.g., when you ran extractExpDetails you said \code{exp_details =
"Summary and Input"} or \code{exp_details = "all"}), you can save some processing
time by supplying that object here, unquoted. If left as NA, this function
will run \code{extractExpDetails} behind the scenes anyway to figure out
some information about your experimental set up.}

\item{compoundsToExtract}{the values they supplied for compoundsToExtract in
the parent function.}

\item{tissues}{the values they supplied for tissues in the parent function}

\item{sheet_PKparameters}{the values they supplied for sheet_PKparameters in
the parent function}

\item{report_input_file}{probably not in use}

\item{sheet_report}{probably not in use}
}
\value{
a list of: 1) "PKparameters" -- a tidy data.frame of PKparameters
  with standardized column names and contents, 2) "existing_exp_details",
  which will only differ from the original in that any missing sim files will
  be added -- none will be removed. 3) "FilePairs" -- if this is from
  calc_PK_ratios, then a tidy data.frame listing which items should be paired
}
\description{
INTERNAL PACKAGE USE ONLY. Reads (as needed), harmonizes and tidies observed
PK input. Output is a data.frame of PK parameters with columns File, Sheet,
CompoundID, Tissue, Value, and Variability. The columns Sheet, Value, and
Variability may contain NAs.
}
\examples{
# nope 
}
