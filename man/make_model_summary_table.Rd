% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_model_summary_table.R
\name{make_model_summary_table}
\alias{make_model_summary_table}
\title{Summarize a model pretty simply - UNDER CONSTRUCTION}
\usage{
make_model_summary_table(
  existing_exp_details,
  sims_to_include = "all",
  compoundID = "substrate",
  font = "Palatino Linotype",
  fontsize = 11,
  save_table = NA,
  page_orientation = "portrait"
)
}
\arguments{
\item{existing_exp_details}{the output from running
\code{\link{extractExpDetails_mult}}}

\item{sims_to_include}{optionally specify which simulation files you'd like
to include in the annotated output. Acceptable input: "all" to get
everything or else a character vector of the sim results files you want. If
these are not present in existing_exp_details, they will be ignored.}

\item{compoundID}{For which compound do you want to summarize the model
parameters? Options are "substrate", "primary metabolite 1", "primary
metabolite 2", "secondary metabolite", "inhibitor 1", "inhibitor 2", or
"inhibitor 1 metabolite".}

\item{font}{font to use. Default is "Arial" and any fonts available on your
machine in either Word or PowerPoint should be acceptable. If you get Times
New Roman in your table when you asked for something else, it means that
that font isn't available or maybe wasn't spelled the way R is expecting
it. For example, "Calibri" works but "Calibri (Body)" doesn't even though
the latter is listed in PowerPoint and Word.}

\item{fontsize}{the numeric font size for the output table. Default is 11
point.}

\item{save_table}{optionally save the output table by supplying a file name
in quotes here, e.g., "My nicely formatted table.docx".  Do not include any
slashes, dollar signs, or periods in the file name. If you leave off the
file extension, we'll assume you want it to be ".docx". If there is a
column titled "File" in your table, we'll add a caption listing which files
were included.}

\item{page_orientation}{set the page orientation for the Word file output to
"portrait" (default) or "landscape"}
}
\value{
a formatted table
}
\description{
\code{make_model_summary_table} is meant for use with writing compound
summary pdfs. It includes minimal error catching at this point. NB: This
rounds numeric values to the 5th digit.
}
\examples{
# none yet
}
