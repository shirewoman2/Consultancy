% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_Simcyp_inputs_table.R
\name{make_Simcyp_inputs_table}
\alias{make_Simcyp_inputs_table}
\title{Summarize a PBPK model using parameters pulled from simulations - UNDER
CONSTRUCTION}
\usage{
make_Simcyp_inputs_table(
  existing_exp_details,
  sims_to_include = "all",
  compoundID = NA,
  compound = NA,
  references = NA,
  parameters_to_omit = NA,
  parameters_to_add = NA,
  separate_column_for_ADME = TRUE,
  font = "Palatino Linotype",
  fontsize = 11,
  column_widths = NA,
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

\item{compound}{optionally supply a specific compound name or part of a
specific compound name to get all possible compounds that match that and
\emph{only} compounds that match that. Regular expressions are acceptable
here, e.g., \code{compound = "midaz|keto"} to find any compound with either
"midaz" or "keto" in the name. Not case sensitive. If you request only
information on a specific compound, we will assume what what you care about
is the set of parameters used for that compound and not whether that
compound was the substrate or the inhibitor, so we'll ignore anything you
supply for compoundID.}

\item{references}{a data.frame or csv file listing the source of all the
parameters used in the model. The columns must include:
\describe{\item{"Parameter" or "Detail" (either is fine)}{the specific
parameter of interest with the coded name for that parameter. How can you
find out the coded name? Run something like this:
\code{annotateDetails(existing_exp_details = Details, compoundID = "substrate", detail_set = "Simcyp inputs")}
and the column "Detail" will
list all of the coded names for the parameters that will be included in
this table. They must match exactly or the reference will be omitted.}
\item{"Reference"}{the reference for that parameter}}}

\item{parameters_to_omit}{optionally specify any parameters to omit from the
table. For example, maybe you don't want to include that the biliary
clearance was set to 0 because that's just not important. To omit that,
you'll need the exact, coded name of that parameter, and you can list any
parameters to omit as a character vector. How does one obtain the exact,
coded name of a parameter? So glad you asked. Please run something like
this: \code{annotateDetails(existing_exp_details = Details, compoundID =
"substrate", detail_set = "Simcyp inputs")} and the column "Detail" will
list all of the coded names for the parameters that will be included in
this table by default. In our example, you would specify
\code{parameters_to_omit = "CLint_biliary_sub"}}

\item{parameters_to_add}{optionally add any parameters that weren't included
by default. Acceptable input is a data.frame with the following columns:
\describe{\item{SimulatorSection}{the section of the simulator, e.g.,
"Phys Chem and Blood Binding", "Absorption", "Distribution", "Elimination",
"Interaction", "Transport", "Trial Design", or "Population". These must match those options exactly.}
\item{Detail (also ok to call this column "Parameter")}{the parameter name}
\item{Value}{the value of this parameter}
\item{Reference}{the reference for the parameter (optional)}}}

\item{separate_column_for_ADME}{In the table, do you want a separate column
for ADME headings -- "Absoprtion", "Distribution", etc. -- or would you
like the ADME headings to be included in the "Parameter" column in the
table? Default is TRUE for having a separate column.}

\item{font}{font to use. Default is "Arial" and any fonts available on your
machine in either Word or PowerPoint should be acceptable. If you get Times
New Roman in your table when you asked for something else, it means that
that font isn't available or maybe wasn't spelled the way R is expecting
it. For example, "Calibri" works but "Calibri (Body)" doesn't even though
the latter is listed in PowerPoint and Word.}

\item{fontsize}{the numeric font size for the output table. Default is 11
point.}

\item{column_widths}{optionally specify what the widths of the columns should
be with a numeric vector of the widths in inches, e.g., \code{column_widths
= c(1, 2, 1, 1)}}

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
\code{make_Simcyp_inputs_table} will make a neatly formatted,
  organized, human-readable table of the main Simcyp model inputs based on
  information included in your simulation(s). It was designed for use with
  writing compound summary pdfs, and, if you supply the references for each
  parameter, it will include that information in the table.

  This function requires you to first run
  \code{\link{extractExpDetails_mult}} to get information about your
  simulations in a format this function can use. To see what will be included
  in this table and to see the coded names for each of the parameters that
  will be included in the table, please run something like:

  \code{Details <- extractExpDetails_mult()}

  \code{TableCheck <- annotateDetails(existing_exp_details = Details, compoundID =
  "substrate", detail_set = "Simcyp inputs")}

  and the object Details is what you will provide as input for
  \code{existing_exp_details}. The object TableCheck will show you what
  information will be included and how to refer to each parameter or detail.
  The values to listed for each parameter in your table will come from the
  column titled something like "All files have this value for this compound".

  Notes: \itemize{\item{This rounds numeric values to the 5th digit.}
  \item{There are some parameters that are included in the standard
  set of outputs from \code{\link{extractExpDetails_mult}} that we will
  ignore: the Simulator version, Ki values set to 1e+06 (the default value in the Simulator for,
  really, no inhibition), Indmax values of 1 (also the default value in the
  Simulator for no interaction), etc.}}
}
\examples{
# none yet
}
