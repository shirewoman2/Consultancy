% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractEnzAbund_mult.R
\name{extractEnzAbund_mult}
\alias{extractEnzAbund_mult}
\title{Pull enzyme-abundance data from multiple Simcyp Simulator output files}
\usage{
extractEnzAbund_mult(
  sim_data_files = NA,
  sim_enz_dataframe = NA,
  overwrite = FALSE,
  enzymes = "CYP3A4",
  tissues = "liver",
  time_units_to_use = "hours",
  returnAggregateOrIndiv = "aggregate",
  existing_exp_details = NA,
  fromMultFunction = FALSE,
  ...
)
}
\arguments{
\item{sim_data_files}{a character vector of simulator output files, each in
quotes and encapsulated with \code{c(...)}, NA to extract enzyme-abundance
data for \emph{all} the Excel files in the current folder, or "recursive"
to extract enzyme-abundance data for \emph{all} the Excel files in the
current folder and \emph{all} subfolders. Example of acceptable input:
\code{c("sim1.xlsx", "sim2.xlsx")}. The path should be included with the
file names if they are located somewhere other than your working directory.
If some of your Excel files are not regular simulator output, e.g. they are
sensitivity analyses or a file where you were doing some calculations,
those files will be skipped.}

\item{sim_enz_dataframe}{(optional) a data.frame that contains previously
extracted enzyme-abundance data. This should NOT be in quotes. Because we
can see scenarios where you might want to extract some enzyme-abundance
data, play around with those data, and then later decide you want to pull
more enzyme-abundance data for comparisons, this data.frame can already
exist. When that is the case, this function will \emph{add} data to that
data.frame. It will \emph{not} overwrite existing data unless
\code{overwrite} is set to TRUE.}

\item{overwrite}{TRUE or FALSE (default) on whether to re-extract the
enzyme-abundance data from output files that are already included in
\code{sim_enz_dataframe}. Since pulling data from Excel files is slow, by
default, this will \emph{not} overwrite existing data and instead will only
add data from any Excel files that aren't already included. A situation
where you might want to set this to TRUE would be when you have changed
input parameters for simulations and re-run them.}

\item{enzymes}{the enzymes of interest, e.g., "CYP3A4" (default), "UGT1A1",
etc. Any enzyme present in the simulator output should work fine. To
request multiple enzymes, enclose them in \code{c(...)}, e.g.,
\code{enzymes = c("CYP3A4", "CYP2D6", "CYP2C19")}. Spaces or hyphens in
enzyme names will be ignored. Not case sensitive.}

\item{tissues}{From which tissues should the desired enzyme abundances be
extracted? Options are "liver" (default), "gut", or "kidney". Note: If
"gut" is selected, the output will return both colon and small intestine
concentrations. To request multiple tissues, enclose them in \code{c(...)},
e.g., \code{tissues = c("liver", "gut", "kidney")}}

\item{time_units_to_use}{time units to use so that all data will be
comparable. Options are "hours" (default) or "minutes".}

\item{returnAggregateOrIndiv}{Return aggregate and/or individual simulated
enzyme-abundance data? Options are "aggregate" (default), "individual", or
"both". Aggregated data are not calculated here but are pulled from the
simulator output rows labeled as "Population Statistics".}

\item{existing_exp_details}{If you have already run
\code{\link{extractExpDetails_mult}} to get all the details from the "Input
Sheet" (e.g., when you ran extractExpDetails_mult you said
\code{exp_details = "Input Sheet"} or \code{exp_details = "all"}), you can
save some processing time by supplying that object here, unquoted. If left
as NA, this function will run \code{extractExpDetails} behind the scenes to
figure out some information about your experimental set up.}

\item{fromMultFunction}{INTERNAL USE ONLY. TRUE or FALSE on whether this is
being called on by \code{\link{extractConcTime_mult}}.}
}
\value{
Returns a large data.frame with multiple sets of enzyme-abundance
  data, formatted the same way as output from the function
  \code{\link{extractEnzAbund}}
}
\description{
\code{extractEnzAbund_mult} is meant to be used in conjunction with
\code{\link{enz_plot_overlay}} to create graphs from multiple Simcyp
Simulator output files or from multiple enzymes or tissues. If you list
multiple files, multiple tissues, and/or multiple enzymes to extract (see
notes on options below), this will extract \emph{all} possible variations of
them. For example, if you ask for data from the files "sim1.xlsx" and
"sim2.xlsx" and then also ask for the enzymes "CYP3A4" and "CYP2C9", you will
get the CYP3A4 and CYP2C9 abundance data from \emph{both} files.
\strong{NOTE:} If ANY of the Excel files you wish to extract data from are
open, this WILL CRASH and WILL NOT save whatever progress it has made so far.
Be sure to close all of the source Excel files. For detailed instructions and
examples, please see the SharePoint file "Simcyp PBPKConsult R Files - Simcyp
PBPKConsult R Files/SimcypConsultancy function examples and
instructions/Enzyme abundance plots/Enzyme-abundance-plot-examples.docx".
(Sorry, we are unable to include a link to it here.)
}
\examples{
ConcTimeData <-
      extractEnzAbund_mult(
            sim_data_files = c("MyFile1.xlsx", "MyFile2.xlsx"),
            overwrite = FALSE,
            enzymes = c("CYP3A4", "CYP2C9"),
            tissues = c("liver", "gut", "kidney"))

}
