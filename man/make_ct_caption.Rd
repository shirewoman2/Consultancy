% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_ct_caption.R
\name{make_ct_caption}
\alias{make_ct_caption}
\title{INTERNAL - make caption for conc-time plots}
\usage{
make_ct_caption(
  ct_dataframe,
  single_or_multiple_profiles = "single",
  plot_type = "concentration-time",
  existing_exp_details = NA,
  mean_type = "arithmetic",
  include_errorbars = FALSE,
  linear_or_log = "linear",
  figure_type = "percentiles",
  prettify_compound_names = TRUE,
  name_clinical_study = NA,
  hline_position = NA,
  hline_style = "red dotted",
  vline_position = NA,
  vline_style = "red dotted"
)
}
\arguments{
\item{ct_dataframe}{ct_dataframe}

\item{single_or_multiple_profiles}{whether this is for a ct_plot graph (only
1 compound, tissue, file, etc.) or a graph with multiple profiles (multiple
compounds, tissues, files, etc.). Options are "single" (default) or
"multiple".}

\item{plot_type}{options are "concentration-time", "enzyme-abundance",
"dissolution-profile", "release-profile". May change these to be prettier.}

\item{existing_exp_details}{OR supply: a data.frame with the following
columns: File, Population, Substrate, Inhibitor1, Inhibitor1Metabolite,
Inhibitor2, ... If left as NA, we'll note some generic parameters and make
them bold to note that they should be changed.}

\item{mean_type}{"arithmetic" (default) or "geometric"}

\item{linear_or_log}{"linear" (default), "both", "both horizontal", "both
vertical", "log", "semi-log", "horizontal and vertical"}

\item{figure_type}{default is percentiles}

\item{prettify_compound_names}{default is TRUE}

\item{name_clinical_study}{optionally specify the name(s) of the clinical
study or studies for any observed data. This only affects the caption of
the graph. For example, specifying \code{name_clinical_study = "101, fed
cohort"} will result in a figure caption that reads in part "clinical study
101, fed cohort". If you have more than one study, that's fine; we'll take
care of stringing them together appropriately. Just list them as a
character vector, e.g., \code{name_clinical_study = c("101",
"102", "103")} will become "clinical studies 101, 102, and 103."}

\item{hline_position}{hline_position}

\item{hline_style}{hline_style}

\item{vline_position}{vline_position}

\item{vline_style}{vline_style}
}
\value{
a string of text for making a caption
}
\description{
INTERNAL - make caption for conc-time plots
}
