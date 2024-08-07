% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ct_y_axis.R
\name{ct_y_axis}
\alias{ct_y_axis}
\title{Set up y axis in a conc-time plot}
\usage{
ct_y_axis(
  ADAMorAdvBrain,
  Tissue_subtype,
  EnzPlot,
  y_axis_limits_lin,
  time_range,
  y_axis_interval = NA,
  prettify_compound_names = TRUE,
  normalize_by_dose = FALSE,
  y_axis_limits_log,
  Ylim_data,
  pad_y_axis,
  time_range_relative
)
}
\arguments{
\item{ADAMorAdvBrain}{T or F for whether ADAM or advanced brain model used}

\item{Tissue_subtype}{which ADAM-model or advanced-brain-model subsection
the data include}

\item{EnzPlot}{T or F for whether this was a plot of enzyme abundance}

\item{y_axis_limits_lin}{user input for y axis limits for a linear plot}

\item{time_range}{user-specified time range}

\item{y_axis_interval}{user input for y axis tick-mark interval}

\item{prettify_compound_names}{prettify?}

\item{normalize_by_dose}{T or F for whether to normalize concs by dose}

\item{y_axis_limits_log}{user input for y axis limits for a semi-log plot}

\item{Ylim_data}{data used for determining y axis limits}

\item{pad_y_axis}{user-specified value for pad_y_axis}

\item{time_range_relative}{relative time range}
}
\value{
values to use for ct_plots
}
\description{
This function is specifically for setting options for the y axis in a
concentration-time graph and is NOT meant to be called on its own.
}
